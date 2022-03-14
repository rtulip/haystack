use crate::compiler::compiler_error;
use crate::ir::{
    function::Function,
    keyword::Keyword,
    marker::Marker,
    op::{Op, OpKind},
    operator::Operator,
    program::Program,
    token::{eof_tok, Token, TokenKind},
    types::{Signature, Type},
};
use crate::lex::logos_lex::{into_token, LogosToken};
use logos::Logos;
use std::collections::HashMap;
use std::fs;

fn parse_tokens_until_tokenkind(
    tokens: &mut Vec<Token>,
    ops: &mut Vec<Op>,
    break_on: Vec<TokenKind>,
) -> Token {
    while let Some(token) = tokens.pop() {
        if break_on.contains(&token.kind) {
            return token;
        }

        match token.kind {
            TokenKind::Keyword(Keyword::Var) => {
                let (_tok, mut idents) = parse_var_from_tokens(&token, tokens);
                ops.append(&mut idents);
            }
            TokenKind::Keyword(Keyword::If) => {
                let _tok = parse_if_block_from_tokens(&token, tokens, ops);
            }
            TokenKind::Keyword(Keyword::Else) => {
                panic!("Else keyword should be turned into an op")
            }
            TokenKind::Keyword(Keyword::Function) => {
                panic!("Function keyword can't be converted into ops.")
            }
            TokenKind::Keyword(Keyword::Struct) => {
                panic!("Struct keyword can't be converted into ops")
            }
            TokenKind::Keyword(Keyword::While) => {
                let _tok = parse_while_block_from_tokens(&token, tokens, ops);
            }
            TokenKind::Comment(_) => (),
            TokenKind::Operator(_) => ops.push(Op::from(token.clone())),
            TokenKind::Literal(_) => ops.push(Op::from(token.clone())),
            TokenKind::Marker(_) => panic!("Markers shouldn't be converted into ops..."),
            TokenKind::Word(_) => ops.push(Op::from(token.clone())),
            TokenKind::EndOfFile => panic!("End of file shouldn't be converted into an op."),
            // _ => ops.push(Op::from(token.clone())),
        }
    }
    compiler_error(
        &eof_tok(),
        format!(
            "Expected one of {:?}, but found end of file instead",
            break_on
        )
        .as_str(),
        vec![],
    )
}

fn expect_token_kind(prev_tok: &Token, tokens: &mut Vec<Token>, kind: TokenKind) -> Token {
    if let Some(token) = tokens.pop() {
        if token.kind != kind {
            compiler_error(
                &token,
                format!("Expected {:?}, but found {:?} instead", kind, token.kind).as_str(),
                vec![],
            );
        }
        token
    } else {
        compiler_error(
            prev_tok,
            format!("Expected {:?}, but found end of file instead", kind).as_str(),
            vec![],
        );
    }
}

fn peek_token_kind(tokens: &mut Vec<Token>, kind: TokenKind) -> bool {
    if let Some(token) = tokens.last() {
        token.kind == kind
    } else {
        false
    }
}

fn expect_word(prev_tok: &Token, tokens: &mut Vec<Token>) -> (Token, String) {
    if let Some(token) = tokens.pop() {
        let s = match &token.kind {
            TokenKind::Word(s) => s.clone(),
            _ => compiler_error(
                &token,
                format!("Expected a word, but found {:?} instead", token.kind).as_str(),
                vec![],
            ),
        };
        (token, s)
    } else {
        compiler_error(
            prev_tok,
            "Expected a word, but found end of file instead",
            vec![],
        );
    }
}

fn peek_word(tokens: &[Token]) -> bool {
    if let Some(token) = tokens.last() {
        matches!(token.kind, TokenKind::Word(_))
    } else {
        false
    }
}

fn parse_maybe_tagged_type_from_tokens(
    start_tok: &Token,
    tokens: &mut Vec<Token>,
    type_map: &HashMap<String, Type>,
) -> Option<(Token, Type, Option<String>)> {
    if peek_word(tokens) {
        let (tok, name) = expect_word(start_tok, tokens);
        let typ = if type_map.contains_key(&name) {
            type_map.get(&name).unwrap().clone()
        } else {
            Type::Placeholder { name }
        };
        if peek_token_kind(tokens, TokenKind::Marker(Marker::Colon)) {
            let tok = expect_token_kind(&tok, tokens, TokenKind::Marker(Marker::Colon));
            let (tok, ident) = expect_word(&tok, tokens);
            Some((tok, typ, Some(ident)))
        } else {
            Some((tok, typ, None))
        }
    } else {
        None
    }
}

fn parse_untagged_type_from_tokens(
    start_tok: &Token,
    tokens: &mut Vec<Token>,
    type_map: &HashMap<String, Type>,
) -> Option<(Token, Type)> {
    if peek_word(tokens) {
        let (tok, name) = expect_word(start_tok, tokens);
        let typ = if type_map.contains_key(&name) {
            type_map.get(&name).unwrap().clone()
        } else {
            Type::Placeholder { name }
        };
        Some((tok, typ))
    } else {
        None
    }
}

fn parse_function_generics(
    start_tok: &Token,
    tokens: &mut Vec<Token>,
    type_map: &HashMap<String, Type>,
) -> Option<(Token, Vec<Type>)> {
    // Check to see if any generics were provided.
    if peek_token_kind(tokens, TokenKind::Marker(Marker::OpenParen)) {
        return None;
    }

    let mut tok = expect_token_kind(start_tok, tokens, TokenKind::Operator(Operator::LessThan));
    let mut gen = vec![];

    while let Some((typ_tok, typ)) = parse_untagged_type_from_tokens(&tok, tokens, type_map) {
        gen.push(typ);
        tok = typ_tok;
    }

    tok = expect_token_kind(&tok, tokens, TokenKind::Operator(Operator::GreaterThan));
    Some((tok, gen))
}

fn parse_maybe_tagged_type_list_from_tokens(
    token: &Token,
    tokens: &mut Vec<Token>,
    type_map: &HashMap<String, Type>,
) -> (Vec<Type>, Vec<Option<String>>) {
    let mut tok = token.clone();
    let mut inputs = vec![];
    let mut idents = vec![];
    while let Some((typ_tok, typ, ident)) =
        parse_maybe_tagged_type_from_tokens(&tok, tokens, type_map)
    {
        inputs.push(typ);
        idents.push(ident);
        tok = typ_tok;
    }
    (inputs, idents)
}

fn parse_function_inputs_from_tokens(
    start_tok: &Token,
    tokens: &mut Vec<Token>,
    type_map: &HashMap<String, Type>,
) -> (Token, Vec<Type>, Vec<Option<String>>) {
    let tok = expect_token_kind(start_tok, tokens, TokenKind::Marker(Marker::OpenParen));
    let (inputs, idents) = parse_maybe_tagged_type_list_from_tokens(&tok, tokens, type_map);
    let tok = expect_token_kind(&tok, tokens, TokenKind::Marker(Marker::CloseParen));
    (tok, inputs, idents)
}

fn parse_function_outputs_from_tokens(
    start_tok: &Token,
    tokens: &mut Vec<Token>,
    type_map: &HashMap<String, Type>,
) -> Option<(Token, Vec<Type>)> {
    if !peek_token_kind(tokens, TokenKind::Marker(Marker::Arrow)) {
        return None;
    }

    let tok = expect_token_kind(start_tok, tokens, TokenKind::Marker(Marker::Arrow));
    let mut tok = expect_token_kind(&tok, tokens, TokenKind::Marker(Marker::OpenBracket));

    let mut outputs = vec![];
    while let Some((typ_tok, typ)) = parse_untagged_type_from_tokens(&tok, tokens, type_map) {
        outputs.push(typ);
        tok = typ_tok;
    }
    let tok = expect_token_kind(&tok, tokens, TokenKind::Marker(Marker::CloseBracket));

    Some((tok, outputs))
}

fn parse_signature_from_tokens(
    start_tok: &Token,
    tokens: &mut Vec<Token>,
    type_map: &HashMap<String, Type>,
) -> (Token, Signature, Vec<Option<String>>) {
    let (tok, inputs, idents) = parse_function_inputs_from_tokens(start_tok, tokens, type_map);
    if let Some((tok, outputs)) = parse_function_outputs_from_tokens(&tok, tokens, type_map) {
        (tok, Signature { inputs, outputs }, idents)
    } else {
        (
            tok,
            Signature {
                inputs,
                outputs: vec![],
            },
            idents,
        )
    }
}

fn parse_word_list_from_tokens(
    start_tok: &Token,
    tokens: &mut Vec<Token>,
) -> (Token, Vec<(Token, String)>) {
    let mut tok = expect_token_kind(start_tok, tokens, TokenKind::Marker(Marker::OpenBracket));
    let mut words = vec![];

    while peek_word(tokens) {
        let (word_tok, word) = expect_word(&tok, tokens);
        tok = word_tok.clone();
        words.push((word_tok, word));
    }
    let tok = expect_token_kind(&tok, tokens, TokenKind::Marker(Marker::CloseBracket));

    (tok, words)
}

fn parse_var_from_tokens(start_tok: &Token, tokens: &mut Vec<Token>) -> (Token, Vec<Op>) {
    let (tok, idents) = parse_word_list_from_tokens(start_tok, tokens);
    if idents.is_empty() {
        compiler_error(
            &tok,
            "Must provide at least one identifier in `var` block.",
            vec![],
        )
    }
    (
        tok,
        idents
            .iter()
            .rev()
            .map(|(token, ident)| Op {
                kind: OpKind::MakeIdent(ident.clone()),
                token: token.clone(),
            })
            .collect(),
    )
}

fn parse_function_from_tokens(
    start_tok: &Token,
    tokens: &mut Vec<Token>,
    type_map: &HashMap<String, Type>,
) -> Function {
    let t = expect_token_kind(start_tok, tokens, TokenKind::Keyword(Keyword::Function));
    let (name_tok, name) = expect_word(&t, tokens);
    let (tok, gen) =
        parse_function_generics(&name_tok, tokens, type_map).unwrap_or((name_tok.clone(), vec![]));
    let (tok, sig, sig_idents) = parse_signature_from_tokens(&tok, tokens, type_map);

    sig.inputs.iter().for_each(|i| {
        if matches!(i, Type::Placeholder { name: _ }) && !gen.contains(i) {
            compiler_error(
                &tok,
                format!("Unrecognized type: `{:?}`", i).as_str(),
                vec![format!("Consider adding `{:?}` to the generic list", i).as_str()],
            )
        }
    });

    let _tok = expect_token_kind(&tok, tokens, TokenKind::Marker(Marker::OpenBrace));
    let mut ops = vec![Op {
        kind: OpKind::PrepareFunc,
        token: name_tok.clone(),
    }];

    sig_idents.iter().rev().for_each(|maybe_ident| {
        if let Some(ident) = maybe_ident {
            ops.push(Op {
                kind: OpKind::MakeIdent(ident.clone()),
                token: name_tok.clone(),
            });
        }
    });

    let tok = parse_tokens_until_tokenkind(
        tokens,
        &mut ops,
        vec![TokenKind::Marker(Marker::CloseBrace)],
    );

    ops.push(Op {
        kind: OpKind::Return,
        token: tok,
    });

    Function {
        name,
        token: name_tok,
        gen,
        sig,
        sig_idents,
        ops,
    }
}

fn start_if_ops(token: &Token, ops: &mut Vec<Op>) -> usize {
    let if_idx = ops.len();
    ops.push(Op {
        kind: OpKind::Nop(Keyword::If),
        token: token.clone(),
    });
    ops.push(Op {
        kind: OpKind::JumpCond(None),
        token: token.clone(),
    });
    ops.push(Op {
        kind: OpKind::StartBlock,
        token: token.clone(),
    });

    if_idx
}

fn make_ident_count(ops: &[Op], start_ip: usize) -> usize {
    let mut var_count = 0;
    let mut ip = start_ip;
    while ip < ops.len() {
        match ops[ip].kind {
            OpKind::MakeIdent(_) => {
                var_count += 1;
                ip += 1
            }
            OpKind::JumpCond(Some(n)) => ip = n,
            _ => ip += 1,
        }
    }

    var_count
}

fn close_if_block(token: &Token, ops: &mut Vec<Op>, if_idx: usize) -> usize {
    let var_count = make_ident_count(ops, if_idx);
    ops.push(Op {
        kind: OpKind::EndBlock(var_count),
        token: token.clone(),
    });
    ops.push(Op {
        kind: OpKind::Jump(None),
        token: token.clone(),
    });
    let jump_dest = ops.len();

    assert!(
        matches!(ops[if_idx + 1].kind, OpKind::JumpCond(None)),
        "Expected JumpCond(None), but found {:?}",
        ops[if_idx].kind
    );
    ops[if_idx + 1].kind = OpKind::JumpCond(Some(jump_dest));

    ops.push(Op {
        kind: OpKind::JumpDest(jump_dest),
        token: token.clone(),
    });

    jump_dest
}

fn if_block_to_ops(
    start_tok: &Token,
    tokens: &mut Vec<Token>,
    ops: &mut Vec<Op>,
) -> (Token, usize) {
    let if_idx = start_if_ops(start_tok, ops);
    let _tok = expect_token_kind(start_tok, tokens, TokenKind::Marker(Marker::OpenBrace));
    let tok =
        parse_tokens_until_tokenkind(tokens, ops, vec![TokenKind::Marker(Marker::CloseBrace)]);
    // Count how many times we push a variable onto the frame stack.
    (tok.clone(), close_if_block(&tok, ops, if_idx))
}

pub fn parse_if_block_from_tokens(
    token: &Token,
    tokens: &mut Vec<Token>,
    ops: &mut Vec<Op>,
) -> Token {
    let (tok, mut jump_dest) = if_block_to_ops(token, tokens, ops);

    while peek_token_kind(tokens, TokenKind::Keyword(Keyword::Else)) {
        let tok = expect_token_kind(&tok, tokens, TokenKind::Keyword(Keyword::Else));
        ops.push(Op {
            kind: OpKind::Nop(Keyword::Else),
            token: tok.clone(),
        });
        if peek_token_kind(tokens, TokenKind::Marker(Marker::OpenBrace)) {
            let tok = expect_token_kind(&tok, tokens, TokenKind::Marker(Marker::OpenBrace));
            let block_idx = ops.len();
            ops.push(Op {
                kind: OpKind::StartBlock,
                token: tok,
            });
            let tok = parse_tokens_until_tokenkind(
                tokens,
                ops,
                vec![TokenKind::Marker(Marker::CloseBrace)],
            );
            let var_count = make_ident_count(ops, block_idx);
            // ops.append(&mut block_ops);
            ops.push(Op {
                kind: OpKind::EndBlock(var_count),
                token: tok.clone(),
            });

            jump_dest = ops.len();

            ops.push(Op {
                kind: OpKind::JumpDest(jump_dest),
                token: tok,
            });
            break;
        } else {
            let tok =
                parse_tokens_until_tokenkind(tokens, ops, vec![TokenKind::Keyword(Keyword::If)]);

            let if_idx = start_if_ops(&tok, ops);
            let _tok = expect_token_kind(&tok, tokens, TokenKind::Marker(Marker::OpenBrace));
            let tok = parse_tokens_until_tokenkind(
                tokens,
                ops,
                vec![TokenKind::Marker(Marker::CloseBrace)],
            );
            // Count how many times we push a variable onto the frame stack.
            jump_dest = close_if_block(&tok, ops, if_idx);
        }
    }

    ops.iter_mut()
        .filter(|op| matches!(op.kind, OpKind::Jump(None)))
        .for_each(|op| op.kind = OpKind::Jump(Some(jump_dest)));

    tok
}

pub fn parse_while_block_from_tokens(token: &Token, tokens: &mut Vec<Token>, ops: &mut Vec<Op>) {
    ops.push(Op {
        kind: OpKind::Nop(Keyword::While),
        token: token.clone(),
    });
    let loop_dest = ops.len();
    ops.push(Op {
        kind: OpKind::JumpDest(loop_dest),
        token: token.clone(),
    });
    let tok = parse_tokens_until_tokenkind(tokens, ops, vec![TokenKind::Marker(Marker::OpenBrace)]);
    let cond_jump_loc = ops.len();
    ops.push(Op {
        kind: OpKind::JumpCond(None),
        token: tok.clone(),
    });
    ops.push(Op {
        kind: OpKind::StartBlock,
        token: tok,
    });

    let tok =
        parse_tokens_until_tokenkind(tokens, ops, vec![TokenKind::Marker(Marker::CloseBrace)]);

    let var_count = make_ident_count(ops, cond_jump_loc + 1);
    ops.push(Op {
        kind: OpKind::EndBlock(var_count),
        token: tok.clone(),
    });
    ops.push(Op {
        kind: OpKind::Jump(Some(loop_dest)),
        token: tok.clone(),
    });
    let end_loc = ops.len();
    ops.push(Op {
        kind: OpKind::JumpDest(end_loc),
        token: tok,
    });

    assert!(matches!(ops[cond_jump_loc].kind, OpKind::JumpCond(None)));
    ops[cond_jump_loc].kind = OpKind::JumpCond(Some(end_loc));
}

fn parse_struct_from_tokens(
    start_tok: &Token,
    tokens: &mut Vec<Token>,
    type_map: &HashMap<String, Type>,
) -> (String, Type) {
    let tok = expect_token_kind(start_tok, tokens, TokenKind::Keyword(Keyword::Struct));
    let (name_tok, name) = expect_word(&tok, tokens);
    let tok = expect_token_kind(&name_tok, tokens, TokenKind::Marker(Marker::OpenBrace));
    let (members, idents) = parse_maybe_tagged_type_list_from_tokens(&tok, tokens, type_map);
    let _tok = expect_token_kind(&name_tok, tokens, TokenKind::Marker(Marker::CloseBrace));

    // TODO: Should all/none of the types have to be annotated?
    (
        name.clone(),
        Type::StructType {
            name,
            members,
            idents,
        },
    )
}

pub fn hay_into_ir<P: AsRef<std::path::Path> + std::fmt::Display + Clone>(
    input_path: P,
) -> Program {
    let file = fs::read_to_string(input_path.clone()).unwrap();

    let mut lexer = LogosToken::lexer(file.as_str());
    let mut tokens: Vec<Token> = vec![];
    while let Some(token) = unsafe { into_token(&mut lexer, input_path.clone()) } {
        if !matches!(
            token,
            Token {
                kind: TokenKind::Comment(_),
                ..
            }
        ) {
            tokens.push(token);
        }
    }
    tokens.reverse();

    let mut program = Program::new();
    loop {
        let maybe_tok = tokens.last();
        match maybe_tok {
            Some(Token {
                kind: TokenKind::Keyword(Keyword::Function),
                ..
            }) => program.functions.push(parse_function_from_tokens(
                &maybe_tok.unwrap().clone(),
                &mut tokens,
                &program.types,
            )),
            Some(Token {
                kind: TokenKind::Keyword(Keyword::Struct),
                ..
            }) => {
                let (name, typ) = parse_struct_from_tokens(
                    &maybe_tok.unwrap().clone(),
                    &mut tokens,
                    &program.types,
                );
                program.types.insert(name, typ);
            }
            Some(tok) => compiler_error(tok, format!("Unexpected token {}", tok).as_str(), vec![]),
            None => break,
        }
    }

    program
}
