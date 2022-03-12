use crate::compiler::compiler_error;
use crate::ir::{
    function::Function,
    keyword::Keyword,
    marker::Marker,
    op::{Op, OpKind},
    operator::Operator,
    token::{eof_tok, Token, TokenKind},
    types::{Signature, Type},
    Program,
};
use crate::lex::logos_lex::{into_token, LogosToken};
use logos::Logos;
use std::fs;

fn parse_tokens_until_tokenkind(
    tokens: &mut Vec<Token>,
    break_on: Vec<TokenKind>,
) -> (Token, Vec<Op>) {
    let mut ops: Vec<Op> = vec![];
    while let Some(token) = tokens.pop() {
        if break_on.contains(&token.kind) {
            return (token, ops);
        }

        match &token.kind {
            &TokenKind::Keyword(Keyword::Var) => {
                let (_tok, mut idents) = parse_var_from_tokens(&token, tokens);
                ops.append(&mut idents);
            }
            &TokenKind::Keyword(Keyword::If) => {
                let (_tok, mut if_ops) = parse_if_block_from_tokens(&token, tokens, ops.len());
                ops.append(&mut if_ops);
            }
            _ => ops.push(Op::from(token.clone())),
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
            &prev_tok,
            format!("Expected {:?}, but found end of file instead", kind).as_str(),
            vec![],
        );
    }
}

fn peek_token_kind(tokens: &mut Vec<Token>, kind: TokenKind) -> bool {
    if let Some(token) = tokens.last() {
        if token.kind != kind {
            false
        } else {
            true
        }
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
            &prev_tok,
            "Expected a word, but found end of file instead",
            vec![],
        );
    }
}

fn peek_word(tokens: &Vec<Token>) -> bool {
    if let Some(token) = tokens.last() {
        match token.kind {
            TokenKind::Word(_) => true,
            _ => false,
        }
    } else {
        false
    }
}

fn parse_maybe_tagged_type_from_tokens(
    start_tok: &Token,
    tokens: &mut Vec<Token>,
) -> Option<(Token, Type)> {
    if peek_word(tokens) {
        let (tok, name) = expect_word(start_tok, tokens);
        if peek_token_kind(tokens, TokenKind::Marker(Marker::Colon)) {
            let tok = expect_token_kind(&tok, tokens, TokenKind::Marker(Marker::Colon));
            let (tok, ident) = expect_word(&tok, tokens);
            Some((
                tok,
                Type {
                    name,
                    ident: Some(ident),
                },
            ))
        } else {
            Some((tok, Type { name, ident: None }))
        }
    } else {
        None
    }
}

fn parse_untagged_type_from_tokens(
    start_tok: &Token,
    tokens: &mut Vec<Token>,
) -> Option<(Token, Type)> {
    if peek_word(tokens) {
        let (tok, name) = expect_word(start_tok, tokens);
        Some((tok, Type { name, ident: None }))
    } else {
        None
    }
}

fn parse_function_generics(
    start_tok: &Token,
    tokens: &mut Vec<Token>,
) -> Option<(Token, Vec<Type>)> {
    // Check to see if any generics were provided.
    if peek_token_kind(tokens, TokenKind::Marker(Marker::OpenParen)) {
        return None;
    }

    let mut tok = expect_token_kind(start_tok, tokens, TokenKind::Operator(Operator::LessThan));
    let mut gen = vec![];

    while let Some((typ_tok, typ)) = parse_untagged_type_from_tokens(&tok, tokens) {
        gen.push(typ);
        tok = typ_tok;
    }

    tok = expect_token_kind(&tok, tokens, TokenKind::Operator(Operator::GreaterThan));
    Some((tok, gen))
}

fn parse_function_inputs_from_tokens(
    start_tok: &Token,
    tokens: &mut Vec<Token>,
) -> (Token, Vec<Type>) {
    let mut tok = expect_token_kind(start_tok, tokens, TokenKind::Marker(Marker::OpenParen));
    let mut inputs = vec![];
    while let Some((typ_tok, typ)) = parse_maybe_tagged_type_from_tokens(&tok, tokens) {
        inputs.push(typ);
        tok = typ_tok;
    }
    let tok = expect_token_kind(&tok, tokens, TokenKind::Marker(Marker::CloseParen));
    (tok, inputs)
}

fn parse_function_outputs_from_tokens(
    start_tok: &Token,
    tokens: &mut Vec<Token>,
) -> Option<(Token, Vec<Type>)> {
    if !peek_token_kind(tokens, TokenKind::Marker(Marker::Arrow)) {
        return None;
    }

    let tok = expect_token_kind(start_tok, tokens, TokenKind::Marker(Marker::Arrow));
    let mut tok = expect_token_kind(&tok, tokens, TokenKind::Marker(Marker::OpenBracket));

    let mut outputs = vec![];
    while let Some((typ_tok, typ)) = parse_untagged_type_from_tokens(&tok, tokens) {
        outputs.push(typ);
        tok = typ_tok;
    }
    let tok = expect_token_kind(&tok, tokens, TokenKind::Marker(Marker::CloseBracket));

    Some((tok, outputs))
}

fn parse_signature_from_tokens(start_tok: &Token, tokens: &mut Vec<Token>) -> (Token, Signature) {
    let (tok, inputs) = parse_function_inputs_from_tokens(start_tok, tokens);
    if let Some((tok, outputs)) = parse_function_outputs_from_tokens(&tok, tokens) {
        (tok, Signature { inputs, outputs })
    } else {
        (
            tok,
            Signature {
                inputs,
                outputs: vec![],
            },
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
    let (tok, idents) = parse_word_list_from_tokens(&start_tok, tokens);
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

fn parse_function_from_tokens(start_tok: &Token, tokens: &mut Vec<Token>) -> Function {
    let t = expect_token_kind(start_tok, tokens, TokenKind::Keyword(Keyword::Function));
    let (name_tok, name) = expect_word(&t, tokens);
    let (tok, gen) =
        parse_function_generics(&name_tok, tokens).unwrap_or((name_tok.clone(), vec![]));
    let (tok, sig) = parse_signature_from_tokens(&tok, tokens);
    let _tok = expect_token_kind(&tok, tokens, TokenKind::Marker(Marker::OpenBrace));
    let (_tok, ops) =
        parse_tokens_until_tokenkind(tokens, vec![TokenKind::Marker(Marker::CloseBrace)]);

    Function {
        name,
        token: name_tok,
        gen,
        sig,
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

fn close_if_block(
    token: &Token,
    block_ops: &mut Vec<Op>,
    ops: &mut Vec<Op>,
    if_idx: usize,
    start_idx: usize,
) -> usize {
    let mut var_count = 0;
    block_ops.iter().for_each(|op| match op.kind {
        OpKind::MakeIdent(_) => var_count += 1,
        _ => (),
    });
    block_ops.push(Op {
        kind: OpKind::EndBlock(var_count),
        token: token.clone(),
    });
    block_ops.push(Op {
        kind: OpKind::Jump(None),
        token: token.clone(),
    });

    ops.append(block_ops);
    let jump_dest = ops.len() + start_idx;

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
    start_idx: usize,
) -> (Token, usize) {
    let if_idx = start_if_ops(start_tok, ops);
    let _tok = expect_token_kind(start_tok, tokens, TokenKind::Marker(Marker::OpenBrace));
    let (tok, mut block_ops) =
        parse_tokens_until_tokenkind(tokens, vec![TokenKind::Marker(Marker::CloseBrace)]);
    // Count how many times we push a variable onto the frame stack.
    (
        tok.clone(),
        close_if_block(&tok, &mut block_ops, ops, if_idx, start_idx),
    )
}

pub fn parse_if_block_from_tokens(
    token: &Token,
    tokens: &mut Vec<Token>,
    start_idx: usize,
) -> (Token, Vec<Op>) {
    let mut ops = vec![];
    let (tok, mut jump_dest) = if_block_to_ops(token, tokens, &mut ops, start_idx);

    while peek_token_kind(tokens, TokenKind::Keyword(Keyword::Else)) {
        let tok = expect_token_kind(&tok, tokens, TokenKind::Keyword(Keyword::Else));
        ops.push(Op {
            kind: OpKind::Nop(Keyword::Else),
            token: tok.clone(),
        });
        if peek_token_kind(tokens, TokenKind::Marker(Marker::OpenBrace)) {
            let tok = expect_token_kind(&tok, tokens, TokenKind::Marker(Marker::OpenBrace));
            ops.push(Op {
                kind: OpKind::StartBlock,
                token: tok.clone(),
            });
            let (tok, mut block_ops) =
                parse_tokens_until_tokenkind(tokens, vec![TokenKind::Marker(Marker::CloseBrace)]);
            let mut var_count = 0;
            block_ops.iter().for_each(|op| match op.kind {
                OpKind::MakeIdent(_) => var_count += 1,
                _ => (),
            });
            ops.append(&mut block_ops);
            ops.push(Op {
                kind: OpKind::EndBlock(var_count),
                token: tok.clone(),
            });

            jump_dest = ops.len() + start_idx;

            ops.push(Op {
                kind: OpKind::JumpDest(jump_dest),
                token: tok.clone(),
            });
            break;
        } else {
            let (tok, mut else_ops) =
                parse_tokens_until_tokenkind(tokens, vec![TokenKind::Keyword(Keyword::If)]);
            ops.append(&mut else_ops);

            let if_idx = start_if_ops(&tok, &mut ops);
            let _tok = expect_token_kind(&tok, tokens, TokenKind::Marker(Marker::OpenBrace));
            let (tok, mut block_ops) =
                parse_tokens_until_tokenkind(tokens, vec![TokenKind::Marker(Marker::CloseBrace)]);
            // Count how many times we push a variable onto the frame stack.
            jump_dest = close_if_block(&tok, &mut block_ops, &mut ops, if_idx, start_idx);
        }
    }

    ops.iter_mut()
        .filter(|op| matches!(op.kind, OpKind::Jump(None)))
        .for_each(|op| op.kind = OpKind::Jump(Some(jump_dest)));

    (tok, ops)
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

    let mut program = Program::default();
    loop {
        let maybe_tok = tokens.last();
        match maybe_tok {
            Some(Token {
                kind: TokenKind::Keyword(Keyword::Function),
                ..
            }) => program.functions.push(parse_function_from_tokens(
                &maybe_tok.unwrap().clone(),
                &mut tokens,
            )),
            Some(tok) => compiler_error(&tok, format!("Unexpected token {}", tok).as_str(), vec![]),
            None => break,
        }
    }

    program
}
