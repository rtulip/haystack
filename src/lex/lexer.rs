use crate::compiler::compiler_error;
use crate::ir::{
    function::Function,
    keyword::Keyword,
    literal::Literal,
    marker::Marker,
    op::{Op, OpKind},
    operator::Operator,
    program::Program,
    token::{Loc, Token, TokenKind},
    types::{Signature, Type},
};
use crate::lex::logos_lex::{into_token, LogosToken};
use logos::Logos;
use std::collections::HashMap;
use std::fs;

fn escape_string(unescaped: &str) -> String {
    let mut escaped = String::new();
    let bytes = unescaped.as_bytes();
    let mut idx = 0;
    while idx < bytes.len() {
        match char::from(bytes[idx]) {
            '\\' => {
                if idx + 1 < unescaped.len() {
                    idx += 1;
                    match char::from(bytes[idx]) {
                        'n' => escaped.push('\n'),
                        _ => unimplemented!("Only `\\n` is implemented yet"),
                    }
                } else {
                    panic!("Unescaping failed...");
                }
            }
            c => escaped.push(c),
        }
        idx += 1
    }
    escaped
}

fn parse_tokens_until_tokenkind(
    start_tok: &Token,
    tokens: &mut Vec<Token>,
    ops: &mut Vec<Op>,
    type_map: &HashMap<String, Type>,
    string_list: &mut Vec<String>,
    break_on: Vec<TokenKind>,
) -> Token {
    while let Some(token) = tokens.pop() {
        if break_on.contains(&token.kind) {
            return token;
        }

        match token.kind {
            TokenKind::Keyword(Keyword::Var) => {
                let (_tok, mut idents) = parse_var(&token, tokens);
                ops.append(&mut idents);
            }
            TokenKind::Keyword(Keyword::Cast) => {
                let op = parse_cast(&token, tokens, type_map);
                ops.push(op);
            }
            TokenKind::Keyword(Keyword::Syscall) => {
                ops.push(parse_syscall(&token, tokens));
            }
            TokenKind::Keyword(Keyword::Split) => ops.push(Op {
                kind: OpKind::Split,
                token: token.clone(),
            }),
            TokenKind::Keyword(Keyword::If) => {
                let _tok = parse_if_block(&token, tokens, ops, type_map, string_list);
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
                let _tok = parse_while_block(&token, tokens, ops, type_map, string_list);
            }
            TokenKind::Keyword(Keyword::Include) => {
                panic!("Include keyword can't be converted into ops")
            }
            TokenKind::Comment(_) => (),
            TokenKind::Operator(_) => ops.push(Op::from(token.clone())),
            TokenKind::Literal(Literal::String(ref s)) => {
                ops.push(Op {
                    kind: OpKind::PushString(string_list.len()),
                    token: token.clone(),
                });
                string_list.push(escape_string(s));
            }
            TokenKind::Literal(_) => ops.push(Op::from(token.clone())),
            TokenKind::Marker(_) => panic!("Markers shouldn't be converted into ops..."),
            TokenKind::Word(ref s) => {
                if peek_token_kind(tokens, TokenKind::Marker(Marker::DoubleColon)) {
                    let mut tok = token.clone();
                    let mut inner = vec![];
                    while peek_token_kind(tokens, TokenKind::Marker(Marker::DoubleColon)) {
                        let marker_tok =
                            expect_token_kind(&tok, tokens, TokenKind::Marker(Marker::DoubleColon));
                        let (word_tok, field) = expect_word(&marker_tok, tokens);
                        tok = word_tok;
                        inner.push(field);
                    }

                    ops.push(Op {
                        kind: OpKind::Ident(s.clone(), inner),
                        token: token.clone(),
                    })
                } else {
                    ops.push(Op::from(token.clone()))
                }
            }
            TokenKind::EndOfFile => panic!("End of file shouldn't be converted into an op."),
            // _ => ops.push(Op::from(token.clone())),
        }
    }
    compiler_error(
        start_tok,
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

fn expect_string_literal(prev_tok: &Token, tokens: &mut Vec<Token>) -> (Token, String) {
    if let Some(token) = tokens.pop() {
        let s = match &token.kind {
            TokenKind::Literal(Literal::String(s)) => s.clone(),
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
fn expect_u64(prev_tok: &Token, tokens: &mut Vec<Token>) -> (Token, u64) {
    if let Some(token) = tokens.pop() {
        let x = match &token.kind {
            TokenKind::Literal(Literal::Int(x)) => *x,
            _ => compiler_error(
                &token,
                format!("Expected a word, but found {:?} instead", token.kind).as_str(),
                vec![],
            ),
        };
        (token, x)
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

fn parse_annotation_list(
    start_tok: &Token,
    tokens: &mut Vec<Token>,
    type_map: &HashMap<String, Type>,
) -> (Token, Option<Vec<Type>>) {
    if peek_token_kind(tokens, TokenKind::Operator(Operator::LessThan)) {
        let mut tok = expect_token_kind(start_tok, tokens, TokenKind::Operator(Operator::LessThan));
        let mut annotations = vec![];
        while let Some((typ_tok, typ)) = parse_untagged_type(&tok, tokens, type_map) {
            tok = typ_tok;
            if !matches!(typ, Type::Placeholder { .. }) {
                compiler_error(
                    &tok,
                    "Type annotations cannot include known types",
                    vec![format!(
                        "Type {:?} is a known type, and not a valid generic parameter.",
                        typ
                    )
                    .as_str()],
                )
            }
            annotations.push(typ);
        }

        let tok = expect_token_kind(
            start_tok,
            tokens,
            TokenKind::Operator(Operator::GreaterThan),
        );

        if annotations.is_empty() {
            compiler_error(
                &tok,
                "Type Annotations cannot be empty",
                vec!["Consider removing the annotation list."],
            );
        }

        (tok, Some(annotations))
    } else {
        (start_tok.clone(), None)
    }
}

fn parse_type(
    start_tok: &Token,
    tokens: &mut Vec<Token>,
    type_map: &HashMap<String, Type>,
) -> (Token, Type) {
    if peek_token_kind(tokens, TokenKind::Operator(Operator::Mul)) {
        let tok = expect_token_kind(start_tok, tokens, TokenKind::Operator(Operator::Mul));
        let (tok, typ) = parse_type(&tok, tokens, type_map);

        return (tok, Type::Pointer { typ: Box::new(typ) });
    }

    let (tok, name) = expect_word(start_tok, tokens);
    let (tok, annotations) = parse_annotation_list(&tok, tokens, type_map);

    let typ = match type_map.get(&name) {
        Some(Type::GenericStructBase {
            name: base,
            members,
            idents,
            generics,
        }) => {
            if annotations.is_none() {
                compiler_error(
                    &tok,
                    format!("No type annotations provided for generic struct {base}").as_str(),
                    vec![format!("Expected annotations for {:?}", generics).as_str()],
                );
            }

            let annotations = annotations.unwrap();

            if generics.len() != annotations.len() {
                compiler_error(
                    &tok,
                    "Incorrect number of generic parameters provided",
                    vec![
                        format!("{base} is generic over {:?}", generics).as_str(),
                        format!("These annotations were provided: {:?}", annotations).as_str(),
                    ],
                );
            }
            Type::GenericStructInstance {
                base: name.clone(),
                members: members.clone(),
                idents: idents.clone(),
                alias_list: annotations,
                base_generics: generics.clone(),
            }
        }
        Some(t) => t.clone(),
        None => Type::Placeholder { name },
    };

    (tok, typ)
}

fn parse_tagged_type(
    start_tok: &Token,
    tokens: &mut Vec<Token>,
    type_map: &HashMap<String, Type>,
) -> Option<(Token, Type, String)> {
    if peek_word(tokens) || peek_token_kind(tokens, TokenKind::Operator(Operator::Mul)) {
        let (tok, typ) = parse_type(start_tok, tokens, type_map);
        let tok = expect_token_kind(&tok, tokens, TokenKind::Marker(Marker::Colon));
        let (tok, ident) = expect_word(&tok, tokens);

        Some((tok, typ, ident))
    } else {
        None
    }
}

fn parse_maybe_tagged_type(
    start_tok: &Token,
    tokens: &mut Vec<Token>,
    type_map: &HashMap<String, Type>,
) -> Option<(Token, Type, Option<String>)> {
    if peek_word(tokens) || peek_token_kind(tokens, TokenKind::Operator(Operator::Mul)) {
        let (tok, typ) = parse_type(start_tok, tokens, type_map);
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

fn parse_untagged_type(
    start_tok: &Token,
    tokens: &mut Vec<Token>,
    type_map: &HashMap<String, Type>,
) -> Option<(Token, Type)> {
    if peek_word(tokens) || peek_token_kind(tokens, TokenKind::Operator(Operator::Mul)) {
        Some(parse_type(start_tok, tokens, type_map))
    } else {
        None
    }
}

fn parse_tagged_type_list(
    token: &Token,
    tokens: &mut Vec<Token>,
    type_map: &HashMap<String, Type>,
) -> (Vec<Type>, Vec<String>) {
    let mut tok = token.clone();
    let mut inputs = vec![];
    let mut idents = vec![];
    while let Some((typ_tok, typ, ident)) = parse_tagged_type(&tok, tokens, type_map) {
        inputs.push(typ);
        idents.push(ident);
        tok = typ_tok;
    }
    (inputs, idents)
}

fn parse_maybe_tagged_type_list(
    token: &Token,
    tokens: &mut Vec<Token>,
    type_map: &HashMap<String, Type>,
) -> (Vec<Type>, Vec<Option<String>>) {
    let mut tok = token.clone();
    let mut inputs = vec![];
    let mut idents = vec![];
    while let Some((typ_tok, typ, ident)) = parse_maybe_tagged_type(&tok, tokens, type_map) {
        inputs.push(typ);
        idents.push(ident);
        tok = typ_tok;
    }
    (inputs, idents)
}

fn parse_function_inputs(
    start_tok: &Token,
    tokens: &mut Vec<Token>,
    type_map: &HashMap<String, Type>,
) -> (Token, Vec<Type>, Vec<Option<String>>) {
    let tok = expect_token_kind(start_tok, tokens, TokenKind::Marker(Marker::OpenParen));
    let (inputs, idents) = parse_maybe_tagged_type_list(&tok, tokens, type_map);
    let tok = expect_token_kind(&tok, tokens, TokenKind::Marker(Marker::CloseParen));
    (tok, inputs, idents)
}

fn parse_function_outputs(
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
    while let Some((typ_tok, typ)) = parse_untagged_type(&tok, tokens, type_map) {
        outputs.push(typ);
        tok = typ_tok;
    }
    let tok = expect_token_kind(&tok, tokens, TokenKind::Marker(Marker::CloseBracket));

    Some((tok, outputs))
}

fn parse_signature(
    start_tok: &Token,
    tokens: &mut Vec<Token>,
    type_map: &HashMap<String, Type>,
) -> (Token, Signature, Vec<Option<String>>) {
    let (tok, inputs, idents) = parse_function_inputs(start_tok, tokens, type_map);
    if let Some((tok, outputs)) = parse_function_outputs(&tok, tokens, type_map) {
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

fn parse_word_list(start_tok: &Token, tokens: &mut Vec<Token>) -> (Token, Vec<(Token, String)>) {
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

fn parse_syscall(start_tok: &Token, tokens: &mut Vec<Token>) -> Op {
    let tok = expect_token_kind(start_tok, tokens, TokenKind::Marker(Marker::OpenParen));
    let (tok, x) = expect_u64(&tok, tokens);
    if x > 6 {
        compiler_error(start_tok, "Syscall's can only accept up to 6 args", vec![]);
    }
    let _tok = expect_token_kind(&tok, tokens, TokenKind::Marker(Marker::CloseParen));

    Op {
        kind: OpKind::Syscall(x),
        token: start_tok.clone(),
    }
}

fn parse_cast(start_tok: &Token, tokens: &mut Vec<Token>, type_map: &HashMap<String, Type>) -> Op {
    let tok = expect_token_kind(start_tok, tokens, TokenKind::Marker(Marker::OpenParen));
    let (tok, typ) = parse_type(&tok, tokens, type_map);

    let _tok = expect_token_kind(&tok, tokens, TokenKind::Marker(Marker::CloseParen));

    Op {
        kind: OpKind::Cast(typ),
        token: start_tok.clone(),
    }
}

fn parse_var(start_tok: &Token, tokens: &mut Vec<Token>) -> (Token, Vec<Op>) {
    let (tok, idents) = parse_word_list(start_tok, tokens);
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
                kind: OpKind::MakeIdent {
                    ident: ident.clone(),
                    size: None,
                },
                token: token.clone(),
            })
            .collect(),
    )
}

fn parse_function(
    start_tok: &Token,
    tokens: &mut Vec<Token>,
    type_map: &HashMap<String, Type>,
    string_list: &mut Vec<String>,
) -> Function {
    let t = expect_token_kind(start_tok, tokens, TokenKind::Keyword(Keyword::Function));
    let (name_tok, name) = expect_word(&t, tokens);
    let (tok, gen) = parse_annotation_list(&name_tok, tokens, type_map);
    let gen = gen.unwrap_or_default();
    let (tok, sig, sig_idents) = parse_signature(&tok, tokens, type_map);

    sig.inputs.iter().for_each(|i| {
        if matches!(i, Type::Placeholder { name: _ }) && !gen.contains(i) {
            compiler_error(
                &tok,
                format!("Unrecognized type: `{:?}`", i).as_str(),
                vec![format!("Consider adding `{:?}` to the generic list", i).as_str()],
            )
        }
    });

    let tok = expect_token_kind(&tok, tokens, TokenKind::Marker(Marker::OpenBrace));
    let mut ops = vec![Op {
        kind: OpKind::PrepareFunc,
        token: name_tok.clone(),
    }];

    sig_idents.iter().rev().for_each(|maybe_ident| {
        if let Some(ident) = maybe_ident {
            ops.push(Op {
                kind: OpKind::MakeIdent {
                    ident: ident.clone(),
                    size: None,
                },
                token: name_tok.clone(),
            });
        }
    });

    let tok = parse_tokens_until_tokenkind(
        &tok,
        tokens,
        &mut ops,
        type_map,
        string_list,
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
            OpKind::MakeIdent { .. } => {
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
    type_map: &HashMap<String, Type>,
    string_list: &mut Vec<String>,
) -> (Token, usize) {
    let if_idx = start_if_ops(start_tok, ops);
    let tok = expect_token_kind(start_tok, tokens, TokenKind::Marker(Marker::OpenBrace));
    let tok = parse_tokens_until_tokenkind(
        &tok,
        tokens,
        ops,
        type_map,
        string_list,
        vec![TokenKind::Marker(Marker::CloseBrace)],
    );
    // Count how many times we push a variable onto the frame stack.
    (tok.clone(), close_if_block(&tok, ops, if_idx))
}

pub fn parse_if_block(
    token: &Token,
    tokens: &mut Vec<Token>,
    ops: &mut Vec<Op>,
    type_map: &HashMap<String, Type>,
    string_list: &mut Vec<String>,
) -> Token {
    let (tok, mut jump_dest) = if_block_to_ops(token, tokens, ops, type_map, string_list);

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
                token: tok.clone(),
            });
            let tok = parse_tokens_until_tokenkind(
                &tok,
                tokens,
                ops,
                type_map,
                string_list,
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
            let tok = parse_tokens_until_tokenkind(
                &tok,
                tokens,
                ops,
                type_map,
                string_list,
                vec![TokenKind::Keyword(Keyword::If)],
            );

            let if_idx = start_if_ops(&tok, ops);
            let tok = expect_token_kind(&tok, tokens, TokenKind::Marker(Marker::OpenBrace));
            let tok = parse_tokens_until_tokenkind(
                &tok,
                tokens,
                ops,
                type_map,
                string_list,
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

pub fn parse_while_block(
    token: &Token,
    tokens: &mut Vec<Token>,
    ops: &mut Vec<Op>,
    type_map: &HashMap<String, Type>,
    string_list: &mut Vec<String>,
) {
    ops.push(Op {
        kind: OpKind::Nop(Keyword::While),
        token: token.clone(),
    });
    let loop_dest = ops.len();
    ops.push(Op {
        kind: OpKind::JumpDest(loop_dest),
        token: token.clone(),
    });
    let tok = parse_tokens_until_tokenkind(
        token,
        tokens,
        ops,
        type_map,
        string_list,
        vec![TokenKind::Marker(Marker::OpenBrace)],
    );
    let cond_jump_loc = ops.len();
    ops.push(Op {
        kind: OpKind::JumpCond(None),
        token: tok.clone(),
    });
    ops.push(Op {
        kind: OpKind::StartBlock,
        token: tok.clone(),
    });

    let tok = parse_tokens_until_tokenkind(
        &tok,
        tokens,
        ops,
        type_map,
        string_list,
        vec![TokenKind::Marker(Marker::CloseBrace)],
    );

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

fn parse_struct(
    start_tok: &Token,
    tokens: &mut Vec<Token>,
    type_map: &HashMap<String, Type>,
) -> (String, Type) {
    let tok = expect_token_kind(start_tok, tokens, TokenKind::Keyword(Keyword::Struct));
    let (name_tok, name) = expect_word(&tok, tokens);
    let (tok, generics) = parse_annotation_list(&name_tok, tokens, type_map);
    let tok = expect_token_kind(&tok, tokens, TokenKind::Marker(Marker::OpenBrace));
    let (members, idents) = parse_tagged_type_list(&tok, tokens, type_map);
    let _tok = expect_token_kind(&name_tok, tokens, TokenKind::Marker(Marker::CloseBrace));

    if let Some(gen) = generics {
        (
            name.clone(),
            Type::GenericStructBase {
                name,
                members,
                idents,
                generics: gen,
            },
        )
    } else {
        (
            name.clone(),
            Type::Struct {
                name,
                members,
                idents,
            },
        )
    }
}

pub fn hay_into_ir<P: AsRef<std::path::Path> + std::fmt::Display + Clone>(
    input_path: P,
    program: &mut Program,
) {
    let file = fs::read_to_string(input_path.clone()).unwrap();

    let mut loc = Loc {
        file: input_path.to_string(),
        row: 1,
        col: 1,
    };
    let mut lexer = LogosToken::lexer(file.as_str());
    let mut tokens: Vec<Token> = vec![];
    while let Some(token) = into_token(&mut lexer, &mut loc) {
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

    loop {
        let maybe_tok = tokens.last();
        match maybe_tok {
            Some(Token {
                kind: TokenKind::Keyword(Keyword::Function),
                ..
            }) => program.functions.push(parse_function(
                &maybe_tok.unwrap().clone(),
                &mut tokens,
                &program.types,
                &mut program.strings,
            )),
            Some(Token {
                kind: TokenKind::Keyword(Keyword::Include),
                ..
            }) => {
                let token = expect_token_kind(
                    &maybe_tok.unwrap().clone(),
                    &mut tokens,
                    TokenKind::Keyword(Keyword::Include),
                );
                let (tok, path) = expect_string_literal(&token, &mut tokens);
                let mut include_program = Program::new();
                let path = if std::path::Path::new(&path).exists() {
                    path
                } else if std::path::Path::new(&format!("src/libs/{path}")).exists() {
                    format!("src/libs/{path}")
                } else {
                    compiler_error(&tok, "Cannot find file: {path}", vec![]);
                };
                hay_into_ir(path, &mut include_program);
                include_program.functions.drain(..).for_each(|mut func| {
                    func.ops.iter_mut().for_each(|op| {
                        if let OpKind::PushString(n) = &op.kind {
                            op.kind = OpKind::PushString(n + program.strings.len())
                        }
                    });

                    program.functions.push(func);
                });
                include_program.types.drain().for_each(|(id, t)| {
                    program.types.entry(id).or_insert(t);
                });
                include_program.strings.drain(..).for_each(|s| {
                    program.strings.push(s);
                });
            }
            Some(Token {
                kind: TokenKind::Keyword(Keyword::Struct),
                ..
            }) => {
                let (name, typ) =
                    parse_struct(&maybe_tok.unwrap().clone(), &mut tokens, &program.types);
                program.types.insert(name, typ);
            }
            Some(tok) => compiler_error(tok, format!("Unexpected token {}", tok).as_str(), vec![]),
            None => break,
        }
    }
}
