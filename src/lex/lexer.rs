use crate::compiler::compiler_error;
use crate::ir::{
    data::{InitData, UninitData},
    function::{Function, LocalVar},
    keyword::Keyword,
    literal::Literal,
    marker::Marker,
    op::{Op, OpKind},
    operator::Operator,
    program::Program,
    token::{Loc, Token, TokenKind},
    types::{Signature, Type, TypeName},
};
use crate::lex::logos_lex::{into_token, LogosToken};
use logos::Logos;
use std::collections::{BTreeMap, HashMap, HashSet};
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
                        '0' => escaped.push('\0'),
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
    type_map: &mut HashMap<String, Type>,
    init_data: &mut BTreeMap<String, InitData>,
    mut maybe_locals: Option<&mut BTreeMap<String, LocalVar>>,
    break_on: Vec<TokenKind>,
) -> Token {
    while let Some(token) = tokens.pop() {
        if break_on.contains(&token.kind) {
            return token;
        }

        match token.kind {
            TokenKind::Keyword(Keyword::As) => {
                parse_as(&token, tokens, ops, type_map, init_data);
            }
            TokenKind::Keyword(Keyword::Var) => {
                if let Some(ref mut locals) = maybe_locals {
                    parse_local_var(start_tok, tokens, type_map, *locals);
                } else {
                    compiler_error(
                        start_tok,
                        "Local variables arent supported within this context!",
                        vec![],
                    );
                }
            }
            TokenKind::Keyword(Keyword::Cast) => {
                let op = parse_cast(&token, tokens, type_map);
                ops.push(op);
            }
            TokenKind::Keyword(Keyword::Syscall) => {
                ops.push(parse_syscall(&token, tokens));
            }
            TokenKind::Keyword(Keyword::SizeOf) => {
                ops.push(parse_size_of(&token, tokens, type_map));
            }
            TokenKind::Keyword(Keyword::Split) => ops.push(Op {
                kind: OpKind::Split,
                token: token.clone(),
            }),
            TokenKind::Keyword(Keyword::If) => {
                let _tok = parse_if_block(&token, tokens, ops, type_map, init_data);
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
            TokenKind::Keyword(Keyword::Union) => {
                panic!("Union keyword can't be converted into ops")
            }
            TokenKind::Keyword(Keyword::Enum) => {
                panic!("Union keyword can't be converted into ops")
            }
            TokenKind::Keyword(Keyword::While) => {
                let _tok = parse_while_block(&token, tokens, ops, type_map, init_data);
            }
            TokenKind::Keyword(Keyword::Include) => {
                panic!("Include keyword can't be converted into ops")
            }
            TokenKind::Comment(_) => (),
            TokenKind::Operator(_) => ops.push(Op::from(token.clone())),
            TokenKind::Literal(Literal::String(ref s)) => {
                let ident = format!("str_{}", init_data.len());
                init_data.insert(ident.clone(), InitData::String(escape_string(s)));
                ops.push(Op {
                    kind: OpKind::PushString(ident),
                    token: token.clone(),
                });
            }
            TokenKind::Literal(_) => ops.push(Op::from(token.clone())),
            TokenKind::Marker(_) => {
                panic!("Markers shouldn't be converted into ops... {:?}", token)
            }
            TokenKind::Word(ref s) => {
                if peek_token_kind(tokens, TokenKind::Marker(Marker::DoubleColon)) {
                    let mut tok = token.clone();
                    let mut inner = vec![];
                    let mut annotations: Option<Vec<TypeName>> = None;
                    while peek_token_kind(tokens, TokenKind::Marker(Marker::DoubleColon)) {
                        let marker_tok =
                            expect_token_kind(&tok, tokens, TokenKind::Marker(Marker::DoubleColon));
                        if peek_token_kind(tokens, TokenKind::Operator(Operator::LessThan)) {
                            let (_tok, ann) = parse_annotation_list(&token, tokens, type_map);
                            annotations = ann;

                            if !inner.is_empty() {
                                compiler_error(&token, "This syntax isnt supported", vec![]);
                            }
                            break;
                        } else {
                            let (word_tok, field) = expect_word(&marker_tok, tokens);
                            tok = word_tok;
                            inner.push(field);
                        }
                    }

                    if let Some(annotations) = annotations {
                        ops.push(Op {
                            kind: OpKind::Call(s.clone(), annotations),
                            token: token.clone(),
                        })
                    } else {
                        ops.push(Op {
                            kind: OpKind::Ident(s.clone(), inner),
                            token: token.clone(),
                        })
                    }
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
                format!("Expected a u64, but found {:?} instead", token.kind).as_str(),
                vec![],
            ),
        };
        (token, x)
    } else {
        compiler_error(
            prev_tok,
            "Expected a u64, but found end of file instead",
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
    type_map: &mut HashMap<String, Type>,
) -> (Token, Option<Vec<TypeName>>) {
    if peek_token_kind(tokens, TokenKind::Operator(Operator::LessThan)) {
        let mut tok = expect_token_kind(start_tok, tokens, TokenKind::Operator(Operator::LessThan));
        let mut annotations = vec![];
        while let Some((typ_tok, typ, array_n)) = parse_untagged_type(&tok, tokens, type_map) {
            if array_n.is_some() {
                compiler_error(
                    &typ_tok,
                    "Cannot have array values in annotation list.",
                    vec![],
                );
            }
            tok = typ_tok;
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
    type_map: &mut HashMap<TypeName, Type>,
) -> (Token, TypeName, Option<u64>) {
    if peek_token_kind(tokens, TokenKind::Operator(Operator::Mul)) {
        let tok = expect_token_kind(start_tok, tokens, TokenKind::Operator(Operator::Mul));
        let (tok, typ, array_n) = parse_type(&tok, tokens, type_map);
        let t = Type::Pointer { typ };
        type_map.insert(t.name(), t.clone());
        return (tok, t.name(), array_n);
    }

    let (tok, name) = expect_word(start_tok, tokens);
    let (tok, annotations) = parse_annotation_list(&tok, tokens, type_map);

    let typ_name: TypeName = match type_map.get(&name) {
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

            let mut annotations = annotations.unwrap();

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
            if annotations
                .iter()
                .any(|t| type_map.get(t).unwrap().is_generic(type_map))
            {
                let t = Type::GenericStructInstance {
                    base: name.clone(),
                    members: members.clone(),
                    idents: idents.clone(),
                    alias_list: annotations,
                    base_generics: generics.clone(),
                };
                type_map.insert(t.name(), t.clone());
                t.name()
            } else {
                let generic_map: HashMap<String, TypeName> = HashMap::from_iter(
                    generics
                        .iter()
                        .map(|t| t.clone())
                        .zip(annotations.drain(..)),
                );
                Type::assign_generics(start_tok, &name, &generic_map, type_map)
            }
        }
        Some(Type::GenericUnionBase {
            name: base,
            members,
            idents,
            generics,
        }) => {
            if annotations.is_none() {
                compiler_error(
                    &tok,
                    format!("No type annotations provided for generic union {base}").as_str(),
                    vec![format!("Expected annotations for {:?}", generics).as_str()],
                );
            }

            let mut annotations = annotations.unwrap();

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
            if annotations
                .iter()
                .any(|t| type_map.get(t).unwrap().is_generic(type_map))
            {
                let t = Type::GenericUnionInstance {
                    base: name.clone(),
                    members: members.clone(),
                    idents: idents.clone(),
                    alias_list: annotations,
                    base_generics: generics.clone(),
                };
                type_map.insert(t.name(), t.clone());
                t.name()
            } else {
                let generic_map: HashMap<TypeName, TypeName> = HashMap::from_iter(
                    generics
                        .iter()
                        .map(|t| t.clone())
                        .zip(annotations.drain(..)),
                );
                Type::assign_generics(start_tok, &name, &generic_map, type_map)
            }
        }
        Some(
            Type::U64
            | Type::U8
            | Type::Bool
            | Type::Enum { .. }
            | Type::Pointer { .. }
            | Type::Struct { .. }
            | Type::GenericStructInstance { .. }
            | Type::ResolvedStruct { .. }
            | Type::Union { .. }
            | Type::GenericUnionInstance { .. }
            | Type::ResolvedUnion { .. }
            | Type::Placeholder { .. },
        ) => name.clone(),
        None => {
            let t = Type::Placeholder { name };
            type_map.insert(t.name(), t.clone());
            t.name()
        }
    };

    if peek_token_kind(tokens, TokenKind::Marker(Marker::OpenBracket)) {
        expect_token_kind(&tok, tokens, TokenKind::Marker(Marker::OpenBracket));
        let (tok, size) = expect_u64(&tok, tokens);
        let tok = expect_token_kind(&tok, tokens, TokenKind::Marker(Marker::CloseBracket));
        (tok, typ_name, Some(size))
    } else {
        (tok, typ_name, None)
    }
}

fn parse_partial_type(
    start_tok: &Token,
    tokens: &mut Vec<Token>,
    type_map: &mut HashMap<String, Type>,
) -> (Token, TypeName) {
    if peek_token_kind(tokens, TokenKind::Operator(Operator::Mul)) {
        let tok = expect_token_kind(start_tok, tokens, TokenKind::Operator(Operator::Mul));
        let (tok, typ) = parse_partial_type(&tok, tokens, type_map);
        let t = Type::Pointer { typ };
        type_map.insert(t.name(), t.clone());
        return (tok, t.name());
    }

    let (tok, name) = expect_word(start_tok, tokens);
    let typ = match type_map.get(&name) {
        Some(t) => t.name(),
        None => {
            let t = Type::Placeholder { name };
            type_map.insert(t.name(), t.clone());
            t.name()
        }
    };

    (tok, typ)
}

fn parse_tagged_type(
    start_tok: &Token,
    tokens: &mut Vec<Token>,
    type_map: &mut HashMap<String, Type>,
) -> Option<(Token, String, TypeName, Option<u64>)> {
    if peek_word(tokens) || peek_token_kind(tokens, TokenKind::Operator(Operator::Mul)) {
        let (tok, typ, array_n) = parse_type(start_tok, tokens, type_map);
        let tok = expect_token_kind(&tok, tokens, TokenKind::Marker(Marker::Colon));
        let (tok, ident) = expect_word(&tok, tokens);
        Some((tok, ident, typ, array_n))
    } else {
        None
    }
}

fn parse_maybe_tagged_type(
    start_tok: &Token,
    tokens: &mut Vec<Token>,
    type_map: &mut HashMap<String, Type>,
) -> Option<(Token, Option<String>, TypeName, Option<u64>)> {
    if peek_word(tokens) || peek_token_kind(tokens, TokenKind::Operator(Operator::Mul)) {
        let (tok, typ, array_n) = parse_type(start_tok, tokens, type_map);
        if peek_token_kind(tokens, TokenKind::Marker(Marker::Colon)) {
            let tok = expect_token_kind(&tok, tokens, TokenKind::Marker(Marker::Colon));
            let (tok, ident) = expect_word(&tok, tokens);
            Some((tok, Some(ident), typ, array_n))
        } else {
            Some((tok, None, typ, array_n))
        }
    } else {
        None
    }
}

fn parse_untagged_type(
    start_tok: &Token,
    tokens: &mut Vec<Token>,
    type_map: &mut HashMap<String, Type>,
) -> Option<(Token, TypeName, Option<u64>)> {
    if peek_word(tokens) || peek_token_kind(tokens, TokenKind::Operator(Operator::Mul)) {
        Some(parse_type(start_tok, tokens, type_map))
    } else {
        None
    }
}

fn parse_tagged_type_list(
    token: &Token,
    tokens: &mut Vec<Token>,
    type_map: &mut HashMap<String, Type>,
    maybe_generics: &Option<Vec<TypeName>>,
) -> (Vec<TypeName>, Vec<String>) {
    let mut tok = token.clone();
    let mut inputs = vec![];
    let mut idents = vec![];
    while let Some((typ_tok, ident, typ, array_n)) = parse_tagged_type(&tok, tokens, type_map) {
        if array_n.is_some() {
            compiler_error(&typ_tok, "Cannot have array types in type list", vec![]);
        }
        let generics = type_map.get(&typ).unwrap().deep_check_generics(type_map);
        match (generics.is_empty(), maybe_generics) {
            (true, _) => (),
            (false, None) => compiler_error(
                &typ_tok,
                format!("Unrecognized generic types: {:?}", generics).as_str(),
                vec![
                    "No generics were expected in this context.",
                    "Consider adding a type annotation list.",
                ],
            ),
            (false, Some(known_generics)) => generics.iter().for_each(|t| {
                let mut all_known_generics = known_generics.clone();
                all_known_generics
                    .append(&mut type_map.get(t).unwrap().shallow_check_generics(type_map));
                if !all_known_generics.contains(t) {
                    compiler_error(
                        &typ_tok,
                        format!("Unrecognized generic types: {:?}", t).as_str(),
                        vec![
                            format!(
                                "Only these generics are known in this context: {:?}",
                                known_generics
                            )
                            .as_str(),
                            "Consider adding a type annotation list.",
                        ],
                    )
                }
            }),
        };

        inputs.push(typ);
        idents.push(ident);
        tok = typ_tok;
    }
    (inputs, idents)
}

fn parse_maybe_tagged_type_list(
    token: &Token,
    tokens: &mut Vec<Token>,
    type_map: &mut HashMap<String, Type>,
) -> (Vec<TypeName>, Vec<Option<String>>) {
    let mut tok = token.clone();
    let mut inputs = vec![];
    let mut idents = vec![];
    while let Some((typ_tok, ident, typ, array_n)) = parse_maybe_tagged_type(&tok, tokens, type_map)
    {
        if array_n.is_some() {
            compiler_error(&typ_tok, "Cannot have array types in type list", vec![]);
        }
        inputs.push(typ);
        idents.push(ident);
        tok = typ_tok;
    }
    (inputs, idents)
}

fn parse_function_inputs(
    start_tok: &Token,
    tokens: &mut Vec<Token>,
    type_map: &mut HashMap<String, Type>,
) -> (Token, Vec<TypeName>, Vec<Option<String>>) {
    let tok = expect_token_kind(start_tok, tokens, TokenKind::Marker(Marker::OpenParen));
    let (inputs, idents) = parse_maybe_tagged_type_list(&tok, tokens, type_map);
    let tok = expect_token_kind(&tok, tokens, TokenKind::Marker(Marker::CloseParen));
    (tok, inputs, idents)
}

fn parse_function_outputs(
    start_tok: &Token,
    tokens: &mut Vec<Token>,
    type_map: &mut HashMap<String, Type>,
) -> Option<(Token, Vec<TypeName>)> {
    if !peek_token_kind(tokens, TokenKind::Marker(Marker::Arrow)) {
        return None;
    }

    let tok = expect_token_kind(start_tok, tokens, TokenKind::Marker(Marker::Arrow));
    let mut tok = expect_token_kind(&tok, tokens, TokenKind::Marker(Marker::OpenBracket));

    let mut outputs = vec![];
    while let Some((typ_tok, typ, array_n)) = parse_untagged_type(&tok, tokens, type_map) {
        if array_n.is_some() {
            compiler_error(
                &typ_tok,
                "Cannot have array types in function outputs",
                vec![],
            );
        }
        outputs.push(typ);
        tok = typ_tok;
    }
    let tok = expect_token_kind(&tok, tokens, TokenKind::Marker(Marker::CloseBracket));

    Some((tok, outputs))
}

fn parse_signature(
    start_tok: &Token,
    tokens: &mut Vec<Token>,
    type_map: &mut HashMap<String, Type>,
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

fn parse_word_list(
    start_tok: &Token,
    tokens: &mut Vec<Token>,
    open: TokenKind,
    close: TokenKind,
) -> (Token, Vec<(Token, String)>) {
    let mut tok = expect_token_kind(start_tok, tokens, open);
    let mut words = vec![];

    while peek_word(tokens) {
        let (word_tok, word) = expect_word(&tok, tokens);
        tok = word_tok.clone();
        words.push((word_tok, word));
    }
    let tok = expect_token_kind(&tok, tokens, close);

    (tok, words)
}

fn parse_size_of(
    start_tok: &Token,
    tokens: &mut Vec<Token>,
    type_map: &mut HashMap<String, Type>,
) -> Op {
    let tok = expect_token_kind(start_tok, tokens, TokenKind::Marker(Marker::OpenParen));
    let (tok, typ, array_n) = parse_type(&tok, tokens, type_map);
    if array_n.is_some() {
        compiler_error(&tok, "Cannot have array values in sizeOf", vec![]);
    }
    let _tok = expect_token_kind(&tok, tokens, TokenKind::Marker(Marker::CloseParen));
    Op {
        kind: OpKind::SizeOf(typ),
        token: start_tok.clone(),
    }
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

fn parse_cast(
    start_tok: &Token,
    tokens: &mut Vec<Token>,
    type_map: &mut HashMap<String, Type>,
) -> Op {
    let tok = expect_token_kind(start_tok, tokens, TokenKind::Marker(Marker::OpenParen));
    let (typ_tok, typ) = parse_partial_type(&tok, tokens, type_map);
    let (tok, annotations) = parse_annotation_list(&typ_tok, tokens, type_map);
    let _tok = expect_token_kind(&tok, tokens, TokenKind::Marker(Marker::CloseParen));

    let typ = if let Type::GenericUnionBase {
        name,
        members,
        idents,
        generics,
    } = type_map.get(&typ).unwrap().clone()
    {
        if let Some(mut annotations) = annotations {
            if annotations.len() != generics.len() {
                compiler_error(
                    &typ_tok,
                    "Incorrect number of generic annotations provided",
                    vec![
                        format!("Generic Union {name} is geneic over {:?}", generics).as_str(),
                        format!("Found annotations: {:?}", annotations).as_str(),
                    ],
                )
            }

            if annotations
                .iter()
                .any(|t| matches!(type_map.get(t).unwrap(), Type::Placeholder { .. }))
            {
                let t = Type::GenericUnionInstance {
                    base: name.clone(),
                    members: members.clone(),
                    idents: idents.clone(),
                    alias_list: annotations,
                    base_generics: generics.clone(),
                };
                type_map.insert(t.name(), t.clone());
                t.name()
            } else {
                let generic_map: HashMap<TypeName, TypeName> = HashMap::from_iter(
                    generics
                        .iter()
                        .map(|t| t.clone())
                        .zip(annotations.drain(..)),
                );
                Type::assign_generics(start_tok, &name, &generic_map, type_map)
            }
        } else {
            compiler_error(
                &typ_tok,
                format!("Casting to generic union `{name}` requires type annotations.").as_str(),
                vec![format!("Annotations are required for: {:?}", generics).as_str()],
            )
        }
    } else {
        typ
    };

    Op {
        kind: OpKind::Cast(typ),
        token: start_tok.clone(),
    }
}

fn parse_as(
    start_tok: &Token,
    tokens: &mut Vec<Token>,
    ops: &mut Vec<Op>,
    type_map: &mut HashMap<String, Type>,
    init_data: &mut BTreeMap<String, InitData>,
) {
    let (tok, mut idents) = parse_word_list(
        start_tok,
        tokens,
        TokenKind::Marker(Marker::OpenBracket),
        TokenKind::Marker(Marker::CloseBracket),
    );
    if idents.is_empty() {
        compiler_error(
            &tok,
            "Must provide at least one identifier in `var` block.",
            vec![],
        )
    }
    let start_idx = ops.len();
    idents.drain(..).rev().for_each(|(token, ident)| {
        ops.push(Op {
            kind: OpKind::MakeIdent {
                ident: ident.clone(),
                size: None,
            },
            token: token.clone(),
        })
    });
    if peek_token_kind(tokens, TokenKind::Marker(Marker::OpenBrace)) {
        let tok = expect_token_kind(&tok, tokens, TokenKind::Marker(Marker::OpenBrace));
        ops.push(Op {
            kind: OpKind::StartBlock,
            token: tok.clone(),
        });

        let tok = parse_tokens_until_tokenkind(
            &tok,
            tokens,
            ops,
            type_map,
            init_data,
            None,
            vec![TokenKind::Marker(Marker::CloseBrace)],
        );

        ops.push(Op {
            kind: OpKind::EndBlock(make_ident_count(&ops, start_idx)),
            token: tok.clone(),
        });
    }
}

fn parse_function(
    start_tok: &Token,
    tokens: &mut Vec<Token>,
    type_map: &mut HashMap<String, Type>,
    init_data: &mut BTreeMap<String, InitData>,
) -> Function {
    let t = expect_token_kind(start_tok, tokens, TokenKind::Keyword(Keyword::Function));
    let (name_tok, name) = expect_word(&t, tokens);
    let (tok, generics) = parse_annotation_list(&name_tok, tokens, type_map);
    let generics = generics.unwrap_or_default();
    let (tok, sig, sig_idents) = parse_signature(&tok, tokens, type_map);

    sig.inputs.iter().for_each(|i| {
        if matches!(type_map.get(i).unwrap(), Type::Placeholder { name: _ })
            && !generics.contains(i)
        {
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

    let mut locals: BTreeMap<String, LocalVar> = BTreeMap::new();
    let tok = parse_tokens_until_tokenkind(
        &tok,
        tokens,
        &mut ops,
        type_map,
        init_data,
        Some(&mut locals),
        vec![TokenKind::Marker(Marker::CloseBrace)],
    );

    ops.push(Op {
        kind: OpKind::Return,
        token: tok,
    });

    Function {
        name,
        token: name_tok,
        generics,
        sig,
        ops,
        generics_map: HashMap::new(),
        locals,
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
            OpKind::EndBlock(n) => {
                var_count -= n;
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
    type_map: &mut HashMap<String, Type>,
    init_data: &mut BTreeMap<String, InitData>,
) -> (Token, usize) {
    let if_idx = start_if_ops(start_tok, ops);
    let tok = expect_token_kind(start_tok, tokens, TokenKind::Marker(Marker::OpenBrace));
    let tok = parse_tokens_until_tokenkind(
        &tok,
        tokens,
        ops,
        type_map,
        init_data,
        None,
        vec![TokenKind::Marker(Marker::CloseBrace)],
    );
    // Count how many times we push a variable onto the frame stack.
    (tok.clone(), close_if_block(&tok, ops, if_idx))
}

pub fn parse_if_block(
    token: &Token,
    tokens: &mut Vec<Token>,
    ops: &mut Vec<Op>,
    type_map: &mut HashMap<String, Type>,
    init_data: &mut BTreeMap<String, InitData>,
) -> Token {
    let (tok, mut jump_dest) = if_block_to_ops(token, tokens, ops, type_map, init_data);

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
                init_data,
                None,
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
                init_data,
                None,
                vec![TokenKind::Keyword(Keyword::If)],
            );

            let if_idx = start_if_ops(&tok, ops);
            let tok = expect_token_kind(&tok, tokens, TokenKind::Marker(Marker::OpenBrace));
            let tok = parse_tokens_until_tokenkind(
                &tok,
                tokens,
                ops,
                type_map,
                init_data,
                None,
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
    type_map: &mut HashMap<String, Type>,
    init_data: &mut BTreeMap<String, InitData>,
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
        init_data,
        None,
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
        init_data,
        None,
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

fn parse_enum(start_tok: &Token, tokens: &mut Vec<Token>) -> (String, Type) {
    let tok = expect_token_kind(start_tok, tokens, TokenKind::Keyword(Keyword::Enum));
    let (name_tok, name) = expect_word(&tok, tokens);
    let (_tok, mut variants) = parse_word_list(
        &name_tok,
        tokens,
        TokenKind::Marker(Marker::OpenBrace),
        TokenKind::Marker(Marker::CloseBrace),
    );
    (
        name.clone(),
        Type::Enum {
            name,
            variants: variants.drain(..).map(|(_tok, s)| s).collect(),
        },
    )
}

fn parse_union(
    start_tok: &Token,
    tokens: &mut Vec<Token>,
    type_map: &mut HashMap<String, Type>,
) -> (String, Type) {
    let tok = expect_token_kind(start_tok, tokens, TokenKind::Keyword(Keyword::Union));
    let (name_tok, name) = expect_word(&tok, tokens);
    let (tok, generics) = parse_annotation_list(&name_tok, tokens, type_map);
    let tok = expect_token_kind(&tok, tokens, TokenKind::Marker(Marker::OpenBrace));
    let (members, idents) = parse_tagged_type_list(&tok, tokens, type_map, &generics);
    let _tok = expect_token_kind(&name_tok, tokens, TokenKind::Marker(Marker::CloseBrace));

    if let Some(generics) = generics {
        (
            name.clone(),
            Type::GenericUnionBase {
                name,
                members,
                idents,
                generics,
            },
        )
    } else {
        (
            name.clone(),
            Type::Union {
                name,
                members,
                idents,
            },
        )
    }
}

fn parse_struct(
    start_tok: &Token,
    tokens: &mut Vec<Token>,
    type_map: &mut HashMap<String, Type>,
) -> (Token, String, Type) {
    let tok = expect_token_kind(start_tok, tokens, TokenKind::Keyword(Keyword::Struct));
    let (name_tok, name) = expect_word(&tok, tokens);
    let (tok, generics) = parse_annotation_list(&name_tok, tokens, type_map);

    let tok = expect_token_kind(&tok, tokens, TokenKind::Marker(Marker::OpenBrace));
    let (members, idents) = parse_tagged_type_list(&tok, tokens, type_map, &generics);
    let _tok = expect_token_kind(&name_tok, tokens, TokenKind::Marker(Marker::CloseBrace));

    if let Some(gen) = generics {
        (
            name_tok,
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
            name_tok,
            name.clone(),
            Type::Struct {
                name,
                members,
                idents,
            },
        )
    }
}

fn parse_local_var(
    token: &Token,
    tokens: &mut Vec<Token>,
    type_map: &mut HashMap<String, Type>,
    locals: &mut BTreeMap<String, LocalVar>,
) {
    if let Some((tok, ident, typ, array_n)) = parse_tagged_type(token, tokens, type_map) {
        if let Some(n) = array_n {
            let data_typ = Type::Pointer { typ: typ.clone() };
            type_map.insert(data_typ.name(), data_typ.clone());

            let data_local = LocalVar {
                typ: data_typ.name(),
                size: (type_map.get(&typ).unwrap().size(type_map)
                    * type_map.get(&typ).unwrap().width()) as u64
                    * n,
                value: None,
            };

            let arr_typ = Type::resolve_struct(
                &tok,
                &String::from("Arr"),
                &vec![Type::U64.name(), data_typ.name()],
                type_map,
            );

            let arr_ptr_typ = Type::Pointer {
                typ: arr_typ.clone(),
            };
            type_map.insert(arr_ptr_typ.name(), arr_ptr_typ.clone());
            let arr_local = LocalVar {
                typ: arr_ptr_typ.name(),
                size: (type_map.get(&arr_typ).unwrap().size(type_map)
                    * type_map.get(&arr_typ).unwrap().width()) as u64,
                value: Some(InitData::Arr {
                    size: n,
                    pointer: format!("{ident}_data"),
                }),
            };

            if locals.insert(format!("{ident}_data"), data_local).is_some() {
                compiler_error(
                    &tok,
                    format!("Local var {ident}_data has already been defined").as_str(),
                    vec![
                        format!("This is automatically generated with the array {ident}").as_str(),
                    ],
                )
            };

            if locals.insert(format!("{ident}"), arr_local).is_some() {
                compiler_error(
                    &tok,
                    format!("Local var {ident} has already been defined").as_str(),
                    vec![],
                )
            };
        } else {
            let pointer_typ = Type::Pointer { typ: typ.clone() };
            type_map.insert(pointer_typ.name(), pointer_typ.clone());
            let local = LocalVar {
                typ: pointer_typ.name(),
                size: (type_map.get(&typ).unwrap().size(type_map)
                    * type_map.get(&typ).unwrap().width()) as u64,
                value: None,
            };

            if locals.insert(format!("{ident}"), local).is_some() {
                compiler_error(
                    &tok,
                    format!("Local var {ident} has already been defined").as_str(),
                    vec![],
                )
            };
        }
    }
}

fn parse_global_var(
    token: &Token,
    tokens: &mut Vec<Token>,
    type_map: &mut HashMap<String, Type>,
    globals: &mut BTreeMap<String, (TypeName, String)>,
    init_data: &mut BTreeMap<String, InitData>,
    uninit_data: &mut BTreeMap<String, UninitData>,
) {
    let tok = expect_token_kind(token, tokens, TokenKind::Keyword(Keyword::Var));
    if let Some((tok, ident, typ, array_n)) = parse_tagged_type(&tok, tokens, type_map) {
        let global_ident = format!("global_{}", globals.len());
        let typ = if let Some(n) = array_n {
            let data_ptr = format!("data_{}", uninit_data.len());
            uninit_data.insert(
                data_ptr.clone(),
                UninitData::Region(type_map.get(&typ).unwrap().size(type_map) as u64 * n),
            );
            let t = Type::Pointer { typ };
            type_map.insert(t.name(), t.clone());
            let typ = Type::resolve_struct(
                &tok,
                &String::from("Arr"),
                &vec![Type::U64.name(), t.name()],
                type_map,
            );
            let typ = Type::Pointer { typ };
            type_map.insert(typ.name(), typ.clone());

            if init_data
                .insert(
                    global_ident.clone(),
                    InitData::Arr {
                        size: n,
                        pointer: data_ptr,
                    },
                )
                .is_some()
            {
                compiler_error(
                    &tok,
                    format!("Identifier `{ident}` has already been taken").as_str(),
                    vec![],
                );
            }
            typ.name()
        } else {
            uninit_data.insert(
                global_ident.clone(),
                UninitData::Region(type_map.get(&typ).unwrap().size(type_map) as u64),
            );
            let t = Type::Pointer { typ };
            type_map.insert(t.name(), t.clone());
            t.name()
        };
        if let Some(_t) = globals.insert(ident, (typ, global_ident)) {
            todo!("compiler_error");
        }
    } else {
        compiler_error(&tok, "Failed to parse type...", vec![]);
    }
}

pub fn hay_into_ir<P: AsRef<std::path::Path> + std::fmt::Display + Clone>(
    input_path: P,
    program: &mut Program,
    included_files: &mut HashSet<String>,
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
                &mut program.types,
                &mut program.init_data,
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
                // let mut include_program = Program::new();
                let path = if std::path::Path::new(&path).exists() {
                    path
                } else if std::path::Path::new(&format!("src/libs/{path}")).exists() {
                    format!("src/libs/{path}")
                } else {
                    compiler_error(&tok, "Cannot find file: {path}", vec![]);
                };
                let path = String::from(
                    std::path::Path::new(&path)
                        .canonicalize()
                        .unwrap()
                        .to_str()
                        .unwrap(),
                );
                if !included_files.contains(&path) {
                    hay_into_ir(&path, program, included_files);
                    included_files.insert(path);
                }
            }
            Some(Token {
                kind: TokenKind::Keyword(Keyword::Struct),
                ..
            }) => {
                let (tok, name, typ) =
                    parse_struct(&maybe_tok.unwrap().clone(), &mut tokens, &mut program.types);
                if let Some(t) = program.types.insert(name, typ) {
                    compiler_error(&tok, format!("Redefiniton of type: {}", t).as_str(), vec![]);
                }
            }
            Some(Token {
                kind: TokenKind::Keyword(Keyword::Union),
                ..
            }) => {
                let (name, typ) =
                    parse_union(&maybe_tok.unwrap().clone(), &mut tokens, &mut program.types);
                program.types.insert(name, typ);
            }
            Some(Token {
                kind: TokenKind::Keyword(Keyword::Enum),
                ..
            }) => {
                let (name, typ) = parse_enum(&maybe_tok.unwrap().clone(), &mut tokens);
                program.types.insert(name, typ);
            }
            Some(Token {
                kind: TokenKind::Keyword(Keyword::Var),
                ..
            }) => {
                parse_global_var(
                    &maybe_tok.unwrap().clone(),
                    &mut tokens,
                    &mut program.types,
                    &mut program.global_vars,
                    &mut program.init_data,
                    &mut program.uninit_data,
                );
            }
            Some(tok) => compiler_error(tok, format!("Unexpected token {}", tok).as_str(), vec![]),
            None => break,
        }
    }
}
