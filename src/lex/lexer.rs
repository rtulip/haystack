use crate::compiler::compiler_error;
use crate::ir::{
    eof_tok, Function, Keyword, Marker, Op, Operator, Program, Signature, Token, TokenKind, Type,
};
use crate::lex::logos_lex::{into_token, LogosToken};
use logos::Logos;
use std::fs;

fn parse_maybe_tagged_type_from_tokens(tokens: &mut Vec<Token>) -> Option<Type> {
    match tokens.last() {
        Some(Token {
            kind: TokenKind::Word(_),
            ..
        }) => (),
        _ => return None,
    };

    let name = match tokens.pop() {
        Some(Token {
            kind: TokenKind::Word(s),
            ..
        }) => s,
        _ => unreachable!(),
    };

    match tokens.last() {
        Some(Token {
            kind: TokenKind::Marker(Marker::Colon),
            ..
        }) => (),
        _ => return Some(Type { name, ident: None }),
    }
    tokens.pop();

    match tokens.last() {
        Some(Token {
            kind: TokenKind::Word(_),
            ..
        }) => (),
        Some(tok) => compiler_error(
            tok,
            format!("Expected Word, but found {} instead", tok).as_str(),
            vec![],
        ),
        None => compiler_error(
            &eof_tok(),
            "Expected `word`, but found end of file instead",
            vec![],
        ),
    }

    let ident = match tokens.pop() {
        Some(Token {
            kind: TokenKind::Word(s),
            ..
        }) => Some(s),
        _ => unreachable!(),
    };

    Some(Type { name, ident })
}

fn parse_untagged_type_from_tokens(tokens: &mut Vec<Token>) -> Option<Type> {
    match tokens.last() {
        Some(Token {
            kind: TokenKind::Word(_),
            ..
        }) => (),
        _ => return None,
    }

    match tokens.pop() {
        Some(Token {
            kind: TokenKind::Word(name),
            ..
        }) => Some(Type { name, ident: None }),
        _ => unreachable!(),
    }
}

fn parse_function_generics(tokens: &mut Vec<Token>) -> Vec<Type> {
    match tokens.last() {
        Some(Token {
            kind: TokenKind::Operator(Operator::LessThan),
            ..
        }) => (),
        Some(Token {
            kind: TokenKind::Marker(Marker::OpenParen),
            ..
        }) => return vec![],
        Some(t) => compiler_error(
            t,
            "Unexpected token in function definition.",
            vec![
                format!("Found: {}", t).as_str(),
                "Expected either a generic delcaration: `<...>` or function parameters: `(...)`",
            ],
        ),
        None => compiler_error(
            &eof_tok(),
            "Unexpected token in function definition.",
            vec![
                "Found end of file",
                "Expected either a generic delcaration: `<...>` or function parameters: `(...)`",
            ],
        ),
    }
    tokens.pop();

    let mut gen = vec![];

    while let Some(typ) = parse_untagged_type_from_tokens(tokens) {
        gen.push(typ);
    }

    match tokens.last() {
        Some(Token {
            kind: TokenKind::Operator(Operator::GreaterThan),
            ..
        }) => (),
        Some(t) => compiler_error(
            t,
            "Unexpected token in generic list.",
            vec![format!("Found: {}", t).as_str(), "Expected: `>`"],
        ),
        None => compiler_error(
            &eof_tok(),
            "Unexpected token in generic list.",
            vec!["Expected `>`, but found end of file instead."],
        ),
    }
    tokens.pop();

    gen
}

fn parse_function_inputs_from_tokens(tokens: &mut Vec<Token>) -> Vec<Type> {
    match tokens.last() {
        Some(Token {
            kind: TokenKind::Marker(Marker::OpenParen),
            ..
        }) => (),
        Some(tok) => compiler_error(
            tok,
            format!(
                "Expected `(` after function name, but found {} instead.",
                tok
            )
            .as_str(),
            vec![],
        ),
        None => compiler_error(
            &eof_tok(),
            "Expected `(` after function name, but found end of file instead",
            vec![],
        ),
    }
    tokens.pop();

    let mut inputs = vec![];
    while let Some(typ) = parse_maybe_tagged_type_from_tokens(tokens) {
        inputs.push(typ);
    }

    match tokens.last() {
        Some(Token {
            kind: TokenKind::Marker(Marker::CloseParen),
            ..
        }) => (),
        Some(tok) => compiler_error(
            tok,
            format!("Expected `)`, but found {} instead.", tok).as_str(),
            vec![],
        ),
        None => compiler_error(
            &eof_tok(),
            "Expected `)`, but found end of file instead",
            vec![],
        ),
    }
    tokens.pop();

    inputs
}

fn parse_function_outputs_from_tokens(tokens: &mut Vec<Token>) -> Vec<Type> {
    match tokens.last() {
        Some(Token {
            kind: TokenKind::Marker(Marker::Arrow),
            ..
        }) => (),
        _ => return vec![],
    }
    tokens.pop();

    match tokens.last() {
        Some(Token {
            kind: TokenKind::Marker(Marker::OpenBracket),
            ..
        }) => (),
        Some(tok) => compiler_error(
            tok,
            format!("Expected `[`, but found {} instead", tok).as_str(),
            vec![],
        ),
        None => compiler_error(
            &eof_tok(),
            "Expected `[`, but found end of file instead",
            vec![],
        ),
    }
    tokens.pop();

    let mut outputs = vec![];
    while let Some(typ) = parse_untagged_type_from_tokens(tokens) {
        outputs.push(typ);
    }

    match tokens.last() {
        Some(Token {
            kind: TokenKind::Marker(Marker::CloseBracket),
            ..
        }) => (),
        Some(tok) => compiler_error(
            tok,
            format!("Expected `]`, but found {} instad.", tok).as_str(),
            vec![],
        ),
        None => compiler_error(
            &eof_tok(),
            "Expected `]`, but found end of file instead",
            vec![],
        ),
    }
    tokens.pop();

    outputs
}

fn parse_signature_from_tokens(tokens: &mut Vec<Token>) -> Signature {
    let inputs = parse_function_inputs_from_tokens(tokens);
    let outputs = parse_function_outputs_from_tokens(tokens);
    Signature { inputs, outputs }
}

fn parse_word_list_from_tokens(tokens: &mut Vec<Token>) -> (Token, Vec<String>) {
    let maybe_tok = tokens.last();
    let open_bracket_tok = match maybe_tok {
        Some(Token {
            kind: TokenKind::Marker(Marker::OpenBracket),
            ..
        }) => tokens.pop().unwrap(),
        Some(tok) => compiler_error(
            tok,
            format!(
                "Expected `[` to start word list, but found {} instead.",
                tok
            )
            .as_str(),
            vec![],
        ),
        None => compiler_error(
            &eof_tok(),
            "Expected `[`, but found end of file instead",
            vec![],
        ),
    };

    let mut words = vec![];

    loop {
        let maybe_tok = tokens.last();
        match maybe_tok {
            Some(Token {
                kind: TokenKind::Marker(Marker::CloseBracket),
                ..
            }) => {
                tokens.pop();
                break;
            }
            Some(Token {
                kind: TokenKind::Word(s),
                ..
            }) => {
                words.push(s.clone());
                tokens.pop();
            }
            Some(tok) => compiler_error(
                tok,
                format!("Expected `word` or `]`, but found {} instead.", tok).as_str(),
                vec![],
            ),
            None => compiler_error(
                &eof_tok(),
                "Expected `word` or `]`, but found end of file instead.",
                vec![],
            ),
        }
    }

    (open_bracket_tok, words)
}

fn parse_var_from_tokens(tokens: &mut Vec<Token>) -> Vec<Op> {
    let maybe_tok = tokens.last();
    match maybe_tok {
        Some(&Token {
            kind: TokenKind::Keyword(Keyword::Var),
            ..
        }) => {
            tokens.pop();
        }
        Some(tok) => compiler_error(
            tok,
            format!("Expected keyword `var`, but found {} instead", tok).as_str(),
            vec![],
        ),
        None => compiler_error(
            &eof_tok(),
            "Expected keyword `var`, but found end of file instead",
            vec![],
        ),
    }
    // tokens.pop().unwrap();

    let (tok, idents) = parse_word_list_from_tokens(tokens);
    if idents.is_empty() {
        compiler_error(
            &tok,
            "Must provide at least one identifier in `var` block.",
            vec![],
        )
    }
    idents
        .iter()
        .rev()
        .map(|ident| Op::MakeIdent(ident.clone()))
        .collect()
}

fn parse_function_from_tokens(tokens: &mut Vec<Token>) -> Function {
    let maybe_tok = tokens.last();
    match maybe_tok {
        Some(Token {
            kind: TokenKind::Keyword(Keyword::Function),
            ..
        }) => {
            tokens.pop();
        }
        Some(tok) => compiler_error(
            tok,
            format!("Expected keyword `fn`, but found {} instead", tok).as_str(),
            vec![],
        ),
        None => compiler_error(
            &eof_tok(),
            "Expected keyword `fn`, but found end of file instead",
            vec![],
        ),
    }
    match tokens.last() {
        Some(Token {
            kind: TokenKind::Word(_),
            ..
        }) => (),
        Some(tok) => compiler_error(
            tok,
            format!("Expected function name, but found {} instead.", tok).as_str(),
            vec![],
        ),
        None => compiler_error(
            &eof_tok(),
            "Expected function name, but found end of file instead.",
            vec![],
        ),
    }
    let name_tok = tokens.pop();
    let name = match name_tok {
        Some(Token {
            kind: TokenKind::Word(ref s),
            ..
        }) => s.clone(),
        _ => unreachable!(),
    };

    let gen = parse_function_generics(tokens);

    let sig = parse_signature_from_tokens(tokens);
    match tokens.last() {
        Some(Token {
            kind: TokenKind::Marker(Marker::OpenBrace),
            ..
        }) => (),
        Some(tok) => compiler_error(
            tok,
            format!("Expected `{{`, but found {} instead", tok).as_str(),
            vec![],
        ),
        None => compiler_error(
            &eof_tok(),
            "Expected `{{`, but found end of file instead",
            vec![],
        ),
    }
    tokens.pop();

    let mut ops: Vec<Op> = vec![];

    loop {
        let maybe_tok = tokens.last();
        match maybe_tok {
            Some(Token {
                kind: TokenKind::Marker(Marker::CloseBrace),
                ..
            }) => break,
            Some(Token {
                kind: TokenKind::Keyword(Keyword::Var),
                ..
            }) => {
                let mut idents = parse_var_from_tokens(tokens);
                ops.append(&mut idents);
            }
            Some(_) => ops.push(Op::from(tokens.pop().unwrap())),
            None => compiler_error(
                &eof_tok(),
                "Expected `}}`, but found end of file instead",
                vec![],
            ),
        }
    }
    tokens.pop();

    Function {
        name,
        token: name_tok.unwrap(),
        gen,
        sig,
        ops,
    }
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
            }) => program
                .functions
                .push(parse_function_from_tokens(&mut tokens)),
            Some(tok) => compiler_error(tok, format!("Unexpected token: {}", tok).as_str(), vec![]),
            None => break,
        }
    }

    program
}
