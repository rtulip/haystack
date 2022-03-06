use crate::ir::{Function, Keyword, Marker, Op, Operator, Program, Signature, Token, Type};
use crate::lexer::logos_lex::LogosToken;
use logos::Logos;
use std::fs;

fn parse_maybe_tagged_type_from_tokens(tokens: &mut Vec<Token>) -> Option<Type> {
    match tokens.last() {
        Some(Token::Word(_)) => (),
        _ => return None,
    };

    let name = match tokens.pop() {
        Some(Token::Word(s)) => s,
        _ => unreachable!(),
    };

    match tokens.last() {
        Some(Token::Marker(Marker::Colon)) => (),
        _ => return Some(Type { name, ident: None }),
    }
    tokens.pop();

    match tokens.last() {
        Some(Token::Word(_)) => (),
        tok => panic!(
            "Unexpected token after type identifier. Expected `word` but found {:?}",
            tok
        ),
    }

    let ident = match tokens.pop() {
        Some(Token::Word(s)) => Some(s),
        _ => unreachable!(),
    };

    Some(Type { name, ident })
}

fn parse_untagged_type_from_tokens(tokens: &mut Vec<Token>) -> Option<Type> {
    match tokens.last() {
        Some(Token::Word(_)) => (),
        _ => return None,
    }

    match tokens.pop().unwrap() {
        Token::Word(name) => Some(Type { name, ident: None }),
        _ => unreachable!(),
    }
}

fn parse_function_generics(mut tokens: &mut Vec<Token>) -> Vec<Type> {
    match tokens.last() {
        Some(Token::Operator(Operator::LessThan)) => (),
        Some(Token::Marker(Marker::OpenParen)) => return vec![],
        t => panic!("Unexpected token in function definition: {:?}", t),
    }
    tokens.pop();

    let mut gen = vec![];

    while let Some(typ) = parse_untagged_type_from_tokens(&mut tokens) {
        gen.push(typ);
    }

    match tokens.last() {
        Some(Token::Operator(Operator::GreaterThan)) => (),
        t => panic!(
            "Unexpected token in function definition. Expected `>`, but found {:?}",
            t
        ),
    }
    tokens.pop();

    gen
}

fn parse_function_inputs_from_tokens(mut tokens: &mut Vec<Token>) -> Vec<Type> {
    match tokens.last() {
        Some(Token::Marker(Marker::OpenParen)) => (),
        _ => panic!("Expected `(` after function name."),
    }
    tokens.pop();

    let mut inputs = vec![];
    while let Some(typ) = parse_maybe_tagged_type_from_tokens(&mut tokens) {
        inputs.push(typ);
    }

    match tokens.last() {
        Some(Token::Marker(Marker::CloseParen)) => (),
        _ => panic!("Expected ')'."),
    }
    tokens.pop();

    inputs
}

fn parse_function_outputs_from_tokens(mut tokens: &mut Vec<Token>) -> Vec<Type> {
    match tokens.last() {
        Some(Token::Marker(Marker::Arrow)) => (),
        _ => return vec![],
    }
    tokens.pop();

    match tokens.last() {
        Some(Token::Marker(Marker::OpenBracket)) => (),
        _ => panic!("Expected '['"),
    }
    tokens.pop();

    let mut outputs = vec![];
    while let Some(typ) = parse_untagged_type_from_tokens(&mut tokens) {
        outputs.push(typ);
    }

    match tokens.last() {
        Some(Token::Marker(Marker::CloseBracket)) => (),
        t => panic!("Expected `]`, but found {:?} instead.", t),
    }
    tokens.pop();

    outputs
}

fn parse_signature_from_tokens(mut tokens: &mut Vec<Token>) -> Signature {
    let inputs = parse_function_inputs_from_tokens(&mut tokens);
    let outputs = parse_function_outputs_from_tokens(&mut tokens);
    Signature { inputs, outputs }
}

fn parse_function_from_tokens(mut tokens: &mut Vec<Token>) -> Function {
    match tokens.last() {
        Some(Token::Keyword(Keyword::Function)) => (),
        _ => panic!(
            "Expected keyword `fn` but found {:?} instead",
            tokens.last()
        ),
    }
    tokens.pop();
    match tokens.last() {
        Some(Token::Word(_)) => (),
        t => panic!("Expected function name, but found {:?} instead", t),
    }
    let name = match tokens.pop() {
        Some(Token::Word(s)) => s,
        _ => unreachable!(),
    };

    let gen = parse_function_generics(&mut tokens);

    let sig = parse_signature_from_tokens(&mut tokens);
    match tokens.last() {
        Some(Token::Marker(Marker::OpenBrace)) => (),
        t => panic!("Expected `{{`, but found {:?} instead", t),
    }
    tokens.pop();

    let mut ops: Vec<Op> = vec![];

    loop {
        match tokens.last() {
            Some(Token::Marker(Marker::CloseBrace)) => break,
            Some(_) => (),
            None => panic!("Expected `}}`, but found end of file instead."),
        }

        ops.push(Op::from(tokens.pop().unwrap()))
    }
    tokens.pop();

    Function {
        name,
        gen,
        sig,
        ops,
    }
}

pub fn hay_into_ir<P: AsRef<std::path::Path>>(input_path: P, output_path: P) {
    let file = fs::read_to_string(input_path).unwrap();

    let mut lexer = LogosToken::lexer(file.as_str());
    let mut tokens: Vec<Token> = vec![];
    while let Some(tok) = lexer.next() {
        tokens.push(Token::from((tok, lexer.slice())));
    }
    tokens.reverse();

    let mut program = Program::default();
    loop {
        let maybe_tok = tokens.last();
        match maybe_tok {
            Some(Token::Keyword(Keyword::Function)) => program
                .functions
                .push(parse_function_from_tokens(&mut tokens)),
            _ => break,
        }
    }

    let json = serde_json::to_string_pretty(&program).unwrap();
    fs::write(output_path, json).unwrap();
}
