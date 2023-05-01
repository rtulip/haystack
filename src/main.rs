#![feature(btree_drain_filter)]
#![feature(drain_filter)]
#![feature(box_patterns)]
#![warn(missing_docs)]

//! # Haystack
//!
//! Haystack is a compiled, staticaly typed, stack based programming language
//! with opt-in variable assignment for improved ergonomics. This is it's
//! compiler!
//!
//! ```haystack
//! fn main() {
//!     "Welcome to the Haystack compiler!" println
//! }
//! ```
//!
//! # Compilation Stages
//! To compile a `Haystack` program from an input file into the generated
//! assembly, the compiler transforms the data between representations a number
//! of times.
//!
//! At a high level, the process looks like this:
//! 1. Input `.hay` file is read and converted into tokens (lexing/scanning).
//! 2. Tokens are parsed into (untyped) statements and expressions (parsing).
//! 3. Expressions are type-checked and mapped to Typed Expressions.
//! 4. Typed Expressions are converted into backend instructions
//! 5. Instructions are converted into assembly.
//!  
//! Then `nasm` and `ld` are used to assemble and link the generated program.
//!
//! ## Lexing/Scanning
//! This is the process of turning the raw text file into meaningful units
//! which the compiler can understand. [`lex::token::Token`] represents each
//! individual unit.
//!
//! For example:
//! ```haystack
//! fn main() {
//!     "Hello World" println
//! }
//! ```
//! _Loosely_ turns into this, with some some additional locational information
//! within the file:
//! ```
//! vec![
//!     Token{ kind: TokenKind::Keyword(Keyword::Function), ...},
//!     Token{ kind: TokenKind::Identifier("main"), ...},
//!     Token{ kind: TokenKind::Marker(Marker::LeftParen), ...},
//!     Token{ kind: TokenKind::Marker(Marker::RightParen), ...},
//!     Token{ kind: TokenKind::Marker(Marker::LeftBrace), ...},
//!     Token{ kind: TokenKind::Literal(Literal::String("Hello World")), ...},
//!     Token{ kind: TokenKind::Identifier("println"), ...}
//!     Token{ kind: TokenKind::Marker(Marker::RightBrace), ...},
//!     Token{ kind: TokenKind::EndOfFile, ...},
//! ]
//! ```
//!
//! ## Parsing
//! Once the file has been Tokenized, top-level statements are parsed. Each
//! statement expects a particular sequence of tokens, and will report an error
//! if unexpected tokens are found. The tokens from above would be parsed into
//! a function statement _loosely_ like this:
//! ```
//! Stmt::Function(FunctionStmt {
//!     token: Token{ kind: TokenKind::Keyword(Keyword::Function), ...},
//!     name:  Token{ kind: TokenKind::Identifier("main"), ...},
//!     inputs: vec![],
//!     outputs: vec![],
//!     body: vec![
//!         Expr::Literal(LiteralExpr{ literal: Token {
//!             kind: TokenKind::Literal(Literal::String("Hello World")),
//!             ...},
//!         }),
//!         Expr::Identifier(IdentifierExpr { ident: Token {
//!             kind: TokenKind::Identifier("println"),
//!              ... },
//!         }),
//!     ],
//!     ...
//! })
//! ```
//!
//! ## Type Checking
//! Each expression needs to be type checked to ensure that each expression is
//! given the correct inputs, that function outputs are correct, and to
//! determine the specific sizes & offsets that are required. Each
//! [`ast::expr::Expr`] has a function `type_check()` function, which converts
//! the untyped expression into a typed one.
//!
//! Within the type system, functions are represented in one of three states,
//! generic, unchecked, and checked. A [`types::Function`] represents a type
//! checked function, who has been verified to produce the correct outputs,
//! assuming that the correct inputs are provided.
//!
//! [`types::UncheckedFunction`] represents a non-generic function that has yet
//! to be type checked. The type checking process will continue for as long as
//! there are unchecked functions to convert to checked functions in the global
//! type map.
//!
//! [`types::GenericFunction`] represents generic functions that are __not__
//! type checked. Instead, when a generic function is called and the generics
//! are mapped successfully to non-generic types, a new unchecked function is
//! created, where each generic paramter has been mapped accordingly. This
//! process is called monomorphization.
//!
//! By the end of the type checking process, the compiler will have verified
//! that each expression and function consumes and produces the correct outputs
//! on the stack, or will exit early and report the error.
//!
//! ## Backend
//! Once the program has been type checked, many more details are known about
//! the program, such as exactly which functions are being called, and the size
//! of each type. The typed expressions can then be used to generate lower
//! level instructions, which are easier to convert into assembly. These
//! instructions are a closer representation of what `Haystack` does under the
//! hood.
//!
//! More work should be done to generalize these backend instructions, which
//! would make it easier to create implementations of `Haystack` for other
//! targets.
//!
//! ## Code Generation
//! The last step of this compiler is to emit the generated code. The compiler
//! emits x86-64 assembly, which will be assembled and linked by `nasm` and `ld`
//! respectively.
//!
//! Code generation works by converting each [`backend::Instruction`] into a
//! small piece of assembly. It is __required__ that each instruction works
//! independently of each other, so that the generated code will work when all
//! the pieces are put together.

mod ast;
mod backend;
mod compiler;
mod error;
mod lex;
mod types;
use clap::Parser;
use compiler::compile_haystack;

#[derive(Parser)]
struct Cli {
    file: String,
    #[clap(short, long)]
    run: bool,
}

fn main() {
    let cli = Cli::parse();
    if let Err(e) = compile_haystack(cli.file, cli.run) {
        e.report();
        std::process::exit(1);
    }
}
