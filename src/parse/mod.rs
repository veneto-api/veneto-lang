/// The lexer converts a raw stream of characters into a more practical format.
/// 
/// See https://veneto.notion.site/Lexing-rules-5cdd6b984898418cbeacd416158e5e81 for more information on how the lexer works.
mod lexer;

/// This module contains definitions for the tokens emitted by the `lexer`, 
/// including terminal tokens like punctuation and keywords.  
mod tokens;

/// This contains the representation of the Abstract Syntax Tree emitted by the parser.
mod ast;

#[cfg(test)]
mod lexer_tests;


use std::{backtrace::Backtrace};

use self::{tokens::{Punctuation, Position, TokenKind, Keyword, Token, Terminal}};

#[derive(Debug, PartialEq, Eq)]
pub enum ParseErrorKind { 
    Unexpected(TokenKind),
    UnknownCharacterType, 

    ExpectedPunctuation(Punctuation),
    ExpectedKeyword(Keyword),
    ExpectedIdentifier, 
    ExpectedNumber, 
    ExpectedStringLiteral,

    /// The lexer attempted to process a punctuation word that does not correspond to a valid `Punctuation` 
    UnrecognizedPunctuation(String),

    /// The lexer hit an arbitrary limit while attempting to resolve a word
    /// (namely a punctuation sequence).
    /// 
    /// This might not be necessary but it felt uncomfortable not having one, idk 
    WordTooLong, 

    /// A miscellaneous semantic error while parsing,
    /// described by its associated message.
    Semantic(String),

    /// The syntax used is only valid for structs
    SemanticStructOnly(Terminal), 

    /// The token begins a duplicate declaration
    SemanticDuplicate,

    /// An unrecognized type was used in an interface.
    /// 
    /// Interfaces only support a limited range of types, 
    /// since their representation is limited to the URI syntax 
    UnknownInterfaceValueType, 

    UnknownSpecialType, 

    /// The method name is not recognized as an HTTP verb
    UnknownMethodName, 
}

#[derive(Debug)]
pub struct ParseError { 
    kind: ParseErrorKind,
    position: Position,
    backtrace: Backtrace,
}

pub type ParseResult<T> = Result<T, ParseError>;

#[cfg(test)]
pub trait TestUnwrap<T> { 
    fn test_unwrap(self) -> T; 
}
#[cfg(test)]
impl<T> TestUnwrap<T> for ParseResult<T> {
    fn test_unwrap(self) -> T {
        match self { 
            Ok(x) => x,
            Err(err) => { 
                println!("ParseError at {}:{}: {:?}", err.position.line, err.position.col, err.kind);
                println!("{}", err.backtrace);

                // Currently, abort-on-panic is ignored during testing
                // So I'm doing this hack nonsense instead, 
                // so that only the `ParseError` stacktrace gets printed and not the panic one which is always irrelevant
                std::process::abort()
                // panic!()
            }
        }
    }
}

/// Describes how the clause is terminated and where the TokenStream cursor is pointed, 
/// so that the calling function knows what to do next. 
pub enum ClauseDelim { 
    /// The clause found an explicit end marker, which it consumed
    Exit, 
    /// The clause found an explicit continuation marker, which it consumed
    Continue, 

    /// The clause found a token it does not recognize, which it did not consume
    Unexpected(Token), 
}

/// A **clause** is a processed AST node, along with information about how it was terminated.
/// 
/// This is especially useful for nestable grammar features like Use Trees or Generic Identifiers.
/// These are parsed recursively, and the calling stack frame needs to understand whether it should continue or exit its current context.
pub type ClauseResult<T> = Result<(T, ClauseDelim), ParseError>; 
