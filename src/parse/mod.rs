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



use self::tokens::{Punctuation, Position, TokenKind, Keyword};

#[derive(Debug, PartialEq, Eq)]
pub enum ParseErrorKind { 
    Unexpected(TokenKind),
    UnknownCharacterType, 

    ExpectedPunctuation(Punctuation),
    ExpectedKeyword(Keyword),
    ExpectedIdentifier, 
    ExpectedNumber, 

    /// The lexer attempted to process a punctuation word that does not correspond to a valid `Punctuation` 
    UnrecognizedPunctuation(String),

    /// The lexer hit an arbitrary limit while attempting to resolve a word
    /// (namely a punctuation sequence).
    /// 
    /// This might not be necessary but it felt uncomfortable not having one, idk 
    WordTooLong, 
}
#[derive(Debug)]
pub struct ParseError { 
    kind: ParseErrorKind,
    position: Position,
}

pub type ParseResult<T> = Result<T, ParseError>;




// pub fn parse_literal(input: &str) -> ParseResult<()> { 
//     let mut lex = lexer::TokenStream::new(input.chars()); 

//     loop { 
//         let next = lex.next()?;
//         if next == RawTokenKind::EOF { break } 

//         match next.as_terminal()? { 
//             TerminalToken::Type => { 
//                 let name = lex.next()?.as_identifier()?; 
//                 lex.next()?.expect(TerminalToken::Assign)?;
//                 let value = lex.next()?.as_identifier()?;

//                 println!("New type named '{name}', aliased to '{value}'");
//             },

//             _ => return Err(ParseErrorKind::ExpectedKeyword),
//         }
//     }

//     Ok(())
// }