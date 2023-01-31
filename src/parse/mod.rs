/// The lexer converts a raw stream of characters into a more practical format.
/// 
/// See https://veneto.notion.site/Lexing-rules-5cdd6b984898418cbeacd416158e5e81 for more information on how the lexer works.
mod lexer;
mod tokens;

#[cfg(test)]
mod lexer_tests;

use self::{lexer::RawTokenKind, tokens::{TerminalToken, Position}};

#[derive(Debug)]
pub enum ParseErrorKind { 
    Unexpected(RawTokenKind),
    UnknownCharacterType, 

    Expected(TerminalToken),
    ExpectedKeyword, 
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