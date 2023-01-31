mod lexer;
mod tokens;

use self::{lexer::RawToken, tokens::TerminalToken};

#[derive(Debug)]
pub enum ParseError { 
    Unexpected(RawToken),
    UnknownCharacterType, 

    Expected(TerminalToken),
    ExpectedKeyword, 
}

pub type ParseResult<T> = Result<T, ParseError>;




pub fn parse_literal(input: &str) -> ParseResult<()> { 
    let mut lex = lexer::TokenStream::new(input.chars()); 

    loop { 
        let next = lex.next()?;
        if next == RawToken::EOF { break } 

        match next.as_terminal()? { 
            TerminalToken::Type => { 
                let name = lex.next()?.as_identifier()?; 
                lex.next()?.expect(TerminalToken::Assign)?;
                let value = lex.next()?.as_identifier()?;

                println!("New type named '{name}', aliased to '{value}'");
            },

            _ => return Err(ParseError::ExpectedKeyword),
        }
    }

    Ok(())
}