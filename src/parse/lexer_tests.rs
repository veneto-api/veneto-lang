use crate::parse::lexer::{Span, Position};

use super::ParseResult;
use super::lexer::TokenStream;
use super::tokens::{ TokenKind, Keyword };

use super::tokens::Punctuation;

//
// Helpers
//
// Some of these are used in other tests to ensure that the parsing functions stop in the appropriate places 

pub fn token_stream(str: &str) -> TokenStream { 
    TokenStream::new(str.chars()) 
}
fn assert_identifier(stream: &mut TokenStream, val: &str) { 
    assert_eq!(stream.next().unwrap().as_identifier().unwrap(), val); 
}
pub fn assert_eof(stream: &mut TokenStream) { 
    assert_eq!(stream.next().unwrap().kind, TokenKind::EOF); 
}
pub fn assert_punctuation(stream: &mut TokenStream, expected: Punctuation) { 
    stream.next().unwrap().expect_punctuation(expected).unwrap()
}
pub fn assert_keyword(stream: &mut TokenStream, expected: Keyword) { 
    stream.next().unwrap().expect_keyword(expected).unwrap()
}

//
// Tests
//


#[test]
fn words() { 
    let mut stream = token_stream("asdf jkl ree");

    let token = stream.next().unwrap(); 
    assert_eq!(token.as_identifier().unwrap(), "asdf"); 
    assert_eq!(token.span, Span { lo: Position(0), hi: Position(4) });

    let token = stream.next().unwrap(); 
    assert_eq!(token.as_identifier().unwrap(), "jkl"); 
    assert_eq!(token.span, Span { lo: Position(5), hi: Position(8) });

    let token = stream.next().unwrap(); 
    assert_eq!(token.as_identifier().unwrap(), "ree"); 
    assert_eq!(token.span, Span { lo: Position(9), hi: Position(12) });

    assert_eof(&mut stream);
}

#[test]
fn valid_identifiers() { 
    let mut stream = token_stream("asdf foo_bar f12_a17BAR");
    assert_identifier(&mut stream, "asdf");
    assert_identifier(&mut stream, "foo_bar");
    assert_identifier(&mut stream, "f12_a17BAR");
    assert_eof(&mut stream);
}

#[test]
fn line_comment() -> ParseResult<()> { 
    let mut stream = token_stream("asdf// jsdklf \"heee\" hoo raaa\n\nfoobar endofline");

    let next = stream.next().unwrap();
    assert_eq!(next.as_identifier().unwrap(), "asdf");
    assert_eq!(next.span, Span { lo: Position(0), hi: Position(4) });

    let next = stream.next().unwrap();
    assert_eq!(next.as_identifier().unwrap(), "foobar");
    assert_eq!(next.span, Span { lo: Position(31), hi: Position(37) });

    let next = stream.next().unwrap();
    assert_eq!(next.as_identifier().unwrap(), "endofline");
    assert_eq!(next.span, Span { lo: Position(38), hi: Position(47) });

    assert_eq!(stream.stream.stream.newlines, vec![ 29, 30 ]);

    assert_eof(&mut stream); 
    Ok(())
}

#[test]
fn block_comment() { 
    let mut stream = token_stream("asdf/* eee\neeeass//\n*\n/*\n*/   end");

    let next = stream.next().unwrap();
    assert_eq!(next.as_identifier().unwrap(), "asdf");
    assert_eq!(next.span, Span { lo: Position(0), hi: Position(4) });

    let next = stream.next().unwrap();
    assert_eq!(next.as_identifier().unwrap(), "end");
    assert_eq!(next.span, Span { lo: Position(30), hi: Position(33) });

    assert_eq!(stream.stream.stream.newlines, vec![ 10, 19, 21, 24 ]);

    assert_eof(&mut stream);
}

#[test]
fn spacing() { 
    let mut stream = token_stream("aaa\neee   \n\t  foo  bar \n");

    let next = stream.next().unwrap();
    assert_eq!(next.as_identifier().unwrap(), "aaa");
    assert_eq!(next.span, Span { lo: Position(0), hi: Position(3) });

    let next = stream.next().unwrap();
    assert_eq!(next.as_identifier().unwrap(), "eee");
    assert_eq!(next.span, Span { lo: Position(4), hi: Position(7) });

    let next = stream.next().unwrap();
    assert_eq!(next.as_identifier().unwrap(), "foo");
    assert_eq!(next.span, Span { lo: Position(14), hi: Position(17) });

    let next = stream.next().unwrap();
    assert_eq!(next.as_identifier().unwrap(), "bar");
    assert_eq!(next.span, Span { lo: Position(19), hi: Position(22) });

    assert_eq!(stream.stream.stream.newlines, vec![ 3, 10,  23]);

    assert_eof(&mut stream);
}

#[test]
fn punctuation() { 
    let mut stream = token_stream("asdf @foo% bar #123");
    assert_identifier(&mut stream, "asdf");

    let next = stream.next().unwrap();
    assert_eq!(next.as_punctuation().unwrap(), Punctuation::SpecialType);
    assert_eq!(next.span, Span { lo: Position(5), hi: Position(6) });

    assert_identifier(&mut stream, "foo");

    let next = stream.next().unwrap();
    assert_eq!(next.as_punctuation().unwrap(), Punctuation::Lax);
    assert_eq!(next.span, Span { lo: Position(9), hi: Position(10) });

    assert_identifier(&mut stream, "bar");
    assert_punctuation(&mut stream, Punctuation::HttpStatus);

    assert_eq!(stream.next().unwrap().try_as_number().unwrap(), "123"); 

    assert_eof(&mut stream);
}

#[test]
fn multichar_punctuation() { 
    let mut stream = token_stream("foo :: bar"); 
    assert_identifier(&mut stream, "foo");
    
    let next = stream.next().unwrap();
    assert_eq!(next.as_punctuation().unwrap(), Punctuation::PathSeparator);
    assert_eq!(next.span, Span { lo: Position(4), hi: Position(6) });

    assert_identifier(&mut stream, "bar");
    assert_eof(&mut stream);
}

#[test]
fn conjoined_punctuation() { 
    let mut stream = token_stream(":::");

    let next = stream.next().unwrap();
    assert_eq!(next.as_punctuation().unwrap(), Punctuation::PathSeparator);
    assert_eq!(next.span, Span { lo: Position(0), hi: Position(2) });

    let next = stream.next().unwrap();
    assert_eq!(next.as_punctuation().unwrap(), Punctuation::Colon);
    assert_eq!(next.span, Span { lo: Position(2), hi: Position(3) });

    assert_eof(&mut stream);
}

#[test]
fn string_literal() { 
    // Comments are ignored inside of strings under the current lexing rules
    // Also, we do not restrict newlines inside of literals.
    let mut stream = token_stream("foo \"aa \n\t oo // ee /* aa */ ff \"   bar");
    assert_identifier(&mut stream, "foo");

    let next = stream.next().unwrap();
    if let TokenKind::StringLiteral(str) = next.kind { 
        assert_eq!(str, "aa \n\t oo // ee /* aa */ ff ")
    }
    else { panic!("Expected StringLiteral") }
    assert_eq!(next.span, Span { lo: Position(4), hi: Position(32) });

    assert_identifier(&mut stream, "bar");
    assert_eof(&mut stream);

    assert_eq!(stream.stream.stream.newlines, vec![ 8 ])
}