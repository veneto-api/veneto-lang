use super::ParseResult;
use super::lexer::TokenStream;
use super::tokens::{ TokenKind, Keyword };

use super::tokens::Punctuation;

//
// Helpers
//
// These should probably be macros but I'm not comfortable with those yet 🥴

pub fn token_stream(str: &str) -> TokenStream { 
    TokenStream::new(str.chars()) 
}
pub fn assert_identifier(stream: &mut TokenStream, val: &str) { 
    assert_eq!(stream.next().unwrap().as_identifier().unwrap(), val); 
}
pub fn assert_kind(stream: &mut TokenStream, kind: TokenKind) { 
    assert_eq!(stream.next().unwrap().kind, kind); 
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
    assert_identifier(&mut stream, "asdf");
    assert_identifier(&mut stream, "jkl");
    assert_identifier(&mut stream, "ree");
    assert_eof(&mut stream);
}

#[test]
fn valid_identifiers() { 
    let mut stream = token_stream("asdf foo_bar f12_a-17BAR");
    assert_identifier(&mut stream, "asdf");
    assert_identifier(&mut stream, "foo_bar");
    assert_identifier(&mut stream, "f12_a-17BAR");
    assert_eof(&mut stream);
}

#[test]
fn line_comment() -> ParseResult<()> { 
    let mut stream = token_stream("asdf// jsdklf \"heee\" hoo raaa\n\nfoobar endofline");
    assert_identifier(&mut stream, "asdf");
    assert_identifier(&mut stream, "foobar");
    assert_identifier(&mut stream, "endofline");
    assert_eof(&mut stream); 
    Ok(())
}

#[test]
fn block_comment() { 
    let mut stream = token_stream("asdf/* eee\neeeass//\n*\n/*\n*/   end");
    assert_identifier(&mut stream, "asdf");
    assert_identifier(&mut stream, "end");
    assert_eof(&mut stream);
}

#[test]
fn spacing() { 
    let mut stream = token_stream("aaa\neee   \n\t  foo  bar \n");
    assert_identifier(&mut stream, "aaa");
    assert_identifier(&mut stream, "eee");
    assert_identifier(&mut stream, "foo");
    assert_identifier(&mut stream, "bar");
    assert_eof(&mut stream);
}

#[test]
fn punctuation() { 
    let mut stream = token_stream("asdf @foo% bar #123");
    assert_identifier(&mut stream, "asdf");
    assert_punctuation(&mut stream, Punctuation::SpecialType);
    assert_identifier(&mut stream, "foo");
    assert_punctuation(&mut stream, Punctuation::Lax);
    assert_identifier(&mut stream, "bar");
    assert_punctuation(&mut stream, Punctuation::HttpStatus);

    assert_eq!(stream.next().unwrap().try_as_number().unwrap(), "123"); 

    assert_eof(&mut stream);
}

#[test]
fn multichar_punctuation() { 
    let mut stream = token_stream("foo :: bar"); 
    assert_identifier(&mut stream, "foo");
    assert_punctuation(&mut stream, Punctuation::PathSeparator);
    assert_identifier(&mut stream, "bar");
    assert_eof(&mut stream);
}

#[test]
fn conjoined_punctuation() { 
    let mut stream = token_stream(":::");
    assert_punctuation(&mut stream, Punctuation::PathSeparator);
    assert_punctuation(&mut stream, Punctuation::Colon);
    assert_eof(&mut stream);
}

#[test]
fn string_literal() { 
    // Comments are ignored inside of strings under the current lexing rules
    // Also, we do not restrict newlines inside of literals.  This is dumb, but I never implemented a rule preventing it.  
    // This could change of course 
    let mut stream = token_stream("foo \"aa \n\t oo // ee /* aa */ ff \"   bar");
    assert_identifier(&mut stream, "foo");

    if let TokenKind::StringLiteral(str) = stream.next().unwrap().kind { 
        assert_eq!(str, "aa \n\t oo // ee /* aa */ ff ")
    }
    else { panic!("Expected StringLiteral") }

    assert_identifier(&mut stream, "bar");
    assert_eof(&mut stream);
}