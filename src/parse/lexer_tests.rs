
use crate::parse::{ParseResult, lexer::{RawTokenKind, WordKind, TokenStream}};

fn token_stream(str: &str) -> TokenStream { 
    TokenStream::new(str.chars()) 
}
fn assert_identifier(stream: &mut TokenStream, val: &str) { 
    assert_eq!(stream.next().unwrap().as_identifier().unwrap(), val); 
}
fn assert_kind(stream: &mut TokenStream, kind: RawTokenKind) { 
    assert_eq!(stream.next().unwrap().kind, kind); 
}
fn assert_eof(stream: &mut TokenStream) { 
    assert_eq!(stream.next().unwrap().kind, RawTokenKind::EOF); 
}


#[test]
pub fn words() { 
    let mut stream = token_stream("asdf jkl ree");
    assert_identifier(&mut stream, "asdf");
    assert_identifier(&mut stream, "jkl");
    assert_identifier(&mut stream, "ree");
    assert_eof(&mut stream);
}

#[test]
pub fn line_comment() -> ParseResult<()> { 
    let mut stream = token_stream("asdf// jsdklf \"heee\" hoo raaa\n\n/foobar endofline");
    assert_identifier(&mut stream, "asdf");

    match stream.next()?.kind { 
        RawTokenKind::Word(WordKind::Punctuation, str) if str == "/" => {}
        _ => panic!()
    }

    assert_identifier(&mut stream, "foobar");
    assert_identifier(&mut stream, "endofline");
    assert_eof(&mut stream); 
    Ok(())
}

#[test]
pub fn block_comment() { 
    let mut stream = token_stream("asdf/* eee\neeeass//\n*\n/*\n*/   end");
    assert_identifier(&mut stream, "asdf");
    assert_identifier(&mut stream, "end");
    assert_eof(&mut stream);
}