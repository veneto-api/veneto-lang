use super::ParseResult;
use super::lexer::TokenStream;
use super::tokens::{ RawTokenKind, WordKind };

use super::tokens::TerminalToken;

//
// Helpers
//
// These should probably be macros but I'm not comfortable with those yet 🥴

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
fn assert_terminal(stream: &mut TokenStream, kind: TerminalToken) { 
    assert_eq!(stream.next().unwrap().as_terminal().unwrap(), kind); 
}

//
// Tests
//


#[test]
pub fn words() { 
    let mut stream = token_stream("asdf jkl ree");
    assert_identifier(&mut stream, "asdf");
    assert_identifier(&mut stream, "jkl");
    assert_identifier(&mut stream, "ree");
    assert_eof(&mut stream);
}

#[test]
pub fn valid_identifiers() { 
    let mut stream = token_stream("asdf foo_bar f12_a-17BAR");
    assert_identifier(&mut stream, "asdf");
    assert_identifier(&mut stream, "foo_bar");
    assert_identifier(&mut stream, "f12_a-17BAR");
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

#[test]
pub fn spacing() { 
    let mut stream = token_stream("aaa\neee   \n\t  foo  bar \n");
    assert_identifier(&mut stream, "aaa");
    assert_identifier(&mut stream, "eee");
    assert_identifier(&mut stream, "foo");
    assert_identifier(&mut stream, "bar");
    assert_eof(&mut stream);
}

#[test]
pub fn operators() { 
    let mut stream = token_stream("asdf @foo% bar #123");
    assert_identifier(&mut stream, "asdf");
    assert_terminal(&mut stream, TerminalToken::Ampersat);
    assert_identifier(&mut stream, "foo");
    assert_terminal(&mut stream, TerminalToken::Lax);
    assert_identifier(&mut stream, "bar");
    assert_terminal(&mut stream, TerminalToken::HttpStatus);

    match stream.next().unwrap().kind { 
        RawTokenKind::Word(WordKind::Number, str) if str == "123" => {}
        _ => panic!()
    }

    assert_eof(&mut stream);
}

#[test]
pub fn string_literal() { 
    // Comments are ignored inside of strings under the current lexing rules
    // Also, we do not restrict newlines inside of literals.  This is dumb, but I never implemented a rule preventing it.  
    // This could change of course 
    let mut stream = token_stream("foo \"aa \n\t oo // ee /* aa */ ff \"   bar");
    assert_identifier(&mut stream, "foo");

    if let RawTokenKind::StringLiteral(str) = stream.next().unwrap().kind { 
        assert_eq!(str, "aa \n\t oo // ee /* aa */ ff ")
    }
    else { panic!("Expected StringLiteral") }

    assert_identifier(&mut stream, "bar");
    assert_eof(&mut stream);
}