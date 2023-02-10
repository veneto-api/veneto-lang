use crate::parse::{ClauseResult, ClauseDelim, ParseResult};
use crate::parse::{lexer::TokenStream};
use crate::parse::tokens::{Punctuation};

use super::{Peekable, Expectable};

/// This is an identifier that can accept generic parameters.
#[derive(PartialEq, Eq, Debug)]
pub enum GenericIdentifier { 
    Simple(String), 
    Generic(String, Vec<GenericIdentifier>)
}

impl GenericIdentifier { 

    fn clause_arg(stream: &mut TokenStream) -> ClauseResult<GenericIdentifier> { 

        fn delim(stream: &mut TokenStream) -> ParseResult<ClauseDelim> { 
            let peek = stream.peek()?;
            match peek.as_punctuation() { 
                Some(Punctuation::Comma) => { 
                    stream.next()?;
                    Ok(ClauseDelim::Continue)
                }, 
                Some(Punctuation::GenericClose) => {
                    stream.next()?; 
                    Ok(ClauseDelim::Exit)
                },
                _ => Ok(ClauseDelim::Unexpected(peek))
            }
        }
        
        let ident = stream.next()?.try_as_identifier()?; 
        let peek = stream.peek()?; 
        
        if Some(Punctuation::GenericOpen) == peek.as_punctuation() { 
            stream.next()?; 
            let args = Self::clause_nested(stream)?;
            Ok((GenericIdentifier::Generic(ident, args), delim(stream)?))
        } 
        else { 
            Ok((GenericIdentifier::Simple(ident), delim(stream)?))
        }
    }


    fn clause_nested(stream: &mut TokenStream) -> ParseResult<Vec<GenericIdentifier>> { 

        let mut args = Vec::<GenericIdentifier>::new(); 
        loop { 
            let (clause, delim) = Self::clause_arg(stream)?;
            match delim { 
                ClauseDelim::Continue => { 
                    args.push(clause); 
                    continue; 
                }, 
                ClauseDelim::Exit => { 
                    args.push(clause); 
                    return Ok(args)
                },
                ClauseDelim::Unexpected(t) => return Err(t.as_err_unexpected())
            }
        }
    }

    /// Parses a potentially-generic identifier, starting with the provided `base` identifier. 
    fn finish(stream: &mut TokenStream, ident: String) -> ParseResult<Self> { 
        let peek = stream.peek()?;
        if Some(Punctuation::GenericOpen) == peek.as_punctuation() { 
            stream.next()?; 
            let args = Self::clause_nested(stream)?; 
            Ok(GenericIdentifier::Generic(ident, args))
        } 
        else { 
            Ok(GenericIdentifier::Simple(ident))
        }
    }
}

impl Peekable for GenericIdentifier {
    fn parse_peek(stream: &mut TokenStream) -> ParseResult<Option<Self>> {
        match stream.peek_for_identifier()? { 
            Some(ident) => Self::finish(stream, ident).map(Some),
            None => Ok(None)
        }
    }
}
impl Expectable for GenericIdentifier { 
    fn parse_expect(stream: &mut TokenStream) -> ParseResult<Self> {
        let ident = stream.next()?.try_as_identifier()?;
        Self::finish(stream, ident)
    }
}


#[cfg(test)]
mod test {
    use crate::parse::{lexer_tests::{token_stream, assert_punctuation}, ParseResult, lexer::TokenStream, tokens::Punctuation, ParseErrorKind, ast::Expectable};

    use super::GenericIdentifier;

    fn parse_gid(input: &str) -> ParseResult<(TokenStream, GenericIdentifier)>{ 
        let mut stream = token_stream(input); 
        GenericIdentifier::parse_expect(&mut stream).map(
            |res| (stream, res) 
        )
    }
    fn assert_gid(input: &str, expected: GenericIdentifier) { 
        let (_, gid) = parse_gid(input).unwrap();
        assert_eq!(gid, expected); 
    }

    macro_rules! gid {

        ($base:literal < $($params:tt),+ >) => { 
            GenericIdentifier::Generic($base.to_owned(), vec![ $(gid!($params)),+ ])
        };

        ($base:ident) => { 
            $base
        };

        ($base:literal) => {
            GenericIdentifier::Simple($base.to_owned())
        };

    }
 
    #[test]
    fn simple() { 
        let (mut stream, gid) = parse_gid("foo,").unwrap(); 
        assert_eq!(gid, gid!("foo"));
        assert_punctuation(&mut stream, Punctuation::Comma); 
    }

    #[test] 
    fn single_param() { 
        assert_gid("foo<bar>", gid!("foo"<"bar">))
    }

    #[test]
    fn multi_param() { 
        assert_gid("foo<bar, baz>", gid!("foo"<"bar", "baz">))
    }

    #[test]
    fn complex() { 
        let (mut stream, gid) = parse_gid("foo<bar, baz<asdf, ree>, aaa>,").unwrap();
        let inner = gid!("baz"<"asdf", "ree">);
        assert_eq!(gid, gid!("foo"<"bar", inner, "aaa">));
        assert_punctuation(&mut stream, Punctuation::Comma);
    }

    #[test]
    fn fail_trail() { 
        let res = parse_gid("asdf<jkl, bar,>"); 
        assert!(matches!(res.unwrap_err().kind, ParseErrorKind::ExpectedIdentifier))
    }
}