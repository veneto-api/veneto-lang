use crate::parse::ParseResult;
use crate::parse::lexer::TokenStream;
use crate::parse::tokens::{Punctuation, Terminal};
use crate::peek_match;

use super::{Peekable, Expectable, Finishable};

/// This is an identifier that can accept generic parameters.
#[derive(PartialEq, Eq, Debug)]
pub struct GenericIdentifier { 
    pub base: String, 
    pub args: GenericArgs,

    //TAG: RC_FLAGS
    // https://www.notion.so/veneto/RC-Flags-conditionals-c1ed70a794d14511b57e06d1798a62a8?pvs=4
}
pub type GenericArgs = Vec<GenericIdentifier>; 

impl Peekable for GenericIdentifier { 
    fn parse_peek(stream: &mut TokenStream) -> ParseResult<Option<Self>> {
        if let Some(base) = stream.peek_for_identifier()? { 
            Ok(Some(GenericIdentifier { 
                base, 
                args: GenericArgs::parse_peek(stream)?.unwrap_or_default(),
            }))
        } else { 
            Ok(None)
        }
    }
}
impl Expectable for GenericIdentifier {
    fn parse_expect(stream: &mut TokenStream) -> ParseResult<Self> {
        Ok(GenericIdentifier { 
            base: stream.next()?.try_as_identifier()?, 
            args: GenericArgs::parse_peek(stream)?.unwrap_or_default(),
        })
    }
}


impl Finishable for GenericArgs { 
    const INITIAL_TOKEN: Terminal = Terminal::Punctuation(Punctuation::GenericOpen);

    fn parse_finish(stream: &mut TokenStream) -> ParseResult<Self> {
        let mut args = Self::new(); 
        loop { 
            if let Some(arg) = GenericIdentifier::parse_peek(stream)? {
                args.push(arg);
            }

            peek_match!(stream.peek_for_punctuation { 
                Punctuation::Comma => continue,
                Punctuation::GenericClose => return Ok(args), 
                _ => return Err(stream.next()?.as_err_unexpected())
            })
        }
    }
}

impl GenericIdentifier { 
    /// Helper function for unit tests
    #[cfg(test)]
    pub(crate) fn simple(base: &'static str) -> Self { 
        Self { base: base.to_string(), args: GenericArgs::default() }
    }

    pub fn has_modifiers(&self) -> bool { 
        !self.args.is_empty()
        //TAG: RC_FLAGS
        // https://www.notion.so/veneto/RC-Flags-conditionals-c1ed70a794d14511b57e06d1798a62a8?pvs=4
    }
}


#[cfg(test)]
mod test {
    use crate::parse::{lexer_tests::{token_stream, assert_punctuation}, ParseResult, lexer::TokenStream, tokens::Punctuation, ast::Expectable};

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
            GenericIdentifier { 
                base: $base.to_owned(), 
                args: vec![ $(gid!($params)),+ ]
            }
        };

        ($base:ident) => { 
            $base
        };

        ($base:literal) => {
            GenericIdentifier { 
                base: $base.to_owned(), 
                args: vec![],
            }
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
}