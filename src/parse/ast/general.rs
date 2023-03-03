use crate::parse::ParseResult;
use crate::parse::lexer::{TokenStream};
use crate::parse::tokens::{Punctuation, Terminal, Token};

use super::{Peekable, Expectable, Finishable, Spanned};

/// This is an identifier that can accept generic parameters.
#[derive(PartialEq, Eq, Debug)]
pub struct GenericIdentifier { 
    pub base: Spanned<String>, 
    pub args: Option<GenericArgs>,

    //TAG: RC_FLAGS
    // https://www.notion.so/veneto/RC-Flags-conditionals-c1ed70a794d14511b57e06d1798a62a8?pvs=4
}
pub type GenericArgs = Vec<GenericIdentifier>; 

impl GenericIdentifier { 
    fn parse_finish(stream: &mut TokenStream, base: Spanned<String>) -> ParseResult<Self> { 

        let args = GenericArgs::parse_peek(stream)?; 

        Ok(GenericIdentifier { base, args })

    }
}
impl Peekable for GenericIdentifier {
    fn parse_peek(stream: &mut TokenStream) -> ParseResult<Option<Self>> {
        if let Some(base) = stream.peek_for_identifier()? {
            Self::parse_finish(stream, base).map(Some)
        } else { 
            Ok(None)
        }
    }
}
impl Expectable for GenericIdentifier { 
    fn parse_expect(stream: &mut TokenStream) -> ParseResult<Self> {
        let base = stream.next()?.try_as_identifier()?;
        Self::parse_finish(stream, base)
    }
}


impl Finishable for GenericArgs { 
    const INITIAL_TOKEN: Terminal = Terminal::Punctuation(Punctuation::GenericOpen);

    fn parse_finish(stream: &mut TokenStream, _: Token) -> ParseResult<Self> {
        let mut args = Vec::new(); 
        loop { 
            if let Some(arg) = GenericIdentifier::parse_peek(stream)? {
                args.push(arg);
            }

            let next = stream.next()?; 
            match next.as_punctuation() { 
                Some(Punctuation::Comma) => continue,
                Some(Punctuation::GenericClose) => return Ok(args), 
                _ => return Err(next.as_err_unexpected())
            }
        }
    }
}

#[cfg(test)]
pub mod test {
    use crate::parse::{lexer_tests::{token_stream, assert_punctuation}, ParseResult, lexer::{TokenStream}, tokens::Punctuation, ast::Expectable};

    use super::GenericIdentifier;

    fn parse_gid(input: &str) -> ParseResult<(TokenStream, GenericIdentifier)>{ 
        let mut stream = token_stream(input); 
        GenericIdentifier::parse_expect(&mut stream).map(
            |res| (stream, res) 
        )
    }

    macro_rules! assert_gid {
        ($gid:ident, $base:literal) => { 
            assert_eq!($gid.base.node, $base);
            assert_eq!($gid.args, None); 
        };

        ($gid:ident, $base:literal < $($params:tt),+ > ) => { 
            assert_eq!($gid.base.node, $base);
            
            { 
                let args = $gid.args.unwrap();
                let mut iter = args.iter(); 
                $(
                    let next = iter.next().unwrap();
                    assert_gid!(next, $params);
                )+
                assert_eq!(iter.next(), None); 
            }
        };
    }

    macro_rules! assert_gid_base {
        ($gid:ident: $base:literal @ $pos:literal) => {
            assert_eq!($gid.base.node, $base);
            assert_eq!($gid.base.span, $crate::parse::Span { 
                lo: $crate::parse::lexer::Position($pos), 
                hi: $crate::parse::lexer::Position(($pos + $base.len()).try_into().unwrap()) 
            })
        };
    }

    pub(crate) use assert_gid; 
    pub(crate) use assert_gid_base; 
 
    #[test]
    fn simple() { 
        let (mut stream, gid) = parse_gid("foo,").unwrap(); 
        assert_gid!(gid, "foo");
        assert_punctuation(&mut stream, Punctuation::Comma); 
    }

    #[test] 
    fn single_param() { 
        let (_, gid) = parse_gid("foo<bar>").unwrap(); 
        assert_gid!(gid, "foo" < "bar" >);
    }

    #[test]
    fn multi_param() { 
        let (_, gid) = parse_gid("foo<bar, baz>").unwrap();
        assert_gid!(gid, "foo" < "bar" , "baz" >);
    }

    #[test]
    fn complex() { 
        let (mut stream, gid) = parse_gid("foo<bar, baz<asdf, ree>, aaa>,").unwrap();
        assert_eq!(gid.base.node, "foo");
        
        let args = gid.args.unwrap();
        let mut iter = args.iter(); 
        
        assert_eq!(iter.next().unwrap().base.node, "bar"); 

        let next = iter.next().unwrap(); 
        assert_eq!(next.base.node, "baz"); 

        let inner_args = next.args.as_ref().unwrap();
        let mut inner_iter = inner_args.iter(); 

        let next = inner_iter.next().unwrap(); 
        assert_gid_base!(next: "asdf" @ 13);

        let next = inner_iter.next().unwrap(); 
        assert_gid_base!(next: "ree" @ 19); 

        assert_eq!(inner_iter.next(), None); 

        let next = iter.next().unwrap(); 
        assert_gid_base!(next: "aaa" @ 25); 

        assert_eq!(iter.next(), None); 
        assert_punctuation(&mut stream, Punctuation::Comma);
    }
}