use crate::parse::ParseResult;
use crate::parse::lexer::{TokenStream, Span};
use crate::parse::tokens::{Punctuation, Terminal, Token, Identifier};

use super::{Peekable, Expectable, Finishable, Spanned};

/// This is an identifier that can accept generic parameters.
#[derive(PartialEq, Eq, Debug)]
pub struct GenericIdentifier { 
    pub base: String, 
    pub args: Option<GenericArgs>,
    pub span: Span, 

    //TAG: RC_FLAGS
    // https://www.notion.so/veneto/RC-Flags-conditionals-c1ed70a794d14511b57e06d1798a62a8?pvs=4
}
pub type GenericArgs = Spanned<Vec<GenericIdentifier>>; 

impl GenericIdentifier { 
    fn parse_finish(stream: &mut TokenStream, base: Identifier) -> ParseResult<Self> { 

        let args = GenericArgs::parse_peek(stream)?; 

        let span : Span; 
        if let Some(ref args) = args { span = base.span.through(args.span); }
        else { span = base.span; }

        Ok(GenericIdentifier { base: base.text, args, span })

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

    fn parse_finish(stream: &mut TokenStream, initial: Token) -> ParseResult<Self> {
        let mut args = Vec::new(); 
        loop { 
            if let Some(arg) = GenericIdentifier::parse_peek(stream)? {
                args.push(arg);
            }

            let next = stream.next()?; 
            match next.as_punctuation() { 
                Some(Punctuation::Comma) => continue,
                Some(Punctuation::GenericClose) => return Ok(Spanned { 
                    span: initial.span.through(next.span),
                    node: args, 
                }), 
                _ => return Err(next.as_err_unexpected())
            }
        }
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

    macro_rules! assert_gid {
        ($gid:ident, $base:literal) => { 
            assert_eq!($gid.base, $base);
            assert_eq!($gid.args, None); 
        };

        ($gid:ident, $base:literal < $($params:tt),+ > ) => { 
            assert_eq!($gid.base, $base);
            
            let args = $gid.args.unwrap();
            let mut iter = args.node.iter(); 
            $(
                let next = iter.next().unwrap();
                assert_gid!(next, $params);
            )+
            assert_eq!(iter.next(), None); 
        };
    }

 
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
        assert_eq!(gid.base, "foo");
        
        let args = gid.args.unwrap();
        let mut iter = args.node.iter(); 
        
        assert_eq!(iter.next().unwrap().base, "bar"); 

        let next = iter.next().unwrap(); 
        assert_eq!(next.base, "baz"); 

        let inner_args = next.args.as_ref().unwrap();
        let mut inner_iter = inner_args.node.iter(); 
        assert_eq!(inner_iter.next().unwrap().base, "asdf"); 
        assert_eq!(inner_iter.next().unwrap().base, "ree");
        assert_eq!(inner_iter.next(), None); 

        assert_eq!(iter.next().unwrap().base, "aaa"); 

        assert_eq!(iter.next(), None); 
        assert_punctuation(&mut stream, Punctuation::Comma);
    }
}