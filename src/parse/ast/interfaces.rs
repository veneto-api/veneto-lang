use std::{str::FromStr, backtrace::Backtrace};

use strum_macros::{ Display, EnumString };

use crate::parse::{lexer::{TokenStream, Span}, ParseResult, tokens::{Punctuation, Terminal, Token, Identifier}, ParseErrorKind, ParseError};

use super::{Peekable, Expectable}; 

#[derive(Display, Debug, EnumString, PartialEq, Eq)]
#[strum(serialize_all="lowercase")]
pub enum InterfaceValueType { 
    String, 
}

#[derive(Debug, PartialEq, Eq)]
pub struct InterfaceField { 
    pub name: String, 

    /// The type of the field value, 
    /// named `typ` because `type` is a reserved Rust keyword 
    pub typ: InterfaceValueType,

    /// True if the field is marked as optional with a `?` before the `:`. 
    pub optional: bool, 

    pub span: Span, 
}

/// The body of an Interface literal
#[derive(Debug, PartialEq, Eq)]
pub struct InterfaceBody {
    fields: Vec<InterfaceField>, 
    span: Span, 
}

/// Either an interface literal or an identifier referencing another interface
#[derive(Debug, PartialEq, Eq)]
pub enum InterfaceExpression { 
    Identifier(Identifier), 
    Literal(InterfaceBody), 
}
impl Expectable for InterfaceExpression { 
    fn parse_expect(stream: &mut TokenStream) -> ParseResult<Self> {
        if let Some(lit) = InterfaceBody::parse_peek(stream)? { 
            Ok(InterfaceExpression::Literal(lit))
        } else { 
            stream.next()?.try_as_identifier().map(InterfaceExpression::Identifier)
        }
    }
}
impl super::Finishable for InterfaceBody { 
    const INITIAL_TOKEN: Terminal = Terminal::Punctuation(Punctuation::BraceOpen);

    fn parse_finish(stream: &mut TokenStream, initial: Token) -> ParseResult<Self> {
        let mut fields = Vec::<InterfaceField>::new(); 
        loop { 
            if let Some(name) = stream.peek_for_identifier()? { 
                let optional = stream.peek_for_punctuation(Punctuation::Optional)?; 
                stream.next()?.expect_punctuation(Punctuation::Colon)?; 

                let typ = stream.next()?.try_as_identifier()?;
                let typ_span = typ.span; 
                let Ok(typ) = InterfaceValueType::from_str(&typ.text) else { 
                    return Err(ParseError{ 
                        kind: ParseErrorKind::UnknownInterfaceValueType, 
                        span: typ_span, 
                        backtrace: Backtrace::capture(), 
                    })
                };

                fields.push(InterfaceField { 
                    name: name.text, 
                    typ, 
                    optional,
                    span: name.span.through(typ_span),
                });
            }

            let next = stream.next()?; 
            match next.as_punctuation() { 
                Some(Punctuation::Comma) => continue, 
                Some(Punctuation::BraceClose) => return Ok(InterfaceBody { 
                    fields, 
                    span: initial.span.through(next.span),
                }), 
                _ => return Err(next.as_err_unexpected())
    ,        }
        }
    }
}


#[cfg(test)]
mod test {
    use crate::parse::{ParseResult, lexer::TokenStream, ast::Expectable};

    use super::InterfaceBody;

    fn parse_interface(input: &str) -> ParseResult<InterfaceBody> { 
        let mut stream = TokenStream::new(input.chars()); 
        InterfaceBody::parse_expect(&mut stream)
    }

    fn assert_interface(input: &str, expected: &[(&str, bool)]) { 
        let i = parse_interface(input).unwrap(); 
        let mut fields = i.fields.iter(); 

        for pair in expected { 
            let field = fields.next().unwrap(); 
            assert_eq!(pair.0, field.name);
            assert_eq!(pair.1, field.optional);
        }

        assert_eq!(fields.next(), None); 
    }

    #[test]
    fn empty() { 
        let interface = parse_interface("{}").unwrap();
        assert!(interface.fields.is_empty()); 
    }

    #[test]
    fn simple() { 
        assert_interface("{ foo: string }", &[ 
            ("foo", false)
        ])
    }

    #[test]
    fn optional() { 
        assert_interface("{  asdf? : string }", &[ 
            ("asdf", true)
        ])
    }

    #[test]
    fn complex() { 
        assert_interface(" {foo: string  , bar?: string, baz  : string}", &[ 
            ("foo", false),
            ("bar", true),
            ("baz", false),
        ])
    }


}