use std::str::FromStr;

use strum_macros::{ Display, EnumString };

use crate::parse::{lexer::TokenStream, ParseResult, tokens::Punctuation, ParseErrorKind};

use super::{Peekable, Expectable}; 

#[derive(Display, Debug, EnumString, PartialEq, Eq)]
#[strum(serialize_all="lowercase")]
pub enum InterfaceValueType { 
    String, 
}

#[derive(Debug, PartialEq, Eq)]
pub struct InterfaceField { 
    name: String, 

    /// The type of the field value, 
    /// named `typ` because `type` is a reserved Rust keyword 
    typ: InterfaceValueType,

    /// True if the field is marked as optional with a `?` before the `:`. 
    optional: bool, 
}

/// The body of an Interface literal
type Interface = Vec<InterfaceField>; 

pub fn finish_interface(stream: &mut TokenStream) -> ParseResult<Interface> { 
    let mut fields = Vec::<InterfaceField>::new(); 

    loop { 
        let mut next = stream.next()?; 

        if let Some(name) = next.as_identifier() { 
            let mut optional = false;

            if stream.peek()?.as_punctuation() == Some(Punctuation::Optional) { 
                stream.next()?; 
                optional = true;
            }

            stream.next()?.expect_punctuation(Punctuation::Colon)?; 

            let typ = stream.next()?;
            let typ = typ
                .try_as_identifier()
                .and_then(|s| 
                    InterfaceValueType::from_str(&s).map_err(|_| typ.as_err(ParseErrorKind::UnknownInterfaceValueType))
                )?;


            fields.push(InterfaceField { name, typ, optional });
            next = stream.next()?; 
        }

        match next.as_punctuation() { 
            Some(Punctuation::Comma) => continue, 
            Some(Punctuation::BraceClose) => return Ok(fields), 
            _ => return Err(next.as_err_unexpected())
,        }
    }
}

impl Peekable for Interface { 
    fn parse_peek(stream: &mut TokenStream) -> ParseResult<Option<Self>> {
        if stream.peek_for_puncutation(Punctuation::BraceOpen)? { 
            Ok(Some(finish_interface(stream)?))
        }
        else { 
            Ok(None)
        }
    }
}
impl Expectable for Interface { 
    fn parse_expect(stream: &mut TokenStream) -> ParseResult<Self> {
        stream.next()?.expect_punctuation(Punctuation::BraceOpen)?; 
        finish_interface(stream)
    }
}


#[cfg(test)]
mod test {
    use crate::parse::{ParseResult, lexer::TokenStream, tokens::Punctuation};
    use crate::parse::TestUnwrap; 

    use super::{Interface, finish_interface, InterfaceField, InterfaceValueType};

    fn parse_interface(input: &str) -> ParseResult<Interface> { 
        let mut stream = TokenStream::new(input.chars()); 
        stream.next()?.expect_punctuation(Punctuation::BraceOpen)?;
        finish_interface(&mut stream)
    }

    fn assert_interface(input: &str, expected: Interface) { 
        let i = parse_interface(input).test_unwrap(); 
        assert_eq!(i, expected);
    }

    fn make_ifield(name: &'static str, optional: bool ) -> InterfaceField { 
        InterfaceField { name: name.to_string(), typ: InterfaceValueType::String, optional }
    }

    #[test]
    fn empty() { 
        assert_interface("{}", vec![]);
    }

    #[test]
    fn simple() { 
        assert_interface("{ foo: string }", vec![ 
            make_ifield("foo", false)
        ])
    }

    #[test]
    fn optional() { 
        assert_interface("{  asdf? : string }", vec![ 
            make_ifield("asdf", true)
        ])
    }

    #[test]
    fn complex() { 
        assert_interface(" {foo: string  , bar?: string, baz  : string}", vec![ 
            make_ifield("foo", false),
            make_ifield("bar", true),
            make_ifield("baz", false),
        ])
    }


}