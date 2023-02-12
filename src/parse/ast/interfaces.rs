use std::str::FromStr;

use strum_macros::{ Display, EnumString };

use crate::parse::{lexer::TokenStream, ParseResult, tokens::{Punctuation, Terminal}, ParseErrorKind};

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
}

/// The body of an Interface literal
pub type InterfaceBody = Vec<InterfaceField>; 

/// Either an interface literal or an identifier referencing another interface
#[derive(Debug, PartialEq, Eq)]
pub enum InterfaceExpression { 
    Identifier(String), 
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

    fn parse_finish(stream: &mut TokenStream) -> ParseResult<Self> {
        let mut fields = Self::new(); 
        loop { 
            if let Some(name) = stream.peek_for_identifier()? { 
                let optional = stream.peek_for_puncutation(Punctuation::Optional)?; 
                stream.next()?.expect_punctuation(Punctuation::Colon)?; 

                let typ = stream.next()?;
                let typ = typ
                    .try_as_identifier()
                    .and_then(|s| 
                        InterfaceValueType::from_str(&s).map_err(|_| typ.as_err(ParseErrorKind::UnknownInterfaceValueType))
                    )?;

                fields.push(InterfaceField { name, typ, optional });
            }

            let next = stream.next()?; 
            match next.as_punctuation() { 
                Some(Punctuation::Comma) => continue, 
                Some(Punctuation::BraceClose) => return Ok(fields), 
                _ => return Err(next.as_err_unexpected())
    ,        }
        }
    }
}


#[cfg(test)]
mod test {
    use crate::parse::{ParseResult, lexer::TokenStream, ast::Expectable};

    use super::{InterfaceBody, InterfaceField, InterfaceValueType};

    fn parse_interface(input: &str) -> ParseResult<InterfaceBody> { 
        let mut stream = TokenStream::new(input.chars()); 
        InterfaceBody::parse_expect(&mut stream)
    }

    fn assert_interface(input: &str, expected: InterfaceBody) { 
        let i = parse_interface(input).unwrap(); 
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