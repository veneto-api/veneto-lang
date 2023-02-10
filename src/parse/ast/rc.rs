use std::collections::HashMap;
use std::collections::hash_map::Entry;
use std::str::FromStr;

use strum_macros::{EnumString, Display};

use crate::parse::{ ParseResult, ParseErrorKind }; 
use crate::parse::lexer::TokenStream; 
use crate::parse::tokens::{ Punctuation, Keyword, Number }; 
use crate::parse::ast::general::GenericIdentifier;

use super::{Expectable, types::Type, interfaces::InterfaceExpression, Peekable};

/// The declaration clause of a Resource Class,
/// including its identifier and whether or not it was extended.
/// 
/// This exists to handle parameters (generics, and flags in the future) in the declaration - 
/// currently we have the limitation that flags cannot be defined for extended Resource Classes
/// 
/// TAG: RC_FLAGS
pub enum RCDeclaration { 
    /// A non-extended Resource Class, which may have parameters
    Basic(RCIdentifier), 

    /// An extended resource class;
    /// the `String` is its identifier (it may not have parameters),
    /// the `RCIdentifier` references the extended RC (it may have parameters),
    Extended(String, RCIdentifier), 
}


/// The body of a resource class,
/// beginning with an open brace
pub struct ResourceClass { 
    /// The declaration for this resource class,
    /// including its parameter information
    pub declaration: RCDeclaration, 

    /// The base data type declared with the `data` keyword
    /// 
    /// (This cannot be a reference to another RC at this time)
    pub data : Option<Type>, 

    /// The interface expresssion (reference or literal) declared for this RC
    pub interface : Option<InterfaceExpression>,

    pub links : Option<LinksBlock>,

    pub methods : HashMap<MethodName, Method>,
}
impl Peekable for ResourceClass { 
    fn parse_peek(stream: &mut TokenStream) -> ParseResult<Option<Self>> {
        if stream.peek_for_keyword(Keyword::Resource)? { 
 
            let declaration = { 
                let ident = RCIdentifier::parse_expect(stream)?; 

                if stream.peek_for_keyword(Keyword::Extends)? { 

                    if ident.has_modifiers() { 
                        let err_ref = stream.peek()?; 
                        return Err(err_ref.as_semantic_error("Resource classes cannot have modifiers if they are extended"))
                    }
                    else { 
                        RCDeclaration::Extended(ident.base, RCIdentifier::parse_expect(stream)?)
                    }
    
                }
                else { RCDeclaration::Basic(ident) }
            };

            let mut data : Option<Type> = None; 
            let mut interface : Option<InterfaceExpression> = None;
            let mut links : Option<LinksBlock> = None; 
            let mut methods = HashMap::<MethodName, Method>::new(); 

            stream.next()?.expect_punctuation(Punctuation::BraceOpen)?; 
            loop { 

                if stream.peek_for_keyword(Keyword::Data)? { 
                    data = Some(Type::parse_expect(stream)?);
                }
                else if stream.peek_for_keyword(Keyword::Interface)? { 
                    interface = Some(InterfaceExpression::parse_expect(stream)?);
                }
                else if stream.peek_for_keyword(Keyword::Links)? { 
                    links = Some(LinksBlock::parse_expect(stream)?);
                }
                else if stream.peek_for_puncutation(Punctuation::BraceClose)? { 
                    break
                }
                else { 
                    let err_ref = stream.peek()?; 
                    let method = Method::parse_expect(stream)?; 
                    match methods.entry(method.name) { 
                        Entry::Occupied(_) => { 
                            return Err(err_ref.as_err(ParseErrorKind::SemanticDuplicate))
                        },
                        Entry::Vacant(entry) => { 
                            entry.insert(method);
                        }
                    }
                }

            }

            Ok(Some(ResourceClass { declaration, data, interface, links, methods }))
        }
        else { Ok(None) }
    }
}




//
// Identifiers
//

#[derive(PartialEq, Eq, Debug)]
pub struct RCIdentifier { 
    base: String, 
    generics: Vec<RCIdentifier>, 
    //TAG: RC_FLAGS
    // https://www.notion.so/veneto/RC-Flags-conditionals-c1ed70a794d14511b57e06d1798a62a8?pvs=4
}
impl RCIdentifier { 
    fn has_modifiers(&self) -> bool { 
        //TAG: RC_FLAGS 
        !self.generics.is_empty()
    }
}
impl From<RCIdentifier> for GenericIdentifier { 
    fn from(ident: RCIdentifier) -> Self {
        fn convert(rcid: RCIdentifier) -> GenericIdentifier { 
            if rcid.generics.is_empty() { 
                GenericIdentifier::Simple(rcid.base)
            } else { 
                GenericIdentifier::Generic(
                    rcid.base, 
                    rcid.generics.into_iter().map(convert).collect(),
                )
            }
        }

        convert(ident)
    }
}
impl Expectable for RCIdentifier { 
    fn parse_expect(stream: &mut TokenStream) -> ParseResult<Self> {
        
        //TAG: RC_FLAGS 
        // Until we add flags, this is just the same as a `GenericIdentifier`, so we just convert it here 
        fn convert(gid: GenericIdentifier) -> RCIdentifier { 
            match gid { 
                GenericIdentifier::Simple(base) => RCIdentifier { base, generics: Vec::new() },
                GenericIdentifier::Generic(base, generics) => RCIdentifier { 
                    base, 
                    generics: generics.into_iter().map(convert).collect(),
                }
            }
        }

        Ok(convert(GenericIdentifier::parse_expect(stream)?))
    }
}

//
// ========
// Contents
// ========
// 

//
// Links
//

pub type LinksBlock = Vec<Link>;
pub struct Link { 
    pub rel: String, 
    pub optional: bool, 
    pub typ: RCIdentifier,
    //TAG: DYNAMIC_LINKS
    // That sum type would go here
    // https://veneto.notion.site/Custom-dynamic-link-serialization-957c6c655cc44cddada94a490663cebc
} 

impl Peekable for Link { 
    fn parse_peek(stream: &mut TokenStream) -> ParseResult<Option<Self>> {
        if let Some(rel) = stream.peek_for_identifier()? { 

            let optional = stream.peek_for_puncutation(Punctuation::Optional)?; 

            //TAG: DYNAMIC_LINKS
            stream.next()?.expect_punctuation(Punctuation::Arrow)?; 

            Ok(Some(Link { 
                rel, 
                optional, 
                typ: RCIdentifier::parse_expect(stream)?,
            }))
        }
        else { Ok(None) }
    }
}

impl Expectable for LinksBlock { 
    fn parse_expect(stream: &mut TokenStream) -> ParseResult<Self> {
        stream.next()?.expect_punctuation(Punctuation::BraceOpen)?; 

        let mut links = Vec::<Link>::new(); 
        loop { 
            if let Some(link) = Link::parse_peek(stream)? { 
                links.push(link); 
            }

            let next = stream.next()?; 
            match next.as_punctuation() { 
                Some(Punctuation::Comma) => continue, 
                Some(Punctuation::BraceClose) => return Ok(links), 
                _ => return Err(next.as_err_unexpected()),
            }
        }
    }
}

//
// Methods
// 

#[derive(EnumString, Debug, Display, PartialEq, Eq, Hash, Clone, Copy)]
#[strum(serialize_all="UPPERCASE")]
pub enum MethodName { 
    Get, 
    Post, 
    Patch,
    Put, 
    Delete,
}


#[derive(Debug, PartialEq)]
pub struct Method { 
    /// The name (HTTP verb) of the method.
    /// 
    /// This is extraneous when accessed outside the parser,
    /// as its current representation in the `ResourceClass` lies within a map from its name.
    /// That's kinda stupid, but it seems convenient for the time being.  We'll see
    pub name : MethodName, 

    pub input : Option<MethodInput>, 
    pub outputs : Vec<MethodOutput>, 
}
#[derive(Debug, PartialEq)]
pub struct MethodInput { 
    typ: RCType, 
    lax: bool, 
}
#[derive(Debug, PartialEq)]
pub struct MethodOutput { 
    typ: Option<RCType>, 
    status: Option<Number>
    //TAG: REASON_PHRASES
    // https://veneto.notion.site/HTTP-reason-phrases-instead-of-codes-772c7fc3d21146aa922573913c8adc35
}

impl Expectable for Method { 
    fn parse_expect(stream: &mut TokenStream) -> ParseResult<Self> {
        let name = stream.next()?;
        let name = MethodName::from_str(&name.try_as_identifier()?)
            .map_err(|_| name.as_err(ParseErrorKind::UnknownMethodName))?; 
            

        // First, try to grab the input 

        let mut input : Option<MethodInput> = None;
        

        let err_ref = stream.peek()?; 
        if let Some(typ) = RCType::parse_peek(stream)? { 
            if name == MethodName::Get { 
                return Err(err_ref.as_semantic_error("GET requests cannot have a request body"))
            }

            input = Some(MethodInput { 
                typ, 
                lax: stream.peek_for_puncutation(Punctuation::Lax)?,
            });
        }

        // Now let's handle outputs

        let mut outputs = Vec::<MethodOutput>::new(); 
        if stream.peek_for_puncutation(Punctuation::Arrow)? { 
            loop { 

                let mut status : Option<Number> = None; 
                if stream.peek_for_puncutation(Punctuation::HttpStatus)? { 
                    status = Some(stream.next()?.try_as_number()?);
                }

                let typ = RCType::parse_peek(stream)?; 

                if typ.is_none() && status.is_none() { 
                    // It kind of feels like cheating to just peek the next token for the error location here,
                    // but it works I guess 
                    return Err(stream.peek()?.as_semantic_error("Method outputs must have a status and/or a type"))
                }
                outputs.push(MethodOutput { typ, status });


                if stream.peek_for_puncutation(Punctuation::Comma)? { continue }
                else { break } 
            }
        }

        Ok(Self { name, input, outputs })
        
    }
}

//
// Misc helpers
//

/// These are the spcecial types usable within resource classes
/// 
/// https://veneto.notion.site/Special-data-types-d8aab2afcd4d480b988c4b9ec23ec950
#[derive(EnumString, Debug, Display, PartialEq, Eq)]
#[strum(serialize_all="lowercase")]
pub enum SpecialType { 
    #[strum(serialize="self")]
    RCSelf, 
    Media,
    Empty, 
}

/// This represents a type expression within the context of a resource class;
/// it contains a normal type or an `RCSpecialType`
#[derive(Debug, PartialEq)]
pub enum RCType { 
    Normal(Type),
    Special(SpecialType), 
    //TAG: RC_FLAGS
    // When we add RC flags, `Normal` may have to be changed
}

impl Expectable for RCType { 
    fn parse_expect(stream: &mut TokenStream) -> ParseResult<Self> {
        if stream.peek_for_puncutation(Punctuation::SpecialType)? { 
            let next = stream.next()?;
            match SpecialType::from_str(&next.try_as_identifier()?) { 
                Ok(typ) => Ok(RCType::Special(typ)),
                Err(_) => Err(next.as_err(ParseErrorKind::UnknownSpecialType))
            }
        } 
        else { 
            Type::parse_expect(stream).map(RCType::Normal)
        }
    }
}
impl Peekable for RCType { 
    fn parse_peek(stream: &mut TokenStream) -> ParseResult<Option<Self>> {
        if stream.peek()?.is_identifier() { 
            Ok(Some(Self::parse_expect(stream)?))
        } else { 
            Ok(None) 
        }
    }
}


#[cfg(test)]
mod test {
    use crate::parse::ast::types::{ TypeKind };
    use crate::parse::ast::types::test::make_simple_type;
    use crate::parse::ast::{ general::GenericIdentifier };
    use crate::parse::lexer_tests::token_stream;
    use crate::parse::{ParseResult, TestUnwrap};
    use crate::parse::ast::{Expectable};

    use super::{Method, RCType, MethodOutput, MethodInput};

    fn make_simple_rc_type(kind: TypeKind) -> RCType { 
        RCType::Normal(make_simple_type(kind))
    }
    fn make_ident_type(name: &str) -> RCType { 
        RCType::Normal(make_simple_type(TypeKind::Identifier(GenericIdentifier::Simple(name.to_string()))))
    }
 

    fn parse_method(input: &str) -> ParseResult<Method> { 
        let mut stream = token_stream(input); 
        Method::parse_expect(&mut stream)
    }
    fn assert_method(input: &str, expected: Method) { 
        assert_eq!( parse_method(input).test_unwrap(), expected );
    }

    #[test]
    fn simple_method() { 
        assert_method("POST foo -> #200 bar", Method { 
            name: super::MethodName::Post, 
            input: Some(MethodInput { 
                typ: make_ident_type("foo"), 
                lax: false, 
            }), 
            outputs: vec![ 
                MethodOutput { 
                    typ: Some(make_ident_type("bar")), 
                    status: Some("200".to_string()), 
                }
            ]
        })
    }


}