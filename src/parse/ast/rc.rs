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

/// This represents the output specification of a method - its response type and status.
/// 
/// Both of those things are optional, and their defaults will be specified in documentation.
/// However, the parser currently does not allow them both to be empty.  
/// This might get converted to a sum type in the future if we need to guarantee that for type safety,
/// but since they both have defaults it's staying like this for now 
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


        // First, check for input type
        // Types can't be peeked, so we have to use other context to determine whether or not to expect one 
        // If there's not an arrow yet, expect the input type, then expect an arrow or semicolon afterwards
        let mut input : Option<MethodInput> = None;
        if !stream.peek_for_puncutation(Punctuation::Arrow)? { 

            let err_ref = stream.peek()?; 
            let typ = RCType::parse_expect(stream)?;

            if [ MethodName::Get, MethodName::Delete ].contains(&name) { 
                return Err(err_ref.as_semantic_error("This request type cannot have a request body"))
            }

            input = Some(MethodInput { 
                typ, 
                lax: stream.peek_for_puncutation(Punctuation::Lax)?,
            });

            let next = stream.next()?;
            match next.as_punctuation() { 
                Some(Punctuation::Arrow) => (), // If it's an arrow, continue on below 
                Some(Punctuation::Semicolon) => { 
                    // If it's a semicolon, we're done here
                    return Ok(Method { name, input, outputs: vec![] })
                },
                _ => return Err(next.as_err_unexpected())
            }
        }


        // Now let's handle outputs 
        // We expect them to be here because it's guaranteed above 
        // A sanity check here would be nice but we don't currently have the means to do that really 
        let mut outputs = Vec::<MethodOutput>::new(); 

        loop { 
            // First check for a status.  
            // This is easy since we can just peek for it 
            let mut status : Option<Number> = None; 
            if stream.peek_for_puncutation(Punctuation::HttpStatus)? { 
                status = Some(stream.next()?.try_as_number()?);
            }

            // Now we have to handle types again, which can't be peeked.  Same deal
            let mut typ : Option<RCType> = None; 
            let peek = stream.peek()?; 
            if !(
                peek.as_punctuation() == Some(Punctuation::Comma) || 
                peek.as_punctuation() == Some(Punctuation::Semicolon)
            ) { 
                // If there's not an end-of-clause marker (comma or semicolon), expect the type 
                typ = Some(RCType::parse_expect(stream)?); 
            }
            else if status.is_none() { 
                // Otherwise, there is no type.
                // However, we have to raise this semantic error if there's no status either
                return Err(peek.as_semantic_error("Method outputs must have a status and/or a type"))
            }

            outputs.push(MethodOutput { typ, status });

            let next = stream.next()?; 
            match next.as_punctuation() { 
                Some(Punctuation::Comma) => continue,
                Some(Punctuation::Semicolon) => return Ok(Method { name, input, outputs }),
                _ => return Err(next.as_err_unexpected())
            }
        }
        
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

impl Expectable for SpecialType { 
    fn parse_expect(stream: &mut TokenStream) -> ParseResult<Self> {
        let next = stream.next()?;
        SpecialType::from_str(&next.try_as_identifier()?).map_err(|_| next.as_err(ParseErrorKind::UnknownSpecialType))
    }
}

/// This represents a type expression within the context of a resource class;
/// it contains a normal type or an `RCSpecialType`.
/// 
/// This is not `Peekable` because `Type` is not `Peekable`!
/// That's mainly because there's lots of different token types that can mark the beginning of a `Type` - 
/// it could, in theory, be possible to implement `Peekable for Type` but that introduces a lot of room for error.
/// It's better to just work our way around that as long as it's feasible to do so
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
            SpecialType::parse_expect(stream).map(RCType::Special)
        } 
        else { 
            Type::parse_expect(stream).map(RCType::Normal)
        }
    }
}


#[cfg(test)]
mod test {
    use crate::parse::ast::types::{ TypeKind };
    use crate::parse::ast::types::test::make_simple_type;
    use crate::parse::ast::{ general::GenericIdentifier };
    use crate::parse::lexer_tests::token_stream;
    use crate::parse::{ParseResult, TestUnwrap, ParseErrorKind};
    use crate::parse::ast::{Expectable};

    use super::{Method, RCType, MethodOutput, MethodInput, MethodName, SpecialType};

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

    //
    // Methods
    //

    #[test]
    fn method_complete() { 
        assert_method("POST foo -> #200 bar;", Method { 
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

    #[test]
    fn method_empty_bodies() { 
        assert_method("POST -> @empty;", Method { 
            name: MethodName::Post,
            input: None, 
            outputs: vec![ 
                MethodOutput { 
                    typ: Some(RCType::Special(SpecialType::Empty)),
                    status: None, 
                }
            ]
        })
    }

    #[test]
    fn implied_response() { 
        assert_method("PATCH foo -> bar;", Method { 
            name: MethodName::Patch, 
            input: Some(MethodInput { 
                typ: make_ident_type("foo"), 
                lax: false, 
            }), 
            outputs: vec![ 
                MethodOutput { 
                    typ: Some(make_ident_type("bar")), 
                    status: None, 
                }
            ]
        });

        assert_method("POST foo -> #204;", Method { 
            name: MethodName::Post, 
            input: Some(MethodInput { 
                typ: make_ident_type("foo"), 
                lax: false, 
            }), 
            outputs: vec![ 
                MethodOutput { 
                    typ: None, 
                    status: Some("204".to_string()), 
                }
            ]
        });
    }

    #[test]
    fn err_empty_response() { 
        let err = parse_method("POST foo ->");
        assert!(err.is_err());
    }

    #[test]
    fn err_get_body() { 
        let err = parse_method("GET foo -> #200;").unwrap_err();
        assert!(matches!(err.kind, ParseErrorKind::Semantic(_)));
    }

}