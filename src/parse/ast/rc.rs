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
impl ResourceClass { 
    pub fn default_status(data_is_some: bool, verb: MethodName) -> Number { 
        match verb { 
            MethodName::Get => { 
                if data_is_some { 200.to_string() }
                else { 405.to_string() }
            },
            _ => 200.to_string(),
        }
    }
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

                    // Bonus check for implied duplicate method names
                    // This is an edge case - explicit duplicate names are caught in `Method::parse_expect`, 
                    // but implied names can't be caught there since the default is determined by the `data` field for GET requests
                    // https://veneto.notion.site/Incorrect-position-for-duplicate-implied-method-verb-502f7ba853b442bd80179f6db7af3776
                    //TAG: DUPLICATE_IMPLIED_VERB
                    let default_status = Self::default_status(data.is_some(), method.name);
                    if method.outputs.contains_key(&None) && method.outputs.contains_key(&Some(default_status.clone())) { 
                        return Err(err_ref.as_err(ParseErrorKind::Semantic(
                            format!("Duplicate output for default status {default_status} (one of them is implied)")
                        )))
                    }
                    // some funny borrow checker shenanigans going on here 

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

#[derive(PartialEq, Eq, Debug)]
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
            let err_ref = stream.peek()?; 
            if let Some(link) = Link::parse_peek(stream)? { 
                // Bonus check: Duplicate link refs
                if links.iter().any(|other| other.rel == link.rel) { 
                    return Err(err_ref.as_err(ParseErrorKind::SemanticDuplicate))
                }

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


    /// This represents the outputs of a method - 
    /// mapping their status to their response type.  
    /// 
    /// Both of those things are optional, and their defaults will be specified in documentation.
    /// However, the parser currently does not allow them both to be empty.  
    /// Therefore, the None -> None pair will never exist.
    /// This might get converted to some other type in the future if we need to guarantee that for type safety,
    /// but since they both have defaults it's staying like this for now 
    ///TAG: REASON_PHRASES
    /// https://veneto.notion.site/HTTP-reason-phrases-instead-of-codes-772c7fc3d21146aa922573913c8adc35
    pub outputs : HashMap<Option<Number>, Option<RCType>>, 
}
#[derive(Debug, PartialEq)]
pub struct MethodInput { 
    typ: RCType, 
    lax: bool, 
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
                    return Ok(Method { name, input, outputs: HashMap::new() })
                },
                _ => return Err(next.as_err_unexpected())
            }
        }


        // Now let's handle outputs 
        // We expect them to be here because it's guaranteed above 
        // A sanity check here would be nice but we don't currently have the means to do that really 
        let mut outputs = HashMap::<Option<Number>, Option<RCType>>::new(); 

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
            ){ 
                // If there's not an end-of-clause marker (comma or semicolon), expect the type 
                typ = Some(RCType::parse_expect(stream)?); 
            }
            else if status.is_none() { 
                // Otherwise, there is no type.
                // However, we have to raise this semantic error if there's no status either
                return Err(peek.as_semantic_error("Method outputs must have a status and/or a type"))
            }

            match outputs.entry(status) { 
                Entry::Occupied(_) => { 
                    return Err(peek.as_err(ParseErrorKind::SemanticDuplicate))
                },
                Entry::Vacant(e) => { 
                    e.insert(typ);
                },
            }

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
#[derive(EnumString, Debug, Display, PartialEq, Eq, Hash)]
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
#[derive(Debug, PartialEq, Hash)]
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
    use std::collections::HashMap;

    use crate::parse::ast::types::{ TypeKind, StructField };
    use crate::parse::ast::types::test::make_simple_type;
    use crate::parse::ast::{ general::GenericIdentifier };
    use crate::parse::lexer_tests::token_stream;
    use crate::parse::{ParseResult, TestUnwrap, ParseErrorKind};
    use crate::parse::ast::{Expectable};

    use super::{Method, RCType, MethodInput, MethodName, SpecialType, LinksBlock, Link, RCIdentifier};

    fn parse_method(input: &str) -> ParseResult<Method> { 
        let mut stream = token_stream(input); 
        Method::parse_expect(&mut stream)
    }
    fn assert_method(input: &str, expected: Method) { 
        assert_eq!( parse_method(input).test_unwrap(), expected );
    }


    fn make_simple_rc_type(kind: TypeKind) -> RCType { 
        RCType::Normal(make_simple_type(kind))
    }
    fn make_ident_type(name: &str) -> RCType { 
        RCType::Normal(make_simple_type(TypeKind::Identifier(GenericIdentifier::Simple(name.to_string()))))
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
            outputs: HashMap::from([( Some("200".to_string()), Some(make_ident_type("bar")) )])
        })
    }

    #[test]
    fn method_empty_bodies() { 
        assert_method("POST -> @empty;", Method { 
            name: MethodName::Post,
            input: None, 
            outputs: HashMap::from([( None, Some(RCType::Special(SpecialType::Empty)) )])
        })
    }

    #[test]
    fn method_implied_response() { 
        assert_method("PATCH foo -> bar;", Method { 
            name: MethodName::Patch, 
            input: Some(MethodInput { 
                typ: make_ident_type("foo"), 
                lax: false, 
            }), 
            outputs: HashMap::from([( None, Some(make_ident_type("bar")) )])
        });

        assert_method("POST foo -> #204;", Method { 
            name: MethodName::Post, 
            input: Some(MethodInput { 
                typ: make_ident_type("foo"), 
                lax: false, 
            }), 
            outputs: HashMap::from([( Some("204".to_string()), None )])
        });
    }

    #[test]
    fn method_multiple_response() { 
        assert_method("POST -> bar, #204, #422 foo;", Method { 
            name: MethodName::Post, 
            input: None, 
            outputs: HashMap::from([
                ( None, Some(make_ident_type("bar")) ),
                ( Some("204".to_string()), None ),
                ( Some("422".to_string()), Some(make_ident_type("foo")) ),
            ])
        })
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

    #[test]
    fn err_duplicate_responses() { 
        let err = parse_method("POST -> #201, #400 err, #201 foo;").unwrap_err();
        assert_eq!(err.kind, ParseErrorKind::SemanticDuplicate);
    }

    #[test]
    fn method_type_literal() { 
        assert_method("POST { foo: bar } -> baz[];", Method { 
            name: MethodName::Post, 
            input: Some(MethodInput { 
                typ: RCType::Normal(make_simple_type(TypeKind::Struct(vec![ 
                    StructField { 
                        name: "foo".to_string(), 
                        typ: make_simple_type(TypeKind::Identifier(GenericIdentifier::Simple("bar".to_string()))),
                    }
                ]))), 
                lax: false 
            }), 
            outputs: HashMap::from([ 
                (None, Some( 
                    make_simple_rc_type(TypeKind::Array(Box::new(
                        make_simple_type(TypeKind::Identifier(GenericIdentifier::Simple("baz".to_string())))
                    )))
                ))
            ])
        });
    }

    #[test]
    fn lax() { 
        assert_method("POST foo% -> @empty;", Method { 
            name: MethodName::Post,
            input: Some(MethodInput { 
                typ: make_simple_rc_type(TypeKind::Identifier(GenericIdentifier::Simple("foo".to_string()))), 
                lax: true,
            }),
            outputs: HashMap::from([( None, Some(RCType::Special(SpecialType::Empty)))]),
        })
    }


    //
    // Links
    //

    fn assert_links(input: &str, expected: LinksBlock) { 
        let res = LinksBlock::parse_expect(&mut token_stream(input)).test_unwrap(); 
        assert_eq!(res, expected);
    }

    #[test]
    fn links_simple() { 
        assert_links("{ foo -> bar }", vec![ 
            Link { 
                rel: "foo".to_string(), 
                optional: false, 
                typ: RCIdentifier { base: "bar".to_string(), generics: vec![] }
            }
        ]);
    }

    #[test]
    fn links_multiple_optional() { 
        assert_links("{ bar -> foo, baz? -> optional }", vec![ 
            Link { 
                rel: "bar".to_string(),
                optional: false, 
                typ: RCIdentifier { base: "foo".to_string(), generics: vec![] }
            },
            Link { 
                rel: "baz".to_string(), 
                optional: true, 
                typ: RCIdentifier { base: "optional".to_string(), generics: vec![] }
            }
        ]);
    }

    #[test]
    fn links_generic() { 
        assert_links("{baz -> foo<bar> }", vec![
            Link { 
                rel: "baz".to_string(), 
                optional: false, 
                typ: RCIdentifier { 
                    base: "foo".to_string(), 
                    generics: vec![ RCIdentifier { 
                        base: "bar".to_string(), 
                        generics: vec![] 
                    } ] 
                }
            }
        ]);
    }

    #[test]
    fn err_link_duplicate() { 
        let err = LinksBlock::parse_expect(&mut token_stream("{ foo -> bar, foo -> baz }")).unwrap_err(); 
        assert_eq!(err.kind, ParseErrorKind::SemanticDuplicate);
    }

    //
    // Integration
    //


    //TODO: Test err duplicate in RC integration, like this 
    // let err = parse_method("POST -> bar, #200 foo;").unwrap_err();
    // assert!(matches!(err.kind, ParseErrorKind::Semantic(_)))

}