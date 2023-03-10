use std::str::FromStr;

use strum_macros::{EnumString, Display};

use crate::parse::{ ParseResult, ParseErrorKind }; 
use crate::parse::lexer::TokenStream; 
use crate::parse::tokens::{ Punctuation, Keyword, Number }; 
use crate::parse::ast::general::GenericIdentifier;

use super::parse_list_into;
use super::{Expectable, types::Type, interfaces::InterfaceExpression, Peekable};

/// The declaration clause of a Resource Class,
/// including its identifier and whether or not it was extended.
/// 
/// This exists to handle parameters (generics, and flags in the future) in the declaration - 
/// currently we have the limitation that flags cannot be defined for extended Resource Classes
/// 
/// TAG: RC_FLAGS
#[derive(Debug, PartialEq, Eq)]
pub enum RCDeclaration { 
    /// A non-extended Resource Class, which may have parameters
    Basic(GenericIdentifier), 

    /// An extended resource class;
    /// the `String` is its identifier (it may not have parameters),
    /// the `GenericIdentifier` references the extended RC (it may have parameters),
    Extended(String, GenericIdentifier), 
}


/// The body of a resource class,
/// beginning with an open brace
#[derive(Debug, PartialEq)]
pub struct ResourceClass { 
    /// The declaration for this resource class,
    /// including its parameter information
    pub declaration: RCDeclaration, 

    /// The base data type declared with the `data` keyword
    /// 
    /// (This cannot be a reference to another RC at this time)
    pub data : Option<Type>, 

    /// THe embedded data type, declared with the `embed` keyword
    pub embed : Option<Type>, 

    /// The interface expresssion (reference or literal) declared for this RC
    pub interface : Option<InterfaceExpression>,

    pub links : LinksBlock,

    pub methods : Methods,
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
                let ident = GenericIdentifier::parse_expect(stream)?; 

                if stream.peek_for_keyword(Keyword::Extends)? { 

                    if ident.has_modifiers() { 
                        let err_ref = stream.peek()?; 
                        return Err(err_ref.as_semantic_error("Resource classes cannot have modifiers if they are extended"))
                    }
                    else { 
                        RCDeclaration::Extended(ident.base, GenericIdentifier::parse_expect(stream)?)
                    }
    
                }
                else { RCDeclaration::Basic(ident) }
            };

            let mut data : Option<Type> = None; 
            let mut embed : Option<Type> = None; 
            let mut interface : Option<InterfaceExpression> = None;
            let mut links : Option<LinksBlock> = None; 
            let mut methods = Methods::new(); 

            stream.next()?.expect_punctuation(Punctuation::BraceOpen)?; 
            loop { 

                let err_ref = stream.peek()?; 
                if stream.peek_for_keyword(Keyword::Data)? { 
                    if data.is_some() { 
                        return Err(err_ref.as_err(ParseErrorKind::SemanticDuplicate))
                    }
                    data = Some(Type::parse_expect(stream)?);
                }
                else if stream.peek_for_keyword(Keyword::Embed)? { 
                    if embed.is_some() { 
                        return Err(err_ref.as_err(ParseErrorKind::SemanticDuplicate))
                    }

                    let typ = Type::parse_expect(stream)?; 

                    if !typ.validate_ident_composite() { 
                        return Err(err_ref.as_semantic_error("Embedded resources can only contain references to other resources"))
                    }
                    embed = Some(typ);
                }
                else if stream.peek_for_keyword(Keyword::Interface)? { 
                    if interface.is_some() { 
                        return Err(err_ref.as_err(ParseErrorKind::SemanticDuplicate))
                    }
                    interface = Some(InterfaceExpression::parse_expect(stream)?);
                }
                else if stream.peek_for_keyword(Keyword::Links)? { 
                    if links.is_some() { 
                        return Err(err_ref.as_err(ParseErrorKind::SemanticDuplicate))
                    }
                    links = Some(LinksBlock::parse_expect(stream)?);
                }
                else if stream.peek_for_punctuation(Punctuation::BraceClose)? { 
                    break
                }
                else { 
                    let method = Method::parse_expect(stream)?; 

                    // Bonus check for implied duplicate method names
                    // This is an edge case - explicit duplicate names are caught in `Method::parse_expect`, 
                    // but implied names can't be caught there since the default is determined by the `data` field for GET requests
                    //
                    // https://veneto.notion.site/Incorrect-position-for-duplicate-implied-method-verb-502f7ba853b442bd80179f6db7af3776
                    //TAG: DUPLICATE_IMPLIED_VERB
                    let default_status = Self::default_status(data.is_some(), method.name);
                    
                    if
                        method.outputs.iter().any(|m| m.status.is_none()) &&
                        method.outputs.iter().any(|m| m.status == Some(default_status.clone()))
                    { 
                        return Err(err_ref.as_err(ParseErrorKind::Semantic(
                            format!("Duplicate output for default status {default_status} (one of them is implied)")
                        )))
                    }

                    if methods.iter().any(|other| other.name == method.name) { 
                        return Err(err_ref.as_err(ParseErrorKind::SemanticDuplicate))
                    }

                    methods.push(method); 
                }

            }

            Ok(Some(ResourceClass { 
                declaration, data, embed, interface, methods,
                links: links.unwrap_or_default(),
            }))
        }
        else { Ok(None) }
    }
}




//
// Identifiers
//


/// These are the spcecial types usable within resource classes.
/// 
/// See https://veneto.notion.site/Resource-Class-a0ef96008d83457e99590b35270affeb
#[derive(EnumString, Debug, Display, PartialEq, Eq)]
#[strum(serialize_all="lowercase")]
pub enum Metaclass { 
    #[strum(serialize="self")]
    RCSelf, 
    Media,
    Empty, 
}

impl Expectable for Metaclass { 
    fn parse_expect(stream: &mut TokenStream) -> ParseResult<Self> {
        let next = stream.next()?;
        Metaclass::from_str(&next.try_as_identifier()?).map_err(|_| next.as_err(ParseErrorKind::UnknownSpecialType))
    }
}

/// This represents a type expression within the context of a resource class;
/// it contains a normal type or an `RCReference`.  
#[derive(Debug, PartialEq)]
pub enum RCType { 
    Normal(Type),
    Special(Metaclass), 
    //TAG: RC_FLAGS
    // When we add RC flags, `Normal` may have to be changed
}
impl Expectable for RCType { 
    fn parse_expect(stream: &mut TokenStream) -> ParseResult<Self> {
        if stream.peek_for_punctuation(Punctuation::SpecialType)? { 
            Metaclass::parse_expect(stream).map(RCType::Special)
        } 
        else { 
            Type::parse_expect(stream).map(RCType::Normal)
        }
    }
}
impl Peekable for RCType { 
    fn parse_peek(stream: &mut TokenStream) -> ParseResult<Option<Self>> {
        if stream.peek_for_punctuation(Punctuation::SpecialType)? { 
            Metaclass::parse_expect(stream).map(|t| Some(RCType::Special(t)))
        } 
        else if let Some(typ) = Type::parse_peek(stream)? { 
            Ok(Some(RCType::Normal(typ)))
        }
        else { 
            Ok(None) 
        }
    }
}

/// This is a reference to an RC;
/// either an identifier or a Special Type.  
/// 
/// 
#[derive(Debug, PartialEq, Eq)]
pub enum RCReference { 
    Normal(GenericIdentifier),
    Special(Metaclass)
}
impl Expectable for RCReference { 
    fn parse_expect(stream: &mut TokenStream) -> ParseResult<Self> {
        if stream.peek_for_punctuation(Punctuation::SpecialType)? { 
            Metaclass::parse_expect(stream).map(Self::Special)
        } else {
            GenericIdentifier::parse_expect(stream).map(Self::Normal)
        }
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
    pub typ: RCReference,
    //TAG: DYNAMIC_LINKS
    // That sum type would go here
    // https://veneto.notion.site/Custom-dynamic-link-serialization-957c6c655cc44cddada94a490663cebc
} 

impl Peekable for Link { 
    fn parse_peek(stream: &mut TokenStream) -> ParseResult<Option<Self>> {

        let Some(rel) = stream.peek_for_identifier()? else { 
            return Ok(None)
        };

        let optional = stream.peek_for_punctuation(Punctuation::Optional)?; 

        //TAG: DYNAMIC_LINKS
        stream.next()?.expect_punctuation(Punctuation::Arrow)?; 

        Ok(Some(Link { 
            rel, 
            optional, 
            typ: RCReference::parse_expect(stream)?,
        }))
    }
}

impl Expectable for LinksBlock { 
    fn parse_expect(stream: &mut TokenStream) -> ParseResult<Self> {
        stream.next()?.expect_punctuation(Punctuation::BraceOpen)?; 

        let mut links = LinksBlock::new(); 
        parse_list_into(&mut links, stream)?; 
        //TAG: DUPLICATE_CHECK

        stream.next()?.expect_punctuation(Punctuation::BraceClose)?; 
        Ok(links) 
    }
}

//
// Methods
// 

#[derive(EnumString, Debug, Display, PartialEq, Eq, Clone, Copy)]
#[strum(serialize_all="UPPERCASE")]
pub enum MethodName { 
    Get, 
    Post, 
    Patch,
    Put, 
    Delete,
}
impl Expectable for MethodName { 
    fn parse_expect(stream: &mut TokenStream) -> ParseResult<Self> {
        let token = stream.next()?; 
        let name = token.as_identifier()
            .ok_or(token.as_err_unexpected())?;
            // Here we're looking for a method name,
            // if we don't find an identifier this would return an "expected identifier" error 
            // which doesn't exactly make sense to the user.  This got me during testing.  
        
        Self::from_str(&name).map_err(|_| token.as_err(ParseErrorKind::UnknownMethodName))
    }
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
    /// their statuses and types. 
    /// 
    /// Both of those things are optional.  The default behavior is described here:
    /// https://veneto.notion.site/Resource-Class-a0ef96008d83457e99590b35270affeb
    /// 
    /// However, the parser currently does not allow them both to be empty.  
    /// This might get converted to some other type in the future if we need to guarantee that for type safety,
    /// but since they both have defaults it's staying like this for now 
    /// 
    ///TAG: REASON_PHRASES
    /// https://veneto.notion.site/HTTP-reason-phrases-instead-of-codes-772c7fc3d21146aa922573913c8adc35
    pub outputs : MethodOutputs, 
}

type Methods = Vec<Method>;

#[derive(Debug, PartialEq)]
pub struct MethodInput { 
    pub typ: RCType, 
    pub lax: bool, 
}

#[derive(Debug, PartialEq)]
pub struct MethodOutput { 
    pub status: Option<Number>, 
    pub typ: Option<RCType>
}

type MethodOutputs = Vec<MethodOutput>;


impl Expectable for Method { 
    fn parse_expect(stream: &mut TokenStream) -> ParseResult<Self> {
        let name = MethodName::parse_expect(stream)?;

        let mut input : Option<MethodInput> = None; 
        if let Some(typ) = RCType::parse_peek(stream)? { 
            let err_ref = stream.peek()?; 
            if [ MethodName::Get, MethodName::Delete ].contains(&name) { 
                return Err(err_ref.as_semantic_error("This request type cannot have a request body"))
            }

            let lax = stream.peek_for_punctuation(Punctuation::Lax)?; 

            input = Some(MethodInput { typ, lax });
        }

        let mut outputs = Vec::<MethodOutput>::new(); 
        if stream.peek_for_punctuation(Punctuation::Arrow)? { 
            loop { 
                let mut status : Option<Number> = None; 
                let err_ref = stream.peek()?; 
                if stream.peek_for_punctuation(Punctuation::HttpStatus)? { 
                    status = Some(stream.next()?.try_as_number()?);
                }

                if outputs.iter().any(|other| other.status == status) { 
                    return Err(err_ref.as_err(ParseErrorKind::SemanticDuplicate))
                }

                let typ = RCType::parse_peek(stream)?; 
                outputs.push(MethodOutput { status, typ });

                let next = stream.next()?; 
                match next.as_punctuation() { 
                    Some(Punctuation::Comma) => continue,
                    Some(Punctuation::Semicolon) => break,
                    _ => return Err(next.as_err_unexpected())
                }
            }
        }
        else { 
            stream.next()?.expect_punctuation(Punctuation::Semicolon)?;
        }

        Ok(Method { name, input, outputs })
        
    }
}



#[cfg(test)]
mod test {
    use std::vec;

    use crate::parse::ast::interfaces::{InterfaceExpression, InterfaceField, InterfaceValueType};
    use crate::parse::ast::types::{ TypeKind, StructField };
    use crate::parse::ast::types::test::make_simple_type;
    use crate::parse::ast::{ general::GenericIdentifier };
    use crate::parse::lexer_tests::token_stream;
    use crate::parse::{ParseResult, ParseErrorKind};
    use crate::parse::ast::{Expectable, Peekable};

    use super::{Method, RCType, MethodInput, MethodName, Metaclass, LinksBlock, Link, ResourceClass, RCDeclaration, MethodOutput, RCReference};

    fn parse_method(input: &str) -> ParseResult<Method> { 
        let mut stream = token_stream(input); 
        Method::parse_expect(&mut stream)
    }
    fn assert_method(input: &str, expected: Method) { 
        assert_eq!( parse_method(input).unwrap(), expected );
    }


    fn make_simple_rc_type(kind: TypeKind) -> RCType { 
        RCType::Normal(make_simple_type(kind))
    }
    fn make_ident_type(name: &'static str) -> RCType { 
        RCType::Normal(make_simple_type(TypeKind::Identifier(GenericIdentifier::simple(name))))
    }

    //
    // Methods
    //

    #[test]
    fn method_empty() { 
        assert_method("DELETE;", Method { 
            name: MethodName::Delete, 
            input: None, 
            outputs: vec![], 
        })
    }

    #[test]
    fn method_complete() { 
        assert_method("POST foo -> #200 bar;", Method { 
            name: super::MethodName::Post, 
            input: Some(MethodInput { 
                typ: make_ident_type("foo"), 
                lax: false, 
            }), 
            outputs: vec![ 
                MethodOutput { status: Some("200".to_string()), typ:  Some(make_ident_type("bar")) }
            ]
        })
    }

    #[test]
    fn method_empty_bodies() { 
        assert_method("POST -> @empty;", Method { 
            name: MethodName::Post,
            input: None, 
            outputs: vec![ 
                MethodOutput { status: None, typ: Some(RCType::Special(Metaclass::Empty)) }
            ]
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
            outputs: vec![ 
                MethodOutput { status: None, typ: Some(make_ident_type("bar")) }
            ]
        });

        assert_method("POST foo -> #204;", Method { 
            name: MethodName::Post, 
            input: Some(MethodInput { 
                typ: make_ident_type("foo"), 
                lax: false, 
            }), 
            outputs: vec![ 
                MethodOutput { status: Some("204".to_string()), typ: None }
            ]
        });
    }

    #[test]
    fn method_multiple_response() { 
        assert_method("POST -> bar, #204, #422 foo;", Method { 
            name: MethodName::Post, 
            input: None, 
            outputs: vec![ 
                MethodOutput { status: None, typ: Some(make_ident_type("bar")) },
                MethodOutput { status: Some("204".to_string()), typ: None },
                MethodOutput { status: Some("422".to_string()), typ: Some(make_ident_type("foo")) },
            ]
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
                        typ: make_simple_type(TypeKind::Identifier(GenericIdentifier::simple("bar"))),
                    }
                ]))), 
                lax: false 
            }), 
            outputs: vec![ 
                MethodOutput { 
                    status: None, 
                    typ: Some(make_simple_rc_type(TypeKind::Array(Box::new(
                        make_simple_type(TypeKind::Identifier(GenericIdentifier::simple("baz")))
                    )))),
                }
            ]
        });
    }

    #[test]
    fn lax() { 
        assert_method("POST foo% -> @empty;", Method { 
            name: MethodName::Post,
            input: Some(MethodInput { 
                typ: make_simple_rc_type(TypeKind::Identifier(GenericIdentifier::simple("foo"))), 
                lax: true,
            }),
            outputs: vec![ 
                MethodOutput { status: None, typ: Some(RCType::Special(Metaclass::Empty)) }
            ]
        })
    }


    //
    // Links
    //

    fn assert_links(input: &str, expected: LinksBlock) { 
        let res = LinksBlock::parse_expect(&mut token_stream(input)).unwrap(); 
        assert_eq!(res, expected);
    }

    #[test]
    fn links_simple() { 
        assert_links("{ foo -> bar }", vec![ 
            Link { 
                rel: "foo".to_string(), 
                optional: false, 
                typ: RCReference::Normal(GenericIdentifier { base: "bar".to_string(), args: vec![] })
            }
        ]);
    }

    #[test]
    fn links_special() { 
        assert_links("{ foo? -> @self }", vec![ 
            Link { 
                rel: "foo".to_string(), 
                optional: true, 
                typ: RCReference::Special(Metaclass::RCSelf),
            }
        ])
    }

    #[test]
    fn links_multiple_optional() { 
        assert_links("{ bar -> foo, baz? -> optional }", vec![ 
            Link { 
                rel: "bar".to_string(),
                optional: false, 
                typ: RCReference::Normal(GenericIdentifier { base: "foo".to_string(), args: vec![] })
            },
            Link { 
                rel: "baz".to_string(), 
                optional: true, 
                typ: RCReference::Normal(GenericIdentifier { base: "optional".to_string(), args: vec![] })
            }
        ]);
    }

    #[test]
    fn links_generic() { 
        assert_links("{baz -> foo<bar> }", vec![
            Link { 
                rel: "baz".to_string(), 
                optional: false, 
                typ: RCReference::Normal(GenericIdentifier { 
                    base: "foo".to_string(), 
                    args: vec![ GenericIdentifier { 
                        base: "bar".to_string(), 
                        args: vec![] 
                    } ] 
                })
            }
        ]);
    }

    //TAG: DUPLICATE_CHECK
    // #[test]
    // fn err_link_duplicate() { 
    //     let err = LinksBlock::parse_expect(&mut token_stream("{ foo -> bar, foo -> baz }")).unwrap_err(); 
    //     assert_eq!(err.kind, ParseErrorKind::SemanticDuplicate);
    // }

    //
    // Integration
    //

    fn parse_rc(input: &str) -> ParseResult<Option<ResourceClass>> { 
        ResourceClass::parse_peek(&mut token_stream(input))
    }

    fn assert_rc(input: &str, expected: ResourceClass) { 
        let rc = parse_rc(input).unwrap().unwrap();
        assert_eq!(rc, expected); 
    }

    #[test]
    fn simple_rc() { 
        assert_rc("resource Foo { 
            data int[]

            interface Queryable

            links { 
                bar -> baz, 
            }

            GET -> #200; 

            PATCH @self% -> @self; 

        }", ResourceClass { 

            declaration: RCDeclaration::Basic(GenericIdentifier { base: "Foo".to_string(), args: vec![] }),

            data: Some(make_simple_type(
                TypeKind::Array(Box::new(
                    make_simple_type(TypeKind::Identifier(GenericIdentifier::simple("int")))
                ))
            )),
            embed: None, 

            interface: Some(InterfaceExpression::Identifier("Queryable".to_string())),

            links: vec![ 
                Link { 
                    rel: "bar".to_string(),  
                    optional: false, 
                    typ: RCReference::Normal(GenericIdentifier { 
                        base: "baz".to_string(), 
                        args: vec![], 
                    })
                }
            ], 

            methods: vec![
                Method { 
                    name: MethodName::Get, 
                    input: None, 
                    outputs: vec![ 
                        MethodOutput { status: Some(200.to_string()), typ: None }
                    ]
                },

                Method { 
                    name: MethodName::Patch, 
                    input: Some(MethodInput { 
                        typ: RCType::Special(Metaclass::RCSelf),
                        lax: true, 
                    }), 
                    outputs: vec![ 
                        MethodOutput { status: None, typ: Some(RCType::Special(Metaclass::RCSelf)) }
                    ]
                }
            ],

        })
    }

    #[test]
    fn rc_generics() { 
        assert_rc("resource foo<T> {}", ResourceClass { 
            declaration: RCDeclaration::Basic(GenericIdentifier { 
                base: "foo".to_string(), 
                args: vec![ GenericIdentifier { base: "T".to_string(), args: vec![] } ] 
            }),

            data: None, 
            embed: None, 
            interface: None, 
            links: vec![], 
            methods: vec![], 
        });
    }

    #[test]
    fn rc_extends() { 
        assert_rc("resource foo extends bar<T> {}", ResourceClass { 
            declaration: RCDeclaration::Extended(
                "foo".to_string(), 
                GenericIdentifier { 
                    base: "bar".to_string(), 
                    args: vec![ 
                        GenericIdentifier { base: "T".to_string(), args: vec![] }
                    ]
                }
            ),

            data: None, 
            embed: None, 
            interface: None, 
            links: vec![], 
            methods: vec![], 
        })
    }

    #[test]
    fn err_extends_flags() { 
        let err = parse_rc("resource foo<T> extends bar<T> {}").unwrap_err();
        assert!(matches!(err.kind, ParseErrorKind::Semantic(_)))
    }

    #[test]
    fn err_implied_duplicate_method() { 
        let err = parse_rc("resource foo { 
            POST -> #422; 
            GET -> bar,
                #405 baz; 
        }").unwrap_err();

        assert!(matches!(err.kind, ParseErrorKind::Semantic(_)))
    }

    #[test]
    fn rc_data_literal() { 
        assert_rc(
            "resource rc { 
                data { foo: bar }[]

                interface { 
                    q: string,
                }
            }", 
            ResourceClass { 
                declaration: RCDeclaration::Basic(
                    GenericIdentifier { base: "rc".to_string(), args: vec![] }
                ),

                data: Some(make_simple_type(TypeKind::Array(Box::new(
                    make_simple_type(TypeKind::Struct(vec![ 
                        StructField { 
                            name: "foo".to_string(),
                            typ: make_simple_type(TypeKind::Identifier(GenericIdentifier::simple("bar"))),
                        }
                    ]))
                )))),

                embed: None, 

                interface: Some(InterfaceExpression::Literal(vec![ 
                    InterfaceField { 
                        name: "q".to_string(), 
                        typ: InterfaceValueType::String, 
                        optional: false, 
                    }
                ])),

                links: vec![], 
                methods: vec![],
            }
        )
    }

    #[test]
    fn rc_embed() { 
        assert_rc(
            "resource Embedder { 
                embed { foo: Bar }
            }", 
            ResourceClass { 
                declaration: RCDeclaration::Basic(GenericIdentifier { base: "Embedder".to_string(), args: vec![] }),
                data: None, 
                embed: Some(make_simple_type(TypeKind::Struct(vec![ 
                    StructField { 
                        name: "foo".to_string(), 
                        typ: make_simple_type(TypeKind::Identifier(GenericIdentifier { base: "Bar".to_string(), args: vec![] }))
                    }
                ]))),

                interface: None, 
                links: vec![], 
                methods: vec![],
            }
        )
    }

}