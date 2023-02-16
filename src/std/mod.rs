
use crate::parse::{ ParseResult, lexer::TokenStream } ;
use crate::parse::ast::document::Document;

pub fn parse_std() -> ParseResult<Document> { 
    let stream = TokenStream::new( include_str!("std.veneto").chars() ); 
    Document::parse(stream)

}

lazy_static! { 
    static ref STD : ParseResult<Document> = parse_std(); 
}

#[cfg(test)]
mod test { 


    use crate::parse::ast::general::GenericIdentifier;
    use crate::parse::ast::rc::{ResourceClass, RCDeclaration, Method, MethodName, MethodOutput, Link, RCReference, Metaclass, MethodInput, RCType};
    use crate::parse::ast::types::{Type, TypeKind}; 

    #[test]
    fn test_std() { 
        let doc = super::STD.as_ref().unwrap(); 
        let mut rcs = doc.resource_classes.iter(); 

        // Ref<T>
        assert_eq!(rcs.next().unwrap(), &ResourceClass { 
            declaration: RCDeclaration::Basic(GenericIdentifier { 
                base: "Ref".to_string(), 
                args: vec![ 
                    GenericIdentifier { 
                        base: "T".to_string(), 
                        args: vec![], 
                    }
                 ]
            }),
            
            data: None, 
            embed: Some(Type { 
                kind: TypeKind::Identifier(GenericIdentifier::simple("T")),
                optional: false, 
                in_plus: None, 
                out_plus: None, 
            }),

            interface: None, 
            links: vec![], 

            methods: vec![ 
                Method { 
                    name: MethodName::Get, 
                    input: None, 
                    outputs: vec! [ 
                        MethodOutput { 
                            status: Some(405.to_string()),
                            typ: None, 
                        }
                    ]
                },
                Method { 
                    name: MethodName::Delete,
                    input: None,
                    outputs: vec![],
                }
            ]
        });

        // List<T>
        assert_eq!(rcs.next().unwrap(), &ResourceClass { 
            declaration: RCDeclaration::Basic(GenericIdentifier { 
                base: "List".to_string(), 
                args: vec![ 
                    GenericIdentifier { 
                        base: "T".to_string(), 
                        args: vec![], 
                    }
                ]
            }),

            data: None, 
            embed: Some(Type { 
                kind: TypeKind::Array(Box::new(Type { 
                    kind: TypeKind::Identifier(GenericIdentifier { 
                        base: "T".to_string(), 
                        args: vec![],
                    }),
                    optional: false,
                    in_plus: None,
                    out_plus: None, 
                })),
                optional: false,
                in_plus: None, 
                out_plus: None, 
            }), 

            interface: None, 
            links: vec![ 
                Link { 
                    rel: "next".to_string(), 
                    optional: true, 
                    typ: RCReference::Special(Metaclass::RCSelf),
                }
            ],

            methods: vec![ 
                Method { 
                    name: MethodName::Get, 
                    input: None, 
                    outputs: vec![], 
                },
                Method { 
                    name: MethodName::Post, 
                    input: Some(MethodInput { 
                        typ: RCType::Normal(Type { 
                            kind: TypeKind::Identifier(GenericIdentifier::simple("T")),
                            optional: false,
                            in_plus: None, 
                            out_plus: None, 
                        }),
                        lax: false, 
                    }),
                    outputs: vec![ 
                        MethodOutput { 
                            status: Some(201.to_string()), 
                            typ: Some(RCType::Normal(Type { 
                                kind: TypeKind::Identifier(GenericIdentifier::simple("T")),
                                optional: false,
                                in_plus: None, 
                                out_plus: None, 
                            }))
                        }
                    ]
                }
            ]
        });

        // Media
        assert_eq!(rcs.next().unwrap(), &ResourceClass { 
            declaration: RCDeclaration::Basic(GenericIdentifier { 
                base: "Media".to_string(), 
                args: vec![], 
            }), 
            data: None, 
            embed: None, 
            interface: None, 

            links: vec![], 
            methods: vec![ 
                Method { 
                    name: MethodName::Get, 
                    input: None, 
                    outputs: vec![ 
                        MethodOutput { 
                            status: None, 
                            typ: Some(RCType::Special(Metaclass::Media))
                        }
                    ]
                },
                Method { 
                    name: MethodName::Put, 
                    input: Some(MethodInput { 
                        typ: RCType::Special(Metaclass::Media), 
                        lax: false, 
                    }), 
                    outputs: vec![], 
                }
            ]
        });

        // Action
        assert_eq!(rcs.next().unwrap(), &ResourceClass { 
            declaration: RCDeclaration::Basic(GenericIdentifier { 
                base: "Action".to_string(), 
                args: vec![],
            }), 
            data: None, 
            embed: None, 
            interface: None, 

            links: vec![], 
            methods: vec![
                Method { 
                    name: MethodName::Post, 
                    input: None, 
                    outputs: vec! [ 
                        MethodOutput { 
                            status: Some(204.to_string()), 
                            typ: None, 
                        }
                    ]
                }
            ]
        });


    }
}