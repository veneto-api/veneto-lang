use crate::parse::ParseErrorKind;
use crate::parse::{lexer::TokenStream, ParseResult};
use crate::parse::tokens::{Punctuation, Keyword, Terminal, TokenKind};
use crate::{peek_match};

use super::{Expectable, Peekable, Finishable};
use super::general::GenericIdentifier;

use strum_macros::Display;

#[derive(PartialEq, Display, Debug)]
pub enum TypeKind { 
    /// A reference to an existing type 
    Identifier(GenericIdentifier),

    /// A tuple represented by the included types, in order
    Tuple(Tuple),

    Struct(StructBody),

    /// A homogenous array of a single type 
    Array(Box<Type>), 
    // 👆 the compiler complains if it's "without indirection", so it must be boxed. TIL!
}

/// This is a separate enum of all of the non-array `TypeKind`s. 
/// 
/// Since array brackets are postfix, we have to process all of this first before determining if it's an array
/// This might not be necessary but I did it for now, it felt right to have explicit type checking here 
enum InnerTypeKind { 
    Identifier(GenericIdentifier),

    Tuple(Tuple),

    Struct(StructBody),
}
impl From<InnerTypeKind> for TypeKind {
    fn from(val: InnerTypeKind) -> Self {
        match val { 
            InnerTypeKind::Identifier(x) => TypeKind::Identifier(x),
            InnerTypeKind::Tuple(x) => TypeKind::Tuple(x),
            InnerTypeKind::Struct(x) => TypeKind::Struct(x),
        }
    }
}


/// This represents a type expression - 
/// that can be a reference to an existing type, or a new one. 
#[derive(PartialEq, Debug)]
pub struct Type { 
    pub kind: TypeKind, 
    
    /// If `true`, this type has been marked optional with a `?`.  
    /// 
    /// Note that in the future we will likely have nested optionals, tracked in the below Notion page.
    /// This will invalidate the current system of assuming that there's only one. 
    /// 
    /// https://veneto.notion.site/Unions-Monadic-Optionals-e39088b0b78e48ebafb941faef447dcb
    pub optional: bool, 


    /// The fields added with the `in +` modifier.
    /// Only semantically valid for structs.
    pub in_plus: Option<StructBody>, 
    /// The fields added with the `out +` modifier 
    /// Only semantically valid for structs.
    pub out_plus: Option<StructBody>, 
}

impl Type { 
    fn from_inner_kind(inner_kind: InnerTypeKind) -> Self { 
        Self { 
            kind: inner_kind.into(),
            optional: false, 
            in_plus: None, 
            out_plus: None,
        }
    }

    fn wrap_as_array(old: Self) -> Self { 
        Self { 
            kind: TypeKind::Array(Box::new(old)), 
            optional: false, 
            in_plus: None, 
            out_plus: None,
        }
    }

    fn begin_peek(stream: &mut TokenStream) -> ParseResult<Option<InnerTypeKind>> { 
        if let Some(str) = StructBody::parse_peek(stream)? { 
            Ok(Some(InnerTypeKind::Struct(str)))
        }
        else if let Some(tuple) = Tuple::parse_peek(stream)? { 
            Ok(Some(InnerTypeKind::Tuple(tuple)))
        }
        else if let Some(gid) = GenericIdentifier::parse_peek(stream)? { 
            Ok(Some(InnerTypeKind::Identifier(gid)))
        }
        else { 
            Ok(None) 
        }
    }

    fn finish(inner_kind: InnerTypeKind, stream: &mut TokenStream) -> ParseResult<Self> { 
        let mut typ = Self::from_inner_kind(inner_kind);

        loop { 
            let err_ref = stream.peek()?; 
            peek_match!(stream.peek_for_terminal { 
                Terminal::Punctuation(Punctuation::Optional) => { 
                    if typ.optional { 
                        return Err(err_ref.as_semantic_error("Nested optionals are not supported yet"))
                    } else { 
                        typ.optional = true;
                    }
                },
                Terminal::Punctuation(Punctuation::BracketOpen) => { 
                    stream.next()?.expect_punctuation(Punctuation::BracketClose)?; 
                    typ = Self::wrap_as_array(typ); 
                },

                //TAG: IN_OUT_MODIFIERS
                Terminal::Keyword(Keyword::In) => { 
                    if typ.in_plus.is_some() { 
                        return Err(err_ref.as_err(ParseErrorKind::SemanticDuplicate));
                    }

                    stream.next()?.expect_punctuation(Punctuation::StructExtension)?; 
                    typ.in_plus = Some(StructBody::parse_expect(stream)?);
                },

                Terminal::Keyword(Keyword::Out) => { 
                    if typ.out_plus.is_some() { 
                        return Err(err_ref.as_err(ParseErrorKind::SemanticDuplicate));
                    }

                    stream.next()?.expect_punctuation(Punctuation::StructExtension)?; 
                    typ.out_plus = Some(StructBody::parse_expect(stream)?);
                },

                _ => return Ok(typ)
            })
        }
    }


    /// This returns `true` only if `self` is an identifier or a composite type that ultimately only contains identifiers.
    /// 
    /// This is necessary when validating embedded resources, as non-Resource data is not allowed.  
    pub fn validate_ident_composite(&self) -> bool { 
        true 

        // We've got to actually implement this!
        // In order to do that, we need to know what is or isn't an RC reference
        //TAG: RC_REFERENCE_SYNTAX
        // https://veneto.notion.site/Maybe-we-need-a-special-syntax-for-RC-references-c913241025c740f387a7cd45d40672db 

        // match &self.kind { 
        //     TypeKind::Identifier(_) => true, 
        //     TypeKind::Array(inner) => inner.validate_ident_composite(), 
        //     TypeKind::Struct(fields) => fields.iter().all(|field| field.typ.validate_ident_composite()),
        //     _ => false, 
        // }
    }
}

impl Peekable for Type { 
    fn parse_peek(stream: &mut TokenStream) -> ParseResult<Option<Self>> {
        if let Some(inner_kind) = Self::begin_peek(stream)? { 
            Ok(Some(Self::finish(inner_kind, stream)?))
        } else { 
            Ok(None)
        }
    }
}
impl Expectable for Type { 
    fn parse_expect(stream: &mut TokenStream) -> ParseResult<Self> {
        let err_ref = stream.peek()?; 
        if let Some(inner_kind) = Self::begin_peek(stream)? { 
            Self::finish(inner_kind, stream)
        } else { 
            Err(err_ref.as_err(ParseErrorKind::ExpectedTypeExpression))
        }
    }
}

//
// Struct literals
//


/// This is a Struct's body, which is just its set of fields
pub type StructBody = Vec<StructField>;

/// This is an individual field declaration within a `Struct`
#[derive(PartialEq, Debug)]
pub struct StructField { 
    pub name: String, 
    /// This is the field's type, 
    /// abbreviated to `typ` because `type` is a reserved keyword
    pub typ: Type, 
}
impl Finishable for StructBody { 
    const INITIAL_TOKEN: Terminal = Terminal::Punctuation(Punctuation::BraceOpen);

    fn parse_finish(stream: &mut TokenStream) -> ParseResult<Self> {
        let mut fields = Self::new(); 
        loop { 
            peek_match!(stream.peek_for_punctuation { 
                Punctuation::Comma => continue, 
                Punctuation::BraceClose => return Ok(fields),
                _ => { 
                    let err_ref = stream.peek()?; 

                    if err_ref.kind == TokenKind::EOF { 
                        return Err(err_ref.as_err_unexpected())
                    }

                    let field = StructField::parse_expect(stream)?; 
                    if fields.iter().any(|other| other.name == field.name) { 
                        return Err(err_ref.as_err(ParseErrorKind::SemanticDuplicate))
                    } else { 
                        fields.push(field)
                    }
                }
            })
        }
    }
}

impl Expectable for StructField { 
    fn parse_expect(stream: &mut TokenStream) -> ParseResult<Self> {
        let name = stream.next()?.try_as_identifier()?; 
        stream.next()?.expect_punctuation(Punctuation::Colon)?; 
        let typ = Type::parse_expect(stream)?; 
        Ok(StructField { name, typ })
    }
}

//
// Tuples
//

type Tuple = Vec<Type>; 
impl Peekable for Tuple { 
    fn parse_peek(stream: &mut TokenStream) -> ParseResult<Option<Self>> {
        if stream.peek_for_punctuation(Punctuation::BracketOpen)? { 

            let mut types = Self::new(); 
            loop { 

                peek_match!(stream.peek_for_punctuation { 
                    Punctuation::BracketClose => return Ok(Some(types)),
                    Punctuation::Comma => continue, 
                    _ => types.push( Type::parse_expect(stream)? )
                })

            }

        }
        else { Ok(None) }
    }
}


#[cfg(test)]
pub mod test {
    use crate::parse::ast::Expectable;
    use crate::parse::{ParseResult, lexer::TokenStream, lexer_tests::{token_stream, assert_punctuation}, ParseErrorKind};
    use super::GenericIdentifier; 
    use crate::parse::tokens::Punctuation; 

    use super::{Type, TypeKind, StructField};
 
    // Yeehaw! This is gonna be a big one 

    fn parse_type(input: &str) -> ParseResult<(TokenStream, Type)> { 
        let mut stream = token_stream(input); 
        let typ = Type::parse_expect(&mut stream);
        typ.map(|x| (stream, x))
    }
    fn assert_type(input: &str, expected: Type) { 
        let (_, typ) = parse_type(input).unwrap(); 
        assert_eq!(typ, expected); 
    }
    fn assert_type_kind(input: &str, expected: TypeKind) { 
        let (_, typ) = parse_type(input).unwrap();
        assert_eq!(typ.kind, expected);
    }

    pub fn make_simple_type(kind: TypeKind) -> Type { 
        Type { 
            kind,  
            optional: false, 
            in_plus: None, 
            out_plus: None, 
        }
    }

    #[test]
    fn simple_ident() { 
        let (mut stream, typ) = parse_type("asdf@").unwrap();
        assert_eq!(typ.kind, TypeKind::Identifier(GenericIdentifier::simple("asdf")));
        assert_punctuation(&mut stream, Punctuation::SpecialType);
    }

    #[test]
    fn struct_eof() { 
        let res = parse_type("{ asdf: eee ");
        assert!(matches!(res.unwrap_err().kind, ParseErrorKind::Unexpected(_)))
    }

    #[test]
    fn simple_struct() { 
        assert_type_kind("{ foo: bar }", TypeKind::Struct(vec![ 
            StructField { 
                name: "foo".to_string(), 
                typ: make_simple_type(TypeKind::Identifier(GenericIdentifier::simple("bar"))),
            }
        ]))
    }

    #[test]
    fn simple_array() { 
        assert_type_kind("foo[]", TypeKind::Array(Box::new(
            make_simple_type(TypeKind::Identifier(GenericIdentifier::simple("foo")))
        )));
    }

    #[test] 
    fn tuple_generic() { 
        assert_type_kind("[ foo, bar<baz> ]", TypeKind::Tuple(
            vec![ 
                make_simple_type(TypeKind::Identifier(GenericIdentifier::simple("foo"))),
                make_simple_type(
                    TypeKind::Identifier(GenericIdentifier { 
                        base: "bar".to_string(), 
                        args: vec![ GenericIdentifier::simple("baz") ]
                    })
                ),
            ]
        ));
    }

    #[test] 
    fn generic_double_array() { 
        assert_type_kind("foo<bar<asdf, ree>>[][]", TypeKind::Array( Box::new(
            make_simple_type(
                TypeKind::Array(Box::new(
                    make_simple_type(TypeKind::Identifier(
                        GenericIdentifier { 
                            base: "foo".to_string(), 
                            args: vec![
                                GenericIdentifier { 
                                    base: "bar".to_string(), 
                                    args: vec![ 
                                        GenericIdentifier::simple("asdf"),
                                        GenericIdentifier::simple("ree"),
                                    ]
                                }
                            ]
                        }
                    ))
                ))
            )
        )))
    }

    #[test]
    fn struct_array() { 
        assert_type_kind("{ foo: bar }[]", TypeKind::Array(Box::new(
            make_simple_type(TypeKind::Struct(vec![ StructField { 
                name: "foo".to_string(),
                typ: make_simple_type(TypeKind::Identifier(GenericIdentifier::simple("bar"))),
            }]))
        )))
    }

    #[test]
    fn tuple_array() { 
        assert_type_kind("[foo][]", TypeKind::Array(Box::new(
            make_simple_type(TypeKind::Tuple(vec![ 
                make_simple_type(TypeKind::Identifier(GenericIdentifier::simple("foo")))
            ]))
        )))
    }

    #[test] 
    fn complex_struct() { 
        assert_type_kind("{ foo: bar[], baz: generic<type> }", TypeKind::Struct(vec! [ 
            StructField { 
                name: "foo".to_string(), 
                typ: make_simple_type(TypeKind::Array(Box::new(
                    make_simple_type(TypeKind::Identifier(GenericIdentifier::simple("bar")))
                ))),
            },
            StructField { 
                name: "baz".to_string(), 
                typ: make_simple_type(TypeKind::Identifier(GenericIdentifier{ 
                    base: "generic".to_string(), 
                    args: vec![ GenericIdentifier::simple("type") ]
                })),
            }
        ]))
    }

    #[test]
    fn struct_mod_out() { 
        assert_type("{ foo: bar } out + {asdf: ree}", Type { 
            kind: TypeKind::Struct(vec![ StructField { 
                name: "foo".to_string(), 
                typ: make_simple_type(TypeKind::Identifier(GenericIdentifier::simple("bar"))),
            }]),

            optional: false, 

            in_plus: None, 
            out_plus: Some(vec![ StructField { 
                name: "asdf".to_string(), 
                typ: make_simple_type(TypeKind::Identifier(GenericIdentifier::simple("ree"))),
            }])
        })
    }
    #[test]
    fn struct_mod_both() { 
        assert_type("{ foo: bar } out + {asdf: ree} in + {}", Type { 
            kind: TypeKind::Struct(vec![ StructField { 
                name: "foo".to_string(), 
                typ: make_simple_type(TypeKind::Identifier(GenericIdentifier::simple("bar"))),
            }]),

            optional: false, 

            in_plus: Some(vec![ ]), 
            out_plus: Some(vec![ StructField { 
                name: "asdf".to_string(), 
                typ: make_simple_type(TypeKind::Identifier(GenericIdentifier::simple("ree"))),
            }])
        })
    }

    #[test]
    fn struct_mod_duplicates() { 
        let res = parse_type("{ foo: bar } out + { foo: bar } out + {foo : bar}");
        assert!(matches!(res.unwrap_err().kind, ParseErrorKind::SemanticDuplicate));

        let res = parse_type("{ foo: bar } out + { foo: bar } in + {foo : bar} out + {}");
        assert!(matches!(res.unwrap_err().kind, ParseErrorKind::SemanticDuplicate));
    }

    #[test]
    fn err_struct_duplicate() { 
        let res = parse_type("{ foo: bar, bar: baz, foo: baz }");
        assert_eq!(res.unwrap_err().kind, ParseErrorKind::SemanticDuplicate)
    }

    #[test]
    fn err_nested_optionals() {
        assert_type("foo<T>?", Type { 
            kind: TypeKind::Identifier(GenericIdentifier {
                base: "foo".to_string(), 
                args: vec![ GenericIdentifier::simple("T") ],
        }),
            optional: true, 
            in_plus: None, 
            out_plus: None, 
        })
    }

    #[test]
    fn optional_duplicate() { 
        let res = parse_type("foo??");
        assert!(matches!(res.unwrap_err().kind, ParseErrorKind::Semantic(_)));
    }
}