use crate::parse::{ClauseResult, ClauseDelim, ParseErrorKind};
use crate::parse::{lexer::TokenStream, ParseResult};
use crate::parse::tokens::{Punctuation, TokenKind, Keyword, Terminal};

use super::Expectable;
use super::general::GenericIdentifier;

use strum_macros::Display;

#[derive(PartialEq, Display, Debug)]
pub enum TypeKind { 
    /// A reference to an existing type 
    Identifier(GenericIdentifier),

    /// A tuple represented by the included types, in order
    Tuple(Vec<Type>),

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

    Tuple(Vec<Type>),

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

    fn begin_kind(stream: &mut TokenStream) -> ParseResult<InnerTypeKind> { 
        // First check for the two easy ones
        if stream.peek_for_puncutation(Punctuation::BraceOpen)? { 
            // An open brace means it's a struct
            finish_struct_body(stream).map(InnerTypeKind::Struct)
        } 
        else if stream.peek_for_puncutation(Punctuation::BracketOpen)? { 
            // An open bracket means it's a tuple
            finish_tuple(stream).map(InnerTypeKind::Tuple)
        }
        else { 
            // Otherwise, it must be an identifier. 
            GenericIdentifier::parse_expect(stream).map(InnerTypeKind::Identifier)
        }
    }
}

impl Expectable for Type { 
    fn parse_expect(stream: &mut TokenStream) -> ParseResult<Self> { 
        let mut typ = Self::from_inner_kind(Self::begin_kind(stream)?);


        //TAG: IN_OUT_MODIFIERS
        fn parse_extension(stream: &mut TokenStream) -> ParseResult<StructBody> { 
            stream.next()?.expect_punctuation(Punctuation::StructExtension)?;
            stream.next()?.expect_punctuation(Punctuation::BraceOpen)?; 
            finish_struct_body(stream)
        }

        loop { 
            let peek = stream.peek()?; 
            match peek.as_terminal() { 

                Some(Terminal::Punctuation(Punctuation::Optional)) => { 
                    if typ.optional { 
                        return Err(peek.as_semantic_error("Nested optionals are not supported yet"))
                    }
                    stream.next()?; 
                    typ.optional = true 
                }

                Some(Terminal::Punctuation(Punctuation::BracketOpen)) => { 
                    stream.next()?; 
                    stream.next()?.expect_punctuation(Punctuation::BracketClose)?; 
                    typ = Self::wrap_as_array(typ); 
                }

                
                Some(Terminal::Keyword(Keyword::In)) => { 
                    if typ.in_plus.is_some() { 
                        return Err(peek.as_err(ParseErrorKind::SemanticDuplicate));
                    }
                    if !matches!(typ.kind, TypeKind::Struct(_)) { 
                        return Err(peek.as_err(ParseErrorKind::SemanticStructOnly(Terminal::Keyword(Keyword::In))))
                    }

                    stream.next()?;
                    typ.in_plus = Some(parse_extension(stream)?);
                },

                Some(Terminal::Keyword(Keyword::Out)) => { 
                    if typ.out_plus.is_some() { 
                        return Err(peek.as_err(ParseErrorKind::SemanticDuplicate));
                    }
                    if !matches!(typ.kind, TypeKind::Struct(_)) { 
                        return Err(peek.as_err(ParseErrorKind::SemanticStructOnly(Terminal::Keyword(Keyword::Out))))
                    }

                    stream.next()?;
                    typ.out_plus = Some(parse_extension(stream)?);
                },

                _ => return Ok(typ) 
            }
        }
    }
}


//
// Struct literals
//


impl StructField { 
    /// Parses a StructField from the current position, starting with the `name`, 
    /// returning `None` if the struct is empty from the current position.  
    /// (This accounts for both completely empty structs as well as extraneous commas) 
    pub fn begin(stream: &mut TokenStream) -> ClauseResult<Option<Self>> { 
        let first = stream.next()?; 
        match first.kind { 
            TokenKind::Word(name) => { 
                stream.next()?.expect_punctuation(Punctuation::Colon)?;
                let typ = Type::parse_expect(stream)?;
                let res = Some(Self{ name, typ });
        
                let peek = stream.peek()?; 
                match peek.as_punctuation() { 
                    Some(Punctuation::Comma) => { 
                        stream.next()?; 
                        Ok((res, ClauseDelim::Continue))
                    }, 
                    Some(Punctuation::BraceClose) => { 
                        stream.next()?; 
                        Ok((res, ClauseDelim::Exit))
                    },
                    _ => Err(peek.as_err_unexpected())
                }
            },

            TokenKind::Punctuation(Punctuation::BraceClose) => { 
                Ok((None, ClauseDelim::Exit))
            },

            _ => Err(first.as_err_unexpected())
        }

    }
}

fn finish_struct_body(stream: &mut TokenStream) -> ParseResult<StructBody> { 
    let mut fields = Vec::<StructField>::new(); 

    loop { 
        let (field, delim) = StructField::begin(stream)?; 
        if let Some(field) = field { fields.push(field); }

        match delim { 
            ClauseDelim::Continue => continue, 
            ClauseDelim::Exit => return Ok(fields), 
            ClauseDelim::Unexpected(t) => return Err(t.as_err_unexpected()), 
        }
    }
}

//
// Tuples
//

fn finish_tuple(stream: &mut TokenStream) -> ParseResult<Vec<Type>> { 

    let mut types = Vec::<Type>::new(); 
    loop { 
        // Handle empty tuple or extraneous comma
        if Some(Punctuation::BracketClose) == stream.peek()?.as_punctuation() { 
            stream.next()?; 
            return Ok(types)
        }

        let typ = Type::parse_expect(stream)?; 
        types.push(typ); 

        let next = stream.next()?; 
        match next.as_punctuation() {
            Some(Punctuation::Comma) => continue, 
            Some(Punctuation::BracketClose) => return Ok(types),
            _ => return Err(next.as_err_unexpected()) 
        }
    }

}


#[cfg(test)]
pub mod test {
    use crate::parse::TestUnwrap; 
    use crate::parse::ast::Expectable;
    use crate::parse::{ParseResult, lexer::TokenStream, lexer_tests::{token_stream, assert_punctuation}, ParseErrorKind};
    use super::GenericIdentifier; 
    use crate::parse::tokens::{ Terminal, Keyword, Punctuation }; 

    use super::{Type, TypeKind, StructField};
 
    // Yeehaw! This is gonna be a big one 

    fn parse_type(input: &str) -> ParseResult<(TokenStream, Type)> { 
        let mut stream = token_stream(input); 
        let typ = Type::parse_expect(&mut stream);
        typ.map(|x| (stream, x))
    }
    fn assert_type(input: &str, expected: Type) { 
        let (_, typ) = parse_type(input).test_unwrap(); 
        assert_eq!(typ, expected); 
    }
    fn assert_type_kind(input: &str, expected: TypeKind) { 
        let (_, typ) = parse_type(input).test_unwrap();
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
        assert_eq!(typ.kind, TypeKind::Identifier(GenericIdentifier::Simple("asdf".to_string())));
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
                typ: make_simple_type(TypeKind::Identifier(GenericIdentifier::Simple("bar".to_string()))),
            }
        ]))
    }

    #[test]
    fn simple_array() { 
        assert_type_kind("foo[]", TypeKind::Array(Box::new(
            make_simple_type(TypeKind::Identifier(GenericIdentifier::Simple("foo".to_string())))
        )));
    }

    #[test] 
    fn tuple_generic() { 
        assert_type_kind("[ foo, bar<baz> ]", TypeKind::Tuple(
            vec![ 
                make_simple_type(TypeKind::Identifier(GenericIdentifier::Simple("foo".to_string()))),
                make_simple_type(
                    TypeKind::Identifier(GenericIdentifier::Generic("bar".to_string(), vec![ GenericIdentifier::Simple("baz".to_string()) ]))
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
                        GenericIdentifier::Generic("foo".to_string(), vec![ 
                            GenericIdentifier::Generic("bar".to_string(), vec![ 
                                GenericIdentifier::Simple("asdf".to_string()),
                                GenericIdentifier::Simple("ree".to_string()),
                            ])
                        ])
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
                typ: make_simple_type(TypeKind::Identifier(GenericIdentifier::Simple("bar".to_string()))),
            }]))
        )))
    }

    #[test]
    fn tuple_array() { 
        assert_type_kind("[foo][]", TypeKind::Array(Box::new(
            make_simple_type(TypeKind::Tuple(vec![ 
                make_simple_type(TypeKind::Identifier(GenericIdentifier::Simple("foo".to_owned())))
            ]))
        )))
    }

    #[test] 
    fn complex_struct() { 
        assert_type_kind("{ foo: bar[], baz: generic<type> }", TypeKind::Struct(vec! [ 
            StructField { 
                name: "foo".to_string(), 
                typ: make_simple_type(TypeKind::Array(Box::new(
                    make_simple_type(TypeKind::Identifier(GenericIdentifier::Simple("bar".to_string())))
                ))),
            },
            StructField { 
                name: "baz".to_string(), 
                typ: make_simple_type(TypeKind::Identifier(GenericIdentifier::Generic(
                    "generic".to_string(), 
                    vec![ GenericIdentifier::Simple("type".to_string()) ],
                ))),
            }
        ]))
    }

    #[test]
    fn struct_mod_out() { 
        assert_type("{ foo: bar } out + {asdf: ree}", Type { 
            kind: TypeKind::Struct(vec![ StructField { 
                name: "foo".to_string(), 
                typ: make_simple_type(TypeKind::Identifier(GenericIdentifier::Simple("bar".to_string()))),
            }]),

            optional: false, 

            in_plus: None, 
            out_plus: Some(vec![ StructField { 
                name: "asdf".to_string(), 
                typ: make_simple_type(TypeKind::Identifier(GenericIdentifier::Simple("ree".to_string()))),
            }])
        })
    }
    #[test]
    fn struct_mod_both() { 
        assert_type("{ foo: bar } out + {asdf: ree} in + {}", Type { 
            kind: TypeKind::Struct(vec![ StructField { 
                name: "foo".to_string(), 
                typ: make_simple_type(TypeKind::Identifier(GenericIdentifier::Simple("bar".to_string()))),
            }]),

            optional: false, 

            in_plus: Some(vec![ ]), 
            out_plus: Some(vec![ StructField { 
                name: "asdf".to_string(), 
                typ: make_simple_type(TypeKind::Identifier(GenericIdentifier::Simple("ree".to_string()))),
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
    fn struct_mod_type_err() { 
        let res = parse_type("[ foo<T> ] out + { foo: bar }");
        assert_eq!(res.unwrap_err().kind, ParseErrorKind::SemanticStructOnly(Terminal::Keyword(Keyword::Out)));

        let res = parse_type("[ foo<T> ] in + { foo: bar }");
        assert_eq!(res.unwrap_err().kind, ParseErrorKind::SemanticStructOnly(Terminal::Keyword(Keyword::In)));
    }

    #[test]
    fn optional_generic() {
        assert_type("foo<T>?", Type { 
            kind: TypeKind::Identifier(GenericIdentifier::Generic(
                "foo".to_string(), 
                vec![ GenericIdentifier::Simple("T".to_string()) ]
            )),
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