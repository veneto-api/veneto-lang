use crate::parse::{ClauseResult, ClauseDelim};
use crate::parse::{lexer::TokenStream, ParseResult};
use crate::parse::tokens::{Punctuation, TokenKind, Keyword};

use super::general::GenericIdentifier;

use eyre::eyre;
use strum_macros::Display;

#[derive(PartialEq, Display, Debug)]
pub enum TypeKind { 
    /// A reference to an existing type 
    Identifier(GenericIdentifier),

    /// A tuple represented by the included types, in order
    Tuple(Vec<Type>),

    Struct(Struct),

    /// A homogenous array of a single type 
    Array(Box<Type>), 
    // 👆 the compiler complains if it's "without indirection", so it must be boxed. TIL!
}

/// This is a separate enum of all of the non-array `TypeKind`s. 
/// Since array brackets are postfix, we have to process all of this first before determining if it's an array
enum InnerTypeKind { 
    Identifier(GenericIdentifier),

    Tuple(Vec<Type>),

    Struct(Struct),
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
    kind: TypeKind, 

    /// If `true`, this type has been marked with the lax operator `%`.  
    /// This takes precedence over the `optional`.  
    /// 
    /// This system of precedence is probably bad, we may very well redo this in the future
    lax: bool, 
    
    /// If `true`, this type has been marked optional with a `?`.  
    /// 
    /// Note that in the future we will likely have nested optionals, tracked in the below Notion page.
    /// This will invalidate the current system of assuming that there's only one. 
    /// 
    /// https://veneto.notion.site/Unions-Monadic-Optionals-e39088b0b78e48ebafb941faef447dcb
    optional: bool, 
}

/// This represents a Veneto "struct" 
#[derive(PartialEq, Debug)]
pub struct Struct { 
    /// The main body of the struct
    body: StructBody, 

    /// The fields added with the `in +` modifier
    in_plus: Option<StructBody>, 
    /// The fields added with the `out +` modifier 
    out_plus: Option<StructBody>, 
}

/// This is a Struct's body, which is just its set of fields
pub type StructBody = Vec<StructField>;

/// This is an individual field declaration within a `Struct`
#[derive(PartialEq, Debug)]
pub struct StructField { 
    name: String, 
    /// This is the field's type, 
    /// abbreviated to `typ` because `type` is a reserved keyword
    typ: Type, 
}

impl Type { 
    fn from_inner_kind(inner_kind: InnerTypeKind) -> Self { 
        Self { 
            kind: inner_kind.into(),
            lax: false,
            optional: false, 
        }
    }

    fn wrap_as_array(old: Self) -> Self { 
        Self { 
            kind: TypeKind::Array(Box::new(old)), 
            lax: false, 
            optional: false, 
        }
    }

    fn begin_kind(stream: &mut TokenStream) -> ParseResult<InnerTypeKind> { 
        let first = stream.next()?; 

        // First check for the easy ones - a brace or bracket indicates a struct or tuple, respectively
        match first.as_punctuation() {
            // This argument lining up reminds me of this video
            // https://www.youtube.com/watch?v=VMEAjTYwEew          👇 
            Some(Punctuation::BraceOpen) => return Struct::finish(stream).map(InnerTypeKind::Struct),
            Some(Punctuation::BracketOpen) => return finish_tuple(stream).map(InnerTypeKind::Tuple),
            _ => {}, 
        }

        // Now we have to deal with identifiers, which could be a `GenericIdentifier` itself or an array of them 
        // We avoid expecting an identifier explicitly so that the correct error is presented downstream
        // We have to check for arrays downstream since the array brackets are postfix, which is why we use `InnerTypeKind` here
        if let Some(ident) = first.as_identifier() { 
            let ident = GenericIdentifier::finish(stream, ident)?;
            Ok(InnerTypeKind::Identifier(ident))
        }
        else { 
            Err(first.as_err_unexpected())
        }
    }

    pub fn begin(stream: &mut TokenStream) -> ParseResult<Self> { 
        let mut typ = Self::from_inner_kind(Self::begin_kind(stream)?);

        loop { 
            let peek = stream.peek()?; 
            match peek.as_punctuation() { 
                Some(Punctuation::Lax) => { 
                    if typ.lax { 
                        return Err(peek.as_semantic_error(eyre!("The lax operator can only be applied once (Nested optionals are not supported yet)")))
                    }
                    stream.next()?; 
                    typ.lax = true 
                }

                Some(Punctuation::Optional) => { 
                    if typ.optional { 
                        return Err(peek.as_semantic_error(eyre!("Nested optionals are not supported yet")))
                    }
                    stream.next()?; 
                    typ.optional = true 
                }

                Some(Punctuation::BracketOpen) => { 
                    stream.next()?; 
                    stream.next()?.expect_punctuation(Punctuation::BracketClose)?; 
                    typ = Self::wrap_as_array(typ); 
                }

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
                let typ = Type::begin(stream)?;
                let res = Some(Self{ name, typ });
        
                let peek = stream.peek()?; 
                match peek.as_punctuation() { 
                    Some(Punctuation::Comma) => Ok((res, ClauseDelim::Continue)), 
                    Some(Punctuation::BraceClose) => Ok((res, ClauseDelim::Exit)),
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

impl Struct { 
    pub fn finish(stream: &mut TokenStream) -> ParseResult<Self> { 
        let body = finish_struct_body(stream)?; 
        let mut in_plus : Option<StructBody>    = None;
        let mut out_plus : Option<StructBody>   = None; 

        //TAG: IN_OUT_MODIFIERS
        fn parse_extension(stream: &mut TokenStream) -> ParseResult<StructBody> { 
            stream.next()?.expect_punctuation(Punctuation::StructExtension)?;
            stream.next()?.expect_punctuation(Punctuation::BraceOpen)?; 
            finish_struct_body(stream)
        }

        loop { 
            if let Some(Punctuation::Comma) = stream.peek()?.as_punctuation() { 
                let kw = stream.next()?;
                match kw.as_keyword() { 
                    Some(Keyword::In) => { 
                        if in_plus.is_some() { 
                            return Err(kw.as_semantic_error(eyre!("This struct already has an `in +` modifier")));
                        }
                        in_plus = Some(parse_extension(stream)?);
                    },

                    Some(Keyword::Out) => { 
                        if out_plus.is_some() { 
                            return Err(kw.as_semantic_error(eyre!("This struct already has an `out +` modifier")));
                        }
                        out_plus = Some(parse_extension(stream)?);
                    },

                    _ => return Err(kw.as_err_unexpected()), 
                }
            }
            else { 
                return Ok(Struct { body, in_plus, out_plus })    
            }
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

        let typ = Type::begin(stream)?; 
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
mod test {
    use crate::parse::{ParseResult, lexer::TokenStream, lexer_tests::{token_stream, assert_punctuation}, ast::general::GenericIdentifier, tokens::Punctuation, ParseErrorKind};

    use super::{Type, TypeKind, Struct, StructField};
 
    // Yeehaw! This is gonna be a big one 

    fn parse_type(input: &str) -> ParseResult<(TokenStream, Type)> { 
        let mut stream = token_stream(input); 
        let typ = Type::begin(&mut stream);
        typ.map(|x| (stream, x))
    }
    fn assert_type_kind(input: &str, expected: TypeKind) { 
        let (_, typ) = parse_type(input).unwrap();
        assert_eq!(typ.kind, expected);
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
        assert_type_kind("{ foo: bar }", TypeKind::Struct(Struct {
            body: vec![ 
                StructField { 
                    name: "foo".to_string(), 
                    typ: Type { 
                        kind: TypeKind::Identifier(GenericIdentifier::Simple("bar".to_string())), 
                        lax: false, 
                        optional: false 
                    }
                }
            ],
            in_plus: None,
            out_plus: None,
        }))
    }

    #[test]
    fn simple_array() { 
        assert_type_kind("foo[]", TypeKind::Array(
            Box::new(Type { 
                kind: TypeKind::Identifier(GenericIdentifier::Simple("foo".to_string())), 
                lax: false, 
                optional: false 
            })
        ));
    }

    #[test] 
    fn tuple_generic() { 
        assert_type_kind("[ foo, bar<baz> ]", TypeKind::Tuple(
            vec![ 
                Type { 
                    kind: TypeKind::Identifier(GenericIdentifier::Simple("foo".to_string())),
                    lax: false, 
                    optional: false, 
                }, 
                Type { 
                    kind: TypeKind::Identifier(GenericIdentifier::Generic("bar".to_string(), vec![ GenericIdentifier::Simple("baz".to_string()) ])),
                    lax: false, 
                    optional: false, 
                }
            ]
        ));
    }

    #[test] 
    fn generic_double_array() { 
        assert_type_kind("foo<bar<asdf, ree>>[][]", TypeKind::Array( Box::new(Type { 
                kind: TypeKind::Array(Box::new(Type { 
                    kind: TypeKind::Identifier(
                        GenericIdentifier::Generic("foo".to_string(), vec![ 
                            GenericIdentifier::Generic("bar".to_string(), vec![ 
                                GenericIdentifier::Simple("asdf".to_string()),
                                GenericIdentifier::Simple("ree".to_string()),
                            ])
                        ])
                    ), 
                    lax: false, 
                    optional: false, 
                })),
                lax: false, 
                optional: false, 
            })
        ))
    }
}