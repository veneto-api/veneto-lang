use std::str::FromStr;

use crate::parse::ParseErrorKind;
use crate::parse::{lexer::TokenStream, ParseResult};
use crate::parse::tokens::{Punctuation, Keyword, Terminal, TokenKind, Token, Primitive};
use crate::{peek_match};

use super::{Expectable, Peekable, Finishable, Spanned};
use super::general::GenericIdentifier;

use strum_macros::Display;

#[derive(PartialEq, Display, Debug)]
pub enum TypeKind { 
    Primitive(Primitive),

    /// A reference to an existing type 
    Identifier(GenericIdentifier),

    /// A tuple represented by the included types, in order
    Tuple(Tuple),

    Struct(StructBody),

    /// A homogenous array of a single type 
    Array(Box<Type>), 
}

/// This is a separate enum of all of the non-array `TypeKind`s. 
/// 
/// Since array brackets are postfix, we have to process all of this first before determining if it's an array
/// This might not be necessary but I did it for now, it felt right to have explicit type checking here 
enum InnerTypeKind { 
    Identifier(GenericIdentifier),

    Tuple(Tuple),

    Struct(StructBody),

    Primitive(Primitive), 
}
impl From<InnerTypeKind> for TypeKind {
    fn from(val: InnerTypeKind) -> Self {
        match val { 
            InnerTypeKind::Primitive(x) => TypeKind::Primitive(x), 
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

    //TODO: Implement span here, 
    // then remove `todo!()` for error spans and stuff in `rc.rs` 
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
        let peek = stream.peek()?; 
        if let TokenKind::Word(word) = peek.kind { 
            if let Ok(prim) = Primitive::from_str(&word) { 
                stream.next()?; 
                return Ok(Some(InnerTypeKind::Primitive(prim)))
            }
        }

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
        if let Some(inner_kind) = Self::begin_peek(stream)? { 
            Self::finish(inner_kind, stream)
        } else { 
            Err(stream.next()?.as_err(ParseErrorKind::ExpectedTypeExpression))
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
    pub name: Spanned<String>, 
    /// This is the field's type, 
    /// abbreviated to `typ` because `type` is a reserved keyword
    pub typ: Type, 
}
impl Finishable for StructBody { 
    const INITIAL_TOKEN: Terminal = Terminal::Punctuation(Punctuation::BraceOpen);

    fn parse_finish(stream: &mut TokenStream, _initial: Token) -> ParseResult<Self> {
        let mut fields = Vec::<StructField>::new(); 
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

pub type Tuple = Vec<Type>; 
impl Peekable for Tuple { 
    fn  parse_peek(stream: &mut TokenStream) -> ParseResult<Option<Self>> {
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
    use core::panic;

    use super::super::general::test::{assert_gid, assert_gid_base};
    use crate::parse::ast::Expectable;
    use crate::parse::{ParseResult, lexer::TokenStream, lexer_tests::{token_stream, assert_punctuation}, ParseErrorKind};
    use crate::parse::tokens::{Punctuation}; 

    use super::{Type, TypeKind};
 
    // Yeehaw! This is gonna be a big one 

    fn parse_type(input: &str) -> ParseResult<(TokenStream, Type)> { 
        let mut stream = token_stream(input); 
        let typ = Type::parse_expect(&mut stream);
        typ.map(|x| (stream, x))
    }

    pub fn make_simple_type(kind: TypeKind) -> Type { 
        Type { 
            kind,  
            optional: false, 
            in_plus: None, 
            out_plus: None, 
        }
    }

    macro_rules! assert_type_gid {
        ($typ:expr, $gid:tt) => {
            let TypeKind::Identifier(gid) = $typ.kind else { panic!("Expected GID") };
            assert_gid!(gid, $gid);
        };
    }
    pub(crate) use assert_type_gid; 

    #[test]
    fn simple_ident() { 
        let (mut stream, typ) = parse_type("asdf@").unwrap();
        assert_type_gid!(typ, "asdf"); 
        assert_punctuation(&mut stream, Punctuation::SpecialType);
    }

    #[test]
    fn struct_eof() { 
        let res = parse_type("{ asdf: eee ");
        assert!(matches!(res.unwrap_err().kind, ParseErrorKind::Unexpected(_)))
    }

    #[test]
    fn simple_struct() { 
        let (_, typ) = parse_type("{ foo: bar }").unwrap(); 
        let TypeKind::Struct(s) = typ.kind else { panic!("Expected Struct") }; 

        let mut iter = s.into_iter(); 

        let field = iter.next().unwrap(); 
        assert_eq!(field.name.node, "foo"); 
        assert_type_gid!(field.typ, "bar"); 
    }

    #[test]
    fn simple_array() { 
        let (_, typ) = parse_type("foo[]").unwrap(); 
        let TypeKind::Array(inner) = typ.kind else { panic!("Expected Array") };

        assert_type_gid!(inner, "foo"); 
    }

    #[test] 
    fn tuple_generic() { 

        let (_, typ) = parse_type("[ foo, bar<baz> ]").unwrap();
        let TypeKind::Tuple(tup) = typ.kind else { panic!("Expected tuple"); };

        let mut iter = tup.into_iter(); 

        let next = iter.next().unwrap(); 
        let TypeKind::Identifier(gid) = next.kind else { panic!("Expected GID") };
        assert_gid!(gid, "foo"); 

        let next = iter.next().unwrap(); 
        let TypeKind::Identifier(gid) = next.kind else { panic!("Expected GID") }; 
        assert_gid!(gid, "bar" < "baz" >); 
    }

    #[test] 
    fn generic_double_array() { 
        let (_, typ) = parse_type("foo<bar<asdf, ree>>[][]").unwrap(); 

        let TypeKind::Array(inner) = typ.kind else { panic!("Expected outer array") };
        let TypeKind::Array(inner) = inner.kind else { panic!("Expected inner array") }; 
        let TypeKind::Identifier(gid) = inner.kind else { panic!("Expected GID") };

        assert_gid_base!(gid: "foo" @ 0); 

        let mut iter2 = gid.args.unwrap().into_iter(); 
        let arg = iter2.next().unwrap(); 
        assert_gid_base!(arg: "bar" @ 4); 

        let mut iter3 = arg.args.unwrap().into_iter(); 

        let arg = iter3.next().unwrap(); 
        assert_gid_base!(arg: "asdf" @ 8); 

        let arg = iter3.next().unwrap(); 
        assert_gid_base!(arg: "ree" @ 14);

        assert_eq!(iter3.next(), None); 
        assert_eq!(iter2.next(), None); 
    }

    #[test]
    fn struct_array() { 
        let (_, typ) = parse_type("{ foo: bar }[]").unwrap(); 
        let TypeKind::Array(typ) = typ.kind else { panic!("Expected array") };
        let TypeKind::Struct(typ) = typ.kind else { panic!("Expected struct") }; 

        let mut iter = typ.into_iter(); 

        let field = iter.next().unwrap(); 
        assert_eq!(field.name.node, "foo");
        assert_type_gid!(field.typ, "bar"); 

        assert_eq!(iter.next(), None); 
    }

    #[test]
    fn tuple_array() { 
        let (_, typ) = parse_type("[foo][]").unwrap();

        let TypeKind::Array(typ) = typ.kind else { panic!("Expected array") }; 
        let TypeKind::Tuple(typ) = typ.kind else { panic!("Expected tuple") };
        let mut iter = typ.into_iter();

        let typ = iter.next().unwrap(); 
        assert_type_gid!(typ, "foo"); 

        assert_eq!(iter.next(), None); 
    }

    #[test]
    fn complex_struct() { 
        let (_, typ) = parse_type("{ foo: bar[], baz: generic<type> }").unwrap(); 

        let TypeKind::Struct(fields) = typ.kind else { panic!("Expected struct") }; 
        let mut iter = fields.into_iter(); 

        let field = iter.next().unwrap(); 
        assert_eq!(field.name.node, "foo"); 

        let TypeKind::Array(field_type) = field.typ.kind else { panic!("Expected array field") };
        assert_type_gid!(field_type, "bar"); 

        let field = iter.next().unwrap(); 
        assert_eq!(field.name.node, "baz"); 
        let TypeKind::Identifier(field_typ) = field.typ.kind else { panic!() };
        assert_gid_base!(field_typ: "generic" @ 19);

        assert_eq!(iter.next(), None); 
    }

    #[test]
    fn struct_mod_out() { 
        let (_, typ) = parse_type("{ foo: bar } out + {asdf: ree}").unwrap();
        assert_eq!(typ.in_plus, None); 

        let Some(s) = typ.out_plus else { panic!() }; 
        let mut iter = s.into_iter(); 

        assert_eq!(iter.next().unwrap().name.node, "asdf"); 
        assert_eq!(iter.next(), None); 
    }

    #[test]
    fn optional() { 
        let (_, typ) = parse_type("foo<T>?").unwrap(); 
        assert!(typ.optional); 

        let TypeKind::Identifier(typ) = typ.kind else { panic!() };
        assert_eq!(typ.base.node, "foo"); 
    }


    #[test]
    fn err_optional_duplicate() { 
        let res = parse_type("foo??");
        assert!(matches!(res.unwrap_err().kind, ParseErrorKind::Semantic(_)));
    }
}