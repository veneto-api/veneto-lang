use std::backtrace::Backtrace;
use std::str::FromStr;

use strum_macros::{EnumString, Display};

use crate::parse::{ ParseResult, ParseErrorKind, ParseError }; 
use crate::parse::lexer::{TokenStream, Span}; 
use crate::parse::tokens::{ Punctuation, Keyword, Number, Terminal, TokenKind, Token }; 
use crate::parse::ast::general::GenericIdentifier;
use crate::peek_match;

use super::general::GenericDeclaration;
use super::{parse_list_into, Spanned, Finishable};
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
    Basic(GenericDeclaration), 

    /// An extended resource class;
    /// the `String` is its identifier (it may not have parameters),
    /// the `GenericIdentifier` references the extended RC (it may have parameters),
    Extended(Spanned<String>, GenericIdentifier), 
}

#[derive(Debug)]
pub enum RCComponent { 

    /// The base data type declared with the `data` keyword
    /// 
    /// (This cannot be a reference to another RC at this time)
    Data(Type),

    Embed(Type), 

    /// The interface expresssion (reference or literal) declared for this RC
    Interface(InterfaceExpression), 

    Links(LinksBlock), 

    Method(Method),
}


/// The body of a resource class,
/// beginning with an open brace
#[derive(Debug)]
pub struct ResourceClass { 
    /// The declaration for this resource class,
    /// including its parameter information
    pub declaration: RCDeclaration, 

    pub components: Vec<RCComponent>, 
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

    pub fn base_name(&self) -> &Spanned<String> { 
        match &self.declaration { 
            RCDeclaration::Basic(gid) => &gid.base, 
            RCDeclaration::Extended(base, _) => base, 
        }
    }
}

impl Finishable for ResourceClass { 
    const INITIAL_TOKEN: Terminal = Terminal::Keyword(Keyword::Resource);

    fn parse_finish(stream: &mut TokenStream, _: Token) -> ParseResult<Self> {
        
        let declaration = { 
            let ident : GenericDeclaration = GenericIdentifier::parse_expect(stream)?.try_into()?; 

            if stream.peek_for_keyword(Keyword::Extends)? { 

                if ident.args.is_some() { 
                    let err_ref = stream.peek()?; 
                    return Err(err_ref.as_semantic_error("Resource classes cannot have modifiers if they are extended"))
                }
                else { 
                    RCDeclaration::Extended(ident.base, GenericIdentifier::parse_expect(stream)?)
                }

            }
            else { RCDeclaration::Basic(ident) }
        };

        let mut components = Vec::<RCComponent>::new(); 

        stream.next()?.expect_punctuation(Punctuation::BraceOpen)?; 
        loop { 

            peek_match!(stream.peek_for_terminal { 
                
                Terminal::Keyword(Keyword::Data) => components.push(RCComponent::Data(Type::parse_expect(stream)?)),
                Terminal::Keyword(Keyword::Embed) => components.push(RCComponent::Embed(Type::parse_expect(stream)?)), 
                Terminal::Keyword(Keyword::Interface) => components.push(RCComponent::Interface(InterfaceExpression::parse_expect(stream)?)),
                Terminal::Keyword(Keyword::Links) => components.push(RCComponent::Links(LinksBlock::parse_expect(stream)?)),
                Terminal::Punctuation(Punctuation::BraceClose) => break, 
                _ => { 

                    if let Some(method) = Method::parse_peek(stream)? { 
                        components.push(RCComponent::Method(method))
                    } else { 
                        return Err(stream.next()?.as_err_unexpected()) 
                    } 
                }
            })
        }

        Ok(ResourceClass { declaration, components })
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
impl Peekable for Spanned<Metaclass> { 
    fn parse_peek(stream: &mut TokenStream) -> ParseResult<Option<Self>> {
        let sigil = stream.peek()?; 
        if sigil.as_punctuation() == Some(Punctuation::SpecialType) { 
            stream.next()?; // Advancing past peeked token...
                            // We should probably make peek_for an `Option<Span>` instead of a boolean 
                            //TAG: TERMINAL_PEEK_SPANS

            let token = stream.next()?; 
            let token_err = |_| ParseError { 
                kind: ParseErrorKind::UnknownSpecialType, 
                span: token.span, 
                backtrace: Backtrace::capture(), 
            };

            let TokenKind::Word(word) = token.kind else { return Err(token.as_err(ParseErrorKind::UnknownSpecialType)) };

            Ok(Some(Spanned { 
                node: Metaclass::from_str(&word).map_err(token_err)?, 
                span: sigil.span.through(token.span) 
            }))
        }
        else { Ok(None) }
    }
}

/// This represents a type expression within the context of a resource class;
/// it contains a normal type or an `RCReference`.  
#[derive(Debug, PartialEq)]
pub enum RCType { 
    Normal(Type),
    Special(Spanned<Metaclass>), 
    //TAG: RC_FLAGS
    // When we add RC flags, `Normal` may have to be changed
}
impl RCType { 
    fn span(&self) -> Span {
        match self { 
            Self::Normal(_) => todo!(), // Uncomment test err_get_body when this is implemented 
            Self::Special(typ) => typ.span, 
        }
    }
}


impl Expectable for RCType { 
    fn parse_expect(stream: &mut TokenStream) -> ParseResult<Self> {
        if let Some(metaclass) = Spanned::<Metaclass>::parse_peek(stream)? { 
            Ok(RCType::Special(metaclass))
        }
        else { 
            Type::parse_expect(stream).map(RCType::Normal)
        }
    }
}
impl Peekable for RCType { 
    fn parse_peek(stream: &mut TokenStream) -> ParseResult<Option<Self>> {
        if let Some(metaclass) = Spanned::<Metaclass>::parse_peek(stream)? { 
            Ok(Some(RCType::Special(metaclass)))
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
    Special(Spanned<Metaclass>)
}
impl Expectable for RCReference { 
    fn parse_expect(stream: &mut TokenStream) -> ParseResult<Self> {
        if let Some(metaclass) = Spanned::<Metaclass>::parse_peek(stream)? { 
            Ok(RCReference::Special(metaclass))
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
    pub rel: Spanned<String>, 
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
impl Peekable for MethodName { 
    fn parse_peek(stream: &mut TokenStream) -> ParseResult<Option<Self>> {
        let token = stream.peek()?; 
        let TokenKind::Word(str) = token.kind else { return Ok(None) }; 

        match Self::from_str(&str) { 
            Ok(x) => { 
                stream.next()?; // Gotta advance the stream if we match a `MethodName`
                Ok(Some(x))
            }, 
            Err(_) => Ok(None), 
        }
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


impl Peekable for Method { 
    fn parse_peek(stream: &mut TokenStream) -> ParseResult<Option<Self>> {
        let Some(name) = MethodName::parse_peek(stream)? else { return Ok(None) };

        let mut input : Option<MethodInput> = None; 
        if let Some(typ) = RCType::parse_peek(stream)? { 
            if [ MethodName::Get, MethodName::Delete ].contains(&name) { 
                return Err(ParseError { 
                    kind: ParseErrorKind::Semantic(format!("{} methods cannot have a request body", &name)), 
                    span: typ.span(), 
                    backtrace: Backtrace::capture(), 
                })
            }

            let lax = stream.peek_for_punctuation(Punctuation::Lax)?; 

            input = Some(MethodInput { typ, lax });
        }

        let mut outputs = Vec::<MethodOutput>::new(); 
        if stream.peek_for_punctuation(Punctuation::Arrow)? { 
            loop { 
                let err_ref = stream.peek()?; 

                let mut status : Option<Number> = None; 
                let mut typ : Option<RCType> = None; 

                if stream.peek_for_punctuation(Punctuation::HttpStatus)? { 
                    status = Some(stream.next()?.try_as_number()?);

                    if stream.peek_for_punctuation(Punctuation::Colon)? { 
                        typ = Some(RCType::parse_expect(stream)?);
                    }
                }
                else { 
                    typ = RCType::parse_peek(stream)?; 
                }

                if outputs.iter().any(|other| other.status == status) { 
                    return Err(err_ref.as_err(ParseErrorKind::SemanticDuplicate))
                }

                if status.is_none() && typ.is_none() { 
                    return Err(stream.next()?.as_semantic_error("Expected a response status and/or type"))
                }

                outputs.push(MethodOutput { status, typ });

                if stream.peek_for_punctuation(Punctuation::Comma)? { continue } 
                else { break } 
            }
        }

        Ok(Some(Method { name, input, outputs }))
        
    }
}



#[cfg(test)]
mod test {

    use crate::parse::ast::general::test::{assert_gid};
    use crate::parse::ast::interfaces::{InterfaceExpression};
    use crate::parse::ast::rc::RCComponent;
    use crate::parse::ast::types::{ TypeKind };
    use crate::parse::lexer::{Span, Position};
    use crate::parse::lexer_tests::token_stream;
    use crate::parse::{ParseResult, ParseErrorKind};
    use crate::parse::ast::{Expectable, Peekable};

    use super::{Method, RCType, MethodName, Metaclass, LinksBlock, ResourceClass, RCDeclaration, RCReference };

    macro_rules! assert_rc_type {
        ($typ:expr, $val:literal) => {
            let RCType::Normal(typ) = $typ else { panic!("Expected normal RC type") }; 
            let TypeKind::Identifier(gid) = typ.kind else { panic!("Expected RC type to be an Identifier") }; 
            assert_eq!(gid.base.node, $val); 
        };
    }

    //
    // Methods
    //

    fn parse_method(input: &str) -> ParseResult<Method> { 
        let mut stream = token_stream(input); 
        Method::parse_peek(&mut stream).map(|opt| opt.expect("Expected a method here"))
    }  

    #[test]
    fn method_empty() { 
        let method = parse_method("DELETE").unwrap(); 
        assert_eq!(method.name, MethodName::Delete); 
        assert_eq!(method.input, None);
        assert!(method.outputs.is_empty()); 
    }

    #[test]
    fn method_complete() { 
        let method = parse_method("POST foo -> #200: bar").unwrap(); 
        assert_eq!(method.name, MethodName::Post); 
        let Some(input) = method.input else { panic!() }; 
        assert!(!input.lax); 
        assert_rc_type!(input.typ, "foo"); 

        let mut iter = method.outputs.into_iter(); 
        let next = iter.next().unwrap(); 
        assert_eq!(next.status.unwrap(), "200"); 
        assert_rc_type!(next.typ.unwrap(), "bar"); 

        assert_eq!(iter.next(), None); 
    }

    #[test]
    fn method_empty_bodies() { 
        let method = parse_method("POST -> @empty").unwrap();
        assert_eq!(method.input, None); 

        let mut iter = method.outputs.into_iter();
        let next = iter.next().unwrap(); 
        assert_eq!(next.status, None); 

        let Some(RCType::Special(metaclass)) = next.typ else { panic!() }; 
        assert_eq!(metaclass.node, Metaclass::Empty);
        assert_eq!(metaclass.span, Span { lo: Position(8), hi: Position(14) });

        assert_eq!(iter.next(), None); 
    }

    #[test]
    fn method_multiple_response() { 
        let method = parse_method("POST -> bar, #204, #422: foo").unwrap(); 
        let mut iter = method.outputs.into_iter(); 

        let next = iter.next().unwrap(); 
        assert_eq!(next.status, None); 
        assert_rc_type!(next.typ.unwrap(), "bar"); 

        let next = iter.next().unwrap(); 
        assert_eq!(next.status, Some("204".to_string())); 
        assert_eq!(next.typ, None); 

        let next = iter.next().unwrap(); 
        assert_eq!(next.status, Some("422".to_string())); 
        assert_rc_type!(next.typ.unwrap(), "foo"); 
    }

    #[test]
    fn err_empty_response() { 
        let err = parse_method("POST foo ->");
        assert!(err.is_err());
    }

    // #[test]
    // fn err_get_body() { 
    //     let err = parse_method("GET foo -> #200").unwrap_err();
    //     assert!(matches!(err.kind, ParseErrorKind::Semantic(_)));
    // }

    #[test]
    fn err_duplicate_responses() { 
        let err = parse_method("POST -> #201, #400: err, #201: foo").unwrap_err();
        assert_eq!(err.kind, ParseErrorKind::SemanticDuplicate);
    }

    #[test]
    fn method_type_literal() { 
        let method = parse_method("PATCH { foo: bar } -> baz[]").unwrap(); 
        assert_eq!(method.name, MethodName::Patch); 

        let Some(input) = method.input else { panic!() };
        let RCType::Normal(typ) = input.typ else { panic!() }; 
        let TypeKind::Struct(_) = typ.kind else { panic!() }; 
        assert!(!input.lax); 

        let mut iter = method.outputs.into_iter(); 
        let next = iter.next().unwrap(); 
        let Some(RCType::Normal(typ)) = next.typ else { panic!() }; 
        let TypeKind::Array(_) = typ.kind else { panic!() }; 

        assert_eq!(iter.next(), None); 
    }

    #[test]
    fn lax() { 
        let method = parse_method("PATCH foo%").unwrap(); 
        assert_eq!(method.name, MethodName::Patch); 

        let input = method.input.unwrap(); 
        assert_rc_type!(input.typ, "foo"); 
        assert!(input.lax); 
    }


    //
    // Links
    //


    fn parse_links(input: &str) -> ParseResult<LinksBlock> { 
        LinksBlock::parse_expect(&mut token_stream(input))
    }

    #[test]
    fn links_simple() { 
        let links = parse_links("{ foo -> bar }").unwrap();
        let mut iter = links.into_iter(); 

        let next = iter.next().unwrap(); 
        assert_eq!(next.rel.node, "foo"); 

        let RCReference::Normal(typ) = next.typ else { panic!() }; 
        assert_gid!(typ, "bar"); 

        assert_eq!(iter.next(), None); 
    }

    #[test]
    fn links_special() { 
        let links = parse_links("{ foo? -> @self }").unwrap(); 
        let mut iter = links.into_iter(); 

        let next = iter.next().unwrap(); 
        assert_eq!(next.rel.node, "foo"); 
        assert!(next.optional); 

        let RCReference::Special(metaclass) = next.typ else { panic!() }; 
        assert_eq!(metaclass.node, Metaclass::RCSelf);

        assert_eq!(iter.next(), None); 
    }

    #[test]
    fn links_complex() { 
        let links = parse_links("{ bar -> foo<baz>, other -> @media }").unwrap();
        let mut iter = links.into_iter(); 

        let next = iter.next().unwrap(); 
        assert_eq!(next.rel.node, "bar"); 
        assert!(!next.optional); 

        let RCReference::Normal(typ) = next.typ else { panic!() }; 
        assert_gid!(typ, "foo" < "baz" >); 

        let next = iter.next().unwrap(); 
        assert_eq!(next.rel.node, "other"); 

        let RCReference::Special(metaclass) = next.typ else { panic!() }; 
        assert_eq!(metaclass.node, Metaclass::Media);

        assert!(!next.optional); 

        assert_eq!(iter.next(), None); 
    }

    //
    // Integration
    //

    fn parse_rc(input: &str) -> ParseResult<ResourceClass> { 
        ResourceClass::parse_peek(&mut token_stream(input))
            .map(Option::unwrap)
    }

    #[test]
    fn simple_rc() { 
        let rc = parse_rc("resource Foo { 
            data int[]

            interface Queryable

            links { 
                bar -> baz, 
            }

            GET -> #200

            PATCH @self% -> @self

        }").unwrap();

        let RCDeclaration::Basic(ident) = rc.declaration else { panic!() }; 
        assert_gid!(ident, "Foo"); 

        let mut iter = rc.components.into_iter(); 

        let next = iter.next().unwrap(); 
        let RCComponent::Data(typ) = next else { panic!() };
        let TypeKind::Array(typ) = typ.kind else { panic!() }; 
        let TypeKind::Identifier(typ) = typ.kind else { panic!() }; 
        assert_gid!(typ, "int"); 

        let next = iter.next().unwrap(); 
        let RCComponent::Interface(expr) = next else { panic!() }; 
        let InterfaceExpression::Identifier(expr) = expr else { panic!() };
        assert_eq!(expr.node, "Queryable"); 

        let next = iter.next().unwrap(); 
        let RCComponent::Links(_) = next else { panic!() }; 

        assert!(matches!(iter.next().unwrap(), RCComponent::Method(_)));
        assert!(matches!(iter.next().unwrap(), RCComponent::Method(_)));
        assert!(iter.next().is_none()); 
    }

    #[test]
    fn rc_generics() { 
        let rc = parse_rc("resource foo<T> {}").unwrap(); 
        let RCDeclaration::Basic(gid) = rc.declaration else { panic!() }; 
        // assert_gid!(gid, "foo" < "T" >); 
    }

    #[test]
    fn rc_extends() { 
        let rc = parse_rc("resource foo extends bar<T> {}").unwrap(); 
        let RCDeclaration::Extended(base, gid) = rc.declaration else { panic!() }; 
        assert_eq!(base.node, "foo"); 
        assert_gid!(gid, "bar" < "T" >); 
    }
    
    #[test]
    fn rc_data_literal() { 
        let rc = parse_rc("resource foo { embed { foo: bar }[] }").unwrap(); 

        let mut iter = rc.components.into_iter(); 
        let next = iter.next().unwrap(); 
        let RCComponent::Embed(typ) = next else { panic!() };
        let TypeKind::Array(typ) = typ.kind else { panic!() }; 
        let TypeKind::Struct(typ) = typ.kind else { panic!() }; 
        assert!(iter.next().is_none()); 

        let mut iter = typ.into_iter(); 
        let field = iter.next().unwrap(); 
        assert_eq!(iter.next(), None); 

        assert_eq!(field.name.node, "foo"); 
        let TypeKind::Identifier(gid) = field.typ.kind else { panic!() }; 
        assert_eq!(gid.base.node, "bar"); 
    }

}