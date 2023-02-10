use std::str::FromStr;

use strum_macros::{EnumString, Display};

use crate::parse::{ ParseResult, ParseErrorKind }; 
use crate::parse::lexer::TokenStream; 
use crate::parse::tokens::{ Punctuation, Keyword, Number }; 
use crate::parse::ast::general::GenericIdentifier;

use super::{Expectable, types::Type, interfaces::InterfaceExpression, Peekable};

pub struct ResourceClass { 
    /// The identifier for this RC, 
    /// including its generic params, and perhaps flags in the future 
    /// 
    /// TAG: [RC_FLAGS]
    pub ident : RCIdentifier, 

    /// If `Some`, then this RC extends the referenced RC 
    pub extends : Option<RCIdentifier>
}


//
// Identifiers
//

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

enum RCPart { 
    Data(Type), 
    Interface(InterfaceExpression), 
    LinksBlock(LinksBlock), 
    Method(Method), 
}
impl Expectable for RCPart { 
    fn parse_expect(stream: &mut TokenStream) -> ParseResult<Self> {
        
        let first = stream.next()?; 
        match first.as_keyword() { 
            Some(Keyword::Data) => Ok(Self::Data(Type::parse_expect(stream)?)),
            Some(Keyword::Interface) => Ok(Self::Interface(InterfaceExpression::parse_expect(stream)?)),
            Some(Keyword::Links) => Ok(Self::LinksBlock(LinksBlock::parse_expect(stream)?)),
            _ => { 

                if let Some(method) = Method::parse_peek(stream)? { 
                    Ok(Self::Method(method))
                } else { 
                    Err(first.as_err_unexpected())
                }

            }
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

type LinksBlock = Vec<Link>;
struct Link { 
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

#[derive(EnumString, Debug, Display, PartialEq, Eq)]
#[strum(serialize_all="UPPERCASE")]
pub enum MethodName { 
    Get, 
    Post, 
    Patch,
    Put, 
    Delete,
}


struct Method { 
    pub name : MethodName, 
    pub input : Option<MethodInput>, 
    pub outputs : Vec<MethodOutput>, 
}
struct MethodInput { 
    typ: RCType, 
    lax: bool, 
}
struct MethodOutput { 
    typ: Option<RCType>, 
    status: Option<Number>
    //TAG: REASON_PHRASES
    // https://veneto.notion.site/HTTP-reason-phrases-instead-of-codes-772c7fc3d21146aa922573913c8adc35
}

impl Peekable for Method { 
    fn parse_peek(stream: &mut TokenStream) -> ParseResult<Option<Self>> {
        if let Some(name) = stream.peek_for_identifier()?.and_then(|s| MethodName::from_str(&s).ok()) { 

            // First, try to grab the input 

            let mut input : Option<MethodInput> = None; 
            let peek = stream.peek()?; 
            if stream.peek()?.is_identifier() { 
                let typ = RCType::parse_expect(stream)?; 
                input = Some(MethodInput { 
                    typ, 
                    lax: stream.peek_for_puncutation(Punctuation::Lax)?,
                });

                // Can't use `parse_peek` here unfortunately because of this - 
                // we may need the original token to raise this error
                // (This goes away if we move semantic error checking out of the parser, like it probably should be)
                // Namely for https://veneto.notion.site/Recoverable-Errors-92e75e448ded4492a938b7d1c24711de
                if name == MethodName::Get { 
                    return Err(peek.as_semantic_error("GET requests cannot have a request body"))
                }
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

            Ok(Some(Self { name, input, outputs }))
        }
        else { Ok(None) }
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
pub enum RCType { 
    Normal(Type),
    Special(SpecialType), 
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