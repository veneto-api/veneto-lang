use crate::{parse::{lexer::{TokenStream}, ParseResult, tokens::{Keyword, Punctuation, TokenKind, Identifier}}};

use super::{types::Type, rc::ResourceClass, interfaces::InterfaceExpression, use_tree::UseTree, Expectable, Peekable, general::GenericIdentifier};


pub struct EntryPoint { 
    pub uri: String, 
    pub rc: GenericIdentifier, 
}

pub enum Node { 
    Use(UseTree),
    Type(Identifier, Type), 
    Interface(Identifier, InterfaceExpression), 
    ResourceClass(ResourceClass), 
    EntryPoint(EntryPoint), 
}

pub type Document = Vec<Node>; 

impl Expectable for Document { 

    fn parse_expect(stream: &mut TokenStream) -> ParseResult<Self> {
        let mut doc = Self::new(); 

        loop { 
            if let Some(rc) = ResourceClass::parse_peek(stream)? { 
                doc.push(Node::ResourceClass(rc));
                continue;
            }

            let next = stream.next()?; 
            match next.as_keyword() { 
                Some(Keyword::Use) => doc.push(Node::Use(UseTree::parse_expect(stream)?)),

                Some(Keyword::Type) => { 
                    let name = stream.next()?.try_as_identifier()?; 
                    stream.next()?.expect_punctuation(Punctuation::Assign)?; 
                    let typ = Type::parse_expect(stream)?; 
                    doc.push(Node::Type(name, typ))
                },

                Some(Keyword::Interface) => { 
                    let name = stream.next()?.try_as_identifier()?; 
                    stream.next()?.expect_punctuation(Punctuation::Assign)?; 
                    let int = InterfaceExpression::parse_expect(stream)?; 
                    doc.push(Node::Interface(name, int))
                },

                Some(Keyword::Entry) => { 
                    let uri = stream.next()?.try_as_string_literal()?; 
                    stream.next()?.expect_punctuation(Punctuation::Arrow)?; 
                    let rc = GenericIdentifier::parse_expect(stream)?; 
                    doc.push(Node::EntryPoint(EntryPoint { uri, rc }))
                }
                

                _ => {
                    if next.kind == TokenKind::EOF { 
                        return Ok(doc)
                    } else { 
                        return Err(next.as_err_unexpected())
                    }
                }
            }
        }

    }
}