use std::collections::HashMap;

use crate::parse::{lexer::TokenStream, ParseResult, tokens::{Keyword, Punctuation, TokenKind}};

use super::{types::Type, rc::{RCIdentifier, ResourceClass}, interfaces::InterfaceExpression, use_tree::UseTree, Expectable, Peekable};


pub struct EntryPoint { 
    uri: String, 
    rc: RCIdentifier, 
}

pub struct Document { 
    pub imports : Vec<UseTree>, 
    pub types: HashMap<String, Type>,
    pub interfaces : HashMap<String, InterfaceExpression>, 
    pub resource_classes : Vec<ResourceClass>, 
    pub entry_points : Vec<EntryPoint>, 
}
impl Document { 
    fn parse(mut stream: TokenStream) -> ParseResult<Self> { 

        let mut doc = Self { 
            imports: Vec::new(), 
            types: HashMap::new(), 
            interfaces: HashMap::new(), 
            resource_classes: Vec::new(), 
            entry_points: Vec::new(), 
        };

        loop { 

            if stream.peek_for_keyword(Keyword::Use)? { 
                doc.imports.push(UseTree::parse_expect(&mut stream)?);
            }
            else if stream.peek_for_keyword(Keyword::Type)? { 
                let name = stream.next()?.try_as_identifier()?; 
                stream.next()?.expect_punctuation(Punctuation::Assign)?; 
                let typ = Type::parse_expect(&mut stream)?; 
                doc.types.insert(name, typ);
            }
            else if stream.peek_for_keyword(Keyword::Interface)? { 
                let name = stream.next()?.try_as_identifier()?; 
                stream.next()?.expect_punctuation(Punctuation::Assign)?; 
                let int = InterfaceExpression::parse_expect(&mut stream)?; 
                doc.interfaces.insert(name, int); 
            }
            else if let Some(rc) = ResourceClass::parse_peek(&mut stream)? { 
                doc.resource_classes.push(rc);
            }
            else if stream.peek_for_keyword(Keyword::Entry)? { 
                let uri = stream.next()?.try_as_string_literal()?; 
                stream.next()?.expect_punctuation(Punctuation::Arrow)?; 
                let rc = RCIdentifier::parse_expect(&mut stream)?; 
                doc.entry_points.push(EntryPoint { uri, rc })
            }
            else { 
                let next = stream.next()?; 
                if next.kind == TokenKind::EOF { 
                    return Ok(doc)
                } else { 
                    return Err(next.as_err_unexpected())
                }
            }

        }

    }
}