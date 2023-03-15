
use crate::{parse::{lexer::{TokenStream}, ParseResult, tokens::{Keyword, Punctuation, TokenKind, Terminal, Token}}};

use super::{types::Type, rc::ResourceClass, interfaces::InterfaceExpression, use_tree::UseTree, Expectable, general::{GenericIdentifier, GenericDeclaration}, Spanned, Finishable, AsFinishable};


pub struct EntryPoint { 
    pub uri: String, 
    pub rc: GenericIdentifier, 
}

pub enum Node { 
    Use(UseTree),
    Type(TypeDeclaration), 
    Interface(Declaration<InterfaceExpression>), 
    ResourceClass(ResourceClass), 
    EntryPoint(EntryPoint), 
}

pub type Document = Vec<Node>; 

/// This is a simple generic AST node that contains a `Spanned<String>` as its identifier, 
/// along with a wrapped inner node.
/// 
/// This is used for interface declarations.  
/// It used to be used for Types as well, but I flubbed that one because Types need to support generic params 
pub struct Declaration<T> { 
    pub name: Spanned<String>, 
    pub node: T, 
}
impl Finishable for Declaration<InterfaceExpression> {
    const INITIAL_TOKEN: Terminal = Terminal::Keyword(Keyword::Interface);

    fn parse_finish(stream: &mut TokenStream, _: Token) -> ParseResult<Self> {
        let name = stream.next()?.try_as_identifier()?; 
        stream.next()?.expect_punctuation(Punctuation::Assign)?; 
        Ok(Self { name, node: InterfaceExpression::parse_expect(stream)? })
    }
}

pub struct TypeDeclaration { 
    pub name: GenericDeclaration, 
    pub node: Type, 
}
impl Finishable for TypeDeclaration { 
    const INITIAL_TOKEN: Terminal = Terminal::Keyword(Keyword::Type);

    fn parse_finish(stream: &mut TokenStream, _initial: Token) -> ParseResult<Self> {
        let name = GenericIdentifier::parse_expect(stream)?.try_into()?; 
        stream.next()?.expect_punctuation(Punctuation::Assign)?; 
        Ok(Self{ name, node: Type::parse_expect(stream)? })
    }
}


impl Expectable for Document { 

    fn parse_expect(stream: &mut TokenStream) -> ParseResult<Self> {
        let mut doc = Self::new(); 

        loop { 
            let initial = stream.next()?; 
            let kw = initial.as_keyword();

            if let Some(rc) = ResourceClass::as_finish(stream, &initial)? { 
                doc.push(Node::ResourceClass(rc));
            }
            else if let Some(node) = TypeDeclaration::as_finish(stream, &initial)? { 
                doc.push(Node::Type(node))
            }
            else if let Some(node) = Declaration::<InterfaceExpression>::as_finish(stream, &initial)? { 
                doc.push(Node::Interface(node))
            }
            else if kw == Some(Keyword::Use) { 
                doc.push(Node::Use(UseTree::parse_expect(stream)?))
            }
            else if kw == Some(Keyword::Entry) { 
                let uri = stream.next()?.try_as_string_literal()?; 
                stream.next()?.expect_punctuation(Punctuation::Arrow)?; 
                let rc = GenericIdentifier::parse_expect(stream)?; 
                doc.push(Node::EntryPoint(EntryPoint { uri, rc }))
            }
            else if initial.kind == TokenKind::EOF { 
                return Ok(doc)
            } else { 
                return Err(initial.as_err_unexpected())
            }
        }

    }
}