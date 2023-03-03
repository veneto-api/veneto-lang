use super::{lexer::{TokenStream, Span}, ParseResult, tokens::{Terminal, Punctuation, Token}};

/// This is parses common miscellaneous expressions.
/// At the time, that's only generic identifiers, so maybe this module should be renamed.
/// 
/// This is documented in `grammar/general.ebnf`
pub mod general;

/// This contains everything related to imports and the `use` directive;
/// heavily inspired by `rustc_ast::ast::UseTree` 
/// 
/// This is documented in `grammar/general.ebnf`
pub mod use_tree;

/// This parses type expressions, as well as top-level type alias directives.
/// 
/// This is documented in `general/data.ebnf`
pub mod types;

/// This parses interface expressions.
pub mod interfaces;

/// This parses the body of a Resource Class.
pub mod rc; 


pub mod document; 


//
// Parsing interface
//

pub trait Expectable where Self: std::marker::Sized { 
    /// Tries to extract the implementing AST node from the `stream`,
    /// returning an Unexpected error if it cannot.  
    fn parse_expect(stream: &mut TokenStream) -> ParseResult<Self>;
}

/// A `Peekable` is an AST node with an unambiguous initial token.
/// That lets us implement `parse_peek`, which can gracefully backtrack
/// if the stream cannot derive the implementing AST node from its current position.
pub trait Peekable where Self: std::marker::Sized { 
    /// Tries to extract the implementing AST node from the `stream`,
    /// first by having a peek at the first token.
    /// If the first token doesn't match, return `None` 
    fn parse_peek(stream: &mut TokenStream) -> ParseResult<Option<Self>>;
}


/// A `Finishable` is an AST node with an unambiguous initial token.  
/// 
/// A `Finishable` implementation declares an initial token 
/// - the token that starts off a clause of the implementing node - 
/// and a method to _finish_ parsing that clause **after** the initial token.
/// 
/// This provides `Peekable` and `Expectable` impls for free.
pub trait Finishable where Self: std::marker::Sized{ 

    /// The "initial token" is the token kind that marks the start of this clause. 
    const INITIAL_TOKEN: Terminal; 

    /// Finish parsing this clause **after** the initial token. 
    fn parse_finish(stream: &mut TokenStream, initial: Token) -> ParseResult<Self>; 
}
impl<T> Peekable for T where T: Finishable { 
    fn parse_peek(stream: &mut TokenStream) -> ParseResult<Option<Self>> {
        let peek = stream.peek()?;
        if peek.as_terminal() == Some(Self::INITIAL_TOKEN) { 
            let peek = stream.next()?;
            Ok(Some(T::parse_finish(stream, peek)?))
        } else { 
            Ok(None) 
        }
    }
}
impl<T> Expectable for T where T: Finishable { 
    fn parse_expect(stream: &mut TokenStream) -> ParseResult<Self> {
        let next = stream.next()?;
        next.expect_terminal(Self::INITIAL_TOKEN)?; 
        T::parse_finish(stream, next)
    }
}



fn parse_list_into<T: Peekable>(vec: &mut Vec<T>, stream: &mut TokenStream) -> ParseResult<()> { 

    if let Some(node) = T::parse_peek(stream)? { 
        vec.push(node); 

        if stream.peek_for_punctuation(Punctuation::Comma)? { 
            parse_list_into(vec, stream)?; 
        }
    } 
    
    Ok(())
}

#[derive(Debug)]
pub struct Spanned<T> { 
    pub node: T, 
    pub span: Span, 
}

impl<T> PartialEq for Spanned<T> where T: PartialEq { 
    fn eq(&self, other: &Self) -> bool {
        self.node == other.node && self.span == other.span
    }
}
impl<T> Eq for Spanned<T> where T: Eq {}