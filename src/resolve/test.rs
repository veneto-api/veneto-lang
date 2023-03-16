use crate::parse::{ast::{Document, Expectable}, lexer::{TokenStream, Span, Position}, tokens::Primitive};

use super::{resolver::Resolver, DocumentSource, ResolutionKind, resolved::TypeLiteral};

#[test]
fn simple_alias() { 
    let doc = Document::parse_expect(&mut TokenStream::new("
        type A = B
        type B = int
    ".chars())).unwrap(); 

    let mut resolver = Resolver::new();
    resolver.process_document(&doc, DocumentSource::Userland("asdf".to_string()));

    let mut iter = resolver.scopes.iter(); 

    let scope = iter.next().unwrap(); 
    let b_id = scope.symbols.lookup("B").unwrap();
    let b = scope.symbols.get(b_id); 

    // Bug!
    // assert_eq!(b.resolution.as_ref().unwrap().declared_at.unwrap().span, Span { lo: Position(33), hi: Position(34) })

    assert!(matches!(b.resolution.as_ref().unwrap().kind, ResolutionKind::TypeLiteral(TypeLiteral::Primitive(Primitive::Int))));

    let a = scope.symbols.lookup("A").unwrap(); 
    let a = scope.symbols.get(a); 

    assert!(matches!(a.resolution.as_ref().unwrap().kind, ResolutionKind::Alias(index) if index.symbol_id == b_id));
}