
use crate::parse::ast::Expectable;
use crate::parse::{ ParseResult, lexer::TokenStream } ;
use crate::parse::ast::document::Document;

pub fn parse_std() -> ParseResult<Document> { 
    let mut stream = TokenStream::new( include_str!("std.veneto").chars() ); 
    Document::parse_expect(&mut stream)

}

lazy_static! { 
    static ref STD : ParseResult<Document> = parse_std(); 
}

#[cfg(test)] 
mod test {
    use crate::parse::ast::{document::Node, rc::{RCComponent, MethodName, RCDeclaration, RCReference, Metaclass, RCType}, types::TypeKind, general::test::assert_gid};
 
    #[test]
    fn test_std() { 
        let doc = super::parse_std().unwrap();
        let mut nodes = doc.into_iter(); 

        //
        // Ref<T>
        //

        let Some(Node::ResourceClass(rc)) = nodes.next() else { panic!() }; 
        let RCDeclaration::Basic(gid) = rc.declaration else { panic!() }; 
        // assert_gid!(gid, "Ref" < "T" >); 

        let mut components = rc.components.into_iter(); 

        let Some(RCComponent::Embed(typ)) = components.next() else { panic!() }; 
        let TypeKind::Identifier(gid) = typ.kind else { panic!() }; 
        assert_gid!(gid, "T"); 

        let Some(RCComponent::Method(method)) = components.next() else { panic!() }; 
        assert_eq!(method.name, MethodName::Get); 
        assert!(method.input.is_none()); 
        let mut iter = method.outputs.into_iter(); 
        let output = iter.next().unwrap(); 
        assert_eq!(output.status.unwrap(), "405"); 
        assert!(output.typ.is_none()); 
        assert!(iter.next().is_none()); 

        let Some(RCComponent::Method(method)) = components.next() else { panic!() }; 
        assert_eq!(method.name, MethodName::Delete); 
        assert!(method.input.is_none()); 
        assert!(method.outputs.is_empty()); 

        assert!(components.next().is_none()); 

        //
        // List<T>
        // 

        let Some(Node::ResourceClass(rc)) = nodes.next() else { panic!() }; 
        let RCDeclaration::Basic(gid) = rc.declaration else { panic!() }; 
        // assert_gid!(gid, "List" < "T" > ); 

        let mut components = rc.components.into_iter(); 

        let Some(RCComponent::Embed(typ)) = components.next() else { panic!() }; 
        let TypeKind::Array(typ) = typ.kind else { panic!() }; 
        let TypeKind::Identifier(typ) = typ.kind else { panic!() }; 
        assert_gid!(typ, "T"); 

        let Some(RCComponent::Links(links)) = components.next() else { panic!() }; 
        let mut links = links.into_iter(); 
        let link = links.next().unwrap(); 
        assert_eq!(link.rel.node, "next"); 
        assert!(link.optional); 
        let RCReference::Special(metaclass) = link.typ else { panic!() }; 
        assert_eq!(metaclass.node, Metaclass::RCSelf); 
        assert!(links.next().is_none()); 

        let Some(RCComponent::Method(method)) = components.next() else { panic!() }; 
        assert_eq!(method.name, MethodName::Get); 
        assert!(method.input.is_none()); 
        assert!(method.outputs.is_empty()); 

        let Some(RCComponent::Method(method)) = components.next() else { panic!() }; 
        assert_eq!(method.name, MethodName::Post); 
        let Some(input) = method.input else { panic!() }; 
        assert!(!input.lax); 
        let RCType::Normal(typ) = input.typ else { panic!() }; 
        let TypeKind::Identifier(typ) = typ.kind else { panic!() }; 
        assert_gid!(typ, "T"); 

        let mut outputs = method.outputs.into_iter(); 
        let output = outputs.next().unwrap(); 
        assert!(matches!(output.status, Some(x) if x == "201"));
        let RCType::Normal(typ) = output.typ.unwrap() else { panic!() }; 
        let TypeKind::Identifier(typ) = typ.kind else { panic!() }; 
        assert_gid!(typ, "T"); 

        assert!(outputs.next().is_none()); 
        assert!(components.next().is_none()); 

        // Aight I give up lol this should be enough for now...

    }
}