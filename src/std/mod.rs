#[cfg(test)]
mod test { 
    use crate::STD;
    use crate::parse::ast::general::GenericIdentifier;
    use crate::parse::ast::rc::{ResourceClass, RCDeclaration, RCIdentifier, Method, MethodName, MethodOutput};
    use crate::parse::ast::types::{Type, TypeKind}; 

    #[test]
    fn test_std() { 
        let doc = STD.as_ref().unwrap(); 
        let mut rcs = doc.resource_classes.iter(); 

        // Ref<T>
        assert_eq!(rcs.next().unwrap(), &ResourceClass { 
            declaration: RCDeclaration::Basic(RCIdentifier { 
                base: "Ref".to_string(), 
                generics: vec![ 
                    RCIdentifier { 
                        base: "T".to_string(), 
                        generics: vec![], 
                    }
                 ]
            }),
            data: Some(Type { 
                kind: TypeKind::Identifier(GenericIdentifier::simple("T")),
                optional: false, 
                in_plus: None, 
                out_plus: None, 
            }),
            interface: None, 
            links: None, 

            methods: vec![ 
                Method { 
                    name: MethodName::Get, 
                    input: None, 
                    outputs: vec! [ 
                        MethodOutput { 
                            status: Some(405.to_string()),
                            typ: None, 
                        }
                    ]
                }
            ]
        })


    }
}