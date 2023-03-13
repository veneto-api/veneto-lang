use std::{rc::Rc}; 
use std::cell::RefCell; 

use super::resolved;
use super::{scope::Scope, ResolutionKind, Symbol, Resolution, Reference, DocumentSource, ResolverError, Location}; 
use crate::parse::ast::{self, Spanned, document::{Node, TypeDeclaration}, TypeKind};

pub struct Resolver { 
    scopes: Vec<Rc<RefCell<Scope>>>, 

    documents: Vec<DocumentSource>, 

    errors: Vec<ResolverError>, 
}
impl Resolver { 
    fn new_scope(&mut self, document: usize) -> Rc<RefCell<Scope>> { 
        let len = self.scopes.len(); 
        let scope = Rc::new(RefCell::new(Scope::new(len, document)));
        self.scopes.push(scope.clone());
        scope
    }
    fn new_scope_with_params(&mut self, document: usize, parent: usize, params: Vec<Spanned<String>>) -> Rc<RefCell<Scope>> { 
        let len = self.scopes.len(); 
        let scope = Rc::new(RefCell::new(Scope::new_with_params(len, document, parent, params)));
        self.scopes.push(scope.clone());
        scope
    }

    fn process_document(&mut self, doc: &ast::Document, src: DocumentSource) { 

        let document = { 
            let len = self.documents.len(); 
            self.documents.push(src); 
            len 
        };

        let scope = self.new_scope(document);
        let scope_id = scope.borrow().id; 

        for node in doc { 
            match node { 
                Node::Type(declared) => { 

                    let param_scope = declared.name.args.clone().map(|params| { 
                        self.new_scope_with_params(document, scope_id, params)
                    });

                    match self.handle_type_declaration(document, scope.clone(), declared, param_scope) { 
                        Ok(_) => {}, 
                        Err(err) => { 
                            self.errors.push(err); 
                            continue; 
                        }
                    }

                }
                _ => todo!(),
            }
        }


    }

    /// Processes a type declaration, returning the new symbol ID if successful 
    fn handle_type_declaration(&mut self, 
        document: usize, 
        scope: Rc<RefCell<Scope>>, 
        declared: &TypeDeclaration, 
        param_scope: Option<Rc<RefCell<Scope>>>, 
    ) -> Result<Reference, ResolverError> { 
        
        match &declared.node.kind { 
            TypeKind::Identifier(referenced) => { 
                let mut scope = scope.borrow_mut();
                let scope_id = scope.id; 

                // Resolve an alias 

                let referenced_index = scope.get_symbol(referenced.base.node.clone()); 

                let mut kind = ResolutionKind::Alias { 
                    scope: scope_id, 
                    entry: referenced_index,
                };
                if let Some(param_scope) = param_scope { 
                    kind = ResolutionKind::Scoped(param_scope.borrow().id, Box::new(kind));
                }

                let location = Location { 
                    document, 
                    span: declared.name.base.span, 
                }; 

                let new_index = scope.create_or_resolve(
                    declared.name.base.node.clone(), 
                    Resolution { location, kind }
                )?; 

                let reference = Reference { 
                    location,
                    scope: scope_id, 
                    symbol: new_index, 
                }; 
                scope.symbols.get_mut(referenced_index).references.push(reference);
                Ok(reference)
            },

            TypeKind::Struct(body) => { 
                let mut fields = resolved::ReferenceMap::new(); 

                //   👇 ast::types::StructField
                for field in body { 
                    let resolved = Self::handle_type_declaration(self, document, scope.clone(), declared, param_scope.clone());

                    /*
                        TODO:
                         - We might have more than one error here.... hrrrngh... 
                         - Resolved types here would have to cascade upwards!  This is a potential logic error and we might have to fuck around with it
                            we need rules for this - when do types roll up?  when they have params?  how do real languages do this? 
                     */
                }

                todo!()
            }

            _ => todo!(), 
        }
    }
}