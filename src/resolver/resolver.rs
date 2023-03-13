use std::rc::Rc; 
use std::cell::RefCell; 

use super::{scope::Scope, ResolutionKind, Symbol, Resolution, Reference}; 
use crate::parse::ast::{self, Spanned, document::Node, TypeKind};

pub struct Resolver { 
    docs: Vec<Rc<RefCell<()>>>,
    scopes: Vec<Rc<RefCell<Scope>>>, 
}
impl Resolver { 
    fn new_scope(&mut self) -> Rc<RefCell<Scope>> { 
        let len = self.scopes.len(); 
        let scope = Rc::new(RefCell::new(Scope::new(len)));
        self.scopes.push(scope.clone());
        scope
    }
    fn new_scope_with_params(&mut self, parent: usize, params: Vec<Spanned<String>>) -> Rc<RefCell<Scope>> { 
        let len = self.scopes.len(); 
        let scope = Rc::new(RefCell::new(Scope::new_with_params(len, parent, params)));
        self.scopes.push(scope.clone());
        scope
    }

    fn process_document(&mut self, doc: &ast::Document) { 
        let scope = self.new_scope();
        let mut scope = scope.borrow_mut();
        let scope_id = scope.id; 

        for node in doc { 
            match node { 
                Node::Type(declared) => { 

                    let param_scope = declared.name.args.clone().map(|params| { 
                        self.new_scope_with_params(scope_id, params)
                    });


                    match &declared.node.kind { 
                        TypeKind::Identifier(referenced) => { 

                            // Resolve an alias 

                            let referenced_index = scope.get_symbol(referenced.base.node.clone()); 

                            let mut kind = ResolutionKind::Alias { 
                                scope: scope_id, 
                                entry: referenced_index,
                            };
                            if let Some(param_scope) = param_scope { 
                                kind = ResolutionKind::Scoped(param_scope.borrow().id, Box::new(kind));
                            }

                            let new_index = scope.symbols.insert(
                                declared.name.base.node.clone(), 
                                Symbol { 
                                    references: Vec::new(), 
                                    resolution: Some(Resolution { 
                                        declaration: declared.name.base.span, 
                                        kind,
                                    })
                                }
                            );

                            scope.symbols.get_mut(referenced_index).references.push(Reference { 
                                scope: scope_id, 
                                symbol: new_index, 
                                span: referenced.base.span, 
                            });

                        },

                        _ => todo!(), 
                    }

                }
                _ => todo!(),
            }
        }

    }
}