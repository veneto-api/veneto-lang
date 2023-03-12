use std::{collections::{hash_map::Entry, HashMap}, rc::Rc, cell::RefCell};

use crate::parse::{ast::{self, Spanned, document::Node, TypeKind}, lexer::Span}; 
 
mod resolved; 

/// Utility trait to push an item to a collection and return its inserted index 
trait PushIndex<T> { 
    /// Pushes the provided item, and returns the index at which it was inserted
    fn push_index(&mut self, value: T) -> usize; 
}
impl<T> PushIndex<T> for Vec<T> { 
    fn push_index(&mut self, value: T) -> usize {
        let len = self.len(); 
        self.push(value);
        len
    }
}

struct Reference { 
    pub scope: usize, 
    pub symbol: usize, 
    pub document: usize, 
    pub span: Span, 
}

struct Arena<T> { 
    pub map: HashMap<String, usize>, 
    pub vec: Vec<T>, 
}
impl<T> Arena<T> { 
    pub fn new() -> Self { 
        Self { 
            map: HashMap::new(), 
            vec: Vec::new(), 
        }
    }

    pub fn insert(&mut self, key: String, value: T) -> usize { 
        let len = self.vec.len(); 
        self.vec.push(value); 
        self.map.insert(key, len); 
        len 
    }

    pub fn get(&self, key: &str) -> Option<&T>{ 
        let i = self.map.get(key); 
        i.and_then(|i| self.vec.get(*i))
    }
}



struct Symbol { 
    references: Vec<Span>, 
    resolution: Option<Resolution> 
}

/// This information is added to a `Symbol` once it's resolved - it describes what it is and where it is defined. 
struct Resolution { 
    declaration: Span, 
    kind: ResolutionKind, 
}

enum ResolutionKind { 
    Param(usize), 

    Alias { 
        scope: usize, 
        entry: usize, 
    },

    ResourceClass(Rc<ast::ResourceClass>), 
    Type(resolved::Type),

    Scoped(usize, Box<ResolutionKind>), 
}

pub struct Scope { 
    id: usize, 
    parent: Option<usize>, 
 
    params: Option<Vec<Spanned<String>>>, 

    symbols: Arena<Symbol>, 
}
impl Scope { 
    fn new(_id: usize) -> Self { 
        todo!()
    }

    fn new_with_params(id: usize, parent: usize, params: Vec<Spanned<String>>) -> Self { 

        let mut s = Self { 
            id, 
            parent: Some(parent), 

            params: Some(params.clone()), 

            symbols: Arena::new(), 
        }; 

        for (index, param) in params.iter().enumerate() { 
            s.symbols.insert(param.node.clone(), Symbol { 
                references: Vec::new(), 
                resolution: Some(Resolution { 
                    declaration: param.span, 
                    kind: ResolutionKind::Param(index),
                })
            });
        }

        s
    }

    /// Records a symbol reference in the current scope, and returns the index of the referenced symbol. 
    /// 
    /// If the symbol is already tracked, it adds `referenced_from` to the existing list of references.
    /// Otherwise, it creates a new unresolved symbol, initialized with the provided reference.  
    fn add_symbol_reference(&mut self, name: String, referenced_from: Span) -> usize { 
        match self.symbols.map.entry(name) { 
            Entry::Occupied(entry) => { 
                let i = *entry.get(); 
                self.symbols.vec.get_mut(i).unwrap().references.push(referenced_from); 
                i
            },
            Entry::Vacant(entry) => { 
                let i = self.symbols.vec.push_index(Symbol { 
                    references: vec![ referenced_from ], 
                    resolution: None
                }); 
                entry.insert(i); 
                i
            }
        }
    }


 
    fn process_document(&mut self, doc: &ast::Document) { 

    }
}

pub struct Resolver { 
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

                            let referenced_index = scope.add_symbol_reference(referenced.base.node.clone(), referenced.base.span); 

                            let mut kind = ResolutionKind::Alias { 
                                scope: scope_id, 
                                entry: referenced_index,
                            };
                            if let Some(param_scope) = param_scope { 
                                kind = ResolutionKind::Scoped(param_scope.borrow().id, Box::new(kind));
                            }

                            let new_index = scope.symbols.vec.push_index(Symbol { 
                                references: Vec::new(), 
                                resolution: Some(Resolution { 
                                    declaration: declared.name.base.span, 
                                    kind,
                                })
                            });

                            scope.symbols.map.insert(declared.name.base.node.clone(), new_index);

                        },

                        _ => todo!(), 
                    }

                }
                _ => todo!(),
            }
        }

    }
}