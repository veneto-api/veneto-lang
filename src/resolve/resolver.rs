use super::{resolved, SymbolIndex};
use super::{scope::Scope, ResolutionKind, Symbol, Resolution, Reference, DocumentSource, ResolverError, Location}; 
use crate::parse::ast::{self, Spanned, document::Node, TypeKind};

macro_rules! try_or_continue {
    (vec $res:expr => $errs:expr) => {
        match $res { 
            Ok(x) => x, 
            Err(errs) => { 
                $errs.extend(errs); 
                continue 
            }
        }
    };

    ($res:expr => $errs:expr) => { 
        match $res { 
            Ok(x) => x, 
            Err(err) => { 
                $errs.push(err); 
                continue 
            }
        }
    }
}


#[derive(Clone)]
struct RefSource { 
    pub scope_id: usize, 
    pub location: Location, 
    pub key: String, 
}
impl RefSource { 
    pub fn new(document_id: usize, scope_id: usize, text: Spanned<String>) -> Self { 
        Self { 
            scope_id, 
            location: Location { 
                document: document_id, 
                span: text.span, 
            }, 
            key: text.node, 
        }
    }
}


pub struct Resolver { 
    pub(super) scopes: Vec<Scope>, 

    documents: Vec<DocumentSource>, 

    errors: Vec<ResolverError>, 
}
impl Resolver { 
    fn new_scope(&mut self, document: usize) -> usize { 
        let len = self.scopes.len(); 
        self.scopes.push(Scope::new(len, document));
        len
    }
    fn new_scope_with_params(&mut self, document: usize, parent: usize, params: Vec<Spanned<String>>) -> usize { 
        let len = self.scopes.len(); 
        self.scopes.push(Scope::new_generic(len, document, parent, params));
        len
    }


    fn get_mut(&mut self, index: SymbolIndex) -> &mut Symbol { 
        self.scopes[index.scope_id].symbols.get_mut(index.symbol_id)
    }


    /// Recursively looks up a symbol by key 
    /// hrngh 
    fn try_lookup(&self, scope_id: usize, key: &str) -> Option<SymbolIndex> { 
        let mut scope_id = scope_id; 
        loop { 
            let scope = &self.scopes[scope_id];
            if let Some(symbol_id) = scope.symbols.lookup(key) { 
                return Some(SymbolIndex { scope_id, symbol_id })
            }
            else if let Some(parent) = scope.parent { 
                scope_id = parent; 
                continue 
            }
            else { 
                return None 
            }
        }
    }

    /// Creates the symbol in the appropriate scope, starting with the provided `scope_id` 
    /// 
    /// This handles the `definitions_allowed()` condition, moving up in scope until the appropriate scope is found
    fn create_symbol(&mut self, scope_id: usize, key: String, value: Symbol) -> SymbolIndex { 
        let mut scope_id = scope_id; 
        loop { 
            let scope = &mut self.scopes[scope_id];

            if scope.definitions_allowed() { 
                let symbol_id = scope.symbols.insert(key, value); 
                return SymbolIndex{ scope_id, symbol_id }
            }
            else { 
                scope_id = scope.parent.expect("Definitions forbidden in this scope, but it doesn't have a parent");
                continue 
            }
        }
    }

    /// Creates the symbol in the appropriate scope, **without** associating it to a key
    fn push_symbol(&mut self, scope_id: usize, value: Symbol) -> SymbolIndex { 
        //TODO: Consolidate this scope_id search with the above function 
        let mut scope_id = scope_id; 
        loop { 
            let scope = &mut self.scopes[scope_id];

            if scope.definitions_allowed() { 
                let symbol_id = scope.symbols.push(value); 
                return SymbolIndex{ scope_id, symbol_id }
            }
            else { 
                scope_id = scope.parent.expect("Definitions forbidden in this scope, but it doesn't have a parent");
                continue 
            }
        }
    }


    /// Obtains an `SymbolIndex` for the provided symbol `key` starting in `scope_id`.
    /// 
    /// This searches up the stack of scopes and returns the index to a matching symbol if it finds it, 
    /// or creates it in the appropriate scope if no matching symbol exists yet.
    /// 
    /// It also adds a `Location` to the list of references for the symbol 
    fn get_symbol_index(&mut self, source: RefSource) -> SymbolIndex { 
        self.try_lookup(source.scope_id, &source.key).unwrap_or_else(|| {
            self.create_symbol(source.scope_id, source.key, Symbol::new_unresolved())
        })
    }

    /// Resolves the symbol within the provided scope, 
    /// looking up to parent scopes and creating it where necessary 
    fn resolve(&mut self, scope_id: usize, key: String, resolution: Resolution) -> Result<SymbolIndex, ResolverError> { 
        if let Some(index) = self.try_lookup(scope_id, &key) { 
            //TODO: Validate generics or whatever here 
            //TAG: VALIDATE_GENERIC_ARGS
            self.scopes[index.scope_id].symbols.get_mut(index.symbol_id).resolve(resolution)?;
            Ok(index)
        } else { 
            Ok(self.create_symbol(scope_id, key, Symbol::new_resolved(resolution)))
            //TODO: Iterate through existing references to validate
            //TAG: VALIDATE_GENERIC_ARGS
        }
    }


    fn process_document(&mut self, doc: &ast::Document, src: DocumentSource) { 

        let document = { 
            let len = self.documents.len(); 
            self.documents.push(src); 
            len 
        };

        let document_scope_id = self.new_scope(document);

        for node in doc { 
            match node { 
                Node::Type(declared) => { 

                    // Determine scope 
                    let param_scope = declared.name.args.clone().map(|params| { 
                        self.new_scope_with_params(document, document_scope_id, params)
                    });
                    let scope_id = param_scope.unwrap_or(document_scope_id);

                    // Obtain reference to the right-hand side value 
                    let reference = self.handle_type_expression(
                        document, 
                        scope_id, 
                        &declared.node.kind
                    );

                    // Create the `ResolutionKind` from our reference 
                    let mut kind = ResolutionKind::Alias(reference); 
                    if let Some(scope) = param_scope { 
                        kind = ResolutionKind::Scoped(scope, Box::new(kind));
                    }

                    // Resolve the symbol 
                    let res = self.resolve(scope_id, declared.name.base.node.clone(), Resolution { 
                        declared_at: Some(Location { 
                            document, 
                            span: declared.name.base.span, 
                        }),
                        kind,
                    });
                    try_or_continue!(res => self.errors);

                }
                _ => todo!(),
            }
        }


    }

    /// Processes a type expression,
    /// returning its symbol ID if successful 
    fn handle_type_expression(&mut self, 
        document: usize, 
        scope_id: usize, 
        kind: &TypeKind, 
    ) -> SymbolIndex { 
        match kind { 
            TypeKind::Identifier(ident) => { 

                let source = RefSource::new(document, scope_id, ident.base.clone()); 
                let location = source.location;
                let index = self.get_symbol_index(source); 

                self.get_mut(index).references.push(Reference { location, index });

                index
            }, 
 

            TypeKind::Struct(body) => { 
                let mut fields = resolved::ReferenceMap::new(); 

                //   👇 ast::types::StructField
                for field in body { 
                    let index = self.handle_type_expression(
                        document, 
                        scope_id,  
                        &field.typ.kind,
                    );

                    fields.insert(field.name.node.clone(), Reference { 
                        index,
                        location: Location { 
                            document, 
                            span: field.name.span, 
                        }
                    }); 
                }

                self.push_symbol(
                    scope_id,
                    Symbol::new_resolved(Resolution { 
                        declared_at: None, 
                        kind: ResolutionKind::Type(resolved::Type::Struct(fields))
                    })
                )
            }

            _ => todo!()
        }
    }
}