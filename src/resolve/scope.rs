use super::{arena::*, Location, ResolverError}; 
use super::{ Symbol, Resolution, ResolutionKind };

use crate::parse::ast::Spanned; 

pub struct Scope { 
    pub id: usize, 
    pub document: usize, 

    parent: Option<usize>, 
 
    params: Option<Vec<Spanned<String>>>, 

    pub symbols: Arena<Symbol>, 
}
impl Scope { 
    pub fn new(id: usize, document: usize) -> Self { 
        Self { 
            id, 
            document, 

            parent: None, 
            params: None, 

            symbols: Arena::new(), 
        }
    }

    pub fn new_with_params(id: usize, document: usize, parent: usize, params: Vec<Spanned<String>>) -> Self { 

        let mut s = Self { 
            id, 
            document, 

            parent: Some(parent), 

            params: Some(params.clone()), 

            symbols: Arena::new(), 
        }; 

        for (index, param) in params.iter().enumerate() { 
            s.symbols.insert(param.node.clone(), Symbol { 
                references: Vec::new(), 
                resolution: Some(Resolution { 
                    location: Location { document, span: param.span },
                    kind: ResolutionKind::Param(index),
                })
            });
        }

        s
    }


    /// Returns the ID of a symbol table entry, creating a new unresolved symbol if none exists
    pub fn get_symbol(&mut self, name: String) -> usize { 
        match self.symbols.entry(name) { 
            Entry::Occupied(entry) => entry.get_id(), 
            Entry::Vacant(entry) => entry.insert(Symbol::new_unresolved()),
        }
    }


    /// Attempts to resolve a symbol at `key` with `resolution`, creating the symbol if necessary, returning the index of the symbol. 
    /// 
    /// If the symbol has already been resolved, a `ResolverError` is returned.
    pub fn create_or_resolve(&mut self, key: String, resolution: Resolution) -> Result<usize, ResolverError> { 
        match self.symbols.entry(key) { 
            Entry::Vacant(entry) => Ok(entry.insert(Symbol::new_resolved(resolution))), 
            Entry::Occupied(entry) => { 
                let id = entry.get_id(); 
                let val = entry.get_mut_val(); 

                if let Some(original) = &val.resolution { 
                    Err(ResolverError::Duplicate { 
                        original: original.location,
                        redefined_at: resolution.location, 
                    })
                } else { 
                    val.resolve(resolution); 
                    Ok(id)
                }
            }
        }
    }

}