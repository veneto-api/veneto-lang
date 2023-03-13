use super::{arena::*, Reference}; 
use super::{ Symbol, Resolution, ResolutionKind };

use crate::parse::ast::Spanned; 

pub struct Scope { 
    pub id: usize, 

    parent: Option<usize>, 
 
    params: Option<Vec<Spanned<String>>>, 

    pub symbols: Arena<Symbol>, 
}
impl Scope { 
    pub fn new(_id: usize) -> Self { 
        todo!()
    }

    pub fn new_with_params(id: usize, parent: usize, params: Vec<Spanned<String>>) -> Self { 

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


    /// Returns the ID of a symbol table entry, creating a new unresolved symbol if none exists
    pub fn get_symbol(&mut self, name: String) -> usize { 
        match self.symbols.entry(name) { 
            Entry::Occupied(entry) => entry.get_id(), 
            Entry::Vacant(entry) => entry.insert(Symbol::new_unresolved()),
        }
    }

}