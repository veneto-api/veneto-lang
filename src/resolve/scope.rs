use super::{arena::*, Location}; 
use super::{ Symbol, Resolution, ResolutionKind };

use crate::parse::ast::Spanned; 

pub struct Scope { 
    pub id: usize, 
    pub document: usize, 

    pub(super) parent: Option<usize>, 
 
    /// Generic params, if applicable
    params: Option<Vec<Spanned<String>>>, 

    pub(super) symbols: Arena<Symbol>, 
}
impl Scope { 
    pub(super) fn new(id: usize, document: usize) -> Self { 
        Self { 
            id, 
            document, 

            parent: None, 
            params: None, 

            symbols: Arena::new(), 
        }
    }

    pub(super) fn new_generic(id: usize, document: usize, parent: usize, params: Vec<Spanned<String>>) -> Self { 

        let mut s = Self { 
            id, 
            document, 

            parent: Some(parent), 

            params: Some(params.clone()), 

            symbols: Arena::new(), 
        }; 

        for (index, param) in params.iter().enumerate() { 
            s.symbols.insert(
                param.node.clone(),
                Symbol::new_resolved(Resolution { 
                    declared_at: Some(Location { document, span: param.span }),
                    kind: ResolutionKind::Param(index),
                })
            );
        }

        s
    }

    /// If `false`, new type new symbol definitions roll up to the parent scope
    /// 
    /// This is pretty poorly defined at the moment, 
    /// but the basic idea is that unresolved symbols within a generic context
    /// should be defined in the parent scope.  
    /// So this is `false` for only those scopes
    /// 
    /// We'll want this to be a lot more rigorous later on 
    pub(super) fn definitions_allowed(&self) -> bool { 
        self.params.is_none()
    }

}