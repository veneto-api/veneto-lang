use std::rc::Rc;
use crate::parse::lexer::Span; 

use crate::parse::ast; 
mod resolved; 

mod arena;

mod resolver; 
mod scope; 


struct Reference { 
    pub scope: usize, 
    pub symbol: usize, 
    pub span: Span, 
}

pub struct Symbol { 
    references: Vec<Reference>, 
    resolution: Option<Resolution> 
}
impl Symbol { 
    fn new_unresolved() -> Self { 
        Self { 
            references: Vec::new(), 
            resolution: None, 
        }
    }
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
