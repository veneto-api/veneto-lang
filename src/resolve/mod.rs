use std::rc::Rc;
use crate::parse::lexer::Span; 

use crate::parse::ast; 
mod resolved; 

mod arena;

mod resolver; 
mod scope; 

enum DocumentSource { 
    Standard, 
    Userland(String), 
}

#[derive(Clone, Copy)]
pub struct Location { 
    pub document: usize, 
    pub span: Span, 
}

struct Reference { 
    pub location: Location, 
    pub scope: usize, 
    pub symbol: usize, 
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

    fn new_resolved(resolution: Resolution) -> Self { 
        Self { 
            references: Vec::new(), 
            resolution: Some(resolution), 
        }
    }

    fn resolve(&mut self, resolution: Resolution) { 
        self.resolution = Some(resolution)
    }
}

/// This information is added to a `Symbol` once it's resolved - it describes what it is and where it is defined. 
pub struct Resolution { 
    location: Location, 
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

pub enum ResolverError { 
    Duplicate { 
        original: Location, 
        redefined_at: Location, 
    }
}