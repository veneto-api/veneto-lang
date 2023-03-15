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

/// A location in source code - a `Span`, together with the index of the document it was obtained from 
#[derive(Clone, Copy)]
pub struct Location { 
    pub document: usize, 
    pub span: Span, 
}

/// A location in source code, as well as the scope and symbol IDs of the symbol it refers to. 
#[derive(Copy, Clone)]
pub struct Reference { 
    pub location: Location, 
    pub index: SymbolIndex, 
}

/// A complete index of a symbol; its scope and symbol IDs 
#[derive(Clone, Copy)]
pub struct SymbolIndex { 
    pub scope_id: usize, 
    pub symbol_id: usize,
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

    fn resolve(&mut self, resolution: Resolution) -> Result<(), ResolverError> { 
        if let Some(original) = &self.resolution { 
            Err(ResolverError::Duplicate { original: original.location, redefined_at: resolution.location })
        } else { 
            self.resolution = Some(resolution);
            Ok(())
        }
    }
}

/// This information is added to a `Symbol` once it's resolved - it describes what it is and where it is defined. 
#[derive(Clone)]
pub struct Resolution { 
    location: Location, 
    kind: ResolutionKind, 
}

#[derive(Clone)]
enum ResolutionKind { 
    Param(usize), 

    Alias(Reference),

    ResourceClass(Rc<ast::ResourceClass>), 
    Type(resolved::Type),

    Scoped(usize, Box<ResolutionKind>), 
}

#[derive(Clone, Copy)]
pub enum ResolverError { 
    Duplicate { 
        original: Location, 
        redefined_at: Location, 
    }
}

pub type ResolverResult<T> = Result<T, Vec<ResolverError>>;
