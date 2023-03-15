use crate::parse::lexer::Span; 

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
    resolution: Option<Resolution>,

    /// This has two purposes
    /// 1) The IDE functionality, "find references"
    /// 2) Validating generic params and stuff after the fact
    /// 
    /// soo.... we need to figure out how #2 works 
    references: Vec<Reference>, 
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
            let original = original.declared_at.expect("Overwriting a literal declaration"); 
            let redefined_at = resolution.declared_at.expect("Overwriting a declaration with a literal");

            Err(ResolverError::Duplicate { original, redefined_at })
        } else { 
            self.resolution = Some(resolution);
            Ok(())
        }
    }
}

/// This information is added to a `Symbol` once it's resolved - it describes what it is and where it is defined. 
#[derive(Clone)]
pub struct Resolution { 
    kind: ResolutionKind, 

    /// The `Location` where the symbol's _key_ is declared.
    /// 
    /// This may be `None` where the symbol is a literal.  
    /// This is kinda messy, we may have to revisit 
    declared_at: Option<Location>, 
}

#[derive(Clone)]
enum ResolutionKind { 
    Param(usize), 

    Alias(SymbolIndex),

    Scoped(usize, Box<ResolutionKind>), 

    Type(resolved::Type),
}

#[derive(Clone, Copy)]
pub enum ResolverError { 
    Duplicate { 
        original: Location, 
        redefined_at: Location, 
    }
}

pub type ResolverResult<T> = Result<T, Vec<ResolverError>>;
