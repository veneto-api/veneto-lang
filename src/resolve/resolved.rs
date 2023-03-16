use std::collections::HashMap;
use std::collections::hash_map::Entry;


use crate::parse::ast::Spanned;
use crate::parse::lexer::Span;
use crate::parse::tokens::Primitive;

use super::Location;
use super::ResolutionKind;
use super::ResolverError;
use super::SymbolIndex;

#[derive(Clone, Debug)]
pub enum Type { 
    Alias(SymbolIndex), 
    Literal(TypeLiteral), 
}

#[allow(clippy::from_over_into)] // this is a one-way mapping, not a surjection 🫤
impl Into<ResolutionKind> for Type { 
    fn into(self) -> ResolutionKind {
        match self { 
            Self::Alias(index) => ResolutionKind::Alias(index), 
            Self::Literal(lit) => ResolutionKind::TypeLiteral(lit), 
        }
    }
}


#[derive(Clone, Debug)]
pub enum TypeLiteral { 
    Struct(Struct),
    Primitive(Primitive), 
}

#[derive(Clone, Debug)]
pub struct StructField { 
    key_span: Span, 
    typ: Type, 
}

#[derive(Clone, Debug)]
pub struct Struct { 
    pub document_id: usize, 
    pub fields: HashMap<String, StructField>, 
}

impl Struct { 
    pub fn new(document_id: usize) -> Self { 
        Self { 
            document_id,
            fields: HashMap::new(), 
        }
    }

    pub fn try_add_reference(&mut self, errs: &mut Vec<ResolverError>, key: Spanned<String>, typ: Type) { 
        match self.fields.entry(key.node) { 
            Entry::Vacant(entry) => {
                entry.insert(StructField { 
                    key_span: key.span, 
                    typ, 
                });
            },

            Entry::Occupied(entry) => { 
                errs.push(ResolverError::Duplicate { 
                    original: Location { 
                        document: self.document_id, 
                        span: entry.get().key_span, 
                    }, 
                    redefined_at: Location { 
                        document: self.document_id, 
                        span: key.span, 
                    }, 
                });
            }
        }
    }
}