use std::collections::HashMap;
use std::collections::hash_map::Entry;


use super::Reference;
use super::ResolverError;

#[derive(Clone)]
pub enum Type { 
    Struct(ReferenceMap)
}

pub type ReferenceMap = HashMap<String, Reference>; 

/*
    We want to differentiate between types of "reference" here
    In this Reference, the `Location` refers to the definition of the **key**, 
    while the `SymbolIndex` refers to the type of the value

    This is not the case in other uses of it, 
    like type alia, 

    well actually maybe it is
    jesus christ this is confusing
 */

/// Tries to add a `key`/`val` pair to `map`, but adds an error to `errs` if the key already exists
pub(crate) fn try_add_reference(map: &mut ReferenceMap, errs: &mut Vec<ResolverError>, key: String, val: Reference) { 
    match map.entry(key) { 
        Entry::Occupied(entry) => { 
            errs.push(ResolverError::Duplicate { 
                original: entry.get().location, 
                redefined_at: val.location,  
            })
        }, 
        Entry::Vacant(entry) => { 
            entry.insert(val);
        }
    }
}
