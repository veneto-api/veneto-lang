use std::collections::HashMap;
use std::collections::hash_map;

use super::Reference;
use super::ResolverError; 

pub struct Arena<T> { 
    map: HashMap<String, usize>, 
    vec: Vec<ArenaVal<T>>, 
}
pub struct ArenaVal<T> { 
    id: usize, 
    val: T, 
}

impl<T> Arena<T> { 
    pub fn new() -> Self { 
        Self { 
            map: HashMap::new(), 
            vec: Vec::new(), 
        }
    }

    pub fn insert(&mut self, key: String, value: T) -> usize { 
        let len = self.vec.len(); 
        self.vec.push(ArenaVal { id: len, val: value }); 
        self.map.insert(key, len); 
        len 
    }
 
    pub fn entry(&mut self, key: String) -> Entry<T> { 
        match self.map.entry(key) { 
            hash_map::Entry::Occupied(inner) => { 
                Entry::Occupied(OccupiedEntry { vec: &mut self.vec, inner })
            },
            hash_map::Entry::Vacant(inner) => { 
                Entry::Vacant(VacantEntry { vec: &mut self.vec, inner })
            }
        }
    }

    /// Retrieves an entry by its ID, panicking if no such entry exists
    pub fn get(&self, id: usize) -> &T { 
        &self.vec[id].val
    }

    /// Retrieves an entry by its ID, panicking if no such entry exists
    pub fn get_mut(&mut self, id: usize) -> &mut T { 
        &mut self.vec.get_mut(id).unwrap().val
    }
}


pub enum Entry<'a, T> { 
    Occupied(OccupiedEntry<'a, T>), 
    Vacant(VacantEntry<'a, T>), 
}

pub struct OccupiedEntry<'a, T> { 
    vec: &'a mut Vec<ArenaVal<T>>,
    inner: hash_map::OccupiedEntry<'a, String, usize>
}
pub struct VacantEntry<'a, T> { 
    vec: &'a mut Vec<ArenaVal<T>>,
    inner: hash_map::VacantEntry<'a, String, usize>,
}

impl<'a, T> OccupiedEntry<'a, T> { 
    pub fn get_id(&self) -> usize { 
        *self.inner.get()
    }

    pub fn get_mut_val(self) -> &'a mut T { 
        &mut self.vec.get_mut(*self.inner.get()).unwrap().val
    }
}

impl<'a, T> VacantEntry<'a, T> { 
    /// Inserts the provided value into the collection, returning the ID of the inserted value 
    pub fn insert(self, val: T) -> usize { 
        let id = self.vec.len(); 
        self.vec.push(ArenaVal { id, val }); 
        self.inner.insert(id); 
        id
    }
}