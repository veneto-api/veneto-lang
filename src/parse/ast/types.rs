use super::general::GenericIdentifier;

pub enum TypeKind { 
    /// A reference to an existing type 
    Identifier(GenericIdentifier),

    /// A homogenous array of a single type 
    Array(Box<Type>), 
    // 👆 the compiler complains if it's "without indirection", so it must be boxed. TIL!

    /// A tuple represented by the included types, in order
    Tuple(Vec<Type>),

}

/// This represents a type expression - 
/// that can be a reference to an existing type, or a new one. 
pub struct Type { 
    kind: TypeKind, 
    
    /// If `true`, this type has been marked optional with a `?`.  
    /// 
    /// Note that in the future we will likely have nested optionals, tracked in the below Notion page.
    /// This will invalidate the current system of assuming that there's only one. 
    /// 
    /// https://veneto.notion.site/Unions-Monadic-Optionals-e39088b0b78e48ebafb941faef447dcb
    optional: bool, 

    /// If `true`, this type has been marked with the lax operator `%`.  
    lax: bool, 
}

/// This represents a Veneto "struct" 
pub struct Struct { 
    /// The main body of the struct
    body: StructBody, 

    /// The fields added with the `in +` modifier
    in_plus: Option<StructBody>, 
    /// The fields added with the `out +` modifier 
    out_plus: Option<StructBody>, 
}

/// This is a Struct's body, which is just its set of fields
pub type StructBody = Vec<StructField>;

/// This is an individual field declaration within a `Struct`
pub struct StructField { 
    name: String, 
    /// This is the field's type, 
    /// abbreviated to `typ` because `type` is a reserved keyword
    typ: Type, 
}

