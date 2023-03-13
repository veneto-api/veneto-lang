use crate::parse::ast::Spanned;

pub enum Type { 
    Struct(Vec<StructField>)
}

pub struct StructField { 
    pub name: Spanned<String>, 
    pub typ: usize, 
}