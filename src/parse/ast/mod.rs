/// `data` represents the grammar related to data types, documented in `grammar/data.ebnf `
pub mod data;

/// `use_tree` contains everything related to imports and the `use` directive;
/// heavily inspired by `rustc_ast::ast::UseTree` 
pub mod use_tree;