/// This is parses common miscellaneous expressions.
/// At the time, that's only generic identifiers, so maybe this module should be renamed.
/// 
/// This is documented in `grammar/general.ebnf`
pub mod general;

/// This contains everything related to imports and the `use` directive;
/// heavily inspired by `rustc_ast::ast::UseTree` 
/// 
/// This is documented in `grammar/general.ebnf`
pub mod use_tree;

/// This parses type expressions, as well as top-level type alias directives.
/// 
/// This is documented in `general/data.ebnf`
pub mod types;

/// This parses interface expressions.
pub mod interfaces;