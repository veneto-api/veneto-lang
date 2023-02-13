#[macro_use]
extern crate lazy_static;

pub mod parse;

mod std;

use parse::ParseResult;
use parse::ast::document::Document;

lazy_static! { 
    static ref STD : ParseResult<Document> = { 
        let stream = parse::lexer::TokenStream::new( include_str!("std/std.veneto").chars() ); 
        Document::parse(stream)
    };
}