use crate::parse::{lexer::TokenStream, ParseResult, tokens::{Punctuation, TokenKind, Terminal, Keyword}, ClauseResult, ClauseDelim};

use super::Expectable;

#[derive(PartialEq, Debug)]
pub enum UseTreeKind { 
    /// A single use, e.g. `use foo::bar`
    Simple,
    /// A use with an alias, e.g. `use foo::bar as baz`
    Alias(String), 
    /// A nested `use`; e.g. `use foo::{ bar, baz::ree }`
    Nested(Vec<UseTree>),
    /// A wildcard; e.g. `use foo::*`
    Glob, 
}

/// A "use tree" - the part that comes right after the `use` keyword.
/// 
/// This is heavily inspired by `rustc_ast::ast::UseTree` 
#[derive(PartialEq, Debug)]
pub struct UseTree { 
    path: Vec<String>, 
    kind: UseTreeKind, 
}

fn pull_delim(stream: &mut TokenStream, result: UseTree) -> ClauseResult<UseTree> { 
    let peek = stream.peek()?; 
    match peek.as_punctuation() { 
        Some(Punctuation::Comma) => { 
            stream.next()?; 
            Ok(( result, ClauseDelim::Continue ))
        },
        Some(Punctuation::BraceClose) => { 
            stream.next()?; 
            Ok(( result, ClauseDelim::Exit ))
        },
        _ => { 
            Ok(( result, ClauseDelim::Unexpected(peek)))
        }
    }
}

impl UseTree { 

    /// Parses a single clause within a Use Tree; 
    /// it does this by grabbing tokens until it finds one that cannot be a part of the current clause.
    /// 
    /// The kind of token it stops at is indicated by the `UseDelim` in the returned tuple;
    /// this way, calling functions can know what token it ends at and act accordingly,
    /// since it works differently inside a nested Use Tree.  
    fn parse_inner(stream: &mut TokenStream) -> ClauseResult<UseTree> { 
        let mut path = Vec::<String>::new();
        
        loop { 

            let first = stream.next()?; 
            match first.kind { 
                // First we have the simplest end-of-clause case - the glob
                TokenKind::Punctuation(Punctuation::Glob) => return pull_delim(stream, UseTree{ path, kind: UseTreeKind::Glob }),

                // If it's an open brace, things get fun.
                // That means this is a nested use, so we recursively parse the next few clauses
                TokenKind::Punctuation(Punctuation::BraceOpen) => { 
                    let mut nested = Vec::<UseTree>::new(); 
                    loop { 
                        let (clause, delim) = Self::parse_inner(stream)?; 
                        nested.push(clause);
                        match delim { 
                            ClauseDelim::Continue => continue, 
                            ClauseDelim::Exit => { 
                                let result = UseTree { path, kind: UseTreeKind::Nested(nested) };
                                return pull_delim(stream, result);
                            },
                            ClauseDelim::Unexpected(t) => return Err(t.as_err_unexpected()),
                        }
                    }

                },

                // And here is the most common scenario. 
                // We've probably found an identifier - add it to the `path` and see what comes next. 
                TokenKind::Word(_) => { 

                    path.push( first.try_as_identifier()? );
                    
                    let peek = stream.peek()?; 
                    match peek.as_terminal() { 

                        // If it's another `::`, continue the process
                        Some(Terminal::Punctuation(Punctuation::PathSeparator)) => { 
                            stream.next()?;
                            continue 
                        }

                        // If it's an `as`, handle that junk, then terminate the clause
                        Some(Terminal::Keyword(Keyword::PathAlias)) => { 
                            stream.next()?; 
                            let alias = stream.next()?.try_as_identifier()?; 
                            
                            let result = UseTree { 
                                path, 
                                kind: UseTreeKind::Alias(alias)
                            };
                            return pull_delim(stream, result);
                        }

                        // Otherwise, this is unexpected, terminate the clause
                        _ => return pull_delim(stream, UseTree { path, kind: UseTreeKind::Simple }),
                    }
                },

                // Otherwise, this is unexpected. 
                _ => return Err(first.as_err_unexpected())
            }
        }
    }

}

impl Expectable for UseTree { 
    fn parse_expect(stream: &mut TokenStream) -> ParseResult<Self> { 
        let (clause, _) = Self::parse_inner(stream)?;
        Ok(clause)
    }
}


#[cfg(test)]
mod test {
    use crate::parse::ast::Expectable;
    use crate::parse::tokens::{Keyword};
    use crate::parse::{ParseResult};
    use crate::parse::ast::use_tree::UseTreeKind;
    use crate::parse::{ lexer::TokenStream} ;
    use crate::parse::lexer_tests::{ token_stream, assert_eof, assert_keyword };

    use super::UseTree;

    fn parse_use(str: &str) -> ParseResult<(TokenStream, UseTree)> { 
        let mut stream = token_stream(str); 
        assert_keyword(&mut stream, Keyword::Use);
        UseTree::parse_expect(&mut stream).map(|tree| (stream, tree))
    }

    #[test]
    fn simple() { 
        let (mut stream, tree) = parse_use("use foo::bar use").unwrap();
        assert_eq!(tree.path, vec![ "foo", "bar"]);
        assert_eq!(tree.kind, UseTreeKind::Simple);

        // `use` is a valid top-level keyword, so it should end the parsing gracefully
        assert_keyword(&mut stream, Keyword::Use);
        assert_eof(&mut stream);
    }

    #[test]
    fn glob() { 
        let (mut stream, tree) = parse_use("use foo::bar::baz::* use").unwrap();
        assert_eq!(tree.path, vec!["foo", "bar", "baz"]);
        assert_eq!(tree.kind, UseTreeKind::Glob); 

        assert_keyword(&mut stream, Keyword::Use);
        assert_eof(&mut stream); 
    }

    #[test]
    fn nested() { 
        let (_, tree) = parse_use("use foo::{ bar::baz::boo, glob::*, asdf::{ jkl, qwerty } }").unwrap(); 

        assert_eq!(tree.path, vec![ "foo" ]);
        if let UseTreeKind::Nested(outer) = tree.kind { 
            assert_eq!(outer.len(), 3);

            assert_eq!(outer[0].kind, UseTreeKind::Simple);
            assert_eq!(outer[0].path, vec![ "bar", "baz", "boo" ]);

            assert_eq!(outer[1].kind, UseTreeKind::Glob); 
            assert_eq!(outer[1].path, vec![ "glob" ]);

            assert_eq!(outer[2].path, vec!["asdf"]);
            if let UseTreeKind::Nested(inner) = &outer[2].kind { 
                assert_eq!(inner.len(), 2); 

                assert_eq!(inner[0].kind, UseTreeKind::Simple);
                assert_eq!(inner[0].path, vec![ "jkl"]);

                assert_eq!(inner[1].kind, UseTreeKind::Simple);
                assert_eq!(inner[1].path, vec![ "qwerty" ]);
            }
            else { panic!("Expected inner to be nested") }
        }
        else { panic!("Expected outer to be nested")}
    }
}