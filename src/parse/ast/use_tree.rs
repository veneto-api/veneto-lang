use crate::parse::{lexer::TokenStream, ParseResult, tokens::{Punctuation, Token, TokenKind, Terminal, Keyword}};

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

#[derive(PartialEq, Debug)]
pub struct UseTree { 
    path: Vec<String>, 
    kind: UseTreeKind, 
}

/// This describes how the parsing of a `use` clause ended;
/// used when determining what to do afterward,
/// primarily in handling nested `UseTree`s 
enum DelimKind { 
    BraceClose, 
    Comma, 

    /// This describes any other kind of delimiter, whether expected or not
    End, 
}

struct UseTreeClause { 
    tree: UseTree, 
    delim: Token,
    delim_kind: DelimKind, 
}

fn pull_delim(stream: &mut TokenStream) -> ParseResult<(Token, DelimKind)> { 
    let token = stream.peek()?;
    match token.as_punctuation() { 
        Some(Punctuation::BraceClose) => { 
            stream.next()?;
            Ok((token, DelimKind::BraceClose))
        },
        Some(Punctuation::Comma) => { 
            stream.next()?;
            Ok((token, DelimKind::Comma))
        },
        _ => { 
            Ok((token, DelimKind::End))
        }
    }
}

impl UseTree { 

    /// Parses a nested Use clause, beginning right after the opening brace.
    fn parse_nested(stream: &mut TokenStream) -> ParseResult<Vec<Self>> { 
        let mut children = Vec::<Self>::new();
        loop { 
            let clause = Self::parse_inner(stream)?;
            children.push(clause.tree); 
            match clause.delim_kind { 
                DelimKind::Comma => continue, 
                DelimKind::BraceClose => break, 
                DelimKind::End => return Err(clause.delim.as_err_unexpected())
            }
        }
        Ok(children) 
    }

    /// Parses a single clause within a Use Tree; 
    /// it does this by grabbing tokens until it finds one that cannot be a part of the current clause.
    /// 
    /// The kind of token it stops at is indicated by the `UseDelim` in the returned tuple;
    /// this way, calling functions can know what token it ends at and act accordingly,
    /// since it works differently inside a nested Use Tree.  
    fn parse_inner(stream: &mut TokenStream) -> ParseResult<UseTreeClause> { 
        let mut path = Vec::<String>::new();
        
        loop { 

            let first = stream.next()?; 
            match first.kind { 
                // First we have our two simplest end-of-clause cases - 
                // end-of-file and glob 

                TokenKind::EOF => return Ok(UseTreeClause { 
                    tree: UseTree { path, kind: UseTreeKind::Simple }, 
                    delim: first, 
                    delim_kind: DelimKind::End 
                }),

                TokenKind::Punctuation(Punctuation::Glob) => return Ok(UseTreeClause { 
                    tree: UseTree{ path, kind: UseTreeKind::Glob }, 
                    delim: first, 
                    delim_kind: DelimKind::End,
                }),

                // If it's an open brace, things get fun.
                // That means this is a nested use, so we recursively parse the next few clauses
                TokenKind::Punctuation(Punctuation::BraceOpen) => { 
                    stream.next()?; 
                    return Ok(UseTreeClause { 
                        tree: UseTree { 
                            path,
                            kind: UseTreeKind::Nested(Self::parse_nested(stream)?)
                        }, 
                        // This isn't the correct end delimiter, but it shouldn't matter since this is successful
                        // I'm sure that will bite us soon 
                        delim: first, 
                        delim_kind: DelimKind::End 
                    })
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
                            let (delim, delim_kind ) = pull_delim(stream)?;
                            return Ok(UseTreeClause { 
                                tree: UseTree { 
                                    path, 
                                    kind: UseTreeKind::Alias(alias)
                                },
                                delim,
                                delim_kind
                            })
                        }


                        // These next two are only relevant for parsing nested clauses
                        // If we encounter a comma or a close brace, we terminate the current clause
                        // Outside of this function, we must make sure these are only handled while looking for nested clauses 

                        Some(Terminal::Punctuation(Punctuation::BraceClose)) => { 
                            return Ok(UseTreeClause { 
                                tree: UseTree { path, kind: UseTreeKind::Simple },
                                delim: peek,
                                delim_kind: DelimKind::BraceClose, 
                            })

                        }
                        Some(Terminal::Punctuation(Punctuation::Comma)) => { 
                            return Ok(UseTreeClause { 
                                tree: UseTree { path, kind: UseTreeKind::Simple },
                                delim: peek,
                                delim_kind: DelimKind::Comma, 
                            })
                        }


                        // Otherwise, we're at the end of this clause.
                        // If it's a valid top-level keyword, we're done here
                        // If it's not, that's unexpected
                        Some(Terminal::Keyword(kw)) if kw.is_top_level() => return Ok(UseTreeClause { 
                            tree: Self{ path, kind: UseTreeKind::Simple}, 
                            delim: peek, 
                            delim_kind: DelimKind::End 
                        }),

                        _ => return Err(peek.as_err_unexpected()) 
                    }
                },

                // Otherwise, this is unexpected. 
                _ => return Err(first.as_err_unexpected())
            }
        }
    }

    /// Parses a `UseTree` with the provided stream;
    /// assuming that the stream is pointing right after the `use` declaration.
    pub fn parse(stream: &mut TokenStream) -> ParseResult<Self> { 
        let clause = Self::parse_inner(stream)?;
        match clause.delim_kind { 
            DelimKind::End => Ok(clause.tree),
            _ => Err(clause.delim.as_err_unexpected()),
        }
    }
}


#[cfg(test)]
mod test {
    use crate::parse::tokens::{TokenKind, Keyword};
    use crate::parse::{ParseResult, ParseErrorKind};
    use crate::parse::ast::use_tree::UseTreeKind;
    use crate::parse::{ lexer::TokenStream} ;
    use crate::parse::lexer_tests::{ token_stream, assert_eof, assert_keyword };

    use super::UseTree;

    fn parse_use(str: &str) -> ParseResult<(TokenStream, UseTree)> { 
        let mut stream = token_stream(str); 
        assert_keyword(&mut stream, Keyword::Use);
        UseTree::parse(&mut stream).map(|tree| (stream, tree))
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
    fn unexpected_end() { 
        let res = parse_use("use foo::bar errrrar");
        assert!(res.is_err());
        assert_eq!(res.unwrap_err().kind, ParseErrorKind::Unexpected(TokenKind::Word("errrrar".to_string())))
    }

    #[test]
    fn glob() { 
        let (mut stream, tree) = parse_use("use foo::bar::baz::* use").unwrap();
        assert_eq!(tree.path, vec!["foo", "bar", "baz"]);
        assert_eq!(tree.kind, UseTreeKind::Glob); 

        assert_keyword(&mut stream, Keyword::Use);
        assert_eof(&mut stream); 
    }
}