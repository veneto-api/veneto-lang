use crate::parse::{lexer::TokenStream, ParseResult, tokens::{TerminalToken, RawToken, RawTokenKind}};

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
    delim: RawToken,
    delim_kind: DelimKind, 
}

fn pull_delim(stream: &mut TokenStream) -> ParseResult<(RawToken, DelimKind)> { 
    let token = stream.peek()?;
    match token.as_terminal()? { 
        TerminalToken::BraceClose => { 
            stream.next()?;
            Ok((token, DelimKind::BraceClose))
        },
        TerminalToken::Comma => { 
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

            // First we grab an identifier to add to `path` 
            let ident = stream.next()?.as_identifier()?; 
            path.push(ident); 

            let peek = stream.peek()?;
            if peek.kind == RawTokenKind::EOF { 
                return Ok(UseTreeClause { 
                    tree: UseTree { path, kind: UseTreeKind::Simple }, 
                    delim: peek, 
                    delim_kind: DelimKind::End 
                })
            }

            match peek.as_terminal()? { 
                // If we encounter a path separator, continue so we keep adding to the current `path`
                TerminalToken::PathSeparator => {
                    stream.next()?;
                    continue
                }, 

                // If we have a glob, that's the end of this clause.  Pack it in, we're done here
                TerminalToken::Glob => { 
                    stream.next()?;
                    return Ok(UseTreeClause { 
                        tree: UseTree{ path, kind: UseTreeKind::Glob }, 
                        delim: peek, 
                        delim_kind: DelimKind::End,
                    })
                }

                // We've gotta handle aliases too
                TerminalToken::PathAlias => { 
                    stream.next()?; 
                    let alias = stream.next()?.as_identifier()?; 
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

                // Here's where things get fun 
                // If we have a `{`, this is a nested use, so we recursively parse the next few clauses
                TerminalToken::BraceOpen => { 
                    stream.next()?; 
                    return Ok(UseTreeClause { 
                        tree: UseTree { 
                            path,
                            kind: UseTreeKind::Nested(Self::parse_nested(stream)?)
                        }, 
                        delim: peek, 
                        delim_kind: DelimKind::End 
                    })
                },

                // These next two are only relevant for parsing nested clauses
                // If we encounter a comma or a close brace, we terminate the current clause
                // Outside of this function, we must make sure these are only handled while looking for nested clauses 
                TerminalToken::BraceClose | TerminalToken::Comma => { 
                    let (delim, delim_kind) = pull_delim(stream)?;
                    return Ok(UseTreeClause { 
                        tree: UseTree { path, kind: UseTreeKind::Simple },
                        delim,
                        delim_kind
                    })
                }


                // 

                kind if kind.is_top_level_keyword() => { 
                    return Ok(UseTreeClause { 
                        tree: Self{ path, kind: UseTreeKind::Simple}, 
                        delim: peek, 
                        delim_kind: DelimKind::End 
                    })
                },

                _ => return Err(peek.as_err_unexpected()) 

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
    use crate::parse::ParseResult;
    use crate::parse::ast::use_tree::UseTreeKind;
    use crate::parse::{ lexer::TokenStream, TerminalToken} ;
    use crate::parse::lexer_tests::{ token_stream, assert_eof, assert_terminal };

    use super::UseTree;

    fn parse_use(str: &str) -> ParseResult<(TokenStream, UseTree)> { 
        let mut stream = token_stream(str); 
        assert_terminal(&mut stream, TerminalToken::Use);
        UseTree::parse(&mut stream).map(|tree| (stream, tree))
    }

    #[test]
    pub fn simple() { 
        let (mut stream, tree) = parse_use("use foo::bar").unwrap();
        assert_eq!(tree.path, vec![ "foo", "bar"]);
        assert_eq!(tree.kind, UseTreeKind::Simple);
        assert_eof(&mut stream);
    }
}