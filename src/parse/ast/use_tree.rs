use crate::parse::tokens::Keyword;
use crate::parse::{lexer::TokenStream, ParseResult, tokens::Punctuation};
use crate::peek_match; 

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

/// This is heavily inspired by `rustc_ast::ast::UseTree` 
#[derive(PartialEq, Debug)]
pub struct UseTree { 
    path: Vec<String>, 
    kind: UseTreeKind, 
}
impl Expectable for UseTree { 
    fn parse_expect(stream: &mut TokenStream) -> ParseResult<Self> {
        let mut path = Vec::<String>::new(); 
        loop { 

            if let Some(ident) = stream.peek_for_identifier()? { 
                path.push(ident); 

                if stream.peek_for_punctuation(Punctuation::PathSeparator)? { 
                    continue
                } 
                else if stream.peek_for_keyword(Keyword::As)? { 
                    let alias = stream.next()?.try_as_identifier()?; 
                    return Ok(Self { path, kind: UseTreeKind::Alias(alias) })
                }
                else { 
                    return Ok(Self { path, kind: UseTreeKind::Simple })
                }
            } 
            else { 

                let err_ref = stream.peek()?; 
                peek_match!(stream.peek_for_punctuation { 
                    Punctuation::Glob => return Ok(Self { path, kind: UseTreeKind::Glob }),
                    Punctuation::BraceOpen => { 
                        let mut nested = Vec::<Self>::new(); 
                        loop { 
                            nested.push(Self::parse_expect(stream)?);
    
                            let err_ref = stream.peek()?; 
                            peek_match!(stream.peek_for_punctuation { 
                                Punctuation::Comma => continue, 
                                Punctuation::BraceClose => return Ok(Self { path, kind: UseTreeKind::Nested(nested) }), 
                                _ => return Err(err_ref.as_err_unexpected())
                            })
                        }
                    },

                    _ => return Err(err_ref.as_err_unexpected())
                })

            }
        }
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
    fn alias() { 
        let (_, tree) = parse_use("use foo::bar as baz").unwrap();
        assert_eq!(tree.path, vec![ "foo", "bar" ]);
        assert!(matches!(tree.kind, UseTreeKind::Alias(s) if s == "baz"));
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