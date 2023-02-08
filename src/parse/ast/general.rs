use crate::parse::{ClauseResult, ClauseDelim};
use crate::parse::{lexer::TokenStream};
use crate::parse::tokens::Punctuation;

/// This is an identifier that can accept generic parameters.
#[derive(PartialEq, Eq, Debug)]
pub enum GenericIdentifier { 
    Simple(String), 
    Generic(String, Vec<GenericIdentifier>)
}

impl GenericIdentifier { 

    /// Parses a potentially-generic identifier, starting with the provided `base` identifier. 
    fn parse(stream: &mut TokenStream, base: String) -> ClauseResult<Self> { 
        let next = stream.peek()?; 
        
        match next.as_punctuation() { 
            Some(Punctuation::GenericOpen) => { 
                stream.next()?;

                let mut params = Vec::<GenericIdentifier>::new(); 
                loop { 
                    let ident = stream.next()?.try_as_identifier()?;
                    let (clause, delim) = Self::parse(stream, ident)?;
                    params.push(clause);

                    match delim { 
                        ClauseDelim::Continue => continue, 
                        ClauseDelim::Exit => { 
                            let result = GenericIdentifier::Generic(base, params);
                            if stream.peek()?.as_punctuation() == Some(Punctuation::Comma) { 
                                stream.next()?;
                                return Ok((result, ClauseDelim::Continue))
                            } else { 
                                return Ok((result, delim))
                            }
                        },

                        ClauseDelim::Unexpected(t) => return Err(t.as_err_unexpected()) 
                    }

                }

            },

            Some(Punctuation::Comma) => { 
                stream.next()?; 
                Ok((Self::Simple(base), ClauseDelim::Continue))
            },

            Some(Punctuation::GenericClose) => { 
                stream.next()?; 
                Ok((Self::Simple(base), ClauseDelim::Exit))
            }

            _ => { 
                Ok((Self::Simple(base), ClauseDelim::Unexpected(next)))
            }
        }

    }
}


#[cfg(test)]
mod test {
    use crate::parse::{lexer_tests::token_stream, ClauseResult};

    use super::GenericIdentifier;

    fn parse_gid(input: &str) -> ClauseResult<GenericIdentifier>{ 
        let mut stream = token_stream(input); 
        let base = stream.next().unwrap().try_as_identifier().unwrap(); 
        GenericIdentifier::parse(&mut stream, base)
    }
    fn expect_gid(input: &str, expected: GenericIdentifier) { 
        assert_eq!(parse_gid(input).unwrap().0, expected); 
    }

    macro_rules! gid {

        ($base:literal < $($params:tt),+ >) => { 
            GenericIdentifier::Generic($base.to_owned(), vec![ $(gid!($params)),+ ])
        };

        ($base:ident) => { 
            $base
        };

        ($base:literal) => {
            GenericIdentifier::Simple($base.to_owned())
        };

    }
 
    #[test]
    fn simple() { 
        expect_gid("foo", gid!("foo"));
    }

    #[test] 
    fn single_param() { 
        expect_gid("foo<bar>", gid!("foo"<"bar">))
    }

    #[test]
    fn multi_param() { 
        expect_gid("foo<bar, baz>", gid!("foo"<"bar", "baz">))
    }

    #[test]
    fn complex() { 
        let inner = gid!("baz"<"asdf", "ree">);
        expect_gid("foo<bar, baz<asdf, ree>, aaa>", gid!("foo"<"bar", inner, "aaa">));
    }
}