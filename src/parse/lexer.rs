use std::backtrace::Backtrace;
use std::fmt::Debug;
use std::str::FromStr;
use std::{str::Chars, collections::VecDeque};

use super::{ ParseResult, ParseErrorKind, ParseError };
use super::tokens::{ Token, TokenKind, Position, Punctuation, Keyword, Terminal };

#[allow(clippy::upper_case_acronyms)]
enum Unit { 
    Char(char), 
    StringLiteral(String), 

    /// Explicitly a chunk type at this level 
    /// This also includes comments for now 
    Whitespace,

    EOF
}

/// The first stage of the lexer: a wrapper around a raw `Chars` stream.  
/// 
/// This primarily passes `char`s downstream, but also handles chunking of anything that breaks the flow of the `TokenStream`, including:
///  - Processing comments
///  - Chunking together string literals
///  - Converting whitespace and EOFs into their own enum variants for pattern matching convenience 
/// 
/// Doc comments will be handled here too
/// https://www.notion.so/veneto/Support-Doc-Comments-67e5f478d063488cabf2daba91eccf12
/// TAG: DOC_COMMENTS
struct UnitStream<'a> { 
    chars: Chars<'a>,
    peeked: VecDeque<char>,
    position: Position,
}
impl<'a> UnitStream<'a> { 
    fn new(chars: Chars<'a>) -> Self { 
        Self { 
            chars, 
            peeked: VecDeque::new(),
            position: Position { line: 0, col: 0 }
        }
    }

    fn position(&self) -> Position { self.position }

    //TAG: POSITION_DRIFT
    // The position here doesn't account for backtracking - that's a later problem
    // https://veneto.notion.site/Position-drifts-in-lexer-c98e2380f3f646d891f2da09f75f5999

    /// Takes the next character, from either the queue or the source stream.
    fn inner_next(&mut self) -> Option<char> { 
        let next = self.peeked.pop_front().or_else(|| self.chars.next());
        if let Some(ch) = next { 
            if ch == '\n' { 
                self.position.line += 1;
                self.position.col = 0;
            } else { 
                self.position.col += 1; 
            }
        }
        next
    }

    /// Peeks the next character, without advancing the stream.
    /// 
    /// Returns the next character from the queue if applicable,
    /// or takes the next character from the source stream and pushes it onto the queue.
    fn inner_peek(&mut self) -> Option<char> { 
        match self.peeked.front() { 
            Some(ch) => Some(*ch), 
            None => { 
                match self.chars.next() { 
                    None => None, 
                    Some(ch) => { 
                        self.peeked.push_back(ch);
                        Some(ch) 
                    }
                }
            }
        }
    }

    /// Advances the stream until the `predicate` returns false, or the end of the stream is reached.
    /// 
    /// When the `predicate` returns false, that character will be pushed onto the queue - 
    /// so, effectively, the stream will be stopped at the point right before that happens. 
    fn skip_while(&mut self, predicate: fn(char) -> bool) { 
        loop { 
            match self.inner_next() { 
                None => break, 
                Some(ch) => { 
                    if predicate(ch) { continue }
                    else { 
                        self.peeked.push_back(ch); 
                        break
                    }
                }
            }
        }
    }

    /// Advances the stream past the next instance of `expected`.
    /// 
    /// If `expected` never occurs before the end of the stream, this returns `UnexpectedEOF`. 
    fn skip_past(&mut self, expected: char) -> ParseResult<()> { 
        loop { 
            match self.inner_next() { 
                None => return Err(ParseError{
                    kind: ParseErrorKind::Unexpected(TokenKind::EOF),
                    position: self.position,
                    backtrace: Backtrace::capture(),
                }),
                Some(ch) => { 
                    if ch == expected { return Ok(()) }
                    else { continue }
                }
            }
        }
    }

    /// Like `skip_past`, except it returns all of the scanned characters up to that point. 
    fn take_up_to(&mut self, expected: char) -> ParseResult<String> { 
        let mut str = String::new(); 

        loop { 
            match self.inner_next() { 
                None => return Err(ParseError{
                    kind: ParseErrorKind::Unexpected(TokenKind::EOF),
                    position: self.position,
                    backtrace: Backtrace::capture(),
                }),
                Some(ch) => { 
                    if ch == expected { return Ok(str) }
                    else { 
                        str.push(ch);
                        continue 
                    }
                }
            }
        }
    }

    /// Peeks the stream for an instance of `expected`, and only advances the stream if it is found.
    /// 
    /// If `expected` is the next character, the function returns `true` and the stream is advanced.
    /// Otherwise, the function returns `false` and the stream is not advanced. 
    fn check_for(&mut self, expected: char) -> bool { 
        if self.inner_peek() == Some(expected) { 
            let _ = self.inner_next(); // pop it off before continuing 
            true 
        }
        else { false }
    }

    pub fn next(&mut self) -> ParseResult<Unit> { 
        match self.inner_next() { 
            None => Ok(Unit::EOF), 
            Some(ch) => { 
                if ch.is_whitespace() { 
                    self.skip_while(|ch| ch.is_whitespace());
                    Ok(Unit::Whitespace)
                }
                else if ch == '"' { 
                    // Handle string literals. 
                    // If we add escaping, it'll happen here
                    //TAG: ESCAPING https://veneto.notion.site/Escaping-in-string-literals-d76caa72c65546a09ce0e668025f36bc
                    Ok(Unit::StringLiteral(
                        self.take_up_to('"')?
                    ))
                }
                else if ch == '/' { 
                    // Check for comments
                    
                    if self.check_for('/') { 
                        // If the cursor is at a `//`, this is a line comment.
                        // Read to the end of line and past any remaining whitespace, and emit a `Whitespace` char. 

                        // Ignoring EOF errors as that is a valid end of comment
                        // This is kinda sloppy
                        let _ = self.skip_past('\n');

                        self.skip_while(|ch| ch.is_whitespace());
                        Ok(Unit::Whitespace)
                    }
                    else if self.check_for('*') { 
                        // If the cursor is at a `/*`, this is a block comment.
                        // Keep reading to the next `*` until a `/` is found after it, then do the same as above. 

                        loop { 
                            self.skip_past('*')?;
                            if self.check_for('/') { break }
                        }

                        self.skip_while(|ch| ch.is_whitespace());
                        Ok(Unit::Whitespace)
                    }
                    else { 
                        // No `/` or `*` was found after the first `/`, so this is not a comment.
                        // Return the character as normal
                        Ok(Unit::Char(ch))
                    }

                }
                else { 
                    Ok(Unit::Char(ch))
                }
            }
        }
    }
}

/// Private wrapper struct to ensure that the `RawTokenStream` doesn't lose its position for queued units 
struct UnitCursor { 
    unit: Unit, 
    position: Position, 
}


/// The second stage of the lexer:  
/// On top of the output from the first stage, this coalesces character types into single tokens. 
struct RawTokenStream<'a> { 
    stream: UnitStream<'a>,
    peeked: VecDeque<UnitCursor>,
}
impl<'a> RawTokenStream<'a> { 

    fn new(chars: Chars<'a>) -> Self { 
        Self { 
            stream: UnitStream::new(chars), 
            peeked: VecDeque::new(), 
        }
    }

    /// Takes the next character, from either the queue or the source stream.
    fn inner_next(&mut self) -> ParseResult<UnitCursor> { 
        match self.peeked.pop_front() { 
            Some(t) => Ok(t), 
            None => self.stream.next().map(|unit| UnitCursor { unit, position: self.stream.position() }),
        }
    }

    /// Starting with the given `starting` char, this takes characters from the stream while they match `predicate`,
    /// returning the captured string once they stop matching the `predicate`.
    /// 
    /// This requeues the first non-matching character, 
    /// so the cursor effectively remains between the captured sequence and the rest of the stream. 
    fn take_while(&mut self, starting: char, predicate: fn(char) -> bool) -> ParseResult<String> { 
        let mut str = String::new();
        str.push(starting);

        loop { 
            let next = self.inner_next()?; 

            match next.unit { 
                Unit::Char(ch) if predicate(ch) => { 
                    str.push(ch); 
                    continue
                },

                _ => {
                    self.peeked.push_back(next);
                    return Ok(str)
                }
            }
        }
    }

    pub fn next(&mut self) -> ParseResult<Token> { 

        loop { 
            let next = self.inner_next()?;
            let position = next.position;

            match next.unit { 
                Unit::Whitespace => continue, 
                Unit::EOF => return Ok(Token { kind: TokenKind::EOF, position }), 
                Unit::StringLiteral(s) => return Ok(Token { kind: TokenKind::StringLiteral(s), position }),
                Unit::Char(c) => { 
                    if c.is_alphabetic() { 
                        return Ok(Token { 
                            kind: TokenKind::Word(
                                self.take_while(c, |ch| ch.is_alphanumeric() || ch == '-' || ch == '_')?
                            ), 
                            position
                        })
                    }
                    else if c.is_numeric() { 
                        return Ok(Token { 
                            kind: TokenKind::Number(self.take_while(c, |ch| ch.is_numeric())?), 
                            position
                        })
                    }
                    else if c.is_ascii_punctuation() { 

                        // We can't just use `take_while` here because multi-char punctuations can be right next to each other
                        // We also can't just return the first one we find, because some punctuations are substrings of another
                        // (notably, PathSeparator :: vs Colon :)

                        let mut word = String::new(); 
                        word.push(c); 

                        /*
                            So the way we do this is to process the entire word and keep track of our longest match.
                            We have a list so that we can backtrack if we end up overshooting the longest match,
                            and it's mutable so that it can be reset every time we have a new match. 
                         */

                        let mut match_longest : Option<Token> = Punctuation::from_str(&word).ok().map(|op| Token { 
                            kind: TokenKind::Punctuation(op),
                            position,
                        });
                        let mut match_backtrack = Vec::<UnitCursor>::new();

                        // Arbitrary limit - this could be totally unnecesssary but it's here for now 
                        let size_limit = 50;
                        let mut size_i = 0;

                        loop { 
                            size_i += 1;
                            if size_i > size_limit { 
                                return Err(ParseError { 
                                    kind: ParseErrorKind::WordTooLong, 
                                    position,
                                    backtrace: Backtrace::capture(), 
                                })
                            }

                            let next = self.inner_next()?; 

                            match next.unit { 
                                Unit::Char(ch) if ch.is_ascii_punctuation() => { 
                                    word.push(ch); 
                                    if let Ok(op) = Punctuation::from_str(&word) { 
                                        match_longest = Some(Token{ kind: TokenKind::Punctuation(op), position });
                                        match_backtrack = Vec::new(); 
                                    } else { 
                                        match_backtrack.push(next); 
                                    }
                                },

                                _ => { 
                                    match_backtrack.push(next); 
                                    break
                                }
                            }
                        }

                        if let Some(token) = match_longest { 

                            for token in match_backtrack.into_iter() { self.peeked.push_back(token); }
                            return Ok(token)

                        } else { 
                            return Err(ParseError { 
                                kind: ParseErrorKind::UnrecognizedPunctuation(word), 
                                position,
                                backtrace: Backtrace::capture(),
                            })
                        }
                    }
                    else { 
                        return Err(ParseError { 
                            kind: ParseErrorKind::UnknownCharacterType, 
                            position,
                            backtrace: Backtrace::capture(),
                        })
                    }
                }
            }
        }
    }
}


/// This is a wrapper around the `RawTokenStream` that just exists to enable peeking features.
pub struct TokenStream<'a> { 
    stream: RawTokenStream<'a>,
    peeked: VecDeque<Token>,
}
impl<'a> Debug for TokenStream<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("<TokenStream (stub Debug impl)>")
    }
}

impl<'a> TokenStream<'a> { 

    pub fn new(chars: Chars<'a>) -> Self { 
        Self { 
            stream: RawTokenStream::new(chars),
            peeked: VecDeque::new(),
        }
    }

    /// Pulls the next token from the stream, and advances the cursor.
    #[allow(clippy::should_implement_trait)]
    pub fn next(&mut self) -> ParseResult<Token> { 
        match self.peeked.pop_front() { 
            Some(t) => Ok(t), 
            None => self.stream.next(),
        }
    }

    /// Peeks the next token from the stream, without advancing the cursor.
    pub fn peek(&mut self) -> ParseResult<Token> { 
        match self.peeked.front() { 
            Some(t) => Ok(t.clone()), 
            None => { 
                let next = self.stream.next()?;
                self.peeked.push_back(next.clone()); 
                Ok(next) 
                // I wish there were an Entry-like API here 
            }
        }
    }

    /// Peek the next token from the stream.
    /// If it's a valid identifier, advance the cursor and return it.
    /// Otherwise, return `None`.  
    pub fn peek_for_identifier(&mut self) -> ParseResult<Option<String>> { 
        match self.peek()?.as_identifier() { 
            Some(ident) => { 
                self.next()?;
                Ok(Some(ident))
            },
            None => Ok(None)
        }
    }

    /// Peek the next token from the stream.
    /// If it matches the provided punctuation, advance the cursor and return `true`
    /// Otherwise, return `false`.  
    pub fn peek_for_punctuation(&mut self, expected: Punctuation) -> ParseResult<bool> { 
        match self.peek()?.as_punctuation() { 
            Some(x) if x == expected => { 
                self.next()?;
                Ok(true)
            },
            _ => Ok(false)
        }
    }

    /// Peek the next token from the stream.
    /// If it matches the provided keyword, advance the cursor and return `true`
    /// Otherwise, return `false`.  
    pub fn peek_for_keyword(&mut self, expected: Keyword) -> ParseResult<bool> { 
        match self.peek()?.as_keyword() { 
            Some(x) if x == expected => { 
                self.next()?;
                Ok(true)
            },
            _ => Ok(false) 
        }
    }

    /// Peek the next token from the stream.
    /// If it matches the provided terminal, advance the cursor and return `true`
    /// Otherwise, return `false`.
    pub fn peek_for_terminal(&mut self, expected: Terminal) -> ParseResult<bool> { 
        match self.peek()?.as_terminal() { 
            Some(x) if x == expected => { 
                self.next()?;
                Ok(true)
            },
            _ => Ok(false) 
        }
    }  
    

}

#[macro_export]
macro_rules! peek_match {
    ($stream:ident.$fn:ident { $first:expr => $first_then:expr, $($other:expr => $other_then:expr),+ , _ => $else_then:expr }) => { 
        if $stream.$fn($first)? { $first_then }
        $(
            else if $stream.$fn($other)? { $other_then }
        )+
        else { $else_then }
    }
}