use std::{str::Chars, collections::VecDeque};

use super::{ ParseResult, ParseErrorKind, ParseError, tokens::WordKind };
use super::tokens::{ RawToken, RawTokenKind, Position };

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
                    kind: ParseErrorKind::Unexpected(RawTokenKind::EOF),
                    position: self.position,
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
                    kind: ParseErrorKind::Unexpected(RawTokenKind::EOF),
                    position: self.position,
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

    pub fn next(&mut self) -> ParseResult<RawToken> { 
        loop { 
            let next = self.inner_next()?;
            let position = next.position;

            match next.unit { 
                Unit::Whitespace => continue, 
                Unit::EOF => return Ok(RawToken { kind: RawTokenKind::EOF, position }), 
                Unit::StringLiteral(s) => return Ok(RawToken { kind: RawTokenKind::StringLiteral(s), position }),
                Unit::Char(c) => { 
                    if c.is_alphabetic() { 
                        return Ok(RawToken { 
                            kind: RawTokenKind::Word( WordKind::Alpha,
                                self.take_while(c, |ch| ch.is_alphanumeric() || ch == '-' || ch == '_')?
                            ), 
                            position
                        })
                    }
                    else if c.is_numeric() { 
                        return Ok(RawToken { 
                            kind: RawTokenKind::Word( WordKind::Number,
                                self.take_while(c, |ch| ch.is_numeric())?
                            ), 
                            position
                        })
                    }
                    else if c.is_ascii_punctuation() { 
                        return Ok(RawToken { 
                            kind: RawTokenKind::Word( WordKind::Punctuation,
                                self.take_while(c, |ch| ch.is_ascii_punctuation())?
                            ),
                            position
                        })
                    }
                    else { 
                        return Err(ParseError { kind: ParseErrorKind::UnknownCharacterType, position })
                    }
                }
            }
        }
    }
}



pub struct TokenStream<'a> { 
    stream: RawTokenStream<'a>,
    peeked: VecDeque<RawToken>,
}

impl<'a> TokenStream<'a> { 

    pub fn new(chars: Chars<'a>) -> Self { 
        Self { 
            stream: RawTokenStream::new(chars),
            peeked: VecDeque::new(),
        }
    }

    pub fn next(&mut self) -> ParseResult<RawToken> { 
        match self.peeked.pop_front() { 
            Some(t) => Ok(t), 
            None => self.stream.next(),
        }
    }

}
