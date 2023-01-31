use std::{str::{Chars, FromStr}, collections::VecDeque};

use super::{ParseResult, ParseError, tokens::{ TerminalToken }};


#[allow(clippy::upper_case_acronyms)]
enum CharStreamOut { 
    Char(char), 
    StringLiteral(String), 

    /// This includes comments for now 
    Whitespace,

    EOF
}

/// The first stage of the lexer: a wrapper around a raw `Chars` stream.  
/// This handles whitespace, comments, and string literals.
struct CharStream<'a> { 
    chars: Chars<'a>,
    buf: VecDeque<char>,
}
impl<'a> CharStream<'a> { 
    fn new(chars: Chars<'a>) -> Self { 
        Self { 
            chars, 
            buf: VecDeque::new(),
        }
    }

    /// Takes the next character, from either the queue or the source stream.
    fn inner_next(&mut self) -> Option<char> { 
        self.buf.pop_front().or_else(|| self.chars.next())
    }

    /// Peeks the next character, without advancing the stream.
    /// 
    /// Returns the next character from the queue if applicable,
    /// or takes the next character from the source stream and pushes it onto the queue.
    fn inner_peek(&mut self) -> Option<char> { 
        match self.buf.front() { 
            Some(ch) => Some(*ch), 
            None => { 
                match self.chars.next() { 
                    None => None, 
                    Some(ch) => { 
                        self.buf.push_back(ch);
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
                        self.buf.push_back(ch); 
                        break
                    }
                }
            }
        }
    }

    /// Advances the stream to after the next instance of `expected`.
    /// 
    /// If `expected` never occurs before the end of the stream, this returns `UnexpectedEOF`. 
    fn skip_to(&mut self, expected: char) -> ParseResult<()> { 
        loop { 
            match self.inner_next() { 
                None => return Err(ParseError::Unexpected(RawToken::EOF)),
                Some(ch) => { 
                    if ch == expected { return Ok(()) }
                    else { continue }
                }
            }
        }
    }

    /// Like `skip_to`, except it returns all of the scanned characters up to that point. 
    fn take_up_to(&mut self, expected: char) -> ParseResult<String> { 
        let str = String::new(); 

        loop { 
            match self.inner_next() { 
                None => return Err(ParseError::Unexpected(RawToken::EOF)),
                Some(ch) => { 
                    if ch == expected { return Ok(str) }
                    else { continue }
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

    pub fn next(&mut self) -> ParseResult<CharStreamOut> { 
        match self.inner_next() { 
            None => Ok(CharStreamOut::EOF), 
            Some(ch) => { 
                if ch.is_whitespace() { 
                    self.skip_while(|ch| ch.is_whitespace());
                    Ok(CharStreamOut::Whitespace)
                }
                else if ch == '"' { 
                    // Handle string literals. 
                    // If we add escaping, it'll happen here
                    //TAG: ESCAPING https://veneto.notion.site/Escaping-in-string-literals-d76caa72c65546a09ce0e668025f36bc
                    Ok(CharStreamOut::StringLiteral(
                        self.take_up_to('"')?
                    ))
                }
                else if ch == '/' { 
                    // Check for comments
                    
                    if self.check_for('/') { 
                        // If the cursor is at a `//`, this is a line comment.
                        // Read to the end of line, and then past any subsequent whitespace, and return. 

                        // Ignoring EOF errors as that is a valid end of comment
                        // This is kinda sloppy
                        let _ = self.skip_to('\n');

                        self.skip_while(|ch| ch.is_whitespace());
                        Ok(CharStreamOut::Whitespace)
                    }
                    else if self.check_for('*') { 
                        // If the cursor is at a `/*`, this is a block comment.
                        // Keep reading to the next `*` until a `/` is found after it, then do the same as above. 

                        loop { 
                            self.skip_to('*')?;
                            if self.inner_peek() == Some('/') { break }
                        }

                        self.skip_while(|ch| ch.is_whitespace());
                        Ok(CharStreamOut::Whitespace)
                    }
                    else { 
                        // No `/` or `*` was found after the first `/`, so this is not a comment.
                        // Return the character as normal
                        Ok(CharStreamOut::Char(ch))
                    }

                }
                else { 
                    Ok(CharStreamOut::Char(ch))
                }
            }
        }
    }
}

#[derive(PartialEq, Eq, Debug)]
pub enum WordType { 
    Alpha, 
    Number,
    Punctuation, 
}

#[allow(clippy::upper_case_acronyms)]
#[derive(PartialEq, Eq, Debug)]
pub enum RawToken { 
    Word(WordType, String), 
    StringLiteral(String), 
    EOF, 
}


/// The second stage of the lexer:  
/// On top of the output from the first stage, this coalesces character types into single tokens. 
struct RawTokenStream<'a> { 
    stream: CharStream<'a>,
    buf: VecDeque<CharStreamOut>,
}
impl<'a> RawTokenStream<'a> { 

    fn new(chars: Chars<'a>) -> Self { 
        Self { 
            stream: CharStream::new(chars), 
            buf: VecDeque::new(), 
        }
    }

    /// Takes the next character, from either the queue or the source stream.
    fn inner_next(&mut self) -> ParseResult<CharStreamOut> { 
        match self.buf.pop_front() { 
            Some(t) => Ok(t), 
            None => self.stream.next(),
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

            match next { 
                CharStreamOut::Char(ch) if predicate(ch) => { 
                    str.push(ch); 
                    continue
                },

                _ => {
                    self.buf.push_back(next);
                    return Ok(str)
                }
            }
        }
    }

    pub fn next(&mut self) -> ParseResult<RawToken> { 
        loop { 
            match self.inner_next()? { 
                CharStreamOut::Whitespace => continue, 
                CharStreamOut::EOF => return Ok(RawToken::EOF), 
                CharStreamOut::StringLiteral(s) => return Ok(RawToken::StringLiteral(s)),
                CharStreamOut::Char(c) => { 
                    if c.is_alphabetic() { 
                        return Ok(RawToken::Word( WordType::Alpha,
                            self.take_while(c, |ch| ch.is_alphanumeric() || ch == '-' || ch == '_')?
                        ))
                    }
                    else if c.is_numeric() { 
                        return Ok(RawToken::Word( WordType::Number,
                            self.take_while(c, |ch| ch.is_numeric())?
                        ))
                    }
                    else if c.is_ascii_punctuation() { 
                        return Ok(RawToken::Word( WordType::Punctuation,
                            self.take_while(c, |ch| ch.is_ascii_punctuation())?
                        ))
                    }
                    else { 
                        return Err(ParseError::UnknownCharacterType)
                    }
                }
            }
        }
    }
}



impl RawToken {
    pub fn as_terminal(self) -> ParseResult<TerminalToken>{ 
        if let RawToken::Word(_, ref val) = self { 
            TerminalToken::from_str(val).map_err(|_| ParseError::Unexpected(self))
        } else { 
            Err(ParseError::Unexpected(self))
        }
    }

    pub fn as_identifier(self) -> ParseResult<String> { 
        if let RawToken::Word(WordType::Alpha, val) = self { 
            Ok(val)
        }
        else { 
            Err(ParseError::Unexpected(self))
        }
    }

    pub fn expect(self, terminal: TerminalToken) -> ParseResult<()> { 
        if let RawToken::Word(_, ref val) = self { 
            if TerminalToken::from_str(val) == Ok(terminal) { return Ok(()) }
        }
        Err(ParseError::Expected(terminal))
    }
}


pub struct TokenStream<'a> { 
    stream: RawTokenStream<'a>,
    buf: VecDeque<RawToken>,
}

impl<'a> TokenStream<'a> { 

    pub fn new(chars: Chars<'a>) -> Self { 
        Self { 
            stream: RawTokenStream::new(chars),
            buf: VecDeque::new(),
        }
    }

    pub fn next(&mut self) -> ParseResult<RawToken> { 
        match self.buf.pop_front() { 
            Some(t) => Ok(t), 
            None => self.stream.next(),
        }
    }

}