use std::backtrace::Backtrace;
use std::fmt::{Debug, Display};
use std::str::FromStr;
use std::{str::Chars, collections::VecDeque};

use crate::parse::tokens::LONGEST_PUNCTUATION;

use super::ast::Spanned;
use super::{ ParseResult, ParseErrorKind, ParseError };
use super::tokens::{ Token, TokenKind, Punctuation, Keyword, Terminal };


pub type Index = u32; 

/// The position _before_ the token begins
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Position (pub Index);
impl Display for Position { 
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(&self.0, f)   
    }
}

/// A span of text from the input stream; a start and end position
/// 
/// Rustc has a different struct called [`SpanData`](https://doc.rust-lang.org/beta/nightly-rustc/rustc_span/struct.SpanData.html) 
/// that contains other syntactic information for diagnostics - we can look there when the time comes
/// Some other potential optimizations to be found in [rustc](https://doc.rust-lang.org/beta/nightly-rustc/rustc_span/span_encoding/struct.Span.html)
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Span { 
    pub lo: Position,
    pub hi: Position, 
}
impl Span { 
    pub fn through(&self, last: Self) -> Self { 
        Self { 
            lo: self.lo,
            hi: last.hi, 
        }
    }
}

impl Display for Span { 
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.lo == self.hi { 
            f.write_fmt(format_args!("i={}", self.lo))
        } else { 
            f.write_fmt(format_args!("i=[{},{}]", self.lo, self.hi))
        }
    }
}
impl From<Position> for Span { 
    fn from(pos: Position) -> Self {
        Self { 
            lo: pos, 
            hi: pos, 
        }
    }
}



#[allow(clippy::upper_case_acronyms)]
enum UnitKind { 
    /// A single `char`, presumed to have a length of 1 
    Char(char), 

    /// A string literal - contains its decoded contents, as well as the index **after** the closing quote 
    /// 
    /// We have to track the end index separately since this becomes a proper token 
    StringLiteral(String, Index), 

    /// Explicitly a chunk type at this level 
    /// This also includes comments for now 
    Whitespace,

    /// End-of-file
    EOF
}

struct Unit { 
    kind: UnitKind, 
    index: Index, 
}
impl Unit { 
    fn get_end(&self) -> Position { 
        match self.kind { 
            UnitKind::StringLiteral(_, end) => Position(end), 
            _ => Position(self.index + 1), 
        }
    }
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
pub(super) struct UnitStream<'a> { 
    chars: Chars<'a>,
    peeked: VecDeque<char>,

    index: Index,

    /// This is the ordered set of indices that have newline characters.
    /// 
    /// (line, col) positions can be found by looking for the last newline before any given index, and subtracting
    pub newlines: Vec<Index>, 
}
impl<'a> UnitStream<'a> { 
    fn new(chars: Chars<'a>) -> Self { 
        Self { 
            chars, 
            peeked: VecDeque::new(),
            index: 0, 
            newlines: Vec::new(), 
        }
    }

    fn backtrack(&mut self, ch: char) { 
        self.index -= 1; 
        self.peeked.push_back(ch); 
    }

    /// Reads the next character from the source stream, also handling newlines
    /// 
    /// This should only be called from `inner_next` and `inner_peek`, 
    /// so that the rest of the implementation doesn't have to care about the peek queue 
    fn read_next(&mut self) -> Option<char> { 
        let ch = self.chars.next(); 

        if let Some('\n') = ch { 
            self.newlines.push(self.index); 
        }

        ch
    }

    /// Takes the next character, from either the queue or the source stream.
    fn inner_next(&mut self) -> Option<char> { 
        let ch = self.peeked.pop_front().or_else(|| self.read_next());
        self.index += 1; 
        ch
    }

    /// Peeks the next character, without advancing the stream.
    /// 
    /// Returns the next character from the queue if applicable,
    /// or takes the next character from the source stream and pushes it onto the queue.
    fn inner_peek(&mut self) -> Option<char> { 
        match self.peeked.front() { 
            Some(ch) => Some(*ch), 
            None => { 
                let next = self.read_next();
                if let Some(ch) = next { self.peeked.push_back(ch) }
                next 
            }, 
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
                        self.backtrack(ch); 
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
                    span: Position(self.index).into(),
                    backtrace: Backtrace::capture(),
                }),
                Some(ch) => { 
                    if ch == expected { return Ok(()) }
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

    fn next(&mut self) -> ParseResult<Unit> { 

        let i = self.index;
        let make_unit = |kind: UnitKind| Unit { kind, index: i };

        match self.inner_next() { 
            None => Ok(make_unit(UnitKind::EOF)), 
            Some(ch) => { 
                if ch.is_whitespace() { 
                    self.skip_while(|ch| ch.is_whitespace());
                    Ok(make_unit(UnitKind::Whitespace))
                }
                else if ch == '"' { 
                    // Handle string literals. 
                    // If we add escaping, it'll happen here
                    //TAG: ESCAPING https://veneto.notion.site/Escaping-in-string-literals-d76caa72c65546a09ce0e668025f36bc

                    let mut str = String::new(); 
                    let mut end_index;

                    loop { 
                        end_index = self.index;
                        match self.inner_next() { 
                            None => return Err(ParseError{
                                kind: ParseErrorKind::Unexpected(TokenKind::EOF),
                                span: Position(self.index).into(),
                                backtrace: Backtrace::capture(),
                            }),
                            Some(ch) => { 
                                if ch == '"' { 
                                    return Ok(make_unit(UnitKind::StringLiteral(
                                        str,
                                        end_index 
                                    )))
                                }
                                else { 
                                    str.push(ch);
                                    continue 
                                }
                            }
                        }
                    }
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
                        Ok(make_unit(UnitKind::Whitespace))
                    }
                    else if self.check_for('*') { 
                        // If the cursor is at a `/*`, this is a block comment.
                        // Keep reading to the next `*` until a `/` is found after it, then do the same as above. 

                        loop { 
                            self.skip_past('*')?;
                            if self.check_for('/') { break }
                        }

                        self.skip_while(|ch| ch.is_whitespace());
                        Ok(make_unit(UnitKind::Whitespace))
                    }
                    else { 
                        // No `/` or `*` was found after the first `/`, so this is not a comment.
                        // Return the character as normal
                        Ok(make_unit(UnitKind::Char(ch)))
                    }

                }
                else { 
                    Ok(make_unit(UnitKind::Char(ch)))
                }
            }
        }
    }
}


/// The second stage of the lexer:  
/// On top of the output from the first stage, this coalesces character types into single tokens. 
pub(super) struct RawTokenStream<'a> { 
    pub(super) stream: UnitStream<'a>,
    peeked: VecDeque<Unit>,
}
impl<'a> RawTokenStream<'a> { 

    fn new(chars: Chars<'a>) -> Self { 
        Self { 
            stream: UnitStream::new(chars), 
            peeked: VecDeque::new(), 
        }
    }

    /// Takes the next character, from either the queue or the source stream.
    fn inner_next(&mut self) -> ParseResult<Unit> { 
        match self.peeked.pop_front() { 
            Some(t) => Ok(t), 
            None => self.stream.next(),
        }
    }

    fn backtrack(&mut self, unit: Unit) { 
        self.peeked.push_back(unit);
    }

    fn consume(
        &mut self, 
        first_unit: Unit,  
        first_char: char, 
        kind: fn(String) -> TokenKind, 
        predicate: fn(char) -> bool
    ) -> ParseResult<Token> { 
        let mut str = String::new(); 
        let mut last_pos = first_unit.get_end();
        str.push(first_char); 

        loop { 
            let next = self.inner_next()?; 

            match next.kind { 
                UnitKind::Char(ch) if predicate(ch) => { 
                    str.push(ch); 
                    last_pos = next.get_end();
                    continue 
                },

                _ => { 
                    self.backtrack(next); 
                    return Ok(Token { 
                        kind: kind(str), 
                        span: Span { 
                            lo: Position(first_unit.index),
                            hi: last_pos, 
                        }
                    })
                }
            }
        }
    }

    pub fn next(&mut self) -> ParseResult<Token> { 

        loop { 
            let next = self.inner_next()?;

            match next.kind { 
                UnitKind::Whitespace => continue, 
                UnitKind::EOF => return Ok(Token { 
                    kind: TokenKind::EOF, 
                    span: Position(next.index).into(),
                }), 

                UnitKind::StringLiteral(contents, end) => {
                    return Ok(Token{ 
                        kind: TokenKind::StringLiteral(contents),
                        span: Span { 
                            lo: Position(next.index),
                            hi: Position(end), 
                        },
                    })
                },

                UnitKind::Char(c) => { 
                    if c.is_alphabetic() { 
                        return self.consume(
                            next, 
                            c, 
                            TokenKind::Word, 
                            |ch| ch.is_alphanumeric() || ch == '_'
                        )
                    }
                    else if c.is_numeric() { 
                        return self.consume(
                            next, 
                            c, 
                            TokenKind::Number, 
                            |ch| ch.is_numeric()
                        )
                    }
                    else if c.is_ascii_punctuation() { 

                        /*
                            We can't just use `consume` here because multi-char punctuations can be right next to each other
                            We also can't just return the first one we find, because some punctuations are substrings of another
                            (notably, PathSeparator `::` vs Colon `:`)

                            So the way we do this is to process the entire word and keep track of our longest match.
                            We have a list so that we can backtrack if we end up overshooting the longest match,
                            and it's mutable so that it can be reset every time we have a new match. 

                            TODO: Perhaps a better idea is to just preemptively peek all of the possible characters...
                            that way i'm not re-peeking those characters in the case of consecutive punctuations
                            To make it stateless, we can just read from the source stream until a non-punctuation char is found each time
                                so that way it won't happen twice 
                            
                            But honestly this probably needs to all go in the first stage anyways.
                            The more I think about it the less sense the multi-stage lexer makes
                         */

                        // Lol we have another `next` below so it gets confusing
                        let first = next; 
                        let first_index = first.index; 

                        let mut word = String::new(); 
                        let mut last_position = first.get_end();

                        let mut match_longest : Option<Token> = None; 
                        let mut match_backtrack : Vec<Unit> = Vec::new(); 

                        // This tries to process an incoming unit as punctuation.  
                        // If the unit is not a valid punctuation character, add it to the `backtrack` and return false.
                        // Otherwise, process it (see below) and return true 
                        let mut try_punct = |unit: Unit| -> bool{ 
                            /*
                                This is kinda awkward
                                We need the `Unit` here so that we can push to the backtrack queue 
                                But we have to extract it with the below bindings
                             */
                            let UnitKind::Char(ch) = unit.kind else { 
                                match_backtrack.push(unit);
                                return false; 
                            };
                            if !ch.is_ascii_punctuation() { 
                                match_backtrack.push(unit);
                                return false; 
                            }

                            // Here we update our `word` and `last_position` 
                            word.push(ch); 
                            last_position = unit.get_end(); 

                            if let Ok(p) = Punctuation::from_str(&word) { 
                                // If we have a new longest match, replace the previous match and clear the backtrack queue
                                
                                match_longest = Some(Token { 
                                    kind: TokenKind::Punctuation(p), 
                                    span: Span { 
                                        lo: Position(first_index), 
                                        hi: unit.get_end(), 
                                    }
                                });

                                match_backtrack = Vec::new(); 
                            }
                            else { 
                                // Otherwise, add it to the backtrack queue but keep moving 
                                match_backtrack.push(unit); 
                            }

                            true
                        };

                        // Here we try the very first unit we found,
                        // This should always be `true` since we verified those conditions to get to this point,
                        //  but we `assert!` on it just to keep ourselves sane 
                        let first_res = try_punct(first); 
                        assert!(first_res, "first char must be a valid punctuation");
 
                        let mut size_i : u8 = 0;

                        loop { 
                            // We have a hardcoded limit on how long the longest punctuation is, 
                            // and there's no sense looking past that.  
                            // (See `LONGEST_PUNCTUATION` docs idk)
                            size_i += 1;
                            if size_i > LONGEST_PUNCTUATION { break }


                            let next = self.inner_next()?; 

                            // Now we keep repeating the process for each subsequent `Unit`.
                            // As soon as we find one that's not a valid punctuation character,
                            // `try_punct` will return `false`, so we know to break 

                            if try_punct(next) { 
                                continue
                            } else { 
                                break
                            }
                        }

                        // Now we're done looking for characters. 
                        // If we have a valid match, that's our answer.  
                        //  It gets returned, after we requeue everything that we needed to backtrack on earlier
                        // 

                        if let Some(token) = match_longest { 

                            for token in match_backtrack.into_iter() { self.peeked.push_back(token); }
                            return Ok(token)

                        } else { 
                            return Err(ParseError { 
                                kind: ParseErrorKind::UnrecognizedPunctuation(word), 
                                span: Span { 
                                    lo: Position(first_index),
                                    hi: last_position, 
                                },
                                backtrace: Backtrace::capture(),
                            })
                        }
                    }

                    else { 
                        // Here, we have not matched any of our basic character types (number, word, punctuation),
                        // so we don't know what to do.
                        // We have an error for that    
                        return Err(ParseError { 
                            kind: ParseErrorKind::UnknownCharacterType, 
                            span: Position(next.index).into(),
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
    pub(super) stream: RawTokenStream<'a>,
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
    pub fn peek_for_identifier(&mut self) -> ParseResult<Option<Spanned<String>>> { 
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