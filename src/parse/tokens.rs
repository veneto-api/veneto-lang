use std::{str::FromStr, backtrace::Backtrace};
use strum_macros::EnumString;

use super::{ParseResult, ParseError, ParseErrorKind};
use super::lexer::Span; 

// lol dunno if we have to convert these yet 
pub type Number = String; 

#[allow(clippy::upper_case_acronyms)]
#[derive(PartialEq, Eq, Debug, Clone)]
pub enum TokenKind { 
    /// This represents alphanumeric characters, dashes, and underscores.
    /// It could be a `Keyword`, or an identifier.
    /// 
    /// We discourage pattern matching against this directly,
    /// as the logic that denotes an identifier could change.
    /// Use helpers like `as_identifier` and `as_keyword` instead
    Word(String), 

    Number(Number), 
    Punctuation(Punctuation), 
    StringLiteral(String), 
    EOF, 
}

#[derive(Clone, Debug)]
pub struct Token { 
    pub kind: TokenKind, 
    pub span: Span, 
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Identifier { 
    pub text: String, 
    pub span: Span, 
}


impl Token {

    /// Returns the inner `Punctuation` if this token is punctuation, otherwise `None`
    pub fn as_punctuation(&self) -> Option<Punctuation> { 
        if let TokenKind::Punctuation(op) = self.kind { 
            Some(op)
        } else { 
            None
        }
    }
    
    /// Returns the inner `Punctuation` or an Unexpected error.
    pub fn try_as_punctuation(&self) -> ParseResult<Punctuation> { 
        if let TokenKind::Punctuation(op) = self.kind { 
            Ok(op)
        } else { 
            Err(self.as_err_unexpected())
        }
    }

    /// Returns an "Expected" error if this token does not match the provided punctuation.
    pub fn expect_punctuation(&self, expected: Punctuation) -> ParseResult<()> { 
        if let TokenKind::Punctuation(op) = self.kind { 
            if op == expected { 
                return Ok(())
            }
        } 

        Err(ParseError { 
            kind: ParseErrorKind::ExpectedPunctuation(expected), 
            span: self.span,
            backtrace: Backtrace::capture(),
        })
    }

    /// Returns the `Keyword` if this token represents one, otherwise `None` 
    pub fn as_keyword(&self) -> Option<Keyword> { 
        if let TokenKind::Word(str) = self.kind.clone() { 
            Keyword::from_str(&str).ok()
        } else { 
            None
        }
    }

    /// Returns the `Keyword` this token represents, or an "Unexpected" error
    pub fn try_as_keyword(&self) -> ParseResult<Keyword> { 
        self.as_keyword().ok_or_else(|| self.as_err_unexpected())
    }

    /// Returns an "Expected" error if this token does not match the provided `Keyword`. 
    pub fn expect_keyword(&self, expected: Keyword) -> ParseResult<()> { 
        if let TokenKind::Word(str) = self.kind.clone() { 
            if Keyword::from_str(&str) == Ok(expected) { 
                return Ok(())
            }
        }
        Err(ParseError { 
            kind: ParseErrorKind::ExpectedKeyword(expected), 
            span: self.span,
            backtrace: Backtrace::capture(),
        })
    }

    /// Returns a `Terminal` sum type if this token matches an `Punctuation` or `Keyword`, otherwise `None` 
    pub fn as_terminal(&self) -> Option<Terminal> { 
        match self.kind.clone() { 
            TokenKind::Punctuation(op) => Some(Terminal::Punctuation(op)),
            TokenKind::Word(str) => Keyword::from_str(&str).ok().map(Terminal::Keyword), 
            _ => None
        }
    }


    /// Returns an "Expected" error if this token does not match the provided `Terminal`. 
    pub fn expect_terminal(&self, expected: Terminal) -> ParseResult<()> { 
        match expected { 
            Terminal::Punctuation(expected) => { 
                if matches!(self.kind, TokenKind::Punctuation(actual) if actual == expected) { 
                    Ok(())
                } else { 
                    Err(ParseError { 
                        kind: ParseErrorKind::ExpectedPunctuation(expected), 
                        span: self.span,
                        backtrace: Backtrace::capture(),
                    })
                }
            }
            Terminal::Keyword(expected) => { 
                if matches!(self.as_keyword(), Some(actual) if actual == expected) { 
                    Ok(())
                } else { 
                    Err(ParseError { 
                        kind: ParseErrorKind::ExpectedKeyword(expected), 
                        span: self.span,
                        backtrace: Backtrace::capture(),
                    })
                }

            }
        }

    }

    /// Returns `true` if this is a valid identifier.
    /// 
    /// This is preferred to checking the `TokenKind` as the rules may change
    pub fn is_identifier(&self) -> bool { 
        matches!(self.kind, TokenKind::Word(_))
    }

    /// Returns the token's value if this token is a valid identifier, otherwise `None`
    pub fn as_identifier(&self) -> Option<Identifier> { 
        if let TokenKind::Word(str) = self.kind.clone() { 
            Some(Identifier { text: str, span: self.span })
        } else { 
            None
        }
    }

    /// Returns this token value if this token is a valid identifier, otherwise an `ExpectedIdentifier` error.
    pub fn try_as_identifier(&self) -> ParseResult<Identifier> { 
        if let TokenKind::Word(str) = self.kind.clone() { 
            Ok(Identifier { text: str, span: self.span })
        } else { 
            Err(ParseError { 
                kind: ParseErrorKind::ExpectedIdentifier, 
                span: self.span,
                backtrace: Backtrace::capture(),
            })
        }
    }

    pub fn try_as_number(&self) -> ParseResult<String> { 
        if let TokenKind::Number(str) = self.kind.clone() { 
            Ok(str) 
        } else { 
            Err(ParseError { 
                kind: ParseErrorKind::ExpectedNumber, 
                span: self.span,
                backtrace: Backtrace::capture(),
            })
        }
    }

    pub fn try_as_string_literal(self) -> ParseResult<String> { 
        if let TokenKind::StringLiteral(str) = self.kind { 
            Ok(str)
        } else { 
            Err(ParseError { 
                kind: ParseErrorKind::ExpectedStringLiteral, 
                span: self.span,
                backtrace: Backtrace::capture(),
            })
        }
    }

    /// Helper method to convert a `RawToken` to an "Unexpected ____" error
    pub fn as_err_unexpected(&self) -> ParseError { 
        ParseError { 
            kind: ParseErrorKind::Unexpected(self.kind.clone()), 
            span: self.span,
            backtrace: Backtrace::capture(),
        }
    }

    pub fn as_semantic_error(&self, msg: &'static str) -> ParseError { 
        ParseError { 
            kind: ParseErrorKind::Semantic(msg.to_string()), 
            span: self.span,
            backtrace: Backtrace::capture(),
        }
    }

    pub fn as_err(&self, err: ParseErrorKind) -> ParseError { 
        ParseError { 
            kind: err, 
            span: self.span, 
            backtrace: Backtrace::capture(), 
        }
    }
}



/// These represent all valid punctuation sequences in the language;
/// punctuation words that don't match this enum should raise an `UnrecognizedPunctuation` error.
/// 
/// # ⚠️ Before adding a variant:
/// 
/// 1) If we ever add a `/` operator for any reason, 
/// that will be preempted by the comment processing in the lexer.  
/// We'd have to revisit how that works, and make sure to test that comments still work properly
/// 
/// 2) For operators longer than a few characters, make sure to update `LONGEST_PUNCTUATION`
/// or else it will be ignored.  
#[derive(PartialEq, Eq, Debug, EnumString, Clone, Copy)] 
pub enum Punctuation { 

    #[strum(serialize="{")]
    BraceOpen,
    #[strum(serialize="}")]
    BraceClose,

    #[strum(serialize="[")]
    BracketOpen, 
    #[strum(serialize="]")]
    BracketClose, 

    // ⚠️ before adding a variant, check the docs 

    #[strum(serialize=":")]
    Colon,

    #[strum(serialize=",")]
    Comma, 

    #[strum(serialize="=")]
    Assign,

    #[strum(serialize="@")]
    SpecialType,

    #[strum(serialize="&")]
    Reference, 

    #[strum(serialize="#")]
    HttpStatus,

    // ⚠️ before adding a variant, check the docs 

    #[strum(serialize="%")]
    Lax,

    #[strum(serialize="?")]
    Optional,

    #[strum(serialize="::")]
    PathSeparator, 

    #[strum(serialize="*")]
    Glob,

    // ⚠️ before adding a variant, check the docs 

    #[strum(serialize="<")]
    GenericOpen,
    #[strum(serialize=">")]
    GenericClose,

    #[strum(serialize="+")]
    StructExtension,

    #[strum(serialize="->")]
    Arrow, 


    // ⚠️ before adding a variant, check the docs 
}

/// This is the length in chars of the longest possible punctuation sequence.
/// 
/// This is used by the lexer to limit unnecessary peekaheads.  
/// This is probably not necessary, but 
/// extracting a `Punctuation` token is an *O(n log n)* operation, 
/// so it feels weird to just let it potentially go on forever 
pub const LONGEST_PUNCTUATION : u8 = 3; 

#[derive(PartialEq, Eq, Debug, EnumString, Clone, Copy)] 
#[strum(serialize_all="lowercase")]
pub enum Keyword { 

    // Top-level 
    Type, 
    Use, 
    Interface,
    Resource,
    Entry,

    As, 

    // Struct mods
    In, 
    Out, 

    // in resource classes
    Extends, 
    Data,
    Embed, 
    Links, 
    // (interface already defined) 
}

impl Keyword { 
    /// `true` if the keyword is valid at the start of a top-level sequence. 
    pub fn is_top_level(&self) -> bool { 
        matches!(self, Self::Type | Self::Use | Self::Interface | Self::Resource)
    }
}

/// A "terminal" symbol - i.e. a punctuation or a keyword.
/// 
/// `Punctuation` has to be separate from `Word` because of the different lexing rules,
/// but this enum exists for convenience in cases when the parser needs to handle both at the same time
#[derive(PartialEq, Eq, Debug)]
pub enum Terminal { 
    Punctuation(Punctuation),
    Keyword(Keyword), 
}