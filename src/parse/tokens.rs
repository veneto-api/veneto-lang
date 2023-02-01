use std::str::FromStr;
use strum_macros::EnumString;

use super::{ParseResult, ParseError, ParseErrorKind};



#[allow(clippy::upper_case_acronyms)]
#[derive(PartialEq, Eq, Debug, Clone)]
pub enum TokenKind { 
    Word(String), 
    Number(String), 
    Punctuation(Punctuation), 
    StringLiteral(String), 
    EOF, 
}

#[derive(Clone)]
pub struct Token { 
    pub kind: TokenKind, 
    pub position: Position, 
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

        Err(ParseError { kind: ParseErrorKind::ExpectedPunctuation(expected), position: self.position })
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
        Err(ParseError { kind: ParseErrorKind::ExpectedKeyword(expected), position: self.position })
    }

    /// Returns a `Terminal` sum type if this token matches an `Punctuation` or `Keyword`, otherwise `None` 
    pub fn as_terminal(&self) -> Option<Terminal> { 
        match self.kind.clone() { 
            TokenKind::Punctuation(op) => Some(Terminal::Punctuation(op)),
            TokenKind::Word(str) => Keyword::from_str(&str).ok().map(Terminal::Keyword), 
            _ => None
        }
    }

    /// Returns the token's value if this token is a valid identifier, otherwise `None`
    pub fn as_identifier(&self) -> Option<String> { 
        if let TokenKind::Word(str) = self.kind.clone() { 
            Some(str)
        } else { 
            None
        }
    }

    /// Returns this token value if this token is a valid identifier, otherwise an `ExpectedIdentifier` error.
    pub fn try_as_identifier(&self) -> ParseResult<String> { 
        if let TokenKind::Word(str) = self.kind.clone() { 
            Ok(str)
        } else { 
            Err(ParseError { kind: ParseErrorKind::ExpectedIdentifier, position: self.position })
        }
    }

    pub fn try_as_number(&self) -> ParseResult<String> { 
        if let TokenKind::Number(str) = self.kind.clone() { 
            Ok(str) 
        } else { 
            Err(ParseError { kind: ParseErrorKind::ExpectedNumber, position: self.position })
        }
    }

    /// Helper method to convert a `RawToken` to an "Unexpected ____" error
    pub fn as_err_unexpected(&self) -> ParseError { 
        ParseError { kind: ParseErrorKind::Unexpected(self.kind.clone()), position: self.position }
    }
}


/// A position in source code
#[derive(Clone, Copy, Debug)]
pub struct Position { 
    pub line:   u32,
    pub col:    u32, 
}


/// These represent all valid punctuation sequences in the language;
/// punctuation words that don't match this enum should raise an `UnrecognizedPunctuation` error.
#[derive(PartialEq, Eq, Debug, EnumString, Clone, Copy)] 
pub enum Punctuation { 

    #[strum(serialize="{")]
    BraceOpen,

    #[strum(serialize="}")]
    BraceClose,

    #[strum(serialize=":")]
    Colon,

    #[strum(serialize=",")]
    Comma, 

    #[strum(serialize="=")]
    Assign,

    #[strum(serialize="@")]
    SpecialType,

    #[strum(serialize="#")]
    HttpStatus,

    #[strum(serialize="%")]
    Lax,

    #[strum(serialize="::")]
    PathSeparator, 

    #[strum(serialize="*")]
    Glob,

    // Also of note, if we ever add a slash `/` operator for any reason,
    // we should update the tests to make sure that comment handling works properly
    // (that a single slash doesn't start a comment, etc)
}

#[derive(PartialEq, Eq, Debug, EnumString, Clone, Copy)] 
#[strum(serialize_all="lowercase")]
pub enum Keyword { 

    Type, 

    Use, 

    Interface,

    Resource,

    #[strum(serialize="as")]
    PathAlias, 

}

impl Keyword { 
    /// `true` if the keyword is valid at the start of a top-level sequence. 
    pub fn is_top_level(&self) -> bool { 
        matches!(self, Self::Type | Self::Use | Self::Interface | Self::Resource)
    }
}

/// A "terminal" symbol - i.e. a punctuation or a keyword.
/// 
/// I'm abusing this term a bit here, but the idea is that these are a literally-defined token,
/// rather than something variable like a `Number` or an `Identifier` 
pub enum Terminal { 
    Punctuation(Punctuation),
    Keyword(Keyword), 
}