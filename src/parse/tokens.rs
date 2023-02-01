use std::str::FromStr;
use strum_macros::EnumString;

use super::{ParseResult, ParseError, ParseErrorKind};



#[derive(PartialEq, Eq, Debug, Clone)]
pub enum WordKind { 
    Alpha, 
    Number,
    Punctuation, 
}

#[allow(clippy::upper_case_acronyms)]
#[derive(PartialEq, Eq, Debug, Clone)]
pub enum RawTokenKind { 
    Word(WordKind, String), 
    StringLiteral(String), 
    EOF, 
}

#[derive(Clone)]
pub struct RawToken { 
    pub kind: RawTokenKind, 
    pub position: Position, 
}


impl RawToken {
    /// Attempts to convert this token to a terminal symbol,
    /// like an operator or a language keyword.
    /// Returns an `Unexpected` error if this token does not represent a valid terminal. 
    /// 
    /// This error can be ignored by the parser if a nonterminal symbol is also valid at its current position.
    pub fn as_terminal(&self) -> ParseResult<TerminalToken> { 
        if let RawTokenKind::Word(_, ref val) = self.kind { 
            TerminalToken::from_str(val).map_err(|_| self.as_err_unexpected())
        } else { 
            Err(self.as_err_unexpected())
        }
    }

    /// Attempts to extract an identifier from this token.
    /// Returns an `Unexpected` error if this token is not a Word.
    /// 
    /// This error can be ignored by the parser if another kind of token is also valid at its current position.
    pub fn as_identifier(&self) -> ParseResult<String> { 
        if let RawTokenKind::Word(WordKind::Alpha, val) = self.kind.clone() { 
            Ok(val)
        }
        else { 
            Err(self.as_err_unexpected())
        }
    }

    pub fn expect(self, terminal: TerminalToken) -> ParseResult<()> { 
        if let RawTokenKind::Word(_, ref val) = self.kind { 
            if TerminalToken::from_str(val) == Ok(terminal) { return Ok(()) }
        }
        Err(ParseError { kind: ParseErrorKind::Expected(terminal), position: self.position })
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

pub enum TokenKind { 
    Terminal(TerminalToken), 

    Identifier(String), 
    Number(String), 
    StringLiteral(String), 
}
pub struct Token { 
    kind: TokenKind,
    position: Position,
}


#[derive(PartialEq, Eq, Debug, EnumString, Clone, Copy)] 
#[strum(serialize_all="lowercase")]
pub enum TerminalToken { 

    // Punctuation

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
    
    // Keywords

    Type, 

    Use, 

    Interface,

    Resource,

    // 
    #[strum(serialize="as")]
    PathAlias, 

    #[strum(serialize="*")]
    Glob,
}

impl TerminalToken { 
    pub fn is_top_level_keyword(&self) -> bool { 
        matches!(self, TerminalToken::Type | TerminalToken::Use | TerminalToken::Interface | TerminalToken::Resource)
    }
}