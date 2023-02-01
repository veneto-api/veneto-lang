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

pub struct RawToken { 
    pub kind: RawTokenKind, 
    pub position: Position, 
}


impl RawToken {
    pub fn as_terminal(&self) -> ParseResult<TerminalToken> { 
        if let RawTokenKind::Word(_, ref val) = self.kind { 
            TerminalToken::from_str(val).map_err(|_| ParseError { 
                kind: ParseErrorKind::Unexpected(self.kind.clone()), 
                position: self.position,
            })
        } else { 
            Err(ParseError { 
                kind: ParseErrorKind::Unexpected(self.kind.clone()), 
                position: self.position
            })
        }
    }

    pub fn as_identifier(&self) -> ParseResult<String> { 
        if let RawTokenKind::Word(WordKind::Alpha, val) = self.kind.clone() { 
            Ok(val)
        }
        else { 
            Err(ParseError { 
                kind: ParseErrorKind::Unexpected(self.kind.clone()), 
                position: self.position,
            })
        }
    }

    pub fn expect(self, terminal: TerminalToken) -> ParseResult<()> { 
        if let RawTokenKind::Word(_, ref val) = self.kind { 
            if TerminalToken::from_str(val) == Ok(terminal) { return Ok(()) }
        }
        Err(ParseError { kind: ParseErrorKind::Expected(terminal), position: self.position })
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
    Ampersat,

    #[strum(serialize="#")]
    HttpStatus,

    #[strum(serialize="%")]
    Lax,
    
    // Keywords

    Type, 
}