use strum_macros::EnumString;

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