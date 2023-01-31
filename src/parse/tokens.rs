use strum_macros::EnumString;

// pub enum TokenClass { 
//     Terminal(TerminalToken), 

//     Identifier(String), 
//     Number(String), 
//     StringLiteral(String), 
// }


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
    
    // Keywords

    Type, 
}