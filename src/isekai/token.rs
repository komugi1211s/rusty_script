use std::fmt;

#[derive(Clone, Debug, PartialEq)]
pub enum TokenType {
    // 予約語
    Define, // define {}
    Reveal, // reveal(foreshadow)
    If,     // if expr {
    Else,   // else {
    True,   // true
    False,  // false
    Print,  // print "message"
    Var,    // TYPE name = ...

    // データ型
    ForeShadow, // foreshadow ABC;
    Chapter,    // chapter "chapterName" {}
    Story,      // story "storyname" {}

    // 括弧とか文字とか
    DoubleQuote, // ""
    Slash,       // /
    OpenParen,   // (
    CloseParen,  // )
    OpenBrace,   // {
    CloseBrace,  // }
    AtMark,      // @
    Colon,       // :
    SemiColon,   // ;
    Equal,       // =
    Dot,         // .
    Comma,       // ,

    // 比較
    Bang,        // !
    Less,        // <
    More,        // >
    NotEqual,    // !=
    LessEqual,   // <=
    MoreEqual,   // >= 
    EqualEqual,  // == 

    // 計算用
    Plus,     // +
    Minus,    // -
    Asterisk, // *
    Percent,  // %
    
    // 概念
    Str(String),
    Iden(String),
    Digit(f64),
    EOF,
}

pub fn match_identity(keywords: &str) -> Option<TokenType>
{
    use TokenType::*;
    match keywords
    {
        "define" => Some(Define),
        "reveal" => Some(Reveal),
        "if"     => Some(If),
        "else"   => Some(Else),
        "true"   => Some(True),
        "false"  => Some(False),
        "print"  => Some(Print),
        "var"    => Some(Var),
        _ => None,
    }
}

#[derive(Debug, Clone)]
pub struct Token {
    pub tokentype: TokenType,
    pub line: usize,
    pub lexeme: Vec<char>,
}

impl Token {
    pub fn new(tokentype: TokenType, line: usize, lexeme: &[char]) -> Self {
        let lexeme = lexeme.to_owned();
        Token {
            tokentype,
            line,
            lexeme,
        }
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "トークン {:?}, {}行目", self.tokentype, self.line)
    }
}
