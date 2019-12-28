use std::fmt;

#[derive(Clone, Debug, PartialEq)]
pub enum TokenType {
    // 予約語
    If,       // if expr {
    Else,     // else {
    While,    // while expr {
    For,      // for int: i in 0..10 {
    Break,    // break;
    Continue, // continue;

    True,   // true
    False,  // false
    Print,  // print "message"
    Return, // return "";

    // TODO - @Cleanup: Obsolete
    TypeAny,   // a: any = ...
    TypeInt,   // a: int =
    TypeFloat, // a: float =
    TypeStr,   // a: string =
    TypeBool,  // a: bool =
    TypeStruct,  // a: struct { 
    Null,      // null

    // 括弧とか文字とか
    DoubleQuote, // ""
    Slash,       // / <- 計算でも使う
    OpenParen,   // (
    CloseParen,  // )
    OpenBrace,   // {
    CloseBrace,  // }
    OpenSquareBracket,    // [
    CloseSquareBracket,   // ]
    AtMark,      // @
    Colon,       // :
    SemiColon,   // ;
    Equal,       // =
    Dot,         // .
    Comma,       // ,
    Question,    // ?

    // 比較, 論理
    Or,         // or
    And,        // and
    Bang,       // !
    Less,       // <
    More,       // >
    NotEqual,   // !=
    LessEqual,  // <=
    MoreEqual,  // >=
    EqualEqual, // ==

    // 計算用
    Plus,     // +
    Minus,    // -
    Asterisk, // *
    // Slash, <- 既に上の方で定義済み
    Percent, // %

    // 概念 ( 基本Lexedじゃないと駄目 )
    Str,
    Iden { is_reserved: bool },
    Digit { has_decimal_point: bool },
    EOF,
}

impl TokenType {
    // TODO - @Cleanup: Obsolete
    pub fn is_typekind(given: &TokenType) -> bool {
        match given {
            TokenType::TypeAny => true,
            TokenType::TypeInt => true,
            TokenType::TypeFloat => true,
            TokenType::TypeBool => true,
            TokenType::TypeStr => true,
            _ => false,
        }
    }
}

pub fn match_identity(keywords: &str) -> Option<TokenType> {
    use TokenType::*;
    match keywords {
        "if" => Some(If),
        "else" => Some(Else),
        "while" => Some(While),
        "for" => Some(For),
        "print" => Some(Print),
        "break" => Some(Break),
        "continue" => Some(Continue),
        "return" => Some(Return),

        "true" => Some(True),
        "false" => Some(False),
        "null" => Some(Null),

        // TODO - @Cleanup: Obsolete, These are an identity
        "any" | "ANY" => Some(TypeAny),
        "int" | "INT" => Some(TypeInt),
        "float" | "FLOAT" => Some(TypeFloat),
        "string" | "STRING" => Some(TypeStr),
        "bool" | "BOOL" => Some(TypeBool),
        "struct" => Some(TypeStruct),

        "and" => Some(And),
        "or" => Some(Or),
        _ => None,
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub tokentype: TokenType,
    // TODO - @Improvement: Replace it with trace::CodeSpan
    pub line: usize,
    pub lexeme: Option<String>,
}

impl Token {
    pub fn simple(tokentype: TokenType, line: usize) -> Self {
        Token {
            tokentype,
            line,
            lexeme: None,
        }
    }
    pub fn lexed(tokentype: TokenType, line: usize, lexeme: String) -> Self {
        Token {
            tokentype,
            line,
            lexeme: Some(lexeme),
        }
    }

    pub fn is_simple(&self) -> bool {
        self.lexeme.is_none()
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "トークン {:?}, {}行目", self.tokentype, self.line)
    }
}
