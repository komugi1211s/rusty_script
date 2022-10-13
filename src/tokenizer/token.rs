use std::fmt;
use crate::trace::prelude::*;

#[allow(dead_code)]
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum TokenType
{
    // 予約語
    If,       // if expr {
    Else,     // else {
    While,    // while expr {
    Break,    // break;
    Continue, // continue;

    Print,    // print "message"       // TODO Remove
    Return,   // return "";
    Use,      // use element = "something";

    Struct,   // new_type: struct {
    Fn,   // new_type: fn(...

    // 括弧とか文字とか
    DoubleQuote,        // ""
    Slash,              // / <- 計算でも使う
    OpenParen,          // (
    CloseParen,         // )
    OpenBrace,          // {
    CloseBrace,         // }
    OpenSquareBracket,  // [
    CloseSquareBracket, // ]
    AtMark,             // @
    Colon,              // :
    SemiColon,          // ;
    Equal,              // =
    Dot,                // .
    Comma,              // ,
    Question,           // ?
    Caret,              // ^

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
    Iden,
    Digit,
    EOF,
}

pub fn match_identity(keywords: &str) -> Option<TokenType>
{
    use TokenType::*;
    match keywords
    {
        "if" => Some(If),
        "else" => Some(Else),
        "while" => Some(While),
        "print" => Some(Print),
        "break" => Some(Break),
        "continue" => Some(Continue),
        "return" => Some(Return),
        "fn"  => Some(Fn),
        "use" => Some(Use),
        "struct" => Some(Struct),

        "and" => Some(And),
        "or" => Some(Or),
        _ => None,
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Token<'m>
{
    pub file: &'m SourceFile,
    pub tokentype: TokenType,
    pub span: CodeSpan,
    pub lexeme: Option<String>,
}

impl<'m> Token<'m>
{
    pub fn simple(file: &'m SourceFile, tokentype: TokenType, span: CodeSpan) -> Self
    {
        Token {
            file,
            tokentype,
            span,
            lexeme: None,
        }
    }
    pub fn lexed(file: &'m SourceFile, tokentype: TokenType, span: CodeSpan, lexeme: String)
        -> Self
    {
        Token {
            file,
            tokentype,
            span,
            lexeme: Some(lexeme),
        }
    }

    pub fn report(&self, title: &str, message: &str)
    {
        report(title, message);
        spit_line(self.file, &self.span);
    }
}

impl fmt::Display for Token<'_>
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result
    {
        write!(
            f,
            "トークン {:?}, {}行目",
            self.tokentype, self.span.row_start
        )
    }
}
