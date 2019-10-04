use std::fmt;

#[derive(Clone, Debug)]
pub enum TokenType {
    // 予約語
    Define,
    Reveal,

    // データ型
    ForeShadow,
    Plot,
    Chapter,
    Story,

    // 括弧とか文字とか
    DoubleQuote, // ""
    Slash, // /
    OpenParen, // (
    CloseParen, // )
    OpenBrace, // {
    CloseBrace, // }
    AtMark, // @
    Colon, // :
    SemiColon, // ;
    Equal, // =
    Dot, // .

    // 概念　
    Str(String),
    Int(i32),
    Iden,
    EOF
}

impl fmt::Display for TokenType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Debug::fmt(self, f)
    }
}

// Do we need a clone?
#[derive(Clone, Debug)]
pub struct Token {
    tokentype: TokenType,
    lexeme: String,
    line: usize
}

impl Token {
    fn new(tokentype: TokenType, lexeme: &str, line: usize) -> Self {
        Token {
            tokentype: tokentype,
            lexeme: lexeme.to_string(),
            line: line,
        }
    }

    pub fn to_info_str(&self) -> String {
        format!("トークン {} - \"{}\" ({}行目)", self.tokentype, self.lexeme, self.line)
    }
}

#[derive(Debug)]
pub struct CodeScanner {
    source: String,
    tokens: Vec<Token>,
    start: usize,
    current: usize,
    line: usize,
}

impl CodeScanner {
    pub fn new(token: &String) -> Self {
        CodeScanner {
            source: token.to_owned(),
            tokens: Vec::new(),
            start: 0,
            current: 0,
            line: 1,
        }
    }

    pub fn scan(&mut self) -> Result<&Vec<Token>, ()> {
        let chars: Vec<char> = self.source.chars().collect::<Vec<char>>();
        while !self.is_at_end() {
            self.start = self.current;
            self.scan_next_token(&chars)?;
        }

        self.tokens.push(Token::new(TokenType::EOF, "/eof/", self.line));
        Ok(&self.tokens)
    }

    fn scan_next_token(&mut self, chars: &Vec<char>) -> Result<(), ()>{
        let c: char = self.advance(chars);
        return match c {
            '(' => self.add_token(TokenType::OpenParen),
            ')' => self.add_token(TokenType::CloseParen),
            '{' => self.add_token(TokenType::OpenBrace),
            '}' => self.add_token(TokenType::CloseBrace),
            '@' => self.add_token(TokenType::AtMark),
            ':' => self.add_token(TokenType::Colon),
            ';' => self.add_token(TokenType::SemiColon),
            '=' => self.add_token(TokenType::Equal),
            '.' => self.add_token(TokenType::Dot),
            '/' => {
                // 次の文字列も '/' であれば、これはコメントである
                if chars[self.current] == '/' {
                    // 改行、若しくは最後の文字にぶつかるまでループ
                    while !self.is_at_end() && chars[self.current] != '\n' { 
                        self.advance(chars);
                    }
                    Ok(())
                } else {
                    // ただのスラッシュだった…
                    self.add_token(TokenType::Slash)
                }
            },

            // スペース、特殊文字は全て無視
            ' ' | '\r' | '\t' => return Ok(()),

            // 改行
            '\n' => {
                self.line += 1;
                return Ok(());
            }

            // 文字列を追加
            '"' => self.add_string(chars),

            // Default
            _ => return Err(())
        };
    }

    fn add_string(&mut self, chars: &Vec<char>) -> Result<(), ()> {
        while chars[self.current] != '"' && !self.is_at_end() {
            if chars[self.current] == '\n' { self.line += 1; }
            self.advance(chars);
        }

        // Unterminated String
        if self.is_at_end() {
            return Err(());
        }

        self.advance(chars);
        let given_string: &[char] = &chars[(self.start + 1) .. (self.current - 1)];
        let given_string: String = given_string.iter().collect();

        self.add_token(TokenType::Str(given_string))
    }

    fn advance(&mut self, chars:&Vec<char>) -> char {
        self.current += 1;
        chars[self.current -1]
    }

    fn add_token(&mut self, tokentype: TokenType) -> Result<(), ()> {

    }

    fn is_at_end(&self) -> bool {
        self.source.len() < self.current
    }
}
