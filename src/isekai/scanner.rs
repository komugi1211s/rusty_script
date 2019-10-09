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
    Iden(String),
    Digit(f32),
    EOF,
}

#[derive(Debug)]
struct ErrData {
    line: i32,
    col: i32,
    text: String
}

impl fmt::Display for ErrData {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} ({}行目 {}文字目)", self.text, self.line, self.col)
    }
}

impl ErrData {
    fn new(line: i32, col: i32, text: &str) -> Self {
        Self {
            line: line,
            col: col,
            text: text.to_string()
        }
    }
}

// Mostly Syntax Error
#[derive(Debug)]
pub enum ScannerError {
    UnexpectedToken(usize, usize, String),
    ReservedWord(usize, usize, String),
    UnterminatedString(usize, usize, String),
}

// Do we need a clone?
#[derive(Clone)]
pub struct Token {
    tokentype: TokenType,
    line: usize
}

impl Token {
    fn new(tokentype: TokenType, line: usize) -> Self {
        Token {
            tokentype: tokentype,
            line: line,
        }
    }

    pub fn to_info_str(&self) -> String {
        self.to_string()
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "トークン {:?}, {}行目", self.tokentype, self.line)
    }
}

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

    pub fn scan(&mut self) -> Result<Vec<Token>, ScannerError> {
        let chars: Vec<char> = self.source.chars().collect::<Vec<char>>();

        println!("追跡を開始します。");
        println!("文字列の長さ: {}", self.source.len());
        println!("現在位置: {}", self.current);
        while !self.is_at_end() {
            self.start = self.current;
            self.scan_next_token(&chars)?;
        }

        self.tokens.push(Token::new(TokenType::EOF, self.line));
        Ok(self.tokens.clone())
    }

    fn scan_next_token(&mut self, chars: &Vec<char>) -> Result<(), ScannerError>{
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
            a @ '0' ..= '9' => self.add_digit(a),

            // Default
            _ => return Err(ScannerError::UnexpectedToken(self.current, self.line, "認知できないトークンです。".to_string()))
        };
    }

    fn add_string(&mut self, chars: &Vec<char>) -> Result<(), ScannerError> {
        while chars[self.current] != '"' && !self.is_at_end() {
            if chars[self.current] == '\n' { self.line += 1; }
            self.advance(chars);
        }

        // Unterminated String
        if self.is_at_end() {
            return Err(ScannerError::UnterminatedString(self.current, self.line, "文字列が閉じられていません。".to_string()));
        }

        self.advance(chars);
        let given_string: &[char] = &chars[(self.start + 1) .. (self.current - 1)];
        let given_string: String = given_string.iter().collect();

        self.add_token(TokenType::Str(given_string))
    }

    fn add_digit(&mut self, digit: char) -> Result<(), ScannerError> {
        Ok(())
    }

    fn advance(&mut self, chars:&Vec<char>) -> char {
        self.current += 1;
        chars[self.current -1]
    }

    fn add_token(&mut self, tokentype: TokenType) -> Result<(), ScannerError> { 
        self.tokens.push(Token::new(tokentype, self.line));
        Ok(())
    }

    fn is_at_end(&self) -> bool {
        self.source.len() <= self.current
    }
}
