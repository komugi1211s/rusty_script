use std::fmt;
use std::mem;

/*
 10 + 2 * 4
 expression -> (unary)calcnum operator (unary)calcnum;
 calcnum -> ;
 digit -> 0..9;
*/
#[derive(Clone, Debug)]
pub enum TokenType {
    // 予約語
    Define, // define {}
    Reveal, // reveal(foreshadow)

    // 変数定義
    Var, // var Character = ...

    // データ型
    ForeShadow, // foreshadow ABC;
    Chapter,    // chapter "chapterName" {}
    Story,      // story "storyname" {}

    // 組み込み
    If,    // if (expr) {
    Print, // print "message";

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
    
    // 計算
    Expression(&Expr),

    // 概念
    Str(String),
    Iden(String),
    Digit(f32),
    EOF,
}

#[derive(Debug)]
pub enum ErrorType {
    UnexpectedToken(char),
    ReservedWord,
    UnterminatedString(String),
}

#[derive(Debug)]
pub struct SyntaxError {
    _type: ErrorType,
    line: usize,
    col: usize,
}

impl SyntaxError {
    fn new(_type: ErrorType, line: usize, col: usize) -> Self {
        Self {
            _type,
            line,
            col,
        }
    }
}

impl fmt::Display for SyntaxError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?} ({}行目 {}文字目)", self._type, self.line, self.col)
    }
}

#[derive(Clone)]
pub struct Token {
    tokentype: TokenType,
    line: usize,
}

impl Token {
    fn new(tokentype: TokenType, line: usize) -> Self {
        Token {
            tokentype,
            line,
        }
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
    column: usize,
}

impl CodeScanner {
    pub fn new(token: &str) -> Self {
        CodeScanner {
            source: token.to_string(),
            tokens: Vec::new(),
            start: 0,
            current: 0,
            line: 1,
            column: 1,
        }
    }

    pub fn scan(&mut self) -> Result<&Vec<Token>, SyntaxError> {
        let vectors = self.source.chars().collect::<Vec<char>>();
        let chars: &[char] = &vectors.as_slice();
        println!("&vectors: {}", mem::size_of_val(&vectors));
        println!("&[char]: {}", mem::size_of_val(chars));


        println!("追跡を開始します。");
        println!("文字列の長さ: {}", self.source.len());
        while !self.is_at_end() {
            self.start = self.current;
            self.scan_next_token(chars)?;
        }

        self.tokens.push(Token::new(TokenType::EOF, self.line));
        Ok(&self.tokens)
    }

    fn scan_next_token(&mut self, chars: &[char]) -> Result<(), SyntaxError> {
        println!("scan &[char]: {}", mem::size_of_val(chars));
        let c: char = self.advance(chars);
        match c {
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
            }

            // スペース、特殊文字は全て無視
            ' ' | '\r' | '\t' => Ok(()),

            // 改行
            '\n' => {
                self.line += 1;
                self.column = 1;
                Ok(())
            }

            // 文字列を追加
            '"' => self.add_string(chars),

            // 数字全般を単発で判定
            '0'..='9' => self.add_digit(chars),

            'A'..='z' => self.add_possible_iden(chars),

            // Default
            def => {
                Err(SyntaxError::new(
                    ErrorType::UnexpectedToken(def),
                    self.line,
                    self.column,
                ))
            }
        }
    }

    fn add_string(&mut self, chars: &[char]) -> Result<(), SyntaxError> {
        while !self.is_at_end() && chars[self.current] != '"' {
            if chars[self.current] == '\n' {
                self.line += 1;
                self.column = 1;
            }
            self.advance(chars);
        }

        // Unterminated String
        if self.is_at_end() {
            let given_string: String = (&chars[(self.start + 1)..(self.current - 1)])
                .iter()
                .collect();
            return Err(SyntaxError::new(
                ErrorType::UnterminatedString(given_string),
                self.line,
                self.column,
            ));
        }

        self.advance(chars);
        let given_string: &[char] = &chars[(self.start + 1)..(self.current - 1)];
        let given_string: String = given_string.iter().collect();

        self.add_token(TokenType::Str(given_string))
    }

    fn add_digit(&mut self, chars: &[char]) -> Result<(), SyntaxError> {
        // FIXME: How to manage both is_at_end && check_if_digit
        while !self.is_at_end() {
            match chars[self.current] {
                '0'..='9' => self.advance(chars),
                '.' => {
                    if let '0'..='9' = chars[self.current + 1] {
                        self.advance(chars)
                    } else {
                        break;
                    }
                }
                _ => break,
            };
        }

        let digits: f32 = {
            let stri: String = chars[self.start..self.current].iter().collect();
            stri.parse().unwrap()
        };
        self.add_token(TokenType::Digit(digits))
    }

    fn add_possible_iden(&mut self, chars: &[char]) -> Result<(), SyntaxError> {
        Ok(())
    }

    fn advance(&mut self, chars: &[char]) -> char {
        self.current += 1;
        self.column += 1;
        chars[self.current - 1]
    }

    fn add_token(&mut self, tokentype: TokenType) -> Result<(), SyntaxError> {
        self.tokens.push(Token::new(tokentype, self.line));
        Ok(())
    }

    fn is_at_end(&self) -> bool {
        self.source.len() <= self.current
    }
}
