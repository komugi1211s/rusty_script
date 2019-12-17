use std::fmt;
// use std::mem;
use super::token::{match_identity, Token, TokenType};
use super::report::Error;

/*
 10 + 2 * 4
 expression -> (unary)calcnum operator (unary)calcnum;
 calcnum -> ;
 digit -> 0..9;
*/

pub struct Tokenizer {
    source: Vec<char>,
    tokens: Vec<Token>,
    start: usize,
    current: usize,
    line: usize,
    column: usize,
}

impl Tokenizer {
    pub fn new(token: &str) -> Self {
        Tokenizer {
            source: token.chars().collect::<Vec<char>>(),
            tokens: Vec::new(),
            start: 0,
            current: 0,
            line: 1,
            column: 1,
        }
    }

    pub fn scan(&mut self) -> Result<Vec<Token>, Error> {
        while !self.is_at_end() {
            self.start = self.current;
            self.scan_next_token()?;
        }

        self.tokens
            .push(Token::new(TokenType::EOF, self.line, String::new()));
        Ok(self.tokens.to_owned())
    }

    fn scan_next_token(&mut self) -> Result<(), Error> {
        let c: char = self.advance();
        match c {
            '(' => self.add_token(TokenType::OpenParen),
            ')' => self.add_token(TokenType::CloseParen),
            '{' => self.add_token(TokenType::OpenBrace),
            '}' => self.add_token(TokenType::CloseBrace),
            '@' => self.add_token(TokenType::AtMark),
            ':' => self.add_token(TokenType::Colon),
            ';' => self.add_token(TokenType::SemiColon),
            '.' => self.add_token(TokenType::Dot),
            ',' => self.add_token(TokenType::Comma),
            '+' => self.add_token(TokenType::Plus),
            '-' => self.add_token(TokenType::Minus),
            '*' => self.add_token(TokenType::Asterisk),
            '%' => self.add_token(TokenType::Percent),
            '?' => self.add_token(TokenType::Question),
            '=' => match self.next_is('=') {
                true => self.add_token(TokenType::EqualEqual),
                false => self.add_token(TokenType::Equal),
            },
            '>' => match self.next_is('=') {
                true => self.add_token(TokenType::MoreEqual),
                false => self.add_token(TokenType::More),
            },
            '<' => match self.next_is('=') {
                true => self.add_token(TokenType::LessEqual),
                false => self.add_token(TokenType::Less),
            },
            '!' => match self.next_is('=') {
                true => self.add_token(TokenType::NotEqual),
                false => self.add_token(TokenType::Bang),
            },
            '/' => {
                match self.peek() {
                    // 次の文字列も '/' であれば、これはコメントである
                    '/' => {
                        while self.peek() != '\n' {
                            self.advance();
                        }
                        Ok(())
                    }

                    // 次の文字列がアスタリスクならブロックコメント
                    '*' => {
                        while !(self.peek() == '*' && self.peek_shifted_to(1) == '/') {
                            if self.peek() == '\n' {
                                self.add_newline();
                            }
                            if self.peek() == '\0' {
                                break;
                            }
                            self.advance();
                        }

                        // 最後の */ を食いつぶして終了
                        self.advance();
                        self.advance();
                        Ok(())
                    }

                    _ => self.add_token(TokenType::Slash),
                }
            }

            // スペース、特殊文字は全て無視
            ' ' | '\r' | '\t' => Ok(()),

            // 改行
            '\n' => self.add_newline(),

            // 文字列を追加
            '"' => self.add_string(),

            // 数字全般を単発で判定
            '0'..='9' => self.add_digit(),

            'A'..='z' => self.add_possible_iden(),

            // Default
            def => Err(Error::new_while_tokenizing(
                format!("Unexpected Token: {}", def).as_str(),
                self.line,
            )),
        }
    }

    fn next_is(&mut self, expect: char) -> bool {
        if self.peek() != expect {
            return false;
        }

        self.advance();
        true
    }

    fn add_newline(&mut self) -> Result<(), Error> {
        self.line += 1;
        self.column = 1;
        Ok(())
    }

    fn add_string(&mut self) -> Result<(), Error> {
        while !self.is_at_end() && self.peek() != '"' {
            if self.source[self.current] == '\n' {
                self.add_newline()?;
            }
            self.advance();
        }

        // Unterminated String
        if self.is_at_end() {
            let given_string: String = (&self.source[(self.start + 1)..(self.current - 1)])
                .iter()
                .collect();
            return Err(Error::new_while_tokenizing(
                "Unterminated String",
                self.line,
            ));
        }

        self.advance();
        // start + 1 & current - 1 は "" ←これを削る
        self.start += 1;
        self.current -= 1;
        let z = self.add_token(TokenType::Str);
        // もとに戻す
        self.start -= 1;
        self.current += 1;

        // 結果をリターン
        z
    }

    fn peek(&self) -> char {
        if self.is_at_end() {
            '\0'
        } else {
            self.source[self.current]
        }
    }

    fn peek_shifted_to(&self, shift: usize) -> char {
        if self.is_at_end() {
            '\0'
        } else if self.source.len() <= self.current + shift {
            '\0'
        } else {
            self.source[self.current + shift]
        }
    }

    fn add_digit(&mut self) -> Result<(), Error> {
        // Advance while it's numeric
        loop {
            let n = self.peek();
            if n.is_ascii_digit() {
                self.advance();
            } else {
                let next_char = self.peek_shifted_to(1);
                if n == '.' && (next_char == ';' || next_char.is_ascii_digit()) {
                    self.advance();
                } else {
                    break;
                }
            }
        }

        self.add_token(TokenType::Digit)
    }

    fn add_possible_iden(&mut self) -> Result<(), Error> {
        while self.peek().is_ascii_alphanumeric() || self.peek() == '_' {
            self.advance();
        }
        let stri: String = self.source[self.start..self.current].iter().collect();

        let possible_result = match_identity(&stri);
        if possible_result.is_none() {
            self.add_token(TokenType::Iden)
        } else {
            self.add_token(possible_result.unwrap())
        }
    }

    fn advance(&mut self) -> char {
        self.current += 1;
        self.column += 1;
        self.source[self.current - 1]
    }

    fn add_token(&mut self, tokentype: TokenType) -> Result<(), Error> {
        self.tokens.push(Token::new(
            tokentype,
            self.line,
            self.source[self.start..self.current].iter().collect(),
        ));
        Ok(())
    }

    fn is_at_end(&self) -> bool {
        self.source.len() <= self.current
    }
}
