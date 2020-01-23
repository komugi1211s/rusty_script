
// use std::mem;
pub mod token;
use token::{match_identity, Token, TokenType};
use trace::{ position::CodeSpan, Error, source::Module };

pub struct Tokenizer {
    source: Vec<char>,
    tokens: Vec<Token>,
    start: usize,
    current: usize,
    start_line: usize,
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
            start_line: 0,
            line: 1,
            column: 1,
        }
    }

    pub fn scan(&mut self) -> Result<Vec<Token>, Error> {
        while !self.is_at_end() {
            self.start = self.current;
            self.start_line = self.line;
            self.scan_next_token()?;
        }

        self.tokens
            .push(Token::simple(TokenType::EOF, self.line, self.line));
        Ok(self.tokens.to_owned())
    }

    fn scan_next_token(&mut self) -> Result<(), Error> {
        let c: char = self.advance();
        match c {
            '(' => self.add_simple(TokenType::OpenParen),
            ')' => self.add_simple(TokenType::CloseParen),
            '{' => self.add_simple(TokenType::OpenBrace),
            '}' => self.add_simple(TokenType::CloseBrace),
            '[' => self.add_simple(TokenType::OpenSquareBracket),
            ']' => self.add_simple(TokenType::CloseSquareBracket),
            '@' => self.add_simple(TokenType::AtMark),
            ':' => self.add_simple(TokenType::Colon),
            ';' => self.add_simple(TokenType::SemiColon),
            '.' => self.add_simple(TokenType::Dot),
            ',' => self.add_simple(TokenType::Comma),
            '+' => self.add_simple(TokenType::Plus),
            '-' => self.add_simple(TokenType::Minus),
            '*' => self.add_simple(TokenType::Asterisk),
            '%' => self.add_simple(TokenType::Percent),
            '?' => self.add_simple(TokenType::Question),
            '^' => self.add_simple(TokenType::Caret),
            '=' => match self.next_is('=') {
                true => self.add_simple(TokenType::EqualEqual),
                false => self.add_simple(TokenType::Equal),
            },
            '>' => match self.next_is('=') {
                true => self.add_simple(TokenType::MoreEqual),
                false => self.add_simple(TokenType::More),
            },
            '<' => match self.next_is('=') {
                true => self.add_simple(TokenType::LessEqual),
                false => self.add_simple(TokenType::Less),
            },
            '!' => match self.next_is('=') {
                true => self.add_simple(TokenType::NotEqual),
                false => self.add_simple(TokenType::Bang),
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

                    _ => self.add_simple(TokenType::Slash),
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
                CodeSpan::new(self.start_line, self.line),
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
            let _given_string: String = (&self.source[(self.start + 1)..(self.current - 1)])
                .iter()
                .collect();
            return Err(Error::new_while_tokenizing(
                "Unterminated String",
                CodeSpan::new(self.start_line, self.line),
            ));
        }

        self.advance();
        // start + 1 & current - 1 は "" ←これを削る
        self.start += 1;
        self.current -= 1;
        let z = self.add_lexed(TokenType::Str);
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

        self.add_lexed(TokenType::Digit)
    }

    fn add_possible_iden(&mut self) -> Result<(), Error> {
        while self.peek().is_ascii_alphanumeric() || self.peek() == '_' {
            self.advance();
        }
        let stri: String = self.source[self.start..self.current].iter().collect();

        let possible_result = match_identity(&stri);
        if possible_result.is_none() {
            self.add_lexed(TokenType::Iden)
        } else {
            self.add_simple(possible_result.unwrap())
        }
    }

    fn advance(&mut self) -> char {
        self.current += 1;
        self.column += 1;
        self.source[self.current - 1]
    }

    fn add_simple(&mut self, tokentype: TokenType) -> Result<(), Error> {
        self.tokens
            .push(Token::simple(tokentype, self.start_line, self.line));
        Ok(())
    }

    fn add_lexed(&mut self, tokentype: TokenType) -> Result<(), Error> {
        let string: String = self.source[self.start..self.current].iter().collect();

        self.tokens
            .push(Token::lexed(tokentype, self.start_line, self.line, string));
        Ok(())
    }

    fn is_at_end(&self) -> bool {
        self.source.len() <= self.current
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn assert_lexed_token(token: &Token, requiretype: TokenType, requirelex: &str) {
        assert_eq!(token.tokentype, requiretype);
        assert!(token.lexeme.is_some());
        assert_eq!(token.lexeme.as_ref().unwrap().as_str(), requirelex);
    }

    fn assert_simple_token(token: &Token, requiretype: TokenType) {
        assert_eq!(token.tokentype, requiretype);
        assert!(token.lexeme.is_none());
    }

    #[test]
    fn tokenizer_arithmetic() {
        let code = "10 + 2 - 5.2 * 10 / 12 % 9";
        let result = Tokenizer::new(code).scan();

        assert!(result.is_ok());
        let tokens = result.unwrap();

        assert_eq!(tokens.len(), 10);

        assert_lexed_token(&tokens[0], TokenType::Digit, "10");
        assert_simple_token(&tokens[1], TokenType::Plus);
        assert_lexed_token(&tokens[0], TokenType::Digit, "2");
        assert_simple_token(&tokens[0], TokenType::Minus);
        assert_lexed_token(&tokens[0], TokenType::Digit, "5.2");
        assert_simple_token(&tokens[0], TokenType::Asterisk);
        assert_lexed_token(&tokens[0], TokenType::Digit, "10");
        assert_simple_token(&tokens[0], TokenType::Slash);
        assert_lexed_token(&tokens[0], TokenType::Digit, "12");
        assert_simple_token(&tokens[0], TokenType::Percent);
        assert_lexed_token(&tokens[0], TokenType::Digit, "9");
    }

    #[test]
    fn tokenize_string() {
        let correct = r#" "Hello World." "#;

        let correct_result = Tokenizer::new(correct).scan();
        assert!(correct_result.is_ok());

        let correct_vec = correct_result.unwrap();
        assert_eq!(correct_vec.len(), 2);
        assert_lexed_token(&correct_vec[0], TokenType::Str, "Hello World.");
    }

    #[test]
    fn ignore_comments() {
        let only_comment = r"// Oh Hey mark.";
        let result = Tokenizer::new(only_comment).scan().unwrap();
        assert_eq!(result.len(), 1);
    }

    #[test]
    fn ignore_block_comments() {
        let only_comment = r"/* Hello there */ /* general reposti */";
        let result = Tokenizer::new(only_comment).scan().unwrap();
        assert_eq!(result.len(), 1);
    }

    #[test]
    fn reject_unterminated_string() {
        let invalid = r#" "This string is unterminated "#;
        let invalid_result = Tokenizer::new(invalid).scan();
        assert!(invalid_result.is_err());
    }

    #[test]
    fn reject_unknown_char() {
        let unknown = "🌔";
        let invalid_result = Tokenizer::new(unknown).scan();
        assert!(invalid_result.is_err());
    }

    #[test]
    fn different_line_break() {
        let windows_style = "100\r\n";
        let unix_style = "100\n";
        let windows_result = Tokenizer::new(windows_style).scan();
        let unix_result = Tokenizer::new(unix_style).scan();
        assert!(windows_result.is_ok());
        assert!(unix_result.is_ok());

        let win_vec = windows_result.unwrap();
        let unix_vec = unix_result.unwrap();
        assert_eq!(win_vec[0], unix_vec[0]);
    }
}
