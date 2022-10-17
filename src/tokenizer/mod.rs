// use std::mem;
pub mod token;
use token::{match_identity, Token, TokenType};
use super::trace::prelude::*;

pub struct Tokenizer
{
    id: FileId,
    source: Vec<char>,
    tokens: Vec<Token>,
    start: usize,
    current: usize,

    start_line: usize,
    current_line: usize,
    start_column: usize,
    current_column: usize,
}

type TResult = KaiResult<()>;

impl Tokenizer
{
    pub fn new(reserve_length: usize) -> Self
    {
        Tokenizer {
            id: FileId(0),
            source: Vec::with_capacity(reserve_length),
            tokens: Vec::with_capacity(reserve_length),
            start: 0,
            current: 0,

            start_line: 1,
            current_line: 1,

            start_column: 1,
            current_column: 1,
        }
    }

    pub fn set_start_index(&mut self)
    {
        self.start = self.current;
        self.start_line = self.current_line;
        self.start_column = self.current_column;
    }

    pub fn scan(&mut self, modu: &SourceFile) -> KaiResult<Vec<Token>>
    {
        self.id = modu.id;
        self.source.extend(modu.code.chars());
        while !self.is_at_end()
        {
            self.set_start_index();
            self.scan_next_token()?;
        }

        let span = self.create_current_span();
        self.tokens.push(Token::simple(TokenType::EOF, span));
        Ok(self.tokens.to_owned())
    }

    fn scan_next_token(&mut self) -> TResult
    {
        let c: char = self.advance();
        match c
        {
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
            '=' => match self.next_is('=')
            {
                true => self.add_simple(TokenType::EqualEqual),
                false => self.add_simple(TokenType::Equal),
            },
            '>' => match self.next_is('=')
            {
                true => self.add_simple(TokenType::MoreEqual),
                false => self.add_simple(TokenType::More),
            },
            '<' => match self.next_is('=')
            {
                true => self.add_simple(TokenType::LessEqual),
                false => self.add_simple(TokenType::Less),
            },
            '!' => match self.next_is('=')
            {
                true => self.add_simple(TokenType::NotEqual),
                false => self.add_simple(TokenType::Bang),
            },
            '/' =>
            {
                match self.peek()
                {
                    // 次の文字列も '/' であれば、これはコメントである
                    '/' =>
                    {
                        while self.peek() != '\n'
                        {
                            self.advance();
                        }
                        Ok(())
                    }

                    // 次の文字列がアスタリスクならブロックコメント
                    '*' =>
                    {
                        while !(self.peek() == '*' && self.peek_shifted_to(1) == '/')
                        {
                            if self.peek() == '\n'
                            {
                                self.add_newline()?;
                            }
                            if self.peek() == '\0'
                            {
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
            def =>
            {
                let span = self.create_current_span();

                Err(KaiError {
                    title: String::from("Unknown Token"),
                    message: format!("未知のトークン {} を発見しました。", def),
                    span
                })
            }
        }
    }

    fn next_is(&mut self, expect: char) -> bool
    {
        if self.peek() != expect
        {
            return false;
        }

        self.advance();
        true
    }

    fn add_newline(&mut self) -> TResult
    {
        self.current_line += 1;
        self.current_column = 1;
        Ok(())
    }

    fn add_string(&mut self) -> TResult
    {
        while !self.is_at_end() && self.peek() != '"'
        {
            if self.source[self.current] == '\n'
            {
                self.add_newline()?;
            }
            self.advance();
        }

        // Unterminated String
        if self.is_at_end()
        {
            return Err(KaiError {
                title: "Unterminated String".to_owned(),
                message: "\n文字列が閉じられていません。\n".to_owned(),
                span: self.create_current_span()
            });
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

    fn peek(&self) -> char
    {
        if self.is_at_end()
        {
            '\0'
        }
        else
        {
            self.source[self.current]
        }
    }

    fn peek_shifted_to(&self, shift: usize) -> char
    {
        if self.is_at_end()
        {
            '\0'
        }
        else if self.source.len() <= self.current + shift
        {
            '\0'
        }
        else
        {
            self.source[self.current + shift]
        }
    }

    fn add_digit(&mut self) -> TResult
    {
        // Advance while it's numeric
        loop
        {
            let n = self.peek();
            if n.is_ascii_digit()
            {
                self.advance();
            }
            else
            {
                let next_char = self.peek_shifted_to(1);
                if n == '.' && (next_char == ';' || next_char.is_ascii_digit())
                {
                    self.advance();
                }
                else
                {
                    break;
                }
            }
        }

        self.add_lexed(TokenType::Digit)
    }

    fn add_possible_iden(&mut self) -> TResult
    {
        while self.peek().is_ascii_alphanumeric() || self.peek() == '_'
        {
            self.advance();
        }
        let stri: String = self.source[self.start..self.current].iter().collect();

        let possible_result = match_identity(&stri);
        if possible_result.is_none()
        {
            self.add_lexed(TokenType::Iden)
        }
        else
        {
            self.add_simple(possible_result.unwrap())
        }
    }

    fn advance(&mut self) -> char
    {
        self.current += 1;
        self.current_column += 1;
        self.source[self.current - 1]
    }

    fn create_current_span(&self) -> CodeSpan
    {
        CodeSpan::new(
            self.id,
            self.start_line,
            self.current_line.saturating_sub(self.start_line),
            self.start_column,
            self.current_column.saturating_sub(self.start_column),
        )
    }

    fn add_simple(&mut self, tokentype: TokenType) -> TResult
    {
        self.tokens
            .push(Token::simple(tokentype, self.create_current_span()));
        Ok(())
    }

    fn add_lexed(&mut self, tokentype: TokenType) -> TResult
    {
        let string: String = self.source[self.start..self.current].iter().collect();

        self.tokens.push(Token::lexed(
            tokentype,
            self.create_current_span(),
            string,
        ));
        Ok(())
    }

    fn is_at_end(&self) -> bool
    {
        self.source.len() <= self.current
    }
}

