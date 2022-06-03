// use std::mem;
pub mod token;
use token::{match_identity, Token, TokenType};
use super::trace::prelude::*;

pub struct Tokenizer<'m>
{
    source: Vec<char>,
    tokens: Vec<Token<'m>>,
    start: usize,
    current: usize,

    start_line: usize,
    current_line: usize,
    start_column: usize,
    current_column: usize,
}

type TResult = Result<(), ()>;

impl<'m> Tokenizer<'m>
{
    pub fn new(reserve_length: usize) -> Self
    {
        Tokenizer {
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

    pub fn scan(&mut self, modu: &'m SourceFile) -> Result<Vec<Token<'m>>, ()>
    {
        self.source.extend(modu.code.chars());
        while !self.is_at_end()
        {
            self.set_start_index();
            self.scan_next_token(modu)?;
        }

        let span = self.create_current_span();
        self.tokens.push(Token::simple(modu, TokenType::EOF, span));
        Ok(self.tokens.to_owned())
    }

    fn scan_next_token(&mut self, module: &'m SourceFile) -> TResult
    {
        let c: char = self.advance();
        match c
        {
            '(' => self.add_simple(module, TokenType::OpenParen),
            ')' => self.add_simple(module, TokenType::CloseParen),
            '{' => self.add_simple(module, TokenType::OpenBrace),
            '}' => self.add_simple(module, TokenType::CloseBrace),
            '[' => self.add_simple(module, TokenType::OpenSquareBracket),
            ']' => self.add_simple(module, TokenType::CloseSquareBracket),
            '@' => self.add_simple(module, TokenType::AtMark),
            ':' => self.add_simple(module, TokenType::Colon),
            ';' => self.add_simple(module, TokenType::SemiColon),
            '.' => self.add_simple(module, TokenType::Dot),
            ',' => self.add_simple(module, TokenType::Comma),
            '+' => self.add_simple(module, TokenType::Plus),
            '-' => self.add_simple(module, TokenType::Minus),
            '*' => self.add_simple(module, TokenType::Asterisk),
            '%' => self.add_simple(module, TokenType::Percent),
            '?' => self.add_simple(module, TokenType::Question),
            '^' => self.add_simple(module, TokenType::Caret),
            '=' => match self.next_is('=')
            {
                true => self.add_simple(module, TokenType::EqualEqual),
                false => self.add_simple(module, TokenType::Equal),
            },
            '>' => match self.next_is('=')
            {
                true => self.add_simple(module, TokenType::MoreEqual),
                false => self.add_simple(module, TokenType::More),
            },
            '<' => match self.next_is('=')
            {
                true => self.add_simple(module, TokenType::LessEqual),
                false => self.add_simple(module, TokenType::Less),
            },
            '!' => match self.next_is('=')
            {
                true => self.add_simple(module, TokenType::NotEqual),
                false => self.add_simple(module, TokenType::Bang),
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

                    _ => self.add_simple(module, TokenType::Slash),
                }
            }

            // スペース、特殊文字は全て無視
            ' ' | '\r' | '\t' => Ok(()),

            // 改行
            '\n' => self.add_newline(),

            // 文字列を追加
            '"' => self.add_string(module),

            // 数字全般を単発で判定
            '0'..='9' => self.add_digit(module),
            'A'..='z' => self.add_possible_iden(module),

            // Default
            def =>
            {
                let span = self.create_current_span();
                report("Unknown Token", &format!("未知のトークン {} を発見しました。", def));
                spit_line(module, &span);
                Err(())
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

    fn add_string(&mut self, module: &'m SourceFile) -> TResult
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
            report("Unterminated String", "\n文字列が閉じられていません。\n");
            spit_line(module, &self.create_current_span());
            return Err(());
        }

        self.advance();
        // start + 1 & current - 1 は "" ←これを削る
        self.start += 1;
        self.current -= 1;
        let z = self.add_lexed(module, TokenType::Str);
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

    fn add_digit(&mut self, module: &'m SourceFile) -> TResult
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

        self.add_lexed(module, TokenType::Digit)
    }

    fn add_possible_iden(&mut self, module: &'m SourceFile) -> TResult
    {
        while self.peek().is_ascii_alphanumeric() || self.peek() == '_'
        {
            self.advance();
        }
        let stri: String = self.source[self.start..self.current].iter().collect();

        let possible_result = match_identity(&stri);
        if possible_result.is_none()
        {
            self.add_lexed(module, TokenType::Iden)
        }
        else
        {
            self.add_simple(module, possible_result.unwrap())
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
            self.start_line,
            self.current_line.saturating_sub(self.start_line),
            self.start_column,
            self.current_column.saturating_sub(self.start_column),
        )
    }

    fn add_simple(&mut self, module: &'m SourceFile, tokentype: TokenType) -> TResult
    {
        self.tokens
            .push(Token::simple(module, tokentype, self.create_current_span()));
        Ok(())
    }

    fn add_lexed(&mut self, module: &'m SourceFile, tokentype: TokenType) -> TResult
    {
        let string: String = self.source[self.start..self.current].iter().collect();

        self.tokens.push(Token::lexed(
            module,
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

