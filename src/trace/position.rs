
pub struct CodeLine {
    pub start: usize,
    pub end: usize,
}

pub struct CodeColumn {
    pub start: usize,
    pub end: usize,
}

impl CodeSpan {
    fn new(st: usize, en: usize) -> Self {
        CodeSpan {
            start: st,
            end: en
        }
    }
}


/*

    Tokenizer

    make...

    self.current_line = 15;
    self.end_line = 16;
    Token {
        span: CodeSpan::new(self.current_line, self.end_line);
    }

    then when error happens...
    if err {
        Trace::error(TokenizerError)
            .while_tokenizing("Tokenizer knows what's up")
            .caused_at(span)
            .related_info(error)
    }

    this would result to...

    TokenizerError: Tokenizer knows what's up.
    14  | ...
    15!!| ... .., ...
    16!!| ... ...... .. .. . . . ...
    17  | .. .. . . ...

    Parser

    make...

    Expr::


*/
