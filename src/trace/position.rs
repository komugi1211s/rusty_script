use std::cmp;
use std::fmt;

// 意図的にInvalidなスパンを作ってそれをEmptyとして使う
pub const EMPTY_SPAN: CodeSpan = CodeSpan {
    row_start: 0,
    row_len: 0,
    col_start: 0,
    col_len: 0,
};

// ソースコード内で特定の範囲を指定するStruct
// 誰も4,294,967,295行以上のコードなんて書くわけがないので
// どっちもu32で固定
#[derive(Clone, Copy, PartialEq, Debug, Hash)]
pub struct CodeSpan
{
    pub row_start: u32,
    pub row_len: u32,
    pub col_start: u32,
    pub col_len: u32,
}

impl fmt::Display for CodeSpan
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result
    {
        write!(
            f,
            "{} - {}行目, {} - {}列目",
            self.row_start,
            self.row_start + self.row_len,
            self.col_start,
            self.col_start + self.col_len
        )
    }
}

impl CodeSpan
{
    pub fn new(row_st: usize, row_len: usize, col_st: usize, col_len: usize) -> Self
    {
        CodeSpan {
            row_start: row_st as u32,
            row_len: row_len as u32,
            col_start: col_st as u32,
            col_len: col_len as u32,
        }
    }

    pub fn oneline(row: usize, col_st: usize, col_len: usize) -> Self
    {
        Self::new(row, 0, col_st, col_len)
    }

    pub fn combine(a: &CodeSpan, b: &CodeSpan) -> Self
    {
        if a.row_start == b.row_start
        {
            let new_colstart = cmp::min(a.col_start, b.col_start);
            let new_col_len = (cmp::max(a.col_start, b.col_start) - new_colstart)
                + cmp::max(a.col_len, b.col_len);
            Self {
                row_start: a.row_start,
                row_len: cmp::max(a.row_len, b.row_len),
                col_start: new_colstart,
                col_len: new_col_len,
            }
        }
        else
        {
            let new_rowstart = cmp::min(a.row_start, b.row_start);
            let new_rowlen = cmp::max(a.row_len, b.row_len);
            Self {
                row_start: new_rowstart,
                row_len: new_rowlen,
                col_start: if new_rowstart == a.row_start
                {
                    a.col_start
                }
                else
                {
                    b.col_start
                },
                col_len: if new_rowlen == a.row_len
                {
                    a.col_len
                }
                else
                {
                    b.col_len
                },
            }
        }
    }

    #[inline(always)]
    pub fn cols(&self) -> (usize, usize)
    {
        (self.col_start as usize, self.col_len as usize)
    }

    #[inline(always)]
    pub fn rows(&self) -> (usize, usize)
    {
        (self.row_start as usize, self.row_len as usize)
    }

    #[inline(always)]
    pub fn is_oneliner(&self) -> bool
    {
        self.row_len == 0
    }

    #[inline(always)]
    pub fn is_invalid(&self) -> bool
    {
        self.row_start == 0 && self.row_len == 0 && self.col_start == 0 && self.col_len == 0
    }
}
// This test is old and it's kind of pointless
// So i'm partially removing it
// FIXME - @DumbCode: Come back and fix this.
/*
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_force_swap() {
        let start: usize = 5;
        let end: usize = 15;
        let invalid = CodeSpan::new(end, start);

        assert_eq!(invalid.start_usize(), start);
        assert_eq!(invalid.end_usize(), end);
    }

    #[test]
    fn test_contains() {
        let parent = CodeSpan::new(0, 10);
        let child = CodeSpan::new(1, 9);
        assert_eq!(parent.contains(&child), true);

        let parent = CodeSpan::new(0, 10);
        let same_size = CodeSpan::new(0, 10);
        assert_eq!(parent.contains(&same_size), false);
    }

    #[test]
    fn test_intersects() {
        let first = CodeSpan::new(0, 8);
        let second = CodeSpan::new(4, 12);
        assert_eq!(first.intersects(&second), true);

        let faraway = CodeSpan::new(0, 5);
        let independent = CodeSpan::new(50, 90);
        assert_eq!(faraway.intersects(&independent), false);
    }

    #[test]
    fn test_combine() {
        let a = CodeSpan::new(0, 10);
        let b = CodeSpan::new(5, 15);

        let combined = CodeSpan::combine(&a, &b);
        assert_eq!(combined.start, a.start);
        assert_eq!(combined.end, b.end);
    }
}
*/
