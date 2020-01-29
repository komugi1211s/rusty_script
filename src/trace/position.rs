use std::cmp;
use std::fmt;
use std::mem;

// 意図的にInvalidなスパンを作ってそれをEmptyとして使う
pub const EMPTY_SPAN: CodeSpan = CodeSpan {
    start: u32::max_value(),
    end: u32::min_value(),
};

// ソースコード内で特定の範囲を指定するStruct
// 誰も4,294,967,295行以上のコードなんて書くわけがないので
// どっちもu32で固定
#[derive(Clone, Copy, PartialEq, Debug, Hash)]
pub struct CodeSpan {
    pub start: u32,
    pub end: u32,
}

impl fmt::Display for CodeSpan {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "line {} ~ {}", self.start, self.end)
    }
}

impl CodeSpan {
    pub fn new(mut st: usize, mut en: usize) -> Self {
        // NOTE - @Improvement: 意図的な挙動でないので危険かも知れないのでは
        if st > en {
            mem::swap(&mut st, &mut en);
        }
        CodeSpan {
            start: st as u32,
            end: en as u32,
        }
    }

    pub fn oneline(line: usize) -> Self {
        CodeSpan {
            start: line as u32,
            end: line as u32,
        }
    }

    pub fn combine(a: &CodeSpan, b: &CodeSpan) -> Self {
        Self {
            start: cmp::min(a.start, b.start),
            end: cmp::max(a.end, b.end),
        }
    }

    #[inline]
    pub fn contains(&self, other: &CodeSpan) -> bool {
        self.start < other.start // self は other より前に始まって
            && self.end > other.end // other が終わったあとに終わる => 完全に中に収まっている
    }

    #[inline]
    pub fn intersects(&self, other: &CodeSpan) -> bool {
        self.start < other.end // self は other が終わる前に始まって
            && other.start < self.end // かつ other が始まった後に終わる => どこかが接触している
    }

    #[inline(always)]
    pub fn start_usize(&self) -> usize {
        self.start as usize
    }

    #[inline(always)]
    pub fn end_usize(&self) -> usize {
        self.end as usize
    }

    #[inline(always)]
    pub fn is_oneliner(&self) -> bool {
        self.end == self.start
    }

    #[inline(always)]
    pub fn is_invalid(&self) -> bool {
        self.end < self.start
    }

    #[inline(always)]
    pub fn length(&self) -> usize {
        if self.is_invalid() {
            0
        } else {
            self.end_usize() - self.start_usize()
        }
    }
}

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
