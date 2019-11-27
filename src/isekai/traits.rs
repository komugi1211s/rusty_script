
pub trait Visitor<T>
{
    type Result;
    fn visit(&mut self, t: &T) -> Self::Result;
}