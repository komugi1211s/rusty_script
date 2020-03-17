
use crate::proc_macro::TokenStream;
use quote::quote;
use syn;

use std::time::Instant;
trait Recordable 
{
    fn queue(&mut self, record: PerfCounter);
    fn report(&mut self);
}

struct NoopReporter;
impl NoopReporter
{
    fn queue(&mut self, record: PerfCounter) {}
    fn report(&mut self) {}
}

struct PerfReporter 
{
}

impl PerfReporter
{
    fn new() -> Self 
    {
        Self 
        {
            perfcounts: Vec<PerfCounter>
        }
    }
}

#[derive(Copy, Clone, Debug)]
pub struct PerfCounter 
{
    pub count: u64,
}


