#![feature(register_tool)]
#![register_tool(kanitool)]

extern crate self as kani;

pub use kani_macros::*;

pub fn any<T>() -> T {
    panic!();
}

#[inline(never)]
#[kanitool::fn_marker = "assert"]
pub const fn assert(_cond: bool, _msg: &'static str) {}

#[inline(never)]
#[kanitool::fn_marker = "assume"]
pub const fn assume(_cond: bool) {}

pub trait Arbitrary
where
    Self: Sized,
{
    fn any() -> Self;
    fn any_array<const MAX_ARRAY_LENGTH: usize>() -> [Self; MAX_ARRAY_LENGTH] {
        [(); MAX_ARRAY_LENGTH].map(|_| Self::any())
    }
}
