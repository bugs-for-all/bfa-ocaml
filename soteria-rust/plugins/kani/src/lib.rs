// SPDX-License-Identifier: Apache-2.0 OR MIT

#![feature(f16)]
#![feature(f128)]
#![feature(register_tool)]
#![register_tool(kanitool)]

extern crate self as kani;

pub use kani_macros::*;

#[inline(never)]
pub const fn panic(message: &'static str) -> ! {
    rusteria::panic(message)
}

#[inline(never)]
pub const fn assert(cond: bool, msg: &'static str) {
    rusteria::assert(cond, msg);
}

#[inline(never)]
pub const fn assume(cond: bool) {
    rusteria::assume(cond);
}

#[inline(never)]
pub const fn nondet<T>() -> T {
    rusteria::nondet::<T>()
}

#[inline(never)]
pub const fn cover(_cond: bool, _msg: &'static str) {}

#[macro_export]
macro_rules! cover {
    () => {
        kani::cover(true, "cover location");
    };
    ($cond:expr $(,)?) => {
        kani::cover($cond, concat!("cover condition: ", stringify!($cond)));
    };
    ($cond:expr, $msg:literal) => {
        kani::cover($cond, $msg);
    };
}

pub mod invariant;
pub use invariant::Invariant;

mod arbitrary;
pub use arbitrary::*;

pub fn any<T: Arbitrary>() -> T {
    T::any()
}

#[inline(always)]
pub fn any_where<T: Arbitrary, F: FnOnce(&T) -> bool>(f: F) -> T {
    let result = T::any();
    assume(f(&result));
    result
}
