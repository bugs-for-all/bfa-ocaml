// Example 7 in
// https://perso.crans.org/vanille/treebor/aux/preprint.pdf
fn main() {
    let mut root = 42;
    let ptr = &mut root as *mut i32;
    let (x, y) = unsafe { (&mut *ptr, &mut *ptr) };
    *x = 13;
    *y = 20; // UB: y is disabled
    let val = *x;
}
