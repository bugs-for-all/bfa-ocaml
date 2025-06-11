#[kani::proof]
fn aliasing() {
    let mut root = 42;
    let ptr = &mut root as *mut i32;
    let (x, y) = unsafe { (&mut *ptr, &mut *ptr) };
    *x = 13;
    *y = 20;
    let val = *x; // we used y, so x is now disabled
}
