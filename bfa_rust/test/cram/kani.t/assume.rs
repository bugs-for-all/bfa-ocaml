#[kani::proof]
fn assume_bool() {
    let b: bool = kani::any();
    kani::assume(b);
    if !b {
        panic!("Assumption failed");
    }
}

#[kani::proof]
fn assume_i32() {
    let x: i32 = kani::any();
    kani::assume(x != 0);
    let _ = 11 / x; // cannot panic
}
