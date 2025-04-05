#[kani::proof]
fn any_bool() {
    let b: bool = kani::any();
    if b {
        assert!(b);
    } else {
        assert!(!b);
    }
}

#[kani::proof]
fn any_i8() {
    let i: i8 = kani::any();
    if i == 0 {
        assert!(i == 0);
    } else {
        assert!(i < 0 || i > 0);
    }
}
