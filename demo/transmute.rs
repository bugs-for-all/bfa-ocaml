#[rusteria::test]
fn transmutating() {
    let a = rusteria::nondet();
    let b = rusteria::nondet();
    let c = rusteria::nondet();
    let d = rusteria::nondet();
    let as_arr: [u8; 4] = [a, b, c, d];
    let as_u32: u32 = unsafe { std::mem::transmute(as_arr) };
    assert_eq!(
        as_u32,
        (a as u32) + ((b as u32) << 8) + ((c as u32) << 16) + ((d as u32) << 24)
    );
}
