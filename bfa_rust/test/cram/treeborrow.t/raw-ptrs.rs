// Example 9 in
// https://perso.crans.org/vanille/treebor/aux/preprint.pdf
fn main() {
    let mut root = 42;
    let ref1 = &mut root;
    let ptr1 = ref1 as *mut i32;
    unsafe {
        *ref1 = 43;
        *ptr1 = 44;
        *ref1 = 45;
    }
}
