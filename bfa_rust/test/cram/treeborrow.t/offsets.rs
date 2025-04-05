// Example 10 in
// https://perso.crans.org/vanille/treebor/aux/preprint.pdf
fn main() {
    let mut v = [0u8, 0];
    let x = &mut v[0];
    unsafe {
        let y = (x as *mut u8).add(1);
        *y = 1;
    }
}
