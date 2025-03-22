fn foo(e: (i16, i16)) -> i16 {
    let (a, b) = e;
    a + b
}

fn main() {
    let a = (1, 128);
    foo(a);
}
