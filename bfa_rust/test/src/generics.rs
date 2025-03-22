enum Foo<T> {
    Bar(T),
    Baz,
}

fn is_bar<T>(f: Foo<T>) -> bool {
    match f {
        Foo::Bar(_) => true,
        _ => false,
    }
}

pub fn any<T>() -> T {
    panic!()
}

fn main() {
    let a: Foo<i64> = any();
    let b: Foo<u128> = any();

    is_bar(a);
    is_bar(b);
}
