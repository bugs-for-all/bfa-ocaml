enum MyEnum {
    Empty,
    Simple(u16),
    Double(u16, u32),
    Triple(u16, u32, u64),
    #[rustfmt::skip]
    Hundred(
        u8, u8, u8, u8, u8, u8, u8, u8, u8, u8,
        u8, u8, u8, u8, u8, u8, u8, u8, u8, u8,
        u8, u8, u8, u8, u8, u8, u8, u8, u8, u8,
        u8, u8, u8, u8, u8, u8, u8, u8, u8, u8,
        u8, u8, u8, u8, u8, u8, u8, u8, u8, u8,
        u8, u8, u8, u8, u8, u8, u8, u8, u8, u8,
        u8, u8, u8, u8, u8, u8, u8, u8, u8, u8,
        u8, u8, u8, u8, u8, u8, u8, u8, u8, u8,
        u8, u8, u8, u8, u8, u8, u8, u8, u8, u8,
        u8, u8, u8, u8, u8, u8, u8, u8, u8, u8,
    ),
}

fn size(e: MyEnum) -> usize {
    match e {
        MyEnum::Empty => 0,
        MyEnum::Simple(..) => 1,
        MyEnum::Double(..) => 2,
        MyEnum::Triple(..) => 3,
        MyEnum::Hundred(..) => 100,
    }
}

pub fn any<T>() -> T {
    panic!()
}

fn main() {
    let a: MyEnum = any();
    size(a);
}
