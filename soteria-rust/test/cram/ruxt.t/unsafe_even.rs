struct Even {
    value: i32,
}

fn zero() -> Even {
    Even { value: 0 }
}

fn new(n: i32) -> Even {
    Even { value: n - n % 2 }
}

fn succ(x: Even) -> Even {
    if x.value < i32::MAX - 1 {
        Even { value: x.value + 1 }
    } else {
        x
    }
}

fn next(x: Even) -> Even {
    succ(succ(x))
}

fn noop(x: Even) -> () {
    if x.value % 2 != 0 {
        unsafe { *(0 as *mut i32) = 1 }
    }
}
