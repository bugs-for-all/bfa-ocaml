fn fib (n: u32) -> u32 {
   if n <= 0 {
       0
   } else if n == 1 {
       1
   } else {
       fib(n - 1) + fib(n - 2)
   }
}


#[cfg(kani)]
#[kani::proof]
fn main() {
    let _a = 10 / fib(2);
    let _b = 0 / fib(0);
}
