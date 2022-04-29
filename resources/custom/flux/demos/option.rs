#![feature(register_tool)]
#![register_tool(lr)]

#[lr::assume]
fn some<T>(x: T) -> Option<T> {
    Option::Some(x)
}

#[lr::sig(fn() -> i32{v:0 < v})]
pub fn test1() -> i32 { //~ ERROR postcondition might not hold
    let opt = some(0);
    opt.unwrap()
}

#[lr::sig(fn(i32[10]) -> i32)]
pub fn foo(n: i32) -> i32 {
    n
}

pub fn test2() -> i32 {
    foo(4) //~ ERROR precondition might not hold
}
