#![feature(register_tool)]
#![register_tool(lr)]

#[path = "../demos/lib/rvec.rs"]
mod rvec;
use rvec::RVec;

// Creating vectors ----------------------------------------------

#[lr::sig(fn(n:i32{0 <= n}, val:i32) -> RVec<i32>[n])]
pub fn init(n:i32, val:i32) -> RVec<i32> {
    let mut i = 0;
    let mut res = RVec::new();
    while i < n {
        res.push(val);
        i += 1;
    }
    res
}

// Reading vectors ----------------------------------------------

#[lr::sig(fn(&mut RVec<i32>[@n]) -> i32 requires 2 <= n)]
pub fn sum2(vec: &mut RVec<i32>) -> i32 {
    let v0 = *vec.get(0);
    let v1 = *vec.get(1);
    v0 + v1
}

#[lr::sig(fn(&RVec<i32{v:0 <= v}>) -> i32{v:0 <= v})]
pub fn sum(vec: &RVec<i32>) -> i32 {
    let n = vec.len();
    let mut i = 0;
    let mut total = 0;
    while i < n {
        total += *vec.get(i);
        i += 1;
    }
    total
}
