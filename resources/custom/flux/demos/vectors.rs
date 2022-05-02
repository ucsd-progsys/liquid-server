#![feature(register_tool)]
#![register_tool(lr)]

#[path = "../../lib/surface/rvec.rs"]
mod rvec;
use rvec::RVec;

// Creating vectors ----------------------------------------------

#[lr::sig(fn(n:i32{0 <= n}, val:i32) -> RVec<i32>[n])]
pub fn init(n:i32, val:i32) -> RVec<i32> {
    let mut i = 0;
    let res = RVec::new();
    while i < n {
        res.push(val);
    }
    res
}

// Reading vectors ----------------------------------------------

#[lr::sig(fn(&mut RVec<i32>[@n]) -> i32[0] requires 2 <= n)]
pub fn sum2(vec: &mut RVec<i32>) -> i32 {
    let v0 = *vec.get(0);
    let v1 = *vec.get(1);
    v0 + v1
}

#[lr::sig(fn(&mut RVec<i32{v:0 <= v}>) -> i32{v:0 <= v})]
pub fn sum(vec: &mut RVec<i32>) -> i32 {
    let n = vec.len();
    let mut i = 0;
    let mut total = 0;
    while i < n {
        total += *vec.get(i);
    }
    total
}

