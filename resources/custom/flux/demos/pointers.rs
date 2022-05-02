#![feature(register_tool)]
#![register_tool(lr)]

// Increment via a value
#[lr::sig(fn(bool[true]) -> i32)]
pub fn assert(_b: bool) -> i32 {
    0
}

// Increment via a value
#[lr::sig(fn(n: i32) -> i32[n+1])]
pub fn inc_val(n: i32) -> i32 {
    n + 1
}

// Increment via a mutable reference
#[lr::sig(fn(x: &mut i32{v: 0 < v}) -> i32)]
pub fn inc_mut(x: &mut i32) -> i32 {
    *x += 1;

    // this would be rejected
    // *x -= 1;
    
    0
}

// Increment via a strong reference
#[lr::sig(fn(x: &strg i32[@n]) -> i32 ensures x: i32[n+1])]
pub fn inc_strg(x: &mut i32) -> i32 {
    *x += 1;
    0
}

// Test 
fn _test() -> i32 { 
   
    // client of inc_val
    assert(inc_val(10) == 11);

    // client of inc_ptr
    let mut p = 10;
    inc_ref(&mut p);
    assert(p == 11);
    0
}
