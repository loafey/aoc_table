#![allow(clippy::missing_safety_doc)]
use std::fmt::Display;

use table_gen::TableGen;

mod table_gen;
mod task;
#[no_mangle]
pub unsafe extern "C" fn create_table(msg: *const u8, len: usize) -> *mut TableGen {
    let s = std::str::from_utf8_unchecked(std::slice::from_raw_parts(msg, len));
    Box::leak(Box::new(TableGen::new(s)))
}

#[no_mangle]
pub unsafe extern "C" fn add_func(
    src: *mut TableGen,
    p1: extern "C" fn() -> usize,
    p2: extern "C" fn() -> usize,
) {
    println!("{}", p1());
    println!("{}", p2());
    let mut dst = TableGen::new("");
    std::ptr::copy(src, &mut dst as *mut TableGen, 1);
    *src = dst.add(|| Box::new(1), || Box::new(2));
}

#[no_mangle]
pub unsafe extern "C" fn run(src: *mut TableGen) {
    let mut dst = TableGen::new("");
    std::ptr::copy(src, &mut dst as *mut TableGen, 1);
    dst.run()
}
