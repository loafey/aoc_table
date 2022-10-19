use crate::table_gen::TableGen;
use ffi_gen::gen_ffi;
use std::fmt::Display;

#[no_mangle]
pub unsafe extern "C" fn create_table(msg: *const u8, len: usize) -> *mut TableGen {
    let s = std::str::from_utf8_unchecked(std::slice::from_raw_parts(msg, len));
    Box::leak(Box::new(TableGen::new(s)))
}

unsafe fn add_func<A, B>(
    src: *mut TableGen,
    p1: extern "C" fn() -> A,
    p2: extern "C" fn() -> B,
) -> *mut TableGen
where
    A: Display + Send + 'static,
    B: Display + Send + 'static,
{
    src.as_mut().unwrap().add_boxed_mut(
        Box::new(move || Box::new(p1())),
        Box::new(move || Box::new(p2())),
    );
    src
}

gen_ffi! {
    #[no_mangle]
    pub unsafe extern "C" fn add_func_TYPE1_TYPE2(
        src: *mut TableGen,
        p1: extern "C" fn() -> TYPE1,
        p2: extern "C" fn() -> TYPE2,
    ) -> *mut TableGen {
        add_func(src, p1, p2)
    },
    bool, u8, i8, u16, i16, u32, i32, u64, i64, f32, f64
}

#[no_mangle]
pub unsafe extern "C" fn run(src: *mut TableGen) {
    let mut dst = TableGen::new("");
    std::ptr::copy(src, &mut dst as *mut TableGen, 1);
    dst.run()
}
