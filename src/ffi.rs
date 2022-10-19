use crate::table_gen::TableGen;

#[no_mangle]
pub unsafe extern "C" fn create_table(msg: *const u8, len: usize) -> *mut TableGen {
    let s = std::str::from_utf8_unchecked(std::slice::from_raw_parts(msg, len));
    Box::leak(Box::new(TableGen::new(s)))
}

#[no_mangle]
pub unsafe extern "C" fn add_func_int_32(
    src: *mut TableGen,
    p1: extern "C" fn() -> i32,
    p2: extern "C" fn() -> i32,
) -> *mut TableGen {
    src.as_mut().unwrap().add_boxed_mut(
        Box::new(move || Box::new(p1())),
        Box::new(move || Box::new(p2())),
    );
    src
}

#[no_mangle]
pub unsafe extern "C" fn run(src: *mut TableGen) {
    let mut dst = TableGen::new("");
    std::ptr::copy(src, &mut dst as *mut TableGen, 1);
    dst.run()
}
