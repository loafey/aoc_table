mod ffi;
mod table_gen;
mod task;

fn main() {
    // unsafe {
    //     use std::ffi::CString;
    //     let string = CString::new("hello").unwrap();
    //     let table = ffi::create_table(string.as_ptr() as *mut u8, 5);
    //
    //     extern "C" fn test1() -> i32 {
    //         1
    //     }
    //     extern "C" fn test2() -> i32 {
    //         151
    //     }
    //     ffi::add_func_int_32(table, test1, test2);
    //
    //     ffi::run(table);
    // }

    use table_gen::TableGen;
    TableGen::new("Howdy AOC_2020 Solver :-)")
        .add(
            || rand::random::<f32>() * 10.0,
            || {
                std::thread::sleep(std::time::Duration::from_secs_f32(
                    rand::random::<f32>() * 10.0,
                ));
                rand::random::<f32>() * 10.0
            },
        )
        .add(rand::random::<i32>, rand::random::<bool>)
        .add(|| "Different", || "types!")
        .run();
}
