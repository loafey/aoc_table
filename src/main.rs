mod ffi;
mod table_gen;
mod task;

fn main() {
    use table_gen::TableGen;
    TableGen::new("Howdy AOC_2020 Solver :-) ")
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
