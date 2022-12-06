mod table_gen;
mod task;

fn main() {
    use table_gen::TableGen;
    TableGen::new("Howdy AOC_2022 Solver :-) ")
        .add(1, rand::random::<f32>, || {
            std::thread::sleep(std::time::Duration::from_secs_f32(rand::random::<f32>()));
            rand::random::<f32>()
        })
        .add(2, rand::random::<i32>, rand::random::<bool>)
        .add(5, || "Different", || "types!")
        .run();

    TableGen::new("Howdy AOC_2022 Solver :-) ")
        .add(2, rand::random::<i32>, rand::random::<bool>)
        .add(5, || "Different", || "types!")
        .run_day(2);

    TableGen::new("Howdy AOC_2022 Solver :-) ")
        .add(2, rand::random::<i32>, rand::random::<bool>)
        .add(6, || "Different", || "types!")
        .run_current_day();
}
