use table_gen::TableGen;
mod table_gen;
mod task;

fn main() {
    TableGen::new("Howdy AOC_2020 Solver :-)")
        .add(
            || rand::random::<f32>() * 10.0,
            || rand::random::<f32>() * 10.0,
        )
        .add(rand::random::<i32>, rand::random::<char>)
        .add(|| "Different", || "types!")
        .run();
}
