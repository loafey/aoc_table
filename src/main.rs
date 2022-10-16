use table_gen::TableGen;
mod table_gen;
mod task;

fn main() {
    TableGen::new("Howdy AOC_2020 Solver :-)")
        .add(
            || Box::new(rand::random::<f32>() * 10.0),
            || Box::new(rand::random::<f32>() * 10.0),
        )
        .add(
            || Box::new(rand::random::<i32>()),
            || Box::new(rand::random::<char>()),
        )
        .add(|| Box::new("Different"), || Box::new("types!"))
        .run();
}
