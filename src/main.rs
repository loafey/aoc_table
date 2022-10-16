use table_gen::TableGen;
mod table_gen;
mod task;

fn main() {
    TableGen::new("Howdy AOC_2020 Solver :-)")
        .add(
            || {
                let time = rand::random::<f32>() * 10.0;
                Box::new(time)
            },
            || {
                let time = rand::random::<f32>() * 10.0;
                Box::new(time)
            },
        )
        .add(
            || {
                let time = rand::random::<f32>() * 10.0;
                Box::new(time as i32)
            },
            || {
                let time = rand::random::<f32>() * 10.0;
                Box::new(time as i32)
            },
        )
        .add(|| Box::new("Different"), || Box::new("types!"))
        .run();
}
