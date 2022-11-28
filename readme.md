A (currently undocumented) library for running AoC tasks in parallel, with a fancy timer that updates in real time :-)
Also includes extremely scuffed and highly ugly Haskell bindings for those who seek danger.

```rust
fn main() {
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
        .add(day1::task1, day1::task2)
        ...
        .add(day25::task25, day25::task25)
        .run();
}
```

```rust
fn main() {
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
        .run_day(2); // only runs day 2
}
```