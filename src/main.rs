mod task;
use std::{thread, time::Duration};

use task::Task;

fn main() {
    let mut tasks = [Task::new(|| {
        let time = rand::random::<f32>() * 10.0;
        thread::sleep(Duration::from_secs_f32(time));
        time
    }); 5]
        .into_iter()
        .map(|t| t.spawn())
        .collect::<Vec<_>>();

    loop {
        for t in tasks.iter_mut() {
            println!("{:?}", t.get_val())
        }
        println!();
        thread::sleep(Duration::from_secs_f32(0.2));
    }

    // let mut t = Task::new(|| {
    //     thread::sleep(Duration::from_secs_f32(rand::random::<f32>() * 10));
    //     10
    // })
    // .spawn();
    //
    // loop {
    //     println!("{:?}", t.get_val());
    //     thread::sleep(Duration::from_secs_f32(0.2));
    // }
}
