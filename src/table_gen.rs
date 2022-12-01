use crate::task::Task;
use crate::task::TaskResult;
use crossterm::{style::Print, ExecutableCommand};
use std::fmt::Debug;
use std::{cell::Cell, fmt::Display, io::stdout};
use std::{thread, time::Duration};

struct PrintableDay {
    day: String,
    p1_result: String,
    p2_result: String,
    time: String,
}

type DisplayFunc = Box<dyn Fn() -> Box<dyn Display + Send> + Send>;
type DisplayTask = Task<Box<dyn Display + Send>>;

pub struct TableGen {
    msg: String,
    tasks: Vec<(DisplayTask, DisplayTask)>,
}
impl TableGen {
    pub fn new<S: Into<String>>(msg: S) -> Self {
        Self {
            tasks: Vec::new(),
            msg: msg.into(),
        }
    }

    pub fn add<A, B, F1, F2>(self, p1: F1, p2: F2) -> Self
    where
        A: Display + Send + 'static,
        B: Display + Send + 'static,
        F1: Fn() -> A + Send + 'static,
        F2: Fn() -> B + Send + 'static,
    {
        fn function_wrapper<T: Display + Send + 'static>(
            f: Box<dyn Fn() -> T + Send>,
        ) -> Box<dyn Fn() -> Box<dyn Display + Send> + Send> {
            Box::new(move || Box::new(f()))
        }

        self.add_boxed(
            function_wrapper(Box::new(p1)),
            function_wrapper(Box::new(p2)),
        )
    }

    pub fn add_boxed_mut(&mut self, p1: DisplayFunc, p2: DisplayFunc) {
        self.tasks.push((Task::new(p1), Task::new(p2)));
    }

    pub fn add_boxed(mut self, p1: DisplayFunc, p2: DisplayFunc) -> Self {
        self.tasks.push((Task::new(p1), Task::new(p2)));
        self
    }
    pub fn into_iter(self) -> impl Iterator<Item = (DisplayTask, DisplayTask)> {
        self.tasks.into_iter()
    }
    pub fn run_day(mut self, day: usize) {
        let funcs = self.tasks.remove(day - 1);
        println!("╍╍╍ Part 1: ╍╍╍");
        println!("{}", funcs.0.spawn().consume().assume_ok());
        println!("╍╍╍ Part 2: ╍╍╍");
        println!("{}", funcs.1.spawn().consume().assume_ok());
    }
    pub fn run(self) {
        if self.tasks.is_empty() {
            println!("Table \"{}\" has no tasks!", self.msg);
            return;
        }

        let msg = self.msg.clone();
        let mut tasks = self
            .into_iter()
            .map(|(p1, p2)| (p1.spawn(), p2.spawn()))
            .collect::<Vec<_>>();

        println!("{}", "\n".repeat(tasks.len() + 2));

        let day_width = Cell::new(0);
        let p1_width = Cell::new(0);
        let p2_width = Cell::new(0);
        let time_width = Cell::new(0);

        let (org_x, org_y) = crossterm::cursor::position().unwrap();
        stdout().execute(crossterm::cursor::Hide).unwrap();
        loop {
            let should_break = tasks
                .iter()
                .map(|(p1, p2)| p1.is_finished() && p2.is_finished())
                .reduce(|accum, item| accum && item)
                .unwrap_or_default();

            let mut printable_table = tasks
                .iter_mut()
                .enumerate()
                .map(|(day, (p1, p2))| {
                    let day = format!("{}", day + 1);
                    let p1_result = if let TaskResult::Done { val, .. } = p1.get_val() {
                        match val {
                            Ok(o) => format!("{}", o),
                            Err(e) => format!("{:?}", e),
                        }
                    } else {
                        "Loading...".to_string()
                    };
                    let p2_result = if let TaskResult::Done { val, .. } = p2.get_val() {
                        match val {
                            Ok(o) => format!("{}", o),
                            Err(e) => format!("{:?}", e),
                        }
                    } else {
                        "Loading...".to_string()
                    };
                    let time = match p1.get_val() {
                        TaskResult::Done { elapsed, .. } => elapsed.as_secs_f32(),
                        TaskResult::Loading(i) => i.elapsed().as_secs_f32(),
                    }
                    .max(match p2.get_val() {
                        TaskResult::Done { elapsed, .. } => elapsed.as_secs_f32(),
                        TaskResult::Loading(i) => i.elapsed().as_secs_f32(),
                    });
                    let time = format!("{:.3}s", time);
                    day_width.set(day_width.get().max(day.len()));
                    p1_width.set(p1_width.get().max(p1_result.len()));
                    p2_width.set(p2_width.get().max(p2_result.len()));
                    time_width.set(time_width.get().max(time.len()));

                    PrintableDay {
                        day,
                        p1_result,
                        p2_result,
                        time,
                    }
                })
                .collect::<Vec<_>>();
            printable_table.reverse();
            let day_width = day_width.get();
            let p1_width = p1_width.get();
            let p2_width = p2_width.get();
            let time_width = time_width.get();

            for PrintableDay {
                day,
                p1_result,
                p2_result,
                time,
            } in printable_table
            {
                let prepped_string = format!(
                    "║ {}{} │ {}{} │ {}{} │ {}{} ║",
                    " ".repeat(day_width - day.len()),
                    day,
                    p1_result,
                    " ".repeat(p1_width - p1_result.len()),
                    p2_result,
                    " ".repeat(p2_width - p2_result.len()),
                    time,
                    " ".repeat(time_width - time.len())
                );
                stdout()
                    .execute(crossterm::cursor::MoveUp(0))
                    .unwrap()
                    .execute(crossterm::terminal::Clear(
                        crossterm::terminal::ClearType::CurrentLine,
                    ))
                    .unwrap()
                    .execute(Print(prepped_string))
                    .unwrap()
                    .execute(crossterm::cursor::MoveTo(
                        0,
                        crossterm::cursor::position().unwrap().1,
                    ))
                    .unwrap();
            }

            let total_len = day_width + p1_width + p2_width + time_width + 11;
            let half_len = (total_len / 2) - (msg.len() / 2) - 1;
            let top_line = format!(
                "╔{}╗\n║{}{}{}║\n╟{}╢",
                "═".repeat(total_len),
                " ".repeat(half_len + 1),
                msg,
                " ".repeat(half_len + (total_len % 2)),
                "─".repeat(total_len)
            );
            let bottom_line = format!("╚{}╝", "═".repeat(total_len));
            stdout()
                .execute(crossterm::cursor::MoveUp(3))
                .unwrap()
                .execute(Print(top_line))
                .unwrap()
                .execute(crossterm::cursor::MoveTo(org_x, org_y))
                .unwrap()
                .execute(Print(bottom_line))
                .unwrap()
                .execute(crossterm::cursor::MoveTo(org_x, org_y))
                .unwrap();
            thread::sleep(Duration::from_secs_f32(0.02));

            if should_break {
                break;
            }
        }
        println!();

        stdout().execute(crossterm::cursor::Show).unwrap();
    }
}
