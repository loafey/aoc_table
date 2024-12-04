use crate::task::Task;
use crate::task::TaskResult;
use chrono::Datelike;
use crossterm::{style::Print, ExecutableCommand};
use std::collections::BTreeMap;
use std::time::Instant;
use std::{cell::Cell, fmt::Display, io::stdout};
use std::{thread, time::Duration};

#[derive(Debug)]
pub struct BenchmarkResults {
    pub day: usize,
    pub p1_best: Duration,
    pub p1_worst: Duration,
    pub p1_avg: Duration,
    pub p2_best: Duration,
    pub p2_worst: Duration,
    pub p2_avg: Duration,
}

struct PrintableDay {
    day: String,
    p1_result: String,
    p2_result: String,
    time: String,
    time_sec: String,
}

// A boxed function that returns
type DisplayFunc = Box<dyn Fn() -> Box<dyn Display + Send> + Send>;

// A tuple of solvers for a day.
struct DaySolvers {
    part1: DisplayFunc,
    part2: DisplayFunc,
}

pub struct TableGen {
    // The header to be printed at the top of the formatted table from `run`.
    msg: String,

    // A map from days to solvers.
    tasks: BTreeMap<usize, DaySolvers>, // TODO: Rename to something else ("solvers"?)
}
impl TableGen {
    /// Create a new TableGen instance.
    ///
    /// # Arguments
    ///
    /// * `table_header` - A header that will be included in the table printed
    /// from `run`.
    pub fn new<S: Into<String>>(table_header: S) -> Self {
        Self {
            tasks: BTreeMap::new(),
            msg: table_header.into(),
        }
    }

    /// Register solvers for both parts of a given day.  A solver is a function
    /// that returns a value that can be displayed.  The solvers do not have to
    /// be added in order.
    ///
    /// # Arguments
    ///
    /// * `day` - The day to register the solvers for.
    /// * `p1` - The solver for the first part of the puzzle.  Should return a
    /// value that can be displayed.
    /// * `p2` - The solver for the first part of the puzzle.  Should return a
    /// value that can be displayed.
    ///
    /// # Examples
    ///
    /// ```
    /// use aoc_table::table_gen::TableGen;
    /// TableGen::new("Howdy AOC_2022 Solver :-)")
    ///     .add(2, rand::random::<i32>, rand::random::<bool>)
    ///     .add(6, || "Different", || "types!");
    ///
    /// // Then call .run() to run the solvers and print the output.
    /// ```
    pub fn add<A, B, F1, F2>(self, day: usize, p1: F1, p2: F2) -> Self
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
            day,
            function_wrapper(Box::new(p1)),
            function_wrapper(Box::new(p2)),
        )
    }

    /// Register solvers for both parts of the next highest day.  The solvers
    /// will be registered for the highest currently registered day + 1.  If no
    /// day is registered for, the default is 1.
    ///
    /// # Arguments
    ///
    /// * `p1` - The solver for the first part of the puzzle.  Should return a
    /// value that can be displayed.
    /// * `p2` - The solver for the first part of the puzzle.  Should return a
    /// value that can be displayed.
    pub fn add_next<A, B, F1, F2>(self, p1: F1, p2: F2) -> Self
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

        let day = match self.tasks.keys().last() {
            Some(d) => d + 1,
            None => 1, // Default to day 1 if no solvers are registered.
        };
        self.add_boxed(
            day,
            function_wrapper(Box::new(p1)),
            function_wrapper(Box::new(p2)),
        )
    }

    fn add_boxed(mut self, day: usize, p1: DisplayFunc, p2: DisplayFunc) -> Self {
        self.tasks.insert(
            day,
            DaySolvers {
                part1: p1,
                part2: p2,
            },
        );
        self
    }

    fn itterify_me(self) -> impl Iterator<Item = (usize, DaySolvers)> {
        self.tasks.into_iter()
    }

    /// Run the solvers for a given day, printing the result and time elapsed.
    ///
    /// # Arguments
    ///
    /// * `day` - The day to run the solvers for.
    ///
    /// # Panics
    ///
    /// Panics if there are no solvers registered for the given day.
    pub fn run_day(mut self, day: usize) {
        let err_msg = format!("No solution for day {day} in the Table!");
        let solvers = self
            .tasks
            .remove(&day)
            .unwrap_or_else(|| panic!("{}", err_msg.to_string()));

        println!("╍╍╍ Part 1: ╍╍╍");
        let (res, time) = Task::new(solvers.part1).spawn().consume().assume_ok();
        println!("{res}");
        println!("╍ Solution took: {}μs ╍", time.as_micros());
        println!("╍╍╍ Part 2: ╍╍╍");
        let (res, time) = Task::new(solvers.part2).spawn().consume().assume_ok();
        println!("{res}");
        println!("╍ Solution took: {}μs ╍", time.as_micros());
    }

    /// Run the solvers for the current day, printing the result and time
    /// elapsed.
    ///
    /// # Panics
    ///
    /// Panics if there are no solvers registered for the current day.
    pub fn run_current_day(self) {
        let today = chrono::Local::now().date_naive().day() as usize;
        if self.tasks.contains_key(&today) {
            self.run_day(today)
        } else {
            panic!("No solution registered for today's date! ({today})")
        }
    }

    /// Run all registered solvers, printing the result and time elapsed into a
    /// nicely formatted table.
    ///
    /// # Panics
    ///
    /// Panics if there is a bug in the implementation of `run`.
    pub fn run(self) {
        if self.tasks.is_empty() {
            println!("Table \"{}\" has no tasks!", self.msg);
            return;
        }

        let msg = self.msg.clone();
        let mut tasks = self
            .itterify_me()
            .map(|(day, solvers)| {
                (
                    day,
                    Task::new(solvers.part1).spawn(),
                    Task::new(solvers.part2).spawn(),
                )
            })
            .collect::<Vec<_>>();

        println!("{}", "\n".repeat(tasks.len() + 2));

        let day_width = Cell::new(0);
        let p1_width = Cell::new(0);
        let p2_width = Cell::new(0);
        let time_width = Cell::new(0);
        let time_sec_width = Cell::new(0);

        let (org_x, org_y) = crossterm::cursor::position().unwrap();
        stdout().execute(crossterm::cursor::Hide).unwrap();
        loop {
            let should_break = tasks
                .iter()
                .map(|(_day, p1, p2)| p1.is_finished() && p2.is_finished())
                .reduce(|accum, item| accum && item)
                .unwrap_or_default();

            let mut printable_table = tasks
                .iter_mut()
                .map(|(day, p1, p2)| {
                    let day = format!("{day}");
                    let (p1_result, p1_time) = if let TaskResult::Done { val, .. } = p1.get_val() {
                        match val {
                            Ok((o, t)) => (format!("{o}"), Some(*t)),
                            Err(e) => (format!("{e:?}"), None),
                        }
                    } else {
                        ("Loading...".to_string(), None)
                    };
                    let (p2_result, p2_time) = if let TaskResult::Done { val, .. } = p2.get_val() {
                        match val {
                            Ok((o, t)) => (format!("{o}"), Some(*t)),
                            Err(e) => (format!("{e:?}"), None),
                        }
                    } else {
                        ("Loading...".to_string(), None)
                    };
                    let time = match (p1_time, p1.get_val()) {
                        (Some(t), _) => t.as_micros(),
                        (None, TaskResult::Done { elapsed, .. }) => elapsed.as_micros(),
                        (None, TaskResult::Loading(i)) => i.elapsed().as_micros(),
                    }
                    .max(match (p2_time, p2.get_val()) {
                        (Some(t), _) => t.as_micros(),
                        (None, TaskResult::Done { elapsed, .. }) => elapsed.as_micros(),
                        (None, TaskResult::Loading(i)) => i.elapsed().as_micros(),
                    });
                    let time_sec = format!("{:.4} s", time as f64 / 1000000.0);
                    let time = format!("{time:.3} μs");
                    day_width.set(day_width.get().max(day.len()));
                    p1_width.set(p1_width.get().max(p1_result.len()));
                    p2_width.set(p2_width.get().max(p2_result.len()));
                    time_width.set(time_width.get().max(time.len()));
                    time_sec_width.set(time_sec_width.get().max(time_sec.len()));

                    PrintableDay {
                        day,
                        p1_result,
                        p2_result,
                        time,
                        time_sec,
                    }
                })
                .collect::<Vec<_>>();
            printable_table.reverse();
            let day_width = day_width.get();
            let p1_width = p1_width.get();
            let p2_width = p2_width.get();
            let time_width = time_width.get();
            let time_sec_width = time_sec_width.get();

            for PrintableDay {
                day,
                p1_result,
                p2_result,
                time,
                time_sec,
            } in printable_table
            {
                let prepped_string = format!(
                    "║ {}{} │ {}{} │ {}{} │ {}{} │ {}{} ║",
                    " ".repeat(day_width - day.len()),
                    day,
                    p1_result,
                    " ".repeat(p1_width - p1_result.len()),
                    p2_result,
                    " ".repeat(p2_width - p2_result.len()),
                    time,
                    " ".repeat(time_width - time.len()),
                    time_sec,
                    " ".repeat(time_sec_width - time_sec.len())
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

            let total_len = day_width + p1_width + p2_width + time_width + time_sec_width + 13;
            let msg_len = msg.chars().map(|c| c.len_utf16()).sum::<usize>();
            let half_len = (total_len / 2) - (msg_len / 2);
            let dif = if (half_len + msg_len + half_len) < total_len {
                total_len - (half_len + msg_len + half_len)
            } else {
                0
            };
            let top_line = format!(
                "╔{}╗\n║{}{}{}║\n╟{}╢",
                "═".repeat(total_len),
                " ".repeat(half_len),
                msg,
                " ".repeat(half_len + dif),
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

    pub fn run_benchmarks(self) -> (String, Vec<BenchmarkResults>) {
        let name = self.msg.clone();
        const TEST_AMOUNT: u32 = 100;
        let tasks = self
            .itterify_me()
            .map(|(day, solvers)| {
                let mut p1_best = Duration::from_secs(10000000);
                let mut p1_avg = Duration::from_secs(0);
                let mut p1_worst = Duration::from_secs(0);
                for _ in 0..TEST_AMOUNT {
                    let instant = Instant::now();
                    (solvers.part1)();
                    let time = instant.elapsed();
                    p1_avg += time;
                    p1_best = p1_best.min(time);
                    p1_worst = p1_worst.max(time);
                }
                p1_avg /= TEST_AMOUNT;

                let mut p2_best = Duration::from_secs(10000000);
                let mut p2_avg = Duration::from_secs(0);
                let mut p2_worst = Duration::from_secs(0);
                for _ in 0..TEST_AMOUNT {
                    let instant = Instant::now();
                    (solvers.part2)();
                    let time = instant.elapsed();
                    p2_avg += time;
                    p2_best = p2_best.min(time);
                    p2_worst = p2_worst.max(time);
                }
                p2_avg /= TEST_AMOUNT;

                BenchmarkResults {
                    day,
                    p1_best,
                    p1_worst,
                    p1_avg,
                    p2_best,
                    p2_worst,
                    p2_avg,
                }
            })
            .collect::<Vec<_>>();

        (name, tasks)
    }
}
