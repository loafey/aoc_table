use std::{
    any::Any,
    thread::{self, JoinHandle},
    time::{Duration, Instant},
};

pub struct Task<T: Send + 'static> {
    f: Box<dyn Fn() -> (T, Duration) + Send + 'static>,
}
impl<T: Send> Task<T> {
    pub fn new(f: Box<dyn Fn() -> T + Send>) -> Self {
        Self {
            f: Box::new(move || {
                let i = Instant::now();
                (f(), i.elapsed())
            }),
        }
    }

    pub fn spawn(self) -> TaskRunner<T> {
        TaskRunner {
            val: TaskResult::Loading(Instant::now()),
            j: Some(thread::spawn(self.f)),
        }
    }
}

#[derive(Debug)]
pub struct TaskRunner<T: Send + 'static> {
    val: TaskResult<T>,
    j: Option<JoinHandle<(T, Duration)>>,
}
impl<T: Send> TaskRunner<T> {
    pub fn is_finished(&self) -> bool {
        self.j.as_ref().map(|j| j.is_finished()).unwrap_or(true)
    }

    pub fn get_val(&mut self) -> &TaskResult<T> {
        if self.is_finished() {
            let mut join_handle = None;
            std::mem::swap(&mut join_handle, &mut self.j);

            if let Some(s) = join_handle {
                self.val = TaskResult::Done {
                    val: s.join(),
                    elapsed: self.val.get_duration(),
                };
            }
        }

        &self.val
    }

    #[allow(unused)]
    pub fn consume(self) -> TaskResult<T> {
        if let Some(j) = self.j {
            TaskResult::Done {
                val: j.join(),
                elapsed: self.val.get_duration(),
            }
        } else {
            self.val
        }
    }
}

pub enum TaskResult<T> {
    Done {
        val: Result<(T, Duration), Box<dyn Any + Send + 'static>>,
        elapsed: Duration,
    },
    Loading(Instant),
}
impl<T: std::fmt::Debug> std::fmt::Debug for TaskResult<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Done { val, elapsed } => f
                .debug_struct("Done")
                .field("val", val)
                .field("elapsed", elapsed)
                .finish(),
            Self::Loading(_) => write!(f, "Loading"),
        }
    }
}
impl<T> TaskResult<T> {
    pub fn get_duration(&self) -> Duration {
        match self {
            TaskResult::Done { elapsed, .. } => *elapsed,
            TaskResult::Loading(i) => i.elapsed(),
        }
    }

    pub fn assume_ok(self) -> (T, Duration) {
        match self {
            TaskResult::Done { val, .. } => val.unwrap(),
            TaskResult::Loading(_) => panic!("assumed correct!"),
        }
    }
}
