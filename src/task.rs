use std::{
    any::Any,
    thread::{self, JoinHandle},
    time::{Duration, Instant},
};

#[derive(Clone, Copy)]
pub struct Task<T: Send + 'static, F: Fn() -> T + Send + 'static> {
    f: F,
}
impl<T: Send, F: Fn() -> T + Send> Task<T, F> {
    pub fn new(f: F) -> Self {
        Self { f }
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
    j: Option<JoinHandle<T>>,
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
        val: Result<T, Box<dyn Any + Send + 'static>>,
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
}
