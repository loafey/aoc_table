// use std::{
//     any::Any,
//     fmt::{Debug, Display, Formatter, Result},
// };

// pub trait Print {
//     fn print(&self) -> String;
// }
// impl<T: Debug + !Display> Print for T {
//     fn print(&self) -> String {
//         format!("{self:?}")
//     }
// }
// impl<T: Display> Print for T {
//     fn print(&self) -> String {
//         format!("{self}")
//     }
// }
// // impl<T: Debug + !Display> Print for T {}
// //
