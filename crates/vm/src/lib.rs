#[macro_use]
pub mod gc;
pub mod task;
pub mod value;
pub mod vm;

pub use gc::TraceVTable;
