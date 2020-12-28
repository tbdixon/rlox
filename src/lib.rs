#![allow(non_camel_case_types)]
pub mod chunk;
pub mod compiler;
pub mod debug;
pub mod memory;
pub mod natives;
pub mod opcode;
pub mod precedence;
pub mod scanner;
pub mod value;
pub mod vm;

use crate::memory::LoxHeap;
pub static mut HEAP: *mut LoxHeap = 0 as *mut LoxHeap;

pub type Result<T> = std::result::Result<T, Box<dyn std::error::Error>>;
pub static mut DEBUG: bool = false;
pub static mut DEBUG_TRACE_EXECUTION: bool = false;
pub static mut DEBUG_TRACE_GC: bool = true;
pub static mut DEBUG_STRESS_GC: bool = true;
pub fn trace_execution() -> bool {
    unsafe { DEBUG || DEBUG_TRACE_EXECUTION }
}
pub fn trace_gc() -> bool {
    unsafe { DEBUG || DEBUG_TRACE_GC }
}
pub fn stress_gc() -> bool {
    unsafe { DEBUG || DEBUG_STRESS_GC }
}
pub fn debug() -> bool {
    unsafe { DEBUG }
}

#[macro_export]
macro_rules! debugln {
     ($($arg:tt)*) => {
         {
             if(super::debug()) {
                println!($($arg)*);
             }
         }
    };
}

#[macro_export]
macro_rules! debug {
     ($($arg:tt)*) => {
         {
            if(super::debug()) {
                print!($($arg)*);
            }
         }
    };
}

#[macro_export]
macro_rules! operand {
     ( $($x:expr ),* ) => {
         {
            let mut tmp_buff = [0;4];
            let mut _idx = 0;
            $(
                tmp_buff[_idx] = $x;
                _idx += 1;
            )*
            tmp_buff
        }
    };
}
