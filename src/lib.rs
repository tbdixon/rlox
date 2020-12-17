#![allow(non_camel_case_types)]
pub mod chunk;
pub mod compiler;
pub mod debug;
pub mod scanner;
pub mod vm;
pub mod value;
pub mod natives;
pub mod precedence;

pub type Result<T> = std::result::Result<T, Box<dyn std::error::Error>>;

pub static mut DEBUG: bool = true;
pub fn debug() -> bool {
    unsafe {
        DEBUG
    }
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
