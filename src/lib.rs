#![allow(non_camel_case_types)]
pub mod chunk;
pub mod debug;
pub mod vm;

#[macro_export]
macro_rules! debugln {
     ($($arg:tt)*) => {
         {
            use std::env;
            if let Ok(_) = env::var("RUST_DEBUG") {
                println!($($arg)*);
            }
         }
    };
}

#[macro_export]
macro_rules! debug {
     ($($arg:tt)*) => {
         {
            use std::env;
            if let Ok(_) = env::var("RUST_DEBUG") {
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


