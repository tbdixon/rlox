#![allow(non_camel_case_types)]
pub mod chunk;

#[macro_export]
macro_rules! operand {
     ( $( $x:expr ),* ) => {
         {
            let mut tmp_buff = [0;8];
            let mut _idx = 0;
            $(
                tmp_buff[_idx] = $x;
                _idx += 1;
             )*
            tmp_buff
        }
    };
}


