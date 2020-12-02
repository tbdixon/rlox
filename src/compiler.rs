use crate::scanner::TokenType::*;
use crate::scanner::Scanner;

pub fn compile(source: &str) {
    let mut scanner = Scanner::new(source.trim());
    loop {
        let current_token = scanner.scan_token();
        println!("{:?}", current_token);
        if current_token.is_eof() {
            break;
        }
    }
}

