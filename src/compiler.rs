use crate::scanner::Scanner;
use crate::scanner::TokenType::*;

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
