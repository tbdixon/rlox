use std::env;
use std::io::prelude::*;
use std::io;
use std::fs;
use rlox::Result;
use rlox::vm::VM;

fn main() -> Result<()>{
    let vm = VM::new();
    let args: Vec<String> = env::args().collect();
    match args.len() {
        1 => {
            repl(vm)?
        },
        2 => {
            let script = fs::read_to_string(&args[1]).expect(&format!("Unable to read script {}", args[1]));
            print!("{}",script);
        },
        _ => {
            panic!("Invalid arguments received: {:?}", args);
        }
    };
    Ok(())
}

fn repl(mut vm: VM) -> Result<()> {
    loop {
        let mut line = String::new();
        print!("> ");
        io::stdout().flush()?;
        io::stdin().read_line(&mut line)?;
        vm.interpret(&line[..]);
        println!("{}",line);
    }
}
