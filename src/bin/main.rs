use std::env;
use std::io::prelude::*;
use std::io;
use std::fs;
use rlox::Result;
use rlox::vm::VM;

fn main() -> Result<()>{
    let mut vm = VM::new();
    let args: Vec<String> = env::args().collect();
    // First arg is the binary name, if there are no other arguments run a REPL
    // If there is a single argument, parse that as the name of a Lox script
    // If there are more than one arguments, panic.
    match args.len() {
        1 => {
            repl(vm)?
        },
        2 => {
            let script = fs::read_to_string(&args[1]).expect(&format!("Unable to read script {}", args[1]));
            vm.interpret(&script);
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
    }
}
