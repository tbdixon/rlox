use rlox::memory::LoxHeap;
use rlox::vm::VM;
use rlox::Result;
use std::cell::RefCell;
use std::env;
use std::fs;
use std::io;
use std::io::prelude::*;
use std::rc::Rc;

fn main() -> Result<()> {
    /* Set up the global LoxHeap that will be used to allocate objects throughout our compiler and
     * VM during runtime
     *
     * Using a direct raw pointer since lazy_static does not seem to work in LoxHeap (*mut u8
     * doesn't implement Send/Sync. rlox is run as a single threaded VM so at the moment there are
     * no concerns with threading issues around an unlocked global mutable variable such as this.
     */
    let vm = Rc::new(RefCell::new(VM::new()));
    let mut heap = LoxHeap::new(vm.clone());
    unsafe {
        rlox::HEAP = &mut heap;
    }
    std::mem::forget(heap);
    vm.borrow_mut().define_natives();

    /* Don't read from environment variable in --release. This ensures that the global rlox::DEBUG
     * is definitely false so compiler can get rid of all conditional checks (non-trivial cost with
     * every loop of the core VM run function). */
    #[cfg(debug_assertions)]
    if let Ok(debug) = env::var("RLOX_DEBUG") {
        unsafe {
            rlox::DEBUG = debug == "1";
        }
    }

    let args: Vec<String> = env::args().collect();
    // First arg is the binary name, if there are no other arguments run a REPL
    // If there is a single argument, parse that as the name of a Lox script
    // If there are more than one arguments, panic.
    match args.len() {
        1 => repl(vm)?,
        2 => {
            let script = fs::read_to_string(&args[1]).expect(&format!("Unable to read script {}", args[1]));
            vm.borrow_mut().interpret(&script)?;
        }
        _ => {
            panic!("Invalid arguments received: {:?}", args);
        }
    };
    Ok(())
}

fn repl(vm: Rc<RefCell<VM>>) -> Result<()> {
    loop {
        let mut line = String::new();
        print!("> ");
        io::stdout().flush()?;
        io::stdin().read_line(&mut line)?;
        vm.borrow_mut().interpret(&line[..])?;
    }
}
