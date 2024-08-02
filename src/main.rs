mod chunk;
mod value;
mod vm;
mod compiler;
mod scanner;

use std::{env, fs};
use vm::{InterpretError, VMOptions, VM};

fn repl(mut vm: VM) {
    // TODO: Improve this repl at some point
    loop {
        print!("> ");
        let mut input = String::new();
        _ = std::io::stdin().read_line(&mut input).unwrap();
        vm.interpret(input);
    }
}

fn run_file(mut vm: VM, path: String) {
    let input = fs::read_to_string(path.clone()).unwrap_or_else(|_| {
        println!("Error opening file {}.", {path});
        std::process::exit(74);
    });

    let result = vm.interpret(input);

    match result {
        Err(InterpretError::InterpretCompileError) => std::process::exit(65),
        Err(InterpretError::InterpretRuntimeError) => std::process::exit(70),
        Ok(_) => {},
    }
}

fn main() {
    let options = VMOptions { debug_trace: true };
    let vm = VM::new(options);
    let args: Vec<String> = env::args().collect();

    if args.len() == 1 {
        repl(vm);
    } else if args.len() == 2 {
        run_file(vm, args[2].clone());
    } else {
        println!("Usage: clox [path]");
        std::process::exit(64);
    }

    std::process::exit(0);
}
