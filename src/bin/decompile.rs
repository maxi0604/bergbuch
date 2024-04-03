use std::{
    env::args_os,
    fs,
    io::{stdin, IsTerminal, self},
    path::Path,
    process::ExitCode,
};


fn main() -> ExitCode {
    if args_os().len() > 2 {
        eprintln!("usage: lox [file]");
        return ExitCode::FAILURE
    }

    println!("{}", bergbuch::vm::chunk::OpCode::Return as u8);

    ExitCode::SUCCESS
}
