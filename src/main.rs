mod chunk;
mod value;
mod vm;

use chunk::{Chunk, OpCode};
use value::Value;
use vm::{VMOptions, VM};

fn main() {
    let options = VMOptions { debug_trace: true };
    let mut vm = VM::new(options);
    let mut chunk = Chunk::new();

    let location = chunk.write_constant(Value::Double(1.2));
    chunk.write_instruction(OpCode::Constant, 123);
    chunk.write_constant_location(location as u8, 123);

    let location = chunk.write_constant(Value::Double(3.4));
    chunk.write_instruction(OpCode::Constant, 123);
    chunk.write_constant_location(location as u8, 123);

    chunk.write_instruction(OpCode::Add, 123);

    let location = chunk.write_constant(Value::Double(5.6));
    chunk.write_instruction(OpCode::Constant, 123);
    chunk.write_constant_location(location as u8, 123);

    chunk.write_instruction(OpCode::Divide, 123);
    chunk.write_instruction(OpCode::Negate, 123);

    chunk.write_instruction(OpCode::Return, 123);
    _ = vm.interpret(chunk);
}
