mod chunk;
mod value;

use chunk::{Chunk, OpCode};
use value::Value;

fn main() {
    let mut chunk = Chunk::new();

    let location = chunk.write_constant(Value::Double(1.2));
    chunk.write_instruction(OpCode::OpConstant, 123);
    chunk.write_constant_location(location as u8, 123);

    let location = chunk.write_constant(Value::Double(2.4));
    chunk.write_instruction(OpCode::OpConstantLong, 246);
    chunk.write_long_constant_location(location as u16, 246);

    chunk.write_instruction(OpCode::OpReturn, 123);
    println!("{}", chunk);
}
