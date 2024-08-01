use std::{error::Error, fmt::{Display, UpperHex}};
use crate::value::Value;

#[derive(Debug)]
pub struct ByteCodeError {
    byte: u8,
    message: String,
}

impl Display for ByteCodeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Invalid bytecode: {:#04x}, {}", self.byte, self.message)
    }
}

impl Error for ByteCodeError {}

impl From<(&u8, &str)> for ByteCodeError {
    fn from((byte, message): (&u8, &str)) -> Self {
        ByteCodeError { byte: *byte, message: message.to_string() }
    }
}

#[repr(u8)]
#[derive(Copy, Clone)]
pub enum OpCode {
    OpConstant = 0x00,
    OpConstantLong = 0x01,
    OpReturn = 0x02,
}

impl Display for OpCode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::OpConstant => write!(f, "OP_CONSTANT"),
            Self::OpConstantLong => write!(f, "OP_CONSTANT_LONG"),
            Self::OpReturn => write!(f, "OP_RETURN")
        }
    }
}

impl UpperHex for OpCode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:X}", *self as u8)
    }
}

impl TryFrom<&u8> for OpCode {
    type Error = ByteCodeError;

    fn try_from(value: &u8) -> Result<Self, Self::Error> {
        match value {
            0x00 => Ok(Self::OpConstant),
            0x01 => Ok(Self::OpConstantLong),
            0x02 => Ok(Self::OpReturn),
            _ => Err((value, "Unknown OpCode").into())
        }
    }
}

pub struct Chunk {
    code: Vec<u8>,
    constants: Vec<Value>,
    // TODO Challenge: Implement run line encoding for lines
    lines: Vec<i32>,
}

impl Chunk {
    pub fn new() -> Self {
        Chunk { code: Vec::new(), constants: Vec::new(), lines: Vec::new() }
    }

    pub fn write_instruction(&mut self, instruction: OpCode, line: i32) {
        self.code.push(instruction as u8);
        self.lines.push(line);
    }

    pub fn write_constant(&mut self, constant: Value) -> usize {
        self.constants.push(constant);
        self.constants.len() - 1
    }

    pub fn write_constant_location(&mut self, location: u8, line: i32) {
        self.code.push(location);
        self.lines.push(line);
    }

    pub fn write_long_constant_location(&mut self, location: u16, line: i32) {
        let bytes = location.to_le_bytes();
        self.code.extend_from_slice(&bytes);
        self.lines.push(line);
    }
}

impl Display for Chunk {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut iter = self.code.iter().enumerate();
        let mut count = 0;
        while let Some((i, instruction)) = iter.next() {
            // Write common header
            let op_code: OpCode = instruction.try_into().unwrap();
            write!(f, "{:#06X} {:04} {: <15}", i, self.lines[count], format!("{}", op_code))?;

            // Write specific data
            match op_code {
                OpCode::OpConstant => {
                    let location = *iter.next().unwrap().1;
                    write!(f, "\t {:#04X} {}\n", location, self.constants[location as usize])?
                },
                OpCode::OpConstantLong => {
                    let bytes = [*iter.next().unwrap().1, *iter.next().unwrap().1];
                    let location = u16::from_le_bytes(bytes);
                    write!(f, "\t {:#06X} {}\n", location, self.constants[location as usize])?
                },
                OpCode::OpReturn => write!(f, "\n")?,
            }

            count += 1;
        }

        Ok(())
    }
}