use std::{slice::Iter, error::Error, fmt::{Display, Write, UpperHex}};
use crate::value::Value;

#[derive(Debug, Clone)]
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
#[derive(Debug, Copy, Clone)]
/// All instruction OpCodes.
pub enum OpCode {
    Constant = 0x00,
    ConstantLong = 0x01,
    Negate = 0x02,
    Add = 0x03,
    Subtract = 0x04,
    Multipliy = 0x05,
    Divide = 0x06,
    Return = 0x07,
}

impl Display for OpCode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Constant => write!(f, "OP_CONSTANT"),
            Self::ConstantLong => write!(f, "OP_CONSTANT_LONG"),
            Self::Negate => write!(f, "OP_NEGATE"),
            Self::Add => write!(f, "OP_ADD"),
            Self::Subtract => write!(f, "OP_SUBTRACT"),
            Self::Multipliy => write!(f, "OP_MULTIPLY"),
            Self::Divide => write!(f, "OP_DIVIDE"),
            Self::Return => write!(f, "OP_RETURN")
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
            0x00 => Ok(Self::Constant),
            0x01 => Ok(Self::ConstantLong),
            0x02 => Ok(Self::Negate),
            0x03 => Ok(Self::Add),
            0x04 => Ok(Self::Subtract),
            0x05 => Ok(Self::Multipliy),
            0x06 => Ok(Self::Divide),
            0x07 => Ok(Self::Return),
            _ => Err((value, "Unknown OpCode").into())
        }
    }
}

#[derive(Debug, Clone)]
/// A struct representing a block of code. The chunk contains the instruction [`OpCode`] bytes, the constants, and the lines of each instruction.
pub struct Chunk {
    code: Vec<u8>,
    constants: Vec<Value>,
    // TODO Challenge: Implement run line encoding for lines
    lines: Vec<i32>,
}

impl Chunk {
    /// Create a new empty chunk.
    pub fn new() -> Self {
        Chunk { code: Vec::new(), constants: Vec::new(), lines: Vec::new() }
    }

    /// Write an instruction to the chunk's code.
    /// 
    /// # Arguments:
    /// - `instruction` - The [`u8`] representation of the instruction.
    /// - `line` - The [`i32`] index of the line in the chunk's lines.
    pub fn write_instruction(&mut self, instruction: u8, line: i32) {
        self.code.push(instruction);
        self.lines.push(line);
    }

    /// Add a constant to the chunk's constants and returns the index.
    /// 
    /// # Arguments:
    /// - `constant` - The [`Value`] to be added to the chunk's constants.
    /// 
    /// # Returns:
    /// The [`usize`] index of the added constant.
    pub fn write_constant(&mut self, constant: Value) -> usize {
        self.constants.push(constant);
        self.constants.len() - 1
    }

    /// Write the one byte location operand for a [`OpCode::Constant`].
    /// 
    /// # Arguments:
    /// - `location` - The [`u8`] index of a constant in the chunk's constants.
    /// - `line` - The [`i32`] index of the line in the chunk's lines.
    pub fn write_constant_location(&mut self, location: u8, line: i32) {
        self.code.push(location);
        self.lines.push(line);
    }

    /// Write the two byte location operand for a [`OpCode::ConstantLong`].
    /// 
    /// # Arguments:
    /// - `location` - The [`u16`] index of a constant in the chunk's constants.
    /// - `line` - The [`i32`] index of the line in the chunk's lines.
    pub fn write_long_constant_location(&mut self, location: u16, line: i32) {
        let bytes = location.to_le_bytes();
        self.code.extend_from_slice(&bytes);
        self.lines.push(line);
    }

    /// Retrieve an iter to the OpCodes
    pub fn get_code_iter(&self) -> Iter<'_, u8> {
        self.code.iter()
    }

    /// Retrieve a slice of the chunk's constants.
    pub fn get_constants(&self) -> &[Value] {
        &self.constants
    }

    /// Retrieve a slice of the chunk's lines.
    pub fn get_lines(&self) -> &[i32] {
        &self.lines
    }
}

/// Display each instruction in the chunk.
impl Display for Chunk {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut iter = self.code.iter();
        let mut instruction_counter = 0;

        while let Some(instruction) = iter.next() {
            write!(f, "{:#06X} ", instruction_counter)?;
            let op_code: OpCode = instruction.try_into().unwrap();

            let mut output = String::new();
            write_instruction(&mut output, &mut iter, op_code, self.lines[instruction_counter], &self.constants)?;
            write!(f, "{}", output)?;
            instruction_counter += 1;
        }

        Ok(())
    }
}

/// Write the instruction and it's information out in formatted way.
pub fn write_instruction(output: &mut String, iter: &mut Iter<'_, u8>, op_code: OpCode, line: i32, constants: &[Value]) -> std::fmt::Result {
    // Write common header
    write!(output, "{:04} {: <15}", line, format!("{}", op_code))?;

    // Write specific data
    match op_code {
        OpCode::Constant => {
            let location = *iter.next().unwrap();
            writeln!(output, "\t {:#04X}   {}", location, constants[location as usize])
        },
        OpCode::ConstantLong => {
            let location = u16::from_le_bytes([*iter.next().unwrap(), *iter.next().unwrap()]);
            writeln!(output, "\t {:#06X} {}", location, constants[location as usize])
        },
        _ => writeln!(output),
    }
}