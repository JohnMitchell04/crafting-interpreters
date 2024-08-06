use std::{error::Error, fmt::{Display, UpperHex, Write}, slice::Iter};
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
    Constant =      0x00,
    ConstantLong =  0x01,
    Nil =           0x02,
    True =          0x03,
    False =         0x04,
    Pop =           0x05,
    GetGlobal =     0x06,
    DefineGlobal =  0x07,
    SetGlobal =     0x08,
    GetLocal =      0x09,
    SetLocal =      0x0A,
    Equal =         0x0B,
    Greater =       0x0C,
    Less =          0x0D,
    Add =           0x0E,
    Subtract =      0x0F,
    Multipliy =     0x10,
    Divide =        0x11,
    Not =           0x12,
    Negate =        0x13,
    Print =         0x14,
    Jump =          0x15,
    JumpIfFalse =   0x16,
    Loop =          0x17,
    Return =        0x18,
}

impl Display for OpCode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Constant => write!(f, "OP_CONSTANT"),
            Self::ConstantLong => write!(f, "OP_CONSTANT_LONG"),
            Self::Nil => write!(f, "OP_NIL"),
            Self::True => write!(f, "OP_TRUE"),
            Self::False => write!(f, "OP_FALSE"),
            Self::Pop => write!(f, "OP_POP"),
            Self::GetGlobal => write!(f, "OP_GET_GLOBAL"),
            Self::DefineGlobal => write!(f, "OP_DEFINE_GLOBAL"),
            Self::SetGlobal => write!(f, "OP_SET_GLOBAL"),
            Self::GetLocal => write!(f, "OP_GET_LOCAL"),
            Self::SetLocal => write!(f, "OP_SET_LOCAL"),
            Self::Equal => write!(f, "OP_EQUAL"),
            Self::Greater => write!(f, "OP_GREATER"),
            Self::Less => write!(f, "OP_LESS"),
            Self::Add => write!(f, "OP_ADD"),
            Self::Subtract => write!(f, "OP_SUBTRACT"),
            Self::Multipliy => write!(f, "OP_MULTIPLY"),
            Self::Divide => write!(f, "OP_DIVIDE"),
            Self::Not => write!(f, "OP_NOT"),
            Self::Negate => write!(f, "OP_NEGATE"),
            Self::Print => write!(f, "OP_PRINT"),
            Self::Jump => write!(f, "OP_JUMP"),
            Self::JumpIfFalse => write!(f, "OP_JUMP_IF_FALSE"),
            Self::Loop => write!(f, "OP_LOOP"),
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
            0x02 => Ok(Self::Nil),
            0x03 => Ok(Self::True),
            0x04 => Ok(Self::False),
            0x05 => Ok(Self::Pop),
            0x06 => Ok(Self::GetGlobal),
            0x07 => Ok(Self::DefineGlobal),
            0x08 => Ok(Self::SetGlobal),
            0x09 => Ok(Self::GetLocal),
            0x0A => Ok(Self::SetLocal),
            0x0B => Ok(Self::Equal),
            0x0C => Ok(Self::Greater),
            0x0D => Ok(Self::Less),
            0x0E => Ok(Self::Add),
            0x0F => Ok(Self::Subtract),
            0x10 => Ok(Self::Multipliy),
            0x11 => Ok(Self::Divide),
            0x12 => Ok(Self::Not),
            0x13 => Ok(Self::Negate),
            0x14 => Ok(Self::Print),
            0x15 => Ok(Self::Jump),
            0x16 => Ok(Self::JumpIfFalse),
            0x17 => Ok(Self::Loop),
            0x18 => Ok(Self::Return),
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

    /// Write the destination of a jump statement in the given offset position
    /// 
    /// # Arguments:
    /// - `offset` - The [`usize`] offset to write the value to.
    /// - `value` - The [`u16`] value to write. 
    pub fn write_jump_dest(&mut self, offset: usize, value: u16) {
        let bytes = value.to_le_bytes();
        self.code[offset] = bytes[0];
        self.code[offset + 1] = bytes[1]
    }

    /// Retrieve the length of instructions emitted so far.
    pub fn get_code_count(&self) -> usize {
        self.code.len()
    }

    /// Get a slice of the chunk's code.
    pub fn get_code(&self) -> &[u8] {
        &self.code
    }

    /// Retrieve an iter to the OpCodes.
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

    pub fn clear_code(&mut self) {
        self.code.clear()
    }
}

/// Display each instruction in the chunk.
impl Display for Chunk {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut iter = self.code.iter();
        let mut instruction_counter = 0;

        writeln!(f, "Count  | Line | {:<16} | Operand | Value", format!("Op Code"))?;

        while let Some(instruction) = iter.next() {
            write!(f, "{:#06X} | ", instruction_counter)?;
            let op_code: OpCode = instruction.try_into().unwrap();

            let mut output = String::new();
            output_instruction(&mut output, &mut iter, op_code, self.lines[instruction_counter], &self.constants)?;
            write!(f, "{}", output)?;
            instruction_counter += 1;
        }

        Ok(())
    }
}

/// Write the instruction and it's information out in formatted way.
pub fn output_instruction(output: &mut String, iter: &mut dyn Iterator<Item = &u8>, op_code: OpCode, line: i32, constants: &[Value]) -> std::fmt::Result {
    // Write common header
    write!(output, "{:04} | {:<16} |", line, format!("{}", op_code))?;

    // Write specific data
    match op_code {
        OpCode::Constant => {
            let location = *iter.next().unwrap();
            writeln!(output, " {:<8}| {}", format!("{:#04X}", location), constants[location as usize])
        },
        OpCode::ConstantLong => {
            let location = u16::from_le_bytes([*iter.next().unwrap(), *iter.next().unwrap()]);
            writeln!(output, " {:<8}| {}", format!("{:#06X}", location), constants[location as usize])
        },
        OpCode::GetLocal => {
            let slot = *iter.next().unwrap();
            writeln!(output, " {:<8}|", format!("{:#04X}", slot))
        },
        OpCode::SetLocal => {
            let slot = *iter.next().unwrap();
            writeln!(output, " {:<8}|", format!("{:#04X}", slot))
        },
        OpCode::Jump => writeln!(output, " {:#06X}  |", u16::from_le_bytes([*iter.next().unwrap(), *iter.next().unwrap()])),
        OpCode::JumpIfFalse => writeln!(output, " {:#06X}  |", u16::from_le_bytes([*iter.next().unwrap(), *iter.next().unwrap()])),
        OpCode::Loop => writeln!(output, " {:#06X}  |", u16::from_le_bytes([*iter.next().unwrap(), *iter.next().unwrap()])),
        _ => writeln!(output, "{}|", " ".repeat(9)),
    }
}