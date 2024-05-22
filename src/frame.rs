use std::fmt::Display;

#[derive(Debug, PartialEq)]
pub struct Frame {
    command: Command,
    length: usize,
    pub data: Vec<u8>,
}

impl Frame {
    pub fn new(command: Command, length: usize, src_data: &[u8]) -> Self {
        let mut frame = Frame { command, length, data: Vec::with_capacity(length) };
        frame.copy_from(src_data);
        frame
    }

    pub fn is_partial(&self) -> bool {
        self.data.len() < self.length
    }

    pub fn copy_from(&mut self, src: &[u8]) {
        // `src_data.len()` should be smaller than `length`
        assert!(src.len() <= self.length);
        self.data.extend_from_slice(src);
    }
}

impl Display for Frame {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let sub = self.data.iter().cloned().take(8).collect::<Vec<_>>();
        let data_str = String::from_utf8_lossy(&sub).replace('\n', "\\n");
        write!(f, "<{:?}, {}, {}...>", self.command, self.length, data_str)
    }
}

#[derive(Debug, PartialEq)]
pub enum Command {
    Save,
}

// Convert `u8` into a `Command` or fail
impl TryFrom<u8> for Command {
    type Error = &'static str;

    fn try_from(value: u8) -> std::result::Result<Self, Self::Error> {
        match value {
            b'&' => Ok(Self::Save),
            _ => Err("invalid command"),
        }
    }
}
