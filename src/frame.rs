use std::fmt::Display;

#[derive(Debug, PartialEq)]
pub struct Frame {
    pub command: FrameType,
    pub data_len: usize,
    pub data: Vec<u8>,
}

impl Frame {
    pub fn new(command: FrameType, length: usize, src_data: &[u8]) -> Self {
        let mut frame = Frame { command, data_len: length, data: Vec::with_capacity(length) };
        frame.copy_from(src_data);
        frame
    }

    pub fn is_partial(&self) -> bool {
        self.data.len() < self.data_len
    }

    pub fn copy_from(&mut self, src: &[u8]) {
        // `src_data.len()` should be smaller than `length`
        assert!(src.len() <= self.data_len);
        self.data.extend_from_slice(src);
    }
}

impl Display for Frame {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let sub = self.data.iter().cloned().take(8).collect::<Vec<_>>();
        let data_str = String::from_utf8_lossy(&sub).replace('\n', "\\n");
        write!(f, "<{}, {}, {}...>", self.command, self.data_len, data_str)
    }
}

#[derive(Debug, PartialEq)]
pub enum FrameType {
    Data,
    Err,
    Open,
    Save,
}

// Convert `u8` into a `Command` or fail
impl TryFrom<u8> for FrameType {
    type Error = &'static str;

    fn try_from(value: u8) -> std::result::Result<Self, Self::Error> {
        match value {
            b'^' => Ok(Self::Data),
            b'(' => Ok(Self::Open),
            b'&' => Ok(Self::Save),
            _ => Err("invalid command"),
        }
    }
}

impl Display for FrameType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}
