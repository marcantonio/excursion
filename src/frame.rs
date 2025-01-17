use std::fmt::Display;

#[derive(Debug, PartialEq)]
pub struct Frame {
    pub ty: FrameType,
    pub payload: Vec<u8>,
    pub payload_len: usize,
    pub segment_lens: Vec<usize>,
}

impl Frame {
    pub fn new(ty: FrameType, payload: &[u8], segment_lens: &[usize]) -> Self {
        let payload_len = segment_lens.iter().sum();
        let mut frame = Frame {
            ty,
            payload: Vec::with_capacity(payload_len),
            payload_len,
            segment_lens: segment_lens.to_owned(),
        };
        frame.copy_from(payload);
        frame
    }

    pub fn is_partial(&self) -> bool {
        self.payload.len() < self.payload_len
    }

    pub fn copy_from(&mut self, src: &[u8]) {
        // `src.len()` should be smaller than `length`
        assert!(src.len() <= self.payload_len);
        self.payload.extend_from_slice(src);
    }

    // test?
    pub fn iter_segments(&self) -> impl Iterator<Item = &[u8]> {
        let mut offest = 0;
        self.segment_lens.iter().map(move |&len| {
            let start = offest;
            let end = start + len;
            let slice = &self.payload[start..end];
            offest = end;
            slice
        })
    }
}

impl Display for Frame {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let sub = self.payload.iter().cloned().take(8).collect::<Vec<_>>();
        let data_str = String::from_utf8_lossy(&sub).replace('\n', "\\n");
        write!(f, "<{}, {}, {}...>", self.ty, self.payload_len, data_str)
    }
}

#[derive(Debug, PartialEq)]
pub enum FrameType {
    Data,
    DirList,
    Err,
    ExpandFileName,
    FileExists,
    Open,
    Save,
}

// Convert `u8` into a `Command` or fail
impl TryFrom<u8> for FrameType {
    type Error = &'static str;

    fn try_from(value: u8) -> std::result::Result<Self, Self::Error> {
        match value {
            b'^' => Ok(Self::Data),
            b'~' => Ok(Self::DirList),
            b'?' => Ok(Self::FileExists),
            b'*' => Ok(Self::ExpandFileName),
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
