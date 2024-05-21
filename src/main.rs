use std::{cmp, error::Error, fmt::Display};

use tokio::{
    io::AsyncReadExt,
    net::{TcpListener, TcpStream},
};

const BIND_ADDR: &str = "127.0.0.1";
const BIND_PORT: &str = "7001";

#[tokio::main]
async fn main() {
    let connect_str = format!("{}:{}", BIND_ADDR, BIND_PORT);
    let listener = TcpListener::bind(&connect_str).await.unwrap();

    println!("Listening on {}", &connect_str);

    loop {
        let (socket, _) = listener.accept().await.unwrap();

        println!("Accepted");

        tokio::spawn(async move {
            process_command(socket).await.unwrap();
        });
    }
}

async fn process_command(socket: TcpStream) -> Result<()> {
    let mut connection = Connection::new(socket);

    while let Some(frame) = connection.read_frame().await? {
        println!("{}", frame);
    }
    Ok(())
}

type ExError = Box<dyn Error>;
type Result<T> = std::result::Result<T, ExError>;

#[derive(Debug)]
struct Frame {
    command: Command,
    length: usize,
    data: Vec<u8>,
}

impl Frame {
    fn new(command: Command, length: usize, src_data: &[u8]) -> Self {
        let mut frame = Frame { command, length, data: Vec::with_capacity(length) };
        frame.copy_from(src_data);
        frame
    }

    fn is_partial(&self) -> bool {
        self.data.len() < self.length
    }

    fn copy_from(&mut self, src: &[u8]) {
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

struct Connection {
    socket: TcpStream,
    buffer: Vec<u8>,
    cursor: usize,
}

impl Connection {
    fn remaining_capacity(&self) -> usize {
        self.buffer.capacity() - self.cursor
    }

    fn reset_cursor(&mut self) {
        self.buffer.clear();
        self.cursor = 0;
    }

    // The segment of the read buffer that hasn't been processed yet
    fn rx_buffer(&self) -> &[u8] {
        &self.buffer[self.cursor..]
    }
}

impl Connection {
    pub fn new(socket: TcpStream) -> Self {
        Connection { socket, buffer: Vec::with_capacity(48), cursor: 0 }
    }

    // Returns a single frame from the socket. More data may be read, but it will sit in
    // the buffer until the next call. An `Ok(None)` indicates that the socket was closed
    // on the far end. If it occurs mid-frame, it should be considered an error
    pub async fn read_frame(&mut self) -> Result<Option<Frame>> {
        // Get the command and frame length. Otherwise, return `None` indicating EOF on
        // the socket. This is where a normal socket close on the far end will occur
        let (cmd, len) = match self.get_preamble().await? {
            Some((cmd, len)) => (cmd, len),
            None => return Ok(None),
        };

        // Always take the min of the rx_buffer and the frame length. Don't make the frame
        // worry about this. The buffer will either truncate the frame (requiring another
        // read) or contain the whole frame
        // XXX: Should this be cap? What about a partial frame at the end of the buffer?
        let n = cmp::min(self.rx_buffer().len(), len);
        let mut frame = Frame::new(cmd, len, &self.rx_buffer()[..n]);
        self.cursor += n;

        // While we don't have the whole frame in rx_buffer, keep reading
        while frame.is_partial() {
            // If rx_buffer is full, clear it and the cursor
            if self.remaining_capacity() == 0 {
                self.reset_cursor()
            }

            // Any EOF here will be a partial frame and thus an error
            let bytes_read = self.read().await?;
            if bytes_read == 0 {
                return Err("data: connection reset by peer".into());
            }

            // Take the lesser of the bytes needed to complete the frame, or
            // `bytes_read`. This ensures we copy as much as possible without overflowing
            // the frame
            let bytes_needed = len - frame.data.len();
            let n = cmp::min(bytes_read, bytes_needed);
            frame.copy_from(&self.rx_buffer()[..n]);
            self.cursor += n;
        }

        Ok(Some(frame))
    }

    // Get the command and the frame length from the buffer, reading if necessary. Must be
    // called only at the beginning of a frame
    async fn get_preamble(&mut self) -> Result<Option<(Command, usize)>> {
        loop {
            // Search for a `|` from the cursor on. If we get it, assume that the first
            // available byte is the command. If we don't get the char, read more.
            //
            // Note: Because this function must be called at the beginning of a frame, we
            // can assume the cursor starts at a command
            if let Some(i) = self.rx_buffer().iter().position(|&x| x == b'|') {
                let cmd = (*self.rx_buffer().first().unwrap()).try_into()?;
                let len = atoi::atoi::<usize>(&self.rx_buffer()[1..i]).ok_or("invalid frame size")?;
                // Increment the cursor the length of the preamble + 1 to start at the data segment
                self.cursor += i + 1;
                return Ok(Some((cmd, len)));
            }

            // Read until we have the whole preamble or the socket closes
            let n = self.read().await?;
            if n == 0 {
                // If there's still data in the buffer (in the case of a partial
                // preamble) and we get an EOF, signal an error
                if !self.rx_buffer().is_empty() {
                    return Err("preamble: connection reset by peer".into());
                }
                // Normal close
                return Ok(None);
            }
        }
    }

    // Read from the socket and write to `self.buffer`. XXX read()
    async fn read(&mut self) -> Result<usize> {
        let n = self.socket.read_buf(&mut self.buffer).await?;
        println!("read {}", n);
        println!("{:?}", String::from_utf8(self.buffer.clone()).unwrap());

        // XXX this doesn't work yet
        if n == 0 && !self.rx_buffer().is_empty() {
            return Err("connection reset by peer".into());
        }
        Ok(n)
    }
}

#[derive(Debug)]
enum Command {
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
