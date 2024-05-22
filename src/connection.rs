use std::cmp;

use tokio::io::{AsyncRead, AsyncReadExt};

use crate::frame::{Command, Frame};

pub struct Connection<SocketRx> {
    rx: SocketRx,
    buffer: Vec<u8>,
    cursor: usize,
}

type Result<T> = crate::Result<T>;

impl<SocketRx: AsyncRead + Unpin> Connection<SocketRx> {
    pub fn new(reader: SocketRx) -> Self {
        Connection { rx: reader, buffer: Vec::with_capacity(4), cursor: 0 }
    }

    fn remaining_rx_capacity(&self) -> usize {
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
        // XXX: What about a partial frame at the end of the buffer?
        let n = cmp::min(self.rx_buffer().len(), len);
        let mut frame = Frame::new(cmd, len, &self.rx_buffer()[..n]);
        self.cursor += n;

        // While we don't have the whole frame in rx_buffer, keep reading
        while frame.is_partial() {
            // Any EOF here will be a partial frame and thus an error
            let bytes_read = self.read().await?;
            if bytes_read == 0 {
                return Err("data: connection reset by peer".into());
            }

            // Take the lesser of the bytes required to complete the frame or
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

    // Read from the socket and write to `self.buffer`.
    async fn read(&mut self) -> Result<usize> {
        // All of this data has been copied so reset the buffer and cursor. Also prevents
        // `read_buf()` from managing its own capacity by never reading when full (it's
        // hardcoded to grow by 64 bytes at a time...)
        if self.remaining_rx_capacity() == 0 {
            self.reset_cursor()
        }

        // `read_buf()` stores its own interal cursor and manages the buffer length (not cap)
        let n = self.rx.read_buf(&mut self.buffer).await?;
        println!("read {}", n);
        println!("{}", self.buffer.capacity());
        println!("{:?}", String::from_utf8(self.buffer.clone()).unwrap());
        Ok(n)
    }
}

#[cfg(test)]
mod tests {
    use tokio_test::io::Builder;
    use super::*;

    #[tokio::test]
    async fn reader() {
        let reader = Builder::new()
            .read(b"&8|testdata")
            .build();

        let mut connection = Connection::new(reader);
        let frame = connection.read_frame().await.unwrap().unwrap();

        assert!(frame == Frame::new(Command::Save, 8, b"testdata"));
        assert!(!frame.is_partial());
    }
}
