use std::fs;
use std::io;
use std::path::Path;

use frame::FrameType;
use tokio::{
    fs::File,
    io::AsyncReadExt,
    net::{
        tcp::{ReadHalf, WriteHalf},
        TcpListener, TcpStream,
    },
};

use connection::Connection;

use crate::frame::Frame;

mod connection;
mod frame;

const BIND_ADDR: &str = "127.0.0.1";
const BIND_PORT: &str = "7001";

type Error = Box<dyn std::error::Error + Send + Sync>;
type Result<T> = std::result::Result<T, Error>;

#[tokio::main]
async fn main() {
    let connect_str = format!("{}:{}", BIND_ADDR, BIND_PORT);
    let listener = TcpListener::bind(&connect_str).await.unwrap();

    println!("Listening on {}", &connect_str);

    loop {
        let (socket, _) = listener.accept().await.unwrap();

        println!("Accepted");

        tokio::spawn(async move {
            process_frames(socket).await.unwrap();
        });
    }
}

async fn process_frames(mut socket: TcpStream) -> Result<()> {
    let (reader, writer) = socket.split();
    let mut connection = Connection::new(reader, writer);

    while let Some(frame) = connection.read_frame().await? {
        use frame::FrameType::*;

        println!("frame type: {}", frame.ty);

        let frame_data = String::from_utf8_lossy(&frame.data);
        match frame.ty {
            DirList => handle_dir_list(&mut connection, &frame_data).await?,
            FileExists => handle_file_exists(&mut connection, &frame_data).await?,
            Open => handle_open(&mut connection, &frame_data).await?,
            Save => todo!(),
            _ => unimplemented!(),
        }
    }
    Ok(())
}

async fn handle_file_exists<'a>(
    connection: &mut Connection<ReadHalf<'a>, WriteHalf<'a>>, filename: &str,
) -> Result<()> {
    let path = Path::new(filename);
    let exists = if path.exists() { "1" } else { "0" };
    connection.write_frame(Frame::new(FrameType::Data, 1, exists.as_bytes())).await
}

async fn handle_dir_list<'a>(
    connection: &mut Connection<ReadHalf<'a>, WriteHalf<'a>>, dirname: &str,
) -> Result<()> {
    let entries = fs::read_dir(dirname)?
        .map(|entry| entry.map(|e| e.file_name().into_string().unwrap_or_default()))
        .collect::<core::result::Result<Vec<_>, io::Error>>()?
        .join(", "); // XXX: escape commas

    connection.write_frame(Frame::new(FrameType::Data, entries.len(), entries.as_bytes())).await
}

async fn handle_open<'a>(
    connection: &mut Connection<ReadHalf<'a>, WriteHalf<'a>>, filename: &str,
) -> Result<()> {
    match open_file(filename).await {
        Ok(contents) => {
            connection.write_frame(Frame::new(FrameType::Data, contents.len(), contents.as_bytes())).await
        },
        Err(e) => {
            let err_msg = e.to_string();
            println!("error: {}", err_msg);
            connection.write_frame(Frame::new(FrameType::Err, err_msg.len(), err_msg.as_bytes())).await
        },
    }
}

async fn open_file(filename: &str) -> Result<String> {
    let mut file = File::open(filename).await?;
    let mut contents = String::new();
    file.read_to_string(&mut contents).await?;
    Ok(contents)
}
