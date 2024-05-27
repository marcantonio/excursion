use tokio::{
    fs::File,
    io::AsyncReadExt,
    net::{TcpListener, TcpStream},
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
            process_command(socket).await.unwrap();
        });
    }
}

async fn process_command(mut socket: TcpStream) -> Result<()> {
    let (reader, writer) = socket.split();
    let mut connection = Connection::new(reader, writer);

    while let Some(frame) = connection.read_frame().await? {
        use frame::FrameType;

        println!("command: {}", frame.command);

        match frame.command {
            FrameType::Open => {
                let filename = String::from_utf8_lossy(&frame.data);
                match open_file(&filename).await {
                    Ok(contents) => {
                        connection
                            .write_frame(Frame::new(FrameType::Data, contents.len(), contents.as_bytes()))
                            .await?
                    },
                    Err(e) => {
                        let err_msg = e.to_string();
                        connection
                            .write_frame(Frame::new(FrameType::Err, err_msg.len(), err_msg.as_bytes()))
                            .await?;
                    },
                }
            },
            FrameType::Save => todo!(),
            _ => unimplemented!(),
        }
    }
    Ok(())
}

async fn open_file(filename: &str) -> Result<String> {
    let mut file = File::open(filename).await?;
    let mut contents = String::new();
    file.read_to_string(&mut contents).await?;
    Ok(contents)
}
