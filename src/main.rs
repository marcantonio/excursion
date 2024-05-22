use tokio::net::{TcpListener, TcpStream};

use connection::Connection;

mod connection;
mod frame;

const BIND_ADDR: &str = "127.0.0.1";
const BIND_PORT: &str = "7001";

type Error = Box<dyn std::error::Error>;
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
    let (reader, _) = socket.split();
    let mut connection = Connection::new(reader);

    while let Some(frame) = connection.read_frame().await? {
        println!("{}", frame);
    }
    Ok(())
}
