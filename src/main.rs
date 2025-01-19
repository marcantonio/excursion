use std::fs;
use std::io;
use std::path::Path;
use std::time::Duration;

use path_absolutize::Absolutize;
use tokio::time::sleep;
use tokio::{
    fs::File,
    io::AsyncReadExt,
    net::{
        tcp::{ReadHalf, WriteHalf},
        TcpListener, TcpStream,
    },
};

use connection::Connection;
use frame::FrameType;

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

        println!("accepted");

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

        let segments =
            frame.iter_segments().map(|s| std::str::from_utf8(s).unwrap_or("")).collect::<Vec<_>>();
        match frame.ty {
            DirList => handle_dir_list(&mut connection, &segments[0]).await?,
            ExpandFileName => handle_expand_file_name(&mut connection, &segments).await?,
            FileExists => handle_file_exists(&mut connection, &segments[0]).await?,
            Open => handle_open(&mut connection, &segments[0]).await?,
            Save => todo!(),
            _ => unimplemented!(),
        }
    }
    Ok(())
}

// async fn handle_expand_file_name<'a>(
//     connection: &mut Connection<ReadHalf<'a>, WriteHalf<'a>>, params: &[&str],
// ) -> Result<()> {
//     let (filename, directory) = match params {
//         [f, d] => Ok((f, d)),
//         _ => Err("handle_expand_file_name: bad segment"),
//     }?;
//     if filename.starts_with("~") && !filename.starts_with("~/") {
//         match expanduser::expanduser(filename) {
//             Ok(expanded) => {
//                 let abs = expanded.absolutize()?.to_path_buf();
//                 let path = abs.to_string_lossy();
//                 return connection
//                     .write_frame(Frame::new(FrameType::Data, path.as_bytes(), &[path.len()]))
//                     .await;
//             },
//             Err(_) => (),
//         }
//     }

//     let expanded = expanduser::expanduser(format!("{}/{}", directory, filename))?;
//     let abs = expanded.absolutize()?.to_path_buf();
//     let path = abs.to_string_lossy();
//     connection.write_frame(Frame::new(FrameType::Data, path.as_bytes(), &[path.len()])).await
// }

// async fn handle_expand_file_name<'a>(
//     connection: &mut Connection<ReadHalf<'a>, WriteHalf<'a>>, params: &[&str],
// ) -> Result<()> {
//     let (file, dir) = match params {
//         [f, d] => Ok((f, d)),
//         _ => Err("handle_expand_file_name: bad segment"),
//     }?;

//     println!("{} {}", dir, file);
//     let expanded = if file.starts_with("~") {
//         if let Ok(exfile) = expanduser::expanduser(file) {
//             Ok(exfile)
//         } else {
//             expanduser::expanduser(format!("{}/{}", dir, file))
//         }
//     } else if dir.starts_with("~") {
//         if let Ok(exdir) = expanduser::expanduser(dir) {
//             expanduser::expanduser(format!("{}/{}", exdir.to_string_lossy(), file))
//         } else {
//             expanduser::expanduser(format!("{}/{}", dir, file))
//         }
//     } else {
//         expanduser::expanduser(format!("{}/{}", dir, file))
//     }
//     .unwrap_or_else(|_| Path::new(&format!("{}/{}", dir, file)).to_path_buf());
//     //sleep(Duration::from_secs(2)).await;
//     println!("{:?}", expanded);
//     let abs = expanded.absolutize()?.to_path_buf();
//     let path = abs.to_string_lossy();
//     println!("{:?}", path);
//     connection.write_frame(Frame::new(FrameType::Data, path.as_bytes(), &[path.len()])).await
// }

async fn handle_expand_file_name<'a>(
    connection: &mut Connection<ReadHalf<'a>, WriteHalf<'a>>, params: &[&str],
) -> Result<()> {
    let (file, dir, default_dir) = match params {
        [f, d, dd] => Ok((f, d, dd)),
        _ => Err("handle_expand_file_name: bad segment"),
    }?;
    println!("{} {} {}", dir, file, default_dir);
    let expanded = expanduser::expanduser(format!("{}/{}", dir, file))?;
    println!("{:?}", expanded);
    let abs = expanded.absolutize()?.to_path_buf();
    let path = abs.to_string_lossy();
    println!("{:?}", path);
    connection.write_frame(Frame::new(FrameType::Data, path.as_bytes(), &[path.len()])).await
}

async fn handle_file_exists<'a>(
    connection: &mut Connection<ReadHalf<'a>, WriteHalf<'a>>, filename: &str,
) -> Result<()> {
    let path = Path::new(filename);
    let exists = if path.exists() { "1" } else { "0" };
    connection.write_frame(Frame::new(FrameType::Data, exists.as_bytes(), &[1])).await
}

async fn handle_dir_list<'a>(
    connection: &mut Connection<ReadHalf<'a>, WriteHalf<'a>>, dirname: &str,
) -> Result<()> {
    let entries = fs::read_dir(dirname)?
        .map(|entry| entry.map(|e| e.file_name().into_string().unwrap_or_default()))
        .collect::<core::result::Result<Vec<_>, io::Error>>()?
        .join(", "); // XXX: escape commas

    connection.write_frame(Frame::new(FrameType::Data, entries.as_bytes(), &[entries.len()])).await
}

async fn handle_open<'a>(
    connection: &mut Connection<ReadHalf<'a>, WriteHalf<'a>>, filename: &str,
) -> Result<()> {
    if filename == "./Cargo.toml" {
        //sleep(Duration::from_secs(1)).await;
    }
    match open_file(filename).await {
        Ok(contents) => {
            connection.write_frame(Frame::new(FrameType::Data, contents.as_bytes(), &[contents.len()])).await
        },
        Err(e) => {
            let err_msg = e.to_string();
            println!("error: {}", err_msg);
            connection.write_frame(Frame::new(FrameType::Err, err_msg.as_bytes(), &[err_msg.len()])).await
        },
    }
}

async fn open_file(filename: &str) -> Result<String> {
    let mut file = File::open(filename).await?;
    let mut contents = String::new();
    file.read_to_string(&mut contents).await?;
    Ok(contents)
}
