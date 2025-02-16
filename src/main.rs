use std::fs;
use std::fs::read_link;
use std::io;
use std::os::linux::fs::MetadataExt;
use std::os::unix::ffi::OsStrExt;
use std::os::unix::fs::MetadataExt as _;
use std::os::unix::fs::PermissionsExt;
use std::path::Path;
use std::path::PathBuf;

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

        println!("frame type: {:?}", frame.ty);

        let segments =
            frame.iter_segments().map(|s| std::str::from_utf8(s).unwrap_or("")).collect::<Vec<_>>();
        match frame.ty {
            DirList => handle_dir_list(&mut connection, &segments[0]).await?,
            ExpandFileName => handle_expand_file_name(&mut connection, &segments).await?,
            FileExists => handle_file_exists(&mut connection, &segments[0]).await?,
            Open => handle_open(&mut connection, &segments[0]).await?,
            Save => todo!(),
            Stat => handle_stat(&mut connection, &segments[0]).await?,
            _ => unimplemented!(),
        }
    }
    Ok(())
}

// Just does tilde expansion right now
async fn handle_expand_file_name<'a>(
    connection: &mut Connection<ReadHalf<'a>, WriteHalf<'a>>, params: &[&str],
) -> Result<()> {
    // TODO: Remove this example of using multiple params
    let [file, _]: [&str; 2] = params.try_into().map_err(|_| "handle_expand_file_name: bad segment")?;

    assert!(file.starts_with("~"));

    let expanded = expanduser::expanduser(&file).unwrap_or_else(|_| Path::new(file).to_path_buf());
    let path = expanded.into_os_string();
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

#[derive(Debug)]
struct StatResult {
    nlinks: u64,
    uid: u32,
    gid: u32,
    atime: (i64, i64),
    mtime: (i64, i64),
    ctime: (i64, i64),
    size: u64,
    mode: u32,
    inode: u64,
    dev: u64,
    target: Option<PathBuf>,
}

impl StatResult {
    fn from_metadata(m: fs::Metadata, target: Option<PathBuf>) -> Self {
        Self {
            nlinks: m.st_nlink(),
            uid: m.st_uid(),
            gid: m.st_gid(),
            atime: (m.atime(), m.atime_nsec()),
            mtime: (m.mtime(), m.mtime_nsec()),
            ctime: (m.ctime(), m.ctime_nsec()),
            size: m.len(),
            mode: m.permissions().mode(),
            inode: m.ino(),
            dev: m.dev(),
            target,
        }
    }

    fn to_frame_data(&self) -> (String, Vec<usize>) {
        let mut fields = vec![
            self.nlinks.to_string(),
            self.uid.to_string(),
            self.gid.to_string(),
            format!("{}.{}", self.atime.0, self.atime.1),
            format!("{}.{}", self.mtime.0, self.mtime.1),
            format!("{}.{}", self.ctime.0, self.ctime.1),
            self.size.to_string(),
            unix_mode::to_string(self.mode),
            self.inode.to_string(),
            self.dev.to_string(),
        ];

        if let Some(target) = &self.target {
            fields.push(target.to_string_lossy().to_string());
        }

        let payload = fields.join("");
        let lengths = fields.iter().map(|s| s.len()).collect();

        (payload, lengths)
    }
}

async fn handle_stat<'a>(
    connection: &mut Connection<ReadHalf<'a>, WriteHalf<'a>>, filename: &str,
) -> Result<()> {
    let path = Path::new(filename);

    match fs::symlink_metadata(path) {
        Ok(m) => {
            let symlink_target = if m.is_symlink() { Some(read_link(path)?) } else { None };
            let res = StatResult::from_metadata(m, symlink_target);
            let (payload, lengths) = res.to_frame_data();
            connection.write_frame(Frame::new(FrameType::Data, payload.as_bytes(), &lengths)).await
        },
        Err(_) => todo!(),
    }
}
