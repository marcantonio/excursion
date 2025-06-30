use std::collections::HashSet;
use std::ffi::CString;
use std::ffi::OsString;
use std::fs;
use std::fs::read_link;
use std::io;
use std::os::linux::fs::MetadataExt;
use std::os::unix::ffi::OsStrExt;
use std::os::unix::fs::MetadataExt as _;
use std::os::unix::fs::PermissionsExt;
use std::path::Component;
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
            Canonicalize => handle_canonicalize(&mut connection, &segments[0]).await?,
            DirList => handle_dir_list(&mut connection, &segments).await?,
            ExpandFileName => handle_expand_file_name(&mut connection, &segments[0]).await?,
            Open => handle_open(&mut connection, &segments[0]).await?,
            Rm => handle_rm(&mut connection, &segments[0]).await?,
            Save => todo!(),
            Stat => handle_stat(&mut connection, &segments[0]).await?,
            Stat2 => handle_stat2(&mut connection, &segments).await?,
            Symlink => handle_symlink(&mut connection, &segments).await?,
            _ => unimplemented!(),
        }
    }
    Ok(())
}

// Just does tilde expansion right now
async fn handle_expand_file_name<'a>(
    connection: &mut Connection<ReadHalf<'a>, WriteHalf<'a>>, filename: &str,
) -> Result<()> {
    assert!(filename.starts_with("~"));
    let expanded = expanduser::expanduser(&filename).unwrap_or_else(|_| Path::new(filename).to_path_buf());
    let path = expanded.to_string_lossy();
    connection.write_frame(Frame::new(FrameType::Data, path.as_bytes(), &[path.len()])).await
}

async fn handle_dir_list<'a>(
    connection: &mut Connection<ReadHalf<'a>, WriteHalf<'a>>, params: &[&str],
) -> Result<()> {
    let [dirname, expand]: [&str; 2] = params.try_into().map_err(|_| "handle_dir_list: bad segment")?;

    let mut entries = match fs::read_dir(dirname).and_then(|entries| {
        entries
            .map(|entry| {
                Ok(if expand == "1" {
                    entry?.path().canonicalize()?.to_string_lossy().into_owned()
                } else {
                    entry?.file_name().to_string_lossy().into_owned()
                })
            })
            .collect::<io::Result<Vec<_>>>()
    }) {
        Ok(entries) => entries,
        Err(e) => return connection.send_err(Box::new(e)).await,
    };

    entries.splice(0..0, [".".to_string(), "..".to_string()]);

    let entry_lens = entries.iter().map(|e| e.len()).collect::<Vec<_>>();
    connection.write_frame(Frame::new(FrameType::Data, entries.join("").as_bytes(), &entry_lens)).await
}

async fn handle_open<'a>(
    connection: &mut Connection<ReadHalf<'a>, WriteHalf<'a>>, filename: &str,
) -> Result<()> {
    match open_file(filename).await {
        Ok(contents) => {
            connection.write_frame(Frame::new(FrameType::Data, contents.as_bytes(), &[contents.len()])).await
        },
        Err(e) => connection.send_err(e).await,
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
        Err(e) => match e.kind() {
            io::ErrorKind::NotFound => connection.write_frame(Frame::new(FrameType::Data, &[], &[0])).await,
            _ => connection.send_err(Box::new(e)).await,
        },
    }
}

async fn handle_stat2<'a>(
    connection: &mut Connection<ReadHalf<'a>, WriteHalf<'a>>, params: &[&str],
) -> Result<()> {
    let [filename, ask]: [&str; 2] = params.try_into().map_err(|_| "handle_stat2: bad segment")?;
    let path = Path::new(filename);
    let p = if match ask {
        "e" => tokio::fs::metadata(path).await.is_ok(),
        "r" => is_readable(path),
        "w" => is_writable(path),
        _ => todo!(),
    } {
        "1"
    } else {
        "0"
    };
    connection.write_frame(Frame::new(FrameType::Data, p.as_bytes(), &[1])).await
}

fn is_readable<P: AsRef<Path>>(path: P) -> bool {
    let path = path.as_ref();
    let c_path = match CString::new(path.as_os_str().as_bytes()) {
        Ok(c) => c,
        Err(_) => return false,
    };

    unsafe { libc::access(c_path.as_ptr(), libc::R_OK) == 0 }
}

fn is_writable<P: AsRef<Path>>(path: P) -> bool {
    let path = path.as_ref();
    let c_path = match CString::new(path.as_os_str().as_bytes()) {
        Ok(c) => c,
        Err(_) => return false,
    };

    if unsafe { libc::access(c_path.as_ptr(), libc::W_OK) == 0 } {
        return true;
    }

    if let Some(errno) = std::io::Error::last_os_error().raw_os_error() {
        if errno == libc::ENOENT {
            if let Some(parent) = path.parent() {
                let c_parent = match CString::new(parent.as_os_str().as_bytes()) {
                    Ok(c) => c,
                    Err(_) => return false,
                };
                return unsafe { libc::access(c_parent.as_ptr(), libc::W_OK) == 0 };
            }
        }
    }

    false
}

async fn handle_symlink<'a>(
    connection: &mut Connection<ReadHalf<'a>, WriteHalf<'a>>, params: &[&str],
) -> Result<()> {
    let [target, link]: [&str; 2] = params.try_into().map_err(|_| "handle_symlink: bad segment")?;
    if tokio::fs::symlink_metadata(&link).await.is_ok() {
        tokio::fs::remove_file(&link).await?;
    }
    match tokio::fs::symlink(target, link).await {
        Ok(_) => connection.write_frame(Frame::new(FrameType::Data, b"1", &[1])).await,
        Err(e) => connection.send_err(Box::new(e)).await,
    }
}

async fn handle_rm<'a>(
    connection: &mut Connection<ReadHalf<'a>, WriteHalf<'a>>, filename: &str,
) -> Result<()> {
    if let Err(e) = tokio::fs::remove_file(filename).await {
        if e.kind() != io::ErrorKind::NotFound {
            return connection.send_err(Box::new(e)).await;
        }
    }
    connection.write_frame(Frame::new(FrameType::Data, b"1", &[1])).await
}

async fn handle_canonicalize<'a>(
    connection: &mut Connection<ReadHalf<'a>, WriteHalf<'a>>, filename: &str,
) -> Result<()> {
    match canonicalize_missing_ok(filename).await {
        Ok(path) => {
            let path_bytes = path.as_os_str().as_bytes();
            connection.write_frame(Frame::new(FrameType::Data, path_bytes, &[path_bytes.len()])).await
        },
        Err(e) => {
            return connection.send_err(Box::new(e)).await;
        },
    }
}

async fn canonicalize_missing_ok<P: AsRef<Path>>(path: P) -> io::Result<PathBuf> {
    let mut components: Vec<OsString> =
        path.as_ref().components().map(|c| c.as_os_str().to_owned()).collect();

    let mut resolved = if path.as_ref().is_absolute() {
        PathBuf::from(Component::RootDir.as_os_str())
    } else {
        return Err(io::Error::new(io::ErrorKind::InvalidInput, "path must be absolute"));
    };

    let mut visited = HashSet::new();
    while let Some(segment) = components.first().cloned() {
        components.remove(0);
        resolved.push(&segment);

        match tokio::fs::symlink_metadata(&resolved).await {
            Ok(metadata) if metadata.file_type().is_symlink() => {
                // Detect symlink loops
                if !visited.insert(resolved.clone()) {
                    return Err(io::Error::new(io::ErrorKind::InvalidData, "symlink loop detected"));
                }

                let link_target = tokio::fs::read_link(&resolved).await?;
                // Replace the symlink
                resolved.pop();
                resolved = if link_target.is_absolute() { link_target } else { resolved.join(link_target) };
            },
            Err(e) if e.kind() == io::ErrorKind::NotFound => {
                // Broken link so just append the rest and bail
                for tail in components {
                    resolved.push(tail);
                }
                break;
            },
            _ => {},
        }
    }
    Ok(resolved)
}
