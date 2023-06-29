use std::process::{Child, Command, Stdio};

use futures::Future;

use crate::error::Error;

pub fn spawn_lemonbar() -> Result<Child, Error> {
    let executable = "lemonbar";
    let font = "UbuntuMono Nerd Font:size=12";
    let args = ["-f", &font, "-a", "30", "-u", "-4"];
    Ok(Command::new(executable)
        .args(args)
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .spawn()?)
}

fn write_tag<Stream>(stream: &mut Stream, tag: &str)
where
    Stream: std::io::Write,
{
    write!(stream, "%{{{tag}}}").unwrap();
}

async fn with_tag<Stream, Lambda, Fut>(
    mut stream: Stream,
    tag: &str,
    lambda: Lambda,
) -> Result<Stream, Error>
where
    Stream: std::io::Write,
    Lambda: Fn(Stream) -> Fut,
    Fut: Future<Output = Result<Stream, Error>>,
{
    write_tag(&mut stream, tag);
    let mut stream = lambda(stream).await?;
    write_tag(&mut stream, tag);
    Ok(stream)
}

pub async fn with_display<Stream, Lambda, Fut>(
    stream: Stream,
    index: usize,
    lambda: Lambda,
) -> Result<Stream, Error>
where
    Stream: std::io::Write,
    Lambda: Fn(Stream) -> Fut,
    Fut: Future<Output = Result<Stream, Error>>,
{
    with_tag(stream, &format!("S{index}"), lambda).await
}

pub async fn with_left<Stream, Lambda, Fut>(stream: Stream, lambda: Lambda) -> Result<Stream, Error>
where
    Stream: std::io::Write,
    Lambda: Fn(Stream) -> Fut,
    Fut: Future<Output = Result<Stream, Error>>,
{
    with_tag(stream, "l", lambda).await
}

pub async fn with_center<Stream, Lambda, Fut>(
    stream: Stream,
    lambda: Lambda,
) -> Result<Stream, Error>
where
    Stream: std::io::Write,
    Lambda: Fn(Stream) -> Fut,
    Fut: Future<Output = Result<Stream, Error>>,
{
    with_tag(stream, "c", lambda).await
}

pub async fn with_right<Stream, Lambda, Fut>(
    stream: Stream,
    lambda: Lambda,
) -> Result<Stream, Error>
where
    Stream: std::io::Write,
    Lambda: Fn(Stream) -> Fut,
    Fut: Future<Output = Result<Stream, Error>>,
{
    with_tag(stream, "r", lambda).await
}
