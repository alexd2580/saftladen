use std::io::Write;
use std::process::ChildStdin;

use chrono::Local;
use log::error;
use tokio::{
    sync::{broadcast, mpsc},
    task::JoinHandle,
};

use crate::{error::Error, ItemMessage, StateItem};

pub struct Time {
    format: String,
}

impl Time {
    pub fn new() -> Result<Self, Error> {
        let format = "%a %F %R".to_owned();
        Ok(Time { format })
    }
}

#[async_trait::async_trait]
impl StateItem for Time {
    async fn print(&self, mut stream: ChildStdin, _output: &str) -> Result<ChildStdin, Error> {
        let now = Local::now();
        write!(stream, "{}", now.format(&self.format))?;
        Ok(stream)
    }

    async fn run(
        &mut self,
        notify_sender: mpsc::Sender<()>,
        message_receiver: broadcast::Receiver<ItemMessage>,
    ) -> Result<JoinHandle<()>, Error> {
        Ok(tokio::spawn(handle_events(notify_sender, message_receiver)))
    }
}

async fn handle_events(
    notify_sender: mpsc::Sender<()>,
    mut message_receiver: broadcast::Receiver<ItemMessage>,
) {
    loop {
        tokio::select! {
            message = message_receiver.recv() => {
                match message {
                    Err(err) => { error!("{err}"); break; }
                    Ok(()) => break,
                }
            }
            _ = tokio::time::sleep(tokio::time::Duration::from_secs(30)) => {
                match notify_sender.send(()).await {
                    Err(err) => { error!("{err}"); break; },
                    Ok(()) => {},
                }
            }

        }
    }
}
