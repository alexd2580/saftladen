use chrono::Local;
use tokio::task::JoinHandle;

use crate::{
    bar::{Direction, SectionWriter, Style, BLUE, LIGHT_GRAY},
    error::Error,
    state_item::{wait_seconds, Notifyer, Receiver, StateItem},
};

pub struct Time {
    format: String,
}

impl Time {
    pub fn new() -> Self {
        Self {
            format: "%a %F %R".to_owned(),
        }
    }
}

#[async_trait::async_trait]
impl StateItem for Time {
    async fn print(&self, writer: &mut SectionWriter, _output: &str) -> Result<(), Error> {
        let now = Local::now();
        writer.set_style(Style::Powerline);
        writer.set_direction(Direction::Left);
        writer.open(BLUE, LIGHT_GRAY);
        writer.write(format!(" {} ", now.format(&self.format)));
        writer.close();
        Ok(())
    }

    fn start_coroutine(&self, notifyer: Notifyer, receiver: Receiver) -> JoinHandle<()> {
        tokio::spawn(time_coroutine(notifyer, receiver))
    }
}

async fn time_coroutine(notifyer: Notifyer, mut receiver: Receiver) {
    loop {
        if !notifyer.send().await {
            break;
        }

        tokio::select! {
            message = receiver.recv() => {
                if message.is_some() {
                    break;
                }
            }
            _ = wait_seconds(30) => {}
        }
    }
}
