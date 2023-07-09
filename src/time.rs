use chrono::Local;
use log::error;
use tokio::{
    sync::{broadcast, mpsc},
    task::JoinHandle,
};

use crate::{
    bar::{Direction, SectionWriter, Style, ACTIVE},
    error::Error,
    ItemMessage, StateItem,
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
        writer.open_(ACTIVE);
        writer.write(format!(" {} ", now.format(&self.format)));
        writer.close();
        Ok(())
    }

    fn start_coroutine(
        &self,
        notify_sender: mpsc::Sender<()>,
        message_receiver: broadcast::Receiver<ItemMessage>,
    ) -> JoinHandle<()> {
        tokio::spawn(time_coroutine(notify_sender, message_receiver))
    }
}

async fn time_coroutine(
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

// {
//     "item": "Weather",
//     "icon": "\ufa8f",
//     "cooldown": 1800
// },
// {
//     "item": "PulseAudio",
//     "cooldown": 5
// },
// {
//     "item": "Net",
//     "icon": "\uf6ff",
//     "cooldown": 10,
//     "interface": "enp39s0",
//     "type": "ethernet",
//     "display": "IPv4",
//     "connection check cooldown": 30
// },
// {
//     "item": "Load",
//     "icon": "\ue266",
//     "cooldown": 5,
//     "button": "terminator -e htop &",
//     "temperature_files": [
//         "/sys/class/hwmon/hwmon1/temp0_input",
//         "/sys/class/hwmon/hwmon1/temp2_input",
//         "/sys/class/hwmon/hwmon1/temp3_input"
//     ]
// },
// {
//     "item": "Space",
//     "cooldown": 30,
//     "mount_points": [
//         {
//             "file": "/",
//             "icon": "\uf0a0"
//         }
//     ]
// },
// {
//     "item": "Date",
//     "icon": "\uf017",
//     "cooldown": 30,
//     "format": "%Y-%m-%d %H:%M"
// }
