use std::sync::Arc;

use log::{error, warn};
use pulsectl::controllers::{DeviceControl, SinkController};
use tokio::{
    sync::{broadcast, mpsc, Mutex},
    task::JoinHandle,
};

use crate::{
    bar::{font_color, mix_colors, Direction, SectionWriter, Style, DARK_GREEN, RED, WARN},
    error::Error,
    ItemMessage, StateItem,
};

struct PulseaudioData {
    port: String,
    mute: bool,
    volume: u32,
}
type SharedData = Arc<Mutex<Option<PulseaudioData>>>;
pub struct Pulseaudio(SharedData);

impl Pulseaudio {
    pub fn new() -> Self {
        Self(Arc::new(Mutex::new(None)))
    }
}

fn try_get_volume() -> Result<PulseaudioData, Error> {
    let mut sink_controller = SinkController::create()?;
    let default_device = sink_controller.get_default_device()?;

    let port = default_device
        .active_port
        .and_then(|port| port.description)
        .unwrap_or_else(|| "unknown".to_string());

    let mute = default_device.mute;

    let cur_vol = default_device.volume.max().0 as f32;
    let max_vol = default_device.base_volume.0 as f32;
    let volume = (100f32 * cur_vol / max_vol) as u32;

    Ok(PulseaudioData { port, mute, volume })
}

async fn pulseaudio_coroutine(
    state: SharedData,
    notify_sender: mpsc::Sender<()>,
    mut message_receiver: broadcast::Receiver<ItemMessage>,
) {
    loop {
        {
            *(state.lock().await) = match try_get_volume() {
                Err(err) => {
                    warn!("{err}");
                    None
                }
                Ok(data) => Some(data),
            };

            if let Err(err) = notify_sender.send(()).await {
                error!("{err}");
                break;
            }
        }

        tokio::select! {
            message = message_receiver.recv() => {
                match message {
                    Ok(()) => break,
                    Err(err) => { error!("{err}"); break; }
                }
            }
            _ = tokio::time::sleep(tokio::time::Duration::from_secs(120)) => {}
        }
    }
}

#[async_trait::async_trait]
impl StateItem for Pulseaudio {
    async fn print(&self, writer: &mut SectionWriter, _output: &str) -> Result<(), Error> {
        writer.set_style(Style::Powerline);
        writer.set_direction(Direction::Left);

        let state = self.0.lock().await;
        if let Some(ref data) = *state {
            let temp_color = mix_colors(data.volume as f32, 100f32, 125f32, DARK_GREEN, RED);
            let font_color = font_color(temp_color);
            writer.open(temp_color, font_color);

            let icon = if data.mute {
                "󰖁"
            } else if data.volume < 33 {
                "󰕿"
            } else if data.volume < 66 {
                "󰖀"
            } else {
                "󰕾"
            };

            let port = &data.port;
            let volume = data.volume;

            writer.write(format!("{icon} {port} {volume}% "));
            writer.close();
        } else {
            writer.open_(WARN);
            writer.write("󰝟".to_string());
            writer.close();
        }
        Ok(())
    }

    fn start_coroutine(
        &self,
        notify_sender: mpsc::Sender<()>,
        message_receiver: broadcast::Receiver<ItemMessage>,
    ) -> JoinHandle<()> {
        tokio::spawn(pulseaudio_coroutine(
            self.0.clone(),
            notify_sender,
            message_receiver,
        ))
    }
}
