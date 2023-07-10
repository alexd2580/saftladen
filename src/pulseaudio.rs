use std::sync::Arc;

use log::warn;
use pulsectl::controllers::{DeviceControl, SinkController};
use tokio::{sync::Mutex, task::JoinHandle};

use crate::{
    bar::{mix_colors, Direction, SectionWriter, Style, DARK_GREEN, RED},
    error::Error,
    state_item::{wait_seconds, Notifyer, Receiver, StateItem},
};

struct PulseaudioData {
    port: String,
    mute: bool,
    volume: f32,
}
type SharedData = Arc<Mutex<Option<PulseaudioData>>>;
pub struct Pulseaudio(SharedData);

impl Pulseaudio {
    pub fn new() -> Self {
        Self(Arc::new(Mutex::new(None)))
    }
}

#[allow(clippy::cast_precision_loss)]
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
    let volume = 100f32 * cur_vol / max_vol;

    Ok(PulseaudioData { port, mute, volume })
}

#[async_trait::async_trait]
impl StateItem for Pulseaudio {
    async fn print(&self, writer: &mut SectionWriter, _output: &str) -> Result<(), Error> {
        writer.set_style(Style::Powerline);
        writer.set_direction(Direction::Left);

        let state = self.0.lock().await;
        if let Some(ref data) = *state {
            let temp_color = mix_colors(data.volume, 100f32, 125f32, DARK_GREEN, RED);
            writer.open_bg(temp_color);

            let icon = if data.mute {
                "󰖁"
            } else if data.volume < 33f32 {
                "󰕿"
            } else if data.volume < 66f32 {
                "󰖀"
            } else {
                "󰕾"
            };

            let port = &data.port;
            let volume = data.volume;

            writer.write(format!("{icon} {port} {volume:.0}% "));
            writer.close();
        } else {
            writer.open_bg(RED);
            writer.write("󰝟".to_string());
            writer.close();
        }
        Ok(())
    }

    fn start_coroutine(&self, notifyer: Notifyer, receiver: Receiver) -> JoinHandle<()> {
        tokio::spawn(pulseaudio_coroutine(self.0.clone(), notifyer, receiver))
    }
}

async fn pulseaudio_coroutine(state: SharedData, notifyer: Notifyer, mut receiver: Receiver) {
    loop {
        {
            *(state.lock().await) = match try_get_volume() {
                Err(err) => {
                    warn!("{err}");
                    None
                }
                Ok(data) => Some(data),
            };

            if !notifyer.send().await {
                break;
            }
        }

        tokio::select! {
            message = receiver.recv() => {
                if message.is_some() {
                    break;
                }
            }
            _ = wait_seconds(120) => {}
        }
    }
}
