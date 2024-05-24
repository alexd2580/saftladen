use std::sync::Arc;

use log::{debug, warn};
use saftbar::bar::{PowerlineDirection, PowerlineStyle};
// use pulsectl::controllers::{DeviceControl, SinkController};
use tokio::{sync::Mutex, task::JoinHandle};

use crate::{
    bar::{mix_colors, SectionWriter, DARK_GREEN, RED},
    error::Error,
    state_item::{
        wait_seconds, ItemAction, ItemActionReceiver, MainAction, MainActionSender, StateItem,
    },
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
    Err(Error::Local("Pulsectl is rip".to_string()))
    // let mut sink_controller = SinkController::create()?;
    // let default_device = sink_controller.get_default_device()?;
    //
    // let port = default_device
    //     .active_port
    //     .and_then(|port| port.description)
    //     .unwrap_or_else(|| "unknown".to_string());
    //
    // let mute = default_device.mute;
    //
    // let cur_vol = default_device.volume.max().0 as f32;
    // let max_vol = default_device.base_volume.0 as f32;
    // let volume = 100f32 * cur_vol / max_vol;
    //
    // Ok(PulseaudioData { port, mute, volume })
}

#[async_trait::async_trait]
impl StateItem for Pulseaudio {
    async fn print(&self, writer: &mut SectionWriter, _output: &str) -> Result<(), Error> {
        writer.set_style(PowerlineStyle::Powerline);
        writer.set_direction(PowerlineDirection::Left);

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

    fn start_coroutine(
        &self,
        main_action_sender: MainActionSender,
        item_action_receiver: ItemActionReceiver,
    ) -> JoinHandle<()> {
        tokio::spawn(pulseaudio_coroutine(
            self.0.clone(),
            main_action_sender,
            item_action_receiver,
        ))
    }
}

async fn pulseaudio_coroutine(
    state: SharedData,
    main_action_sender: MainActionSender,
    mut item_action_receiver: ItemActionReceiver,
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

            if !main_action_sender.enqueue(MainAction::Redraw).await {
                break;
            }
        }

        tokio::select! {
            message = item_action_receiver.next() => {
                match message {
                    None | Some(ItemAction::Update)  => {},
                    Some(ItemAction::Terminate) => break,
                }
            }
            _ = wait_seconds(120) => {}
        }
    }
    debug!("coroutine exiting");
}
