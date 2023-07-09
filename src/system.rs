use std::{sync::Arc, time};

use log::error;
use sysinfo::{CpuExt, SystemExt};
use tokio::{
    sync::{broadcast, mpsc, Mutex},
    task::JoinHandle,
};

use crate::{
    bar::{font_color, mix_colors, Direction, SectionWriter, Style, CRITICAL, DARK_GREEN, RED},
    error::Error,
    ItemMessage, StateItem,
};

struct SystemData {
    cpu_refresh_time: time::Instant,
    mem_refresh_time: time::Instant,
    sysinfo: sysinfo::System,
}

impl SystemData {
    fn new() -> Self {
        let mut sysinfo = sysinfo::System::new();

        sysinfo.refresh_cpu();
        let cpu_refresh_time = time::Instant::now();
        sysinfo.refresh_memory();
        let mem_refresh_time = time::Instant::now();

        Self {
            sysinfo,
            cpu_refresh_time,
            mem_refresh_time,
        }
    }

    fn update(&mut self) -> bool {
        let mut updated = false;
        if self.cpu_refresh_time.elapsed() > time::Duration::from_secs_f32(4.5) {
            self.cpu_refresh_time = time::Instant::now();
            self.sysinfo.refresh_cpu();
            updated = true;
        }

        if self.mem_refresh_time.elapsed() > time::Duration::from_secs_f32(14.5) {
            self.mem_refresh_time = time::Instant::now();
            self.sysinfo.refresh_memory();
            updated = true;
        }

        updated
    }
}

type SharedData = Arc<Mutex<Option<SystemData>>>;
pub struct System(SharedData);

impl System {
    pub fn new() -> Self {
        Self(Arc::new(Mutex::new(None)))
    }
}

fn format_bytes(a: u64) -> String {
    if a < 1_000 {
        format!("{a}B")
    } else if a < 1_000_000 {
        let a = a / 1_000;
        format!("{a}kB")
    } else if a < 1_000_000_000 {
        let a = a / 1_000_000;
        format!("{a}MB")
    } else if a < 1_000_000_000_000 {
        let a = a / 1_000_000_000;
        format!("{a}GB")
    } else {
        let a = a / 1_000_000_000_000;
        format!("{a}TB")
    }
}

#[async_trait::async_trait]
impl StateItem for System {
    async fn print(&self, writer: &mut SectionWriter, _output: &str) -> Result<(), Error> {
        writer.set_style(Style::Powerline);
        writer.set_direction(Direction::Left);

        let state = self.0.lock().await;
        if let Some(ref data) = *state {
            let global_cpu = data.sysinfo.global_cpu_info();
            let usage = global_cpu.cpu_usage();
            let bg_color = mix_colors(usage, 80f32, 100f32, DARK_GREEN, RED);
            let fg_color = font_color(bg_color);
            writer.open(bg_color, fg_color);
            writer.write(format!(" {usage:.0}% "));

            let total_ram = data.sysinfo.total_memory();
            let available_ram = data.sysinfo.available_memory();
            let used_ram = total_ram - available_ram;
            let usage = 100f32 * used_ram as f32 / total_ram as f32;
            let bg_color = mix_colors(usage, 75f32, 100f32, DARK_GREEN, RED);
            let fg_color = font_color(bg_color);
            writer.open(bg_color, fg_color);
            writer.write(format!("󰍛 {usage:.0}% ({})", format_bytes(total_ram)));

            writer.close();
        } else {
            writer.open_(CRITICAL);
            writer.write("".to_string());
            writer.close();
        }
        Ok(())
    }

    fn start_coroutine(
        &self,
        notify_sender: mpsc::Sender<()>,
        message_receiver: broadcast::Receiver<ItemMessage>,
    ) -> JoinHandle<()> {
        tokio::spawn(system_coroutine(
            self.0.clone(),
            notify_sender,
            message_receiver,
        ))
    }
}

async fn system_coroutine(
    state: SharedData,
    notify_sender: mpsc::Sender<()>,
    mut message_receiver: broadcast::Receiver<ItemMessage>,
) {
    loop {
        {
            let mut state_lock = state.lock().await;
            let updated = if state_lock.is_none() {
                *state_lock = Some(SystemData::new());
                true
            } else {
                (*state_lock).as_mut().unwrap().update()
            };
            if updated {
                match notify_sender.send(()).await {
                    Err(err) => {
                        error!("{err}");
                        return;
                    }
                    Ok(()) => {}
                }
            }
        }

        tokio::select! {
            message = message_receiver.recv() => {
                match message {
                    Err(err) => { error!("{err}"); break; }
                    Ok(()) => break,
                }
            }
            _ = tokio::time::sleep(tokio::time::Duration::from_secs(5)) => {}

        }
    }
}
