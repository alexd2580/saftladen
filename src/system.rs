use std::{ops::IndexMut, sync::Arc, time};

use log::debug;
use saftbar::bar::{PowerlineDirection, PowerlineStyle};
use sysinfo::{ComponentExt, CpuExt, SystemExt};
use tokio::{sync::Mutex, task::JoinHandle};

use crate::{
    bar::{mix_colors, SectionWriter, DARK_GREEN, RED, TOO_RED},
    error::Error,
    state_item::{
        wait_seconds, ItemAction, ItemActionReceiver, MainAction, MainActionSender, StateItem,
    },
};

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

struct SystemData {
    /// Refresh period of cpu info.
    cpu_refresh_period: time::Duration,
    /// Timestamp of last refresh of cpu info.
    cpu_refresh_time: time::Instant,
    /// The index of the sysinfo component that holds the "k10temp Tctl" temperature.
    /// We don't support any other CPU/driver at the moment.
    cpu_temp_component_index: Option<usize>,
    /// CPU usage in %.
    cpu_usage: f32,
    /// CPU Temperature. Refreshed every `cpu_refresh_period`.
    cpu_temp: Option<f32>,

    /// Refresh period of memory usage info.
    mem_refresh_period: time::Duration,
    /// Timestamp of last refresh of memory usage info.
    mem_refresh_time: time::Instant,
    /// Ram usage in %.
    ram_usage: f32,
    /// Formatted used/total sizes.
    ram_usage_formatted: (String, String),

    sysinfo: sysinfo::System,
}

impl SystemData {
    fn new() -> Self {
        let mut sysinfo = sysinfo::System::new();

        sysinfo.refresh_cpu();
        let cpu_refresh_time = time::Instant::now();
        sysinfo.refresh_memory();
        let mem_refresh_time = time::Instant::now();
        sysinfo.refresh_components_list();

        let k10_tctl_label = "k10temp Tctl";
        let cpu_temp_component_index = sysinfo
            .components()
            .iter()
            .enumerate()
            .find_map(|(index, value)| (value.label() == k10_tctl_label).then_some(index));

        Self {
            cpu_refresh_period: time::Duration::from_secs_f32(4.5),
            cpu_refresh_time,
            cpu_temp_component_index,
            cpu_usage: 0.0,
            cpu_temp: None,
            mem_refresh_period: time::Duration::from_secs_f32(14.5),
            mem_refresh_time,
            ram_usage: 0.0,
            ram_usage_formatted: (String::default(), String::default()),
            sysinfo,
        }
    }

    #[allow(clippy::cast_precision_loss)]
    fn ram_usage(&self) -> (f32, String, String) {
        let sysinfo = &self.sysinfo;
        let total_ram = sysinfo.total_memory();
        let available_ram = sysinfo.available_memory();
        let used_ram = total_ram - available_ram;
        let usage = 100f32 * used_ram as f32 / total_ram as f32;

        (usage, format_bytes(used_ram), format_bytes(total_ram))
    }

    fn update(&mut self) -> bool {
        let mut updated = false;
        if self.cpu_refresh_time.elapsed() > self.cpu_refresh_period {
            self.cpu_refresh_time = time::Instant::now();
            self.sysinfo.refresh_cpu();

            let global_cpu = self.sysinfo.global_cpu_info();
            self.cpu_usage = global_cpu.cpu_usage();

            self.cpu_temp = self.cpu_temp_component_index.as_ref().map(|&index| {
                let component = self.sysinfo.components_mut().index_mut(index);
                component.refresh();
                component.temperature()
            });

            updated = true;
        }

        if self.mem_refresh_time.elapsed() > self.mem_refresh_period {
            self.mem_refresh_time = time::Instant::now();
            self.sysinfo.refresh_memory();

            let (usage, used_formatted, total_formatted) = self.ram_usage();
            self.ram_usage = usage;
            self.ram_usage_formatted = (used_formatted, total_formatted);

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

#[async_trait::async_trait]
impl StateItem for System {
    async fn print(&self, writer: &mut SectionWriter, _output: &str) -> Result<(), Error> {
        writer.set_style(PowerlineStyle::Powerline);
        writer.set_direction(PowerlineDirection::Left);

        let state = self.0.lock().await;
        if let Some(ref data) = *state {
            let cpu_bg = if let Some(cpu_temp) = data.cpu_temp {
                mix_colors(cpu_temp, 50f32, 70f32, DARK_GREEN, RED)
            } else {
                mix_colors(data.cpu_usage, 80f32, 100f32, DARK_GREEN, RED)
            };
            writer.with_bg(cpu_bg, &|writer| {
                writer.write(format!(" {:.0}% ", data.cpu_usage));
                if let Some(cpu_temp) = data.cpu_temp {
                    writer.write(format!("{cpu_temp:.0} "));
                }
            });

            let ram_usage = data.ram_usage;
            let ram_bg = mix_colors(ram_usage, 75f32, 100f32, DARK_GREEN, RED);
            writer.with_bg(ram_bg, &|writer| {
                let (used_ram, total_ram) = &data.ram_usage_formatted;
                writer.write(format!(" {ram_usage:.0}% ({used_ram}/{total_ram})"));
            });
        } else {
            writer.with_bg(TOO_RED, &|writer| writer.write("".to_string()));
        }
        Ok(())
    }

    fn start_coroutine(
        &self,
        main_action_sender: MainActionSender,
        item_action_receiver: ItemActionReceiver,
    ) -> JoinHandle<()> {
        tokio::spawn(system_coroutine(
            self.0.clone(),
            main_action_sender,
            item_action_receiver,
        ))
    }
}

async fn system_coroutine(
    state: SharedData,
    main_action_sender: MainActionSender,
    mut item_action_receiver: ItemActionReceiver,
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
            if updated && !main_action_sender.enqueue(MainAction::Redraw).await {
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
            _ = wait_seconds(5) => {}

        }
    }
    debug!("coroutine exiting");
}
