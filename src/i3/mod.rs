use std::{collections::HashMap, sync::Arc};

use async_i3ipc::{
    event::{Subscribe, WindowChange, WindowData},
    reply,
};
use log::error;
use saftbar::xft::RGBA;
use tokio::{sync::Mutex, task::JoinHandle};

use crate::{
    bar::{Direction, SectionWriter, Style, BLUE, DARKEST_GRAY, DARK_GRAY, GRAY, LIGHT_GRAY, RED},
    error::Error,
    state_item::{Notifyer, Receiver, StateItem},
};

use self::state::State;

mod state;

struct I3Data {
    tree_state: State,
    last_focused_windows: HashMap<String, usize>,
}

type SharedData = Arc<Mutex<Option<I3Data>>>;
pub struct I3(SharedData);

impl I3 {
    pub async fn displays() -> Result<Vec<(String, (isize, isize))>, Error> {
        let mut i3 = async_i3ipc::I3::connect().await?;
        let outputs_reply = i3.get_outputs().await?;
        Ok(outputs_reply
            .iter()
            .filter_map(|output| {
                let reply::Output {
                    active, name, rect, ..
                } = output;
                active.then_some((name.clone(), (rect.x, rect.y)))
            })
            .collect())
    }

    pub fn new() -> Self {
        Self(Arc::new(Mutex::new(None)))
    }
}

pub const INACTIVE: (RGBA, RGBA) = (DARKEST_GRAY, GRAY);
pub const SEMIACTIVE: (RGBA, RGBA) = (DARK_GRAY, LIGHT_GRAY);
pub const ACTIVE: (RGBA, RGBA) = (BLUE, LIGHT_GRAY);
pub const URGENT: (RGBA, RGBA) = (RED, LIGHT_GRAY);

fn choose_coloring(urgent: bool, active: bool, visible: bool) -> (RGBA, RGBA) {
    match (urgent, active, visible) {
        (true, _, _) => URGENT,
        (false, true, _) => ACTIVE,
        (false, false, true) => SEMIACTIVE,
        (false, false, false) => INACTIVE,
    }
}

#[async_trait::async_trait]
impl StateItem for I3 {
    async fn print(&self, writer: &mut SectionWriter, output_name: &str) -> Result<(), Error> {
        if let Some(I3Data {
            ref tree_state,
            ref last_focused_windows,
        }) = *(self.0.lock().await)
        {
            let output = tree_state
                .0
                .get(output_name)
                .ok_or_else(|| Error::Local(format!("Output '{output_name}' not found")))?;

            writer.set_style(Style::Rounded);
            writer.set_direction(Direction::Right);
            for (num, workspace) in &output.workspaces {
                // TODO
                let workspace_coloring =
                    choose_coloring(workspace.urgent, workspace.active, workspace.visible);
                writer.open_(workspace_coloring);
                writer.write(format!(" {num} "));

                let windows = &workspace.windows;
                if windows.is_empty() {
                } else if windows.len() == 1 {
                    let first = windows.first_key_value().unwrap();
                    writer.write(first.1.short_title());
                } else {
                    for window in windows.values() {
                        let window_visible = last_focused_windows
                            .get(output_name)
                            .is_some_and(|&focused| window.id == focused);

                        let window_coloring =
                            choose_coloring(window.urgent, window.active, window_visible);
                        writer.open_(window_coloring);
                        writer.write(format!(" {}", window.short_title()));
                    }
                }

                writer.close();
            }
        }
        Ok(())
    }

    fn start_coroutine(&self, notifyer: Notifyer, receiver: Receiver) -> JoinHandle<()> {
        tokio::spawn(i3_coroutine(self.0.clone(), notifyer, receiver))
    }
}

async fn init_coroutine_resources() -> Result<(async_i3ipc::EventStream, async_i3ipc::I3), Error> {
    let event_stream = {
        let mut i3 = async_i3ipc::I3::connect().await?;
        let subscription_events = [Subscribe::Output, Subscribe::Workspace, Subscribe::Window];
        let reply::Success { success, error } = i3.subscribe(&subscription_events).await?;
        if !success {
            return Err(Error::Local(
                error.unwrap_or_else(|| "Subscription failed".to_owned()),
            ));
        }
        i3.listen()
    };

    let i3 = async_i3ipc::I3::connect().await?;

    Ok((event_stream, i3))
}

async fn i3_coroutine(state: SharedData, notifyer: Notifyer, mut receiver: Receiver) {
    let (mut event_stream, mut i3) = match init_coroutine_resources().await {
        Ok(x) => x,
        Err(err) => {
            error!("{err}");
            return;
        }
    };

    let mut last_focused_windows = HashMap::new();

    loop {
        {
            match State::query(&mut i3).await {
                Err(err) => {
                    error!("{err}");
                }
                Ok(tree_state) => {
                    let last_focused_windows = last_focused_windows.clone();
                    *state.lock().await = Some(I3Data {
                        tree_state,
                        last_focused_windows,
                    });
                }
            }

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
            maybe_event = event_stream.next() => {
                match maybe_event {
                    Err(err) => error!("{err}"),
                    Ok(async_i3ipc::event::Event::Window(window_event)) => {
                        let &WindowData { change, ref container } = &*window_event;
                        if change == WindowChange::Focus {
                            last_focused_windows.insert(container.output.as_ref().unwrap().clone(), container.id);
                        }
                    }

                    Ok(_) => {},
                }
            }
        }
    }
}
