use std::{collections::HashMap, sync::Arc};

use async_i3ipc::{
    event::{WorkspaceChange, WorkspaceData},
    reply::{self, Node},
};
use log::{debug, error};
use saftbar::{
    bar::{PowerlineDirection, PowerlineStyle},
    xft::RGBA,
};
use tokio::{sync::Mutex, task::JoinHandle};

use crate::{
    bar::{SectionWriter, BLUE, DARKEST_GRAY, DARK_GRAY, GRAY, LIGHT_GRAY, RED},
    error::Error,
    state_item::{ItemAction, ItemActionReceiver, MainAction, MainActionSender, StateItem},
};

use self::state::State;
use self::window_title::Shortener;

mod state;
mod window_title;

struct I3Data {
    tree_state: State,
    last_focused_workspace: i32,
    last_focused_windows: HashMap<String, usize>,
}

type SharedData = Arc<Mutex<Option<I3Data>>>;
pub struct I3(Shortener, SharedData);

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

    pub fn new() -> Result<Self, Error> {
        Ok(Self(Shortener::new()?, Arc::new(Mutex::new(None))))
    }
}

const NUMBERS: [&str; 10] = ["󰲠", "󰲢", "󰲤", "󰲦", "󰲨", "󰲪", "󰲬", "󰲮", "󰲰", "󰿬"];

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

#[allow(clippy::cast_sign_loss)]
#[async_trait::async_trait]
impl StateItem for I3 {
    async fn print(&self, writer: &mut SectionWriter, output_name: &str) -> Result<(), Error> {
        if let Some(I3Data {
            tree_state,
            last_focused_workspace,
            last_focused_windows,
        }) = &*(self.1.lock().await)
        {
            let output = tree_state
                .0
                .get(output_name)
                .ok_or_else(|| Error::Local(format!("Output '{output_name}' not found")))?;

            writer.set_style(PowerlineStyle::Octagon);
            writer.set_direction(PowerlineDirection::Right);
            for (num, workspace) in &output.workspaces {
                // TODO
                let workspace_coloring = choose_coloring(
                    workspace.urgent,
                    workspace.active || num == last_focused_workspace,
                    workspace.visible,
                );
                writer.open_(workspace_coloring);

                let num = *num;
                let ws_num = if num > 0 && num < 11 {
                    NUMBERS[num as usize - 1].to_owned()
                } else {
                    format!(" {num} ")
                };
                writer.write(ws_num);

                let windows = &workspace.windows;
                if windows.is_empty() {
                } else if windows.len() == 1 {
                    let first = windows.first_key_value().unwrap();
                    writer.write(format!(" {}", self.0.shorten(&first.1.title)));
                } else {
                    for window in windows.values() {
                        let window_visible = last_focused_windows
                            .get(output_name)
                            .is_some_and(|&focused| window.id == focused);

                        let window_coloring =
                            choose_coloring(window.urgent, window.active, window_visible);
                        writer.open_(window_coloring);
                        writer.write(self.0.shorten(&window.title));
                    }
                }

                writer.close();
            }
        }
        Ok(())
    }

    fn start_coroutine(
        &self,
        main_action_sender: MainActionSender,
        item_action_receiver: ItemActionReceiver,
    ) -> JoinHandle<()> {
        tokio::spawn(i3_coroutine(
            self.1.clone(),
            main_action_sender,
            item_action_receiver,
        ))
    }
}

async fn init_coroutine_resources() -> Result<(async_i3ipc::EventStream, async_i3ipc::I3), Error> {
    use async_i3ipc::event::Subscribe;
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

async fn i3_coroutine(
    state: SharedData,
    main_action_sender: MainActionSender,
    mut item_action_receiver: ItemActionReceiver,
) {
    use async_i3ipc::event::{Event, OutputData, WindowChange, WindowData};

    let (mut event_stream, mut i3) = match init_coroutine_resources().await {
        Ok(x) => x,
        Err(err) => {
            error!("{err}");
            return;
        }
    };

    let mut last_focused_workspace = 1;
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
                        last_focused_workspace,
                        last_focused_windows,
                    });
                }
            }

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
            maybe_event = event_stream.next() => {
                match maybe_event {
                    Err(err) => error!("{err}"),
                    Ok(Event::Window(window_event)) => {
                        let WindowData { change, container } = &*window_event;
                        if *change == WindowChange::Focus {
                            last_focused_windows.insert(container.output.as_ref().unwrap().clone(), container.id);
                        }
                    }
                    Ok(Event::Workspace(workspace_event)) => {
                        let WorkspaceData { change, current, .. } = &*workspace_event;
                        if *change == WorkspaceChange::Focus {
                            if let Some(Node { num: Some(num), .. }) = &current {
                                last_focused_workspace = *num;
                            }
                        }
                    }
                    Ok(Event::Output(OutputData { .. })) => {
                        if !main_action_sender.enqueue(MainAction::Reinit).await {
                            break;
                        }
                    },
                    Ok(_) => {},
                }
            }
        }
    }
    debug!("coroutine exiting");
}
