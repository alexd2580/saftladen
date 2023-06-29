use async_i3ipc::{
    event::{Event, Subscribe, WindowChange, WindowData, WorkspaceChange, WorkspaceData},
    reply::{self, Node},
};
use log::{debug, error};
use std::{
    collections::{BTreeMap, HashMap},
    ops::{Deref, DerefMut},
    process::ChildStdin,
    sync::Arc,
};
use tokio::{
    sync::{broadcast, mpsc, Mutex, MutexGuard},
    task::JoinHandle,
};

use crate::{
    error::{print_error, Error},
    ItemMessage, StateItem,
};

fn node_window(node: &Node) -> Result<usize, Error> {
    node.window.ok_or_else(|| {
        let msg = format!("Node does not have a window id: {node:#?}");
        Error::Local(msg)
    })
}

fn node_name(node: &Node) -> Result<&String, Error> {
    node.name.as_ref().ok_or_else(|| {
        let msg = format!("Node does not have a name: {node:#?}");
        Error::Local(msg)
    })
}

fn node_num(node: &Node) -> Result<i32, Error> {
    node.num.ok_or_else(|| {
        let msg = format!("Node does not have a num: {node:#?}");
        Error::Local(msg)
    })
}

fn node_output(node: &Node) -> Result<&String, Error> {
    node.output.as_ref().ok_or_else(|| {
        let msg = format!("Node does not have an output: {node:#?}");
        Error::Local(msg)
    })
}

async fn find_workspace_of_window_in_output(
    output: &str,
    window_id: usize,
) -> Result<usize, Error> {
    let mut i3 = async_i3ipc::I3::connect().await?;
    let tree = i3.get_tree().await?;

    let output_node = tree
        .nodes
        .iter()
        .find(|node| node.name.as_ref().is_some_and(|name| name == output))
        .ok_or_else(|| Error::Local(format!("Display node not found for '{output}'")))?;

    let workspace_nodes = output_node
        .nodes
        .iter()
        .filter_map(|node| {
            node.name
                .as_ref()
                .is_some_and(|name| name == "content")
                .then_some(&node.nodes)
        })
        .flatten();

    for workspace_node in workspace_nodes {
        let mut queue = Vec::from_iter(workspace_node.nodes.iter());
        while let Some(node) = queue.pop() {
            if let &Some(window) = &node.window {
                if window == window_id {
                    return Ok(workspace_node.id);
                }
            } else {
                queue.extend(node.nodes.iter());
            }
        }
    }

    let msg = format!("Can't find window '{window_id}' in tree of '{output}'");
    Err(Error::Local(msg))
}

#[derive(Debug)]
struct Output {
    name: String,
    focused_workspace: usize,
    workspaces: BTreeMap<i32, usize>,
}

impl Output {
    fn remove_workspace(&mut self, workspace_id: usize) {
        self.workspaces.retain(|_num, id| *id != workspace_id);
        if self.focused_workspace == workspace_id {
            self.focused_workspace = *self.workspaces.first_key_value().unwrap().1;
        }
    }
}

#[derive(Debug)]
struct Outputs {
    outputs: HashMap<String, Output>,
}

impl Outputs {
    fn insert(&mut self, output: Output) -> Result<(), Error> {
        let output_name = output.name.clone();
        if self.outputs.insert(output_name.clone(), output).is_some() {
            let msg = format!("Failed to create output '{output_name}'");
            Err(Error::Local(msg))
        } else {
            Ok(())
        }
    }

    fn get(&self, name: &str) -> Result<&Output, Error> {
        self.outputs
            .get(name)
            .ok_or_else(|| Error::Local(format!("Didn't find output '{name}'")))
    }

    fn get_mut(&mut self, name: &str) -> Result<&mut Output, Error> {
        self.outputs
            .get_mut(name)
            .ok_or_else(|| Error::Local(format!("Didn't find output '{name}'")))
    }
}

#[derive(Debug)]
struct Workspace {
    output: String,
    id: usize,
    name: String,
    num: i32,
    urgent: bool,
    focused_window: Option<usize>,
    windows: Vec<usize>,
}

impl Workspace {
    fn from_node(node: &Node) -> Result<Self, Error> {
        Ok(Workspace {
            output: node_output(node)?.clone(),
            id: node.id,
            name: node_name(node)?.clone(),
            num: node_num(node)?,
            urgent: node.urgent,
            focused_window: None,
            windows: Vec::new(),
        })
    }

    fn remove_window(&mut self, window_id: usize) {
        if self.focused_window == Some(window_id) {
            self.focused_window = None;
        }
        self.windows.retain(|id| *id != window_id);
    }
}

#[derive(Debug)]
struct Workspaces {
    workspaces: HashMap<usize, Workspace>,
}

impl Workspaces {
    fn insert(&mut self, workspace: Workspace) -> Result<(), Error> {
        let workspace_id = workspace.id;
        if self.workspaces.insert(workspace_id, workspace).is_some() {
            let msg = format!("Failed to create workspace '{workspace_id}'");
            Err(Error::Local(msg))
        } else {
            Ok(())
        }
    }

    fn get(&self, id: usize) -> Result<&Workspace, Error> {
        self.workspaces
            .get(&id)
            .ok_or_else(|| Error::Local(format!("Didn't find workspace '{id}'")))
    }

    fn get_mut(&mut self, id: usize) -> Result<&mut Workspace, Error> {
        self.workspaces
            .get_mut(&id)
            .ok_or_else(|| Error::Local(format!("Didn't find workspace '{id}'")))
    }

    fn get_by_name(&self, name: &str) -> Result<&Workspace, Error> {
        self.workspaces
            .values()
            .find(|workspace| &workspace.name == name)
            .ok_or_else(|| Error::Local(format!("Didn't find workspace '{name}'")))
    }

    fn remove(&mut self, id: usize) -> Result<(), Error> {
        match self.workspaces.remove(&id) {
            Some(_) => Ok(()),
            None => Err(Error::Local(format!("Didn't find workspace '{id}'"))),
        }
    }
}

#[derive(Debug)]
struct Window {
    workspace: usize,
    id: usize,
    urgent: bool,
    title: String,
}

impl Window {
    fn from_node(workspace_id: usize, node: &Node) -> Result<Self, Error> {
        Ok(Window {
            workspace: workspace_id,
            id: node_window(node)?,
            urgent: node.urgent,
            title: node_name(node)?.clone(),
        })
    }
}

#[derive(Debug)]
struct Windows {
    windows: HashMap<usize, Window>,
}

impl Windows {
    fn insert(&mut self, window: Window) -> Result<(), Error> {
        let window_id = window.id;
        if self.windows.insert(window_id, window).is_some() {
            let msg = format!("Failed to create window '{window_id}'");
            Err(Error::Local(msg))
        } else {
            Ok(())
        }
    }

    fn get(&self, id: usize) -> Result<&Window, Error> {
        self.windows
            .get(&id)
            .ok_or_else(|| Error::Local(format!("Didn't find window '{id}'")))
    }

    fn get_mut(&mut self, id: usize) -> Result<&mut Window, Error> {
        self.windows
            .get_mut(&id)
            .ok_or_else(|| Error::Local(format!("Didn't find window '{id}'")))
    }

    fn remove(&mut self, id: usize) -> Result<(), Error> {
        match self.windows.remove(&id) {
            Some(_) => Ok(()),
            None => Err(Error::Local(format!("Didn't find window '{id}'"))),
        }
    }
}

#[derive(Debug)]
struct State {
    focused_output: String,
    outputs: Outputs,
    workspaces: Workspaces,
    windows: Windows,
}

impl State {
    pub async fn print<Stream>(&self, mut stream: Stream, output: &str) -> Result<Stream, Error>
    where
        Stream: std::io::Write,
    {
        let output = self.outputs.get(output)?;
        for workspace_id in output.workspaces.values() {
            let workspace = self.workspaces.get(*workspace_id)?;

            write!(stream, "Workspace {}", workspace.name)?;
            for window_id in &workspace.windows {
                let window = self.windows.get(*window_id)?;

                write!(stream, "  Window {}", window.title)?;
            }
        }

        Ok(stream)
    }

    pub async fn validate(&self) -> Result<(), Error> {
        let mut i3 = async_i3ipc::I3::connect().await?;
        let tree = i3.get_tree().await?;

        let output_nodes: Vec<&Node> = tree
            .nodes
            .iter()
            .filter(|node| node.name.as_ref().is_some_and(|name| name != "__i3"))
            .collect();

        assert_eq!(output_nodes.len(), self.outputs.outputs.len());

        let mut workspaces: Vec<&Node> = Vec::new();
        for output_node in output_nodes {
            let output = self.outputs.get(output_node.name.as_ref().unwrap())?;

            let workspace_nodes: Vec<&Node> = output_node
                .nodes
                .iter()
                .filter_map(|node| {
                    node.name
                        .as_ref()
                        .is_some_and(|name| name == "content")
                        .then_some(&node.nodes)
                })
                .flatten()
                .collect();

            assert_eq!(
                workspace_nodes.len(),
                output.workspaces.len(),
                "{}",
                output_node.name.as_ref().unwrap()
            );
        }

        // for workspace_node in workspace_nodes {
        //     let mut queue = Vec::from_iter(workspace_node.nodes.iter());
        //     while let Some(node) = queue.pop() {
        //         if let &Some(window) = &node.window {
        //             if window == window_id {
        //                 return Ok(workspace_node.id);
        //             }
        //         } else {
        //             queue.extend(node.nodes.iter());
        //         }
        //     }
        // }

        Ok(())
    }
}

pub struct I3 {
    state: Arc<Mutex<State>>,
}

#[async_trait::async_trait]
impl StateItem for I3 {
    async fn print(&self, stream: ChildStdin, output: &str) -> Result<ChildStdin, Error> {
        Ok(self.get_state().await.print(stream, output).await?)
    }

    async fn run(
        &mut self,
        notify_sender: mpsc::Sender<()>,
        message_receiver: broadcast::Receiver<ItemMessage>,
    ) -> Result<JoinHandle<()>, Error> {
        let mut i3 = async_i3ipc::I3::connect().await?;
        let subscription_events = [Subscribe::Output, Subscribe::Workspace, Subscribe::Window];
        let reply::Success { success, error } = i3.subscribe(&subscription_events).await?;
        if !success {
            return Err(Error::Local(
                error.unwrap_or_else(|| "Subscription failed".to_owned()),
            ));
        }

        Ok(tokio::spawn(handle_events(
            i3,
            self.state.clone(),
            notify_sender,
            message_receiver,
        )))
    }
}

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

    fn parse_workspaces(
        node: &Node,
        output: &mut Output,
        workspaces: &mut Workspaces,
        windows: &mut Windows,
    ) -> Result<bool, Error> {
        let workspace_nodes = node
            .nodes
            .iter()
            .filter_map(|node| {
                node.name
                    .as_ref()
                    .is_some_and(|name| name == "content")
                    .then_some(&node.nodes)
            })
            .flatten();

        let mut focused = false;
        for node in workspace_nodes {
            let mut workspace = Workspace::from_node(node)?;
            focused |= I3::parse_windows(node, &mut workspace, windows)?;
            output.workspaces.insert(workspace.num, workspace.id);
            workspaces.insert(workspace)?;
        }

        Ok(focused)
    }

    fn parse_windows(
        node: &Node,
        workspace: &mut Workspace,
        windows: &mut Windows,
    ) -> Result<bool, Error> {
        if node.window.is_some() {
            let window = Window::from_node(workspace.id, node)?;
            workspace.windows.push(window.id);
            windows.insert(window)?;
            Ok(node.focused)
        } else {
            let mut focused = false;
            for node in &node.nodes {
                focused |= I3::parse_windows(node, workspace, windows)?;
            }
            Ok(focused)
        }
    }

    pub async fn new() -> Result<Self, Error> {
        let mut i3 = async_i3ipc::I3::connect().await?;

        let mut focused_output = String::new();
        let mut outputs = Outputs {
            outputs: HashMap::new(),
        };
        let mut workspaces = Workspaces {
            workspaces: HashMap::new(),
        };
        let mut windows = Windows {
            windows: HashMap::new(),
        };

        let tree = i3.get_tree().await?;
        for node in &tree.nodes {
            let Some(name) = node.name.clone() else { continue; };

            if name == "__i3" {
                continue;
            }

            // Collect all output workspaces first.
            let mut output = Output {
                name: name.clone(),
                focused_workspace: usize::MAX, // Oof Dangerous!!
                workspaces: BTreeMap::new(),
            };
            let focused = Self::parse_workspaces(node, &mut output, &mut workspaces, &mut windows)?;
            if focused {
                focused_output = output.name.clone();
            }
            outputs.insert(output)?;
        }

        let outputs_reply = i3.get_outputs().await?;
        for output in &outputs_reply {
            if let reply::Output {
                name,
                current_workspace: Some(workspace_name),
                ..
            } = output
            {
                let output = outputs.get_mut(name)?;
                let workspace = workspaces.get_by_name(workspace_name)?;
                output.focused_workspace = workspace.id;
            }
        }

        let state = State {
            focused_output,
            outputs,
            workspaces,
            windows,
        };

        let state = Arc::new(Mutex::new(state));
        Ok(I3 { state })
    }

    async fn get_state(&self) -> MutexGuard<State> {
        self.state.lock().await
    }
}

async fn handle_events(
    i3: async_i3ipc::I3,
    state: Arc<Mutex<State>>,
    notify_sender: mpsc::Sender<()>,
    mut message_receiver: broadcast::Receiver<ItemMessage>,
) {
    let mut event_stream = i3.listen();
    loop {
        tokio::select! {
            message = message_receiver.recv() => {
                match message {
                    Err(err) => { error!("{err}"); break; },
                    Ok(()) => break,
                }
            }
            maybe_event = event_stream.next() => {
                match maybe_event {
                    Err(err) => error!("{err}"),
                    Ok(event) => {
                        print_error(handle_event(event, &state).await);
                        match notify_sender.send(()).await {
                            Err(err) => { error!("{err}"); break; },
                            Ok(()) => {},
                        }
                    },
                }
            }

        }
    }
}

async fn handle_event(event: Event, state: &Arc<Mutex<State>>) -> Result<(), Error> {
    let mut state = state.lock().await;
    dbg!("before", &state);
    match &event {
        Event::Output(x) => debug!("{x:#?}"),
        Event::Workspace(x) => {
            let WorkspaceData {
                change, current, ..
            } = x.deref();
            let State {
                ref mut focused_output,
                ref mut outputs,
                ref mut workspaces,
                ..
            } = state.deref_mut();
            dbg!(change);
            match change {
                WorkspaceChange::Focus => {
                    let node = current.as_ref().unwrap();
                    let workspace = workspaces.get(node.id)?;
                    let output = outputs.get_mut(&workspace.output)?;
                    output.focused_workspace = workspace.id;
                    *focused_output = output.name.clone();
                }
                WorkspaceChange::Init => {
                    let node = current.as_ref().unwrap();
                    let workspace = Workspace::from_node(node)?;
                    let output = outputs.get_mut(&workspace.output)?;
                    output.workspaces.insert(workspace.num, workspace.id);
                    workspaces.insert(workspace)?;
                }
                WorkspaceChange::Empty => {
                    let node = current.as_ref().unwrap();
                    let workspace = workspaces.get(node.id)?;
                    let output = outputs.get_mut(&workspace.output)?;
                    output.remove_workspace(workspace.id);
                    workspaces.remove(workspace.id)?;
                }
                WorkspaceChange::Urgent => {
                    let node = current.as_ref().unwrap();
                    let workspace = workspaces.get_mut(node.id)?;
                    workspace.urgent = node.urgent;
                }
                WorkspaceChange::Rename => todo!(),
                WorkspaceChange::Reload => todo!(),
                WorkspaceChange::Restored => todo!(),
                WorkspaceChange::Move => {
                    let node = current.as_ref().unwrap();
                    let workspace = workspaces.get_mut(node.id)?;
                    let output = outputs.get_mut(&workspace.output)?;
                    output.remove_workspace(workspace.id);

                    let output = node_output(node)?;
                    let output = outputs.get_mut(output)?;
                    output.workspaces.insert(workspace.num, workspace.id);
                    workspace.output = output.name.clone();
                }
            }
        }
        Event::Window(x) => {
            let WindowData { change, container } = x.deref();
            let State {
                ref mut workspaces,
                ref mut windows,
                ..
            } = state.deref_mut();

            let window_id = node_window(container)?;
            match change {
                WindowChange::New => {
                    let output = container.output.as_ref().unwrap();
                    let workspace_id =
                        find_workspace_of_window_in_output(output, window_id).await?;
                    let workspace = workspaces.get_mut(workspace_id)?;

                    let window = Window::from_node(workspace.id, container)?;
                    workspace.windows.push(window.id);
                    windows.insert(window)?;
                }
                WindowChange::Close => {
                    let window = windows.get(window_id)?;
                    let workspace = workspaces.get_mut(window.workspace)?;
                    workspace.remove_window(window_id);
                    windows.remove(window_id)?;
                }
                WindowChange::Focus => {
                    let window = windows.get(window_id)?;
                    let workspace = workspaces.get_mut(window.workspace)?;
                    workspace.focused_window = Some(window_id)
                }
                WindowChange::Title => {
                    let window = windows.get_mut(window_id)?;
                    window.title = node_name(container)?.clone();
                }
                WindowChange::Move => {
                    let window = windows.get_mut(window_id)?;
                    let old_workspace = workspaces.get_mut(window.workspace)?;
                    old_workspace.remove_window(window_id);

                    let output = container.output.as_ref().unwrap();
                    let workspace_id =
                        find_workspace_of_window_in_output(output, window_id).await?;
                    let workspace = workspaces.get_mut(workspace_id)?;
                    window.workspace = workspace_id;
                    workspace.windows.push(window.id);
                }
                WindowChange::Urgent => {
                    let window = windows.get_mut(window_id)?;
                    window.urgent = container.urgent;
                }
                _ => {}
            }
        }
        _ => error!("Received unwanted event: {event:?}"),
    }

    dbg!("after", &state);
    state.validate().await?;
    Ok(())
}
