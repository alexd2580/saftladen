use async_i3ipc::reply::Node;
use std::collections::{BTreeMap, HashMap};

use crate::error::Error;

// TODO visible but INACTIVE

#[derive(Debug)]
pub struct Window {
    pub id: usize,
    pub active: bool,
    pub urgent: bool,
    pub title: String,
}

impl Window {
    fn from_node(node: &Node) -> Result<Self, Error> {
        Ok(Window {
            id: node.id,
            active: node.focused,
            urgent: node.urgent,
            title: node
                .name
                .as_ref()
                .ok_or_else(|| Error::Local("Window doesn't have a title".to_string()))?
                .clone(),
        })
    }
}

#[derive(Debug)]
pub struct Workspace {
    pub name: String,
    pub active: bool,
    pub visible: bool,
    pub urgent: bool,
    pub windows: BTreeMap<usize, Window>,
}

impl Workspace {
    fn from_node(node: &Node) -> Result<Self, Error> {
        let name = node.name.as_ref().cloned().unwrap();
        let mut active = false;
        let mut urgent = false;
        let mut windows = BTreeMap::new();

        let mut queue = vec![node];
        while let Some(node) = queue.pop() {
            if let Some(window_id) = node.window {
                let window = Window::from_node(node)?;
                active |= window.active;
                urgent |= window.urgent;
                if windows.insert(window_id, window).is_some() {
                    let msg = format!("Window '{window_id}' is present multiple times");
                    return Err(Error::Local(msg));
                }
            } else {
                queue.extend(node.nodes.iter());
            }
        }

        Ok(Self {
            name,
            active,
            visible: false,
            urgent,
            windows,
        })
    }
}

#[derive(Debug)]
pub struct Output {
    pub workspaces: BTreeMap<i32, Workspace>,
}

impl Output {
    fn from_node(node: &Node) -> Result<Self, Error> {
        let mut workspaces = BTreeMap::new();

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
        for node in workspace_nodes {
            let num = node.num.ok_or_else(|| {
                let msg = format!("Node does not have a num: {node:#?}");
                Error::Local(msg)
            })?;
            let workspace = Workspace::from_node(node)?;
            if workspaces.insert(num, workspace).is_some() {
                let msg = format!("Workspace '{num}' is present multiple times");
                return Err(Error::Local(msg));
            }
        }

        Ok(Self { workspaces })
    }
}

#[derive(Debug)]
pub struct State(pub HashMap<String, Output>);

impl State {
    pub async fn query(i3: &mut async_i3ipc::I3) -> Result<Self, Error> {
        let tree = i3.get_tree().await?;
        let mut state = State::from_node(&tree)?;

        // Update state output data with info from i3 outputs.
        let outputs = i3.get_outputs().await?;
        for output_info in outputs {
            if let (Some(state_output), Some(focused_workspace)) = (
                state.0.get_mut(&output_info.name),
                output_info.current_workspace,
            ) {
                let workspace = state_output
                    .workspaces
                    .iter_mut()
                    .find_map(|(_, workspace)| {
                        (workspace.name == focused_workspace).then_some(workspace)
                    })
                    .unwrap();

                workspace.visible = true;
            }
        }

        Ok(state)
    }

    fn from_node(node: &Node) -> Result<Self, Error> {
        let mut outputs = HashMap::new();
        let output_nodes = node
            .nodes
            .iter()
            .filter(|node| node.name.as_ref().is_some_and(|name| name != "__i3"));
        for node in output_nodes {
            let name = node.name.as_ref().ok_or_else(|| {
                let msg = format!("Node does not have a name: {node:#?}");
                Error::Local(msg)
            })?;
            let output = Output::from_node(node)?;
            if outputs.insert(name.clone(), output).is_some() {
                let msg = format!("Output '{name}' is present multiple times");
                return Err(Error::Local(msg));
            }
        }

        Ok(State(outputs))
    }
}
