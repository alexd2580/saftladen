use async_i3ipc::reply::Node;
use std::collections::{BTreeMap, HashMap};

use crate::error::Error;

// TODO visible but INACTIVE
// TODO more icons?

const BROWSERS: [(&str, &str); 2] = [("chrom", ""), ("firefox", "")];
const SITES: [(&str, &str); 11] = [
    ("telegram", ""),
    ("slack", ""),
    ("github", ""),
    ("gitlab", ""),
    ("stack overflow", ""),
    ("youtube", ""),
    ("jira", "󰌃"),
    ("paypal", ""),
    ("gmail", "󰊫"),
    ("amazon", ""),
    ("google", ""),
];

const EDITORS: [(&str, &str); 1] = [("vim", "")];
const LANGUAGES: [(&str, &str); 13] = [
    (".html", ""),
    (".hpp", "ﭱ"),
    (".cpp", "ﭱ"),
    (".h", "ﭰ"),
    (".c", "ﭰ"),
    (".ts", "ﯤ"),
    (".tsx", "ﯤ"),
    (".py", ""),
    (".json", "ﬥ"),
    (".jsx", ""),
    (".js", ""),
    (".rs", ""),
    ("docker", ""),
];

const PROGRAMS: [(&str, &str); 17] = [
    ("vlc", "󰕼"),
    ("gimp", ""),
    ("mumble", ""),
    ("volume control", ""),
    ("telegram", ""),
    ("make", ""),
    ("psql", ""),
    ("htop", ""),
    ("man", ""),
    ("docker", ""),
    ("npm", ""),
    ("discord", "󰙯"),
    ("irssi", ""),
    ("gdb", ""),
    ("cargo", ""),
    ("zsh", ""),
    (": ~", ""),
];

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

    pub fn short_title(&self) -> String {
        let title = self.title.to_lowercase();
        for (browser, browser_icon) in BROWSERS {
            if title.contains(browser) {
                for (site, site_icon) in SITES {
                    if title.contains(site) {
                        return format!("{browser_icon}{site_icon}");
                    }
                }
                return browser_icon.to_string();
            }
        }

        for (editor, editor_icon) in EDITORS {
            if title.contains(editor) {
                for (lang, lang_icon) in LANGUAGES {
                    if title.contains(lang) {
                        return format!("{editor_icon}{lang_icon}");
                    }
                }
                return editor_icon.to_string();
            }
        }

        for (prgm, prgm_icon) in PROGRAMS {
            if title.contains(prgm) {
                return prgm_icon.to_string();
            }
        }

        if title.len() > 20 {
            self.title[..18].to_string()
        } else {
            self.title.to_string()
        }
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
