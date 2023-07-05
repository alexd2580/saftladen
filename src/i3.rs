use async_i3ipc::{
    event::Subscribe,
    reply::{self, Node},
};
use log::error;
use std::{
    collections::{BTreeMap, HashMap},
    sync::Arc,
};
use tokio::{
    sync::{broadcast, mpsc, Mutex, MutexGuard},
    task::JoinHandle,
};

use crate::{
    error::Error,
    lemonbar::{Direction, LemonbarWriter, Style, ACTIVE, INACTIVE, SEMIACTIVE, URGENT},
    ItemMessage, StateItem,
};

const NUMBERS: [&str; 6] = ["", "󰬺", "󰬻", "󰬼", "󰬽", "󰬾"];

const BROWSERS: [(&str, &str); 2] = [("chrom", ""), ("firefox", "")];
const SITES: [(&str, &str); 11] = [
    ("telegram", ""),
    ("slack", ""),
    ("github", ""),
    ("gitlab", ""),
    ("stack overflow", ""),
    ("youtube", ""),
    ("jira", ""),
    ("paypal", ""),
    ("gmail", ""),
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
    (".rs", ""),
    ("docker", ""),
];

const PROGRAMS: [(&str, &str); 15] = [
    ("vlc", "嗢"),
    ("mumble", ""),
    ("volume control", ""),
    ("telegram", ""),
    ("make", ""),
    ("psql", ""),
    ("htop", ""),
    ("man", ""),
    ("docker", ""),
    ("npm", ""),
    ("irssi", ""),
    ("gdb", ""),
    ("cargo", ""),
    ("zsh", ""),
    (": ~", ""),
];

#[derive(Debug)]
struct Window {
    focused: bool,
    urgent: bool,
    title: String,
}

impl Window {
    fn from_node(node: &Node) -> Result<Self, Error> {
        Ok(Window {
            focused: node.focused,
            urgent: node.urgent,
            title: node
                .name
                .as_ref()
                .ok_or_else(|| Error::Local(format!("Window doesn't have a title")))?
                .clone(),
        })
    }

    fn short_title(&self) -> String {
        let title = self.title.to_lowercase();
        for (browser, browser_icon) in BROWSERS {
            if title.contains(browser) {
                for (site, site_icon) in SITES {
                    if title.contains(site) {
                        return format!("{browser_icon}{site_icon}");
                    }
                }
                return browser_icon.to_owned();
            }
        }

        for (editor, editor_icon) in EDITORS {
            if title.contains(editor) {
                for (lang, lang_icon) in LANGUAGES {
                    if title.contains(lang) {
                        return format!("{editor_icon}{lang_icon}");
                    }
                }
                return editor_icon.to_owned();
            }
        }

        for (prgm, prgm_icon) in PROGRAMS {
            if title.contains(prgm) {
                return prgm_icon.to_owned();
            }
        }

        self.title.clone()
    }
}

#[derive(Debug)]
struct Workspace {
    focused: bool,
    urgent: bool,
    windows: BTreeMap<usize, Window>,
}

impl Workspace {
    fn from_node(node: &Node) -> Result<Self, Error> {
        let mut focused = false;
        let mut urgent = false;
        let mut windows = BTreeMap::new();

        let mut queue = vec![node];
        while let Some(node) = queue.pop() {
            if let Some(window_id) = node.window {
                let window = Window::from_node(node)?;
                focused |= window.focused;
                urgent |= window.urgent;
                if windows.insert(window_id, window).is_some() {
                    let msg = format!("Window '{window_id}' is present multiple times");
                    return Err(Error::Local(msg));
                }
            } else {
                queue.extend(node.nodes.iter());
            }
        }

        Ok(Workspace {
            focused,
            urgent,
            windows,
        })
    }
}

#[derive(Debug)]
struct Output {
    focused: bool,
    workspaces: BTreeMap<i32, Workspace>,
}

impl Output {
    fn from_node(node: &Node) -> Result<Self, Error> {
        let mut focused = false;
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
            let workspace = Workspace::from_node(&node)?;
            focused |= workspace.focused;
            if workspaces.insert(num, workspace).is_some() {
                let msg = format!("Workspace '{num}' is present multiple times");
                return Err(Error::Local(msg));
            }
        }

        Ok(Output {
            focused,
            workspaces,
        })
    }
}

#[derive(Debug)]
struct State(HashMap<String, Output>);

impl State {
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
            let output = Output::from_node(&node)?;
            if outputs.insert(name.clone(), output).is_some() {
                let msg = format!("Output '{name}' is present multiple times");
                return Err(Error::Local(msg));
            }
        }

        Ok(State(outputs))
    }

    fn choose_coloring(urgent: bool, active: bool, focused: bool) -> &'static [&'static str; 2] {
        match (urgent, active, focused) {
            (true, _, _) => &URGENT,
            (false, true, _) => &ACTIVE,
            (false, false, true) => &SEMIACTIVE,
            (false, false, false) => &INACTIVE,
        }
    }

    pub async fn print(&self, writer: &mut LemonbarWriter, output: &str) -> Result<(), Error> {
        let output = self
            .0
            .get(output)
            .ok_or_else(|| Error::Local(format!("Output '{output}' not found")))?;

        writer.set_style(Style::Rounded);
        writer.set_direction(Direction::Right);
        for (num, workspace) in output.workspaces.iter() {
            // TODO
            let workspace_coloring =
                Self::choose_coloring(workspace.urgent, workspace.focused, false);
            writer.open_(workspace_coloring);
            writer.write(&format!(" {} ", NUMBERS[*num as usize]));

            let windows = &workspace.windows;
            if windows.is_empty() {
            } else if windows.len() == 1 {
                let first = windows.first_key_value().unwrap();
                writer.write(&first.1.short_title());
            } else {
                for window in windows.values() {
                    writer.split();
                    writer.write(&format!(" {} ", window.short_title()));
                }
            }

            writer.close();
        }

        Ok(())
    }
}

pub struct I3 {
    state: Arc<Mutex<State>>,
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

    pub async fn new() -> Result<Self, Error> {
        let mut i3 = async_i3ipc::I3::connect().await?;
        let tree = i3.get_tree().await?;
        Ok(I3 {
            state: Arc::new(Mutex::new(State::from_node(&tree)?)),
        })
    }

    async fn get_state(&self) -> MutexGuard<State> {
        self.state.lock().await
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

async fn i3_coroutine(
    state: Arc<Mutex<State>>,
    notify_sender: mpsc::Sender<()>,
    mut message_receiver: broadcast::Receiver<ItemMessage>,
) {
    let (mut event_stream, mut i3) = match init_coroutine_resources().await {
        Ok(x) => x,
        Err(err) => {
            error!("{err}");
            return;
        }
    };

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
                    Ok(_) => {
                        let tree = i3.get_tree().await.unwrap();
                        let mut state = state.lock().await;
                        *state = State::from_node(&tree).unwrap();
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

#[async_trait::async_trait]
impl StateItem for I3 {
    async fn print(&self, writer: &mut LemonbarWriter, output: &str) -> Result<(), Error> {
        Ok(self.get_state().await.print(writer, output).await?)
    }

    fn start_coroutine(
        &self,
        notify_sender: mpsc::Sender<()>,
        message_receiver: broadcast::Receiver<ItemMessage>,
    ) -> JoinHandle<()> {
        tokio::spawn(i3_coroutine(
            self.state.clone(),
            notify_sender,
            message_receiver,
        ))
    }
}
