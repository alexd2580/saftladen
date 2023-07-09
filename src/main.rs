use bar::SectionWriter;
use log::{debug, error};
use saftbar::bar::{Alignment, Bar};

use error::{print_error, Error};
use tokio::{
    signal,
    sync::{broadcast, mpsc},
    task::JoinHandle,
};

mod bar;
mod error;
mod i3;
mod pulseaudio;
mod system;
mod time;
mod weather;
use std::cmp::Ordering;

type ItemMessage = ();

async fn get_displays() -> Result<Vec<(String, (isize, isize))>, Error> {
    let mut displays = i3::I3::displays().await?;
    displays.sort_by(|a, b| {
        let xcomp = a.1 .0.partial_cmp(&b.1 .0).unwrap();
        if xcomp == Ordering::Equal {
            a.1 .1.partial_cmp(&b.1 .1).unwrap()
        } else {
            xcomp
        }
    });
    Ok(displays)
}

#[async_trait::async_trait]
trait StateItem {
    async fn print(&self, writer: &mut SectionWriter, output: &str) -> Result<(), Error>;

    fn start_coroutine(
        &self,
        notify_sender: mpsc::Sender<()>,
        message_receiver: broadcast::Receiver<ItemMessage>,
    ) -> JoinHandle<()>;
}

struct StateItems {
    left: Vec<Box<dyn StateItem>>,
    right: Vec<Box<dyn StateItem>>,
}

async fn init_state_items() -> Result<StateItems, Error> {
    let i3 = i3::I3::new().await?;
    let weather = weather::Weather::new();
    let pulseaudio = pulseaudio::Pulseaudio::new();
    let system = system::System::new();
    let time = time::Time::new();

    Ok(StateItems {
        left: vec![Box::new(i3)],
        right: vec![
            Box::new(weather),
            Box::new(pulseaudio),
            Box::new(system),
            Box::new(time),
        ],
    })
}

async fn main_loop(
    bar: &mut Bar,
    mut rerender_receiver: mpsc::Receiver<()>,
    state_items: &StateItems,
    displays: &Vec<(String, (isize, isize))>,
) -> Result<(), Error> {
    loop {
        tokio::select! {
            _ = signal::ctrl_c() => {
                debug!("Received CTRL+C, terminating");
                break;
            }
            _ = rerender_receiver.recv() => {
                debug!("Received update, rerendering");

                let state_items = &state_items;

                bar.clear_monitors();
                for (index, display) in displays.iter().enumerate() {
                    let mut writer = SectionWriter::new();
                    for item in &state_items.left {
                        item.print(&mut writer, &display.0).await?;
                    }
                    bar.render_string(index, Alignment::Left, &writer.unwrap());

                    let mut writer = SectionWriter::new();
                    for item in &state_items.right {
                        item.print(&mut writer, &display.0).await?;
                    }
                    bar.render_string(index, Alignment::Right, &writer.unwrap());
                }
                bar.blit();
            },
        }
    }
    Ok(())
}

async fn run_main() -> Result<(), Error> {
    let displays = get_displays().await?;

    let (message_sender, _message_receiver) = broadcast::channel(32);
    let (rerender_sender, rerender_receiver) = mpsc::channel(32);

    let mut state_items = init_state_items().await?;
    let threads = state_items
        .left
        .iter_mut()
        .chain(state_items.right.iter_mut())
        .map(|item| item.start_coroutine(rerender_sender.clone(), message_sender.subscribe()))
        .collect::<Vec<_>>();

    let mut bar = Bar::new();
    let res = main_loop(&mut bar, rerender_receiver, &state_items, &displays).await;
    print_error(res);

    let _ = message_sender
        .send(())
        .map_err(|err| Error::Local(err.to_string()))?;
    futures::future::join_all(threads).await;

    Ok(())
}

#[tokio::main]
async fn main() {
    simple_logger::init_with_level(log::Level::Debug).unwrap();
    if let Err(err) = run_main().await {
        error!("{err}");
    }
}
