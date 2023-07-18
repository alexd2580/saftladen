use std::{cmp::Ordering, fs::File};

use bar::SectionWriter;
use error::Error;
use log::{debug, error};
use saftbar::bar::{Alignment, Bar};
use state_item::{
    new_item_action_channel, new_main_action_channel, ItemAction, ItemActionSender,
    MainActionReceiver, StateItem,
};
use tokio::signal;

use crate::state_item::MainAction;

mod bar;
mod error;
mod i3;
mod pulseaudio;
mod state_item;
mod system;
mod time;
mod weather;

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

struct StateItems {
    left: Vec<Box<dyn StateItem>>,
    right: Vec<Box<dyn StateItem>>,
}

fn init_state_items() -> StateItems {
    let i3 = i3::I3::new();
    let weather = weather::Weather::new();
    let pulseaudio = pulseaudio::Pulseaudio::new();
    let system = system::System::new();
    let time = time::Time::new();

    StateItems {
        left: vec![Box::new(i3)],
        right: vec![
            Box::new(weather),
            Box::new(pulseaudio),
            Box::new(system),
            Box::new(time),
        ],
    }
}

async fn redraw(
    state_items: &StateItems,
    displays: &[(String, (isize, isize))],
    bar: &mut Bar,
) -> Result<(), Error> {
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
    Ok(())
}

async fn main_loop(
    main_action_receiver: &mut MainActionReceiver,
    item_action_sender: &mut ItemActionSender,
    state_items: &StateItems,
) -> Result<(), Error> {
    let mut displays = get_displays().await?;
    let mut bar = Bar::new();

    loop {
        tokio::select! {
            event = bar.next_x_event() => {
                debug!("{event:#?}");
            }
            _ = signal::ctrl_c() => {
                debug!("Received CTRL+C, terminating");
                break;
            }
            message = main_action_receiver.next() => {
                match message {
                    None => {}
                    Some(MainAction::Reinit) => {
                        displays = get_displays().await?;
                        bar = Bar::new();
                        if !item_action_sender.enqueue(ItemAction::Update) {
                            let msg = "Failed to enqueue item update action".to_owned();
                            return Err(Error::Local(msg));
                        }
                    },
                    Some(MainAction::Redraw) => redraw(state_items, &displays, &mut bar).await?,
                    Some(MainAction::Terminate) => break,
                }
            },
        }
    }
    Ok(())
}

fn init_logger() {
    let mut loggers: Vec<Box<dyn simplelog::SharedLogger>> = Vec::new();

    loggers.push(simplelog::TermLogger::new(
        simplelog::LevelFilter::Debug,
        simplelog::Config::default(),
        simplelog::TerminalMode::Mixed,
        simplelog::ColorChoice::Auto,
    ));

    let time = chrono::Local::now();
    // TODO adjust the log file to include the executable name?
    let log_file_name = time.format("/var/log/saftladen/%FT%T.log").to_string();
    let log_file = File::create(&log_file_name);
    let (log_file, log_file_err) = match log_file {
        Ok(log_file) => (Some(log_file), None),
        Err(log_file_err) => (None, Some(log_file_err)),
    };
    if let Some(log_file) = log_file {
        loggers.push(simplelog::WriteLogger::new(
            simplelog::LevelFilter::Debug,
            simplelog::Config::default(),
            log_file,
        ));
    }

    simplelog::CombinedLogger::init(loggers).unwrap();

    if let Some(err) = log_file_err {
        error!("Failed to initialize file logger at {log_file_name}: {err}");
    }
}

#[tokio::main]
async fn main() {
    init_logger();

    let (main_action_sender, mut main_action_receiver) = new_main_action_channel();
    let (mut item_action_sender, _item_action_receiver) = new_item_action_channel();

    // Start all state items as coroutines.
    let mut state_items = init_state_items();
    let threads = state_items
        .left
        .iter_mut()
        .chain(state_items.right.iter_mut())
        .map(|item| item.start_coroutine(main_action_sender.clone(), item_action_sender.listen()))
        .collect::<Vec<_>>();

    // Wait for messages, then rerender the respective part.
    if let Err(err) = main_loop(
        &mut main_action_receiver,
        &mut item_action_sender,
        &state_items,
    )
    .await
    {
        error!("Main loop terminated with: {err}");
    }

    // Notify all threads about app termination and wait for them to terminate.
    let _ = item_action_sender.enqueue(ItemAction::Terminate);
    futures::future::join_all(threads).await;
}
