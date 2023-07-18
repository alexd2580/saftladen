use chrono::Local;
use tokio::task::JoinHandle;

use crate::{
    bar::{Direction, SectionWriter, Style, BLUE, LIGHT_GRAY},
    error::Error,
    state_item::{
        wait_seconds, ItemAction, ItemActionReceiver, MainAction, MainActionSender, StateItem,
    },
};

pub struct Time {
    format: String,
}

impl Time {
    pub fn new() -> Self {
        Self {
            format: "%a %F %R".to_owned(),
        }
    }
}

#[async_trait::async_trait]
impl StateItem for Time {
    async fn print(&self, writer: &mut SectionWriter, _output: &str) -> Result<(), Error> {
        let now = Local::now();
        writer.set_style(Style::Powerline);
        writer.set_direction(Direction::Left);
        writer.open(BLUE, LIGHT_GRAY);
        writer.write(format!(" {} ", now.format(&self.format)));
        writer.close();
        Ok(())
    }

    fn start_coroutine(
        &self,
        main_action_sender: MainActionSender,
        item_action_receiver: ItemActionReceiver,
    ) -> JoinHandle<()> {
        tokio::spawn(time_coroutine(main_action_sender, item_action_receiver))
    }
}

async fn time_coroutine(
    main_action_sender: MainActionSender,
    mut item_action_receiver: ItemActionReceiver,
) {
    loop {
        if !main_action_sender.enqueue(MainAction::Redraw).await {
            break;
        }

        tokio::select! {
            message = item_action_receiver.next() => {
                match message {
                    None | Some(ItemAction::Update)  => {},
                    Some(ItemAction::Terminate) => break,
                }
            }
            _ = wait_seconds(30) => {}
        }
    }
}
