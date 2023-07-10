use log::error;

use tokio::{
    sync::{broadcast, mpsc},
    task::JoinHandle,
};

use crate::bar::SectionWriter;
use crate::error::Error;

pub struct Notifyer(pub mpsc::Sender<()>);

impl Notifyer {
    pub async fn send(&self) -> bool {
        match self.0.send(()).await {
            Err(err) => {
                error!("{err}");
                false
            }
            Ok(()) => true,
        }
    }
}

pub type ItemMessage = ();
pub struct Receiver(pub broadcast::Receiver<ItemMessage>);

impl Receiver {
    pub async fn recv(&mut self) -> Option<ItemMessage> {
        match self.0.recv().await {
            Err(err) => {
                error!("{err}");
                None
            }
            Ok(message) => Some(message),
        }
    }
}

#[async_trait::async_trait]
pub trait StateItem {
    async fn print(&self, writer: &mut SectionWriter, output: &str) -> Result<(), Error>;
    fn start_coroutine(&self, notifyer: Notifyer, receiver: Receiver) -> JoinHandle<()>;
}

pub async fn wait_seconds(num_seconds: u64) {
    tokio::time::sleep(tokio::time::Duration::from_secs(num_seconds)).await;
}
