use std::fmt::Display;

use log::error;

#[derive(Debug)]
pub enum Error {
    Local(String),
    Io(std::io::Error),
    Tokio(tokio::task::JoinError),
    Pulsectl(pulsectl::ControllerError),
}

impl Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Error::Local(message) => write!(f, "{message}"),
            Error::Io(err) => write!(f, "Io Error\n{err:?}"),
            Error::Tokio(err) => write!(f, "Tokio Error\n{err:}"),
            Error::Pulsectl(error) => write!(f, "Pulsectl Error\n{error:?}"),
        }
    }
}

impl From<std::io::Error> for Error {
    fn from(err: std::io::Error) -> Self {
        Self::Io(err)
    }
}

impl From<tokio::task::JoinError> for Error {
    fn from(err: tokio::task::JoinError) -> Self {
        Self::Tokio(err)
    }
}

impl From<pulsectl::ControllerError> for Error {
    fn from(value: pulsectl::ControllerError) -> Self {
        Self::Pulsectl(value)
    }
}

pub fn print_error(res: Result<(), Error>) {
    match res {
        Err(err) => error!("{err}"),
        Ok(()) => {}
    }
}
