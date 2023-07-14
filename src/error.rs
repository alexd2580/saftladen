use std::fmt::Display;

#[derive(Debug)]
pub enum Error {
    Local(String),
    Io(std::io::Error),
    Tokio(tokio::task::JoinError),
    Pulsectl(pulsectl::ControllerError),
    Bar(saftbar::error::Error),
}

impl Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Error::Local(message) => write!(f, "{message}"),
            Error::Io(err) => write!(f, "Io Error\n{err:?}"),
            Error::Tokio(err) => write!(f, "Tokio Error\n{err:}"),
            Error::Pulsectl(err) => write!(f, "Pulsectl Error\n{err:?}"),
            Error::Bar(err) => write!(f, "Bar Error\n{err:?}"),
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

impl From<saftbar::error::Error> for Error {
    fn from(value: saftbar::error::Error) -> Self {
        Self::Bar(value)
    }
}
