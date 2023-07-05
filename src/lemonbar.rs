use std::io::Write;
use std::process::{Child, ChildStdin, Command, Stdio};

use crate::error::Error;

pub fn spawn_lemonbar() -> Result<Child, Error> {
    let executable = "lemonbar";
    let font = "UbuntuMono Nerd Font:size=12";
    let args = ["-f", &font, "-a", "30", "-u", "-4"];
    Ok(Command::new(executable)
        .args(args)
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .spawn()?)
}

#[derive(Clone, Copy)]
pub enum Alignment {
    Undefined,
    Left,
    Center,
    Right,
}

#[derive(Clone, Copy)]
pub enum Style {
    Powerline,
    Rounded,
}

#[derive(Clone, Copy)]
pub enum Direction {
    Left,
    Right,
}

pub const LEFT_POWERLINE: [&str; 2] = ["", ""];
pub const RIGHT_POWERLINE: [&str; 2] = ["", ""];
pub const LEFT_ROUNDED: [&str; 2] = ["", ""];
pub const RIGHT_ROUNDED: [&str; 2] = ["", ""];

pub const WHITE_ON_BLACK: [&str; 2] = ["#FF000000", "#FFFFFFFF"];
pub const INACTIVE: [&str; 2] = ["#FF2A2A2A", "#FF707070"];
pub const SEMIACTIVE: [&str; 2] = ["#FF454545", "#FFCCCCCC"];
pub const ACTIVE: [&str; 2] = ["#FF1010D0", "#FFCCCCCC"];
pub const URGENT: [&str; 2] = ["#FFD01010", "#FFCCCCCC"];

pub const GOOD: [&str; 2] = ["#FF10D010", "#FF000000"];
pub const NEUTRAL: [&str; 2] = ["#FF008000", "#FFCCCCCC"];
pub const INFO: [&str; 2] = ["#FFCDCD00", "#FF000000"];
pub const WARN: [&str; 2] = ["#FFD01010", "#FFCCCCCC"];
pub const CRITICAL: [&str; 2] = ["#FFFF0000", "#FFFFFFFF"];

fn get_separators(style: Style, direction: Direction) -> &'static [&'static str; 2] {
    match (style, direction) {
        (Style::Powerline, Direction::Left) => &LEFT_POWERLINE,
        (Style::Powerline, Direction::Right) => &RIGHT_POWERLINE,
        (Style::Rounded, Direction::Left) => &LEFT_ROUNDED,
        (Style::Rounded, Direction::Right) => &RIGHT_ROUNDED,
    }
}

pub struct LemonbarWriter {
    stream: ChildStdin,

    display: Option<usize>,
    alignment: Alignment,
    style: Style,
    direction: Direction,

    bg: String,
    fg: String,
}

impl LemonbarWriter {
    pub fn new(stream: ChildStdin) -> Self {
        LemonbarWriter {
            stream,
            display: None,
            alignment: Alignment::Undefined,
            style: Style::Powerline,
            direction: Direction::Right,
            bg: "#FF000000".to_owned(),
            fg: "#FFFFFFFF".to_owned(),
        }
    }

    fn display(&mut self, display: Option<usize>) {
        if let Some(index) = display {
            self.tag(&format!("S{index}"));
        }
    }

    pub fn set_display(&mut self, display: Option<usize>) {
        // Close old section, open new.
        self.display(self.display);
        self.display = display;
        self.display(self.display);
    }
    fn align(&mut self, alignment: Alignment) {
        match alignment {
            Alignment::Undefined => {}
            Alignment::Left => self.tag("l"),
            Alignment::Center => self.tag("c"),
            Alignment::Right => self.tag("r"),
        }
    }

    pub fn set_alignment(&mut self, alignment: Alignment) {
        // Close old section, open new.
        self.align(self.alignment);
        self.alignment = alignment;
        self.align(self.alignment);
    }

    pub fn set_style(&mut self, style: Style) {
        self.style = style;
    }

    pub fn set_direction(&mut self, direction: Direction) {
        self.direction = direction;
    }

    pub fn write(&mut self, data: &str) {
        self.stream.write_all(data.as_bytes()).unwrap();
    }

    fn tag(&mut self, tag: &str) {
        self.write("%{");
        self.write(tag);
        self.write("}");
    }

    fn separate(&mut self, next_bg: &str, next_fg: &str) {
        let separators = get_separators(self.style, self.direction);

        if next_bg == self.bg {
            self.tag("F#FF000000");
            self.write(separators[1]);
        } else {
            match self.direction {
                Direction::Left => {
                    self.tag(&format!("F{next_bg}"));
                    self.write(separators[0]);
                    self.tag("R");
                }
                Direction::Right => {
                    self.tag("R");
                    self.tag(&format!("B{next_bg}"));
                    self.write(separators[0]);
                }
            }
        }

        self.tag(&format!("F{next_fg}"));

        self.fg = next_fg.to_owned();
        self.bg = next_bg.to_owned();
    }

    pub fn open_(&mut self, next_colors: &[&str; 2]) {
        self.open(next_colors[0], next_colors[1]);
    }

    pub fn open(&mut self, next_bg: &str, next_fg: &str) {
        self.separate(next_bg, next_fg);
    }

    pub fn split(&mut self) {
        self.separate(&self.bg.clone(), &self.fg.clone());
    }

    pub fn close(&mut self) {
        self.separate(WHITE_ON_BLACK[0], WHITE_ON_BLACK[1]);
    }

    pub fn flush(&mut self) {
        self.write("\n");
        self.stream.flush().unwrap();
    }
}
