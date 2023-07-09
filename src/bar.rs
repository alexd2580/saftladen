use saftbar::{bar::ColoredText, xft::RGBA};

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

pub const LEFT_POWERLINE: (&str, &str) = ("", "");
pub const RIGHT_POWERLINE: (&str, &str) = ("", "");
pub const LEFT_ROUNDED: (&str, &str) = ("", "");
pub const RIGHT_ROUNDED: (&str, &str) = ("", "");

pub const WHITE: RGBA = (255, 255, 255, 255);
pub const LIGHT_GRAY: RGBA = (204, 204, 204, 255);
pub const GRAY: RGBA = (112, 112, 112, 255);
pub const DARK_GRAY: RGBA = (69, 69, 69, 255);
pub const DARKEST_GRAY: RGBA = (42, 42, 42, 255);
pub const BLACK: RGBA = (0, 0, 0, 255);
pub const RED: RGBA = (200, 20, 20, 255);
pub const TOO_RED: RGBA = (255, 0, 0, 255);
pub const DARK_GREEN: RGBA = (0, 128, 0, 255);
pub const INFO_YELLOW: RGBA = (205, 205, 0, 255);
pub const GREEN: RGBA = (20, 200, 20, 255);
pub const BLUE: RGBA = (20, 20, 200, 255);

pub const WHITE_ON_BLACK: (RGBA, RGBA) = (BLACK, WHITE);
pub const INACTIVE: (RGBA, RGBA) = (DARKEST_GRAY, GRAY);
pub const SEMIACTIVE: (RGBA, RGBA) = (DARK_GRAY, LIGHT_GRAY);
pub const ACTIVE: (RGBA, RGBA) = (BLUE, LIGHT_GRAY);
pub const URGENT: (RGBA, RGBA) = (RED, LIGHT_GRAY);

pub const GOOD: (RGBA, RGBA) = (GREEN, BLACK);
pub const NEUTRAL: (RGBA, RGBA) = (DARK_GREEN, LIGHT_GRAY);
pub const INFO: (RGBA, RGBA) = (INFO_YELLOW, BLACK);
pub const WARN: (RGBA, RGBA) = (RED, LIGHT_GRAY);
pub const CRITICAL: (RGBA, RGBA) = (TOO_RED, WHITE);

pub fn mix_colors(value: f32, min: f32, max: f32, min_color: RGBA, max_color: RGBA) -> RGBA {
    if value <= min {
        return min_color;
    }
    if value >= max {
        return max_color;
    }

    let alpha = ((value - min) / (max - min)).clamp(0f32, 1f32);
    let inv_alpha = 1.0 - alpha;

    let (r1, g1, b1, a1) = min_color;
    let (r2, g2, b2, a2) = max_color;
    (
        (r1 as f32 * alpha + r2 as f32 * inv_alpha) as u8,
        (g1 as f32 * alpha + g2 as f32 * inv_alpha) as u8,
        (b1 as f32 * alpha + b2 as f32 * inv_alpha) as u8,
        (a1 as f32 * alpha + a2 as f32 * inv_alpha) as u8,
    )
}

pub fn mix_colors_multi(
    value: f32,
    min: f32,
    min_green: f32,
    max_green: f32,
    max: f32,
    min_color: RGBA,
    green_color: RGBA,
    max_color: RGBA,
) -> RGBA {
    if value <= max_green {
        let alpha = ((value - min) / (min_green - min)).clamp(0f32, 1f32);
        let inv_alpha = 1.0 - alpha;

        let (r1, g1, b1, a1) = min_color;
        let (r2, g2, b2, a2) = green_color;
        (
            (r1 as f32 * inv_alpha + r2 as f32 * alpha) as u8,
            (g1 as f32 * inv_alpha + g2 as f32 * alpha) as u8,
            (b1 as f32 * inv_alpha + b2 as f32 * alpha) as u8,
            (a1 as f32 * inv_alpha + a2 as f32 * alpha) as u8,
        )
    } else {
        let alpha = ((value - max_green) / (max - max_green)).clamp(0f32, 1f32);
        let inv_alpha = 1.0 - alpha;

        let (r1, g1, b1, a1) = green_color;
        let (r2, g2, b2, a2) = max_color;
        (
            (r1 as f32 * inv_alpha + r2 as f32 * alpha) as u8,
            (g1 as f32 * inv_alpha + g2 as f32 * alpha) as u8,
            (b1 as f32 * inv_alpha + b2 as f32 * alpha) as u8,
            (a1 as f32 * inv_alpha + a2 as f32 * alpha) as u8,
        )
    }
}

pub fn font_color(bg: RGBA) -> RGBA {
    let luminance = 0.299 * bg.0 as f32 + 0.587 * bg.1 as f32 + 0.114 * bg.2 as f32;
    if luminance > 128.0 {
        BLACK
    } else {
        WHITE
    }
}

fn get_separators(style: Style, direction: Direction) -> (&'static str, &'static str) {
    match (style, direction) {
        (Style::Powerline, Direction::Left) => LEFT_POWERLINE,
        (Style::Powerline, Direction::Right) => RIGHT_POWERLINE,
        (Style::Rounded, Direction::Left) => LEFT_ROUNDED,
        (Style::Rounded, Direction::Right) => RIGHT_ROUNDED,
    }
}

pub struct SectionWriter {
    texts: Vec<ColoredText>,

    style: Style,
    direction: Direction,

    bg: RGBA,
    fg: RGBA,
}

impl SectionWriter {
    pub fn new() -> Self {
        SectionWriter {
            texts: Vec::new(),
            style: Style::Powerline,
            direction: Direction::Right,
            bg: BLACK,
            fg: WHITE,
        }
    }

    pub fn set_style(&mut self, style: Style) {
        self.style = style;
    }

    pub fn set_direction(&mut self, direction: Direction) {
        self.direction = direction;
    }

    pub fn write(&mut self, text: String) {
        self.texts.push(ColoredText {
            text,
            fg: self.fg,
            bg: self.bg,
        });
    }

    fn separate(&mut self, next_bg: RGBA, next_fg: RGBA) {
        let separators = get_separators(self.style, self.direction);

        if next_bg == self.bg {
            self.fg = BLACK;
            self.write(separators.1.to_owned());
        } else {
            match self.direction {
                Direction::Left => {
                    self.fg = next_bg;
                    self.write(separators.0.to_owned());
                    self.bg = next_bg;
                }
                Direction::Right => {
                    self.fg = self.bg;
                    self.bg = next_bg;
                    self.write(separators.0.to_owned());
                }
            }
        }

        self.fg = next_fg;
    }

    pub fn open_(&mut self, next_colors: (RGBA, RGBA)) {
        self.open(next_colors.0, next_colors.1);
    }

    pub fn open(&mut self, next_bg: RGBA, next_fg: RGBA) {
        self.separate(next_bg, next_fg);
    }

    pub fn split(&mut self) {
        self.separate(self.bg, self.fg);
    }

    pub fn close(&mut self) {
        self.separate(WHITE_ON_BLACK.0, WHITE_ON_BLACK.1);
    }

    pub fn unwrap(self) -> Vec<ColoredText> {
        self.texts
    }
}
