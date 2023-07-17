use std::sync::Arc;

use chrono::{Local, Timelike};
use saftbar::xft::RGBA;
use tokio::{sync::Mutex, task::JoinHandle};

use crate::{
    bar::{mix_colors_multi, Direction, SectionWriter, Style, BLUE, DARK_GREEN, RED},
    error::Error,
    state_item::{wait_seconds, Notifyer, Receiver, StateItem},
    weather::wttrin::get_weather_data,
};

use self::wttrin::WeatherData;

mod wttrin;

type SharedData = Arc<Mutex<Option<WeatherData>>>;
pub struct Weather(SharedData);

impl Weather {
    pub fn new() -> Self {
        Self(Arc::new(Mutex::new(None)))
    }
}

fn weather_icon(code: u32, is_day: bool) -> &'static str {
    match code {
        // nf-weather-thunderstorm
        // 389 Moderate or heavy rain in area with thunder
        // 386 Patchy light rain in area with thunder
        // 200 Thundery outbreaks in nearby
        389 | 386 | 200 => "",

        // nf-weather-showers
        // 266 Light drizzle
        // 263 Patchy light drizzle
        // 293 Patchy light rain
        // 176 Patchy rain nearby
        // 296 Light rain
        // 353 Light rain shower
        266 | 263 | 293 | 176 | 296 | 353 => "",

        // nf-weather-rain
        // 302 Moderate rain
        // 299 Moderate rain at times
        // 356 Moderate or heavy rain shower
        // 308 Heavy rain
        // 305 Heavy rain at times
        // 359 Torrential rain shower
        302 | 299 | 356 | 308 | 305 | 359 => "",

        // nf-weather-snow
        // 179 Patchy snow nearby
        // 323 Patchy light snow
        // 326 Light snow
        // 368 Light snow showers
        179 | 323 | 326 | 368 => "",

        // nf-weather-snow_wind
        // 395 Moderate or heavy snow in area with thunder
        // 392 Patchy light snow in area with thunder
        // 329 Patchy moderate snow
        // 332 Moderate snow
        // 338 Heavy snow
        // 371 Moderate or heavy snow showers
        // 335 Patchy heavy snow
        // 227 Blowing snow
        // 230 Blizzard
        395 | 392 | 329 | 332 | 338 | 371 | 335 | 227 | 230 => "",

        // nf-weather-sleet
        // 365 Moderate or heavy sleet showers
        // 362 Light sleet showers
        // 350 Ice pellets
        // 320 Moderate or heavy sleet
        // 317 Light sleet
        // 185 Patchy freezing drizzle nearby
        // 182 Patchy sleet nearby
        // 377 Moderate or heavy showers of ice pellets
        // 311 Light freezing rain
        // 374 Light showers of ice pellets
        // 284 Heavy freezing drizzle  w
        // 281 Freezing drizzle
        // 314 Moderate or Heavy freezing rain
        365 | 362 | 350 | 320 | 317 | 185 | 182 | 377 | 311 | 374 | 284 | 281 | 314 => "",

        // nf-weather-fog
        // 260 Freezing fog
        // 248 Fog
        // 143 Mist
        260 | 248 | 143 => "",

        // nf-weather-cloud
        // 122 Overcast
        // 119 Cloudy
        // 116 Partly Cloudy
        122 | 119 | 116 => "",
        // nf-weather-night_clear
        // nf-weather-day_sunny
        // 113 Clear/Sunny
        113 => {
            if is_day {
                ""
            } else {
                ""
            }
        }
        _ => "unknown",
    }
}

pub const COLD: RGBA = BLUE;
pub const NORMAL: RGBA = DARK_GREEN;
pub const HOT: RGBA = RED;

#[async_trait::async_trait]
impl StateItem for Weather {
    #[allow(clippy::cast_precision_loss)]
    async fn print(&self, writer: &mut SectionWriter, _output: &str) -> Result<(), Error> {
        writer.set_style(Style::Powerline);
        writer.set_direction(Direction::Left);

        let state = self.0.lock().await;
        if let Some(ref data) = *state {
            let temp_color = mix_colors_multi(
                data.temp as f32,
                -5f32,
                0f32,
                20f32,
                25f32,
                COLD,
                NORMAL,
                HOT,
            );
            writer.open_bg(temp_color);

            let since_midnight =
                chrono::Duration::seconds(i64::from(Local::now().num_seconds_from_midnight()));
            let is_day = data.midnight_to_sunrise <= since_midnight
                && since_midnight < data.midnight_to_sunset;
            let weather_icon = weather_icon(data.condition_code, is_day);

            writer.write(format!(
                "{} {} {} ",
                weather_icon, data.condition, data.temp
            ));
            writer.split();

            let (icon, duration) = {
                if since_midnight < data.midnight_to_sunrise {
                    ("", data.midnight_to_sunrise - since_midnight)
                } else if since_midnight < data.midnight_to_sunset {
                    ("", data.midnight_to_sunset - since_midnight)
                } else {
                    (
                        "",
                        data.midnight_to_sunrise + chrono::Duration::days(1) - since_midnight,
                    )
                }
            };

            let hours = duration.num_hours();
            let minutes = (duration - chrono::Duration::hours(hours)).num_minutes();

            writer.write(format!("{icon} in {hours:0>2}:{minutes:0>2} "));
            writer.close();
        } else {
            writer.open_bg(RED);
            writer.write("󰅤".to_string());
            writer.close();
        }
        Ok(())
    }

    fn start_coroutine(&self, notifyer: Notifyer, receiver: Receiver) -> JoinHandle<()> {
        tokio::spawn(weather_coroutine(self.0.clone(), notifyer, receiver))
    }
}

async fn weather_coroutine(state: SharedData, notifyer: Notifyer, mut receiver: Receiver) {
    loop {
        {
            let new_state = get_weather_data().await;
            let mut state_lock = state.lock().await;
            *state_lock = new_state;
            if !notifyer.send().await {
                break;
            }
        }

        tokio::select! {
            message = receiver.recv() => {
                if message.is_some() {
                    break;
                }
            }
            _ = wait_seconds(1800) => {}
        }
    }
}
