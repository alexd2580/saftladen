use std::sync::Arc;

use chrono::{Local, Timelike};
use log::{error, warn};
use saftbar::xft::RGBA;
use serde::Deserialize;
use tokio::{
    sync::{broadcast, mpsc, Mutex},
    task::JoinHandle,
};

use crate::{
    bar::{
        font_color, mix_colors_multi, Direction, SectionWriter, Style, BLUE, DARK_GREEN, RED, WARN,
    },
    error::Error,
    ItemMessage, StateItem,
};

#[derive(Debug)]
struct WeatherData {
    temp: i32,
    condition_code: u32,
    condition: String,

    midnight_to_sunrise: chrono::Duration,
    midnight_to_sunset: chrono::Duration,
}

#[derive(Deserialize, Debug)]
struct WttrInFormatJ1CurrentConditionWeatherDesc {
    value: String,
}

#[derive(Deserialize, Debug)]
#[allow(non_snake_case)]
struct WttrInFormatJ1CurrentCondition {
    temp_C: String,
    weatherCode: String,
    weatherDesc: Vec<WttrInFormatJ1CurrentConditionWeatherDesc>,
}

#[derive(Deserialize, Debug)]
struct WttrInFormatJ1WeatherAstronomy {
    sunrise: String,
    sunset: String,
}

#[derive(Deserialize, Debug)]
struct WttrInFormatJ1Weather {
    astronomy: Vec<WttrInFormatJ1WeatherAstronomy>,
}

#[derive(Deserialize, Debug)]
struct WttrInFormatJ1 {
    current_condition: Vec<WttrInFormatJ1CurrentCondition>,
    weather: Vec<WttrInFormatJ1Weather>,
}

fn am_pm_to_duration_since_midnight(text: &str) -> chrono::Duration {
    let bytes = text.as_bytes();
    // The ascii byte value of '0' is 48.
    let mut hours = 10 * (bytes[0] as i64 - 48) + (bytes[1] as i64 - 48);
    let minutes = 10 * (bytes[3] as i64 - 48) + (bytes[4] as i64 - 48);

    // The ascii byte value of 'P' is 80.
    if bytes[6] == 80 {
        hours += 12;
    }

    chrono::Duration::minutes(hours * 60 + minutes)
}

fn handle_response(data: &WttrInFormatJ1) -> WeatherData {
    let current_condition = &data.current_condition[0];
    let astronomy = &data.weather[0].astronomy[0];
    WeatherData {
        temp: current_condition.temp_C.parse().unwrap(),
        condition_code: current_condition.weatherCode.parse().unwrap(),
        condition: current_condition.weatherDesc[0].value.clone(),
        midnight_to_sunrise: am_pm_to_duration_since_midnight(&astronomy.sunrise),
        midnight_to_sunset: am_pm_to_duration_since_midnight(&astronomy.sunset),
    }
}

async fn get_weather_data() -> Option<WeatherData> {
    match reqwest::get("https://wttr.in?format=j1").await {
        Ok(response) => match response.json::<WttrInFormatJ1>().await {
            Ok(data) => Some(handle_response(&data)),
            Err(err) => {
                warn!("{err}");
                None
            }
        },
        Err(err) => {
            warn!("{err}");
            None
        }
    }
}

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
            let font_color = font_color(temp_color);
            writer.open(temp_color, font_color);

            let since_midnight =
                chrono::Duration::seconds(Local::now().num_seconds_from_midnight() as i64);
            let is_day = data.midnight_to_sunrise <= since_midnight
                && since_midnight < data.midnight_to_sunset;
            let weather_icon = weather_icon(data.condition_code, is_day);

            writer.write(format!(
                "{} {} {}°C ",
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

            writer.write(format!("{icon} in {:0>2}:{:0>2} ", hours, minutes));
            writer.close();
        } else {
            writer.open_(WARN);
            writer.write("󰅤".to_string());
            writer.close();
        }
        Ok(())
    }

    fn start_coroutine(
        &self,
        notify_sender: mpsc::Sender<()>,
        message_receiver: broadcast::Receiver<ItemMessage>,
    ) -> JoinHandle<()> {
        tokio::spawn(weather_coroutine(
            self.0.clone(),
            notify_sender,
            message_receiver,
        ))
    }
}

async fn weather_coroutine(
    state: SharedData,
    notify_sender: mpsc::Sender<()>,
    mut message_receiver: broadcast::Receiver<ItemMessage>,
) {
    loop {
        {
            let new_state = get_weather_data().await;
            let mut state_lock = state.lock().await;
            *state_lock = new_state;
            match notify_sender.send(()).await {
                Err(err) => {
                    error!("{err}");
                    return;
                }
                Ok(()) => {}
            }
        }

        tokio::select! {
            message = message_receiver.recv() => {
                match message {
                    Err(err) => { error!("{err}"); break; }
                    Ok(()) => break,
                }
            }
            _ = tokio::time::sleep(tokio::time::Duration::from_secs(1800)) => {}

        }
    }
}
