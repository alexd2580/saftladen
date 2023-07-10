use log::warn;
use serde::Deserialize;

#[derive(Debug)]
pub struct WeatherData {
    pub temp: i32,
    pub condition_code: u32,
    pub condition: String,

    pub midnight_to_sunrise: chrono::Duration,
    pub midnight_to_sunset: chrono::Duration,
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
    let mut hours = 10 * (i64::from(bytes[0]) - 48) + (i64::from(bytes[1]) - 48);
    let minutes = 10 * (i64::from(bytes[3]) - 48) + (i64::from(bytes[4]) - 48);

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

pub async fn get_weather_data() -> Option<WeatherData> {
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
