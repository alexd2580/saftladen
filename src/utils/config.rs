use std::fs;

use crate::error::Error;

pub fn read_config_file(name: &str) -> Result<String, Error> {
    let mut config_dir =
        dirs::config_dir().ok_or_else(|| Error::Local("Config directory unknown".to_string()))?;
    let mut config_path = config_dir.join("saftladen").join(name);
    Ok(fs::read_to_string(config_path)?)
}
