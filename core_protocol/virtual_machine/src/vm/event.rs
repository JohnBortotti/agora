use serde::{Serialize, Deserialize};
use ethnum::U256;

#[derive(PartialEq, Eq, Serialize, Deserialize, Debug, Clone)]
pub struct Event {
  pub name: String,
  pub address: String,
  pub topics: Vec<U256>,
}
