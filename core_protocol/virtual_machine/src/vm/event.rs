use serde::Serialize;
use ethnum::U256;

#[derive(PartialEq, Serialize, Debug, Clone)]
pub struct Event {
  pub name: String,
  pub address: String,
  pub topics: Vec<U256>,
  pub data: Vec<u8>,
}
