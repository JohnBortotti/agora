use serde::Serialize;
use ethnum::U256;

#[derive(PartialEq, Serialize, Debug)]
pub struct Event {
  pub address: String,
  pub topics: Vec<U256>,
  pub data: Vec<u8>,
}
