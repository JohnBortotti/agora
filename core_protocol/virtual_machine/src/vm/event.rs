use super::super::u256::U256;
use serde::Serialize;

#[derive(Serialize, Debug)]
pub struct Event {
  pub address: String,
  pub topics: Vec<U256>,
  pub data: Vec<u8>,
}
