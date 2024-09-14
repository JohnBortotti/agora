#[derive(Debug, Clone, PartialEq)]
pub struct TableEntry {
  pub key: Vec<u8>,
  pub value: Option<Vec<u8>>,
  pub timestamp: i64,
  pub deleted: bool
}

impl TableEntry {
  pub fn size(&self) -> usize {
    let key_size = self.key.len();
    let value_size = match &self.value {
      Some(val) => val.len(),
      None => 0, 
    };
    let timestamp_size = std::mem::size_of::<i64>();
    let deleted_size = std::mem::size_of::<bool>();

    key_size + value_size + timestamp_size + deleted_size
  }
}
