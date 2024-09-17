use super::table_entry;
use std::collections::BTreeMap;

#[derive(Debug)]
pub struct Memtable {
  pub size: usize,
  pub entries: BTreeMap<String, table_entry::TableEntry>
}

impl Memtable {
  pub fn new() -> Self {
    Self {
      size: 0,
      entries: BTreeMap::new()
    }
  }

  pub fn search(&self, key: &[u8]) -> Option<&table_entry::TableEntry> {
    let key_str = std::str::from_utf8(key).expect("Invalid UTF-8 sequence");
    self.entries.get(key_str)
  }

  pub fn insert(&mut self, entry: table_entry::TableEntry) {
    let key_str = std::str::from_utf8(&entry.key).expect("Invalid UTF-8 sequence").to_string();
    match self.entries.insert(key_str.clone(), entry) {
      Some(old_value) => {
        self.size -= old_value.size();
        self.size += self.entries.get(&key_str).unwrap().size();
      }
      None => {
        self.size += self.entries.get(&key_str).unwrap().size();
      }
    }
  }

  pub fn delete(&mut self, key: &[u8], timestamp: i64) {
    let new_entry = table_entry::TableEntry {
      key: key.to_owned(),
      value: None,
      timestamp,
      deleted: true
    };

    let key_str = std::str::from_utf8(key).expect("Invalid UTF-8 sequence");
    match self.entries.get(key_str) {
      Some(val) => {
        if let Some(val) = &val.value {
          self.size -= val.len();
        }
        if let Some(entry) = self.entries.get_mut(key_str) {
          *entry = new_entry
        }
      }
      None => {
        self.entries.insert(key_str.to_string(), new_entry);
      }
    }
  }
}
