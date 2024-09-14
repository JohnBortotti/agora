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


  pub fn insert(&mut self, key: &[u8], value: table_entry::TableEntry) {
    let key_str = std::str::from_utf8(&key).expect("Invalid UTF-8 sequence").to_string();
    match self.entries.insert(key_str.clone(), value) {
      Some(old_value) => {
        println!("Replaced entry for key: {}", key_str);
        self.size -= old_value.size();
        self.size += self.entries.get(&key_str).unwrap().size();
      }
      None => {
        println!("Inserted new entry for key: {}", key_str);
        self.size += self.entries.get(&key_str).unwrap().size();
      }
    }
  }


  pub fn delete(&mut self, key: &str) -> Option<table_entry::TableEntry> {
    match self.entries.remove(key) {
      Some(removed_entry) => {
        self.size -= removed_entry.size();
        println!("Deleted entry for key: {}", key);
        Some(removed_entry)
      }
      None => {
        println!("Key not found: {}", key);
        None
      }
    }
  }
}
