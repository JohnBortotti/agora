use std::path::{Path, PathBuf};
use std::fs::{File, OpenOptions};
use std::io::{BufWriter, Write};

use super::table_entry;

#[derive(Debug)]
pub struct WAL {
    pub path: PathBuf,
    file: BufWriter<File>
}

impl WAL {
  pub fn new(dir: &Path, timestamp: u128) -> Result<Self, std::io::Error> {
    let file_name = format!("wal_{}.log", timestamp);
    let wal_path = dir.join(file_name);

    let file = OpenOptions::new()
      .create(true)
      .write(true)
      .append(true)
      .open(&wal_path)?;

    let file = BufWriter::new(file);

    Ok(Self {
      path: wal_path,
      file,
    })
  }

  pub fn flush(&mut self) -> Result<(), std::io::Error> {
    self.file.flush()
  }

  pub fn append(&mut self, entry: table_entry::TableEntry) -> Result<(), std::io::Error> {
    let entry_str = format!(
      "{},{:?},{},{},{}\n",
      String::from_utf8_lossy(&entry.key),
      entry.value,
      entry.timestamp,
      entry.deleted,
      entry.value.is_some()
      );

    self.file.write_all(entry_str.as_bytes())?;

    self.flush()
  }
}
