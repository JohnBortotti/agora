use std::path::{Path, PathBuf};
use std::fs::{File, OpenOptions};
use std::io::{BufWriter, Write, BufReader};
use std::io::Read;

use super::table_entry;

#[derive(Debug)]
pub struct WAL {
    pub path: PathBuf,
    file: BufWriter<File>
}

impl WAL {
  pub fn new(dir: &Path, timestamp: u128) -> Result<Self, std::io::Error> {
    let file_name = format!("{}.wal", timestamp);
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

  pub fn from_path(path: &Path) -> Result<Self, std::io::Error> {
      let options = OpenOptions::new().append(true).write(true).open(&path)?;
      let file = BufWriter::new(options);

      return Ok(WAL { path: path.to_owned(), file })
  }

  pub fn flush(&mut self) -> Result<(), std::io::Error> {
      self.file.flush()
  }

  pub fn append(&mut self, entry: table_entry::TableEntry) -> Result<(), std::io::Error> {
      let (value_len, value) = match entry.value {
          None => (vec![0].len(), vec![0]),
          Some(val) => (val.len(), val)
      };

      self.file.write_all(&(entry.deleted as u8).to_le_bytes())?;
      self.file.write_all(&entry.key.len().to_le_bytes())?;
      self.file.write_all(&value_len.to_le_bytes())?;
      self.file.write_all(&(entry.key))?;
      self.file.write_all(&value)?;
      self.file.write_all(&entry.timestamp.to_le_bytes())?;

      self.flush()?;
      Ok(())
  }
}

#[derive(Debug)]
pub struct WALIterator {
    reader: BufReader<File>
}

impl WALIterator {
    pub fn new(path: &Path) -> Result<Self, std::io::Error> {
        let options = OpenOptions::new().read(true).open(&path)?;
        let reader = BufReader::new(options);

        Ok(Self { reader })
    }
}

impl Iterator for WALIterator {
    type Item = table_entry::TableEntry;

    fn next(&mut self) -> Option<Self::Item> {
        let mut deleted_buf = [0; 1];
        if self.reader.read_exact(&mut deleted_buf).is_err() {
            return None
        };
        let deleted = deleted_buf[0] > 0;

        let mut key_len_buf = [0; 8];
        if self.reader.read_exact(&mut key_len_buf).is_err() {
            return None
        };
        let key_len = usize::from_le_bytes(key_len_buf);

        let mut value_len_buf = [0; 8];
        if self.reader.read_exact(&mut value_len_buf).is_err() {
            return None
        };
        let value_len = usize::from_le_bytes(value_len_buf);

        let mut key_buf = vec![0; key_len];
        if self.reader.read_exact(&mut key_buf).is_err() {
            return None
        };

        let mut value_buf = vec![0; value_len];
        if self.reader.read_exact(&mut value_buf).is_err() {
            return None
        };
        let value = if deleted { None } else { Some(value_buf) };

        let mut timestamp_buf = [0; 8];
        if self.reader.read_exact(&mut timestamp_buf).is_err() {
            return None
        };
        let timestamp = i64::from_le_bytes(timestamp_buf);

        Some(self::table_entry::TableEntry {
            deleted,
            key: key_buf,
            value,
            timestamp

        })
    }
}
