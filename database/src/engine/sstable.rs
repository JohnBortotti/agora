use std::fs::{File, OpenOptions};
use std::io::{BufWriter, Write, BufReader, Read, Seek, SeekFrom};
use std::path::Path;
use super::memtable::Memtable;
use super::table_entry::TableEntry;

#[derive(Debug)]
pub struct SSTable {
    data_file: BufWriter<File>,
    index_file: BufWriter<File>,
    metadata_file: BufWriter<File>,
    pub timestamp: u128
}

impl SSTable {
  pub fn new(path: &Path, timestamp: u128) -> Result<Self, std::io::Error> {
    let data_file = BufWriter::new(File::create(path.join(format!("{}.sst_data", timestamp)))?);
    let index_file = BufWriter::new(File::create(path.join(format!("{}.sst_index", timestamp)))?);
    let metadata_file = BufWriter::new(File::create(path.join(format!("{}.sst_meta", timestamp)))?);

    Ok(Self {
      data_file,
      index_file,
      metadata_file,
      timestamp,
    })
  }

  pub fn flush(&mut self, memtable: &Memtable) -> Result<(), std::io::Error> {
    let mut offset: u64 = 0;

    for (key, entry) in &memtable.entries {
      let key_len = key.len() as u64;
      let value_len = entry.value.as_ref().map_or(0, |v| v.len()) as u64;
      let entry_size = 1 + 8 + 8 + key_len + value_len + 8;

      self.data_file.write_all(&(entry.deleted as u8).to_le_bytes())?;
      self.data_file.write_all(&key_len.to_le_bytes())?;
      self.data_file.write_all(&value_len.to_le_bytes())?;
      self.data_file.write_all(&entry.key)?;
      if let Some(value) = &entry.value {
        self.data_file.write_all(value)?;
      }
      self.data_file.write_all(&entry.timestamp.to_le_bytes())?;

      self.index_file.write_all(&key_len.to_le_bytes())?;
      self.index_file.write_all(&entry.key)?;
      self.index_file.write_all(&offset.to_le_bytes())?;

      offset += entry_size;
    }

    self.data_file.flush()?;
    self.index_file.flush()?;
    self.metadata_file.flush()?;

    Ok(())
  }

  pub fn search(&self, path: &Path, key: &[u8]) -> Result<Option<u64>, std::io::Error> {
    let index_file_path = path.join(format!("{}.sst_index", self.timestamp));
    let mut index_file = BufReader::new(OpenOptions::new().read(true).open(index_file_path)?);

    let mut key_len_buf = [0; 8];
    let mut offset_buf = [0; 8];

    while index_file.read_exact(&mut key_len_buf).is_ok() {
      let key_len = u64::from_le_bytes(key_len_buf);
      let mut on_disk_key = vec![0u8; key_len as usize];
      index_file.read_exact(&mut on_disk_key)?;

      if on_disk_key == key {
        index_file.read_exact(&mut offset_buf)?;
        let offset = u64::from_le_bytes(offset_buf);
        return Ok(Some(offset));
      } else {
        index_file.seek(SeekFrom::Current(8))?;
      }
    }

    Ok(None)
  }

  pub fn get_value_at_offset(&self, path: &Path, offset: u64) -> Result<Option<TableEntry>, std::io::Error> {
    let data_file_path = path.join(format!("{}.sst_data", self.timestamp));
    let mut data_file = BufReader::new(OpenOptions::new().read(true).open(data_file_path)?);
    data_file.seek(SeekFrom::Start(offset))?;

    data_file.seek(SeekFrom::Start(offset))?;
    let mut tombstone = [0u8; 1];
    data_file.read_exact(&mut tombstone)?;

    if tombstone[0] == 1 {
      return Ok(None);
    }

    let mut buffer = [0u8; 8];

    data_file.read_exact(&mut buffer)?;
    let _key_len = u64::from_le_bytes(buffer);

    data_file.read_exact(&mut buffer)?;
    let value_len = u64::from_le_bytes(buffer);

    let mut key_buffer = vec![0u8; _key_len as usize];
    data_file.read_exact(&mut key_buffer)?;

    let mut value_buffer = vec![0u8; value_len as usize];
    data_file.read_exact(&mut value_buffer)?;

    let mut timestamp_buffer = [0; 8];
    data_file.read_exact(&mut timestamp_buffer)?;
    let timestamp = i64::from_le_bytes(timestamp_buffer);

    let entry = TableEntry {
      key: key_buffer,
      value: Some(value_buffer),
      timestamp,
      deleted: u8::from_be_bytes(tombstone) > 0
    };

    Ok(Some(entry))
  }

}
