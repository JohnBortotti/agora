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
    let data_file = BufWriter::new(File::create(path.join(format!("sstable_{}.data", timestamp)))?);
    let index_file = BufWriter::new(File::create(path.join(format!("sstable_{}.index", timestamp)))?);
    let metadata_file = BufWriter::new(File::create(path.join(format!("sstable_{}.meta", timestamp)))?);

    Ok(Self {
      data_file,
      index_file,
      metadata_file,
      timestamp,
    })
  }

  pub fn flush(&mut self, memtable: &Memtable) -> Result<(), std::io::Error> {
    for (key, entry) in &memtable.entries {
      let key_bytes = key.as_bytes();
      let key_len = key_bytes.len() as u64;
      let value_bytes = entry.value.as_ref().map_or(vec![], |v| v.clone());
      let value_len = value_bytes.len() as u64;

      self.data_file.write_all(&key_len.to_le_bytes())?;
      self.data_file.write_all(key_bytes)?;
      self.data_file.write_all(&value_len.to_le_bytes())?;
      self.data_file.write_all(&value_bytes)?;
      self.data_file.write_all(&entry.timestamp.to_le_bytes())?;
      self.data_file.write_all(&[if entry.deleted { 1 } else { 0 }])?;

      let offset = self.data_file.stream_position()?;

      self.index_file.write_all(&key_len.to_le_bytes())?;
      self.index_file.write_all(key_bytes)?;
      self.index_file.write_all(&offset.to_le_bytes())?;
    }

    self.data_file.flush()?;
    self.index_file.flush()?;
    self.metadata_file.flush()?;

    Ok(())
  }

  pub fn search(&self, path: &Path, key: &[u8]) -> Result<Option<TableEntry>, std::io::Error> {
    let index_file_path = path.join(format!("sstable_{}.index", self.timestamp));
    let mut index_file = BufReader::new(OpenOptions::new().read(true).open(index_file_path)?);

    let mut key_len_buf = [0u8; 8];
    let mut offset_buf = [0u8; 8];

    while index_file.read_exact(&mut key_len_buf).is_ok() {
      let key_len = u64::from_le_bytes(key_len_buf);
      let mut current_key = vec![0u8; key_len as usize];
      index_file.read_exact(&mut current_key)?;

      if current_key == key {
        index_file.read_exact(&mut offset_buf)?;
        let offset = u64::from_le_bytes(offset_buf);

        let data_file_path = path.join(format!("sstable_{}.db", self.timestamp));
        let mut data_file = BufReader::new(OpenOptions::new().read(true).open(data_file_path)?);

        data_file.seek(SeekFrom::Start(offset))?;

        let table_entry = self.read_table_entry(&mut data_file)?;
        return Ok(Some(table_entry));
      } else {
        index_file.seek(SeekFrom::Current(8))?;
      }
    }

    Ok(None)
  }

  pub fn get_value_at_offset(&self, path: &Path, offset: u64) -> Result<Option<TableEntry>, std::io::Error> {
    let data_file_path = path.join(format!("sstable_{}.data", self.timestamp));
    let mut data_file = BufReader::new(OpenOptions::new().read(true).open(data_file_path)?);
    data_file.seek(SeekFrom::Start(offset))?;

    let mut key_len_buf = [0u8; 8];
    data_file.read_exact(&mut key_len_buf)?;
    let key_len = u64::from_le_bytes(key_len_buf);
    let mut key = vec![0u8; key_len as usize];
    data_file.read_exact(&mut key)?;

    let mut value_len_buf = [0u8; 8];
    data_file.read_exact(&mut value_len_buf)?;
    let value_len = u64::from_le_bytes(value_len_buf);
    let mut value = vec![0u8; value_len as usize];
    data_file.read_exact(&mut value)?;

    let mut timestamp_buf = [0u8; 8];
    data_file.read_exact(&mut timestamp_buf)?;
    let timestamp = i64::from_le_bytes(timestamp_buf);

    let mut deleted_flag = [0u8; 1];
    data_file.read_exact(&mut deleted_flag)?;
    let deleted = deleted_flag[0] == 1;

    Ok(Some(TableEntry {
      key,
      value: if value_len == 0 { None } else { Some(value) },
      timestamp,
      deleted,
    }))
  }

  fn read_table_entry<R: Read>(&self, reader: &mut R) -> Result<TableEntry, std::io::Error> {
    let mut key_len_buf = [0u8; 8];
    reader.read_exact(&mut key_len_buf)?;
    let key_len = u64::from_le_bytes(key_len_buf);

    let mut key = vec![0u8; key_len as usize];
    reader.read_exact(&mut key)?;

    let mut value_len_buf = [0u8; 8];
    reader.read_exact(&mut value_len_buf)?;
    let value_len = u64::from_le_bytes(value_len_buf);

    let mut value = vec![0u8; value_len as usize];
    reader.read_exact(&mut value)?;

    let mut timestamp_buf = [0u8; 8];
    reader.read_exact(&mut timestamp_buf)?;
    let timestamp = i64::from_le_bytes(timestamp_buf);

    let mut deleted_buf = [0u8; 1];
    reader.read_exact(&mut deleted_buf)?;
    let deleted = deleted_buf[0] != 0;

    Ok(TableEntry {
      key,
      value: Some(value),
      timestamp,
      deleted,
    })
  }
}
