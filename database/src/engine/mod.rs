use std::path::PathBuf;
use std::fs::File;
use std::io::BufWriter;

mod memtable;
mod table_entry;
mod wal;
mod sstable;

#[derive(Debug)]
pub struct Database {
  path: PathBuf,
  memtable: memtable::Memtable,
  memtable_size: usize,
  wal: wal::WAL,
  tables: Vec<sstable::SSTable>,
}

impl Database {
  pub fn new(path: PathBuf, memtable_size: usize) -> Result<Self, std::io::Error> {
    let wal = wal::WAL::new(&path, chrono::Utc::now().timestamp_nanos_opt().unwrap() as u128)?;
    Ok(Self {
      path,
      memtable: memtable::Memtable::new(),
      memtable_size,
      wal,
      tables: Vec::new(),
    })
  }

  pub fn load(path: PathBuf) -> Result<Self, std::io::Error> {
    let wal = wal::WAL::new(&path, chrono::Utc::now().timestamp_nanos_opt().unwrap() as u128)?;
    Ok(Self {
      path,
      memtable: memtable::Memtable::new(),
      memtable_size: 0,
      wal,
      tables: Vec::new(),
    })
  }

  pub fn write(&mut self, key: &[u8], data: table_entry::TableEntry) -> Result<(), std::io::Error> {
    self.wal.append(data.clone())?;
    self.memtable.insert(key, data);
    if self.memtable.size > self.memtable_size {
      self.flush_memtable_to_sstable()?;
    }
    Ok(())
  }

  pub fn get(&self, key: &[u8]) -> Result<Option<table_entry::TableEntry>, std::io::Error> {
    if let Some(entry) = self.memtable.search(key) {
      return Ok(Some(entry.clone()));
    }

    for table in &self.tables {
      if let Some(entry) = table.search(&self.path, key)? {
        return Ok(Some(entry));
      }
    }

    Ok(None)
  }

  fn flush_memtable_to_sstable(&mut self) -> Result<(), std::io::Error> {
    let timestamp = chrono::Utc::now().timestamp_nanos_opt().unwrap() as u128;

    let mut sstable = sstable::SSTable::new(&self.path, timestamp)?;
    sstable.flush(&self.memtable)?;
    self.tables.push(sstable);

    self.memtable = memtable::Memtable::new();

    Ok(())
  }
}

