use std::path::PathBuf;
use std::fs::read_dir;
use std::path::Path;

mod memtable;
pub mod table_entry;
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

  pub fn try_load(path: PathBuf, memtable_size: usize) -> Result<Self, std::io::Error> {
    let mut memtable = memtable::Memtable::new();
    let wal = match Self::search_for_wal_file(&path) {
      Ok(wal) => { 
        let wal_iterator = wal::WALIterator::new(&wal.path)?.into_iter();
        for entry in wal_iterator {
          if entry.deleted == true {
            memtable.delete(&entry.key, entry.timestamp);
          } else {
            memtable.insert(entry);
          }
        }
        wal 
      },
      Err(..) => {
        wal::WAL::new(&path, chrono::Utc::now().timestamp_nanos_opt().unwrap() as u128)?
      }
    };

    let tables = Self::search_for_table_files(&path)?;

    Ok(Self { path, memtable, memtable_size, wal, tables })
  }

  pub fn write(&mut self, key: &str, value: &str) -> Result<(), std::io::Error> {
    let entry = table_entry::TableEntry {
      key: key.as_bytes().to_vec(),
      value: Some(value.into()),
      timestamp: chrono::Utc::now().timestamp_nanos_opt().unwrap(),
      deleted: false
    };

    self.wal.append(entry.clone())?;
    self.memtable.insert(entry);

    if self.memtable.size > self.memtable_size {
      self.flush_memtable_to_sstable()?;
    }
    Ok(())
  }

  pub fn delete(&mut self, key: &str) -> Result<(), std::io::Error> {
    let key_bytes = key.as_bytes();
    let timestamp = chrono::Utc::now().timestamp_nanos_opt().unwrap();
    let _ = self.memtable.delete(key_bytes, timestamp);
    self.wal.append(table_entry::TableEntry {
      key: key_bytes.to_vec(),
      value: None,
      timestamp,
      deleted: true
    })?;
    Ok(())
  }

  pub fn get(&self, key: &str) -> Result<Option<(String, String)>, std::io::Error> {
    let key_bytes = key.as_bytes();

    if let Some(entry) = self.memtable.search(key_bytes) {
      if !entry.deleted {
        let key_str = String::from_utf8(entry.key.clone()).unwrap();
        let val_str = String::from_utf8(entry.value.clone().unwrap_or(Vec::new())).unwrap();
        return Ok(Some((key_str, val_str)));
      }
      return Ok(None);
    }

    for table in self.tables.iter().rev() {
      if let Some(res) = table.search(&self.path, key_bytes)? {
        if let Some(entry) = table.get_value_at_offset(&self.path, res)? {
          if !entry.deleted {
            let key_str = String::from_utf8(entry.key).unwrap();
            let val_str = String::from_utf8(entry.value.unwrap_or(Vec::new())).unwrap();
            return Ok(Some((key_str, val_str)));
          }
          return Ok(None);
        }
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

  fn search_for_wal_file(path: &Path) -> Result<wal::WAL, std::io::Error> {
    if path.is_dir() {
      for entry in read_dir(path).unwrap() {
        let entry = entry.unwrap();
        let path = entry.path();
        if path.is_file() {
          if let Some(ext) = path.extension() {
            if ext == "wal" {
              let wal = wal::WAL::from_path(&path.to_owned())?;
              return Ok(wal)
            }
          }
        }
      }
    } 

    Err(std::io::Error::new(std::io::ErrorKind::NotFound, "WAL recovery file not found"))
  }

  fn get_timestamp_from_filename(file: &Path) -> Option<u128> {
    file.file_stem()?.to_str()?.parse().ok()
  }

  fn search_for_table_files(path: &Path) -> Result<Vec<sstable::SSTable>, std::io::Error> {
    let mut sstables: Vec<sstable::SSTable> = Vec::new();
    if path.is_dir() {
      for entry in read_dir(path).unwrap() {
        let entry = entry.unwrap();
        let path = entry.path();
        if path.is_file() {
          if let Some(ext) = path.extension() {
            if ext == "sst_index" {
              let parent_dir = path.parent().unwrap();
              let index_file = path.clone();
              let metadata_file = 
                parent_dir.join(path.file_stem().unwrap())
                .with_extension("sst_meta");
              let data_file = 
                parent_dir.join(path.file_stem().unwrap())
                .with_extension("sst_data");

              if metadata_file.exists() && data_file.exists() {
                if let Some(timestamp) = 
                  Self::get_timestamp_from_filename(&index_file) {
                    sstables.push(sstable::SSTable::new(
                        &path.parent().unwrap(),
                        timestamp)?
                                 );
                  }
              }
            }
          }
        }
      }


      sstables.sort_by_key(|table| table.timestamp);
    }

    Ok(sstables)
  }

}
