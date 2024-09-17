use std::path::PathBuf;
use std::ffi::{CString, CStr};
use std::os::raw::c_char;

mod engine;

// - [ ] table compression
// - [ ] handle bytes or string?

#[no_mangle]
pub extern "C" fn database_new(path: *const c_char, memtable_size: usize) -> *mut engine::Database {
  let c_str = unsafe { CStr::from_ptr(path) };
  let path = PathBuf::from(c_str.to_str().unwrap());

  Box::into_raw(Box::new(engine::Database::new(path, memtable_size).unwrap()))
}

#[no_mangle]
pub extern "C" fn database_write(db: *mut engine::Database, key: *const c_char, value: *const c_char) {
  let db = unsafe { &mut *db };
  let key = unsafe { CStr::from_ptr(key) }.to_str();
  let key = match key {
    Ok(v) => v,
    Err(e) => {
      eprintln!("Invalid UTF-8 key sequence encountered: {:?}", e);
      return; 
    }
  };

  let value = unsafe { CStr::from_ptr(value) }.to_str();
  let value = match value {
    Ok(v) => v,
    Err(e) => {
      eprintln!("Invalid UTF-8 value sequence encountered: {:?}", e);
      return; 
    }
  };

  println!("inserting key: {key}");

  db.write(
    key,
    value
    ).unwrap();

}

#[no_mangle]
pub extern "C" fn database_get(db: *mut engine::Database, key: *const c_char) -> *mut c_char {
  let db = unsafe { &mut *db };
  let key = unsafe { CStr::from_ptr(key) }.to_str().unwrap();

  println!("searching key: {key}");

  if let Some((_, value)) = db.get(key).unwrap() {
    CString::new(value).unwrap().into_raw()
  } else {
    std::ptr::null_mut()
  }
}
