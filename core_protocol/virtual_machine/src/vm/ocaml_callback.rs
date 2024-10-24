use ocaml_sys::{caml_callback2, caml_copy_string, caml_named_value, Value};
use std::ffi::CString;

pub trait OcamlCallback {
  fn call(&self, vm_id: &str, json: &str);
}

pub struct RealOcamlCallback;

impl OcamlCallback for RealOcamlCallback {
  fn call(&self, vm_id: &str, json: &str) {
    let vm_id_cstring = CString::new(vm_id).expect("Failed to create CString for vm_id");
    let json_cstring = CString::new(json).expect("Failed to create CString for method");

    unsafe {
      let vm_id_value: Value = caml_copy_string(vm_id_cstring.as_ptr());
      let json_value: Value = caml_copy_string(json_cstring.as_ptr());

      let ocaml_callback_str = CString::new("ocaml_json_rpc_callback").unwrap();
      let ocaml_callback_fn: *const Value = caml_named_value(ocaml_callback_str.as_ptr());

      if !ocaml_callback_fn.is_null() {
        caml_callback2(*ocaml_callback_fn, vm_id_value, json_value);
      } else {
        eprintln!("OCaml callback not found");
      }
    }
  }
}

pub struct MockOcamlCallback;

impl OcamlCallback for MockOcamlCallback {
  fn call(&self, vm_id: &str, json: &str) {
    println!("[MockOcamlCallback] vm_id: {}, json: {}", vm_id, json);
  }
}
