use std::collections::HashMap;
use std::sync::{Arc, RwLock};
use uuid::Uuid;
use super::ocaml_callback::RealOcamlCallback;
use super::vm_core::{VM, Transaction};
use std::sync::mpsc::{Sender, channel};

pub type VMTable = Arc<RwLock<HashMap<Uuid, Sender<String>>>>;

pub struct VMServer {
  vm_table: VMTable,
}

impl VMServer {
  pub fn new() -> Self {
    println!("\n[VM] spawning VM server\n");
    Self {
      vm_table: Arc::new(HashMap::new().into()),
    }
  }

  pub fn spawn_vm(&self, transaction: Transaction) -> String {
    let vm_id = Uuid::new_v4();
    let (tx, rx) = channel();
    let mut vm = VM::new(vm_id, rx, transaction, Box::new(RealOcamlCallback));

    {
      let mut vms = self.vm_table.write().unwrap();
      vms.insert(vm_id, tx);
    }

    println!("\n[VM] spawning VM with ID: {}\n", vm_id);

    let result = vm.run();

    match serde_json::to_string(&result) {
      Ok(json) => json,
      Err(e) => {
        eprintln!("Serialization error: {}", e);
        return "Serialization error".to_string();
      }
    }
  }

  pub fn send_data_to_vm(&self, vm_id: Uuid, json: String) {
    println!("[send_data_to_vm] Received data for VM: {}", vm_id);
    if let Ok(vms) = self.vm_table.read() {
      if let Some(tx) = vms.get(&vm_id) {
        if tx.send(json.clone()).is_ok() {
          println!("[send_data_to_vm] Data sent to VM: {}", vm_id);
        } else {
          println!("[send_data_to_vm] Failed to send data to VM: {}", vm_id);
        }
      } else {
        println!("[send_data_to_vm] VM not found for ID: {}", vm_id);
      }
    } else {
      eprintln!("[send_data_to_vm] Failed to acquire read lock");
    }
  }
}
