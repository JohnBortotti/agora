use std::collections::HashMap;
use std::sync::{Arc, RwLock};
use uuid::Uuid;
use super::ocaml_callback::RealOcamlCallback;
use super::vm_core::{VM, Transaction, VMStateMachine, VMStatus, VMEvent, OverlayedChangeSet};
use std::sync::mpsc::{Sender, channel};

pub type VMTable = Arc<RwLock<HashMap<Uuid, Sender<String>>>>;

pub struct Server {
  vm_table: VMTable,
  vms: Vec<Box<dyn VMStateMachine>>,
}

impl Server {
  pub fn new() -> Self {
    println!("\n[VM] spawning VM server\n");
    Self {
      vm_table: Arc::new(HashMap::new().into()),
      vms: Vec::new(),
    }
  }

  pub fn run(&mut self, transaction: Transaction) -> String {
    self.spawn_vm(transaction);

    loop {
      let mut all_finished = true;
      let mut events_to_handle = Vec::new();

      for vm in self.vms.iter_mut() {
        let (status, event) = vm.poll();
        match status {
          VMStatus::Running | VMStatus::Pending { .. } => all_finished = false,
          _ => (),
        }

        if let Some(event) = event {
          events_to_handle.push(event);
        }
      }

      for event in events_to_handle {
        self.handle_event(event);
      }

      if all_finished {
        let (status, _) = self.vms[0].poll();
        match status {
          VMStatus::Finished { change_set } => {
            match serde_json::to_string(&change_set) {
              Ok(json) => return json,
              Err(e) => {
                eprintln!("Serialization error: {}", e);
                return "Serialization error".to_string();
              }
            }
          },
          _ => panic!("VM finished but no change set found"),
        }
      }
    }
  }

  fn handle_event(&mut self, event: VMEvent) {
    match event {
      VMEvent::SpawnVM(transaction) => {
        self.spawn_vm(transaction);
      },
      VMEvent::RequestData(vm_id, data) => {
        self.send_data_to_vm(vm_id, data);
      },
    }
  }

  pub fn spawn_vm(&mut self, transaction: Transaction) {
    let vm_id = Uuid::new_v4();
    let (tx, rx) = channel();
    let vm = VM::new(vm_id, rx, transaction, Box::new(RealOcamlCallback));

    {
      let mut vms = self.vm_table.write().unwrap();
      vms.insert(vm_id, tx);
    }

    self.vms.push(Box::new(vm));
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
