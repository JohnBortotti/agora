use uuid::Uuid;
use serde_json::json;
use crate::jsonrpc;
use std::collections::HashMap;
use super::vm_core::{VM, Transaction, VMStateMachine, VMStatus, OverlayedChangeSet};
use super::ocaml_callback::{RealOcamlCallback, OcamlCallback};

pub struct Server {
   root_vm_id: Option<Uuid>,
   vms: HashMap<Uuid, Box<dyn VMStateMachine>>,
   vms_results: HashMap<Uuid, OverlayedChangeSet>,
   // request_id, vm_id
   data_requests: HashMap<Uuid, Uuid>,
   ocaml_callback: Box<dyn OcamlCallback>,
}

impl Server {
  pub fn new() -> Self {
    println!("\n[VM] spawning VM server\n");
    Self {
      root_vm_id: None,
      vms: HashMap::new(),
      vms_results: HashMap::new(),
      data_requests: HashMap::new(),
      ocaml_callback: Box::new(RealOcamlCallback),
    }
  }

  pub fn run(&mut self, transaction: Transaction) -> String {
    let root_vm_id = Uuid::new_v4();
    self.spawn_vm(root_vm_id, transaction);
    self.root_vm_id = Some(root_vm_id);

    loop {
      println!("[VM] polling VMs");

      let last_vm = self.vms.values_mut().last().unwrap();
      let mut vms_to_delete: Vec<Uuid> = Vec::new();

      println!("[VM] last_vm: {} {:?}", last_vm.get_id(), last_vm.poll());

      match last_vm.poll() {
        VMStatus::Initializing => {
          match self.data_requests.get_key_value(&last_vm.get_id()) {
            Some((_request_id, _vm_id)) => {
              // if request_id == &last_vm.get_id() && vm_id == &last_vm.get_id() {}
              // TODO: check this arm, probably nothing to do
            },
            None => {
              self.data_requests.insert(last_vm.get_id(), last_vm.get_id());
              let req = jsonrpc::serialize_request(
                "get_code",
                json!([last_vm.get_contract_address()]),
                );
              self.ocaml_callback.call(&last_vm.get_id().to_string(), &req);
            }
          }
        },
        VMStatus::Running => {},
        VMStatus::Pending { request_id, event: _ } => {
          self.data_requests.insert(last_vm.get_id(), request_id);
          // TODO: request data
        },
        VMStatus::Finished { change_set } => {
          if let Some(root_vm_id) = self.root_vm_id {
            if root_vm_id == last_vm.get_id() {
              match serde_json::to_string(&change_set) {
                Ok(json) => return json,
                Err(e) => {
                  eprintln!("Serialization error: {}", e);
                  return "Serialization error".to_string();
                }
              }
            } else {
              self.vms_results.insert(last_vm.get_id(), change_set);
              vms_to_delete.push(last_vm.get_id());
            }
          }
        },
      }

      for vm_id in vms_to_delete {
        self.vms.remove(&vm_id);
      }

      // after loop a little sleep to let other task run
      println!("[VM] sleeping");
      std::thread::sleep(std::time::Duration::from_millis(200));
    }
  }

  fn spawn_vm(&mut self, id: Uuid, transaction: Transaction) {
    println!("[VM] spawning VM: {}", id);

    let vm = VM::new(id, transaction);
    self.vms.insert(id, Box::new(vm));
  }

  pub fn send_data_to_vm(&mut self, request_id: Uuid, json: String) {
    println!("[send_data_to_vm] Received data, request_id: {}", request_id);
    // get request_id from json
    // let json: serde_json::Value = serde_json::from_str(&json).unwrap();
    // let request_id = json["request_id"].as_str().unwrap();
    // search on table to get vm to deliver data
    match self.data_requests.get(&request_id) {
      Some(vm_id) => {
        self.vms.get_mut(vm_id).unwrap().supply_data(request_id, json);
      },
      None => {
        eprintln!("[send_data_to_vm] request_id not found: {}", request_id);
      }
    }
  }
}
