use uuid::Uuid;
use serde_json::json;
use crate::jsonrpc;
use std::collections::HashMap;
use super::vm_core::{VM, VMEvent, Transaction, VMStateMachine, VMStatus, OverlayedChangeSet};
use super::ocaml_callback::{RealOcamlCallback, OcamlCallback};

pub struct Server {
   root_vm_id: Option<Uuid>,
   vms: HashMap<Uuid, Box<dyn VMStateMachine>>,
   ordered_vms: Vec<Uuid>,
   vms_results: HashMap<Uuid, OverlayedChangeSet>,
   // request_id, vm_id
   data_requests: HashMap<Uuid, Uuid>,
   ocaml_callback: Box<dyn OcamlCallback>,
}

impl Server {
  pub fn new() -> Self {
    println!("[VM] spawning VM server");
    Self {
      root_vm_id: None,
      vms: HashMap::new(),
      ordered_vms: Vec::new(),
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
      let last_vm_id = self.ordered_vms.last().unwrap();
      let last_vm = self.vms.get_mut(last_vm_id).unwrap();
           
      let mut vms_to_delete: Vec<Uuid> = Vec::new();

      // println!("[VM] polling vm: {}", last_vm.get_id());
      // println!("[VM] polling vm: {} - state {:?}", last_vm.get_id(), last_vm.poll());

      match last_vm.poll() {
        VMStatus::Initializing { request_id } => {
          match self.data_requests.get_key_value(&request_id) {
            Some((_request_id, _vm_id)) => {
              println!("[VM] contract data already requested");
              // TODO: check this arm
            },
            None => {
              self.data_requests.insert(request_id, last_vm.get_id());
              let req = jsonrpc::serialize_request(
                "get_code",
                request_id,
                json!([last_vm.get_contract_address()]),
                );
              self.ocaml_callback.call(&request_id.to_string(), &req);
            }
          }
        },
        VMStatus::Running | VMStatus::ReceivedData { .. } => {},
        VMStatus::Pending { request_id, event } => {
          match event {
            VMEvent::SpawnVM(tx) => {
              match self.vms.get(&request_id) {
                Some(_) => {
                  // println!("[VM] vm is executing, data not available to send yet");
                },
                None => {
                  // println!("[VM] vm is not running, looking on vms_results");
                  match self.vms_results.get(&request_id) {
                    Some(change_set) => {
                      // println!("[VM] vm found on vms_results, sending data");
                      let serialized_change_set = serde_json::to_string(&change_set).unwrap();

                      let last_vm_id = self.ordered_vms.last().unwrap();
                      let last_vm = self.vms.get_mut(last_vm_id).unwrap();

                      last_vm.supply_data(request_id, serialized_change_set);
                    },
                    None => {
                      // println!("[VM] vm not found on vms_results, spawning vm");
                      self.spawn_vm(request_id, tx);
                    }
                  }
                }
              }
            }
            VMEvent::RequestData(json_req) => {
              match self.data_requests.get(&request_id) {
                Some(..) => {
                  // println!("[VM] data already requested, waiting for response");
                },
                None => {
                  self.data_requests.insert(request_id, last_vm.get_id());
                  let serialized = jsonrpc::serialize_request(
                    json_req.method.as_str(),
                    request_id,
                    json_req.params,
                  );
                  self.ocaml_callback.call(&request_id.to_string(), &serialized);
                }
              }
                          
            }
          }
        },
        VMStatus::Finished { change_set } => {
            if self.root_vm_id.unwrap() == last_vm.get_id() {
              match serde_json::to_string(&change_set) {
                Ok(json) => return json,
                Err(e) => {
                  eprintln!("Serialization error: {}", e);
                  return "Serialization error".to_string();
                }
              }
            } else {
              // println!("[VM] VM finished, inserting vm_id: {}", last_vm.get_id());
              self.vms_results.insert(last_vm.get_id(), change_set);
              vms_to_delete.push(last_vm.get_id());
            }
        },
      }

      for vm_id in vms_to_delete {
        self.vms.remove(&vm_id);
        self.ordered_vms.retain(|&x| x != vm_id);
      }
      
      std::thread::sleep(std::time::Duration::from_millis(100));
    }
  }

  fn spawn_vm(&mut self, id: Uuid, transaction: Transaction) {
    println!("[VM] spawning VM: {}", id);

    let vm = VM::new(id, transaction);
    self.vms.insert(id, Box::new(vm));
    self.ordered_vms.push(id);
  }

  pub fn send_data_to_vm(&mut self, request_id: Uuid, json: String) {
    println!("[VM] received data, request_id: {}", request_id);
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
