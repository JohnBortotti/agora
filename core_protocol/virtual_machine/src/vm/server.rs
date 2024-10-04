use std::collections::HashMap;
use std::sync::{Arc, Mutex};
use uuid::Uuid;
use super::vm_core::{VMState, VM, Transaction, VMStatus};

pub type VMTable = Arc<Mutex<HashMap<Uuid, Arc<Mutex<VMState>>>>>;

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

  pub fn spawn_vm(&self, transaction: Transaction) -> Uuid {
    let vm_id = Uuid::new_v4();
    let vm = VM::new(vm_id, self.vm_table.clone(), transaction);

    let state = VMState {
        status: VMStatus::Running,
        parent_vm: None,
        data: Vec::new(),
        requested_key: None,
    };

    {
      let mut vms_guard = self.vm_table.lock().unwrap();
      vms_guard.insert(vm_id, Arc::new(Mutex::new(state)));
    }

    println!("\n[VM] spawning VM with ID: {}\n", vm_id);

    std::thread::spawn(move ||{
      vm.run()
    });

    vm_id
  }

  pub fn send_data_to_vm(&self, vm_id: Uuid, key: String, data: String) {
    println!("[send_data_to_vm] Received data for VM: {}", vm_id);

    let mut vms_guard = self.vm_table.lock().unwrap();

    if let Some(vm_state) = vms_guard.get_mut(&vm_id) {
      let mut vm_state_guard = vm_state.lock().unwrap();

      let _ = vm_state_guard.data.push((key, data));
      vm_state_guard.status = VMStatus::Running;
    }
  }

}
