use std::collections::HashMap;
use std::sync::Arc;
use tokio::sync::RwLock;
use uuid::Uuid;
use super::vm_core::{VMState, VM, Transaction};

pub type VMTable = Arc<RwLock<HashMap<Uuid, Arc<VMState>>>>;

pub struct VMServer {
  vm_table: VMTable,
}

impl VMServer {
  pub fn new() -> Self {
    Self {
      vm_table: Arc::new(RwLock::new(HashMap::new())),
    }
  }

  pub fn spawn_vm(&self, transaction: Transaction) -> Uuid {
    let vm_id = Uuid::new_v4();
    let vm = VM::new(vm_id, self.vm_table.clone(), transaction);

    {
      let mut vms_guard = self.vm_table.blocking_write();
      vms_guard.insert(vm_id, vm.state.clone());
    }

    println!("Spawning VM with ID: {}", vm_id);

    tokio::spawn(async move {
      vm.run().await;
    });

    vm_id
  }

}
