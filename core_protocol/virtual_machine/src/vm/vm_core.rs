use std::sync::Arc;
use tokio::sync::Notify;
use uuid::Uuid;
use super::server::VMTable;

#[derive(Debug, Clone)]
pub struct Transaction {
  pub hash: String,
  pub sender: String,
  pub receiver: String,
  pub amount: i32,
  pub gas_limit: i32,
  pub gas_price: i32,
  pub nonce: i32,
  pub payload: String,
  pub signature: String,
}

#[derive(Debug, Clone)]
pub enum VMStatus {
  Running,
  WaitingForData,
  Completed,
  Error(String),
}

pub struct VMState {
  pub status: VMStatus,
  pub parent_vm: Option<Uuid>,
  pub notify: Arc<Notify>,
  pub data: Option<String>,
  pub requested_key: Option<String>,
}

pub struct VM {
  pub id: Uuid,
  pub state: Arc<VMState>,
  pub vm_table: VMTable,
  pub transaction: Transaction,
}

impl VM {
  pub fn new(id: Uuid,vm_table: VMTable, transaction: Transaction) -> Self {
    let state = VMState {
      status: VMStatus::Running,
      parent_vm: None,
      notify: Arc::new(Notify::new()),
      data: None,
      requested_key: None,
    };

    VM {
      id,
      state: Arc::new(state),
      vm_table,
      transaction
    }
  }

  pub async fn run(&self) {
    println!("VM {} received transaction: {:?}", self.id, self.transaction);
  }

  async fn request_data(&self, _key: String) {}
}
