use std::collections::HashMap;
use std::sync::Arc;
use tokio::sync::{RwLock, Notify};
use tokio::task;
use uuid::Uuid;
use std::time::Duration;

#[derive(Debug, Clone)]
enum VMStatus {
  Running,
  WaitingForData,
  Completed,
  Error(String),
}

struct VMState {
  status: VMStatus,
  parent_vm: Option<Uuid>,
  // notify: Arc<Notify>,
  // data: Option<String>,
}

type VMTable = Arc<RwLock<HashMap<Uuid, VMState>>>;

struct VMServer {
  vms: VMTable,
}
