use serde::{Deserialize, Serialize};
use serde_json;
use uuid::Uuid;

#[derive(Serialize, Deserialize, Debug)]
pub struct JsonRpcRequest {
    pub jsonrpc: String,
    pub method: String,
    pub params: serde_json::Value,
    pub id: String,
}

#[derive(Serialize, Deserialize, Debug)]
pub struct JsonRpcResponse {
    pub jsonrpc: String,
    pub result: Option<serde_json::Value>,
    pub error: Option<serde_json::Value>,
    pub id: String,
}

pub fn serialize_request(method: &str, params: serde_json::Value) -> String {
    let request = JsonRpcRequest {
        jsonrpc: "2.0".to_string(),
        method: method.to_string(),
        params,
        id: Uuid::new_v4().to_string(),
    };
    serde_json::to_string(&request).expect("Failed to serialize JSON-RPC request")
}

pub fn deserialize_response(response: &str) -> Result<JsonRpcResponse, serde_json::Error> {
    serde_json::from_str(response)
}
