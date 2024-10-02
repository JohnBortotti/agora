mod vm;
mod u256;

use vm::VM;

use std::ffi::CStr;
use std::os::raw::c_char;

#[no_mangle]
pub extern "C" fn run_vm(
    sender_ptr: *const c_char,
    receiver_ptr: *const c_char,
    amount: u64,
    gas_limit: u64,
    gas_price: u64,
    payload_ptr: *const c_char,
) -> u64 {
    let sender = unsafe { CStr::from_ptr(sender_ptr).to_str().unwrap().to_string() };
    let receiver = unsafe { CStr::from_ptr(receiver_ptr).to_str().unwrap().to_string() };
    let payload = unsafe { CStr::from_ptr(payload_ptr).to_str().unwrap().to_string() };

    let tx_context = vm::Transaction {
        sender,
        receiver,
        amount: amount.into(),
        gas_limit: gas_limit.into(),
        gas_price: gas_price.into(),
        payload,
    };

    let mut vm = VM::new(tx_context, gas_limit);
    vm.run();

    2

}
