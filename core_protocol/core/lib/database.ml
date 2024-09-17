open Ctypes
open Foreign

type t = unit ptr

let target_path = Sys.getenv "DATABASE_LIB_PATH"

let rust_lib = Dl.dlopen ~filename:target_path ~flags:[Dl.RTLD_NOW]

let database_new = foreign ~from:rust_lib "database_new"
  (string @-> size_t @-> returning (ptr void))

let database_write = foreign ~from:rust_lib "database_write"
  (ptr void @-> string @-> string @-> returning void)

let database_get = foreign ~from:rust_lib "database_get"
  (ptr void @-> string @-> returning (ptr_opt char))

let free = foreign "free" (ptr char @-> returning void)

let create path memtable_size =
  let db_ptr = database_new path memtable_size in
  if is_null db_ptr then
    failwith "Failed to create a new database instance"
  else
    db_ptr

let write db key value =
  database_write db key value

let get db key =
  match database_get db key with
  | Some ptr ->
      let value = string_from_ptr ptr ~length:(String.length key) in
      free ptr;
      Some value
  | None -> None
