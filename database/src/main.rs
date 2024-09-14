use std::path::PathBuf;

mod engine;

// - [ ] FFI
// - [ ] compression
// - [ ] maybe a bulk insert? useful for inserting the entire trie

fn main() {
    println!("Hello, world!");

    let mut path = PathBuf::new();
    path.push("test");
    let database = engine::Database::new(path, 3).unwrap();

    dbg!(database);
}
