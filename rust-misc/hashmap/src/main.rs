use std::collections::hash_map::DefaultHasher;
use std::hash::{Hash, Hasher};

type Node = Option<Box<Link>>;

struct Link {
    key: String,
    val: i32,
    next: Node
}

const InitBucketCount: usize = 1024;
pub struct HashTable {
    numElems: i32,
    numBuckets: usize,
    buckets: Vec<Link>
}   

impl HashTable{
    pub fn new() -> Self {
        HashTable{ numElems : 0, numBuckets: InitBucketCount, buckets : Vec::with_capacity(InitBucketCount) }
    }

    pub fn put(&self, key: String, val: i32) {
        let hash_val = get_hash(key) % self.numBuckets;

    }
}

fn get_hash(key: String) -> u64 {
    let mut hasher = DefaultHasher::new();
    key.hash(&mut hasher);
    hasher.finish()
}

fn main() {
    let myMap = HashTable::new();
}

#[cfg(test)]
mod tests {
    use super::HashTable;
    use crate::get_hash;

    #[test]
    fn hash_fn() {
        assert_eq!(7982898449168957443, get_hash("Hello World".to_string()))
    }

    #[test]
    fn compiles() {
        assert_eq!(true, true)
    }
}
