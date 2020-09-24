use std::collections::hash_map::DefaultHasher;
use std::hash::{Hash, Hasher};

type Node<'a> = Option<Box<Link<'a>>>;

struct Link<'a> {
    key: &'a str,
    val: i32,
    next: Node<'a>
}

const INIT_BUCKET_COUNT: usize = 1024;
pub struct HashTable<'a> {
    numElems: i32,
    numBuckets: usize,
    buckets: Vec<Node<'a>>
}   

impl<'a> Link<'a>{
    pub fn new(key: &'a str, val: i32) -> Self {
        Link{ key, val, next : None }
    }   
}

impl HashTable<'_>{
    pub fn new() -> Self {
        HashTable{ numElems : 0, numBuckets: INIT_BUCKET_COUNT, buckets : Vec::with_capacity(INIT_BUCKET_COUNT) }
    }

    pub fn put(&self, key: &str, val: i32) {
        // This looks... terrible. 
        let hash_val = ( get_hash(key) % (self.numBuckets as u64) ) as usize;
        println!("{}", hash_val);
        let link = match self.buckets.get(hash_val) {
            None => {
                let new_link = Link::new(key,val);
                Box::new(new_link)
            }
            Some(L) => {
                // Loop through and see if exists
                *L
            }
        };
        link.next = self.buckets.get(hash_val);
        self.buckets[hash_val] = link;
    }

    pub fn get(&self, key: &str) -> i32 {
        let hash_val = get_hash(key) % (self.numBuckets as u64);
        return 5;
    }   
}

fn get_hash(key: &str) -> u64 {
    let mut hasher = DefaultHasher::new();
    key.hash(&mut hasher);
    hasher.finish()
}

fn main() {
    let my_map = HashTable::new();
    my_map.put(&"K1", 1)
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
