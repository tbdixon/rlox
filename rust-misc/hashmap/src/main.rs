use std::collections::hash_map::DefaultHasher;
use std::hash::{Hash, Hasher};

type Node = Option<Box<Link>>;

struct Link {
    key: String,
    val: i32,
    next: Node // None or Box<Link>
}

const INIT_BUCKET_COUNT: usize = 1024;
pub struct HashTable {
    num_elems: i32,
    num_buckets: usize,
    buckets: Vec<Node>
}   

impl Link{
    pub fn new(key: &str, val: i32) -> Self {
        Link{ key: String::from(key), val, next : None }
    }
}

impl HashTable{
    pub fn new() -> Self {
        let mut buckets = Vec::with_capacity(INIT_BUCKET_COUNT);
        for _ in 0..INIT_BUCKET_COUNT {
            buckets.push(None);
        }
        HashTable{ num_elems : 0, num_buckets: INIT_BUCKET_COUNT, buckets }
    }

    pub fn put(&mut self, key: &str, val: i32) {
        let hash_val = get_hash_val(key, self.num_buckets);
        let mut new_link = Link::new(key,val);
        if let Some(n) = self.buckets.get_mut(hash_val) {
            //N is &mut Node
            if let Some(l) = n.take() {
                //L is Box<Link>
                new_link.next = Some(l);
            }
        }
        self.buckets[hash_val] = Some(Box::new(new_link));
        self.num_elems += 1;
    }

    pub fn get(&self, key: &str) -> Option<i32> {
        let hash_val = get_hash_val(key, self.num_buckets);
        match self.buckets.get(hash_val) {
            None => None,
            Some(node) => {
                traverse_bucket(node, key)
           }
        }
    }
}

fn traverse_bucket(node: &Node, key: &str) -> Option<i32> {
    match node{
        None => None,
        Some(link) => {
            if link.key == key {
                Some(link.val)
            }
            else {
                traverse_bucket(&link.next, key)
        }
    }
    }
}

fn get_hash_val(key: &str, num_buckets: usize) -> usize {
    let mut hasher = DefaultHasher::new();
    key.hash(&mut hasher);
    (hasher.finish() % (num_buckets as u64)) as usize
}

fn main() {
    let mut my_map = HashTable::new();
    my_map.put(&"K1", 1);
    my_map.put(&"K2", 2);
    println!("{:?}", my_map.get(&"K1"));
    println!("{:?}", my_map.get(&"K2"));
}

#[cfg(test)]
mod tests {
    use super::HashTable;
    use crate::get_hash;

   #[test]
    fn single_value() {
        let mut my_map = HashTable::new();
        my_map.put(&"K1", 1);
        assert_eq!(Some(1), my_map.get(&"K1")) 
    }

    #[test]
    fn compiles() {
        assert_eq!(true, true)
    }
}
