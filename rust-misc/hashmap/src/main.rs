use std::collections::hash_map::DefaultHasher;
use std::hash::{Hash, Hasher};
use std::mem;

type Node<'a> = Option<Box<Link<'a>>>;

struct Link<'a> {
    key: &'a str,
    val: i32,
    next: Node<'a> // None or Box<Link>
}

const INIT_BUCKET_COUNT: usize = 1024;
pub struct HashTable<'a> {
    num_elems: i32,
    num_buckets: usize,
    buckets: Vec<Node<'a>>
}   

impl<'a> Link<'a>{
    pub fn new(key: &'a str, val: i32) -> Self {
        Link{ key, val, next : None }
    }
}

impl<'a> HashTable<'a>{
    pub fn new() -> Self {
        let mut buckets = Vec::with_capacity(INIT_BUCKET_COUNT);
        for _ in 0..INIT_BUCKET_COUNT {
            buckets.push(None);
        }
        HashTable{ num_elems : 0, num_buckets: INIT_BUCKET_COUNT, buckets }
    }

    pub fn put(&mut self, key: &'a str, val: i32) {
        // This looks... terrible. 
        let hash_val = ( get_hash(key) % (self.num_buckets as u64) ) as usize;
        let mut new_link = Link::new(key,val);
        if let Some(n) = self.buckets.get_mut(hash_val) {
            //N is &mut Node
            if let Some(l) = mem::replace(n, None) {
                //L is Box<Link>
                new_link.next = Some(l);
            }
        }
        self.buckets[hash_val] = Some(Box::new(new_link));
        self.num_elems += 1;
    }

    pub fn get(&self, key: &str) -> Option<i32> {
        let hash_val = ( get_hash(key) % (self.num_buckets as u64) ) as usize;
        match self.buckets.get(hash_val) {
            None => None,
            Some(n) => {
                match n {
                    None => None,
                    Some(l) => Some(l.val)
                }  
            }
        }
    }
}

fn get_hash(key: &str) -> u64 {
    let mut hasher = DefaultHasher::new();
    key.hash(&mut hasher);
    hasher.finish()
}

fn main() {
    let mut my_map = HashTable::new();
    my_map.put(&"K1", 1);
    println!("{:?}", my_map.get(&"K1"))
}

#[cfg(test)]
mod tests {
    use super::HashTable;
    use crate::get_hash;

    #[test]
    fn hash_fn() {
        assert_eq!(7982898449168957443, get_hash(&"Hello World"))
    }
    
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
