//use crate::Result;
use std::mem::{replace, MaybeUninit};
use std::ops::Index;

// Build our own stack so that we can use a small stack allocated bit of space
// rather than a Vec which will continually be allocating around the heap
// given this is the core element of our VM worth having much faster.
// 
// The problem is that array need a fixed size at compile time, so need to determine how to
// generalize this slightly (to handle call stacks and frame stacks) within that constraint.
const MAX_STACK_SIZE: usize = 1024;
#[derive(Debug)]
pub struct Stack<T: std::fmt::Debug + std::cmp::PartialEq + std::clone::Clone> {
    top: usize,
    stack: [Option<T>; MAX_STACK_SIZE],
}

impl<T: std::fmt::Debug + std::cmp::PartialEq + std::clone::Clone> Stack<T> {
    pub fn new() -> Stack<T> {
        // Some fun unsafe rust to initialize a constant array of Option<T> given the
        // T may not implement Copy trait (e.g. Value does not since it has a String).
        // Immedietly replace the values with None and assume_init to get out of the unsafe
        // world.
        let stack = unsafe {
            let mut s = MaybeUninit::<[Option<T>; MAX_STACK_SIZE]>::uninit();
            let p = s.as_mut_ptr() as *mut Option<T>;
            for offset in 0..MAX_STACK_SIZE as isize {
                std::ptr::write(p.offset(offset), None);
            }
            s.assume_init()
        };
        Stack { top: 0, stack }
    }

    pub fn is_empty(&self) -> bool {
        self.top == 0
    }

    pub fn pop(&mut self) -> Result<T, &'static str> {
        if self.top > 0 {
            self.top -= 1;
            replace(&mut self.stack[self.top], None).ok_or("Empty stack")
        } else {
            Err("Empty stack")
        }
    }

    pub fn pop_mult(&mut self, count: usize) -> Result<(), &'static str> {
        for _ in 0..count {
            self.pop()?;
        }
        Ok(())
    }

    // Returns a reference to the current end of the stack; no updates.
    //
    // Since the stack is initialized to all None and we mem repalce anything 
    // with None on pop, stack[0] can be used as a reference to None.
    pub fn peek(&self) -> Option<&T> {
        let mut idx = 0;
        if self.top > 0 {
            idx = self.top - 1;
        }
        self.stack[idx].as_ref()
    }

    pub fn peek_mut(&mut self) -> Option<&mut T> {
        let mut idx = 0;
        if self.top > 0 {
            idx = self.top - 1;
        }
        self.stack[idx].as_mut()
    }


    pub fn push(&mut self, v: T) -> usize {
        self.stack[self.top] = Some(v);
        self.top += 1;
        self.top - 1
    }

    pub fn update(&mut self, index: usize, v: T) {
        self.stack[index] = Some(v);
    }

    pub fn get(&mut self, index: usize) -> Result<&T, &'static str> {
        if let Some(v) = &self.stack[index] {
            Ok(&v)
        } else {
            Err("Nothing at index")
        }
    }

    pub fn take(&mut self, index: usize, replacement: T) -> Result<T, &'static str> {
        replace(&mut self.stack[index], Some(replacement)).ok_or("Nothing at index")
    }

    // Linear scan starting from last pushed element
    pub fn find(&self, needle: T) -> Option<usize> {
        if self.top > 0 {
            let mut idx = self.top - 1;
            while let Some(v) = &self.stack[idx] {
                if *v == needle {
                    return Some(idx);
                }
                if idx == 0 {
                    return None;
                }
                idx -= 1;
            }
        }
        None
    } 

    pub fn len(&self) -> usize {
        self.top
    }

    pub fn print_stack(&self) {
        if !self.is_empty() {
            print!("Stack top {}: ", self.top);
            for idx in 0..self.top {
                print!("[");
                print!("{:?}", self.stack[idx]);
                print!("]");
            }
            println!();
        }
    }
}

impl<T: std::fmt::Debug + std::cmp::PartialEq + std::clone::Clone> Index<usize> for Stack<T> {
    type Output = Option<T>;

    fn index(&self, i: usize) -> &Option<T> {
        &self.stack[i]
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_create_empty() {
        let mut s = Stack::new();
        assert_eq!(true, s.is_empty());
        for idx in 0..MAX_STACK_SIZE {
            assert_eq!(s[idx], None)
        }
        s.push(String::from("A"));
        assert_eq!(false, s.is_empty());
    }

    #[test]
    fn test_push() {
        let mut s = Stack::new();
        s.push(String::from("A"));
        s.push(String::from("B"));
        s.push(String::from("C"));
        assert_eq!(s.stack[0], Some(String::from("A")));
        assert_eq!(s.stack[1], Some(String::from("B")));
        assert_eq!(s.stack[2], Some(String::from("C")));
        assert_eq!(s.top, 3);
    }

    #[test]
    fn test_peek() {
        let mut s = Stack::new();
        assert_eq!(s.peek(), None);
        s.push(String::from("A"));
        s.push(String::from("B"));
        s.push(String::from("C"));
        assert_eq!(s.peek(), Some(String::from("C")));
        assert_eq!(s.top, 3);
    }

    #[test]
    fn test_pop() {
        let mut s = Stack::new();
        s.push(String::from("A"));
        s.push(String::from("B"));
        s.push(String::from("C"));
        assert_eq!(s.pop(), Ok(String::from("C")));
        assert_eq!(s.top, 2);
        assert_eq!(s.pop(), Ok(String::from("B")));
        assert_eq!(s.top, 1);
        assert_eq!(s.pop(), Ok(String::from("A")));
        assert_eq!(s.top, 0);
        assert_eq!(s.is_empty(), true)
    }

    #[test]
    fn test_update_get() {
        let mut s = Stack::new();
        s.push(String::from("A"));
        s.push(String::from("B"));
        s.push(String::from("C"));
        s.update(1, String::from("Z"));
        assert_eq!(s.get(1), Ok(&String::from("Z")));
        assert_eq!(s.top, 3);
    }

    #[test]
    fn test_find() {
        let mut s = Stack::new();
        s.push(String::from("A"));
        s.push(String::from("B"));
        s.push(String::from("C"));
        assert_eq!(s.find(String::from("Z")), None);
        assert_eq!(s.find(String::from("A")), Some(0 as usize));
        assert_eq!(s.top, 3);
    }
}
