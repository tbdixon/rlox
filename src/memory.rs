use crate::value::{Value, LoxClosure, LoxFn};
use std::alloc::{alloc, Layout};
use std::ptr;
use std::mem;

#[derive(Copy, Clone)]
pub enum LoxObject {
    Str(ValuePtr<String>),
    Function(ValuePtr<LoxFn>),
    Closure(ValuePtr<LoxClosure>),
}
impl From<String> for LoxObject {
    fn from(s: String) -> Self {
        LoxObject::Str(ValuePtr::new(s))
    }
}

pub struct LoxHeap {
    objects: Vec<LoxObject>,
    allocated: usize,
    next_gc: usize,
}
const INIT_HEAP_CAPACITY: usize = u8::MAX as usize;
impl LoxHeap {
    pub fn new() -> Self {
        Self {
            objects: Vec::with_capacity(INIT_HEAP_CAPACITY),
            allocated: 0,
            next_gc: INIT_HEAP_CAPACITY * u8::MAX as usize,
        }
    }

    pub fn allocate<T: Into<LoxObject>>(&mut self, obj: T) -> Value {
        if self.allocated >= self.next_gc {
            unimplemented!();
        }
        self.allocated += mem::size_of::<T>();
        let obj = obj.into();
        self.objects.push(obj);
        obj.into()
    }
}

#[derive(Debug, PartialEq)]
pub struct ValuePtr<T> {
    marked: bool,
    ptr: *mut T,
}
impl<T> Copy for ValuePtr<T> {}
impl<T> Clone for ValuePtr<T> {
    fn clone(&self) -> ValuePtr<T> {
        *self
    }
}
//TODO ContravariantLifetime etc.
impl<T> ValuePtr<T> {
    pub fn new(v: T) -> Self {
        Self {
            ptr: ValuePtr::alloc(v),
            marked: false,
        }
    }
    pub fn ptr(&self) -> *const T {
        self.ptr as *const T
    }
    pub fn r#ref(&self) -> &T {
        unsafe { &(*self.ptr) }
    }
    fn alloc(val: T) -> *mut T {
        unsafe {
            let ptr = alloc(Layout::new::<T>()) as *mut T;
            ptr::write(ptr, val);
            ptr
        }
    }
}
