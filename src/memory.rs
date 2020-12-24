use crate::value::{Closure, LoxFn, Value, NativeFn};
use std::alloc::{alloc, Layout};
use std::mem;
use std::ptr;

#[derive(Debug, Copy, Clone)]
pub enum LoxObjectType {
    Str,
    LoxFn,
    Closure,
    NativeFn,
}

#[derive(Debug, Copy, Clone)]
pub struct LoxObject {
    pub kind: LoxObjectType,
    pub marked: bool,
    pub deleted: bool, 
    ptr: *mut u8,
}

impl From<String> for LoxObject {
    fn from(obj: String) -> Self {
        unsafe {
            let ptr = alloc(Layout::new::<String>()) as *mut String;
            ptr::write(ptr, obj);
            Self {
                kind: LoxObjectType::Str,
                marked: false,
                deleted: false,
                ptr: ptr as *mut u8,
            }
        }
    }
}
impl From<LoxFn> for LoxObject {
    fn from(obj: LoxFn) -> Self {
        unsafe {
            let ptr = alloc(Layout::new::<LoxFn>()) as *mut LoxFn;
            ptr::write(ptr, obj);
            Self {
                kind: LoxObjectType::LoxFn,
                marked: false,
                deleted: false,
                ptr: ptr as *mut u8,
            }
        }
    }
}
impl From<Closure> for LoxObject {
    fn from(obj: Closure) -> Self {
        unsafe {
            let ptr = alloc(Layout::new::<Closure>()) as *mut Closure;
            ptr::write(ptr, obj);
            Self {
                kind: LoxObjectType::Closure,
                marked: false,
                deleted: false,
                ptr: ptr as *mut u8,
            }
        }
    }
}
impl From<NativeFn> for LoxObject {
    fn from(obj: NativeFn) -> Self {
        unsafe {
            let ptr = alloc(Layout::new::<NativeFn>()) as *mut NativeFn;
            ptr::write(ptr, obj);
            Self {
                kind: LoxObjectType::NativeFn,
                marked: false,
                deleted: false,
                ptr: ptr as *mut u8,
            }
        }
    }
}/*
#[derive(Debug, Copy, Clone)]
pub enum LoxObject {
    Str(ValuePtr<String>),
    Function(ValuePtr<LoxFn>),
    Closure(ValuePtr<LoxClosure>),
}
impl LoxObject {
    #[allow(dead_code)]
    fn size(&self) -> usize {
        match self {
            LoxObject::Str(p) => mem::size_of_val(&p.ptr),
            LoxObject::Function(p) => mem::size_of_val(&p.ptr),
            LoxObject::Closure(p) => mem::size_of_val(&p.ptr),
        }
    }

    fn drop(self) {
        unsafe {
            match self {
                LoxObject::Str(p) => {
                    println!("dropping {:?} ({:?})", *p.ptr, p.ptr);
                    std::ptr::drop_in_place(p.ptr);
                }
                LoxObject::Function(_) => {
                    unreachable!()
                }
                LoxObject::Closure(_) => {
                    unreachable!()
                }
            };
        }
    }
}
impl From<String> for LoxObject {
    fn from(s: String) -> Self {
        LoxObject::Str(ValuePtr::new(s))
    }
}
impl From<LoxFn> for LoxObject {
    fn from(f: LoxFn) -> Self {
        LoxObject::Function(ValuePtr::new(f))
    }
}
impl From<LoxClosure> for LoxObject {
    fn from(c: LoxClosure) -> Self {
        LoxObject::Closure(ValuePtr::new(c))
    }
}*/

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
            next_gc: INIT_HEAP_CAPACITY - 1 as usize,
        }
    }

    pub fn allocate<T: Into<LoxObject>>(&mut self, obj: T) -> Value {
        self.allocated += 1;
        let obj = obj.into();
        if crate::trace_gc() {
            println!("allocated {:?}", obj);
        }
        self.objects.push(obj);
        obj.into()
    }

    pub fn oom(&self) -> bool {
        crate::stress_gc() || self.allocated >= self.next_gc
    }

    /*    pub fn gc(&mut self) {
        if crate::trace_gc() {
            println!("-- begin gc over {:?}", self.objects);
        }
        for idx in (0..self.objects.len()).rev() {
            match self.objects[idx] {
                LoxObject::Str(ptr) => {
                    if ptr.unmarked() {
                        let obj = self.objects.swap_remove(idx);
                        obj.drop();
                    }
                }
                _ => unreachable!(),
            }
        }
        if crate::trace_gc() {
            println!("-- end gc");
        }
    }*/
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
impl<T: core::fmt::Debug> ValuePtr<T> {
    pub fn new(v: T) -> Self {
        Self {
            ptr: ValuePtr::alloc(v),
            marked: false,
        }
    }
    pub fn mark(&mut self) {
        self.marked = true;
    }
    pub fn unmarked(&self) -> bool {
        !self.marked
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
