use crate::value::{Closure, LoxFn, NativeFn};
use std::alloc::Layout;
use std::marker::PhantomData;
use std::ops::Deref;

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum LoxObjectType {
    Str,
    LoxFn,
    Closure,
    NativeFn,
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub struct LoxObject {
    pub kind: LoxObjectType,
    pub marked: bool,
    pub deleted: bool,
    pub allocated_size: usize,
    ptr: *mut u8,
}
impl std::fmt::Display for LoxObject {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        unsafe {
            match self.kind {
                LoxObjectType::Str => write!(f, "{}", &*(self.ptr as *const String)),
                LoxObjectType::LoxFn => write!(f, "{}", &*(self.ptr as *const LoxFn)),
                LoxObjectType::Closure => write!(f, "{}", &*(self.ptr as *const Closure)),
                LoxObjectType::NativeFn => write!(f, "{}", &*(self.ptr as *const NativeFn)),
            }
        }
    }
}

macro_rules! obj_impl_from {
    ($from_type:ty, $into_type:ident) => {
        impl From<$from_type> for LoxObject {
            fn from(obj: $from_type) -> Self {
                let ptr = std::boxed::Box::into_raw(Box::new(obj));
                Self {
                    kind: LoxObjectType::$into_type,
                    marked: false,
                    deleted: false,
                    ptr: ptr as *mut u8,
                    allocated_size: Layout::new::<$from_type>().size()
                }
            }
        }
    };
}

obj_impl_from!(String, Str);
obj_impl_from!(Closure, Closure);
obj_impl_from!(LoxFn, LoxFn);
obj_impl_from!(NativeFn, NativeFn);

macro_rules! obj_assert {
    ($self:ident, $type:ident) => {
        assert!($self.kind == LoxObjectType::$type && !$self.deleted);
    };
}

impl LoxObject {
    fn str(&self) -> &String {
        obj_assert!(self, Str);
        unsafe { &*(self.ptr as *const String) }
    }

    fn drop(mut self) {
        assert!(!self.deleted);
        unsafe {
            if crate::trace_gc() {
                match self.kind {
                    LoxObjectType::Str => print!("dropping {:?} ", *(self.ptr as *const String)),
                    LoxObjectType::LoxFn => print!("dropping {:?} ", *(self.ptr as *const LoxFn)),
                    LoxObjectType::NativeFn => print!("dropping {:?} ", *(self.ptr as *const NativeFn)),
                    LoxObjectType::Closure => print!("dropping {:?} ", *(self.ptr as *const Closure)),
                }; 
                println!("@ {:p}", self.ptr)
            }
            self.deleted = true;
            let _ = std::boxed::Box::from_raw(self.ptr);
        }
    }
}

pub struct LoxHeap {
    objects: Vec<*mut LoxObject>,
    allocated: usize,
    next_gc: usize,
}
const INIT_HEAP_CAPACITY: usize = 4 * u8::MAX as usize;
impl LoxHeap {
    pub fn new() -> Self {
        Self {
            objects: Vec::with_capacity(INIT_HEAP_CAPACITY),
            allocated: 0,
            next_gc: INIT_HEAP_CAPACITY - 1 as usize,
        }
    }

    fn allocate<T: Into<LoxObject>>(&mut self, obj: T) -> ValuePtr<T> {
        let obj = Box::new(obj.into());
        self.allocated += Layout::new::<T>().size();
        if crate::trace_gc() {
            println!("allocated {:?} @ {:p} (total: {})", obj.kind, obj.ptr, self.allocated);
        }
        let ptr = std::boxed::Box::into_raw(obj);
        self.objects.push(ptr);
        ValuePtr { ptr, phantom: PhantomData }
    }

    fn oom(&self) -> bool {
        crate::stress_gc() || self.allocated >= self.next_gc
    }

    fn gc(&mut self) {
        if crate::trace_gc() {
            println!("-- begin gc");
        }
        for idx in (0..self.objects.len()).rev() {
            unsafe {
                if !(*self.objects[idx]).marked {
                    let obj = self.objects.swap_remove(idx);
                    // Drop the heap allocated value that the LoxObject points to
                    self.allocated -= (*obj).allocated_size;
                    (*obj).drop();
                    // Drop the LoxObject itself
                    let _ = std::boxed::Box::from_raw(obj);
                }
            }
        }
        if crate::trace_gc() {
            println!("-- end gc");
        }
    }
}

pub fn loxallocate<T: Into<LoxObject>>(obj: T) -> ValuePtr<T> {
    // Handle if not initialized here rather than in main()
    unsafe {
        if (*crate::HEAP).oom() {
            (*crate::HEAP).gc();
        }
        (*crate::HEAP).allocate(obj)
    }
}

#[derive(Debug, PartialEq)]
pub struct ValuePtr<T> {
    ptr: *mut LoxObject,
    phantom: PhantomData<T>,
}
impl<T> Copy for ValuePtr<T> {}

impl<T> Clone for ValuePtr<T> {
    fn clone(&self) -> ValuePtr<T> {
        *self
    }
}

impl<T> ValuePtr<T> {
    pub fn mark(&self) {
        unsafe {
            (*self.ptr).marked = true;
        }
    }
    pub fn str(&self) -> &str {
        unsafe { (*self.ptr).str() }
    }
}

impl<T: std::fmt::Debug> Deref for ValuePtr<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        unsafe {
            assert!(!(*self.ptr).deleted);
            &*(((*self.ptr).ptr) as *const T)
        }
    }
}

impl<T> std::fmt::Display for ValuePtr<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        unsafe { write!(f, "{}", *self.ptr) }
    }
}
