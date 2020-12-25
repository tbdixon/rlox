use crate::value::{Closure, LoxFn, NativeFn};
use std::alloc::{alloc, Layout};
use std::marker::PhantomData;
use std::ops::Deref;
use std::ptr;

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
                unsafe {
                    let ptr = alloc(Layout::new::<$from_type>()) as *mut $from_type;
                    ptr::write(ptr, obj);
                    Self {
                        kind: LoxObjectType::$into_type,
                        marked: false,
                        deleted: false,
                        ptr: ptr as *mut u8,
                    }
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

    //TODO capture amount dropped
    fn drop(mut self) {
        assert!(!self.deleted);
        unsafe {
            println!("dropping {:?} ({:?})", *self.ptr, self.ptr);
            self.deleted = true;
            std::ptr::drop_in_place(self.ptr);
        }
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
            next_gc: INIT_HEAP_CAPACITY - 1 as usize,
        }
    }

    fn allocate<T: Into<LoxObject>>(&mut self, obj: T) -> ValuePtr<T> {
        self.allocated += Layout::new::<T>().size();
        let mut obj: LoxObject = obj.into();
        if crate::trace_gc() {
            println!("allocated {:?}", obj);
        }
        self.objects.push(obj);
        ValuePtr {
            ptr: &mut obj as *mut LoxObject,
            phantom: PhantomData,
        }
    }

    fn oom(&self) -> bool {
        crate::stress_gc() || self.allocated >= self.next_gc
    }

    fn gc(&mut self) {
        if crate::trace_gc() {
            println!("-- begin gc over {:?}", self.objects);
        }
/*        for idx in (0..self.objects.len()).rev() {
            if !self.objects[idx].marked {
                let obj = self.objects.swap_remove(idx);
                obj.drop();
            }
        }*/
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

impl<T> Deref for ValuePtr<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        unsafe {
            assert!(!(*self.ptr).deleted);
            &*((*(*self.ptr).ptr) as *const T)
        }
    }
}

impl<T> std::fmt::Display for ValuePtr<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        unsafe { write!(f, "{}", *self.ptr) }
    }
}
