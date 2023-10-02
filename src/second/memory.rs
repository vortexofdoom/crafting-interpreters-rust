use std::{
    ptr::NonNull,
    sync::atomic::{AtomicIsize, Ordering},
};

use prehash::Prehashed;

use crate::GLOBAL;

use super::{
    compiler::Compiler,
    object::{
        IsObj, Obj, ObjBoundMethod, ObjClass, ObjClosure, ObjFunction, ObjInstance, ObjNative,
        ObjString, ObjType, ObjUpvalue, Upvalue,
    },
    value::Value,
};

pub static NEXT_GC: AtomicIsize = AtomicIsize::new(1024 * 1024);
const GC_GROW_FACTOR: isize = 2;

#[derive(Debug)]
pub struct Heap {
    pub objects: Option<NonNull<Obj>>,
    pub graystack: Vec<NonNull<Obj>>,
}

impl Heap {
    pub fn new() -> Self {
        Self {
            graystack: vec![],
            objects: None,
        }
    }

    /// Allocates a new Obj of type T on the heap, and returns a pointer to the Obj header
    /// The new Obj is added to the heap's objects list so it is not lost when the VM can no longer access it.
    pub fn new_obj<T: IsObj>(&mut self, obj: T) -> NonNull<Obj> {
        //println!("currently allocated: {}, next GC: {}", GLOBAL.stats().bytes_reallocated, NEXT_GC.load(Ordering::Relaxed));
        let new: NonNull<Obj> = NonNull::new(Box::into_raw(Box::new(obj)).cast()).unwrap();
        unsafe {
            (*new.as_ptr()).next = self.objects;
            self.objects = Some(new);
        }
        //println!("allocated {size}, Total: {}", ALLOCATED.load(Ordering::Relaxed));
        new
    }

    pub fn free_objects(&mut self) {
        while let Some(obj) = self.objects {
            unsafe {
                let obj = obj.as_ptr();
                self.objects = (*obj).next;
                self.free_object(obj);
            }
        }
    }

    pub fn mark_compiler_roots(&mut self, compiler: NonNull<Compiler>) {
        let mut compiler = Some(compiler);
        while let Some(c) = compiler {
            unsafe {
                self.mark_obj((*c.as_ptr()).function.cast());
                compiler = (*c.as_ptr()).enclosing
            }
        }
    }

    /// Marks the Obj as accessible (not to be deallocated) and pushes it to the graystack to be blackened
    /// If the Obj is already marked, this has no effect.
    pub fn mark_obj(&mut self, obj: NonNull<Obj>) {
        unsafe {
            if !(*obj.as_ptr()).is_marked {
                (*obj.as_ptr()).is_marked = true;
                self.graystack.push(obj);
            }
        }
    }

    /// If the value is of type Obj, mark it as accessible, otherwise do nothing
    pub fn mark_value(&mut self, value: Value) {
        if let Some(o) = value.as_obj() {
            self.mark_obj(o);
        }
    }

    /// Goes through every Obj on the graystack and finds any accessible Obj pointers, adding those to the graystack in turn.
    pub fn trace_references(&mut self) {
        while let Some(obj) = self.graystack.pop() {
            self.blacken_object(obj.as_ptr());
        }
    }

    /// Recursively traverses through the references to allocated Objects accessible from the given pointer, and marks each one in turn.
    fn blacken_object(&mut self, obj: *mut Obj) {
        unsafe {
            match (*obj).kind {
                ObjType::Function => {
                    for val in (*obj.cast::<ObjFunction>()).chunk.constants() {
                        self.mark_value(*val);
                    }
                }
                ObjType::Closure => {
                    let closure = obj.cast::<ObjClosure>();
                    self.mark_obj((*closure).function.cast());
                    for uv in &(*closure).upvalues {
                        self.mark_obj(uv.cast());
                    }
                }
                ObjType::Upvalue => {
                    if let Upvalue::Closed(closed) = &(*obj.cast::<ObjUpvalue>()).value {
                        self.mark_value(closed.get());
                    }
                }
                ObjType::Instance => {
                    let instance = obj.cast::<ObjInstance>();
                    self.mark_obj((*instance).class.cast());
                    for (key, val) in (*instance).fields.iter() {
                        self.mark_value(*Prehashed::as_inner(key));
                        self.mark_value(*val);
                    }
                }
                ObjType::BoundMethod => {
                    let bound = obj.cast::<ObjBoundMethod>();
                    self.mark_value((*bound).receiver);
                    self.mark_obj((*bound).method.cast());
                }
                ObjType::Class => {
                    let class = obj.cast::<ObjClass>();
                    self.mark_obj((*class).name.cast());
                    for (k, v) in (*class).methods.iter() {
                        self.mark_value(*Prehashed::as_inner(k));
                        self.mark_value(*v)
                    }
                }
                _ => {}
            }
        }
    }

    /// Frees the Obj
    /// This is accomplished by casting to the appropriate type so all the memory originally allocated is then deallocated.
    unsafe fn free_object(&mut self, obj: *mut Obj) {
        match (*obj).kind {
            ObjType::BoundMethod => drop(Box::from_raw(obj.cast::<ObjBoundMethod>())),
            ObjType::Class => drop(Box::from_raw(obj.cast::<ObjClass>())),
            ObjType::Closure => drop(Box::from_raw(obj.cast::<ObjClosure>())),
            ObjType::Function => drop(Box::from_raw(obj.cast::<ObjFunction>())),
            ObjType::Instance => drop(Box::from_raw(obj.cast::<ObjInstance>())),
            ObjType::Native => drop(Box::from_raw(obj.cast::<ObjNative>())),
            ObjType::String => drop(Box::from_raw(obj.cast::<ObjString>())),
            ObjType::Upvalue => drop(Box::from_raw(obj.cast::<ObjUpvalue>())),
        }
    }

    /// Traverses the list of objects and drops those that are not marked
    /// Adjusts the GC threshold based on how much memory is still in use post-GC.
    pub fn sweep(&mut self) {
        //let prev_bytes = GLOBAL.stats().bytes_reallocated;
        let mut prev = None;
        let mut object = self.objects;
        while let Some(obj) = object {
            let o = obj.as_ptr();
            unsafe {
                if (*o).is_marked {
                    (*o).is_marked = false;
                    prev = Some(obj);
                    object = (*o).next;
                } else {
                    object = (*o).next;
                    match prev {
                        Some(p) => (*p.as_ptr()).next = object,
                        None => self.objects = object,
                    }

                    self.free_object(o);
                }
            }
        }
        let new = GLOBAL.stats().bytes_reallocated;
        NEXT_GC.store(new * GC_GROW_FACTOR, Ordering::Relaxed);
        //println!("finished GC. Prev bytes: {prev_bytes}, current bytes: {new}");
    }
}
