use std::{ptr::NonNull, alloc::{GlobalAlloc, System, Allocator, AllocError}, sync::atomic::{AtomicUsize, Ordering}};

use super::{
    object::{IsObj, Obj, ObjClosure, ObjFunction, ObjNative, ObjString, ObjType, ObjUpvalue},
    value::Value,
    Vm,
};

static ALLOCATED: AtomicUsize = AtomicUsize::new(0);
static NEXT_GC: AtomicUsize = AtomicUsize::new(1024 * 1024);

// unsafe impl Allocator for Heap {
//     fn allocate(&self, layout: std::alloc::Layout) -> Result<NonNull<[u8]>, std::alloc::AllocError> {
//         let ret = System.alloc(layout);
//         if !ret.is_null() {
//             ALLOCATED.fetch_add(layout.size(), Ordering::Relaxed);
//         }
//         NonNull::new(ret).ok_or(AllocError)
//     }

//     unsafe fn deallocate(&self, ptr: NonNull<u8>, layout: std::alloc::Layout) {
//         System.dealloc(ptr, layout);
//         ALLOCATED.fetch_sub(layout.size(), Ordering::Relaxed);
//     }
// }

#[derive(Debug)]
pub struct Heap {
    pub bytes_allocated: usize,
    next_gc: usize,
    pub objects: Option<NonNull<Obj>>,
    pub graystack: Vec<NonNull<Obj>>,
}

impl Heap {
    pub fn new() -> Self {
        Self {
            bytes_allocated: 0,
            next_gc: 1024 * 1024,
            objects: None,
            graystack: vec![],
        }
    }


    pub fn new_obj<T: IsObj>(&mut self, mut obj: T) -> NonNull<Obj> {
        //self.collect_garbage();
        let size = obj.size();
        let mut new: NonNull<Obj> = NonNull::new(Box::into_raw(Box::new(obj)).cast()).unwrap();
        unsafe {
            (*new.as_ptr()).next = self.objects;
            self.objects = Some(new);
        }
        self.bytes_allocated += size;
        //println!("allocated {size}, Total: {}", self.bytes_allocated);
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

    pub fn mark_vm_roots(&mut self, vm: *const Vm) {
        unsafe {
            for val in &(*vm).stack[..(*vm).sp] {
                self.mark_value(val.get());
            }

            for (name, value) in (*vm).globals.iter() {
                self.mark_value(*name);
                self.mark_value(*value);
            }

            for frame in &(*vm).frames[..(*vm).frame_count] {
                let obj = NonNull::new(frame.closure.cast_mut().cast()).unwrap();
                self.mark_obj(obj);
            }

            let mut upvalue = (*vm).open_upvalues;
            while let Some(uv) = upvalue {
                self.mark_obj(uv.cast());
                upvalue = Some(uv);
            }

            // let mut objects = self.objects;
            // while let Some(obj) = objects {
            //     if (*obj.as_ptr()).is_marked {
            //         println!("{} is marked", Value::Obj(obj));
            //     } else {
            //         println!("{} is not marked", Value::Obj(obj));
            //     }
            //     objects = (*obj.as_ptr()).next;
            // }
        }
    }

    fn mark_obj(&mut self, obj: NonNull<Obj>) {
        unsafe {
            if !(*obj.as_ptr()).is_marked {
                (*obj.as_ptr()).is_marked = true;
                self.graystack.push(obj);
            }
        }
    }

    fn mark_value(&mut self, value: Value) {
        if let Value::Obj(o) = value {
            self.mark_obj(o);
        }
    }

    pub fn trace_references(&mut self) {
        while let Some(obj) = self.graystack.pop() {
            self.blacken_object(obj);
        }
    }

    fn blacken_object(&mut self, obj: NonNull<Obj>) {
        unsafe {
            let obj = obj.as_ptr();
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
                ObjType::Upvalue => self.mark_value((*obj.cast::<ObjUpvalue>()).closed.get()),
                _ => {}
            }
        }
    }

    unsafe fn free_object(&mut self, obj: *mut Obj) {
        match (*obj).kind {
            ObjType::String => drop(Box::from_raw(obj.cast::<ObjString>())),
            ObjType::Function => drop(Box::from_raw(obj.cast::<ObjFunction>())),
            ObjType::Native => drop(Box::from_raw(obj.cast::<ObjNative>())),
            ObjType::Closure => drop(Box::from_raw(obj.cast::<ObjClosure>())),
            ObjType::Upvalue => drop(Box::from_raw(obj.cast::<ObjUpvalue>())),
        }
    }

    pub fn sweep(&mut self) {
        let prev_bytes = self.bytes_allocated;
        self.bytes_allocated = 0;
        let mut prev = None;
        let mut object = self.objects;
        while let Some(obj) = object {
            let o = obj.as_ptr();
            unsafe {
                if (*o).is_marked {
                    (*o).is_marked = false;

                    self.bytes_allocated += match (*o).kind {
                        ObjType::String => (*o.cast::<ObjString>()).size(),
                        ObjType::Function => (*o.cast::<ObjFunction>()).size(),
                        ObjType::Native => (*o.cast::<ObjNative>()).size(),
                        ObjType::Closure => (*o.cast::<ObjClosure>()).size(),
                        ObjType::Upvalue => (*o.cast::<ObjUpvalue>()).size(),
                    };
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
        println!("finished GC. Prev bytes: {prev_bytes}, current bytes: {}", self.bytes_allocated);
    }
}
