use std::ptr::NonNull;

use super::object::{
    IsObj, Obj, ObjClosure, ObjFunction, ObjNative, ObjString, ObjType, ObjUpvalue,
};

#[derive(Debug)]
pub struct Heap {
    pub bytes_allocated: usize,
    pub objects: Option<NonNull<Obj>>,
}

impl Heap {
    pub fn new() -> Self {
        Self {
            bytes_allocated: 0,
            objects: None,
        }
    }

    pub fn new_obj<T: IsObj>(&mut self, mut obj: T) -> NonNull<Obj> {
        let mut new: NonNull<Obj> = NonNull::new(Box::into_raw(Box::new(obj)).cast()).unwrap();
        unsafe {
            (*new.as_ptr()).next = self.objects;
            self.objects = Some(new);
        }
        self.bytes_allocated += std::mem::size_of::<T>();
        println!("allocated: {}", self.bytes_allocated);
        new
    }

    pub fn free_objects(&mut self) {
        while let Some(obj) = self.objects {
            unsafe {
                let obj = obj.as_ptr();
                self.objects = (*obj).next;
                match (*obj).kind {
                    ObjType::String => drop(Box::from_raw(obj.cast::<ObjString>())),
                    ObjType::Function => drop(Box::from_raw(obj.cast::<ObjFunction>())),
                    ObjType::Native => drop(Box::from_raw(obj.cast::<ObjNative>())),
                    ObjType::Closure => drop(Box::from_raw(obj.cast::<ObjClosure>())),
                    ObjType::Upvalue => drop(Box::from_raw(obj.cast::<ObjUpvalue>())),
                };
            }
        }
    }
}
