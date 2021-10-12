pub mod alloc;
use std::ops::Deref;

use modular_bitfield::prelude::*;

/// A garbage-collected pointer
pub struct Gc<T: ?Sized>(*mut GcValue<T>);

impl<T> Gc<T> {
    /// Get a mutable reference from an immutable GC pointer.
    ///
    /// # Safety
    ///
    /// The user is responsible for not getting two mutable pointers at once.
    #[allow(clippy::mut_from_ref)]
    pub unsafe fn get_mut(&self) -> &mut T {
        &mut (*self.0).val
    }

    unsafe fn gc_val_ref(&self) -> &GcValue<T> {
        &*self.0
    }
}

impl<T> Deref for Gc<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        unsafe { &(*self.0).val }
    }
}

impl<T> Clone for Gc<T> {
    fn clone(&self) -> Self {
        let new_gc = Gc(self.0);
        unsafe {
            let val_ref = self.gc_val_ref();
            let rc = val_ref.flags.rc();
        }
        new_gc
    }
}

#[repr(C)]
pub struct GcValue<T: ?Sized> {
    flags: Header,
    val: T,
}

#[bitfield]
#[repr(u32)]
struct Header {
    /// Color used in GC
    #[bits = 2]
    pub color: GcColor,

    /// Reference counting
    pub rc: B18,

    // dummy
    #[skip]
    __: B12,
}

#[derive(Debug, BitfieldSpecifier)]
#[bits = 2]
enum GcColor {
    White,
    Gray,
    Black,
}

pub trait GcTracer {
    fn trace(&mut self, ptr: Gc<dyn GarbageCollected>);
}

pub unsafe trait GarbageCollected {
    fn trace(&self, tracer: &mut dyn GcTracer);
}
