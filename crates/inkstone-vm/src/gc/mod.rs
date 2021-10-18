pub mod alloc;
use std::ops::{Deref, DerefMut};
use std::ptr::NonNull;

use modular_bitfield::prelude::*;
use vtable::VRef;
use vtable::{vtable, VRefMut};

/// A garbage-collected pointer
#[repr(transparent)]
pub struct Gc<T: ?Sized>(NonNull<GcValue<T>>);

impl<T> Gc<T> {
    /// Get a mutable reference from an immutable GC pointer.
    ///
    /// # Safety
    ///
    /// The user is responsible for not getting two mutable pointers at once.
    #[allow(clippy::mut_from_ref)]
    pub unsafe fn get_mut(&self) -> &mut T {
        &mut (*self.0.as_ptr()).val
    }

    unsafe fn gc_val_ref(&self) -> &GcValue<T> {
        &*self.0.as_ptr()
    }

    /// Convert this type into an reference of an untyped [`RawGcPtr`].
    /// This method is necessary for GC implementations.
    pub fn as_raw_ptr(&self) -> &RawGcPtr {
        unsafe { std::mem::transmute(self) }
    }
}

impl<T> Deref for Gc<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        unsafe { &self.0.as_ref().val }
    }
}

impl<T> DerefMut for Gc<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe { &mut self.0.as_mut().val }
    }
}

impl<T> Clone for Gc<T> {
    fn clone(&self) -> Self {
        let new_gc = Gc(self.0);
        unsafe {
            let val_ref = self.gc_val_ref();
            let rc = val_ref.header.flags.rc();
        }
        new_gc
    }
}

#[derive(Hash)]
#[repr(transparent)]
pub struct RawGcPtr(NonNull<GcHeader>);

impl RawGcPtr {
    /// Cast this opaque pointer into the desired type
    pub unsafe fn cast<T>(self) -> Gc<T> {
        Gc(self.0.cast())
    }
}

/// The heap allocated part of a [`GcValue`].
///
/// The layout of this type ensures a `*GcHeader` is always a valid `*GcValue`
#[repr(C)]
pub struct GcValue<T: ?Sized> {
    header: GcHeader,
    val: T,
}

#[repr(C)]
#[repr(align(16))]
struct GcHeader {
    flags: GcHeaderFlags,
    size: u32,
    next_allocation: Option<NonNull<GcHeader>>,
    trace_impl: *const TraceVTable,
}

impl GcHeader {
    pub unsafe fn as_value<T>(&self) -> &GcValue<T> {
        std::mem::transmute(self)
    }

    pub unsafe fn as_value_mut<T>(&mut self) -> &mut GcValue<T> {
        std::mem::transmute(self)
    }
}

#[bitfield]
#[repr(u32)]
struct GcHeaderFlags {
    /// Whether this struct is oversized
    pub oversized: bool,

    /// Color used in GC
    #[bits = 2]
    pub color: GcColor,

    /// Reference counting
    pub rc: B18,

    // dummy
    #[skip]
    __: B11,
}

#[derive(Debug, BitfieldSpecifier)]
#[bits = 2]
enum GcColor {
    White,
    Gray,
    Black,
}

#[vtable]
#[repr(C)]
pub struct GcTracerVTable {
    trace: fn(VRefMut<GcTracerVTable>, item: &RawGcPtr),
}

/// Trait for types that are garbage collected
#[vtable]
#[repr(C)]
pub struct TraceVTable {
    trace: fn(VRef<TraceVTable>, tracer: VRefMut<GcTracerVTable>),
    drop: fn(VRefMut<TraceVTable>),
}

// pub trait GcTracer {
//     fn trace(&mut self, ptr: &RawGcPtr);
// }
// pub trait Trace {
//     fn trace(&self, tracer: &mut dyn GcTracer);
// }
