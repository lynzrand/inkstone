pub mod alloc;
#[cfg(test)]
mod test;
use alloc::RootSetHandle;
use modular_bitfield::prelude::*;
use std::marker::PhantomData;
use std::ops::{Deref, DerefMut};
use std::ptr::NonNull;
use vtable::{vtable, HasStaticVTable, VRef, VRefMut};

use self::alloc::GcAllocator;

/// A garbage-collected pointer
#[repr(transparent)]
pub struct Gc<T: ?Sized>(NonNull<GcValue<T>>);

impl<T: ?Sized> Gc<T> {
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

    fn header_ptr(&self) -> *mut GcHeader {
        self.as_raw_ptr().header() as *const GcHeader as *mut GcHeader
    }
}

impl<T: Trace + HasStaticVTable<TraceVTable>> Gc<T> {
    /// Create a new GC'ed pointer for the given type value and allocator.
    ///
    /// The type of `value` (`T`) must have a static VTable.
    pub fn new(value: T, allocator: &mut GcAllocator) -> Option<Self> {
        let ptr = allocator.alloc::<T>()?;
        unsafe {
            let ptr = RawGcPtr(ptr.cast());
            ptr.value().as_ptr().cast::<T>().write(value);
            Some(ptr.cast())
        }
    }
}

impl<T: Trace> Gc<T> {
    /// Create a new GC'ed pointer with the given value and vtable and allocator.
    ///
    /// # Safety
    ///
    /// The given VTable pointer must be valid among the whole life cycle of `T`,
    /// and matches the type definition.
    pub unsafe fn new_with_vtable(
        value: T,
        vtable: *const TraceVTable,
        allocator: &mut GcAllocator,
    ) -> Option<Self> {
        let ptr = allocator.alloc_raw(std::alloc::Layout::new::<T>(), vtable)?;
        ptr.value().as_ptr().cast::<T>().write(value);
        Some(ptr.cast())
    }
}

impl<T> Deref for Gc<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        unsafe { &self.0.as_ref().val }
    }
}

impl<T> Clone for Gc<T> {
    fn clone(&self) -> Self {
        unsafe {
            let header_ptr = self.header_ptr();
            gc_pointer_before_cloning(header_ptr);
        }
        Gc(self.0)
    }
}

impl<T: ?Sized> Drop for Gc<T> {
    fn drop(&mut self) {
        unsafe {
            gc_pointer_before_dropping(self.header_ptr());
        }
    }
}

unsafe fn gc_pointer_before_cloning(_ptr: *mut GcHeader) {
    // TODO: Increase RC value
}

unsafe fn gc_pointer_before_dropping(_ptr: *mut GcHeader) {
    // TODO: Decrease RC value
}

/// An untyped GC pointer.
///
/// This type should have the same layout as [`Gc`].
#[derive(Hash, Clone, Copy)]
#[repr(transparent)]
pub struct RawGcPtr(NonNull<GcHeader>);

impl RawGcPtr {
    /// Cast this opaque pointer into the desired type.
    ///
    /// # Safety
    ///
    /// The pointer must be pointing to the given type.
    pub unsafe fn cast<T>(self) -> Gc<T> {
        Gc(self.0.cast())
    }

    fn header(&self) -> &GcHeader {
        unsafe { self.0.as_ref() }
    }

    fn header_mut(&mut self) -> &mut GcHeader {
        unsafe { self.0.as_mut() }
    }

    pub fn value(&self) -> NonNull<u8> {
        unsafe {
            NonNull::new_unchecked(
                (self.0.as_ptr() as *mut u8).add(std::mem::size_of::<GcHeader>()),
            )
        }
    }
}

impl<T> From<Gc<T>> for RawGcPtr {
    fn from(val: Gc<T>) -> Self {
        *val.as_raw_ptr()
    }
}

impl<T> AsRef<RawGcPtr> for Gc<T> {
    fn as_ref(&self) -> &RawGcPtr {
        self.as_raw_ptr()
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

impl<T> GcValue<T> {
    fn header(&self) -> &GcHeader {
        &self.header
    }

    fn header_mut(&mut self) -> &mut GcHeader {
        &mut self.header
    }

    pub fn value(&self) -> &T {
        &self.val
    }

    pub fn value_mut(&mut self) -> &mut T {
        &mut self.val
    }
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

#[allow(clippy::all)]
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

/// A persistent handle in root set of an allocator.
///
/// # Safety
///
/// `Persistent<T>` struct can't be used across allocators.
pub struct Persistent<T>(RootSetHandle, PhantomData<T>);

impl<T> Persistent<T> {
    pub fn as_handle(&self, alloc: &GcAllocator) -> Option<Gc<T>> {
        unsafe { Some(alloc.persistent_handles.get(self.0)?.cast()) }
    }
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
    drop_in_place: fn(VRefMut<TraceVTable>) -> Layout,

    /// Dealloc this value using the global allocator. This method is NEVER used,
    /// but present only to make [`vtable`] happy.
    dealloc: fn(&TraceVTable, ptr: *mut u8, layout: Layout),
}

// pub trait GcTracer {
//     fn trace(&mut self, ptr: &RawGcPtr);
// }
// pub trait Trace {
//     fn trace(&self, tracer: &mut dyn GcTracer);
// }
