pub mod alloc;
#[cfg(test)]
mod test;
use alloc::RootSetHandle;
use inkstone_util::by_ptr::AsCmpPtr;
use mimalloc_rust_sys::basic_allocation::mi_free;
use modular_bitfield::prelude::*;
use std::cell::{Cell, UnsafeCell};
use std::marker::PhantomData;
use std::ops::Deref;
use std::ptr::NonNull;
use vtable::{vtable, HasStaticVTable, VRef, VRefMut};

use self::alloc::GcAllocator;

/// A garbage-collected pointer.
///
/// This pointer is both trace-collected and reference-counted, in order to
/// achieve a better garbage collection.
#[repr(transparent)]
pub struct Gc<T: ?Sized>(NonNull<UnsafeCell<GcValue<T>>>);

impl<T: ?Sized> Gc<T> {
    /// Get a mutable reference from an immutable GC pointer.
    ///
    /// # Safety
    ///
    /// The user is responsible for not getting two mutable pointers at once.
    #[allow(clippy::mut_from_ref)]
    pub unsafe fn get_mut(&self) -> &mut T {
        &mut (*(*self.0.as_ptr()).get()).val
    }

    /// Call `f` with a mutable reference of the value `self` is pointing to.
    ///
    /// This method should **not** be used when [`Self::get_mut`] is holding a
    /// mutable reference. (It's handled on `get_mut`'s side anyway)
    ///
    /// # Safety
    ///
    /// Yes, this function is unsafe. You can easily alias a mutable pointer in
    /// this way:
    ///
    /// ```notest
    /// let gc = ...;
    /// let gc2 = gc.clone();
    /// gc.with(|val1| gc2.with(...));  // <- Look ma, aliasing &mut's!
    /// ```
    ///
    /// But since this function is not public, we just leave it as is.
    ///
    /// Be careful not to alias mutable references!
    #[allow(clippy::mut_from_ref)]
    pub unsafe fn with<R>(&self, f: impl FnOnce(&mut T) -> R) -> R {
        f(unsafe { self.get_mut() })
    }

    unsafe fn gc_val_ref(&self) -> &GcValue<T> {
        (*self.0.as_ptr()).get().as_ref().unwrap()
    }

    /// Convert this type into an reference of an untyped [`RawGcPtr`].
    /// This method is necessary for GC implementations.
    pub fn as_raw_ptr(&self) -> &RawGcPtr {
        unsafe { std::mem::transmute(self) }
    }

    fn header_ptr(&self) -> NonNull<GcHeader> {
        unsafe {
            NonNull::new_unchecked(self.as_raw_ptr().header() as *const GcHeader as *mut GcHeader)
        }
    }

    pub fn value_ptr_unsized(&self) -> NonNull<u8> {
        unsafe {
            NonNull::new_unchecked(
                (self.0.as_ptr() as *mut u8).add(std::mem::size_of::<GcHeader>()),
            )
        }
    }
}

impl<T> Gc<T> {
    pub fn value_ptr(&self) -> NonNull<T> {
        unsafe { self.value_ptr_unsized().cast() }
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
        unsafe { &(*self.0.as_ref().get()).val }
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

impl<T: ?Sized> AsCmpPtr for Gc<T> {
    fn as_cmp_ptr(&self) -> *const () {
        self.0.as_ptr() as *const ()
    }
}

impl<T: ?Sized> Trace for Gc<T> {
    fn trace(&self, tracer: VRefMut<GcTracerVTable>) {
        unsafe {
            let header = self.header_ptr().as_ref();
            let trace_vtable_ptr = header.trace_impl;
            let trace_vtable = &*header.trace_impl;
            (trace_vtable.trace)(
                VRef::from_raw(
                    NonNull::new_unchecked(trace_vtable_ptr as *mut _),
                    self.value_ptr_unsized(),
                ),
                tracer,
            );
        }
    }
}

unsafe fn gc_pointer_before_cloning(mut ptr: NonNull<GcHeader>) {
    let header = ptr.as_mut();
    header.inc_rc();
}

unsafe fn gc_pointer_before_dropping(mut ptr: NonNull<GcHeader>) {
    let header = ptr.as_mut();
    let decreased_to_zero = header.dec_rc();
    if decreased_to_zero {
        // Free this pointer
        mi_free(ptr.as_ptr() as *mut _);
    }
}

/// An untyped GC pointer.
///
/// This type should have the same layout as [`Gc`].
#[repr(transparent)]
pub struct RawGcPtr(NonNull<UnsafeCell<GcHeader>>);

impl RawGcPtr {
    /// Cast this opaque pointer into the desired type.
    ///
    /// # Safety
    ///
    /// The pointer must be pointing to the given type.
    pub unsafe fn cast<T>(self) -> Gc<T> {
        std::mem::transmute(self)
    }

    fn header(&self) -> &GcHeader {
        unsafe { &*self.0.as_ref().get() }
    }

    unsafe fn header_mut(&mut self) -> &mut GcHeader {
        unsafe { &mut *self.0.as_mut().get() }
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
        val.as_raw_ptr().clone()
    }
}

impl<T> AsRef<RawGcPtr> for Gc<T> {
    fn as_ref(&self) -> &RawGcPtr {
        self.as_raw_ptr()
    }
}

impl Clone for RawGcPtr {
    fn clone(&self) -> Self {
        let mut ptr = self.0;
        unsafe { &mut *ptr.as_mut().get() }.inc_rc();
        Self(ptr)
    }
}

impl Drop for RawGcPtr {
    fn drop(&mut self) {
        unsafe { &mut *self.0.as_mut().get() }.dec_rc();
    }
}

impl AsCmpPtr for RawGcPtr {
    fn as_cmp_ptr(&self) -> *const () {
        self.0.as_ptr() as *const ()
    }
}

impl Trace for RawGcPtr {
    fn trace(&self, tracer: VRefMut<GcTracerVTable>) {
        unsafe {
            let header = self.header();
            let trace_vtable_ptr = header.trace_impl;
            let trace_vtable = &*header.trace_impl;
            (trace_vtable.trace)(
                VRef::from_raw(
                    NonNull::new_unchecked(trace_vtable_ptr as *mut _),
                    self.value(),
                ),
                tracer,
            );
        }
    }
}

// Assert layout
static_assertions::assert_eq_align!(RawGcPtr, Gc<()>);
static_assertions::assert_eq_size!(RawGcPtr, Gc<()>);
static_assertions::assert_eq_size!(RawGcPtr, *mut ());

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
    rc: Cell<u16>,
    trace_impl: *const TraceVTable,
}

impl GcHeader {
    pub const fn rc_max() -> u16 {
        u16::MAX - 1
    }

    /// Increase reference count.
    pub fn inc_rc(&mut self) {
        let rc = self.rc.get();
        if rc < Self::rc_max() - 1 {
            self.rc.set(rc + 1)
        }
    }

    /// Decrease reference count. If reference count is zero after decrement,
    /// return `true`.
    pub fn dec_rc(&mut self) -> bool {
        let rc = self.rc.get();
        debug_assert!(rc > 0, "reference count was decreased below 0");
        if rc < Self::rc_max() - 1 {
            self.rc.set(rc - 1);
        }
        rc == 1
    }

    pub unsafe fn as_value<T>(&self) -> &GcValue<T> {
        std::mem::transmute(self)
    }

    pub unsafe fn as_value_mut<T>(&mut self) -> &mut GcValue<T> {
        std::mem::transmute(self)
    }
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
        unsafe { Some(alloc.persistent_handles.get(self.0)?.clone().cast()) }
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
