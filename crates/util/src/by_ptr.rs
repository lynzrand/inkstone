use std::borrow::Borrow;
use std::hash::Hash;
use std::ops::{Deref, DerefMut};
use std::ptr::NonNull;

use crate::string::ArcStr;

/// A trait for types that can be compared directly by pointer for equality
pub trait AsCmpPtr {
    /// Get the pointer that directly represents `self`
    fn as_cmp_ptr(&self) -> *const ();

    /// Compare pointer equality.
    fn ptr_eq(this: &Self, other: impl AsCmpPtr) -> bool {
        this.as_cmp_ptr().eq(&other.as_cmp_ptr())
    }
}

/// A wrapping struct to allow types to be compared by pointer.
///
/// This struct differs from [`ByPtr`] as it encapsulates a reference.
#[repr(transparent)]
pub struct ByPtrRef<'a, T>(&'a T);

impl<'a, T> ByPtrRef<'a, T> {
    pub fn new(t: &'a T) -> Self {
        Self(t)
    }
}

/// A wrapping struct to allow types to be compared by pointer.
#[repr(transparent)]
#[derive(Clone, Copy)]
pub struct ByPtr<T>(T);

impl<T: AsCmpPtr> ByPtr<T> {
    /// Create a new `ByPtr` wrapper
    pub fn new(t: T) -> Self {
        Self(t)
    }

    /// Unwrap this type into its contents
    pub fn unwrap(self) -> T {
        self.0
    }

    /// Transform the inner value into another value
    pub fn map<V: AsCmpPtr>(self, f: impl FnOnce(T) -> V) -> ByPtr<V> {
        ByPtr::new(f(self.unwrap()))
    }

    /// Transform a reference of the inner value into another value
    pub fn with<V: AsCmpPtr>(&self, f: impl FnOnce(&Self) -> V) -> ByPtr<V> {
        ByPtr::new(f(&*self))
    }
}

impl<T> Deref for ByPtr<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<T> DerefMut for ByPtr<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl<T> Borrow<T> for ByPtr<T> {
    fn borrow(&self) -> &T {
        &self.0
    }
}

impl<T: AsCmpPtr> From<T> for ByPtr<T> {
    fn from(t: T) -> Self {
        Self::new(t)
    }
}

impl<T: AsCmpPtr, O: AsCmpPtr> PartialEq<O> for ByPtr<T> {
    fn eq(&self, other: &O) -> bool {
        self.0.as_cmp_ptr() == other.as_cmp_ptr()
    }
}

impl<T: AsCmpPtr> Eq for ByPtr<T> {}

impl<T: AsCmpPtr, O: AsCmpPtr> PartialOrd<O> for ByPtr<T> {
    fn partial_cmp(&self, other: &O) -> Option<std::cmp::Ordering> {
        self.0.as_cmp_ptr().partial_cmp(&other.as_cmp_ptr())
    }
}

impl<T: AsCmpPtr> Ord for ByPtr<T> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.0.as_cmp_ptr().cmp(&other.0.as_cmp_ptr())
    }
}

impl<T: AsCmpPtr> Hash for ByPtr<T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.0.as_cmp_ptr().hash(state);
    }
}

// ====== Implementations =======

impl<T> AsCmpPtr for *const T {
    fn as_cmp_ptr(&self) -> *const () {
        self.cast()
    }
}

impl<T> AsCmpPtr for *mut T {
    fn as_cmp_ptr(&self) -> *const () {
        self.cast() as *const ()
    }
}

impl<T> AsCmpPtr for NonNull<T> {
    fn as_cmp_ptr(&self) -> *const () {
        self.cast().as_ptr() as *const ()
    }
}

impl<T> AsCmpPtr for &T {
    fn as_cmp_ptr(&self) -> *const () {
        *self as *const T as *const ()
    }
}

impl<T> AsCmpPtr for &mut T {
    fn as_cmp_ptr(&self) -> *const () {
        *self as *const T as *const ()
    }
}

impl<T> AsCmpPtr for std::sync::Arc<T> {
    fn as_cmp_ptr(&self) -> *const () {
        std::sync::Arc::as_ptr(self) as *const ()
    }
}

impl<T> AsCmpPtr for std::rc::Rc<T> {
    fn as_cmp_ptr(&self) -> *const () {
        std::rc::Rc::as_ptr(self) as *const ()
    }
}

impl<T: AsCmpPtr> AsCmpPtr for ByPtr<T> {
    fn as_cmp_ptr(&self) -> *const () {
        self.0.as_cmp_ptr()
    }
}

impl<T: AsCmpPtr> AsCmpPtr for ByPtrRef<'_, T> {
    fn as_cmp_ptr(&self) -> *const () {
        self.0.as_cmp_ptr()
    }
}

impl AsCmpPtr for ArcStr {
    fn as_cmp_ptr(&self) -> *const () {
        self.as_ptr() as *const ()
    }
}
