use std::fmt::Debug;
use std::hash::Hash;
use std::sync::Arc;

#[repr(transparent)]
pub struct ArcByPtr<T: ?Sized>(Arc<T>);

impl<T: ?Sized> From<Arc<T>> for ArcByPtr<T> {
    fn from(a: Arc<T>) -> Self {
        Self::new_arc(a)
    }
}

impl<T: ?Sized> From<ArcByPtr<T>> for Arc<T> {
    fn from(val: ArcByPtr<T>) -> Self {
        val.0
    }
}

impl<T: ?Sized> Clone for ArcByPtr<T> {
    fn clone(&self) -> Self {
        Self(self.0.clone())
    }
}

impl<T: ?Sized> Hash for ArcByPtr<T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        Arc::as_ptr(&self.0).hash(state);
    }
}

impl<T: ?Sized + Debug> Debug for ArcByPtr<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::result::Result<(), std::fmt::Error> {
        self.0.fmt(f)
    }
}

impl<T: Sized> ArcByPtr<T> {
    pub fn new(t: T) -> Self {
        Self::new_arc(Arc::new(t))
    }
}

impl<T: ?Sized> ArcByPtr<T> {
    pub fn new_arc(t: Arc<T>) -> Self {
        ArcByPtr(t)
    }

    pub fn unwrap(self) -> Arc<T> {
        self.0
    }
}

impl<T: ?Sized> std::ops::Deref for ArcByPtr<T> {
    type Target = Arc<T>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<T: ?Sized> std::ops::DerefMut for ArcByPtr<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl<T: ?Sized> PartialEq<ArcByPtr<T>> for ArcByPtr<T> {
    fn eq(&self, other: &ArcByPtr<T>) -> bool {
        Arc::ptr_eq(&self.0, &other.0)
    }
}
impl<T: ?Sized> Eq for ArcByPtr<T> {}
