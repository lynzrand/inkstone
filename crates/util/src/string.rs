use std::alloc::Layout;
use std::fmt::{Debug, Display};
use std::hash::{BuildHasher, Hash, Hasher};
use std::num::NonZeroU64;
use std::ops::Deref;
use std::ptr::NonNull;
use std::sync::atomic::{AtomicU64, AtomicUsize};

/// An atomically-reference-counted immutable string, with pre-hashed hash value stored in
/// its header.
///
/// Type parameter `H` is the custom header type, which will be placed at the **start** of the heap
/// allocated value. This header **will not** be used in any implementation method in this type,
/// including [`PartialEq`], [`PartialOrd`], [`Hash`]. The header is also immutable (because it
/// can be seen as inside an [`Arc`])
///
/// # Layout
///
/// This type is a single pointer. The heap-allocated part of this type has the following layout:
///
/// ```nodoctest
/// #[repr(C)]
/// struct ArcStrHeap<H> {
///     custom_header: H,
///     strong_count: usize,
///     hash: u64,
///     len: u32,
///     value: [u8; self.len],
/// }
/// ```
pub struct ArcStr<H = ()>(NonNull<ArcStrHeader<H>>);

#[repr(C)]
struct ArcStrHeader<H = ()> {
    custom_header: H,
    rc: AtomicUsize,
    hash: AtomicU64,
    len: u32,
}

impl ArcStr<()> {
    /// Create a new ArcStr.
    ///
    /// # Panics
    ///
    /// Panics if the `s.len() > u32::MAX`.
    pub fn new(s: &str) -> ArcStr<()> {
        Self::with_header(s, ())
    }
}

impl<H> ArcStr<H> {
    /// Create a new ArcStr.
    ///
    /// # Panics
    ///
    /// Panics if the `s.len() > u32::MAX`.
    pub fn with_header(s: &str, header: H) -> ArcStr<H> {
        if s.len() > u32::MAX as usize {
            panic!("ArcStr should not be hold more than u32::MAX bytes")
        }
        unsafe {
            let p = std::alloc::alloc(calc_layout::<H>(s.len()));
            let str_start = p.add(std::mem::size_of::<ArcStrHeader<H>>());
            let p = p as *mut ArcStrHeader<H>;
            p.write(ArcStrHeader::<H> {
                custom_header: header,
                rc: 1.into(),
                hash: 0.into(),
                len: s.len() as u32,
            });
            str_start.copy_from_nonoverlapping(s.as_ptr(), s.len());
            Self(NonNull::new_unchecked(p))
        }
    }
    /// Create a new [`ArcStr`] from a raw pointer to its start.
    ///
    /// # Safety
    ///
    /// This pointer must be previously created from [`Self::into_raw`].
    pub unsafe fn from_raw(p: *mut u8) -> Self {
        ArcStr(NonNull::new_unchecked(p).cast())
    }

    /// Get a raw pointer representing this type.
    ///
    /// # Safety
    ///
    /// The pointer must be later recreated with [`Self::from_raw`] once and only once, or else
    /// either the memory will leak, or the memory will be accessed after free.
    pub fn into_raw(self) -> *mut u8 {
        self.0.cast().as_ptr()
    }

    /// Get the custom header
    pub fn custom_header(&self) -> &H {
        &self.header().custom_header
    }

    fn header(&self) -> &ArcStrHeader<H> {
        unsafe { self.0.as_ref() }
    }

    pub fn ptr_eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }

    pub fn str_start(&self) -> *const u8 {
        let as_u8ptr = self.0.as_ptr() as *const u8;
        unsafe { as_u8ptr.add(std::mem::size_of::<ArcStrHeader>()) }
    }

    /// Rehash this string with the given hasher builder.
    ///
    /// # Safety
    ///
    /// The hash value of a string should not change over time. One should either never hash this
    /// string at all, or set this hash once and use a pass-through hasher forever.
    pub unsafe fn set_hash(&self, build: &impl BuildHasher) {
        let s = self.deref();

        let mut hasher = build.build_hasher();
        s.hash(&mut hasher);

        let mut hash = hasher.finish();

        // 0 is a reserved value, so we must not use it
        if hash == 0 {
            hash = !hash;
        }
        self.header()
            .hash
            .store(hash, std::sync::atomic::Ordering::Relaxed);
    }

    pub fn get_hash(&self) -> Option<NonZeroU64> {
        NonZeroU64::new(
            self.header()
                .hash
                .load(std::sync::atomic::Ordering::Relaxed),
        )
    }
}

fn calc_layout<H>(string_len: usize) -> Layout {
    Layout::new::<ArcStrHeader<H>>()
        .extend(Layout::array::<u8>(string_len).unwrap())
        .unwrap()
        .0
}

impl<H> Clone for ArcStr<H> {
    fn clone(&self) -> Self {
        self.header()
            .rc
            .fetch_add(1, std::sync::atomic::Ordering::AcqRel);
        Self(self.0)
    }
}

impl<H> Drop for ArcStr<H> {
    fn drop(&mut self) {
        let rc = self
            .header()
            .rc
            .fetch_sub(1, std::sync::atomic::Ordering::AcqRel);
        if rc == 0 {
            unsafe {
                self.0.as_ptr().drop_in_place();
                std::alloc::dealloc(self.0.as_ptr() as *mut u8, calc_layout::<H>(self.len()))
            }
        }
    }
}

impl<Head> Hash for ArcStr<Head> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        // load the stored hash. This method is best coupled with a pass-through hasher.
        match self.get_hash() {
            Some(h) => state.write_u64(h.get()),
            None => self.deref().hash(state),
        }
    }
}

impl<H> Deref for ArcStr<H> {
    type Target = str;

    fn deref(&self) -> &Self::Target {
        unsafe {
            let header = self.0.as_ref();
            let str_start = self.str_start();
            std::str::from_utf8_unchecked(std::slice::from_raw_parts(
                str_start,
                header.len as usize,
            ))
        }
    }
}

impl<H> PartialEq for ArcStr<H> {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0 || (self.deref() == other.deref())
    }
}

impl<H> PartialEq<&str> for ArcStr<H> {
    fn eq(&self, other: &&str) -> bool {
        self.deref().eq(*other)
    }
}

impl<H> PartialEq<ArcStr<H>> for &str {
    fn eq(&self, other: &ArcStr<H>) -> bool {
        self.eq(&other.deref())
    }
}

impl<H> Eq for ArcStr<H> {}

impl<H> PartialOrd for ArcStr<H> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.deref().partial_cmp(other.deref())
    }
}

impl<H> PartialOrd<&str> for ArcStr<H> {
    fn partial_cmp(&self, other: &&str) -> Option<std::cmp::Ordering> {
        self.deref().partial_cmp(*other)
    }
}

impl<H> PartialOrd<ArcStr<H>> for &str {
    fn partial_cmp(&self, other: &ArcStr<H>) -> Option<std::cmp::Ordering> {
        self.partial_cmp(&other.deref())
    }
}

impl<H: Default> From<&str> for ArcStr<H> {
    fn from(s: &str) -> Self {
        ArcStr::with_header(s, H::default())
    }
}

impl<H> Debug for ArcStr<H> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Debug::fmt(self.deref(), f)
    }
}

impl<H> Display for ArcStr<H> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Display::fmt(self.deref(), f)
    }
}

#[cfg(test)]
mod test {
    use std::collections::hash_map::DefaultHasher;
    use std::hash::{BuildHasher, Hash, Hasher};

    use super::ArcStr;

    #[test]
    fn test_from_str() {
        let s = "The quick brown fox jumps over a lazy dog.";
        let arcstr = ArcStr::new(s);
        assert_eq!(s, &*arcstr);
        assert_eq!(s, arcstr);
        assert_eq!(arcstr, s);
    }

    #[test]
    fn test_clone() {
        let s = "The quick brown fox jumps over a lazy dog.";
        let arcstr = ArcStr::new(s);
        let arcstr2 = arcstr.clone();

        assert_eq!(arcstr, arcstr2);
        assert_eq!(s, arcstr2);
        assert!(arcstr.ptr_eq(&arcstr2));
    }

    #[test]
    fn test_hash() {
        let s = "The quick brown fox jumps over a lazy dog.";
        let arcstr = ArcStr::new(s);

        let hash_builder = std::hash::BuildHasherDefault::<DefaultHasher>::default();
        assert_eq!(arcstr.get_hash(), None);

        let mut hasher0 = hash_builder.build_hasher();
        arcstr.hash(&mut hasher0);
        let hash0 = hasher0.finish();
        let hash0 = if hash0 == 0 { !hash0 } else { hash0 };

        let mut hasher1 = hash_builder.build_hasher();
        s.hash(&mut hasher1);
        let hash1 = hasher1.finish();
        let hash1 = if hash1 == 0 { !hash1 } else { hash1 };

        unsafe {
            arcstr.set_hash(&hash_builder);
        }
        let hash2 = arcstr.get_hash().expect("The hash should exist").get();

        assert_eq!(
            hash0, hash1,
            "The hash should be the same as hashing its contents, before setting the hash"
        );
        assert_eq!(
            hash1, hash2,
            "The hash should be the same as hashing its contents, after setting the hash"
        );
    }

    #[test]
    fn test_ord() {
        let as1 = ArcStr::new("000001");
        let as2 = ArcStr::new("000002");
        let as3 = ArcStr::new("000003");
        let as2_ = ArcStr::new("000002");

        assert!(as1 < as2);
        assert!(as3 > as2);
        assert_eq!(as2, as2_);
    }

    #[test]
    fn test_header() {
        let s = "The quick brown fox jumps over a lazy dog.";
        let as1 = ArcStr::with_header(s, 123456);
        let as2 = ArcStr::with_header(s, 789012);

        assert_eq!(*as1.custom_header(), 123456);
        assert_eq!(*as2.custom_header(), 789012);

        assert_eq!(as1, as2, "Header should not affect equality comparison");

        let hash_builder = std::hash::BuildHasherDefault::<DefaultHasher>::default();
        unsafe {
            as1.set_hash(&hash_builder);
            as2.set_hash(&hash_builder);
        }
        assert_eq!(
            as1.get_hash(),
            as2.get_hash(),
            "Header should not affect hashing"
        );
    }
}
