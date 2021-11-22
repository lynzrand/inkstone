use std::alloc::Layout;
use std::hash::{BuildHasher, Hash, Hasher};
use std::num::NonZeroU64;
use std::ops::Deref;
use std::ptr::NonNull;
use std::sync::atomic::{AtomicU64, AtomicUsize};

pub struct ArcStr(NonNull<ArcStrHeader>);

struct ArcStrHeader {
    rc: AtomicUsize,
    hash: AtomicU64,
    len: u32,
}

impl ArcStr {
    pub fn new(s: &str) -> ArcStr {
        unsafe {
            let p = std::alloc::alloc(
                Layout::new::<ArcStrHeader>()
                    .extend(Layout::array::<u8>(s.len()).unwrap())
                    .unwrap()
                    .0,
            );
            let str_start = p.add(std::mem::size_of::<ArcStrHeader>());
            let p = p as *mut ArcStrHeader;
            p.write(ArcStrHeader {
                rc: 1.into(),
                hash: 0.into(),
                len: s.len() as u32,
            });
            str_start.copy_from_nonoverlapping(s.as_ptr(), s.len());
            ArcStr(NonNull::new_unchecked(p))
        }
    }

    fn header(&self) -> &ArcStrHeader {
        unsafe { self.0.as_ref() }
    }

    unsafe fn str_start(&self) -> *const u8 {
        let as_u8ptr = self.0.as_ptr() as *const u8;
        as_u8ptr.add(std::mem::size_of::<ArcStrHeader>())
    }

    /// Update the hash of this string to the given value.
    ///
    /// # Safety
    ///
    /// This method assumes it's only hashed by one type of hasher.
    pub unsafe fn update_hash(&self, build: &impl BuildHasher) {
        if self
            .header()
            .hash
            .load(std::sync::atomic::Ordering::Relaxed)
            != 0
        {
            // it's already hashed.
            return;
        }

        let s = &**self;

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

impl Hash for ArcStr {
    fn hash<H: Hasher>(&self, state: &mut H) {
        // load the stored hash. This method is best coupled with a pass-through hasher.
        let v = self
            .header()
            .hash
            .load(std::sync::atomic::Ordering::Relaxed);
        state.write_u64(v);
    }
}

impl Deref for ArcStr {
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
