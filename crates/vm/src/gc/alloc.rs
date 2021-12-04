use std::alloc::Layout;
use std::ptr::NonNull;

use mimalloc_rust_sys::heap::{mi_heap_malloc_aligned, mi_heap_new};
use mimalloc_rust_sys::types::mi_heap_t;
use slotmap::HopSlotMap;
use vtable::HasStaticVTable;

use super::{GcHeader, GcTracer, GcValue, RawGcPtr, Trace, TraceVTable};

slotmap::new_key_type! {pub struct RootSetHandle;}

/// A simple mark-and-sweep garbage collected allocator. All contents must implement the
/// Trace interface.
pub struct GcAllocator {
    /// Total memory allocated using this GC-ed allocator.
    ///
    /// This value doesn't count RC-triggered deallocations (because we can't track them). Instead,
    /// we just recalculate this value after each GC.
    total_mem_allocated: usize,

    /// The threshold for triggering a GC. This value should be recalculated as
    /// `1.5 * total_mem_allocated` after each GC.
    gc_threshold: usize,

    /// A series of handles that are considered as part of the root set.
    pub(crate) persistent_handles: HopSlotMap<RootSetHandle, RawGcPtr>,

    mi_heap: *mut mi_heap_t,
}

pub struct AllocError;

impl std::fmt::Debug for AllocError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("AllocError").finish()
    }
}

impl std::fmt::Display for AllocError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("Failed to allocate memory")
    }
}

/// Initial GC threshold. 100KiB.
const INIT_GC_THRESHOLD: usize = 100 * 1024;

impl GcAllocator {
    pub fn new() -> Result<GcAllocator, AllocError> {
        unsafe {
            Ok(GcAllocator {
                total_mem_allocated: 0,
                gc_threshold: INIT_GC_THRESHOLD,

                persistent_handles: HopSlotMap::with_key(),

                mi_heap: mi_heap_new(),
            })
        }
    }

    /// Allocate some memory for `T`
    pub fn alloc<T: Trace + HasStaticVTable<TraceVTable>>(
        &mut self,
    ) -> Option<NonNull<GcValue<T>>> {
        let trace_vtable = T::static_vtable();
        unsafe {
            self.alloc_raw(std::alloc::Layout::new::<T>(), trace_vtable)
                .map(|ptr| ptr.0.cast())
        }
    }

    /// Allocate a memory region with the given layout and [`Trace`] implementation.
    ///
    /// # Panics
    ///
    /// This function currently requires the `layout` to have a size less than
    /// `u32::MAX` and alignment less then 16 bytes.
    ///
    /// # Safety
    ///
    /// The layout and [`Trace`] implementation must match when allocating
    /// memory.
    pub unsafe fn alloc_raw(
        &mut self,
        layout: Layout,
        vtable: *const TraceVTable,
    ) -> Option<RawGcPtr> {
        // assert T is not more than 16 bytes in alloc
        assert!(
            layout.align() <= 16,
            "Allocated type should not have more than 16-byte alignment"
        );
        assert!(
            layout.size() <= u32::MAX as usize,
            "Allocated type should not have more 4GB in size"
        );

        let header_layout = std::alloc::Layout::new::<GcHeader>().pad_to_align();
        let orig_layout = layout.align_to(16).unwrap().pad_to_align();
        let (layout, _) = header_layout.extend(orig_layout).ok()?;

        debug_assert_eq!(
            layout.size() % 16,
            0,
            "The final allocated chunk must be 16-byte aligned"
        );

        let mut gc_triggered = false;

        // trigger GC when we have allocated too many memory.
        if self.total_mem_allocated + orig_layout.size() >= self.gc_threshold {
            self.trigger_gc();
            gc_triggered = true;
        }

        let new_ptr = mi_heap_malloc_aligned(self.mi_heap, layout.size(), layout.align());
        if new_ptr.is_null() {
            if !gc_triggered {
                self.trigger_gc()
            } else {
                // We cannot allocate more because memory has been exhausted.
                return None;
            }
        }

        // Now that the new vacant entry is set, we can reuse the space of
        // the original vacant entry as memory we'd like to allocate.

        let new_object = new_ptr as *mut GcHeader;
        new_object.write(GcHeader {
            rc: 1.into(),
            trace_impl: vtable,
        });

        let new_object_body = new_object.add(1) as *mut u8;
        new_object_body.write_bytes(0, orig_layout.size());

        self.total_mem_allocated += orig_layout.size();
        Some(RawGcPtr(NonNull::new_unchecked(new_object).cast()))
    }

    pub fn trigger_gc(&mut self) {
        // TODO: do garbage collection

        // The garbage collector has 3 stages:
        //
        // - Marking stage.    In this stage,
    }

    fn mark(&mut self) {}

    /// Amount of memory currently allocated by this allocator
    pub fn total_mem_allocated(&self) -> usize {
        self.total_mem_allocated
    }
}

struct GcMarkAndSweepTracer {}

impl GcTracer for GcMarkAndSweepTracer {
    fn trace(&mut self, ptr: &RawGcPtr) {
        todo!()
    }
}

struct GcRewriter {}

impl GcTracer for GcRewriter {
    fn trace(&mut self, ptr: &RawGcPtr) {
        todo!()
    }
}
