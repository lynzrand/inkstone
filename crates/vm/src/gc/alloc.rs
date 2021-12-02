use std::alloc::Layout;
use std::marker::PhantomData;
use std::ops::Range;
use std::ptr::NonNull;

use slotmap::HopSlotMap;
use vtable::HasStaticVTable;

use crate::gc::Gc;

use super::{GcHeader, GcHeaderFlags, GcTracer, GcValue, RawGcPtr, Trace, TraceVTable};

slotmap::new_key_type! {pub struct RootSetHandle;}

/// A simple mark-and-sweep garbage collected allocator. Requires its contents
/// to
pub struct GcAllocator {
    total_mem_allocated: usize,
    gc_threshold: Range<usize>,

    chunk_size: usize,
    chunks: Option<NonNull<ChunkHeader>>,

    /// A series of handles that are considered as part of the root set.
    pub(crate) persistent_handles: HopSlotMap<RootSetHandle, RawGcPtr>,
    /// A chain of objects that are oversized and allocated separately
    oversize_chain: Option<NonNull<GcHeader>>,
}

/// The header of a chunk
#[repr(align(64))]
struct ChunkHeader {
    /// The size of this chunk
    size: usize,
    /// The next chunk in this chain
    next: Option<NonNull<ChunkHeader>>,
    /// The next allocated entry in this chunk
    next_alloc: Option<NonNull<GcHeader>>,
    /// The next vacant entry in this chunk
    next_vacant: Option<NonNull<VacantEntry>>,
}

/// The header of a contiguous part of vacant memory
struct VacantEntry {
    /// The size of this vacant memory
    size: usize,
    /// The next vacant memory in this chunk
    next_vacant: Option<NonNull<VacantEntry>>,
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

const DEFAULT_GC_CHUNK_SIZE: usize = 1024 * 1024;
const DEFAULT_GC_CHUNK_ALIGN: usize = 64;

const INIT_GC_THRESHOLD_UPPER: usize = 100 * 1024;
const INIT_GC_THRESHOLD_LOWER: usize = 50 * 1024;

impl GcAllocator {
    pub fn new() -> Result<GcAllocator, AllocError> {
        unsafe {
            let init_chunk = alloc_new_chunk(None).ok_or(AllocError)?;

            Ok(GcAllocator {
                total_mem_allocated: 0,
                gc_threshold: INIT_GC_THRESHOLD_LOWER..INIT_GC_THRESHOLD_UPPER,

                chunk_size: DEFAULT_GC_CHUNK_SIZE,
                chunks: Some(init_chunk),

                persistent_handles: HopSlotMap::with_key(),
                oversize_chain: None,
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

        if layout.size() > DEFAULT_GC_CHUNK_SIZE / 4 {
            self.alloc_oversized(layout, orig_layout, vtable)
        } else {
            if self.total_mem_allocated + orig_layout.size() >= self.gc_threshold.end {
                self.trigger_gc();
            }

            let mut first_chunk = first_chunk_with_space(self.chunks, layout.size());
            if first_chunk.is_none() {
                let chunk = alloc_new_chunk(self.chunks)?;
                self.chunks = Some(chunk);
                first_chunk = Some((
                    chunk,
                    (*chunk.as_ptr())
                        .next_vacant
                        .expect("Newly allocated chunk must have a vacant entry"),
                ));
            }
            let (mut chunk, vacant_entry) = first_chunk.unwrap();
            let (chunk, vacant_entry) = (chunk.as_mut(), vacant_entry.as_ptr());

            // allocate space from the start of this VacantEntry
            let remaining_vacant_size = (*vacant_entry).size - layout.size();

            // Before allocation, we need to resize this vacant entry
            let new_vacant = if remaining_vacant_size > 0 {
                let new_vacant_entry = VacantEntry {
                    next_vacant: (*vacant_entry).next_vacant,
                    size: remaining_vacant_size,
                };
                let next_vacant_entry_start =
                    (vacant_entry as *mut u8).add(layout.size()) as *mut VacantEntry;
                next_vacant_entry_start.write(new_vacant_entry);
                Some(NonNull::new_unchecked(next_vacant_entry_start))
            } else {
                None
            };

            if chunk
                .next_vacant
                .map_or(false, |v| v.as_ptr() == vacant_entry)
            {
                // this is the head vacant entry
                chunk.next_vacant = new_vacant;
            }

            // Now that the new vacant entry is set, we can reuse the space of
            // the original vacant entry as memory we'd like to allocate.

            let new_object = vacant_entry as *mut GcHeader;
            new_object.write(GcHeader {
                flags: GcHeaderFlags::new().with_oversized(false).with_rc(1),
                size: orig_layout.size() as u32,
                trace_impl: vtable,
            });

            let new_object_body = new_object.add(1) as *mut u8;
            new_object_body.write_bytes(0, orig_layout.size());

            let header = NonNull::new_unchecked(new_object);
            chunk.next_alloc = Some(header);

            self.total_mem_allocated += orig_layout.size();
            Some(RawGcPtr(header.cast()))
        }
    }

    /// Allocate an oversized memory
    unsafe fn alloc_oversized(
        &mut self,
        layout: Layout,
        orig_layout: Layout,
        vtable: *const TraceVTable,
    ) -> Option<RawGcPtr> {
        // allocate a new memory segment
        let ptr = std::alloc::alloc_zeroed(layout);
        let ptr = NonNull::new(ptr)?.cast::<GcHeader>();

        // Write a GC header into it
        ptr.as_ptr().write(GcHeader {
            flags: GcHeaderFlags::new().with_oversized(true).with_rc(1),
            size: orig_layout.size() as u32,
            trace_impl: vtable,
        });
        self.oversize_chain = Some(ptr);
        self.total_mem_allocated += orig_layout.size();
        Some(RawGcPtr(ptr.cast()))
    }

    pub fn trigger_gc(&mut self) {
        // TODO: do garbage collection

        // The garbage collector has 3 stages:
        //
        // - Marking stage.    In this stage,
    }

    fn mark(&mut self) {}

    /// Dealloc the given GC pointer.
    ///
    /// The pointer must be generated from this allocator.
    unsafe fn dealloc(&mut self, pointer: RawGcPtr) {
        let ptr = pointer.0.as_ptr();

        todo!("Deallocate memory");

        if self.total_mem_allocated < self.gc_threshold.start {
            self.trigger_gc();
        }
    }

    /// Amount of memory currently allocated by this allocator
    pub fn total_mem_allocated(&self) -> usize {
        self.total_mem_allocated
    }
}

/// Find the first chunk with a vacant entry that's larger than `len`.
/// Returns the chunk and vacant entry.
unsafe fn first_chunk_with_space(
    chunk_link_start: Option<NonNull<ChunkHeader>>,
    len: usize,
) -> Option<(NonNull<ChunkHeader>, NonNull<VacantEntry>)> {
    let mut cur_chunk = chunk_link_start;
    while let Some(chunk_ptr) = cur_chunk {
        let chunk = chunk_ptr.as_ref();

        let mut cur_vacant = chunk.next_vacant;
        while let Some(vacant_ptr) = cur_vacant {
            let vacant = vacant_ptr.as_ref();
            if vacant.size >= len {
                return Some((chunk_ptr, vacant_ptr));
            }

            cur_vacant = vacant.next_vacant;
        }

        cur_chunk = chunk.next;
    }
    None
}

unsafe fn alloc_new_chunk(next: Option<NonNull<ChunkHeader>>) -> Option<NonNull<ChunkHeader>> {
    let chunk = std::alloc::alloc(
        Layout::from_size_align(DEFAULT_GC_CHUNK_SIZE, DEFAULT_GC_CHUNK_ALIGN).unwrap(),
    ) as *mut ChunkHeader;
    let chunk = NonNull::new(chunk)?;
    let chunk_ptr = chunk.as_ptr();

    let header_size = std::mem::size_of::<ChunkHeader>();
    let vacant_ptr = (chunk_ptr as *mut u8).add(header_size) as *mut VacantEntry;

    let header = ChunkHeader {
        size: DEFAULT_GC_CHUNK_SIZE,
        next,
        next_alloc: None,
        next_vacant: Some(NonNull::new_unchecked(vacant_ptr)),
    };
    let vacant = VacantEntry {
        size: DEFAULT_GC_CHUNK_SIZE - header_size,
        next_vacant: None,
    };

    chunk_ptr.write(header);
    vacant_ptr.write(vacant);

    Some(chunk)
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
