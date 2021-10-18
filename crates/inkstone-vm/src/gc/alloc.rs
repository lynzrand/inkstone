use std::alloc::Layout;
use std::ptr::NonNull;

use slotmap::HopSlotMap;
use vtable::HasStaticVTable;

use crate::gc::Gc;

use super::{GcHeader, GcHeaderFlags, GcTracer, GcValue, RawGcPtr, Trace, TraceVTable};

slotmap::new_key_type! {pub struct RootSetHandle;}

/// A simple mark-and-sweep garbage collected allocator. Requires its contents
/// to
pub struct GcAllocator {
    chunk_size: usize,
    chunks: Option<NonNull<ChunkHeader>>,

    /// A series of handles that are considered as part of the root set.
    persistent_handles: HopSlotMap<RootSetHandle, GcHeader>,
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

const DEFAULT_GC_CHUNK_SIZE: usize = 1024 * 1024;
const DEFAULT_GC_CHUNK_ALIGN: usize = 64;

impl GcAllocator {
    pub fn new() -> Result<GcAllocator, AllocError> {
        unsafe {
            let init_chunk = alloc_new_chunk(None).ok_or(AllocError)?;

            Ok(GcAllocator {
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

        self.alloc_raw(std::alloc::Layout::new::<T>(), trace_vtable)
            .map(|ptr| unsafe { ptr.0.cast() })
    }

    pub fn alloc_raw(
        &mut self,
        orig_layout: Layout,
        vtable: *const TraceVTable,
    ) -> Option<RawGcPtr> {
        // assert T is not more than 16 bytes in alloc
        assert!(
            orig_layout.align() <= 16,
            "Allocated type should not have more than 16-byte alignment"
        );
        assert!(
            orig_layout.size() <= u32::MAX as usize,
            "Allocated type should not have more 4GB in size"
        );
        let header_layout = std::alloc::Layout::new::<GcHeader>().pad_to_align();
        let orig_layout = orig_layout
            .align_to(2 * std::mem::size_of::<usize>())
            .unwrap()
            .pad_to_align();
        let (layout, _) = header_layout.extend(orig_layout).ok()?;

        if layout.size() > DEFAULT_GC_CHUNK_SIZE / 4 {
            unsafe {
                let ptr = std::alloc::alloc_zeroed(layout);
                let ptr = NonNull::new(ptr)?.cast::<GcHeader>();
                ptr.as_ptr().write(GcHeader {
                    flags: GcHeaderFlags::new().with_oversized(true).with_rc(1),
                    size: orig_layout.size() as u32,
                    next_allocation: self.oversize_chain,
                    trace_impl: vtable,
                });

                self.oversize_chain = Some(ptr);

                Some(RawGcPtr(ptr))
            }
        } else {
            unsafe {
                let first_chunk = first_chunk_with_space(self.chunks, layout);
                if first_chunk.is_none() {
                    self.chunks = Some(alloc_new_chunk(self.chunks)?);
                }

                todo!()
            }
        }
    }

    fn mark(&mut self) {}
}

fn first_chunk_with_space(
    chunk_link_start: Option<NonNull<ChunkHeader>>,
    layout: Layout,
) -> Option<NonNull<ChunkHeader>> {
    todo!()
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

impl GcTracer for GcAllocator {
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
