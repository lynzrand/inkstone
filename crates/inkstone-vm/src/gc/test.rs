use super::alloc::GcAllocator;
use super::GcTracerVTable;
use super::Trace;
use super::TraceVTable;

use crate::*;

struct SimpleTestStruct {
    _foo: u64,
    _bar: u64,
}

impl Trace for SimpleTestStruct {
    fn trace(&self, tracer: vtable::VRefMut<super::GcTracerVTable>) {
        // noop
    }
}

TraceVTable_static!(static SimpleTestStruct_TraceVtable for SimpleTestStruct);

#[test]
fn test_basic_alloc() {
    let mut allocator = GcAllocator::new().expect("Failed to create allocator");
    assert_eq!(
        allocator.total_mem_allocated(),
        0,
        "it should be empty before allocation"
    );

    let _ptr = allocator
        .alloc::<SimpleTestStruct>()
        .expect("Failed to allocate struct");

    assert!(_ptr.as_ptr() as usize % 16 == 0, "pointer is aligned");
    assert_eq!(
        allocator.total_mem_allocated(),
        std::mem::size_of::<SimpleTestStruct>()
    );

    let _ptr2 = allocator
        .alloc::<SimpleTestStruct>()
        .expect("Failed to allocate struct");

    assert!(_ptr2.as_ptr() as usize % 16 == 0, "pointer is aligned");
    assert_eq!(
        allocator.total_mem_allocated(),
        std::mem::size_of::<SimpleTestStruct>() * 2
    );
}

#[test]
fn test_alloc_across_multiple_chunks() {
    let mut allocator = GcAllocator::new().expect("Failed to create allocator");
    assert_eq!(
        allocator.total_mem_allocated(),
        0,
        "it should be empty before allocation"
    );

    for _i in 0..10240 {
        let _ptr = allocator
            .alloc::<SimpleTestStruct>()
            .expect("Failed to allocate struct");

        assert!(_ptr.as_ptr() as usize % 16 == 0, "pointer is aligned");
    }
}
