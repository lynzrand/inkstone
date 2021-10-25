use std::mem::size_of;

use super::alloc::GcAllocator;
use super::Gc;
use super::GcTracerVTable;
use super::Trace;
use super::TraceVTable;

use crate::*;

struct SimpleTestStruct {
    _foo: u64,
    _bar: u64,
}

impl Trace for SimpleTestStruct {
    fn trace(&self, _tracer: vtable::VRefMut<super::GcTracerVTable>) {
        // Since `SimpleTestStruct` doesn't have a reference field, this method
        // is a no-op.
    }
}

#[allow(non_upper_case_globals)]
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

struct CycleReferenceStruct {
    val: i64,
    lhs: Option<Gc<CycleReferenceStruct>>,
    rhs: Option<Gc<CycleReferenceStruct>>,
}
#[allow(non_upper_case_globals)]
TraceVTable_static!(static CycleReferenceStruct_VTable for CycleReferenceStruct);

impl Trace for CycleReferenceStruct {
    fn trace(&self, mut tracer: vtable::VRefMut<GcTracerVTable>) {
        if let Some(lhs) = &self.lhs {
            tracer.trace(lhs.as_raw_ptr());
        }
        if let Some(rhs) = &self.rhs {
            tracer.trace(rhs.as_raw_ptr());
        }
    }
}

fn test_tracer_cycle_reference() {
    let mut alloc = GcAllocator::new().expect("Failed to initialize allocator");

    let p1 = Gc::new(
        CycleReferenceStruct {
            val: 1,
            lhs: None,
            rhs: None,
        },
        &mut alloc,
    )
    .expect("Failed to alloc");

    let p3 = Gc::new(
        CycleReferenceStruct {
            val: 2,
            lhs: None,
            rhs: None,
        },
        &mut alloc,
    )
    .expect("Failed to alloc");

    let p2 = Gc::new(
        CycleReferenceStruct {
            val: 3,
            lhs: Some(p3.clone()),
            rhs: Some(p1.clone()),
        },
        &mut alloc,
    )
    .expect("Failed to alloc");

    unsafe { p3.get_mut() }.rhs = Some(p2.clone());

    drop(p2);
    drop(p3);

    alloc.trigger_gc();

    assert_eq!(
        alloc.total_mem_allocated(),
        size_of::<CycleReferenceStruct>()
    );

    drop(p1);
}
