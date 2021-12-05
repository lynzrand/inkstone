use vtable::{VRef, VRefMut};

use crate::gc::{GcTracerVTable, Trace};
use crate::TraceVTable;

use super::{TupleHeader, Val};

#[allow(non_upper_case_globals)]
pub static Tuple_TraceVTable: TraceVTable = TraceVTable {
    trace,
    drop_in_place,
    dealloc,
};

extern "C" fn trace(this: VRef<TraceVTable>, mut tracer: VRefMut<GcTracerVTable>) {
    let header_ptr = this.as_ptr() as *const TupleHeader;
    let arity = unsafe { (*header_ptr).arity };
    unsafe {
        let body_start = this.as_ptr().add(std::mem::size_of::<TupleHeader>()) as *const Val;
        for i in 0..arity {
            let i = i as usize;
            let val = body_start.add(i);
            let val = val.as_ref().unwrap();
            val.trace(tracer.borrow_mut())
        }
    }
}

extern "C" fn drop_in_place(this: VRefMut<TraceVTable>) -> vtable::Layout {
    let header_ptr = this.as_ptr() as *const TupleHeader;
    let arity = unsafe { (*header_ptr).arity };
    unsafe {
        let body_start = this.as_ptr().add(std::mem::size_of::<TupleHeader>()) as *mut Val;
        for i in 0..arity {
            let i = i as usize;
            let val = body_start.add(i);
            val.drop_in_place();
        }
    }
    vtable::Layout {
        size: std::mem::size_of::<TupleHeader>() + (arity as usize) * std::mem::size_of::<Val>(),
        align: std::cmp::max(
            std::mem::align_of::<TupleHeader>(),
            std::mem::align_of::<Val>(),
        ),
    }
}

extern "C" fn dealloc(_this: &TraceVTable, _ptr: *mut u8, _layout: vtable::Layout) {
    panic!("This function is never used, only to make vtable::vtable happy.")
}
