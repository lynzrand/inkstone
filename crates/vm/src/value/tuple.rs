use vtable::{VRef, VRefMut};

use crate::gc::GcTracerVTable;
use crate::TraceVTable;

use super::{TupleHeader, Val};

static Tuple_TraceVTable: TraceVTable = TraceVTable {
    trace,
    drop_in_place,
    dealloc,
};

extern "C" fn trace(this: VRef<TraceVTable>, tracer: VRefMut<GcTracerVTable>) {
    let header_ptr = this.as_ptr() as *const TupleHeader;
    let arity = unsafe { (*header_ptr).arity };
    unsafe {
        let body_start = this.as_ptr().add(std::mem::size_of::<TupleHeader>()) as *const Val;
        for i in 0..arity {
            let i = i as usize;
            let val = body_start.add(i);
            let val = val.as_ref();
            // todo: trace val
        }
    }
}

extern "C" fn drop_in_place(this: VRefMut<TraceVTable>) -> vtable::Layout {
    todo!()
}

extern "C" fn dealloc(this: &TraceVTable, ptr: *mut u8, layout: vtable::Layout) {
    todo!()
}
