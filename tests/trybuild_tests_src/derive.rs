use bytemuck::{Zeroable, Pod, TransparentWrapper, Contiguous};

#[derive(Contiguous, Copy, Clone)]
#[repr(u8)]
enum ContEnum {
    A = 11,
    B = 12,
    C = 13,
}

#[derive(Copy, Clone, Pod, Zeroable)]
#[repr(C)]
struct Test {
    a: u16,
    b: u16,
}

#[derive(TransparentWrapper)]
#[repr(transparent)]
struct TransparentSingle {
    a: u16,
}

#[derive(TransparentWrapper)]
#[repr(transparent)]
#[transparent(u16)]
struct TransparentWithZeroSized {
    a: u16,
    b: ()
}

fn main() {}
