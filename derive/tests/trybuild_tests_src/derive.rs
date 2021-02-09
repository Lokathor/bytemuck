use bytemuck_derive::{Zeroable, Pod, TransparentWrapper, Contiguous};
use std::marker::PhantomData;

#[derive(TransparentWrapper)]
#[repr(transparent)]
struct TransparentSingle {
    a: u16,
}

#[derive(Copy, Clone, Pod, Zeroable)]
#[repr(C)]
struct Test {
    a: u16,
    b: u16,
}

#[derive(Zeroable)]
struct ZeroGeneric<T: bytemuck::Zeroable> {
    a: T,
}

#[derive(TransparentWrapper)]
#[repr(transparent)]
#[transparent(u16)]
struct TransparentWithZeroSized<T> {
    a: u16,
    b: PhantomData<T>,
}

#[repr(u8)]
#[derive(Clone, Copy, Contiguous)]
enum ContiguousWithValues {
    A = 0,
    B = 1,
    C = 2,
    D = 3,
    E = 4,
}

#[repr(i8)]
#[derive(Clone, Copy, Contiguous)]
enum ContiguousWithImplicitValues {
    A = -10,
    B,
    C,
    D,
    E,
}

fn main() {}
