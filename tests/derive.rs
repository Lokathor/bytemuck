#![cfg(feature = "derive")]
#![allow(dead_code)]

use bytemuck::{Pod, TransparentWrapper, Zeroable};

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
  b: (),
}

#[derive(TransparentWrapper)]
#[repr(transparent)]
struct TransparentWithGeneric<T> {
  a: T,
}

/// Ensuring that no additional bounds are emitted.
/// See https://github.com/Lokathor/bytemuck/issues/145
fn test_generic<T>(x: T) -> TransparentWithGeneric<T> {
  TransparentWithGeneric::wrap(x)
}

#[derive(TransparentWrapper)]
#[repr(transparent)]
#[transparent(T)]
struct TransparentWithGenericAndZeroSized<T> {
  a: T,
  b: ()
}

/// Ensuring that no additional bounds are emitted.
/// See https://github.com/Lokathor/bytemuck/issues/145
fn test_generic_with_zst<T>(x: T) -> TransparentWithGenericAndZeroSized<T> {
  TransparentWithGenericAndZeroSized::wrap(x)
}
