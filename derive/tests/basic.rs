#![allow(dead_code)]

use bytemuck::{
  Contiguous, CheckedCastFromPod, NoPadding, Pod, TransparentWrapper, Zeroable,
};
use std::marker::PhantomData;

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
struct TransparentSingle {
  a: u16,
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

#[derive(Copy, Clone, NoPadding)]
#[repr(C)]
struct NoPaddingTest {
  a: u16,
  b: u16,
}

#[repr(u8)]
#[derive(Debug, Clone, Copy, NoPadding, CheckedCastFromPod, PartialEq, Eq)]
enum CheckedCastFromPodEnumWithValues {
  A = 0,
  B = 1,
  C = 2,
  D = 3,
  E = 4,
}

#[repr(i8)]
#[derive(Clone, Copy, NoPadding, CheckedCastFromPod)]
enum CheckedCastFromPodEnumWithImplicitValues {
  A = -10,
  B,
  C,
  D,
  E,
}

#[repr(u8)]
#[derive(Debug, Clone, Copy, NoPadding, CheckedCastFromPod, PartialEq, Eq)]
enum CheckedCastFromPodEnumNonContiguous {
  A = 1,
  B = 8,
  C = 2,
  D = 3,
  E = 56,
}

#[derive(Debug, Copy, Clone, NoPadding, CheckedCastFromPod, PartialEq, Eq)]
#[repr(C)]
struct CheckedCastFromPodStruct {
  a: u8,
  b: CheckedCastFromPodEnumNonContiguous,
}

#[test]
fn fails_cast_contiguous() {
  let can_cast = CheckedCastFromPodEnumWithValues::cast_is_valid(&5);
  assert!(!can_cast);
}

#[test]
fn passes_cast_contiguous() {
  let res = bytemuck::checked::try_from_bytes::<CheckedCastFromPodEnumWithValues>(&[2u8]).unwrap();
  assert_eq!(*res, CheckedCastFromPodEnumWithValues::C);
}

#[test]
fn fails_cast_noncontiguous() {
  let can_cast = CheckedCastFromPodEnumNonContiguous::cast_is_valid(&4);
  assert!(!can_cast);
}

#[test]
fn passes_cast_noncontiguous() {
  let res =
    bytemuck::checked::try_from_bytes::<CheckedCastFromPodEnumNonContiguous>(&[56u8]).unwrap();
  assert_eq!(*res, CheckedCastFromPodEnumNonContiguous::E);
}

#[test]
fn fails_cast_struct() {
  let pod = [0u8, 24u8];
  let res = bytemuck::checked::try_from_bytes::<CheckedCastFromPodStruct>(&pod);
  assert!(res.is_err());
}

#[test]
fn passes_cast_struct() {
  let pod = [0u8, 8u8];
  let res = bytemuck::checked::try_from_bytes::<CheckedCastFromPodStruct>(&pod).unwrap();
  assert_eq!(*res, CheckedCastFromPodStruct { a: 0, b: CheckedCastFromPodEnumNonContiguous::B });
}
