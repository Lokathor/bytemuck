#![allow(dead_code)]

use bytemuck::{
  Contiguous, CheckedBitPattern, NoPadding, Pod, TransparentWrapper, Zeroable, AnyBitPattern,
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

#[derive(Copy, Clone, NoPadding)]
#[repr(C)]
union UnionTestNoPadding {
  a: NoPaddingTest,
  b: Test,
}

#[derive(Copy, Clone, AnyBitPattern)]
#[repr(C)]
union UnionTestAnyBitPattern {
  a: u8,
  b: u16,
}

#[repr(u8)]
#[derive(Debug, Clone, Copy, NoPadding, CheckedBitPattern, PartialEq, Eq)]
enum CheckedBitPatternEnumWithValues {
  A = 0,
  B = 1,
  C = 2,
  D = 3,
  E = 4,
}

#[repr(i8)]
#[derive(Clone, Copy, NoPadding, CheckedBitPattern)]
enum CheckedBitPatternEnumWithImplicitValues {
  A = -10,
  B,
  C,
  D,
  E,
}

#[repr(u8)]
#[derive(Debug, Clone, Copy, NoPadding, CheckedBitPattern, PartialEq, Eq)]
enum CheckedBitPatternEnumNonContiguous {
  A = 1,
  B = 8,
  C = 2,
  D = 3,
  E = 56,
}

#[derive(Debug, Copy, Clone, NoPadding, CheckedBitPattern, PartialEq, Eq)]
#[repr(C)]
struct CheckedBitPatternStruct {
  a: u8,
  b: CheckedBitPatternEnumNonContiguous,
}

#[derive(Debug, Copy, Clone, AnyBitPattern, PartialEq, Eq)]
#[repr(C)]
struct AnyBitPatternTest {
  a: u16,
  b: u16
}

#[test]
fn fails_cast_contiguous() {
  let can_cast = CheckedBitPatternEnumWithValues::is_valid_bit_pattern(&5);
  assert!(!can_cast);
}

#[test]
fn passes_cast_contiguous() {
  let res = bytemuck::checked::from_bytes::<CheckedBitPatternEnumWithValues>(&[2u8]);
  assert_eq!(*res, CheckedBitPatternEnumWithValues::C);
}

#[test]
fn fails_cast_noncontiguous() {
  let can_cast = CheckedBitPatternEnumNonContiguous::is_valid_bit_pattern(&4);
  assert!(!can_cast);
}

#[test]
fn passes_cast_noncontiguous() {
  let res =
    bytemuck::checked::from_bytes::<CheckedBitPatternEnumNonContiguous>(&[56u8]);
  assert_eq!(*res, CheckedBitPatternEnumNonContiguous::E);
}

#[test]
fn fails_cast_struct() {
  let pod = [0u8, 24u8];
  let res = bytemuck::checked::try_from_bytes::<CheckedBitPatternStruct>(&pod);
  assert!(res.is_err());
}

#[test]
fn passes_cast_struct() {
  let pod = [0u8, 8u8];
  let res = bytemuck::checked::from_bytes::<CheckedBitPatternStruct>(&pod);
  assert_eq!(*res, CheckedBitPatternStruct { a: 0, b: CheckedBitPatternEnumNonContiguous::B });
}

#[test]
fn anybitpattern_implies_zeroable() {
  let test = AnyBitPatternTest::zeroed();
  assert_eq!(test, AnyBitPatternTest { a: 0, b: 0 });
}
