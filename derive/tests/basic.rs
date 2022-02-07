#![allow(dead_code)]

use bytemuck::{Contiguous, Pod, TransparentWrapper, Zeroable, NoPadding, MaybePod};
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
#[derive(Debug, Clone, Copy, NoPadding, MaybePod, PartialEq, Eq)]
enum MaybePodEnumWithValues {
    A = 0,
    B = 1,
    C = 2,
    D = 3,
    E = 4,
}

#[repr(i8)]
#[derive(Clone, Copy, NoPadding, MaybePod)]
enum MaybePodEnumWithImplicitValues {
    A = -10,
    B,
    C,
    D,
    E,
}

#[repr(u8)]
#[derive(Debug, Clone, Copy, NoPadding, MaybePod, PartialEq, Eq)]
enum MaybePodEnumNonContiguous {
    A = 1,
    B = 8,
    C = 2,
    D = 3,
    E = 56,
}

#[derive(Debug, Copy, Clone, NoPadding, MaybePod, PartialEq, Eq)]
#[repr(C)]
struct MaybePodStruct {
    a: u8,
    b: MaybePodEnumNonContiguous,
}

#[test]
fn fails_cast_contiguous() {
    let can_cast = MaybePodEnumWithValues::cast_is_valid(&24);
    assert!(!can_cast);
}

#[test]
fn passes_cast_contiguous() {
    let res = bytemuck::try_from_bytes::<MaybePodEnumWithValues>(&[2u8]).unwrap();
    assert_eq!(*res, MaybePodEnumWithValues::C);
}

#[test]
fn fails_cast_noncontiguous() {
    let can_cast = MaybePodEnumNonContiguous::cast_is_valid(&24);
    assert!(!can_cast);
}

#[test]
fn passes_cast_noncontiguous() {
    let res = bytemuck::try_from_bytes::<MaybePodEnumNonContiguous>(&[56u8]).unwrap();
    assert_eq!(*res, MaybePodEnumNonContiguous::E);
}

#[test]
fn fails_cast_struct() {
    let pod = [0u8, 24u8];
    let res = bytemuck::try_from_bytes::<MaybePodStruct>(&pod);
    assert!(res.is_err());
}

#[test]
fn passes_cast_struct() {
    let pod = [0u8, 8u8];
    let res = bytemuck::try_from_bytes::<MaybePodStruct>(&pod).unwrap();
    assert_eq!(*res, MaybePodStruct {
        a: 0,
        b: MaybePodEnumNonContiguous::B,
    });
}
