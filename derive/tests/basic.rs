#![allow(dead_code)]

use bytemuck::{
  AnyBitPattern, CheckedBitPattern, Contiguous, NoUninit, Pod,
  TransparentWrapper, Zeroable,
};
use std::marker::{PhantomData, PhantomPinned};

#[derive(Copy, Clone, Pod, Zeroable)]
#[repr(C)]
struct Test {
  a: u16,
  b: u16,
}

#[derive(Pod, Zeroable)]
#[repr(C, packed)]
struct GenericPackedStruct<T: Pod> {
  a: u32,
  b: T,
  c: u32,
}

impl<T: Pod> Clone for GenericPackedStruct<T> {
  fn clone(&self) -> Self {
    *self
  }
}

impl<T: Pod> Copy for GenericPackedStruct<T> {}

#[derive(Pod, Zeroable)]
#[repr(C, packed(1))]
struct GenericPackedStructExplicitPackedAlignment<T: Pod> {
  a: u32,
  b: T,
  c: u32,
}

impl<T: Pod> Clone for GenericPackedStructExplicitPackedAlignment<T> {
  fn clone(&self) -> Self {
    *self
  }
}

impl<T: Pod> Copy for GenericPackedStructExplicitPackedAlignment<T> {}

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

struct MyZst<T>(PhantomData<T>, [u8; 0], PhantomPinned);
unsafe impl<T> Zeroable for MyZst<T> {}

#[derive(TransparentWrapper)]
#[repr(transparent)]
#[transparent(u16)]
struct TransparentTupleWithCustomZeroSized<T>(u16, MyZst<T>);

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

#[derive(Copy, Clone, NoUninit)]
#[repr(C)]
struct NoUninitTest {
  a: u16,
  b: u16,
}

#[derive(Copy, Clone, AnyBitPattern)]
#[repr(C)]
union UnionTestAnyBitPattern {
  a: u8,
  b: u16,
}

#[repr(u8)]
#[derive(Debug, Clone, Copy, NoUninit, CheckedBitPattern, PartialEq, Eq)]
enum CheckedBitPatternEnumWithValues {
  A = 0,
  B = 1,
  C = 2,
  D = 3,
  E = 4,
}

#[repr(i8)]
#[derive(Clone, Copy, NoUninit, CheckedBitPattern)]
enum CheckedBitPatternEnumWithImplicitValues {
  A = -10,
  B,
  C,
  D,
  E,
}

#[repr(u8)]
#[derive(Debug, Clone, Copy, NoUninit, CheckedBitPattern, PartialEq, Eq)]
enum CheckedBitPatternEnumNonContiguous {
  A = 1,
  B = 8,
  C = 2,
  D = 3,
  E = 56,
}

#[repr(u8)]
#[derive(Debug, Clone, Copy, NoUninit, CheckedBitPattern, PartialEq, Eq)]
enum CheckedBitPatternEnumByteLit {
  A = b'A',
  B = b'B',
  C = b'C',
  D = b'D',
  E = b'E',
}

#[derive(Debug, Copy, Clone, NoUninit, CheckedBitPattern, PartialEq, Eq)]
#[repr(C)]
struct CheckedBitPatternStruct {
  a: u8,
  b: CheckedBitPatternEnumNonContiguous,
}

#[derive(Debug, Copy, Clone, AnyBitPattern, PartialEq, Eq)]
#[repr(C)]
struct AnyBitPatternTest<A: AnyBitPattern, B: AnyBitPattern> {
  a: A,
  b: B,
}

#[derive(Clone, Copy, CheckedBitPattern)]
#[repr(C, align(8))]
struct CheckedBitPatternAlignedStruct {
  a: u16,
}

#[derive(Debug, Clone, Copy, CheckedBitPattern, PartialEq, Eq)]
#[repr(C)]
enum CheckedBitPatternCDefaultDiscriminantEnumWithFields {
  A(u64),
  B { c: u64 },
}

#[derive(Debug, Clone, Copy, CheckedBitPattern, PartialEq, Eq)]
#[repr(C, u8)]
enum CheckedBitPatternCEnumWithFields {
  A(u32),
  B { c: u32 },
}

#[derive(Debug, Clone, Copy, CheckedBitPattern, PartialEq, Eq)]
#[repr(u8)]
enum CheckedBitPatternIntEnumWithFields {
  A(u8),
  B { c: u32 },
}

#[derive(Debug, Clone, Copy, CheckedBitPattern, PartialEq, Eq)]
#[repr(transparent)]
enum CheckedBitPatternTransparentEnumWithFields {
  A { b: u32 },
}

/// ```compile_fail
/// use bytemuck::{Pod, Zeroable};
///
/// #[derive(Pod, Zeroable)]
/// #[repr(transparent)]
/// struct TransparentSingle<T>(T);
///
/// struct NotPod(u32);
///
/// let _: u32 = bytemuck::cast(TransparentSingle(NotPod(0u32)));
/// ```
#[derive(
  Debug, Copy, Clone, PartialEq, Eq, Pod, Zeroable, TransparentWrapper,
)]
#[repr(transparent)]
struct NewtypeWrapperTest<T>(T);

#[test]
fn fails_cast_contiguous() {
  let can_cast = CheckedBitPatternEnumWithValues::is_valid_bit_pattern(&5);
  assert!(!can_cast);
}

#[test]
fn passes_cast_contiguous() {
  let res =
    bytemuck::checked::from_bytes::<CheckedBitPatternEnumWithValues>(&[2u8]);
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
    bytemuck::checked::from_bytes::<CheckedBitPatternEnumNonContiguous>(&[
      56u8,
    ]);
  assert_eq!(*res, CheckedBitPatternEnumNonContiguous::E);
}

#[test]
fn fails_cast_bytelit() {
  let can_cast = CheckedBitPatternEnumByteLit::is_valid_bit_pattern(&b'a');
  assert!(!can_cast);
}

#[test]
fn passes_cast_bytelit() {
  let res =
    bytemuck::checked::cast_slice::<u8, CheckedBitPatternEnumByteLit>(b"CAB");
  assert_eq!(
    res,
    [
      CheckedBitPatternEnumByteLit::C,
      CheckedBitPatternEnumByteLit::A,
      CheckedBitPatternEnumByteLit::B
    ]
  );
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
  assert_eq!(
    *res,
    CheckedBitPatternStruct { a: 0, b: CheckedBitPatternEnumNonContiguous::B }
  );
}

#[test]
fn anybitpattern_implies_zeroable() {
  let test = AnyBitPatternTest::<isize, usize>::zeroed();
  assert_eq!(test, AnyBitPatternTest { a: 0isize, b: 0usize });
}

#[test]
fn checkedbitpattern_try_pod_read_unaligned() {
  let pod = [0u8];
  let res = bytemuck::checked::try_pod_read_unaligned::<
    CheckedBitPatternEnumWithValues,
  >(&pod);
  assert!(res.is_ok());

  let pod = [5u8];
  let res = bytemuck::checked::try_pod_read_unaligned::<
    CheckedBitPatternEnumWithValues,
  >(&pod);
  assert!(res.is_err());
}

#[test]
fn checkedbitpattern_aligned_struct() {
  let pod = [0u8; 8];
  bytemuck::checked::pod_read_unaligned::<CheckedBitPatternAlignedStruct>(&pod);
}

#[test]
fn checkedbitpattern_c_default_discriminant_enum_with_fields() {
  let pod = [
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xcc, 0x55, 0x55, 0x55,
    0x55, 0x55, 0x55, 0xcc,
  ];
  let value = bytemuck::checked::pod_read_unaligned::<
    CheckedBitPatternCDefaultDiscriminantEnumWithFields,
  >(&pod);
  assert_eq!(
    value,
    CheckedBitPatternCDefaultDiscriminantEnumWithFields::A(0xcc555555555555cc)
  );

  let pod = [
    0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xcc, 0x55, 0x55, 0x55,
    0x55, 0x55, 0x55, 0xcc,
  ];
  let value = bytemuck::checked::pod_read_unaligned::<
    CheckedBitPatternCDefaultDiscriminantEnumWithFields,
  >(&pod);
  assert_eq!(
    value,
    CheckedBitPatternCDefaultDiscriminantEnumWithFields::B {
      c: 0xcc555555555555cc
    }
  );
}

#[test]
fn checkedbitpattern_c_enum_with_fields() {
  let pod = [0x00, 0x00, 0x00, 0x00, 0xcc, 0x55, 0x55, 0xcc];
  let value = bytemuck::checked::pod_read_unaligned::<
    CheckedBitPatternCEnumWithFields,
  >(&pod);
  assert_eq!(value, CheckedBitPatternCEnumWithFields::A(0xcc5555cc));

  let pod = [0x01, 0x00, 0x00, 0x00, 0xcc, 0x55, 0x55, 0xcc];
  let value = bytemuck::checked::pod_read_unaligned::<
    CheckedBitPatternCEnumWithFields,
  >(&pod);
  assert_eq!(value, CheckedBitPatternCEnumWithFields::B { c: 0xcc5555cc });
}

#[test]
fn checkedbitpattern_int_enum_with_fields() {
  let pod = [0x00, 0x55, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00];
  let value = bytemuck::checked::pod_read_unaligned::<
    CheckedBitPatternIntEnumWithFields,
  >(&pod);
  assert_eq!(value, CheckedBitPatternIntEnumWithFields::A(0x55));

  let pod = [0x01, 0x00, 0x00, 0x00, 0xcc, 0x55, 0x55, 0xcc];
  let value = bytemuck::checked::pod_read_unaligned::<
    CheckedBitPatternIntEnumWithFields,
  >(&pod);
  assert_eq!(value, CheckedBitPatternIntEnumWithFields::B { c: 0xcc5555cc });
}

#[test]
fn checkedbitpattern_transparent_enum_with_fields() {
  let pod = [0xcc, 0x55, 0x55, 0xcc];
  let value = bytemuck::checked::pod_read_unaligned::<
    CheckedBitPatternTransparentEnumWithFields,
  >(&pod);
  assert_eq!(
    value,
    CheckedBitPatternTransparentEnumWithFields::A { b: 0xcc5555cc }
  );
}

#[derive(Copy, Clone, bytemuck::Pod, bytemuck::Zeroable)]
#[repr(C, align(16))]
struct Issue127 {}
