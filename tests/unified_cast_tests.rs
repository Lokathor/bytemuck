#![cfg(feature = "unified_cast")]

use bytemuck::{
  PodCastError::{AlignmentMismatch, OutputSliceWouldHaveSlop, SizeMismatch},
  Reinterpret, ReinterpretInner, TryReinterpret, TryReinterpretInner,
};
#[cfg(feature = "non_null_slice_cast")]
use core::ptr;
use core::{
  convert::identity, num::NonZeroI32, pin::Pin, ptr::NonNull,
  sync::atomic::AtomicPtr,
};

macro_rules! test_assert {
  ($target:expr, $init:expr) => {
    assert_eq!($target, $init);
  };
  ($target:expr, $_init:expr, $e:expr) => {
    assert_eq!($target, $e);
  };
}

// Test both `Reinterpret` and `TryReinterpret`
macro_rules! test_reinterpret {
  (
    $init:expr => $ty:ty $(= $assert_val:expr)?
  ) => {{
    test_assert!(identity::<$ty>($init.reinterpret()), $init $(, $assert_val)?);
    test_assert!(identity::<$ty>($init.try_reinterpret().unwrap()), $init $(, $assert_val)?);
  }};
}

// Test both `TryReinterpret`
macro_rules! test_try_reinterpret {
  (
    $init:expr => $ty:ty $(= $assert_val:expr)?
  ) => {{
    test_assert!(identity::<Option<$ty>>($init.try_reinterpret()), Some($init) $(, $assert_val)?);
  }};
}

// Test both `ReinterpretInner` and `TryReinterpretInner`
macro_rules! test_reinterpret_inner {
  (
    $init:expr => $ty:ty $(= $assert_val:expr)?
  ) => {{
    test_assert!(identity::<$ty>($init.reinterpret_inner()), $init $(, $assert_val)?);
    test_assert!(identity::<$ty>($init.try_reinterpret_inner().unwrap()), $init $(, $assert_val)?);
  }};
}

// Test both `ReinterpretInner` and `TryReinterpretInner`
macro_rules! test_try_reinterpret_inner {
  (
    $init:expr => $ty:ty $(= $assert_val:expr)?
  ) => {{
    test_assert!(identity::<Result<$ty, _>>($init.try_reinterpret_inner()), $init $(, $assert_val)?);
  }};
}

#[test]
fn reinterpret_self() {
  test_reinterpret!(0u8 => u8);
  test_reinterpret!(() => ());
  test_reinterpret!([0i32, 1i32, 2i32] => [i32; 3]);
}

#[test]
fn reinterpret_same_align() {
  test_reinterpret!(1u8 => i8 = 1i8);
  test_reinterpret!(
    [u32::MAX, 0, 1] => [i32; 3]
    = [i32::from_ne_bytes(u32::MAX.to_ne_bytes()), 0, 1]
  );
  test_reinterpret!(0u32 => Option<NonZeroI32> = None);
}

#[test]
fn reinterpret_lesser_align() {
  test_reinterpret!(0u16 => [u8; 2] = [0u8; 2]);
  test_reinterpret!([0u64; 2] => [u16; 8] = [0u16; 8]);
}

#[test]
fn reinterpret_greater_align() {
  test_reinterpret!([0u16; 2] => u32 = 0);
}

#[test]
fn reinterpret_no_uninit() {
  test_reinterpret!(true => u8 = 1);
}

#[test]
fn try_reinterpret() {
  test_try_reinterpret!(0u8 => bool = Some(false));
  test_try_reinterpret!(2u8 => bool = None);
}

#[test]
fn reinterpret_inner_self() {
  test_reinterpret_inner!(&() => &());
  test_reinterpret_inner!([0u32; 2].as_slice() => &[u32]);
  test_reinterpret_inner!(&mut 1u32 => &mut u32);
  test_reinterpret_inner!([1i32, 4i32].as_mut_slice() => &mut [i32]);

  let x = &[0u8; 2] as *const [u8; 2];
  test_reinterpret_inner!(x => *const [u8; 2]);
  let x = &mut [0u8; 2] as *mut [u8; 2];
  test_reinterpret_inner!(x => *mut [u8; 2]);
  let x = NonNull::from(&5u64);
  test_reinterpret_inner!(x => NonNull<u64>);

  test_reinterpret_inner!(Some(&0u8) => Option<&u8>);
  test_reinterpret_inner!(Some(Some([0u8; 4].as_slice())) => Option<Option<&[u8]>>);
  test_reinterpret_inner!(Option::<&u8>::None => Option<&u8>);

  let x = Some(NonNull::from(&0i32));
  test_reinterpret_inner!(x => Option<NonNull<i32>>);

  let _: AtomicPtr<i8> = AtomicPtr::new(&mut 1i8).reinterpret_inner();
  let _: AtomicPtr<i8> =
    AtomicPtr::new(&mut 1i8).try_reinterpret_inner().unwrap();

  test_reinterpret_inner!(Pin::new(&1u8) => Pin<&u8>);
  test_reinterpret_inner!(Pin::new([0u16; 2].as_slice()) => Pin<&[u16]>);
}

#[test]
fn reinterpret_inner_same_align() {
  test_reinterpret_inner!(&50u8 => &i8 = &50);
  test_reinterpret_inner!([0u32; 2].as_slice() => &[i32] = [0; 2]);
  test_reinterpret_inner!(
    &mut 1f32 => &mut u32
    = &mut u32::from_ne_bytes(1f32.to_ne_bytes())
  );
  test_reinterpret_inner!([1i32, 4i32].as_mut_slice() => &mut [u32] = [1u32, 4u32]);

  let x = &[0u8; 2] as *const [u8; 2];
  test_reinterpret_inner!(x => *const [i8; 2] = x.cast());
  let x = &mut [0u8; 2] as *mut [u8; 2];
  test_reinterpret_inner!(x => *mut [i8; 2] = x.cast());
  let x = NonNull::from(&5u64);
  test_reinterpret_inner!(x => NonNull<f64> = x.cast());

  test_reinterpret_inner!(Some(&0u8) => Option<&i8> = Some(&0i8));
  test_reinterpret_inner!(
    Some(Some([127u8; 4].as_slice())) => Option<Option<&[i8]>>
    = Some(Some([127i8; 4].as_slice()))
  );
  test_reinterpret_inner!(Option::<&u8>::None => Option<&u8> = None);

  let x = Some(NonNull::from(&0i32));
  test_reinterpret_inner!(x => Option<NonNull<u32>> = x.map(|x| x.cast()));

  let _: AtomicPtr<u8> = AtomicPtr::new(&mut 1i8).reinterpret_inner();
  let _: AtomicPtr<u8> =
    AtomicPtr::new(&mut 1i8).try_reinterpret_inner().unwrap();

  test_reinterpret_inner!(Pin::new(&1u8) => Pin<&i8> = Pin::new(&1i8));
  test_reinterpret_inner!(
    Pin::new([0xFFFFu16; 2].as_slice()) => Pin<&[i16]>
    = Pin::new([i16::from_ne_bytes(0xFFFFu16.to_ne_bytes()); 2].as_slice())
  );
}

#[test]
fn reinterpret_inner_lesser_align() {
  test_reinterpret_inner!(&0xFF01u16 => &[u8; 2] = &0xFF01u16.to_ne_bytes());
  test_reinterpret_inner!([0u32; 2].as_slice() => &[[u16; 2]] = [[0; 2]; 2]);
  test_reinterpret_inner!(&mut 1f32 => &mut [u8; 4] = &1f32.to_ne_bytes());
  test_reinterpret_inner!(
    [1i32, 4i32].as_mut_slice() => &mut [[u8; 4]]
    = &[1u32.to_ne_bytes(), 4u32.to_ne_bytes()]
  );

  let x = &[0u64; 2] as *const [u64; 2];
  test_reinterpret_inner!(x => *const [[u32; 2]; 2] = x.cast());
  let x = &mut 0u16 as *mut u16;
  test_reinterpret_inner!(x => *mut [i8; 2] = x.cast());
  let x = NonNull::from(&5u64);
  test_reinterpret_inner!(x => NonNull<[f32; 2]> = x.cast());

  test_reinterpret_inner!(Some(&0u32) => Option<&[u8; 4]> = Some(&[0; 4]));
  test_reinterpret_inner!(
    Some(Some([127u16; 2].as_slice())) => Option<Option<&[[u8; 2]]>>
    = Some(Some([127u16.to_ne_bytes(); 2].as_slice()))
  );
  test_reinterpret_inner!(Option::<&u16>::None => Option<&[u8; 2]> = None);

  let x = Some(NonNull::from(&0i32));
  test_reinterpret_inner!(x => Option<NonNull<[u16; 2]>> = x.map(|x| x.cast()));

  let _: AtomicPtr<[u8; 2]> = AtomicPtr::new(&mut 0u16).reinterpret_inner();
  let _: AtomicPtr<[u8; 2]> =
    AtomicPtr::new(&mut 0u16).try_reinterpret_inner().unwrap();

  test_reinterpret_inner!(
    Pin::new(&1u16) => Pin<&[u8; 2]>
    = Pin::new(&1u16.to_ne_bytes())
  );
  test_reinterpret_inner!(
    Pin::new([0xFF00u16; 2].as_slice()) => Pin<&[[u8; 2]]>
    = Pin::new([0xFF00u16.to_ne_bytes(); 2].as_slice())
  );
}

#[test]
fn reinterpret_inner_no_uninit() {
  test_reinterpret_inner!(&true => &u8 = &1);
  test_reinterpret_inner!([true, false].as_slice() => &[u8] = [1, 0]);
  let x = &true as *const bool;
  test_reinterpret_inner!(x => *const u8 = x.cast());
  test_reinterpret_inner!(Some(&true) => Option<&u8> = Some(&1));
  test_reinterpret_inner!(Some(Some(&true)) => Option<Option<&u8>> = Some(Some(&1)));
  test_reinterpret_inner!(Pin::new(&true) => Pin<&u8> = Pin::new(&1));
}

#[test]
fn reinterpret_inner_unsize() {
  test_reinterpret_inner!(&0u32 => &[u8] = [0; 4]);
  test_reinterpret_inner!(&0u32 => &[u16] = [0; 2]);
  test_reinterpret_inner!(&mut 0xFFEEDDCCu32 => &mut [u8] = 0xFFEEDDCCu32.to_ne_bytes());
  test_reinterpret_inner!(&mut 0u32 => &mut [u16] = [0; 2]);
  #[cfg(feature = "non_null_slice_cast")]
  {
    let x = NonNull::from(&mut 0u32);
    test_reinterpret_inner!(
        x => NonNull<[u8]>
        = NonNull::new(ptr::slice_from_raw_parts_mut(x.as_ptr().cast(), 4)).unwrap()
    );
    test_reinterpret_inner!(
        x => NonNull<[u16]>
        = NonNull::new(ptr::slice_from_raw_parts_mut(x.as_ptr().cast(), 2)).unwrap()
    );
  }
}

#[test]
fn try_reinterpret_inner_misaligned() {
  let x = [0u32, 0u32];
  let x: &[u16; 4] = (&x).reinterpret_inner();
  let x: &[u16; 2] = x[1..3].try_reinterpret_inner().unwrap();
  test_try_reinterpret_inner!(x => &u32 = Err(AlignmentMismatch));
  test_try_reinterpret_inner!(x.as_slice() => &[u32] = Err(AlignmentMismatch));
  test_try_reinterpret_inner!(Pin::new(x) => Pin<&u32> = Err(AlignmentMismatch));
  test_try_reinterpret_inner!(x as *const [u16; 2] => *const u32 = Err(AlignmentMismatch));
  test_try_reinterpret_inner!(Some(x) => Option<&u32> = Err(AlignmentMismatch));

  let mut x = [0u32, 0u32];
  let x: &mut [u16; 4] = (&mut x).reinterpret_inner();
  let x: &mut [u16; 2] = (&mut x[1..3]).try_reinterpret_inner().unwrap();
  test_try_reinterpret_inner!(x => &mut u32 = Err(AlignmentMismatch));
  test_try_reinterpret_inner!(x.as_mut_slice() => &mut [u32] = Err(AlignmentMismatch));
  test_try_reinterpret_inner!(Pin::new(&mut *x) => Pin<&mut u32> = Err(AlignmentMismatch));
  test_try_reinterpret_inner!(x as *mut [u16; 2] => *mut u32 = Err(AlignmentMismatch));
  test_try_reinterpret_inner!(Some(&mut *x) => Option<&mut u32> = Err(AlignmentMismatch));
  test_try_reinterpret_inner!(NonNull::from(&mut *x) => NonNull<u32> = Err(AlignmentMismatch));
  let err: Result<AtomicPtr<u32>, _> =
    AtomicPtr::new(&mut *x).try_reinterpret_inner();
  assert!(matches!(err, Err(AlignmentMismatch)));
  #[cfg(feature = "non_null_slice_cast")]
  {
    test_try_reinterpret_inner!(NonNull::from(x.as_mut_slice()) => NonNull<[u32]> = Err(AlignmentMismatch));
  }
}

#[test]
fn try_reinterpret_inner_greater_align() {
  let x = 0u32;
  let y: &[u16; 2] = (&x).reinterpret_inner();
  test_try_reinterpret_inner!(y => &u32 = Ok(&x));
  test_try_reinterpret_inner!(y.as_slice() => &[u32] = Ok([x].as_slice()));
  test_try_reinterpret_inner!(Pin::new(y) => Pin<&u32> = Ok(Pin::new(&x)));
  test_try_reinterpret_inner!(y as *const [u16; 2] => *const u32 = Ok(&x as *const u32));
  test_try_reinterpret_inner!(NonNull::from(y) => NonNull<u32> = Ok(NonNull::from(&x)));
  test_try_reinterpret_inner!(Some(y) => Option<&u32> = Ok(Some(&x)));

  let mut x = 0u32;
  let ptr = &mut x as *mut u32;
  let y: &mut [u16; 2] = (&mut x).reinterpret_inner();
  test_try_reinterpret_inner!(y => &mut u32 = Ok(&mut 0));
  test_try_reinterpret_inner!(y.as_mut_slice() => &mut [u32] = Ok([0].as_mut_slice()));
  test_try_reinterpret_inner!(Pin::new(&mut *y) => Pin<&mut u32> = Ok(Pin::new(&mut 0)));
  test_try_reinterpret_inner!(y as *mut [u16; 2] => *mut u32 = Ok(ptr));
  test_try_reinterpret_inner!(Some(&mut *y) => Option<&mut u32> = Ok(Some(&mut 0)));
  let _: AtomicPtr<u32> =
    AtomicPtr::new(&mut *y).try_reinterpret_inner().unwrap();
  #[cfg(feature = "non_null_slice_cast")]
  {
    test_try_reinterpret_inner!(
        NonNull::from(y.as_mut_slice()) => NonNull<[u32]>
        = Ok(NonNull::new(ptr::slice_from_raw_parts_mut(ptr, 1)).unwrap())
    );
  }
}

#[test]
fn try_reinterpret_change_element_size() {
  test_try_reinterpret_inner!(
    [0u32; 3].as_slice() => &[[u8; 3]]
    = Ok([[0; 3]; 4].as_slice())
  );
  test_try_reinterpret_inner!(
    [0u32; 1].as_slice() => &[[u8; 2]]
    = Ok([[0; 2]; 2].as_slice())
  );
  test_try_reinterpret_inner!(
    [0u32; 3].as_mut_slice() => &mut [[u8; 3]]
    = Ok([[0; 3]; 4].as_mut_slice())
  );
  test_try_reinterpret_inner!(
    [0u32; 1].as_mut_slice() => &mut [[u8; 2]]
    = Ok([[0; 2]; 2].as_mut_slice())
  );
  test_try_reinterpret_inner!(
    Some([0u32; 3].as_slice()) => Option<&[[u8; 3]]>
    = Ok(Some([[0; 3]; 4].as_slice()))
  );
  test_try_reinterpret_inner!(
    Pin::new([0u32; 1].as_slice()) => Pin<&[[u8; 2]]>
    = Ok(Pin::new([[0; 2]; 2].as_slice()))
  );
  #[cfg(feature = "non_null_slice_cast")]
  {
    let mut x = [0u32; 3];
    test_try_reinterpret_inner!(
        NonNull::from(x.as_mut_slice()) => NonNull<[[u8; 3]]>
        = Ok(NonNull::new(ptr::slice_from_raw_parts_mut(x.as_mut_ptr().cast::<[u8; 3]>(), 4)).unwrap())
    );
  }
}

#[test]
fn try_reinterpret_wrong_element_size() {
  test_try_reinterpret_inner!(
    [0u32; 3].as_slice() => &[[u8; 5]]
    = Err(OutputSliceWouldHaveSlop)
  );
  test_try_reinterpret_inner!(
    [0u32; 1].as_slice() => &[[u32; 2]]
    = Err(OutputSliceWouldHaveSlop)
  );
  test_try_reinterpret_inner!(
    [0u32; 3].as_mut_slice() => &mut [[u8; 5]]
    = Err(OutputSliceWouldHaveSlop)
  );
  test_try_reinterpret_inner!(
    [0u32; 1].as_mut_slice() => &mut [[u32; 2]]
    = Err(OutputSliceWouldHaveSlop)
  );
  test_try_reinterpret_inner!(
    Some([0u32; 3].as_slice()) => Option<&[[u8; 5]]>
    = Err(OutputSliceWouldHaveSlop)
  );
  test_try_reinterpret_inner!(
    Pin::new([0u32; 1].as_slice()) => Pin<&[[u32; 2]]>
    = Err(OutputSliceWouldHaveSlop)
  );
  #[cfg(feature = "non_null_slice_cast")]
  {
    let mut x = [0u32; 3];
    test_try_reinterpret_inner!(
      NonNull::from(x.as_mut_slice()) => NonNull<[[u8; 5]]>
      = Err(OutputSliceWouldHaveSlop)
    );
  }
}

#[test]
fn try_reinterpret_inner_resize() {
  let x = 0u32;
  let x: &[u8; 4] = (&x).reinterpret_inner();
  test_try_reinterpret_inner!(x.as_slice() => &u32 = Ok(&0));
  test_try_reinterpret_inner!(x.as_slice() => &[u16; 2] = Ok(&[0u16; 2]));
  test_try_reinterpret_inner!(x.as_slice() => &[u32; 2] = Err(SizeMismatch));

  let mut x = 0u32;
  let x: &mut [u8; 4] = (&mut x).reinterpret_inner();
  test_try_reinterpret_inner!(x.as_mut_slice() => &mut u32 = Ok(&mut 0u32));
  test_try_reinterpret_inner!(x.as_mut_slice() => &mut [u16; 2] = Ok(&mut [0u16; 2]));
  test_try_reinterpret_inner!(x.as_mut_slice() => &mut [u32; 2] = Err(SizeMismatch));
}
