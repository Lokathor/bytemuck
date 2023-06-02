#![cfg(all(feature = "unified_cast", feature = "extern_crate_alloc"))]

extern crate alloc;

use alloc::{
  borrow::Cow,
  boxed::Box,
  rc::{Rc, Weak as RcWeak},
  sync::{Arc, Weak as ArcWeak},
  vec::Vec,
};
use bytemuck::{
  PodCastError::{AlignmentMismatch, OutputSliceWouldHaveSlop, SizeMismatch},
  ReinterpretInner, TryReinterpretInner,
};
use core::convert::identity;

macro_rules! test_assert {
  ($target:expr, $init:expr) => {
    assert_eq!($target, $init);
  };
  ($target:expr, $_init:expr, $e:expr) => {
    assert_eq!($target, $e);
  };
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
fn reinterpret_inner_self() {
  test_reinterpret_inner!(Cow::Borrowed(&1u32) => Cow<u32>);
  test_reinterpret_inner!(Cow::<u32>::Owned(1u32) => Cow<u32>);
  test_reinterpret_inner!(Cow::Borrowed([1u32].as_slice()) => Cow<[u32]>);
  test_reinterpret_inner!(Cow::<[u32]>::Owned(vec![1u32]) => Cow<[u32]>);
  test_reinterpret_inner!(Box::new(1u32) => Box<u32>);
  test_reinterpret_inner!(vec![1u32].into_boxed_slice() => Box<[u32]>);
  test_reinterpret_inner!(Rc::new(1u32) => Rc<u32>);
  test_reinterpret_inner!(Rc::<[u32]>::from([1u32].as_slice()) => Rc<[u32]>);
  let _: RcWeak<u32> = Rc::downgrade(&Rc::new(1u32)).reinterpret_inner();
  let _: RcWeak<u32> =
    Rc::downgrade(&Rc::new(1u32)).try_reinterpret_inner().unwrap();
  test_reinterpret_inner!(Arc::new(1u32) => Arc<u32>);
  test_reinterpret_inner!(Arc::<[u32]>::from([1u32].as_slice()) => Arc<[u32]>);
  let _: ArcWeak<u32> = Arc::downgrade(&Arc::new(1u32)).reinterpret_inner();
  let _: ArcWeak<u32> =
    Arc::downgrade(&Arc::new(1u32)).try_reinterpret_inner().unwrap();
  test_reinterpret_inner!(vec![1u32] => Vec<u32>);
}

#[test]
fn reinterpret_inner_same_align() {
  test_reinterpret_inner!(Cow::Borrowed(&1u32) => Cow<i32> = Cow::Borrowed(&1i32));
  test_reinterpret_inner!(Cow::<u32>::Owned(1u32) => Cow<i32> = Cow::<i32>::Owned(1i32));
  test_reinterpret_inner!(
    Cow::Borrowed([1u32].as_slice()) => Cow<[i32]>
    = Cow::Borrowed([1i32].as_slice())
  );
  test_reinterpret_inner!(
    Cow::<[u32]>::Owned(vec![1u32]) => Cow<[i32]>
    = Cow::<[i32]>::Owned(vec![1i32])
  );
  test_reinterpret_inner!(Box::new(1u32) => Box<i32> = Box::new(1i32));
  test_reinterpret_inner!(
    vec![1u32].into_boxed_slice() => Box<[i32]>
    = vec![1i32].into_boxed_slice()
  );
  test_reinterpret_inner!(Rc::new(1u32) => Rc<i32> = Rc::new(1i32));
  test_reinterpret_inner!(
    Rc::<[u32]>::from([1u32].as_slice()) => Rc<[i32]>
    = Rc::<[i32]>::from([1i32].as_slice())
  );
  let _: RcWeak<i32> = Rc::downgrade(&Rc::new(1u32)).reinterpret_inner();
  let _: RcWeak<i32> =
    Rc::downgrade(&Rc::new(1u32)).try_reinterpret_inner().unwrap();
  test_reinterpret_inner!(Arc::new(1u32) => Arc<i32> = Arc::new(1i32));
  test_reinterpret_inner!(
    Arc::<[u32]>::from([1u32].as_slice()) => Arc<[i32]>
    = Arc::<[i32]>::from([1i32].as_slice())
  );
  let _: ArcWeak<i32> = Arc::downgrade(&Arc::new(1u32)).reinterpret_inner();
  let _: ArcWeak<i32> =
    Arc::downgrade(&Arc::new(1u32)).try_reinterpret_inner().unwrap();
  test_reinterpret_inner!(vec![1u32] => Vec<i32> = vec![1i32]);
}

#[test]
fn reinterpret_inner_lesser_align() {
  test_reinterpret_inner!(Cow::Borrowed(&0u32) => Cow<[u16; 2]> = Cow::Borrowed(&[0u16; 2]));
  test_reinterpret_inner!(
    Cow::<u32>::Owned(0u32) => Cow<[u16; 2]>
    = Cow::<[u16; 2]>::Owned([0u16; 2])
  );
  test_try_reinterpret_inner!(
    Cow::<[u32]>::Owned(vec![0u32; 2]) => Cow<[u8]>
    = Err((AlignmentMismatch, Cow::<[u32]>::Owned(vec![0u32; 2])))
  );
}

#[test]
fn reinterpret_inner_no_uninit() {
  test_reinterpret_inner!(Cow::Borrowed(&false) => Cow<u8> = Cow::Borrowed(&0));
  test_reinterpret_inner!(
    Cow::<bool>::Owned(false) => Cow<u8>
    = Cow::<u8>::Owned(0u8)
  );
  test_reinterpret_inner!(
    Cow::Borrowed([false].as_slice()) => Cow<[u8]>
    = Cow::Borrowed([0u8].as_slice())
  );
  test_reinterpret_inner!(
    Cow::<[bool]>::Owned(vec![false]) => Cow<[u8]>
    = Cow::<[u8]>::Owned(vec![0u8])
  );
  test_reinterpret_inner!(Box::new(false) => Box<u8> = Box::new(0u8));
  test_reinterpret_inner!(
    vec![false].into_boxed_slice() => Box<[u8]>
    = vec![0u8].into_boxed_slice()
  );
  test_reinterpret_inner!(Rc::new(false) => Rc<u8> = Rc::new(0u8));
  test_reinterpret_inner!(
    Rc::<[bool]>::from([false].as_slice()) => Rc<[u8]>
    = Rc::<[u8]>::from([0u8].as_slice())
  );
  let _: RcWeak<u8> = Rc::downgrade(&Rc::new(false)).reinterpret_inner();
  let _: RcWeak<u8> =
    Rc::downgrade(&Rc::new(false)).try_reinterpret_inner().unwrap();
  test_reinterpret_inner!(Arc::new(false) => Arc<u8> = Arc::new(0u8));
  test_reinterpret_inner!(
    Arc::<[bool]>::from([false].as_slice()) => Arc<[u8]>
    = Arc::<[u8]>::from([0u8].as_slice())
  );
  let _: ArcWeak<u8> = Arc::downgrade(&Arc::new(false)).reinterpret_inner();
  let _: ArcWeak<u8> =
    Arc::downgrade(&Arc::new(false)).try_reinterpret_inner().unwrap();
  test_reinterpret_inner!(vec![false] => Vec<u8> = vec![0u8]);
}

#[test]
fn reinterpret_inner_unsize() {
  test_reinterpret_inner!(
    Box::new([0u32]) => Box<[u32]>
    = vec!(0u32).into_boxed_slice()
  );
  test_reinterpret_inner!(
    Rc::new([0u32]) => Rc<[u32]>
    = Rc::<[u32]>::from([0u32].as_slice())
  );
  test_reinterpret_inner!(
    Arc::new([0u32]) => Arc<[u32]>
    = Arc::<[u32]>::from([0u32].as_slice())
  );
}

#[test]
fn try_reinterpret_inner_misaligned() {
  let x = [0u32; 2];
  let x: &[u8] = x.as_slice().reinterpret_inner();
  let x = &x[1..5];
  test_try_reinterpret_inner!(
    Cow::<[u8; 4]>::Borrowed(x.try_reinterpret_inner().unwrap()) => Cow<u32>
    = Err(AlignmentMismatch)
  );
  test_try_reinterpret_inner!(
    Cow::<[u8]>::Borrowed(x) => Cow<[u32]>
    = Err((AlignmentMismatch, Cow::<[u8]>::Borrowed(x)))
  );
}

#[test]
fn try_reinterpret_inner_change_align() {
  let x = [0u32; 2];
  let x: &[u8; 8] = (&x).reinterpret_inner();
  test_try_reinterpret_inner!(
    Cow::<[u8; 8]>::Borrowed(x) => Cow<[u32; 2]>
    = Ok(Cow::<[u32; 2]>::Borrowed(&[0u32; 2]))
  );
  test_try_reinterpret_inner!(
    Cow::<[u8]>::Borrowed(x) => Cow<[u32]>
    = Ok(Cow::<[u32]>::Borrowed([0u32; 2].as_slice()))
  );
  test_try_reinterpret_inner!(
    Cow::<[u8]>::Owned(vec![0u8; 4]) => Cow<[u32]>
    = Err((AlignmentMismatch, Cow::<[u8]>::Owned(vec![0u8; 4])))
  );
}

#[test]
fn try_reinterpret_change_element_size() {
  let x = [[0u16; 2]; 2].as_slice();
  let y = [0u16; 4].as_slice();
  test_try_reinterpret_inner!(
    Cow::<[[u16; 2]]>::Borrowed(x) => Cow<[u16]>
    = Ok(Cow::<[u16]>::Borrowed(y))
  );
  test_try_reinterpret_inner!(
    Cow::<[[u16; 2]]>::Owned(Vec::from(x)) => Cow<[u16]>
    = Ok(Cow::<[u16]>::Owned(Vec::from(y)))
  );
  test_try_reinterpret_inner!(
    Box::<[[u16; 2]]>::from(x) => Box<[u16]>
    = Ok(Box::<[u16]>::from(y))
  );
  test_try_reinterpret_inner!(
    Rc::<[[u16; 2]]>::from(x) => Rc<[u16]>
    = Ok(Rc::<[u16]>::from(y))
  );
  test_try_reinterpret_inner!(
    Arc::<[[u16; 2]]>::from(x) => Arc<[u16]>
    = Ok(Arc::<[u16]>::from(y))
  );
  test_try_reinterpret_inner!(
    Vec::<[u16; 2]>::from(x) => Vec<u16>
    = Ok(Vec::<u16>::from(y))
  );
}

#[test]
fn try_reinterpret_wrong_element_size() {
  let x = [[0u16; 2]; 2].as_slice();
  test_try_reinterpret_inner!(
    Cow::<[[u16; 2]]>::Borrowed(x) => Cow<[[u16; 3]]>
    = Err((OutputSliceWouldHaveSlop, Cow::<[[u16; 2]]>::Borrowed(x)))
  );
  test_try_reinterpret_inner!(
    Cow::<[[u16; 2]]>::Owned(Vec::from(x)) => Cow<[[u16; 3]]>
    = Err((OutputSliceWouldHaveSlop, Cow::<[[u16; 2]]>::Owned(Vec::from(x))))
  );
  test_try_reinterpret_inner!(
    Box::<[[u16; 2]]>::from(x) => Box<[[u16; 3]]>
    = Err((OutputSliceWouldHaveSlop, Box::<[[u16; 2]]>::from(x)))
  );
  test_try_reinterpret_inner!(
    Rc::<[[u16; 2]]>::from(x) => Rc<[[u16; 3]]>
    = Err((OutputSliceWouldHaveSlop, Rc::<[[u16; 2]]>::from(x)))
  );
  test_try_reinterpret_inner!(
    Arc::<[[u16; 2]]>::from(x) => Arc<[[u16; 3]]>
    = Err((OutputSliceWouldHaveSlop, Arc::<[[u16; 2]]>::from(x)))
  );
  test_try_reinterpret_inner!(
    Vec::<[u16; 2]>::from(x) => Vec<[u16; 3]>
    = Err((OutputSliceWouldHaveSlop, Vec::<[u16; 2]>::from(x)))
  );
  let mut x = Vec::with_capacity(4);
  x.extend([[0u16; 2]; 3]);
  test_try_reinterpret_inner!(
    x => Vec<[u16; 3]>
    = Err((OutputSliceWouldHaveSlop, vec![[0u16; 2]; 3]))
  );
}

#[test]
fn try_reinterpret_inner_resize() {
  let x: Box<[u32]> = Box::new(0u32).reinterpret_inner();
  test_try_reinterpret_inner!(x => Box<u32> = Ok(Box::new(0u32)));
  let x: Box<[u32]> = Box::new(0u32).reinterpret_inner();
  test_try_reinterpret_inner!(x => Box<[u32; 2]> = Err((SizeMismatch, Box::from([0u32].as_slice()))));

  let x: Rc<[u32]> = Rc::new(0u32).reinterpret_inner();
  test_try_reinterpret_inner!(x => Rc<u32> = Ok(Rc::new(0u32)));
  let x: Rc<[u32]> = Rc::new(0u32).reinterpret_inner();
  test_try_reinterpret_inner!(x => Rc<[u32; 2]> = Err((SizeMismatch, Rc::from([0u32].as_slice()))));

  let x: Arc<[u32]> = Arc::new(0u32).reinterpret_inner();
  test_try_reinterpret_inner!(x => Arc<u32> = Ok(Arc::new(0u32)));
  let x: Arc<[u32]> = Arc::new(0u32).reinterpret_inner();
  test_try_reinterpret_inner!(x => Arc<[u32; 2]> = Err((SizeMismatch, Arc::from([0u32].as_slice()))));
}
