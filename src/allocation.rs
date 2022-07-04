#![cfg(feature = "extern_crate_alloc")]

//! Stuff to boost things in the `alloc` crate.
//!
//! * You must enable the `extern_crate_alloc` feature of `bytemuck` or you will
//!   not be able to use this module! This is generally done by adding the
//!   feature to the dependency in Cargo.toml like so:
//!
//!   `bytemuck = { version = "VERSION_YOU_ARE_USING", features =
//! ["extern_crate_alloc"]}`

use super::*;
use alloc::{
  alloc::{alloc_zeroed, Layout, LayoutError},
  boxed::Box,
  vec,
  vec::Vec,
};
use core::convert::TryInto;

/// As [`try_cast_box`](try_cast_box), but unwraps for you.
#[inline]
pub fn cast_box<A: NoUninit, B: AnyBitPattern>(input: Box<A>) -> Box<B> {
  try_cast_box(input).map_err(|(e, _v)| e).unwrap()
}

/// Attempts to cast the content type of a [`Box`](alloc::boxed::Box).
///
/// On failure you get back an error along with the starting `Box`.
///
/// ## Failure
///
/// * The start and end content type of the `Box` must have the exact same
///   alignment.
/// * The start and end size of the `Box` must have the exact same size.
#[inline]
pub fn try_cast_box<A: NoUninit, B: AnyBitPattern>(
  input: Box<A>,
) -> Result<Box<B>, (PodCastError, Box<A>)> {
  if align_of::<A>() != align_of::<B>() {
    Err((PodCastError::AlignmentMismatch, input))
  } else if size_of::<A>() != size_of::<B>() {
    Err((PodCastError::SizeMismatch, input))
  } else {
    // Note(Lokathor): This is much simpler than with the Vec casting!
    let ptr: *mut B = Box::into_raw(input) as *mut B;
    Ok(unsafe { Box::from_raw(ptr) })
  }
}

/// Allocates a `Box<T>` with all of the contents being zeroed out.
///
/// This uses the global allocator to create a zeroed allocation and _then_
/// turns it into a Box. In other words, it's 100% assured that the zeroed data
/// won't be put temporarily on the stack. You can make a box of any size
/// without fear of a stack overflow.
///
/// ## Failure
///
/// This fails if the allocation fails.
#[inline]
pub fn try_zeroed_box<T: Zeroable>() -> Result<Box<T>, ()> {
  if size_of::<T>() == 0 {
    // This will not allocate but simple create a dangling slice pointer.
    // NB: We go the way via a push to `Vec<T>` to ensure the compiler
    // does not allocate space for T on the stack even if the branch
    // would not be taken.
    let mut vec = Vec::with_capacity(1);
    vec.resize_with(1, || T::zeroed());
    let ptr: Box<[T; 1]> = vec.into_boxed_slice().try_into().ok().unwrap();
    debug_assert!(
      align_of::<[T; 1]>() == align_of::<T>()
        && size_of::<[T; 1]>() == size_of::<T>()
    );
    // NB: We basically do the same as in try_cast_box here:
    let ptr: Box<T> = unsafe { Box::from_raw(Box::into_raw(ptr) as *mut _) };
    return Ok(ptr);
  }
  let layout =
    Layout::from_size_align(size_of::<T>(), align_of::<T>()).unwrap();
  let ptr = unsafe { alloc_zeroed(layout) };
  if ptr.is_null() {
    // we don't know what the error is because `alloc_zeroed` is a dumb API
    Err(())
  } else {
    Ok(unsafe { Box::<T>::from_raw(ptr as *mut T) })
  }
}

/// As [`try_zeroed_box`], but unwraps for you.
#[inline]
pub fn zeroed_box<T: Zeroable>() -> Box<T> {
  try_zeroed_box().unwrap()
}

/// Allocates a `Vec<T>` of length and capacity exactly equal to `length` and all elements zeroed.
/// 
/// ## Failure
///
/// This fails if the allocation fails, or if a layout cannot be calculated for the allocation.
pub fn try_zeroed_vec<T: Zeroable>(length: usize) -> Result<Vec<T>, LayoutError> {
  if length == 0 {
    Ok(Vec::new())
  } else {
    let layout = Layout::array::<T>(length)?;
    let ptr = unsafe { alloc_zeroed(layout) };
    if ptr.is_null() {
      alloc::alloc::handle_alloc_error(layout);
    }
    Ok(unsafe { Vec::from_raw_parts(ptr as *mut T, length, length) })
  }
}

/// As [`try_zeroed_vec`] but unwraps for you
pub fn zeroed_vec<T: Zeroable>(length: usize) -> Vec<T> {
  try_zeroed_vec(length).unwrap()
}

/// Allocates a `Box<[T]>` with all contents being zeroed out.
///
/// This uses the global allocator to create a zeroed allocation and _then_
/// turns it into a Box. In other words, it's 100% assured that the zeroed data
/// won't be put temporarily on the stack. You can make a box of any size
/// without fear of a stack overflow.
///
/// ## Failure
///
/// This fails if the allocation fails, or if a layout cannot be calculated for the allocation.
#[inline]
pub fn try_zeroed_slice_box<T: Zeroable>(
  length: usize,
) -> Result<Box<[T]>, ()> {
  if size_of::<T>() == 0 {
    // This will not allocate but simple create a dangling slice pointer.
    let mut vec = Vec::with_capacity(length);
    vec.resize_with(length, || T::zeroed());
    return Ok(vec.into_boxed_slice());
  }
  if length == 0 {
    // This will also not allocate.
    return Ok(Vec::new().into_boxed_slice());
  }
  let layout = core::alloc::Layout::array::<T>(length).map_err(|_| ())?;
  let ptr = unsafe { alloc_zeroed(layout) };
  if ptr.is_null() {
    alloc::alloc::handle_alloc_error(layout);
  }
  let slice =
    unsafe { core::slice::from_raw_parts_mut(ptr as *mut T, length) };
  Ok(unsafe { Box::<[T]>::from_raw(slice) })
}

/// As [`try_zeroed_slice_box`](try_zeroed_slice_box), but unwraps for you.
pub fn zeroed_slice_box<T: Zeroable>(length: usize) -> Box<[T]> {
  try_zeroed_slice_box(length).unwrap()
}

/// As [`try_cast_slice_box`](try_cast_slice_box), but unwraps for you.
#[inline]
pub fn cast_slice_box<A: NoUninit, B: AnyBitPattern>(
  input: Box<[A]>,
) -> Box<[B]> {
  try_cast_slice_box(input).map_err(|(e, _v)| e).unwrap()
}

/// Attempts to cast the content type of a `Box<[T]>`.
///
/// On failure you get back an error along with the starting `Box<[T]>`.
///
/// ## Failure
///
/// * The start and end content type of the `Box<[T]>` must have the exact same
///   alignment.
/// * The start and end content size in bytes of the `Box<[T]>` must be the
///   exact same.
#[inline]
pub fn try_cast_slice_box<A: NoUninit, B: AnyBitPattern>(
  input: Box<[A]>,
) -> Result<Box<[B]>, (PodCastError, Box<[A]>)> {
  if align_of::<A>() != align_of::<B>() {
    Err((PodCastError::AlignmentMismatch, input))
  } else if size_of::<A>() != size_of::<B>() {
    if size_of::<A>() * input.len() % size_of::<B>() != 0 {
      // If the size in bytes of the underlying buffer does not match an exact
      // multiple of the size of B, we cannot cast between them.
      Err((PodCastError::SizeMismatch, input))
    } else {
      // Because the size is an exact multiple, we can now change the length
      // of the slice and recreate the Box
      // NOTE: This is a valid operation because according to the docs of
      // std::alloc::GlobalAlloc::dealloc(), the Layout that was used to alloc
      // the block must be the same Layout that is used to dealloc the block.
      // Luckily, Layout only stores two things, the alignment, and the size in
      // bytes. So as long as both of those stay the same, the Layout will
      // remain a valid input to dealloc.
      let length = size_of::<A>() * input.len() / size_of::<B>();
      let box_ptr: *mut A = Box::into_raw(input) as *mut A;
      let ptr: *mut [B] =
        unsafe { core::slice::from_raw_parts_mut(box_ptr as *mut B, length) };
      Ok(unsafe { Box::<[B]>::from_raw(ptr) })
    }
  } else {
    let box_ptr: *mut [A] = Box::into_raw(input);
    let ptr: *mut [B] = box_ptr as *mut [B];
    Ok(unsafe { Box::<[B]>::from_raw(ptr) })
  }
}

/// As [`try_cast_vec`](try_cast_vec), but unwraps for you.
#[inline]
pub fn cast_vec<A: NoUninit, B: AnyBitPattern>(input: Vec<A>) -> Vec<B> {
  try_cast_vec(input).map_err(|(e, _v)| e).unwrap()
}

/// Attempts to cast the content type of a [`Vec`](alloc::vec::Vec).
///
/// On failure you get back an error along with the starting `Vec`.
///
/// ## Failure
///
/// * The start and end content type of the `Vec` must have the exact same
///   alignment.
/// * The start and end content size in bytes of the `Vec` must be the exact
///   same.
/// * The start and end capacity in bytes of the `Vec` mest be the exact same.
#[inline]
pub fn try_cast_vec<A: NoUninit, B: AnyBitPattern>(
  input: Vec<A>,
) -> Result<Vec<B>, (PodCastError, Vec<A>)> {
  if align_of::<A>() != align_of::<B>() {
    Err((PodCastError::AlignmentMismatch, input))
  } else if size_of::<A>() != size_of::<B>() {
    if size_of::<A>() * input.len() % size_of::<B>() != 0
      || size_of::<A>() * input.capacity() % size_of::<B>() != 0
    {
      // If the size in bytes of the underlying buffer does not match an exact
      // multiple of the size of B, we cannot cast between them.
      // Note that we have to pay special attention to make sure that both
      // length and capacity are valid under B, as we do not want to
      // change which bytes are considered part of the initialized slice
      // of the Vec
      Err((PodCastError::SizeMismatch, input))
    } else {
      // Because the size is an exact multiple, we can now change the length and
      // capacity and recreate the Vec
      // NOTE: This is a valid operation because according to the docs of
      // std::alloc::GlobalAlloc::dealloc(), the Layout that was used to alloc
      // the block must be the same Layout that is used to dealloc the block.
      // Luckily, Layout only stores two things, the alignment, and the size in
      // bytes. So as long as both of those stay the same, the Layout will
      // remain a valid input to dealloc.

      // Note(Lokathor): First we record the length and capacity, which don't
      // have any secret provenance metadata.
      let length: usize = size_of::<A>() * input.len() / size_of::<B>();
      let capacity: usize = size_of::<A>() * input.capacity() / size_of::<B>();
      // Note(Lokathor): Next we "pre-forget" the old Vec by wrapping with
      // ManuallyDrop, because if we used `core::mem::forget` after taking the
      // pointer then that would invalidate our pointer. In nightly there's a
      // "into raw parts" method, which we can switch this too eventually.
      let mut manual_drop_vec = ManuallyDrop::new(input);
      let vec_ptr: *mut A = manual_drop_vec.as_mut_ptr();
      let ptr: *mut B = vec_ptr as *mut B;
      Ok(unsafe { Vec::from_raw_parts(ptr, length, capacity) })
    }
  } else {
    // Note(Lokathor): First we record the length and capacity, which don't have
    // any secret provenance metadata.
    let length: usize = input.len();
    let capacity: usize = input.capacity();
    // Note(Lokathor): Next we "pre-forget" the old Vec by wrapping with
    // ManuallyDrop, because if we used `core::mem::forget` after taking the
    // pointer then that would invalidate our pointer. In nightly there's a
    // "into raw parts" method, which we can switch this too eventually.
    let mut manual_drop_vec = ManuallyDrop::new(input);
    let vec_ptr: *mut A = manual_drop_vec.as_mut_ptr();
    let ptr: *mut B = vec_ptr as *mut B;
    Ok(unsafe { Vec::from_raw_parts(ptr, length, capacity) })
  }
}

/// This "collects" a slice of pod data into a vec of a different pod type.
///
/// Unlike with [`cast_slice`] and [`cast_slice_mut`], this will always work.
///
/// The output vec will be of a minimal size/capacity to hold the slice given.
///
/// ```rust
/// # use bytemuck::*;
/// let halfwords: [u16; 4] = [5, 6, 7, 8];
/// let vec_of_words: Vec<u32> = pod_collect_to_vec(&halfwords);
/// if cfg!(target_endian = "little") {
///   assert_eq!(&vec_of_words[..], &[0x0006_0005, 0x0008_0007][..])
/// } else {
///   assert_eq!(&vec_of_words[..], &[0x0005_0006, 0x0007_0008][..])
/// }
/// ```
pub fn pod_collect_to_vec<
  A: NoUninit + AnyBitPattern,
  B: NoUninit + AnyBitPattern,
>(
  src: &[A],
) -> Vec<B> {
  let src_size = size_of_val(src);
  // Note(Lokathor): dst_count is rounded up so that the dest will always be at
  // least as many bytes as the src.
  let dst_count = src_size / size_of::<B>()
    + if src_size % size_of::<B>() != 0 { 1 } else { 0 };
  let mut dst = vec![B::zeroed(); dst_count];

  let src_bytes: &[u8] = cast_slice(src);
  let dst_bytes: &mut [u8] = cast_slice_mut(&mut dst[..]);
  dst_bytes[..src_size].copy_from_slice(src_bytes);
  dst
}

/// An extension trait for `TransparentWrapper` and alloc types.
pub trait TransparentWrapperAlloc<Inner: ?Sized>: TransparentWrapper<Inner> {
  /// Convert a vec of the inner type into a vec of the wrapper type.
  fn wrap_vec(s: Vec<Inner>) -> Vec<Self>
  where
    Self: Sized,
    Inner: Sized
  {
    let mut s = core::mem::ManuallyDrop::new(s);

    let length = s.len();
    let capacity = s.capacity();
    let ptr = s.as_mut_ptr();

    unsafe {
      // SAFETY:
      // * ptr comes from Vec (and will not be double-dropped)
      // * the two types have the identical representation
      // * the len and capacity fields are valid
      Vec::from_raw_parts(
        ptr as *mut Self,
        length,
        capacity
      )
    }
  }

  /// Convert a vec of the wrapper type into a vec of the inner type.
  fn peel_vec(s: Vec<Self>) -> Vec<Inner>
  where
    Self: Sized,
    Inner: Sized
  {
    let mut s = core::mem::ManuallyDrop::new(s);

    let length = s.len();
    let capacity = s.capacity();
    let ptr = s.as_mut_ptr();

    unsafe {
      // SAFETY:
      // * ptr comes from Vec (and will not be double-dropped)
      // * the two types have the identical representation
      // * the len and capacity fields are valid
      Vec::from_raw_parts(
        ptr as *mut Inner,
        length,
        capacity
      )
    }
  }
}
impl<I: ?Sized, T: TransparentWrapper<I>> TransparentWrapperAlloc<I> for T {}
