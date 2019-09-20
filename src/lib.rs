#![no_std]

#[cfg(target_arch = "x86")]
pub(crate) use core::arch::x86;
#[cfg(target_arch = "x86_64")]
pub(crate) use core::arch::x86_64;
//
pub(crate) use core::{marker::*, mem::*, num::*, ptr::*};

macro_rules! impl_unsafe_marker_for_array {
  ( $marker:ident , $( $n:expr ),* ) => {
    $(unsafe impl<T> $marker for [T; $n] where T: $marker {})*
  }
}

#[cfg(feature = "extern_crate_alloc")]
extern crate alloc;
#[cfg(feature = "extern_crate_alloc")]
mod allocation;
#[cfg(feature = "extern_crate_alloc")]
pub use allocation::*;

mod zeroable;
pub use zeroable::*;

mod pod;
pub use pod::*;

/// Re-interprets `&T` as `&[u8]`.
///
/// Any ZST becomes an empty slice, and in that case the pointer value of that
/// empty slice might not match the pointer value of the input reference.
#[inline]
pub fn bytes_of<T: Pod>(t: &T) -> &[u8] {
  try_cast_slice::<T, u8>(core::slice::from_ref(t)).unwrap_or(&[])
}

/// Re-interprets `&mut T` as `&mut [u8]`.
///
/// Any ZST becomes an empty slice, and in that case the pointer value of that
/// empty slice might not match the pointer value of the input reference.
#[inline]
pub fn bytes_of_mut<T: Pod>(t: &mut T) -> &mut [u8] {
  try_cast_slice_mut::<T, u8>(core::slice::from_mut(t)).unwrap_or(&mut [])
}

/// The things that can go wrong when casting between [`Pod`] data forms.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PodCastError {
  /// You tried to cast a slice to an element type with a higher alignment
  /// requirement but the slice wasn't aligned.
  TargetAlignmentGreaterAndInputNotAligned,
  /// If the element size changes then the output slice changes length
  /// accordingly. If the output slice wouldn't be a whole number of elements
  /// then the conversion fails.
  OutputSliceWouldHaveSlop,
  /// When casting a slice you can't convert between ZST elements and non-ZST
  /// elements. When casting an individual `T`, `&T`, or `&mut T` value the
  /// source size and destination size must be an exact match.
  SizeMismatch,
  /// For this type of cast the alignments must be exactly the same and they
  /// were not so now you're sad.
  AlignmentMismatch,
}

/// Cast `T` into `U`
///
/// ## Panics
///
/// This is [`try_cast`] with an unwrap.
#[inline]
pub fn cast<A: Pod, B: Pod>(a: A) -> B {
  try_cast(a).unwrap()
}

/// Cast `&mut T` into `&mut U`.
///
/// ## Panics
///
/// This is [`try_cast_mut`] with an unwrap.
#[inline]
pub fn cast_mut<A: Pod, B: Pod>(a: &mut A) -> &mut B {
  try_cast_mut(a).unwrap()
}

/// Cast `&T` into `&U`.
///
/// ## Panics
///
/// This is [`try_cast_ref`] with an unwrap.
#[inline]
pub fn cast_ref<A: Pod, B: Pod>(a: &A) -> &B {
  try_cast_ref(a).unwrap()
}

/// Cast `&[T]` into `&[U]`.
///
/// ## Panics
///
/// This is [`try_cast_slice`] with an unwrap.
#[inline]
pub fn cast_slice<A: Pod, B: Pod>(a: &[A]) -> &[B] {
  try_cast_slice(a).unwrap()
}

/// Cast `&mut [T]` into `&mut [U]`.
///
/// ## Panics
///
/// This is [`try_cast_slice_mut`] with an unwrap.
#[inline]
pub fn cast_slice_mut<A: Pod, B: Pod>(a: &mut [A]) -> &mut [B] {
  try_cast_slice_mut(a).unwrap()
}

/// As `align_to`, but safe because of the [`Pod`] bound.
#[inline]
pub fn pod_align_to<T: Pod, U: Pod>(vals: &[T]) -> (&[T], &[U], &[T]) {
  unsafe { vals.align_to::<U>() }
}

/// As `align_to_mut`, but safe because of the [`Pod`] bound.
#[inline]
pub fn pod_align_to_mut<T: Pod, U: Pod>(vals: &mut [T]) -> (&mut [T], &mut [U], &mut [T]) {
  unsafe { vals.align_to_mut::<U>() }
}

/// Try to cast `T` into `U`.
///
/// ## Failure
///
/// * If the types don't have the same size this fails.
#[inline]
pub fn try_cast<A: Pod, B: Pod>(a: A) -> Result<B, PodCastError> {
  if size_of::<A>() == size_of::<B>() {
    let mut b = B::zeroed();
    // Note(Lokathor): We copy in terms of `u8` because that allows us to bypass
    // any potential alignment difficulties.
    let ap = &a as *const A as *const u8;
    let bp = &mut b as *mut B as *mut u8;
    unsafe { ap.copy_to_nonoverlapping(bp, size_of::<A>()) };
    Ok(b)
  } else {
    Err(PodCastError::SizeMismatch)
  }
}

/// Try to convert a `&T` into `&U`.
///
/// ## Failure
///
/// * If the reference isn't aligned in the new type
/// * If the source type and target type aren't the same size.
#[inline]
pub fn try_cast_ref<A: Pod, B: Pod>(a: &A) -> Result<&B, PodCastError> {
  // Note(Lokathor): everything with `align_of` and `size_of` will optimize away
  // after monomorphization.
  if align_of::<B>() > align_of::<A>() && (a as *const A as usize) % align_of::<B>() != 0 {
    Err(PodCastError::TargetAlignmentGreaterAndInputNotAligned)
  } else if size_of::<B>() == size_of::<A>() {
    Ok(unsafe { &*(a as *const A as *const B) })
  } else {
    Err(PodCastError::SizeMismatch)
  }
}

/// Try to convert a `&mut T` into `&mut U`.
///
/// As [`try_cast_ref`], but `mut`.
#[inline]
pub fn try_cast_mut<A: Pod, B: Pod>(a: &mut A) -> Result<&mut B, PodCastError> {
  // Note(Lokathor): everything with `align_of` and `size_of` will optimize away
  // after monomorphization.
  if align_of::<B>() > align_of::<A>() && (a as *mut A as usize) % align_of::<B>() != 0 {
    Err(PodCastError::TargetAlignmentGreaterAndInputNotAligned)
  } else if size_of::<B>() == size_of::<A>() {
    Ok(unsafe {
      (a as *mut A as *mut B)
        .as_mut()
        .unwrap_or_else(|| core::hint::unreachable_unchecked())
    })
  } else {
    Err(PodCastError::SizeMismatch)
  }
}

/// Try to convert `&[T]` into `&[U]` (possibly with a change in length).
///
/// * `input.as_ptr() as usize == output.as_ptr() as usize`
/// * `input.len() * size_of::<A>() == output.len() * size_of::<B>()`
///
/// ## Failure
///
/// * If the target type has a greater alignment requirement and the input slice
///   isn't aligned.
/// * If the target element type is a different size from the current element
///   type, and the output slice wouldn't be a whole number of elements when
///   accounting for the size change (eg: three `u16` values is 1.5 `u32`
///   values, so that's a failure).
/// * Similarly, you can't convert between a
///   [ZST](https://doc.rust-lang.org/nomicon/exotic-sizes.html#zero-sized-types-zsts)
///   and a non-ZST.
#[inline]
pub fn try_cast_slice<A: Pod, B: Pod>(a: &[A]) -> Result<&[B], PodCastError> {
  // Note(Lokathor): everything with `align_of` and `size_of` will optimize away
  // after monomorphization.
  if align_of::<B>() > align_of::<A>() && (a.as_ptr() as usize) % align_of::<B>() != 0 {
    Err(PodCastError::TargetAlignmentGreaterAndInputNotAligned)
  } else if size_of::<B>() == size_of::<A>() {
    Ok(unsafe { core::slice::from_raw_parts(a.as_ptr() as *const B, a.len()) })
  } else if size_of::<A>() == 0 || size_of::<B>() == 0 {
    Err(PodCastError::SizeMismatch)
  } else if core::mem::size_of_val(a) % size_of::<B>() == 0 {
    let new_len = core::mem::size_of_val(a) / size_of::<B>();
    Ok(unsafe { core::slice::from_raw_parts(a.as_ptr() as *const B, new_len) })
  } else {
    Err(PodCastError::OutputSliceWouldHaveSlop)
  }
}

/// Try to convert `&mut [T]` into `mut [U]` (possibly with a change in length).
///
/// As [`try_cast_slice`], but `mut`.
#[inline]
pub fn try_cast_slice_mut<A: Pod, B: Pod>(a: &mut [A]) -> Result<&mut [B], PodCastError> {
  // Note(Lokathor): everything with `align_of` and `size_of` will optimize away
  // after monomorphization.
  if align_of::<B>() > align_of::<A>() && (a.as_ptr() as usize) % align_of::<B>() != 0 {
    Err(PodCastError::TargetAlignmentGreaterAndInputNotAligned)
  } else if size_of::<B>() == size_of::<A>() {
    Ok(unsafe { core::slice::from_raw_parts_mut(a.as_ptr() as *mut B, a.len()) })
  } else if size_of::<A>() == 0 || size_of::<B>() == 0 {
    Err(PodCastError::SizeMismatch)
  } else if core::mem::size_of_val(a) % size_of::<B>() == 0 {
    let new_len = core::mem::size_of_val(a) / size_of::<B>();
    Ok(unsafe { core::slice::from_raw_parts_mut(a.as_ptr() as *mut B, new_len) })
  } else {
    Err(PodCastError::OutputSliceWouldHaveSlop)
  }
}
