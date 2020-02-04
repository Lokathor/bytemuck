#![no_std]
#![warn(missing_docs)]

//! This crate gives small utilities for casting between plain data types.
//!
//! ## Basics
//!
//! Data comes in five basic forms in Rust, so we have five basic casting
//! functions:
//!
//! * `T` uses [`cast`]
//! * `&T` uses [`cast_ref`]
//! * `&mut T` uses [`cast_mut`]
//! * `&[T]` uses [`cast_slice`]
//! * `&mut [T]` uses [`cast_slice_mut`]
//!
//! Some casts will never fail (eg: `cast::<u32, f32>` always works), other
//! casts might fail (eg: `cast_ref::<[u8; 4], u32>` will fail if the reference
//! isn't already aligned to 4). Each casting function has a "try" version which
//! will return a `Result`, and the "normal" version which will simply panic on
//! invalid input.
//!
//! ## Using Your Own Types
//!
//! All the functions here are guarded by the [`Pod`] trait, which is a
//! sub-trait of the [`Zeroable`] trait.
//!
//! If you're very sure that your type is eligible, you can implement those
//! traits for your type and then they'll have full casting support. However,
//! these traits are `unsafe`, and you should carefully read the requirements
//! before adding the them to your own types.
//!
//! ## Features
//!
//! * This crate is core only by default, but if you're using Rust 1.36 or later
//!   you can enable the `extern_crate_alloc` cargo feature for some additional
//!   methods related to `Box` and `Vec`. Note that the `docs.rs` documentation
//!   is always built with `extern_crate_alloc` cargo feature enabled.

#[cfg(target_arch = "x86")]
use core::arch::x86;
#[cfg(target_arch = "x86_64")]
use core::arch::x86_64;
//
use core::{marker::*, mem::*, num::*, ptr::*};
use core::hint::unreachable_unchecked;

macro_rules! impl_unsafe_marker_for_array {
  ( $marker:ident , $( $n:expr ),* ) => {
    $(unsafe impl<T> $marker for [T; $n] where T: $marker {})*
  }
}

#[cfg(feature = "extern_crate_alloc")]
extern crate alloc;
#[cfg(feature = "extern_crate_alloc")]
pub mod allocation;
#[cfg(feature = "extern_crate_alloc")]
pub use allocation::*;

mod zeroable;
pub use zeroable::*;

mod pod;
pub use pod::*;

mod contiguous;
pub use contiguous::*;

mod transparent;
pub use transparent::*;

// Used from macros to ensure we aren't using some locally defined name and
// actually are referencing libcore. This also would allow pre-2018 edition
// crates to use our macros, but I'm not sure how important that is.
#[doc(hidden)]
pub use ::core as __core;

/// Re-interprets `&T` as `&[u8]`.
///
/// Any ZST becomes an empty slice, and in that case the pointer value of that
/// empty slice might not match the pointer value of the input reference.
#[inline]
pub fn bytes_of<T: Pod>(t: &T) -> &[u8] {
  // We never fail to go from T to u8s, so we hint the compiler about it.
  try_cast_slice::<T, u8>(core::slice::from_ref(t))
    .unwrap_or_else(|_| unsafe { unreachable_unchecked() })
}

/// Re-interprets `&mut T` as `&mut [u8]`.
///
/// Any ZST becomes an empty slice, and in that case the pointer value of that
/// empty slice might not match the pointer value of the input reference.
#[inline]
pub fn bytes_of_mut<T: Pod>(t: &mut T) -> &mut [u8] {
  // We never fail to go from T to u8s, so we hint the compiler about it.
  try_cast_slice_mut::<T, u8>(core::slice::from_mut(t))
    .unwrap_or_else(|_| unsafe { unreachable_unchecked() })
}

/// Re-interprets `&[u8]` as `&T`.
///
/// ## Panics
///
/// This is [`try_from_bytes`] with an unwrap.
#[inline]
pub fn from_bytes<T: Pod>(s: &[u8]) -> &T {
  try_from_bytes(s).unwrap()
}

/// Re-interprets `&mut [u8]` as `&mut T`.
///
/// ## Panics
///
/// This is [`try_from_bytes_mut`] with an unwrap.
#[inline]
pub fn from_bytes_mut<T: Pod>(s: &mut [u8]) -> &mut T {
  try_from_bytes_mut(s).unwrap()
}

/// Re-interprets `&[u8]` as `&T`.
///
/// ## Failure
///
/// * If the slice isn't aligned for the new type
/// * If the slice's length isn’t exactly the size of the new type
#[inline]
pub fn try_from_bytes<T: Pod>(s: &[u8]) -> Result<&T, PodCastError> {
  if s.len() != size_of::<T>() {
    Err(PodCastError::SizeMismatch)
  } else if (s.as_ptr() as usize) % align_of::<T>() != 0 {
    Err(PodCastError::AlignmentMismatch)
  } else {
    Ok(unsafe { &*(s.as_ptr() as *const T) })
  }
}

/// Re-interprets `&mut [u8]` as `&mut T`.
///
/// ## Failure
///
/// * If the slice isn't aligned for the new type
/// * If the slice's length isn’t exactly the size of the new type
#[inline]
pub fn try_from_bytes_mut<T: Pod>(
  s: &mut [u8],
) -> Result<&mut T, PodCastError> {
  if s.len() != size_of::<T>() {
    Err(PodCastError::SizeMismatch)
  } else if (s.as_ptr() as usize) % align_of::<T>() != 0 {
    Err(PodCastError::AlignmentMismatch)
  } else {
    Ok(unsafe { &mut *(s.as_mut_ptr() as *mut T) })
  }
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
  if size_of::<A>() == size_of::<B>() {
    // In this case it will not fail, and so we hint the compiler.
    try_cast(a).unwrap_or_else(|_| unsafe { unreachable_unchecked() })
  } else {
    try_cast(a).unwrap()
  }
}

/// Cast `&mut T` into `&mut U`.
///
/// ## Panics
///
/// This is [`try_cast_mut`] with an unwrap.
#[inline]
pub fn cast_mut<A: Pod, B: Pod>(a: &mut A) -> &mut B {
  if size_of::<A>() == size_of::<B>() && align_of::<A>() >= align_of::<B>() {
    // In this case it will not fail, and so we hint the compiler.
    try_cast_mut(a).unwrap_or_else(|_| unsafe { unreachable_unchecked() })
  } else {
    try_cast_mut(a).unwrap()
  }
}

/// Cast `&T` into `&U`.
///
/// ## Panics
///
/// This is [`try_cast_ref`] with an unwrap.
#[inline]
pub fn cast_ref<A: Pod, B: Pod>(a: &A) -> &B {
  if size_of::<A>() == size_of::<B>() && align_of::<A>() >= align_of::<B>() {
    // In this case it will not fail, and so we hint the compiler.
    try_cast_ref(a).unwrap_or_else(|_| unsafe { unreachable_unchecked() })
  } else {
    try_cast_ref(a).unwrap()
  }
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
pub fn pod_align_to_mut<T: Pod, U: Pod>(
  vals: &mut [T],
) -> (&mut [T], &mut [U], &mut [T]) {
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
  if align_of::<B>() > align_of::<A>()
    && (a as *const A as usize) % align_of::<B>() != 0
  {
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
  if align_of::<B>() > align_of::<A>()
    && (a as *mut A as usize) % align_of::<B>() != 0
  {
    Err(PodCastError::TargetAlignmentGreaterAndInputNotAligned)
  } else if size_of::<B>() == size_of::<A>() {
    Ok(unsafe { &mut *(a as *mut A as *mut B) })
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
///   accounting for the size change (eg: 3 `u16` values is 1.5 `u32` values, so
///   that's a failure).
/// * Similarly, you can't convert between a
///   [ZST](https://doc.rust-lang.org/nomicon/exotic-sizes.html#zero-sized-types-zsts)
///   and a non-ZST.
#[inline]
pub fn try_cast_slice<A: Pod, B: Pod>(a: &[A]) -> Result<&[B], PodCastError> {
  // Note(Lokathor): everything with `align_of` and `size_of` will optimize away
  // after monomorphization.
  if align_of::<B>() > align_of::<A>()
    && (a.as_ptr() as usize) % align_of::<B>() != 0
  {
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

/// Try to convert `&mut [T]` into `&mut [U]` (possibly with a change in length).
///
/// As [`try_cast_slice`], but `&mut`.
#[inline]
pub fn try_cast_slice_mut<A: Pod, B: Pod>(
  a: &mut [A],
) -> Result<&mut [B], PodCastError> {
  // Note(Lokathor): everything with `align_of` and `size_of` will optimize away
  // after monomorphization.
  if align_of::<B>() > align_of::<A>()
    && (a.as_mut_ptr() as usize) % align_of::<B>() != 0
  {
    Err(PodCastError::TargetAlignmentGreaterAndInputNotAligned)
  } else if size_of::<B>() == size_of::<A>() {
    Ok(unsafe {
      core::slice::from_raw_parts_mut(a.as_mut_ptr() as *mut B, a.len())
    })
  } else if size_of::<A>() == 0 || size_of::<B>() == 0 {
    Err(PodCastError::SizeMismatch)
  } else if core::mem::size_of_val(a) % size_of::<B>() == 0 {
    let new_len = core::mem::size_of_val(a) / size_of::<B>();
    Ok(unsafe {
      core::slice::from_raw_parts_mut(a.as_mut_ptr() as *mut B, new_len)
    })
  } else {
    Err(PodCastError::OutputSliceWouldHaveSlop)
  }
}

/// Find the offset in bytes of the given `$field` of `$Type`, using `$instance`
/// as an already-initialized value to work with.
///
/// This is similar to the macro from `memoffset`, however it's fully well
/// defined even in current versions of Rust (and uses no unsafe code).
///
/// It does by using the `$instance` argument to have an already-initialized
/// instance of `$Type` rather than trying to find a way access the fields of an
/// uninitialized one without hitting soundness problems. The value passed to
/// the macro is referenced but not moved.
///
/// This means the API is more limited, but it's also sound even in rather
/// extreme cases, like some of the examples.
///
/// ## Caveats
///
/// 1. The offset is in bytes, and so you will likely have to cast your base
///    pointers to `*const u8`/`*mut u8` before getting field addresses.
///
/// 2. The offset values of repr(Rust) types are not stable, and may change
///    wildly between releases of the compiler. Use repr(C) if you can.
///
/// 3. The value of the `$instance` parameter has no bearing on the output of
///    this macro. It is just used to avoid soundness problems. The only
///    requirement is that it be initialized. In particular, the value returned
///    is not a field pointer, or anything like that.
///
/// ## Examples
///
/// ### Use with zeroable types
/// A common requirement in GPU apis is to specify the layout of vertices. These
/// will generally be [`Zeroable`] (if not [`Pod`]), and are a good fit for
/// `offset_of!`.
/// ```
/// # use bytemuck::{Zeroable, offset_of};
/// #[repr(C)]
/// struct Vertex {
///   pos: [f32; 2],
///   uv: [u16; 2],
///   color: [u8; 4],
/// }
/// unsafe impl Zeroable for Vertex {}
///
/// let pos = offset_of!(Zeroable::zeroed(), Vertex, pos);
/// let uv = offset_of!(Zeroable::zeroed(), Vertex, uv);
/// let color = offset_of!(Zeroable::zeroed(), Vertex, color);
///
/// assert_eq!(pos, 0);
/// assert_eq!(uv, 8);
/// assert_eq!(color, 12);
/// ```
///
/// ### Use with other types
///
/// More esoteric uses are possible too, including with types generally not safe
/// to otherwise use with bytemuck. `Strings`, `Vec`s, etc.
///
/// ```
/// #[derive(Default)]
/// struct Foo {
///   a: u8,
///   b: &'static str,
///   c: i32,
/// }
///
/// let a_offset = bytemuck::offset_of!(Default::default(), Foo, a);
/// let b_offset = bytemuck::offset_of!(Default::default(), Foo, b);
/// let c_offset = bytemuck::offset_of!(Default::default(), Foo, c);
///
/// assert_ne!(a_offset, b_offset);
/// assert_ne!(b_offset, c_offset);
/// // We can't check against hardcoded values for a repr(Rust) type,
/// // but prove to ourself this way.
///
/// let foo = Foo::default();
/// // Note: offsets are in bytes.
/// let as_bytes = &foo as *const _ as *const u8;
///
/// // we're using wrapping_offset here becasue it's not worth
/// // the unsafe block, but it would be valid to use `add` instead,
/// // as it cannot overflow.
/// assert_eq!(&foo.a as *const _ as usize, as_bytes.wrapping_add(a_offset) as usize);
/// assert_eq!(&foo.b as *const _ as usize, as_bytes.wrapping_add(b_offset) as usize);
/// assert_eq!(&foo.c as *const _ as usize, as_bytes.wrapping_add(c_offset) as usize);
/// ```
#[macro_export]
macro_rules! offset_of {
  ($instance:expr, $Type:path, $field:tt) => {{
    // This helps us guard against field access going through a Deref impl.
    #[allow(clippy::unneeded_field_pattern)]
    let $Type { $field: _, .. };
    let reference: &$Type = &$instance;
    let address = reference as *const _ as usize;
    let field_pointer = &reference.$field as *const _ as usize;
    // These asserts/unwraps are compiled away at release, and defend against
    // the case where somehow a deref impl is still invoked.
    let result = field_pointer.checked_sub(address).unwrap();
    assert!(result <= $crate::__core::mem::size_of::<$Type>());
    result
  }};
}
