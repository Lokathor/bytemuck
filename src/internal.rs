//! Internal implementation of casting functions not bound by marker traits
//! and therefore marked as unsafe. This is used so that we don't need to
//! duplicate the business logic contained in these functions between the
//! versions exported in the crate root, `checked`, and `relaxed` modules.
#![allow(unused_unsafe)]

use crate::PodCastError;
use core::{marker::*, mem::*};

/*

Note(Lokathor): We've switched all of the `unwrap` to `match` because there is
apparently a bug: https://github.com/rust-lang/rust/issues/68667
and it doesn't seem to show up in simple godbolt examples but has been reported
as having an impact when there's a cast mixed in with other more complicated
code around it. Rustc/LLVM ends up missing that the `Err` can't ever happen for
particular type combinations, and then it doesn't fully eliminated the panic
possibility code branch.

*/

/// Immediately panics.
#[cfg(not(target_arch = "spirv"))]
#[cold]
#[inline(never)]
pub(crate) fn something_went_wrong<D: core::fmt::Display>(
  _src: &str, _err: D,
) -> ! {
  // Note(Lokathor): Keeping the panic here makes the panic _formatting_ go
  // here too, which helps assembly readability and also helps keep down
  // the inline pressure.
  panic!("{src}>{err}", src = _src, err = _err);
}

/// Immediately panics.
#[cfg(target_arch = "spirv")]
#[cold]
#[inline(never)]
pub(crate) fn something_went_wrong<D>(_src: &str, _err: D) -> ! {
  // Note: On the spirv targets from [rust-gpu](https://github.com/EmbarkStudios/rust-gpu)
  // panic formatting cannot be used. We we just give a generic error message
  // The chance that the panicking version of these functions will ever get
  // called on spir-v targets with invalid inputs is small, but giving a
  // simple error message is better than no error message at all.
  panic!("Called a panicing helper from bytemuck which paniced");
}

/// # Safety
///
/// Must only be implemented on types where it is valid to pass a
/// pointer to from_raw that came from into_raw on a different instantiation of
/// the same ADT, e.g.
/// ```rs
/// let ptr: *mut U = <MyPointer<U> as CastablePointer<U>>::into_raw(my_pointer);
/// let new_ptr: *mut T = / cast ptr to *mut T upholding the invariants below */;
/// let my_new_pointer = <MyPointer<T> as CastablePointer<T>>::from_raw(new_ptr);
/// ```
///
/// For all implementors, it must be valid to pass the same pointer returned
/// from `Self::into_raw(self)` (uncasted) back to `Self::from_raw(ptr)` to get
/// `self` back.
///
/// Looser requirements are given by subtraits. In particular, changing the
/// pointee type is generally allowed as long as layout invariants and
/// bytemuck's castability trait invariants are upheld.
pub(crate) unsafe trait CastablePointer<T: ?Sized> {
  /// Return a pointer that can be given to `Self<U>::from_raw``, if all other
  /// invariants hold. This pointer should not dereferenced, as it may be
  /// invalid for anything other than passing to from_raw. (e.g. it may actually
  /// be a `*const T``, or it may point to a dropped T that has not been
  /// deallocated.)
  fn into_raw(self) -> *mut T;
  /// # Safety
  ///
  /// `ptr` must have come from CastablePointer::into_raw on the same ADT
  /// (e.g. `Box<T>` -> `Box<U>`, `&'a T` -> `&'a U`), and any additional
  /// invariants of the ADT must be upheld. See the ADT's particular
  /// `AdjectiveCastablePointer` impls' documentation for those invariants.
  unsafe fn from_raw(ptr: *mut T) -> Self;
}

/// This marker trait indicates that a [`CastablePointer`] is a borrowed
/// pointer.
///
/// # Safety
///
/// * `CastablePointer::from_raw` must accept pointers where the pointee has the
///   same size as the `into_raw`'d pointee, and the pointer is validly aligned
///   for `Self`'s pointee.
/// * Additional castability invariants on the pointee (e.g. NoUninit) must also
///   be upheld.
pub(crate) unsafe trait BorrowedCastablePointer {}

/// This marker trait indicates that a [`CastablePointer`] is an owned
/// pointer.
///
/// # Safety
///
/// * `from_raw` must accept pointers where the pointee has the same layout
///   (size and alignment) as the `into_raw`'d pointee.
/// * Additional castability invariants on the pointee (e.g. NoUninit) must also
///   be upheld.
#[cfg(feature = "extern_crate_alloc")]
pub(crate) unsafe trait OwnedCastablePointer {}

unsafe impl<'a, T: ?Sized> CastablePointer<T> for &'a T {
  #[inline]
  fn into_raw(self) -> *mut T {
    self as *const T as *mut T
  }

  #[inline]
  unsafe fn from_raw(ptr: *mut T) -> Self {
    &*(ptr as *const T)
  }
}

unsafe impl<'a, T: ?Sized> CastablePointer<T> for &'a mut T {
  #[inline]
  fn into_raw(self) -> *mut T {
    self as *mut T
  }

  #[inline]
  unsafe fn from_raw(ptr: *mut T) -> Self {
    &mut *ptr
  }
}

unsafe impl<'a, T: ?Sized> BorrowedCastablePointer for &'a T {}
unsafe impl<'a, T: ?Sized> BorrowedCastablePointer for &'a mut T {}

/// Attempts to cast the content type of an owned `CastablePointer<T>`.
///
/// On failure you get back an error along with the starting `CastablePointer`.
///
/// ## Failure
///
/// * The start and end pointee type must have the exact same size and
///   alignment.
///
/// ## Safety
///
/// * `AP` and `BP` must be instantiations of the same ADT with `A` and `B`,
///   respectively (e.g. `Box<A>` and `Box<B>`).
/// * The invariants on A and B (e.g. NoUninit) must be upheld by the caller.
#[cfg(feature = "extern_crate_alloc")]
#[inline]
pub(crate) unsafe fn try_cast_owned_ptr<
  A: Copy,
  B: Copy,
  AP: CastablePointer<A> + OwnedCastablePointer,
  BP: CastablePointer<B> + OwnedCastablePointer,
>(
  input: AP,
) -> Result<BP, (PodCastError, AP)> {
  if align_of::<A>() != align_of::<B>() {
    Err((PodCastError::AlignmentMismatch, input))
  } else if size_of::<A>() != size_of::<B>() {
    Err((PodCastError::SizeMismatch, input))
  } else {
    // Safety: we uphold the requirements of OwnedCastablePointer because `A`
    // and `B` have the same layout. Castability invariants on A
    // and B are upheld by our caller.
    unsafe {
      let ptr: *mut A = CastablePointer::into_raw(input);
      Ok(CastablePointer::from_raw(ptr as *mut B))
    }
  }
}

/// Try to convert an owned `CastablePointer<[A]>` into `CastablePointer<[B]>`
/// (possibly with a change in length).
///
/// * `input.as_ptr() as usize == output.as_ptr() as usize`
/// * `input.len() * size_of::<A>() == output.len() * size_of::<B>()`
///
/// ## Failure
///
/// * The start and end content type of the `CastablePointer<[T]>` must have the
///   exact same alignment.
/// * The start and end content size in bytes of the `CastablePointer<[T]>` must
///   be the exact same.
///
/// ## Safety
///
/// * `AP` and `BP` must be instantiations of the same ADT (e.g. `Box<[A]>` and
///   `Box<[B]>`).
/// * The castabilty invariants on A and B, if any (e.g. NoUninit) must be
///   upheld by the caller.
///
/// TODO: if we want to support Weak, we'd have to remove the Deref bound and
/// get the length from the pointer directly, but that is not possible in
/// bytemuck's MSRV, so would have to be under a feature flag.
#[cfg(feature = "extern_crate_alloc")]
#[inline]
pub(crate) unsafe fn try_cast_owned_slice_ptr<
  A: Copy,
  B: Copy,
  AP: CastablePointer<[A]> + OwnedCastablePointer + core::ops::Deref<Target = [A]>,
  BP: CastablePointer<[B]> + OwnedCastablePointer,
>(
  input: AP,
) -> Result<BP, (PodCastError, AP)> {
  if align_of::<A>() != align_of::<B>() {
    Err((PodCastError::AlignmentMismatch, input))
  } else if size_of::<A>() != size_of::<B>() {
    let input_bytes = size_of_val::<[A]>(&*input);
    if (size_of::<B>() == 0 && input_bytes != 0)
      || (size_of::<B>() != 0 && input_bytes % size_of::<B>() != 0)
    {
      // If the size in bytes of the underlying buffer does not match an exact
      // multiple of the size of B, we cannot cast between them.
      Err((PodCastError::OutputSliceWouldHaveSlop, input))
    } else {
      // Because the size is an exact multiple, we can now change the length
      // of the slice and recreate the pointer.
      let length =
        if size_of::<B>() != 0 { input_bytes / size_of::<B>() } else { 0 };
      let ptr: *mut [A] = CastablePointer::into_raw(input);
      let ptr: *mut [B] =
        core::ptr::slice_from_raw_parts_mut(ptr as *mut B, length);
      // Safety: We uphold the requirements of OwnedCastablePointer because *ptr
      // has the same size as before, and `A` and `B` have the same alignemnt.
      unsafe { Ok(CastablePointer::from_raw(ptr)) }
    }
  } else {
    // Safety: we uphold the requirements of OwnedCastablePointer because `A`
    // and `B` have the same layout and we did not change the slice length.
    // Castability invariants on A and B are upheld by our caller.
    unsafe {
      let ptr: *mut [A] = CastablePointer::into_raw(input);
      Ok(CastablePointer::from_raw(ptr as *mut [B]))
    }
  }
}

/// Attempts to cast the content type of an `CastablePointer<T>`. The alignment
/// of `A` and `B` need not be the same as long as `input` is validly aligned
/// for `B`.
///
/// On failure you get back an error along with the starting `CastablePointer`.
///
/// ## Failure
///
/// * The start and end pointee type must have the exact same size.
///
/// ## Safety
///
/// * `AP` and `BP` must be instantiations of the same ADT with `A` and `B`,
///   respectively (e.g. `Box<A>` and `Box<B>`) such that `from_raw` is valid to
///   pass same-size types where the pointer is validly aligned for the
///   destination type.
/// * The pointer returned from `AP::into_raw` must be aligned for `A`.
/// * The invariants on A and B (e.g. NoUninit) must be upheld by the caller.
#[inline]
pub(crate) unsafe fn try_cast_borrowed_ptr<
  A: Copy,
  B: Copy,
  AP: CastablePointer<A> + BorrowedCastablePointer,
  BP: CastablePointer<B> + BorrowedCastablePointer,
>(
  input: AP,
) -> Result<BP, (PodCastError, AP)> {
  if size_of::<A>() != size_of::<B>() {
    Err((PodCastError::SizeMismatch, input))
  } else {
    let ptr: *mut A = CastablePointer::into_raw(input);
    // only do the alignment check if it could fail
    if align_of::<A>() >= align_of::<B>()
      || is_aligned_to(ptr as *const (), align_of::<B>())
    {
      // Safety: same size, and the pointer is aligned. Other invariants are
      // upheld by our caller.
      unsafe { Ok(CastablePointer::from_raw(ptr as *mut B)) }
    } else {
      // Cannot cast unaligned pointer to more-aligned type.
      // Safety: AP::from_raw(AP::into_raw(ptr)) is always safe.
      Err((PodCastError::TargetAlignmentGreaterAndInputNotAligned, unsafe {
        CastablePointer::from_raw(ptr)
      }))
    }
  }
}

/// Try to convert `CastablePointer<[A]>` into `CastablePointer<[B]>` (possibly
/// with a change in length). The alignment of `A` and `B` need not be the same
/// as long as `input` is validly aligned for `B`.
///
/// * `input.as_ptr() as usize == output.as_ptr() as usize`
/// * `input.len() * size_of::<A>() == output.len() * size_of::<B>()`
///
/// ## Failure
///
/// * The start and end content size in bytes of the `CastablePointer<[T]>` must
///   be the exact same.
///
/// ## Safety
///
/// * `AP` and `BP` must be instantiations of the same ADT with `[A]` and `[B]`
///   (e.g. `Box<[A]>` and `Box<[B]>`) such that `from_raw` is valid to pass
///   same-size types where the pointer is validly aligned for the destination
///   type.
/// * The pointer returned from `AP::into_raw` must be aligned for `A`.
/// * The invariants on A and B, if any (e.g. NoUninit) must be upheld by the
///   caller.
///
/// TODO: if we want to support Weak, we'd have to remove the Deref bound and
/// get the length from the pointer directly, but that is not possible in
/// bytemuck's MSRV, so would have to be under a feature flag.
#[inline]
unsafe fn try_cast_borrowed_slice_ptr<
  A: Copy,
  B: Copy,
  AP: CastablePointer<[A]>
    + BorrowedCastablePointer
    + core::ops::Deref<Target = [A]>,
  BP: CastablePointer<[B]> + BorrowedCastablePointer,
>(
  input: AP,
) -> Result<BP, (PodCastError, AP)> {
  if size_of::<A>() != size_of::<B>() {
    let input_bytes = size_of::<A>() * input.len();
    if (size_of::<B>() == 0 && input_bytes != 0)
      || (size_of::<B>() != 0 && input_bytes % size_of::<B>() != 0)
    {
      // If the size in bytes of the underlying buffer does not match an exact
      // multiple of the size of B, we cannot cast between them.
      Err((PodCastError::OutputSliceWouldHaveSlop, input))
    } else {
      let length = if size_of::<B>() != 0 {
        size_of::<A>() * input.len() / size_of::<B>()
      } else {
        0
      };
      let ptr: *mut [A] = CastablePointer::into_raw(input);
      // only do the alignment check if it could fail
      if align_of::<A>() >= align_of::<B>()
        || is_aligned_to(ptr as *const (), align_of::<B>())
      {
        // Because the size is an exact multiple, we can now change the length
        // of the slice and recreate the pointer.
        // NOTE: This is a valid operation because according to the contract of
        // CastablePointer, the size in bytes and type alignment must be the
        // same. The invariants on A and B must be upheld by our caller.
        let ptr: *mut [B] =
          core::ptr::slice_from_raw_parts_mut(ptr as *mut B, length);
        unsafe { Ok(CastablePointer::from_raw(ptr)) }
      } else {
        // Cannot cast unaligned pointer to more-aligned type.
        // Safety: AP::from_raw(AP::into_raw(ptr)) is always safe.
        Err((PodCastError::TargetAlignmentGreaterAndInputNotAligned, unsafe {
          CastablePointer::from_raw(ptr)
        }))
      }
    }
  } else {
    let ptr: *mut [A] = CastablePointer::into_raw(input);
    // only do the alignment check if it could fail
    if align_of::<A>() >= align_of::<B>()
      || is_aligned_to(ptr as *const (), align_of::<B>())
    {
      // Because the size is the same, we can recreate the pointer without
      // changing the length. NOTE: This is a valid operation because
      // according to the contract of CastablePointer, the size in bytes
      // and type alignment must be the same. The invariants on A and B
      // must be upheld by our caller.
      unsafe { Ok(CastablePointer::from_raw(ptr as *mut [B])) }
    } else {
      // Cannot cast unaligned pointer to more-aligned type.
      // Safety: AP::from_raw(AP::into_raw(ptr)) is always safe.
      Err((PodCastError::TargetAlignmentGreaterAndInputNotAligned, unsafe {
        CastablePointer::from_raw(ptr)
      }))
    }
  }
}

/// Re-interprets `&T` as `&[u8]`.
///
/// Any ZST becomes an empty slice, and in that case the pointer value of that
/// empty slice might not match the pointer value of the input reference.
#[inline(always)]
pub(crate) unsafe fn bytes_of<T: Copy>(t: &T) -> &[u8] {
  if size_of::<T>() == 0 {
    &[]
  } else {
    match try_cast_slice::<T, u8>(core::slice::from_ref(t)) {
      Ok(s) => s,
      Err(_) => unreachable!(),
    }
  }
}

/// Re-interprets `&mut T` as `&mut [u8]`.
///
/// Any ZST becomes an empty slice, and in that case the pointer value of that
/// empty slice might not match the pointer value of the input reference.
#[inline]
pub(crate) unsafe fn bytes_of_mut<T: Copy>(t: &mut T) -> &mut [u8] {
  if size_of::<T>() == 0 {
    &mut []
  } else {
    match try_cast_slice_mut::<T, u8>(core::slice::from_mut(t)) {
      Ok(s) => s,
      Err(_) => unreachable!(),
    }
  }
}

/// Re-interprets `&[u8]` as `&T`.
///
/// ## Panics
///
/// This is [`try_from_bytes`] but will panic on error.
#[inline]
pub(crate) unsafe fn from_bytes<T: Copy>(s: &[u8]) -> &T {
  match try_from_bytes(s) {
    Ok(t) => t,
    Err(e) => something_went_wrong("from_bytes", e),
  }
}

/// Re-interprets `&mut [u8]` as `&mut T`.
///
/// ## Panics
///
/// This is [`try_from_bytes_mut`] but will panic on error.
#[inline]
pub(crate) unsafe fn from_bytes_mut<T: Copy>(s: &mut [u8]) -> &mut T {
  match try_from_bytes_mut(s) {
    Ok(t) => t,
    Err(e) => something_went_wrong("from_bytes_mut", e),
  }
}

/// Reads from the bytes as if they were a `T`.
///
/// ## Failure
/// * If the `bytes` length is not equal to `size_of::<T>()`.
#[inline]
pub(crate) unsafe fn try_pod_read_unaligned<T: Copy>(
  bytes: &[u8],
) -> Result<T, PodCastError> {
  if bytes.len() != size_of::<T>() {
    Err(PodCastError::SizeMismatch)
  } else {
    Ok(unsafe { (bytes.as_ptr() as *const T).read_unaligned() })
  }
}

/// Reads the slice into a `T` value.
///
/// ## Panics
/// * This is like `try_pod_read_unaligned` but will panic on failure.
#[inline]
pub(crate) unsafe fn pod_read_unaligned<T: Copy>(bytes: &[u8]) -> T {
  match try_pod_read_unaligned(bytes) {
    Ok(t) => t,
    Err(e) => something_went_wrong("pod_read_unaligned", e),
  }
}

/// Checks if `ptr` is aligned to an `align` memory boundary.
///
/// ## Panics
/// * If `align` is not a power of two. This includes when `align` is zero.
#[inline]
pub(crate) fn is_aligned_to(ptr: *const (), align: usize) -> bool {
  #[cfg(feature = "align_offset")]
  {
    // This is in a way better than `ptr as usize % align == 0`,
    // because casting a pointer to an integer has the side effect that it
    // exposes the pointer's provenance, which may theoretically inhibit
    // some compiler optimizations.
    ptr.align_offset(align) == 0
  }
  #[cfg(not(feature = "align_offset"))]
  {
    ((ptr as usize) % align) == 0
  }
}

/// Re-interprets `&[u8]` as `&T`.
///
/// ## Failure
///
/// * If the slice isn't aligned for the new type
/// * If the slice's length isn’t exactly the size of the new type
#[inline]
pub(crate) unsafe fn try_from_bytes<T: Copy>(
  s: &[u8],
) -> Result<&T, PodCastError> {
  if s.len() != size_of::<T>() {
    Err(PodCastError::SizeMismatch)
  } else if !is_aligned_to(s.as_ptr() as *const (), align_of::<T>()) {
    Err(PodCastError::TargetAlignmentGreaterAndInputNotAligned)
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
pub(crate) unsafe fn try_from_bytes_mut<T: Copy>(
  s: &mut [u8],
) -> Result<&mut T, PodCastError> {
  if s.len() != size_of::<T>() {
    Err(PodCastError::SizeMismatch)
  } else if !is_aligned_to(s.as_ptr() as *const (), align_of::<T>()) {
    Err(PodCastError::TargetAlignmentGreaterAndInputNotAligned)
  } else {
    Ok(unsafe { &mut *(s.as_mut_ptr() as *mut T) })
  }
}

/// Cast `T` into `U`
///
/// ## Panics
///
/// * This is like [`try_cast`](try_cast), but will panic on a size mismatch.
#[inline]
pub(crate) unsafe fn cast<A: Copy, B: Copy>(a: A) -> B {
  if size_of::<A>() == size_of::<B>() {
    unsafe { transmute!(a) }
  } else {
    something_went_wrong("cast", PodCastError::SizeMismatch)
  }
}

/// Cast `&mut T` into `&mut U`.
///
/// ## Panics
///
/// This is [`try_cast_mut`] but will panic on error.
#[inline]
pub(crate) unsafe fn cast_mut<A: Copy, B: Copy>(a: &mut A) -> &mut B {
  if size_of::<A>() == size_of::<B>() && align_of::<A>() >= align_of::<B>() {
    // Plz mr compiler, just notice that we can't ever hit Err in this case.
    match try_cast_mut(a) {
      Ok(b) => b,
      Err(_) => unreachable!(),
    }
  } else {
    match try_cast_mut(a) {
      Ok(b) => b,
      Err(e) => something_went_wrong("cast_mut", e),
    }
  }
}

/// Cast `&T` into `&U`.
///
/// ## Panics
///
/// This is [`try_cast_ref`] but will panic on error.
#[inline]
pub(crate) unsafe fn cast_ref<A: Copy, B: Copy>(a: &A) -> &B {
  if size_of::<A>() == size_of::<B>() && align_of::<A>() >= align_of::<B>() {
    // Plz mr compiler, just notice that we can't ever hit Err in this case.
    match try_cast_ref(a) {
      Ok(b) => b,
      Err(_) => unreachable!(),
    }
  } else {
    match try_cast_ref(a) {
      Ok(b) => b,
      Err(e) => something_went_wrong("cast_ref", e),
    }
  }
}

/// Cast `&[A]` into `&[B]`.
///
/// ## Panics
///
/// This is [`try_cast_slice`] but will panic on error.
#[inline]
pub(crate) unsafe fn cast_slice<A: Copy, B: Copy>(a: &[A]) -> &[B] {
  match try_cast_slice(a) {
    Ok(b) => b,
    Err(e) => something_went_wrong("cast_slice", e),
  }
}

/// Cast `&mut [T]` into `&mut [U]`.
///
/// ## Panics
///
/// This is [`try_cast_slice_mut`] but will panic on error.
#[inline]
pub(crate) unsafe fn cast_slice_mut<A: Copy, B: Copy>(a: &mut [A]) -> &mut [B] {
  match try_cast_slice_mut(a) {
    Ok(b) => b,
    Err(e) => something_went_wrong("cast_slice_mut", e),
  }
}

/// Try to cast `T` into `U`.
///
/// Note that for this particular type of cast, alignment isn't a factor. The
/// input value is semantically copied into the function and then returned to a
/// new memory location which will have whatever the required alignment of the
/// output type is.
///
/// ## Failure
///
/// * If the types don't have the same size this fails.
#[inline]
pub(crate) unsafe fn try_cast<A: Copy, B: Copy>(
  a: A,
) -> Result<B, PodCastError> {
  if size_of::<A>() == size_of::<B>() {
    Ok(unsafe { transmute!(a) })
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
pub(crate) unsafe fn try_cast_ref<A: Copy, B: Copy>(
  a: &A,
) -> Result<&B, PodCastError> {
  // Safety: `&A` and `&B` are valid to cast between under the requirements of
  // try_cast_ptr. Additional invariants on `A` and `B` are upheld by our
  // caller.
  unsafe { try_cast_borrowed_ptr(a) }.map_err(|(err, _)| err)
}

/// Try to convert a `&mut T` into `&mut U`.
///
/// As [`try_cast_ref`], but `mut`.
#[inline]
pub(crate) unsafe fn try_cast_mut<A: Copy, B: Copy>(
  a: &mut A,
) -> Result<&mut B, PodCastError> {
  // Safety: `&mut A` and `&mut B` are valid to cast between under the
  // requirements of try_cast_ptr. Additional invariants on `A` and `B` are
  // upheld by our caller.
  unsafe { try_cast_borrowed_ptr(a) }.map_err(|(err, _)| err)
}

/// Try to convert `&[A]` into `&[B]` (possibly with a change in length).
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
/// * Similarly, you can't convert between a [ZST](https://doc.rust-lang.org/nomicon/exotic-sizes.html#zero-sized-types-zsts)
///   and a non-ZST.
#[inline]
pub(crate) unsafe fn try_cast_slice<A: Copy, B: Copy>(
  a: &[A],
) -> Result<&[B], PodCastError> {
  // Safety: `&[A]` and `&[B]` are valid to cast between under the requirements
  // of try_cast_ptr. Additional invariants on `A` and `B` are upheld by our
  // caller.
  unsafe { try_cast_borrowed_slice_ptr(a) }.map_err(|(err, _)| err)
}

/// Try to convert `&mut [A]` into `&mut [B]` (possibly with a change in
/// length).
///
/// As [`try_cast_slice`], but `&mut`.
#[inline]
pub(crate) unsafe fn try_cast_slice_mut<A: Copy, B: Copy>(
  a: &mut [A],
) -> Result<&mut [B], PodCastError> {
  // Safety: `&mut [A]` and `&mut [B]` are valid to cast between under the
  // requirements of try_cast_ptr. Additional invariants on `A` and `B` are
  // upheld by our caller.
  unsafe { try_cast_borrowed_slice_ptr(a) }.map_err(|(err, _)| err)
}
