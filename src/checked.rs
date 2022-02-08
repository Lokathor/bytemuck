//! Checked versions of the casting functions exposed in crate root
//! that support [`MaybePod`] types.

use crate::{internal, Pod, MaybePod, NoPadding};

/// The things that can go wrong when casting between [`MaybePod`] data forms.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum CheckedCastError {
  /// An error occurred during a true-[`Pod`] cast
  PodCastError(crate::PodCastError),
  /// When casting to a [`MaybePod`] type, it is possible that the original
  /// data contains an invalid bit pattern. If so, the cast will fail and
  /// this error will be returned. Will never happen on casts between always
  /// [`Pod`] types.
  InvalidBitPattern,
}

#[cfg(not(target_arch = "spirv"))]
impl core::fmt::Display for CheckedCastError {
  fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
    write!(f, "{:?}", self)
  }
}
#[cfg(feature = "extern_crate_std")]
impl std::error::Error for CheckedCastError {}

impl From<crate::PodCastError> for CheckedCastError {
  fn from(err: crate::PodCastError) -> CheckedCastError {
    CheckedCastError::PodCastError(err)
  }
}

/// Re-interprets `&[u8]` as `&T`.
///
/// ## Failure
///
/// * If the slice isn't aligned for the new type
/// * If the slice's length isn’t exactly the size of the new type
/// * If the slice contains an invalid bit pattern for `T`
#[inline]
pub fn checked_from_bytes<T: MaybePod>(s: &[u8]) -> Result<&T, CheckedCastError> {
  let pod = unsafe { internal::try_from_bytes(s)? };

  if <T as MaybePod>::cast_is_valid(pod) {
    Ok(unsafe { &*(pod as *const <T as MaybePod>::PodTy as *const T) })
  } else {
    Err(CheckedCastError::InvalidBitPattern)
  }
}

/// Re-interprets `&mut [u8]` as `&mut T`.
///
/// ## Failure
///
/// * If the slice isn't aligned for the new type
/// * If the slice's length isn’t exactly the size of the new type
/// * If the slice contains an invalid bit pattern for `T`
#[inline]
pub fn try_from_bytes_mut<T: MaybePod>(
  s: &mut [u8],
) -> Result<&mut T, CheckedCastError> {
  let pod = unsafe { internal::try_from_bytes_mut(s)? };

  if <T as MaybePod>::cast_is_valid(pod) {
    Ok(unsafe { &mut *(pod as *mut <T as MaybePod>::PodTy as *mut T) })
  } else {
    Err(CheckedCastError::InvalidBitPattern)
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
/// * If `a` contains an invalid bit pattern for `B` this fails.
#[inline]
pub fn try_cast<A: NoPadding, B: MaybePod>(a: A) -> Result<B, CheckedCastError> {
  let pod = unsafe { internal::try_cast(a) }?;

  if <B as MaybePod>::cast_is_valid(&pod) {
    Ok(unsafe { transmute!(pod) })
  } else {
    Err(CheckedCastError::InvalidBitPattern)
  }
}

/// Try to convert a `&T` into `&U`.
///
/// ## Failure
///
/// * If the reference isn't aligned in the new type
/// * If the source type and target type aren't the same size.
/// * If `a` contains an invalid bit pattern for `B` this fails.
#[inline]
pub fn try_cast_ref<A: NoPadding, B: MaybePod>(a: &A) -> Result<&B, CheckedCastError> {
  let pod = unsafe { internal::try_cast_ref(a) }?;

  if <B as MaybePod>::cast_is_valid(pod) {
    Ok(unsafe { &*(pod as *const <B as MaybePod>::PodTy as *const B) })
  } else {
    Err(CheckedCastError::InvalidBitPattern)
  }
}

/// Try to convert a `&mut T` into `&mut U`.
///
/// As [`try_cast_ref`], but `mut`.
#[inline]
pub fn try_cast_mut<A: Pod, B: MaybePod>(a: &mut A) -> Result<&mut B, CheckedCastError> {
  let pod = unsafe { internal::try_cast_mut(a) }?;

  if <B as MaybePod>::cast_is_valid(pod) {
    Ok(unsafe { &mut *(pod as *mut <B as MaybePod>::PodTy as *mut B) })
  } else {
    Err(CheckedCastError::InvalidBitPattern)
  }
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
/// * If any element of the converted slice would contain an invalid bit pattern for `B` this fails.
#[inline]
pub fn try_cast_slice<A: NoPadding, B: MaybePod>(a: &[A]) -> Result<&[B], CheckedCastError> {
  let pod = unsafe { internal::try_cast_slice(a) }?;

  if pod.iter().all(|pod| <B as MaybePod>::cast_is_valid(pod)) {
    Ok(unsafe { core::slice::from_raw_parts(pod.as_ptr() as *const B, pod.len()) })
  } else {
    Err(CheckedCastError::InvalidBitPattern)
  }
}

/// Try to convert `&mut [A]` into `&mut [B]` (possibly with a change in
/// length).
///
/// As [`try_cast_slice`], but `&mut`.
#[inline]
pub fn try_cast_slice_mut<A: Pod, B: MaybePod>(
  a: &mut [A],
) -> Result<&mut [B], CheckedCastError> {
  let pod = unsafe { internal::try_cast_slice_mut(a) }?;

  if pod.iter().all(|pod| <B as MaybePod>::cast_is_valid(pod)) {
    Ok(unsafe { core::slice::from_raw_parts_mut(pod.as_ptr() as *mut B, pod.len()) })
  } else {
    Err(CheckedCastError::InvalidBitPattern)
  }
}