//! Checked versions of the casting functions exposed in crate root
//! that support [`CheckedCastFromPod`] types.

use crate::{internal, NoPadding, Pod};

/// Marker trait for a type that is [`Pod`] for *at least some* bit patterns
/// of its underlying data.
///
/// [`Pod`] is a superset of [`CheckedCastFromPod`], meaning that any `T: Pod` is also [`CheckedCastFromPod`]. If it's possible,
/// prefer implementing [`Pod`] for your type directly instead of [`CheckedCastFromPod`] as it gives greater flexibility.
///
/// The point of this trait is to allow some of the benefits of the [`Pod`] trait for
/// types that are [`NoPadding`] but not [`Zeroable`]. This is particularly
/// useful for types like fieldless ('C-style') enums, [`char`], and structs containing them.
///
/// To do this, we define a `PodTy` which is a type with equivalent layout
/// to `Self` other than the invalid bit patterns which disallow `Self` from
/// being [`Zeroable`]. This `PodTy` must itself implement [`Pod`] (and therefore
/// [`Zeroable`]). Then, we implement a function that checks wheter a certain instance
/// of the `PodTy` is also a valid bit pattern of `Self`. If this check passes, then we
/// can allow casting from the `PodTy` to `Self` (and therefore also any type which
/// is able to be cast to `PodTy` is also able to be cast to `Self`).
///
/// # Derive
///
/// A `#[derive(CheckedCastFromPod)]` macro is provided under the `derive` feature flag which will
/// automatically validate the requirements of this trait and implement the
/// trait for you for both enums and structs. This is the recommended method for
/// implementing the trait, however it's also possible to do manually.
///
/// # Example
///
/// If manually implementing the trait, we can do something like so:
///
/// ```rust
/// use bytemuck::{CheckedCastFromPod, NoPadding};
///
/// #[repr(u32)]
/// #[derive(Copy, Clone)]
/// enum MyEnum {
///     Variant0 = 0,
///     Variant1 = 1,
///     Variant2 = 2,
/// }
///
/// // Since `CheckedCastFromPod` is a subtrait of `NoPadding`, we must
/// // also implement it on our type. See the docs of that trait for more.
/// unsafe impl NoPadding for MyEnum {}
///
/// unsafe impl CheckedCastFromPod for MyEnum {
///     type PodTy = u32;
///
///     fn cast_is_valid(pod: &u32) -> bool {
///         matches!(*pod, 0 | 1 | 2)
///     }
/// }
/// ```
///
/// We can now use relevant casting functions. For example,
///
/// ```rust
/// # use bytemuck::{CheckedCastFromPod, NoPadding};
/// # #[repr(u32)]
/// # #[derive(Copy, Clone, PartialEq, Eq, Debug)]
/// # enum MyEnum {
/// #     Variant0 = 0,
/// #     Variant1 = 1,
/// #     Variant2 = 2,
/// # }
/// # unsafe impl NoPadding for MyEnum {}
/// # unsafe impl CheckedCastFromPod for MyEnum {
/// #     type PodTy = u32;
/// #     fn cast_is_valid(pod: &u32) -> bool {
/// #         matches!(*pod, 0 | 1 | 2)
/// #     }
/// # }
/// use bytemuck::bytes_of;
/// use bytemuck::checked::try_from_bytes;
///
/// let bytes = bytes_of(&2u32);
/// let result = try_from_bytes::<MyEnum>(bytes);
/// assert_eq!(result, Ok(&MyEnum::Variant2));
///
/// // Fails for non-valid discriminant
/// let bytes = bytes_of(&100u32);
/// let result = try_from_bytes::<MyEnum>(bytes);
/// assert!(matches!(result, Err(_)));
/// ```
///
/// # Safety
///
/// * `Self` *must* have the same layout as the specified `PodTy` except for
/// the possible invalid bit patterns being checked during [`cast_is_valid`].
/// * If [`cast_is_valid`] returns true, then it must be valid to cast `pod`
/// as `&Self` (i.e. the bit pattern has been checked to be valid)
///
/// [`Zeroable`]: crate::Zeroable
/// [`cast_is_valid`]: CheckedCastFromPod::cast_is_valid
pub unsafe trait CheckedCastFromPod: NoPadding {
  /// `Self` *must* have the same layout as the specified `PodTy` except for
  /// the possible invalid bit patterns being checked during [`cast_is_valid`].
  ///
  /// [`cast_is_valid`]: CheckedCastFromPod::cast_is_valid
  type PodTy: Pod;

  /// If this function returns true, then it must be valid to reinterpret `pod` as `&Self`.
  fn cast_is_valid(pod: &Self::PodTy) -> bool;
}

unsafe impl<T: Pod> CheckedCastFromPod for T {
  type PodTy = T;

  #[inline(always)]
  fn cast_is_valid(_pod: &T) -> bool {
    true
  }
}

unsafe impl CheckedCastFromPod for char {
  type PodTy = u32;

  #[inline]
  fn cast_is_valid(pod: &Self::PodTy) -> bool {
    char::from_u32(*pod).is_some()
  }
}

/// The things that can go wrong when casting between [`CheckedCastFromPod`] data forms.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum CheckedCastError {
  /// An error occurred during a true-[`Pod`] cast
  PodCastError(crate::PodCastError),
  /// When casting to a [`CheckedCastFromPod`] type, it is possible that the original
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
pub fn checked_from_bytes<T: CheckedCastFromPod>(
  s: &[u8],
) -> Result<&T, CheckedCastError> {
  let pod = unsafe { internal::try_from_bytes(s)? };

  if <T as CheckedCastFromPod>::cast_is_valid(pod) {
    Ok(unsafe { &*(pod as *const <T as CheckedCastFromPod>::PodTy as *const T) })
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
pub fn checked_from_bytes_mut<T: CheckedCastFromPod>(
  s: &mut [u8],
) -> Result<&mut T, CheckedCastError> {
  let pod = unsafe { internal::try_from_bytes_mut(s)? };

  if <T as CheckedCastFromPod>::cast_is_valid(pod) {
    Ok(unsafe { &mut *(pod as *mut <T as CheckedCastFromPod>::PodTy as *mut T) })
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
pub fn checked_cast<A: NoPadding, B: CheckedCastFromPod>(
  a: A,
) -> Result<B, CheckedCastError> {
  let pod = unsafe { internal::try_cast(a) }?;

  if <B as CheckedCastFromPod>::cast_is_valid(&pod) {
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
pub fn checked_cast_ref<A: NoPadding, B: CheckedCastFromPod>(
  a: &A,
) -> Result<&B, CheckedCastError> {
  let pod = unsafe { internal::try_cast_ref(a) }?;

  if <B as CheckedCastFromPod>::cast_is_valid(pod) {
    Ok(unsafe { &*(pod as *const <B as CheckedCastFromPod>::PodTy as *const B) })
  } else {
    Err(CheckedCastError::InvalidBitPattern)
  }
}

/// Try to convert a `&mut T` into `&mut U`.
///
/// As [`checked_cast_ref`], but `mut`.
#[inline]
pub fn checked_cast_mut<A: Pod, B: CheckedCastFromPod>(
  a: &mut A,
) -> Result<&mut B, CheckedCastError> {
  let pod = unsafe { internal::try_cast_mut(a) }?;

  if <B as CheckedCastFromPod>::cast_is_valid(pod) {
    Ok(unsafe { &mut *(pod as *mut <B as CheckedCastFromPod>::PodTy as *mut B) })
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
pub fn checked_cast_slice<A: NoPadding, B: CheckedCastFromPod>(
  a: &[A],
) -> Result<&[B], CheckedCastError> {
  let pod = unsafe { internal::try_cast_slice(a) }?;

  if pod.iter().all(|pod| <B as CheckedCastFromPod>::cast_is_valid(pod)) {
    Ok(unsafe {
      core::slice::from_raw_parts(pod.as_ptr() as *const B, pod.len())
    })
  } else {
    Err(CheckedCastError::InvalidBitPattern)
  }
}

/// Try to convert `&mut [A]` into `&mut [B]` (possibly with a change in
/// length).
///
/// As [`checked_cast_slice`], but `&mut`.
#[inline]
pub fn checked_cast_slice_mut<A: Pod, B: CheckedCastFromPod>(
  a: &mut [A],
) -> Result<&mut [B], CheckedCastError> {
  let pod = unsafe { internal::try_cast_slice_mut(a) }?;

  if pod.iter().all(|pod| <B as CheckedCastFromPod>::cast_is_valid(pod)) {
    Ok(unsafe {
      core::slice::from_raw_parts_mut(pod.as_ptr() as *mut B, pod.len())
    })
  } else {
    Err(CheckedCastError::InvalidBitPattern)
  }
}
