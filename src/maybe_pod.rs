use crate::{NoPadding, Pod};

/// Marker trait for a type that is [`Pod`] for *at least some* bit patterns
/// of its underlying data.
///
/// [`Pod`] is a superset of [`MaybePod`], meaning that any `T: Pod` is also [`MaybePod`]. If it's possible,
/// prefer implementing [`Pod`] for your type directly instead of [`MaybePod`] as it gives greater flexibility.
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
/// A `#[derive(MaybePod)]` macro is provided under the `derive` feature flag which will
/// automatically validate the requirements of this trait and implement the
/// trait for you for both enums and structs. This is the recommended method for
/// implementing the trait, however it's also possible to do manually.
///
/// # Example
///
/// If manually implementing the trait, we can do something like so:
///
/// ```rust
/// use bytemuck::{MaybePod, NoPadding};
///
/// #[repr(u32)]
/// #[derive(Copy, Clone)]
/// enum MyEnum {
///     Variant0 = 0,
///     Variant1 = 1,
///     Variant2 = 2,
/// }
///
/// // Since `MaybePod` is a subtrait of `NoPadding`, we must
/// // also implement it on our type. See the docs of that trait for more.
/// unsafe impl NoPadding for MyEnum {}
///
/// unsafe impl MaybePod for MyEnum {
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
/// # use bytemuck::{MaybePod, NoPadding};
/// # #[repr(u32)]
/// # #[derive(Copy, Clone, PartialEq, Eq, Debug)]
/// # enum MyEnum {
/// #     Variant0 = 0,
/// #     Variant1 = 1,
/// #     Variant2 = 2,
/// # }
/// # unsafe impl NoPadding for MyEnum {}
/// # unsafe impl MaybePod for MyEnum {
/// #     type PodTy = u32;
/// #     fn cast_is_valid(pod: &u32) -> bool {
/// #         matches!(*pod, 0 | 1 | 2)
/// #     }
/// # }
/// use bytemuck::{bytes_of, try_from_bytes};
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
/// [`cast_is_valid`]: MaybePod::cast_is_valid
pub unsafe trait MaybePod: NoPadding {
  /// `Self` *must* have the same layout as the specified `PodTy` except for
  /// the possible invalid bit patterns being checked during [`cast_is_valid`].
  ///
  /// [`cast_is_valid`]: MaybePod::cast_is_valid
  type PodTy: Pod;

  /// If this function returns true, then it must be valid to reinterpret `pod` as `&Self`.
  fn cast_is_valid(pod: &Self::PodTy) -> bool;
}

unsafe impl<T: Pod> MaybePod for T {
  type PodTy = T;

  #[inline(always)]
  fn cast_is_valid(_pod: &T) -> bool {
    true
  }
}

unsafe impl MaybePod for char {
  type PodTy = u32;

  #[inline]
  fn cast_is_valid(pod: &Self::PodTy) -> bool {
    char::from_u32(*pod).is_some()
  }
}
