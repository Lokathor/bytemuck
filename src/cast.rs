//! Safe byte-wise conversion between types.
//!
//! The public interface consist of a few traits:
//! * `Reinterpret` and `TryReinterpret` implement by-value conversion.
//! * `ReinterpretInner` and `TryReinterpretInner` implement in-place conversion
//!   for various container types.
//!
//! `ReinterpretInner` is backed by several helper traits:
//! * `Container` is a concrete container with a pointer to zero or more items.
//!   This trait handles several things:
//!   * Holding a tag type representing the containers type class. This prevents
//!     casts between incompatible containers and allows any additional
//!     constraints required by the container not handled by it's raw
//!     representation.
//!   * Conversion to and from a container's raw form. This allows the actual
//!     cast implementation to be shared between multiple containers.
//!   * Whether the original value should be returned in case of an error. This
//!     allows simplifying the return type of `try_reinterpret_inner` for `Copy`
//!     types.
//!   Examples of containers include references, raw pointers, `Box<T>` and
//!   `Vec<T>`.
//! * `AssertClassContraints` handles any additional constraints a container
//!   places on its items.
//! * `RawPointer` Gives a more unified interface for handling `T`, `[T]` and
//!   `str` for `Container` impls.
//! * `CastRaw` implements the conversion between a container's raw forms. This
//!   is responsible for verifying that the container's item types are
//!   compatible and the item's size/alignment constraints are met.
//! * `TryCastRaw` is the falliable form of `CastRaw`

use core::{
  marker::{PhantomData, Unpin},
  mem::{align_of, size_of, transmute_copy, ManuallyDrop},
  ops::Deref,
  pin::Pin,
  ptr::{self, NonNull},
  sync::atomic::AtomicPtr,
};

#[cfg(feature = "extern_crate_alloc")]
use alloc::{
  borrow::{Cow, ToOwned},
  boxed::Box,
  rc::{Rc, Weak as RcWeak},
  string::String,
  sync::{Arc, Weak as ArcWeak},
  vec::Vec,
};

use crate::{AnyBitPattern, CheckedBitPattern, NoUninit, PodCastError};

/// Safe byte-wise conversion of a value.
///
/// This requires the `unified_cast` feature to be enabled and a rust version
/// `>=1.57`.
pub trait Reinterpret<Dst>: Sized {
  #[doc(hidden)]
  const ASSERT: ();
  /// Performs the conversion.
  fn reinterpret(self) -> Dst;
}
impl<Src, Dst> Reinterpret<Dst> for Src
where
  Src: NoUninit,
  Dst: AnyBitPattern,
{
  const ASSERT: () = assert!(
    size_of::<Src>() == size_of::<Dst>(),
    "attempted conversion between types of different sizes"
  );

  #[allow(path_statements, clippy::no_effect)]
  fn reinterpret(self) -> Dst {
    <Self as Reinterpret<Dst>>::ASSERT;
    // SAFETY:
    // There are no uninitialized bytes in the source type.
    // All bit patterns are accepted by the target type.
    // Both types have the same size.
    unsafe {
      transmute_copy::<ManuallyDrop<Src>, Dst>(&ManuallyDrop::new(self))
    }
  }
}

/// Safe byte-wise conversion of a value which may fail due to the source value
/// not being a valid bit pattern for the target type.
///
/// This requires the `unified_cast` feature to be enabled and a rust version
/// `>=1.57`.
pub trait TryReinterpret<Dst>: Sized {
  #[doc(hidden)]
  const ASSERT: ();
  /// Performs the conversion.
  fn try_reinterpret(self) -> Option<Dst>;
}
impl<Src, Dst> TryReinterpret<Dst> for Src
where
  Src: NoUninit,
  Dst: CheckedBitPattern,
{
  const ASSERT: () = {
    assert!(
      size_of::<Src>() == size_of::<Dst>(),
      "attempted conversion between types of different sizes"
    );
    assert!(
      size_of::<Dst>() == size_of::<Dst::Bits>(),
      "target type has an invalid implementation of `CheckedBitPattern`"
    )
  };

  #[allow(path_statements, clippy::no_effect)]
  fn try_reinterpret(self) -> Option<Dst> {
    <Self as TryReinterpret<Dst>>::ASSERT;
    let bits = unsafe {
      transmute_copy::<ManuallyDrop<Src>, Dst::Bits>(&ManuallyDrop::new(self))
    };
    if Dst::is_valid_bit_pattern(&bits) {
      // SAFETY:
      // There are no uninitialized bytes in the source type.
      // The value has been confirmed to be a valid bit pattern for the target type.
      // Both types have the same size.
      Some(unsafe {
        transmute_copy::<ManuallyDrop<Dst::Bits>, Dst>(&ManuallyDrop::new(bits))
      })
    } else {
      None
    }
  }
}

pub trait ItemLayout {
  const SIZE: usize;
  const ALIGN: usize;
}
impl<T> ItemLayout for T {
  const SIZE: usize = size_of::<T>();
  const ALIGN: usize = align_of::<T>();
}
impl<T> ItemLayout for [T] {
  const SIZE: usize = size_of::<T>();
  const ALIGN: usize = align_of::<T>();
}
impl ItemLayout for str {
  const SIZE: usize = 1;
  const ALIGN: usize = 1;
}

// Container classes.
pub struct RefT;
pub struct PtrT;
pub struct NonNullT;
pub struct AtomicPtrT;
pub struct OptionT<C>(PhantomData<C>);
pub struct PinT<C>(PhantomData<C>);
#[cfg(feature = "extern_crate_alloc")]
pub struct BoxT;
#[cfg(feature = "extern_crate_alloc")]
pub struct CowT;
#[cfg(feature = "extern_crate_alloc")]
pub struct RcT;
#[cfg(feature = "extern_crate_alloc")]
pub struct RcWeakT;
#[cfg(feature = "extern_crate_alloc")]
pub struct ArcT;
#[cfg(feature = "extern_crate_alloc")]
pub struct ArcWeakT;
#[cfg(feature = "extern_crate_alloc")]
pub struct VecT;

/// Policy trait for whether the original value should be returned with the
/// error.
pub trait CastErrWithValue<T> {
  /// The error type returned.
  type Err;
  /// Combines the original value with the error.
  fn cast_error_with_value(err: PodCastError, value: T) -> Self::Err;
}
/// Return the error without the original value.
pub struct OnlyErr;
impl<T> CastErrWithValue<T> for OnlyErr {
  type Err = PodCastError;
  fn cast_error_with_value(err: PodCastError, _: T) -> Self::Err {
    err
  }
}
/// Return both the error and the original value.
pub struct ErrWithValue;
impl<T> CastErrWithValue<T> for ErrWithValue {
  type Err = (PodCastError, T);
  fn cast_error_with_value(err: PodCastError, value: T) -> Self::Err {
    (err, value)
  }
}

/// Like `*const [T]`, but the length can be retrieved safely.
pub struct RawSlice<T: ?Sized> {
  data: *const T,
  len: usize,
}
impl<T: ?Sized> Clone for RawSlice<T> {
  fn clone(&self) -> Self {
    *self
  }
}
impl<T: ?Sized> Copy for RawSlice<T> {}

/// Like `*mut [T]`, but the length can be retrieved safely.
pub struct RawMutSlice<T: ?Sized> {
  data: *mut T,
  len: usize,
}
impl<T: ?Sized> Clone for RawMutSlice<T> {
  fn clone(&self) -> Self {
    *self
  }
}
impl<T: ?Sized> Copy for RawMutSlice<T> {}

/// A single byte from a `str` slice.
#[derive(Clone, Copy)]
#[repr(transparent)]
pub struct StrByte(u8);
// SAFETY: A transparent wrapper over a single byte has no uninitialized bytes.
unsafe impl NoUninit for StrByte {}

/// Converts between a pointer type and it's raw form. This allows treating
/// various DST pointers similarly.
///
/// # Safety
/// Converting a pointer to and from it's raw form must result in the same
/// pointer.
pub unsafe trait RawPtr {
  /// The raw form of the pointer.
  type Raw: Copy;

  /// Performs the conversion to the raw form.
  ///
  /// # Safety
  /// For DST targets the pointer must be safe to materialize. If the
  /// `non_null_slice_cast` feature is enabled, then it must only be non-null.
  unsafe fn into_raw_ptr(self) -> Self::Raw;

  /// Performs the conversion from the raw form.
  ///
  /// # Safety
  /// The value must have been created from `into_raw_ptr` and possibly
  /// converted to a different type using `CastRaw` or `TryCastRaw`.
  unsafe fn from_raw_ptr(raw: Self::Raw) -> Self;
}
unsafe impl<T: Copy> RawPtr for *const T {
  type Raw = *const T;
  unsafe fn into_raw_ptr(self) -> Self::Raw {
    self
  }
  unsafe fn from_raw_ptr(raw: Self::Raw) -> Self {
    raw
  }
}
unsafe impl<T: Copy> RawPtr for *mut T {
  type Raw = *mut T;
  unsafe fn into_raw_ptr(self) -> Self::Raw {
    self
  }
  unsafe fn from_raw_ptr(raw: Self::Raw) -> Self {
    raw
  }
}
unsafe impl<T: Copy> RawPtr for *const [T] {
  type Raw = RawSlice<T>;
  #[cfg(feature = "non_null_slice_cast")]
  unsafe fn into_raw_ptr(self) -> Self::Raw {
    let len = NonNull::new_unchecked(self as *mut [T]).len();
    RawSlice { data: self as *const T, len }
  }
  #[cfg(not(feature = "non_null_slice_cast"))]
  unsafe fn into_raw_ptr(self) -> Self::Raw {
    let len = (*self).len();
    RawSlice { data: self as *const T, len }
  }
  unsafe fn from_raw_ptr(raw: Self::Raw) -> Self {
    ptr::slice_from_raw_parts(raw.data, raw.len)
  }
}
unsafe impl<T: Copy> RawPtr for *mut [T] {
  type Raw = RawMutSlice<T>;
  #[cfg(feature = "non_null_slice_cast")]
  unsafe fn into_raw_ptr(self) -> Self::Raw {
    let len = NonNull::new_unchecked(self).len();
    RawMutSlice { data: self as *mut T, len }
  }
  #[cfg(not(feature = "non_null_slice_cast"))]
  unsafe fn into_raw_ptr(self) -> Self::Raw {
    let len = (*self).len();
    RawMutSlice { data: self as *mut T, len }
  }
  unsafe fn from_raw_ptr(raw: Self::Raw) -> Self {
    ptr::slice_from_raw_parts_mut(raw.data, raw.len)
  }
}
unsafe impl RawPtr for *const str {
  type Raw = RawSlice<StrByte>;
  unsafe fn into_raw_ptr(self) -> Self::Raw {
    (self as *const [StrByte]).into_raw_ptr()
  }
  unsafe fn from_raw_ptr(raw: Self::Raw) -> Self {
    <*const [StrByte]>::from_raw_ptr(raw) as *const str
  }
}
unsafe impl RawPtr for *mut str {
  type Raw = RawMutSlice<StrByte>;
  unsafe fn into_raw_ptr(self) -> Self::Raw {
    (self as *mut [StrByte]).into_raw_ptr()
  }
  unsafe fn from_raw_ptr(raw: Self::Raw) -> Self {
    <*mut [StrByte]>::from_raw_ptr(raw) as *mut str
  }
}

/// A concrete container type. e.g `&'a T` or `Box<T>`
///
/// # Safety
/// * `Class` must be set such that calling `CastRaw` on this containers raw
///   form is valid.
/// * `Item` must match the contained item type.
/// * `into_raw` -> `CastRaw`/`TryCastRaw` -> `from_raw` must be valid.
pub unsafe trait Container<'a>: Sized {
  /// The type class of this container. Used to limit which raw casts should be
  type Class: AssertClassContraints<Self::Item, Self::Item>;
  /// The item type held within this container.
  type Item: 'a + ?Sized + ItemLayout;
  /// The 'raw' form of this container. Used to allow different containers to
  /// share the same `CastRaw` and `TryCastRaw` impls.
  type Raw: 'a + Copy;
  /// Whether the cast should return the original value along with the error.
  type CastErr: CastErrWithValue<Self>;

  /// Converts the container into it's raw form.
  fn into_raw(self) -> Self::Raw;

  /// Reconstructs the container from it's raw form.
  ///
  /// # Safety
  /// The values must have to come from `into_parts` of the same container
  /// class. Casting to a different item type must meet the following
  /// constraints:
  /// * Casting between zero-sized types and non-zero-sized types is forbidden.
  /// * The data pointer's alignment must meet the alignment constraints of it's
  ///   item type.
  /// * Size and alignment requirements of the container class must be met.
  /// * The additional data must be adjusted for the new type.
  unsafe fn from_raw(raw: Self::Raw) -> Self;
}

unsafe impl<'a, T> Container<'a> for &'a T
where
  T: 'a + ?Sized + ItemLayout,
  *const T: RawPtr,
{
  type Class = RefT;
  type Item = T;
  type Raw = <*const T as RawPtr>::Raw;
  type CastErr = OnlyErr;

  fn into_raw(self) -> Self::Raw {
    // SAFETY: Materializing pointers backed by a reference is safe.
    unsafe { (self as *const T).into_raw_ptr() }
  }
  unsafe fn from_raw(raw: Self::Raw) -> Self {
    &*<*const T>::from_raw_ptr(raw)
  }
}

unsafe impl<'a, T> Container<'a> for &'a mut T
where
  T: 'a + ?Sized + ItemLayout,
  *mut T: RawPtr,
{
  type Class = RefT;
  type Item = T;
  type Raw = <*mut T as RawPtr>::Raw;
  type CastErr = OnlyErr;

  fn into_raw(self) -> Self::Raw {
    // SAFETY: Materializing pointers backed by a reference is safe.
    unsafe { (self as *mut T).into_raw_ptr() }
  }
  unsafe fn from_raw(raw: Self::Raw) -> Self {
    &mut *<*mut T>::from_raw_ptr(raw)
  }
}

// No safe way to get the length of a slice. Only implement for sized types.
unsafe impl<'a, T: 'a> Container<'a> for *const T {
  type Class = PtrT;
  type Item = T;
  type Raw = Self;
  type CastErr = OnlyErr;

  fn into_raw(self) -> Self::Raw {
    self
  }
  unsafe fn from_raw(raw: Self::Raw) -> Self {
    raw
  }
}

// No safe way to get the length of a slice. Only implement for sized types.
unsafe impl<'a, T: 'a> Container<'a> for *mut T {
  type Class = PtrT;
  type Item = T;
  type Raw = Self;
  type CastErr = OnlyErr;

  fn into_raw(self) -> Self::Raw {
    self
  }
  unsafe fn from_raw(raw: Self::Raw) -> Self {
    raw
  }
}

unsafe impl<
    'a,
    #[cfg(feature = "non_null_slice_cast")] T: 'a + ?Sized + ItemLayout,
    #[cfg(not(feature = "non_null_slice_cast"))] T: 'a,
  > Container<'a> for NonNull<T>
where
  *mut T: RawPtr,
{
  type Class = NonNullT;
  type Item = T;
  type Raw = <*mut T as RawPtr>::Raw;
  type CastErr = OnlyErr;

  fn into_raw(self) -> Self::Raw {
    // SAFETY: We only attempt to get the length with the `non_null_slice_cast`
    // feature.
    unsafe { self.as_ptr().into_raw_ptr() }
  }
  unsafe fn from_raw(raw: Self::Raw) -> Self {
    NonNull::new_unchecked(<*mut T>::from_raw_ptr(raw))
  }
}

unsafe impl<'a, T: 'a> Container<'a> for AtomicPtr<T> {
  type Class = AtomicPtrT;
  type Item = T;
  type Raw = *mut T;
  type CastErr = OnlyErr;

  fn into_raw(self) -> Self::Raw {
    self.into_inner()
  }
  unsafe fn from_raw(raw: Self::Raw) -> Self {
    Self::new(raw)
  }
}

unsafe impl<'a, C> Container<'a> for Option<C>
where
  C: Container<'a>,
  C::CastErr: CastErrWithValue<Self>,
{
  type Class = OptionT<C::Class>;
  type Item = C::Item;
  type Raw = Option<C::Raw>;
  type CastErr = C::CastErr;

  fn into_raw(self) -> Self::Raw {
    self.map(|x| x.into_raw())
  }
  unsafe fn from_raw(raw: Self::Raw) -> Self {
    raw.map(|raw| C::from_raw(raw))
  }
}

// SAFETY: `Pin` has no safety requirements for types which deref to an `Unpin`
// type.
unsafe impl<'a, C> Container<'a> for Pin<C>
where
  C: Container<'a> + Deref<Target = <C as Container<'a>>::Item>,
  C::Item: Unpin,
  C::CastErr: CastErrWithValue<Self>,
{
  type Class = PinT<C::Class>;
  type Item = C::Item;
  type Raw = C::Raw;
  type CastErr = C::CastErr;

  fn into_raw(self) -> Self::Raw {
    Self::into_inner(self).into_raw()
  }
  unsafe fn from_raw(raw: Self::Raw) -> Self {
    Self::new(C::from_raw(raw))
  }
}

#[cfg(feature = "extern_crate_alloc")]
unsafe impl<'a, T> Container<'a> for Box<T>
where
  T: 'a + ?Sized + ItemLayout,
  *const T: RawPtr,
{
  type Class = BoxT;
  type Item = T;
  // Uses `*const T` as the old value can't be read after the conversion.
  type Raw = <*const T as RawPtr>::Raw;
  type CastErr = ErrWithValue;

  fn into_raw(self) -> Self::Raw {
    // SAFETY: Materializing a pointer to an allocated box is safe.
    unsafe { (Self::into_raw(self) as *const T).into_raw_ptr() }
  }
  unsafe fn from_raw(raw: Self::Raw) -> Self {
    Self::from_raw(<*const T>::from_raw_ptr(raw) as *mut T)
  }
}

#[cfg(feature = "extern_crate_alloc")]
unsafe impl<'a, T> Container<'a> for Rc<T>
where
  T: 'a + ?Sized + ItemLayout,
  *const T: RawPtr,
{
  type Class = RcT;
  type Item = T;
  type Raw = <*const T as RawPtr>::Raw;
  type CastErr = ErrWithValue;

  fn into_raw(self) -> Self::Raw {
    // SAFETY: Materializing a pointer to an allocated `Rc` is safe.
    unsafe { Self::into_raw(self).into_raw_ptr() }
  }
  unsafe fn from_raw(raw: Self::Raw) -> Self {
    Self::from_raw(<*const T>::from_raw_ptr(raw))
  }
}
#[cfg(feature = "extern_crate_alloc")]
unsafe impl<'a, T: 'a> Container<'a> for RcWeak<T> {
  type Class = RcWeakT;
  type Item = T;
  // `get_mut_unchecked` requires no other `Rc`s with a different type exist.
  type Raw = *const T;
  type CastErr = ErrWithValue;

  fn into_raw(self) -> Self::Raw {
    Self::into_raw(self)
  }
  unsafe fn from_raw(raw: Self::Raw) -> Self {
    Self::from_raw(raw)
  }
}

#[cfg(feature = "extern_crate_alloc")]
unsafe impl<'a, T> Container<'a> for Arc<T>
where
  T: 'a + ?Sized + ItemLayout,
  *const T: RawPtr,
{
  type Class = ArcT;
  type Item = T;
  type Raw = <*const T as RawPtr>::Raw;
  type CastErr = ErrWithValue;

  fn into_raw(self) -> Self::Raw {
    // SAFETY: Materializing a pointer to an allocated `Arc` is safe.
    unsafe { Self::into_raw(self).into_raw_ptr() }
  }
  unsafe fn from_raw(raw: Self::Raw) -> Self {
    Self::from_raw(<*const T>::from_raw_ptr(raw))
  }
}
#[cfg(feature = "extern_crate_alloc")]
unsafe impl<'a, T: 'a> Container<'a> for ArcWeak<T> {
  type Class = ArcWeakT;
  type Item = T;
  type Raw = *const T;
  type CastErr = ErrWithValue;

  fn into_raw(self) -> Self::Raw {
    Self::into_raw(self)
  }
  unsafe fn from_raw(raw: Self::Raw) -> Self {
    Self::from_raw(raw)
  }
}

/// The raw form of a vec.
#[cfg(feature = "extern_crate_alloc")]
#[derive(Clone, Copy)]
pub struct RawVec<T: Copy> {
  slice: RawSlice<T>,
  cap: usize,
}
#[cfg(feature = "extern_crate_alloc")]
unsafe impl<'a, T: 'a + Copy> Container<'a> for Vec<T> {
  type Class = VecT;
  type Item = T;
  type Raw = RawVec<T>;
  type CastErr = ErrWithValue;

  fn into_raw(self) -> Self::Raw {
    let mut x = ManuallyDrop::new(self);
    RawVec {
      // Use `as_mut_ptr` to get the correct provenance.
      slice: RawSlice { data: x.as_mut_ptr() as *const T, len: x.len() },
      cap: x.capacity(),
    }
  }
  unsafe fn from_raw(raw: Self::Raw) -> Self {
    Self::from_raw_parts(raw.slice.data as *mut T, raw.slice.len, raw.cap)
  }
}

/// Conversion between a types `Owned` type, and that type's raw form.
#[cfg(feature = "extern_crate_alloc")]
pub trait RawToOwned: ToOwned {
  type RawOwned: Copy;
  type CastErr: CastErrWithValue<*const Self>;
  fn raw_from_owned(value: Self::Owned) -> Self::RawOwned;
  unsafe fn owned_from_raw(raw: Self::RawOwned) -> Self::Owned;
}
#[cfg(feature = "extern_crate_alloc")]
impl<T: Copy> RawToOwned for T {
  type RawOwned = T;
  type CastErr = OnlyErr;
  fn raw_from_owned(value: Self::Owned) -> Self::RawOwned {
    value
  }
  unsafe fn owned_from_raw(raw: Self::RawOwned) -> Self::Owned {
    raw
  }
}
#[cfg(feature = "extern_crate_alloc")]
impl<T: Copy> RawToOwned for [T] {
  type RawOwned = RawVec<T>;
  type CastErr = ErrWithValue;
  fn raw_from_owned(value: Self::Owned) -> Self::RawOwned {
    value.into_raw()
  }
  unsafe fn owned_from_raw(raw: Self::RawOwned) -> Self::Owned {
    Vec::from_raw(raw)
  }
}
#[cfg(feature = "extern_crate_alloc")]
impl RawToOwned for str {
  type RawOwned = RawVec<StrByte>;
  type CastErr = ErrWithValue;
  fn raw_from_owned(value: Self::Owned) -> Self::RawOwned {
    let raw = value.into_bytes().into_raw();
    RawVec {
      slice: RawSlice { data: raw.slice.data.cast(), len: raw.slice.len },
      cap: raw.cap,
    }
  }
  unsafe fn owned_from_raw(raw: Self::RawOwned) -> Self::Owned {
    String::from_utf8_unchecked(Vec::from_raw(raw.cast_raw()))
  }
}

/// The raw form of a `Cow`.
#[cfg(feature = "extern_crate_alloc")]
pub enum RawCow<T: ?Sized + RawToOwned>
where
  *const T: RawPtr,
{
  Borrowed(<*const T as RawPtr>::Raw),
  Owned(<T as RawToOwned>::RawOwned),
}
#[cfg(feature = "extern_crate_alloc")]
impl<T: ?Sized + RawToOwned> Clone for RawCow<T>
where
  *const T: RawPtr,
{
  fn clone(&self) -> Self {
    *self
  }
}
#[cfg(feature = "extern_crate_alloc")]
impl<T: ?Sized + RawToOwned> Copy for RawCow<T> where *const T: RawPtr {}

#[cfg(feature = "extern_crate_alloc")]
unsafe impl<'a, T> Container<'a> for Cow<'a, T>
where
  T: 'a + ?Sized + RawToOwned + ItemLayout,
  <T as RawToOwned>::CastErr: CastErrWithValue<Self>,
  *const T: RawPtr,
{
  type Class = CowT;
  type Item = T;
  type Raw = RawCow<T>;
  type CastErr = <T as RawToOwned>::CastErr;

  fn into_raw(self) -> Self::Raw {
    match self {
      Self::Borrowed(x) => {
        // SAFETY: Materializing a pointer backed by a reference is safe.
        RawCow::Borrowed(unsafe { (x as *const T).into_raw_ptr() })
      }
      Self::Owned(x) => RawCow::Owned(<T as RawToOwned>::raw_from_owned(x)),
    }
  }
  unsafe fn from_raw(raw: Self::Raw) -> Self {
    match raw {
      RawCow::Borrowed(x) => Self::Borrowed(&*<*const T>::from_raw_ptr(x)),
      RawCow::Owned(x) => Self::Owned(<T as RawToOwned>::owned_from_raw(x)),
    }
  }
}

/// Attempts to convert the pointer type. Will fail if the pointer is not
/// suitably aligned for the target type.
fn try_cast_ptr<T, U>(ptr: *const T) -> Result<*const U, PodCastError> {
  if align_of::<T>() >= align_of::<U>() || ptr as usize % align_of::<U>() == 0 {
    Ok(ptr.cast())
  } else {
    Err(PodCastError::AlignmentMismatch)
  }
}
/// Attempts to convert the pointer type. Will fail if the pointer is not
/// suitably aligned for the target type.
fn try_cast_mut_ptr<T, U>(ptr: *mut T) -> Result<*mut U, PodCastError> {
  Ok(try_cast_ptr::<T, U>(ptr)? as *mut U)
}

/// Attempts to convert the length of `[T]` to the length of `[U]`. Will fail if
/// there is no length for the target type which will occupy all the bytes of
/// the input.
fn try_cast_len<T, U>(size: usize) -> Result<usize, PodCastError> {
  if size_of::<T>() == size_of::<U>() {
    Ok(size)
  } else if size_of::<T>() % size_of::<U>() == 0 {
    Ok(size * (size_of::<T>() / size_of::<U>()))
  } else {
    let byte_size = size * size_of::<T>();
    if byte_size % size_of::<U>() == 0 {
      Ok(byte_size / size_of::<U>())
    } else {
      Err(PodCastError::OutputSliceWouldHaveSlop)
    }
  }
}

#[test]
fn test_try_cast_len() {
  assert_eq!(try_cast_len::<(), ()>(0), Ok(0));
  assert_eq!(try_cast_len::<(), ()>(1), Ok(1));
  assert_eq!(try_cast_len::<(), ()>(2), Ok(2));
  assert_eq!(try_cast_len::<u8, u8>(0), Ok(0));
  assert_eq!(try_cast_len::<u8, u8>(1), Ok(1));
  assert_eq!(try_cast_len::<u8, u8>(2), Ok(2));
  assert_eq!(try_cast_len::<u8, u16>(0), Ok(0));
  assert_eq!(try_cast_len::<u8, u16>(2), Ok(1));
  assert_eq!(try_cast_len::<u8, u16>(4), Ok(2));
  assert_eq!(try_cast_len::<u8, u32>(0), Ok(0));
  assert_eq!(try_cast_len::<u8, u32>(4), Ok(1));
  assert_eq!(try_cast_len::<u8, u32>(8), Ok(2));
  assert_eq!(try_cast_len::<u16, u16>(0), Ok(0));
  assert_eq!(try_cast_len::<u16, u16>(1), Ok(1));
  assert_eq!(try_cast_len::<u16, u16>(2), Ok(2));
  assert_eq!(try_cast_len::<u16, u32>(0), Ok(0));
  assert_eq!(try_cast_len::<u16, u32>(2), Ok(1));
  assert_eq!(try_cast_len::<u16, u32>(4), Ok(2));
  assert_eq!(try_cast_len::<u32, u8>(1), Ok(4));
  assert_eq!(try_cast_len::<u32, u8>(2), Ok(8));
  assert_eq!(try_cast_len::<u32, u8>(3), Ok(12));
  assert_eq!(try_cast_len::<[u8; 3], u16>(0), Ok(0));
  assert_eq!(try_cast_len::<[u8; 3], u16>(2), Ok(3));
  assert_eq!(try_cast_len::<[u8; 3], u16>(4), Ok(6));
  assert_eq!(try_cast_len::<[u8; 3], u32>(0), Ok(0));
  assert_eq!(try_cast_len::<[u8; 3], u32>(4), Ok(3));
  assert_eq!(try_cast_len::<[u8; 3], u32>(8), Ok(6));
  assert!(try_cast_len::<u8, u16>(1).is_err());
  assert!(try_cast_len::<u8, u16>(3).is_err());
  assert!(try_cast_len::<u8, u16>(5).is_err());
  assert!(try_cast_len::<u8, u32>(1).is_err());
  assert!(try_cast_len::<u8, u32>(2).is_err());
  assert!(try_cast_len::<u8, u32>(3).is_err());
  assert!(try_cast_len::<[u8; 3], [u8; 2]>(1).is_err());
  assert!(try_cast_len::<[u8; 3], [u8; 2]>(3).is_err());
  assert!(try_cast_len::<[u8; 3], [u8; 2]>(5).is_err());
}

/// A conversion from a container's raw type to a compatible container's raw
/// type. This is not required to uphold any container specific constraints
/// which would be upheld by `AssertClassContraints`.
///
/// # Safety
/// Assuming `AssertClassContraints` succeeds, the resulting value must be safe
/// to use for the following two steps:
/// * `RawPtr::from_raw_ptr` if the input was created from `RawPtr::to_raw_ptr`.
/// * `Container::from_raw` for the resulting type's container.
///
/// Possible constraints include, but are not limited to:
/// * Any contained pointers must point to the same location after the
///   conversion.
/// * Assuming an input pointer is aligned, the matching result pointer must be
///   suitably aligned for the target type.
/// * All bit patterns of any converted input type must be valid for the target
///   type.
/// * For any converted mutable pointers, the reverse is true as well.
/// * Any length field must be converted from it's input type to occupy the same
///   number of bytes in it's output type.
pub unsafe trait CastRaw<Dst: Copy>: Copy {
  const ASSERT: ();
  /// Performs the conversion.
  ///
  /// # Safety
  /// `ASSERT` must be materialized before calling this function.
  unsafe fn cast_raw(self) -> Dst;
}

unsafe impl<Src, Dst> CastRaw<*const Dst> for *const Src
where
  Src: NoUninit,
  Dst: AnyBitPattern,
{
  const ASSERT: () = {
    assert!(
      size_of::<Src>() == size_of::<Dst>(),
      "Attempted conversion between types of different sizes."
    );
    assert!(
      align_of::<Src>() >= align_of::<Dst>(),
      "Attempted conversion to a type with a stricter alignment"
    );
  };
  unsafe fn cast_raw(self) -> *const Dst {
    self as *const Dst
  }
}
unsafe impl<Src, Dst> CastRaw<*const Dst> for *mut Src
where
  *const Src: CastRaw<*const Dst>,
{
  const ASSERT: () = <*const Src as CastRaw<*const Dst>>::ASSERT;
  unsafe fn cast_raw(self) -> *const Dst {
    (self as *const Src).cast_raw()
  }
}
unsafe impl<Src, Dst> CastRaw<*mut Dst> for *mut Src
where
  Src: NoUninit + AnyBitPattern,
  Dst: NoUninit + AnyBitPattern,
{
  const ASSERT: () = <*const Src as CastRaw<*const Dst>>::ASSERT;
  unsafe fn cast_raw(self) -> *mut Dst {
    self as *mut Dst
  }
}

unsafe impl<Src, Dst> CastRaw<RawSlice<Dst>> for *const Src
where
  Src: NoUninit,
  Dst: AnyBitPattern,
{
  const ASSERT: () = {
    assert!(
      size_of::<Src>() == size_of::<Dst>()
        || (size_of::<Dst>() != 0 && size_of::<Src>() % size_of::<Dst>() == 0),
      "Attempted conversion to a slice which cannot match the size of the item"
    );
  };
  unsafe fn cast_raw(self) -> RawSlice<Dst> {
    let len = if size_of::<Dst>() == 0 {
      1
    } else {
      size_of::<Src>() / size_of::<Dst>()
    };
    RawSlice { data: self.cast(), len }
  }
}
unsafe impl<Src, Dst> CastRaw<RawSlice<Dst>> for *mut Src
where
  *const Src: CastRaw<RawSlice<Dst>>,
{
  const ASSERT: () = <*const Src as CastRaw<RawSlice<Dst>>>::ASSERT;
  unsafe fn cast_raw(self) -> RawSlice<Dst> {
    (self as *const Src).cast_raw()
  }
}
unsafe impl<Src, Dst> CastRaw<RawMutSlice<Dst>> for *mut Src
where
  Src: NoUninit + AnyBitPattern,
  Dst: NoUninit + AnyBitPattern,
{
  const ASSERT: () = <*const Src as CastRaw<RawSlice<Dst>>>::ASSERT;
  unsafe fn cast_raw(self) -> RawMutSlice<Dst> {
    let len = if size_of::<Dst>() == 0 {
      1
    } else {
      size_of::<Src>() / size_of::<Dst>()
    };
    RawMutSlice { data: self.cast(), len }
  }
}

unsafe impl<Src, Dst> CastRaw<RawSlice<Dst>> for RawSlice<Src>
where
  Src: NoUninit,
  Dst: AnyBitPattern,
{
  const ASSERT: () = {
    assert!(
      size_of::<Src>() == size_of::<Dst>()
        || (size_of::<Dst>() != 0 && size_of::<Src>() % size_of::<Dst>() == 0),
      "Attempted conversion between slice types which may not succeed"
    );
    assert!(
      align_of::<Src>() >= align_of::<Dst>(),
      "Attempted conversion to a type with a stricter alignment"
    )
  };
  unsafe fn cast_raw(self) -> RawSlice<Dst> {
    let m = if size_of::<Src>() == 0 {
      1
    } else {
      size_of::<Src>() / size_of::<Dst>()
    };
    RawSlice { data: self.data as *const Dst, len: self.len * m }
  }
}
unsafe impl<Src, Dst> CastRaw<RawSlice<Dst>> for RawMutSlice<Src>
where
  RawSlice<Src>: CastRaw<RawSlice<Dst>>,
{
  const ASSERT: () = <RawSlice<Src> as CastRaw<RawSlice<Dst>>>::ASSERT;
  unsafe fn cast_raw(self) -> RawSlice<Dst> {
    RawSlice { data: self.data as *const Src, len: self.len }.cast_raw()
  }
}
unsafe impl<Src, Dst> CastRaw<RawMutSlice<Dst>> for RawMutSlice<Src>
where
  Src: NoUninit + AnyBitPattern,
  Dst: NoUninit + AnyBitPattern,
{
  const ASSERT: () = <RawSlice<Src> as CastRaw<RawSlice<Dst>>>::ASSERT;
  unsafe fn cast_raw(self) -> RawMutSlice<Dst> {
    let m = if size_of::<Src>() == 0 {
      1
    } else {
      size_of::<Src>() / size_of::<Dst>()
    };
    RawMutSlice { data: self.data as *mut Dst, len: self.len * m }
  }
}

unsafe impl<Src, Dst> CastRaw<Option<Dst>> for Option<Src>
where
  Src: CastRaw<Dst>,
  Dst: Copy,
{
  const ASSERT: () = <Src as CastRaw<Dst>>::ASSERT;
  unsafe fn cast_raw(self) -> Option<Dst> {
    self.map(|x| x.cast_raw())
  }
}

#[cfg(feature = "extern_crate_alloc")]
unsafe impl<Src, Dst> CastRaw<RawVec<Dst>> for RawVec<Src>
where
  Src: NoUninit,
  Dst: AnyBitPattern,
{
  const ASSERT: () = <RawSlice<Src> as CastRaw<RawSlice<Dst>>>::ASSERT;
  unsafe fn cast_raw(self) -> RawVec<Dst> {
    let m = if size_of::<Src>() == 0 {
      1
    } else {
      size_of::<Src>() / size_of::<Dst>()
    };
    RawVec { slice: self.slice.cast_raw(), cap: self.cap * m }
  }
}

#[cfg(feature = "extern_crate_alloc")]
unsafe impl<Src, Dst> CastRaw<RawCow<Dst>> for RawCow<Src>
where
  Src: NoUninit,
  Dst: AnyBitPattern,
{
  const ASSERT: () = <*const Src as CastRaw<*const Dst>>::ASSERT;
  unsafe fn cast_raw(self) -> RawCow<Dst> {
    match self {
      Self::Borrowed(x) => RawCow::Borrowed(x.cast_raw()),
      Self::Owned(x) => RawCow::Owned(
        transmute_copy::<ManuallyDrop<Src>, Dst>(&ManuallyDrop::new(x)),
      ),
    }
  }
}
#[cfg(feature = "extern_crate_alloc")]
unsafe impl<Src, Dst> CastRaw<RawCow<[Dst]>> for RawCow<[Src]>
where
  Src: NoUninit,
  Dst: AnyBitPattern,
{
  #[allow(path_statements, clippy::no_effect)]
  const ASSERT: () = {
    assert!(
      align_of::<Src>() == align_of::<Dst>(),
      "Attempted conversion between `Cow<[_]>` types with different alignments"
    );
    <RawVec<Src> as CastRaw<RawVec<Dst>>>::ASSERT;
  };
  unsafe fn cast_raw(self) -> RawCow<[Dst]> {
    match self {
      Self::Borrowed(x) => RawCow::Borrowed(x.cast_raw()),
      Self::Owned(x) => RawCow::Owned(x.cast_raw()),
    }
  }
}

/// An attempted conversion from a container's raw type to a compatible
/// container's raw type. This is not required to uphold any container specific
/// constraints which would be upheld by `AssertClassContraints`.
///
/// # Safety
/// Assuming `AssertClassContraints` succeeds, the value resulting from a
/// successful conversion must be safe to use for the following two steps:
/// * `RawPtr::from_raw_ptr` if the input was created from `RawPtr::to_raw_ptr`.
/// * `Container::from_raw` for the resulting type's container.
///
/// Possible constraints include, but are not limited to:
/// * Any contained pointers must point to the same location after the
///   conversion.
/// * Assuming an input pointer is aligned, the matching result pointer must be
///   suitably aligned for the target type.
/// * All bit patterns of any converted input type must be valid for the target
///   type.
/// * For any converted mutable pointers, the reverse is true as well.
/// * Any length field must be converted from it's input type to occupy the same
///   number of bytes in it's output type.
pub unsafe trait TryCastRaw<Dst: Copy>: Copy {
  const ASSERT: ();
  /// Perform the cast.
  ///
  /// # Safety
  /// `ASSERT` must be materialized before calling this function.
  unsafe fn try_cast_raw(self) -> Result<Dst, PodCastError>;
}

unsafe impl<Src, Dst> TryCastRaw<*const Dst> for *const Src
where
  Src: NoUninit,
  Dst: AnyBitPattern,
{
  const ASSERT: () = assert!(
    size_of::<Src>() == size_of::<Dst>(),
    "Attempted conversion between types of different sizes."
  );
  unsafe fn try_cast_raw(self) -> Result<*const Dst, PodCastError> {
    try_cast_ptr(self)
  }
}
unsafe impl<Src, Dst> TryCastRaw<*const Dst> for *mut Src
where
  *const Src: TryCastRaw<*const Dst>,
{
  const ASSERT: () = <*const Src as TryCastRaw<*const Dst>>::ASSERT;
  unsafe fn try_cast_raw(self) -> Result<*const Dst, PodCastError> {
    (self as *const Src).try_cast_raw()
  }
}
unsafe impl<Src, Dst> TryCastRaw<*mut Dst> for *mut Src
where
  Src: NoUninit + AnyBitPattern,
  Dst: NoUninit + AnyBitPattern,
{
  const ASSERT: () = <*const Src as TryCastRaw<*const Dst>>::ASSERT;
  unsafe fn try_cast_raw(self) -> Result<*mut Dst, PodCastError> {
    try_cast_mut_ptr(self)
  }
}

unsafe impl<Src, Dst> TryCastRaw<RawSlice<Dst>> for RawSlice<Src>
where
  Src: NoUninit,
  Dst: AnyBitPattern,
{
  const ASSERT: () = assert!(
    (size_of::<Dst>() == 0) == (size_of::<Src>() == 0),
    "Attempted conversion between a zero-sized type and a non-zero-sized type"
  );
  unsafe fn try_cast_raw(self) -> Result<RawSlice<Dst>, PodCastError> {
    Ok(RawSlice {
      data: try_cast_ptr(self.data)?,
      len: try_cast_len::<Src, Dst>(self.len)?,
    })
  }
}
unsafe impl<Src, Dst> TryCastRaw<RawSlice<Dst>> for RawMutSlice<Src>
where
  RawSlice<Src>: TryCastRaw<RawSlice<Dst>>,
{
  const ASSERT: () = <RawSlice<Src> as TryCastRaw<RawSlice<Dst>>>::ASSERT;
  unsafe fn try_cast_raw(self) -> Result<RawSlice<Dst>, PodCastError> {
    RawSlice { data: self.data as *const Src, len: self.len }.try_cast_raw()
  }
}
unsafe impl<Src, Dst> TryCastRaw<RawMutSlice<Dst>> for RawMutSlice<Src>
where
  Src: NoUninit + AnyBitPattern,
  Dst: NoUninit + AnyBitPattern,
{
  const ASSERT: () = <RawSlice<Src> as TryCastRaw<RawSlice<Dst>>>::ASSERT;
  unsafe fn try_cast_raw(self) -> Result<RawMutSlice<Dst>, PodCastError> {
    Ok(RawMutSlice {
      data: try_cast_mut_ptr(self.data)?,
      len: try_cast_len::<Src, Dst>(self.len)?,
    })
  }
}

unsafe impl<Src, Dst> TryCastRaw<*const Dst> for RawSlice<Src>
where
  Src: NoUninit,
  Dst: AnyBitPattern,
{
  const ASSERT: () = assert!(
    size_of::<Src>() == size_of::<Dst>()
      || (size_of::<Src>() != 0 && size_of::<Dst>() % size_of::<Src>() == 0),
    "Attempted conversion from a slice which cannot match the size of the item"
  );
  unsafe fn try_cast_raw(self) -> Result<*const Dst, PodCastError> {
    let len = if size_of::<Src>() == 0 {
      1
    } else {
      size_of::<Dst>() / size_of::<Src>()
    };
    if len == self.len {
      Ok(try_cast_ptr(self.data)?)
    } else {
      Err(PodCastError::SizeMismatch)
    }
  }
}
unsafe impl<Src, Dst> TryCastRaw<*const Dst> for RawMutSlice<Src>
where
  RawSlice<Src>: TryCastRaw<*const Dst>,
{
  const ASSERT: () = <RawSlice<Src> as TryCastRaw<*const Dst>>::ASSERT;
  unsafe fn try_cast_raw(self) -> Result<*const Dst, PodCastError> {
    RawSlice { data: self.data as *const Src, len: self.len }.try_cast_raw()
  }
}
unsafe impl<Src, Dst> TryCastRaw<*mut Dst> for RawMutSlice<Src>
where
  Src: NoUninit + AnyBitPattern,
  Dst: NoUninit + AnyBitPattern,
{
  const ASSERT: () = <RawSlice<Src> as TryCastRaw<*const Dst>>::ASSERT;
  unsafe fn try_cast_raw(self) -> Result<*mut Dst, PodCastError> {
    let len = if size_of::<Src>() == 0 {
      1
    } else {
      size_of::<Dst>() / size_of::<Src>()
    };
    if len == self.len {
      Ok(try_cast_mut_ptr(self.data)?)
    } else {
      Err(PodCastError::SizeMismatch)
    }
  }
}

unsafe impl<Src, Dst> TryCastRaw<RawSlice<Dst>> for *const Src
where
  Src: NoUninit,
  Dst: AnyBitPattern,
{
  const ASSERT: () = assert!(
    size_of::<Src>() == size_of::<Dst>()
      || (size_of::<Dst>() != 0 && size_of::<Src>() % size_of::<Dst>() == 0),
    "Attempted conversion to a slice which cannot match the size of the item"
  );
  unsafe fn try_cast_raw(self) -> Result<RawSlice<Dst>, PodCastError> {
    Ok(RawSlice {
      data: try_cast_ptr(self)?,
      len: if size_of::<Src>() == size_of::<Dst>() {
        1
      } else {
        size_of::<Src>() / size_of::<Dst>()
      },
    })
  }
}
unsafe impl<Src, Dst> TryCastRaw<RawSlice<Dst>> for *mut Src
where
  *const Src: TryCastRaw<RawSlice<Dst>>,
{
  const ASSERT: () = <*const Src as TryCastRaw<RawSlice<Dst>>>::ASSERT;
  unsafe fn try_cast_raw(self) -> Result<RawSlice<Dst>, PodCastError> {
    (self as *const Src).try_cast_raw()
  }
}
unsafe impl<Src, Dst> TryCastRaw<RawMutSlice<Dst>> for *mut Src
where
  Src: NoUninit + AnyBitPattern,
  Dst: NoUninit + AnyBitPattern,
{
  const ASSERT: () = <*const Src as TryCastRaw<RawSlice<Dst>>>::ASSERT;
  unsafe fn try_cast_raw(self) -> Result<RawMutSlice<Dst>, PodCastError> {
    Ok(RawMutSlice {
      data: try_cast_mut_ptr(self)?,
      len: if size_of::<Src>() == size_of::<Dst>() {
        1
      } else {
        size_of::<Src>() / size_of::<Dst>()
      },
    })
  }
}

unsafe impl<Src, Dst> TryCastRaw<Option<Dst>> for Option<Src>
where
  Src: TryCastRaw<Dst>,
  Dst: Copy,
{
  const ASSERT: () = <Src as TryCastRaw<Dst>>::ASSERT;
  unsafe fn try_cast_raw(self) -> Result<Option<Dst>, PodCastError> {
    match self {
      Some(x) => Ok(Some(x.try_cast_raw()?)),
      None => Ok(None),
    }
  }
}

#[cfg(feature = "extern_crate_alloc")]
unsafe impl<Src, Dst> TryCastRaw<RawVec<Dst>> for RawVec<Src>
where
  Src: NoUninit,
  Dst: AnyBitPattern,
{
  const ASSERT: () = <RawSlice<Src> as TryCastRaw<RawSlice<Dst>>>::ASSERT;
  unsafe fn try_cast_raw(self) -> Result<RawVec<Dst>, PodCastError> {
    Ok(RawVec {
      slice: self.slice.try_cast_raw()?,
      cap: try_cast_len::<Src, Dst>(self.cap)?,
    })
  }
}

#[cfg(feature = "extern_crate_alloc")]
unsafe impl<Src, Dst> TryCastRaw<RawCow<Dst>> for RawCow<Src>
where
  Src: NoUninit,
  Dst: AnyBitPattern,
{
  const ASSERT: () = <*const Src as TryCastRaw<*const Dst>>::ASSERT;
  unsafe fn try_cast_raw(self) -> Result<RawCow<Dst>, PodCastError> {
    Ok(match self {
      Self::Borrowed(x) => RawCow::Borrowed(x.try_cast_raw()?),
      Self::Owned(x) => RawCow::Owned(
        transmute_copy::<ManuallyDrop<Src>, Dst>(&ManuallyDrop::new(x)),
      ),
    })
  }
}
#[cfg(feature = "extern_crate_alloc")]
unsafe impl<Src, Dst> TryCastRaw<RawCow<[Dst]>> for RawCow<[Src]>
where
  Src: NoUninit,
  Dst: AnyBitPattern,
{
  const ASSERT: () = <RawVec<Src> as TryCastRaw<RawVec<Dst>>>::ASSERT;
  unsafe fn try_cast_raw(self) -> Result<RawCow<[Dst]>, PodCastError> {
    match self {
      Self::Borrowed(x) => Ok(RawCow::Borrowed(x.try_cast_raw()?)),
      Self::Owned(x) if align_of::<Src>() == align_of::<Dst>() => {
        Ok(RawCow::Owned(x.try_cast_raw()?))
      }
      Self::Owned(_) => Err(PodCastError::AlignmentMismatch),
    }
  }
}

/// Checks any constraints the container requires when casting between types.
pub trait AssertClassContraints<Src: ?Sized, Dst: ?Sized> {
  const ASSERT: () = ();
}
impl<Src: ?Sized, Dst: ?Sized> AssertClassContraints<Src, Dst> for RefT {}
impl<Src: ?Sized, Dst: ?Sized> AssertClassContraints<Src, Dst> for PtrT {}
impl<Src: ?Sized, Dst: ?Sized> AssertClassContraints<Src, Dst> for NonNullT {}
impl<Src, Dst> AssertClassContraints<Src, Dst> for AtomicPtrT {}
impl<C, Src: ?Sized, Dst: ?Sized> AssertClassContraints<Src, Dst> for OptionT<C>
where
  C: AssertClassContraints<Src, Dst>,
{
  const ASSERT: () = C::ASSERT;
}
impl<C, Src: ?Sized, Dst: ?Sized> AssertClassContraints<Src, Dst> for PinT<C>
where
  C: AssertClassContraints<Src, Dst>,
{
  const ASSERT: () = C::ASSERT;
}
#[cfg(feature = "extern_crate_alloc")]
impl<Src: ?Sized + ItemLayout, Dst: ?Sized + ItemLayout>
  AssertClassContraints<Src, Dst> for BoxT
{
  const ASSERT: () = assert!(
    Src::ALIGN == Dst::ALIGN,
    "Attempted conversion between `Box` types with different alignments"
  );
}
#[cfg(feature = "extern_crate_alloc")]
impl<Src: ?Sized + ItemLayout, Dst: ?Sized + ItemLayout>
  AssertClassContraints<Src, Dst> for RcT
{
  const ASSERT: () = assert!(
    Src::ALIGN == Dst::ALIGN,
    "Attempted conversion between `Rc` types with different alignments"
  );
}
#[cfg(feature = "extern_crate_alloc")]
impl<Src, Dst> AssertClassContraints<Src, Dst> for RcWeakT {
  const ASSERT: () = assert!(
    Src::ALIGN == Dst::ALIGN,
    "Attempted conversion between `rc::Weak` types with different alignments"
  );
}
#[cfg(feature = "extern_crate_alloc")]
impl<Src: ?Sized + ItemLayout, Dst: ?Sized + ItemLayout>
  AssertClassContraints<Src, Dst> for ArcT
{
  const ASSERT: () = assert!(
    Src::ALIGN == Dst::ALIGN,
    "Attempted conversion between `Arc` types with different alignments"
  );
}
#[cfg(feature = "extern_crate_alloc")]
impl<Src, Dst> AssertClassContraints<Src, Dst> for ArcWeakT {
  const ASSERT: () = assert!(
    Src::ALIGN == Dst::ALIGN,
    "Attempted conversion between `sync::Weak` types with different alignments"
  );
}
#[cfg(feature = "extern_crate_alloc")]
impl<Src, Dst> AssertClassContraints<Src, Dst> for VecT {
  const ASSERT: () = assert!(
    Src::ALIGN == Dst::ALIGN,
    "Attempted conversion between `Vec` types with different alignments"
  );
}
#[cfg(feature = "extern_crate_alloc")]
impl<
    Src: ?Sized + RawToOwned + ItemLayout,
    Dst: ?Sized + RawToOwned + ItemLayout,
  > AssertClassContraints<Src, Dst> for CowT
{
}

/// Safe byte-wise conversion between two values which contain some number of
/// values without allocation. This conversion should not fail for any reason.
///
/// This supports the following conversions:
/// * `&[mut] T`/`&[mut] [T]` -> `&U`/`&[U]`
/// * `&mut T`/`&mut [T]` -> `&mut U`/`&mut [T]`
/// * `*[const|mut] T` -> `*const U`
/// * `*mut T` -> `*mut U`
/// * `NonNull<T>`/`NonNull<[T]>` -> `NonNull<U>`/`NonNull<[U]>` (slice version
///   requires the `non_null_slice_cast` feature)
/// * `AtomicPtr<T>` -> `AtomicPtr<U>`
/// * `Pin<T>` -> `Pin<U>` where `T` -> `U` is valid
/// * `Option<T>` -> `Option<U>` where `T` -> `U` is valid
///
/// With the `extern_crate_alloc` feature the following are also supported:
/// `Box<T>`/`Box<[T]>` -> `Box<U>`/`Box<U>`
/// `Rc<T>`/`Rc<[T]>` -> `Rc<U>`/`Rc<U>`
/// `rc::Weak<T>`/`rc::Weak<[T]>` -> `rc::Weak<U>`/`rc::Weak<U>`
/// `Arc<T>`/`Arc<[T]>` -> `Arc<U>`/`Arc<U>`
/// `sync::Weak<T>` -> `sync::Weak<U>`
/// `Vec<T>` -> `Vec<U>`
/// `Cow<T>` -> `Cow<U>`
/// `Cow<[T]>` -> `Cow<U>`
///
/// This requires the `unified_cast` feature to be enabled and a rust version
/// `>=1.57`.
pub trait ReinterpretInner<'a, Dst: 'a>: 'a + Sized {
  /// Performs the conversion.
  fn reinterpret_inner(self) -> Dst;
}
impl<'a, Src: 'a, Dst: 'a> ReinterpretInner<'a, Dst> for Src
where
  Src: Container<'a, Class = Dst::Class>,
  Dst: Container<'a>,
  Src::Class: AssertClassContraints<Src::Item, Dst::Item>,
  Src::Raw: CastRaw<Dst::Raw>,
{
  #[allow(path_statements, clippy::no_effect)]
  fn reinterpret_inner(self) -> Dst {
    <Src::Class as AssertClassContraints<Src::Item, Dst::Item>>::ASSERT;
    <Src::Raw as CastRaw<Dst::Raw>>::ASSERT;
    unsafe { Dst::from_raw(self.into_raw().cast_raw()) }
  }
}

/// Attempt at a safe byte-wise conversion between two values which contain some
/// number of values. This conversion may fail due runtime conditions such as
/// size and alignment errors.
///
/// This supports the following conversions:
/// * `&[mut] T`/`&[mut] [T]` -> `&U`/`&[U]`
/// * `&mut T`/`&mut [T]` -> `&mut U`/`&mut [T]`
/// * `*[const|mut] T` -> `*const U`
/// * `*mut T` -> `*mut U`
/// * `NonNull<T>`/`NonNull<[T]>` -> `NonNull<U>`/`NonNull<[U]>` (slice version
///   requires the `non_null_slice_cast` feature)
/// * `AtomicPtr<T>` -> `AtomicPtr<U>`
/// * `Pin<T>` -> `Pin<U>` where `T` -> `U` is valid
/// * `Option<T>` -> `Option<U>` where `T` -> `U` is valid
///
/// With the `extern_crate_alloc` feature the following are also supported:
/// `Box<T>`/`Box<[T]>` -> `Box<U>`/`Box<U>`
/// `Rc<T>`/`Rc<[T]>` -> `Rc<U>`/`Rc<U>`
/// `rc::Weak<T>`/`rc::Weak<[T]>` -> `rc::Weak<U>`/`rc::Weak<U>`
/// `Arc<T>`/`Arc<[T]>` -> `Arc<U>`/`Arc<U>`
/// `sync::Weak<T>` -> `sync::Weak<U>`
/// `Vec<T>` -> `Vec<U>`
/// `Cow<T>` -> `Cow<U>`
/// `Cow<[T]>` -> `Cow<U>`
///
/// This requires the `unified_cast` feature to be enabled and a rust version
/// `>=1.57`.
pub trait TryReinterpretInner<'a, Dst: 'a>: 'a + Sized {
  /// The type returned in the event of a conversion error.
  type Error;
  /// Perform the conversion.
  fn try_reinterpret_inner(self) -> Result<Dst, Self::Error>;
}
impl<'a, Src: 'a, Dst: 'a> TryReinterpretInner<'a, Dst> for Src
where
  Src: Container<'a, Class = Dst::Class>,
  Dst: Container<'a>,
  Src::Class: AssertClassContraints<Src::Item, Dst::Item>,
  Src::Raw: TryCastRaw<Dst::Raw>,
{
  type Error = <Src::CastErr as CastErrWithValue<Src>>::Err;
  #[allow(path_statements, clippy::no_effect)]
  fn try_reinterpret_inner(self) -> Result<Dst, Self::Error> {
    <Src::Class as AssertClassContraints<Src::Item, Dst::Item>>::ASSERT;
    <Src::Raw as TryCastRaw<Dst::Raw>>::ASSERT;
    let raw = self.into_raw();
    match unsafe { raw.try_cast_raw() } {
      Ok(raw) => Ok(unsafe { Dst::from_raw(raw) }),
      Err(e) => {
        Err(<Src::CastErr as CastErrWithValue<Src>>::cast_error_with_value(
          e,
          unsafe { Src::from_raw(raw) },
        ))
      }
    }
  }
}
