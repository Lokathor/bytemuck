use super::*;

/// A trait which indicates that a type is a `repr(transparent)` wrapper around
/// the `Wrapped` value.
///
/// This allows safely creating references to `T` from those to `T::Wrapped`,
/// using the [`wrap_ref`] and [`wrap_mut`] functions.
///
/// # Safety
///
/// The safety contract of `TransparentWrapper` is relatively simple:
///
/// For a given `T` which implements `TransparentWrapper`:
///
/// 1. T must be a `#[repr(transparent)]` struct and contain a field of type
///    `T::Wrapped`.
///
/// 2. Any fields *other* than the `T::Wrapped` field must be trivially
///    constructable ZSTs, for example `PhantomData`, `PhantomPinned`, etc.
///
/// 3. The `T` may not impose additional alignment requirements over
///    `T::Wrapped`.
///     - Note: this is currently guaranteed by repr(transparent), but there
///       have been discussions of lifting it, so it's stated here explictly.
///
/// ## Caveats
///
/// If the wrapper imposes additional constraints upon the wrapped type which
/// are required for safety, it's responsible for ensuring those still hold --
/// this generally requires preventing access to instances of the wrapped type,
/// as implementing `TransparentWrapper` means anybody can call
/// `bytemuck::cast_ref`.
///
/// For example, it would be invalid to implement TransparentWrapper for `str`
/// to implement `TransparentWrapper` around `[u8]` because of this.
///
/// # Examples
///
/// ## Basic
///
/// ```
/// use bytemuck::{wrap_ref, wrap_mut, TransparentWrapper};
/// # #[derive(Default)]
/// # struct SomeStruct(u32);
///
/// #[repr(transparent)]
/// struct MyWrapper(SomeStruct);
///
/// unsafe impl TransparentWrapper for MyWrapper {
///   type Wrapped = SomeStruct;
/// }
///
/// // interpret a reference to &SomeStruct as a &MyWrapper
/// let thing = SomeStruct::default();
/// let wrapped_ref: &MyWrapper = wrap_ref(&thing);
///
/// // Works with &mut too.
/// let mut mut_thing = SomeStruct::default();
/// let wrapped_mut: &mut MyWrapper = wrap_mut(&mut mut_thing);
///
/// # let _ = (wrapped_ref, wrapped_mut); // silence warnings
/// ```
///
/// ## Use with dynamically sized types
///
/// ```
/// use bytemuck::{wrap_ref, wrap_mut, TransparentWrapper};
///
/// #[repr(transparent)]
/// struct Slice<T>([T]);
///
/// unsafe impl<T> TransparentWrapper for Slice<T> {
///     type Wrapped = [T];
/// }
///
/// let s = wrap_ref::<Slice<_>>(&[1u32, 2, 3]);
/// assert_eq!(&s.0, &[1, 2, 3]);
///
/// // Otherwise you'd need separate Slice<'a, T> and SliceMut<'a, T>, for example
/// let mut buf = [1, 2, 3u8];
/// let sm = wrap_mut::<Slice<_>>(&mut buf);
/// assert_eq!(&s.0, &[1, 2, 3]);
/// ```
pub unsafe trait TransparentWrapper {
  /// The wrapped type.
  type Wrapped: ?Sized;
}

/// Convert a reference to a wrapped type into a reference to the wrapper.
///
/// See [`TransparentWrapper`] for details.
#[inline]
pub fn wrap_ref<W: TransparentWrapper + ?Sized>(r: &W::Wrapped) -> &W {
  unsafe {
    // Ideally we'd check more than just this, but...
    assert!(size_of::<*const W::Wrapped>() == size_of::<*const W>());
    // Using a pointer cast doesn't work here because rustc can't tell that the
    // vtables match (if we lifted the ?Sized restriction, this would go away),
    // and transmute doesn't work for the usual reasons it doesn't work inside
    // generic functions.
    //
    // SAFETY: The unsafe contract requires that these have identical
    // representations. Using this transmute_copy instead of transmute here is
    // annoying, but is required as `Self` and `Self::Int` have unspecified
    // sizes still.
    let wrapped_ptr = r as *const W::Wrapped;
    let wrapper_ptr: *const W = transmute_copy(&wrapped_ptr);
    &*wrapper_ptr
  }
}

/// Convert a mut reference to a wrapped type into a mut reference to the
/// wrapper.
///
/// See [`TransparentWrapper`] for details.
#[inline]
pub fn wrap_mut<W: TransparentWrapper + ?Sized>(r: &mut W::Wrapped) -> &mut W {
  unsafe {
    assert!(size_of::<*mut W::Wrapped>() == size_of::<*mut W>());
    // Using a pointer cast doesn't work here because rustc can't tell that the
    // vtables match (if we lifted the ?Sized restriction, this would go away),
    // and transmute doesn't work for the usual reasons it doesn't work inside
    // generic functions.
    //
    // SAFETY: The unsafe contract requires that these have identical
    // representations. Using this transmute_copy instead of transmute here is
    // annoying, but is required as `Self` and `Self::Int` have unspecified
    // sizes still.
    let wrapped_ptr = r as *mut W::Wrapped;
    let wrapper_ptr: *mut W = transmute_copy(&wrapped_ptr);
    &mut *wrapper_ptr
  }
}
#[cfg(all(test, feature = "extern_crate_alloc"))]
mod test {
  use super::*;

  use alloc::format;
  use core::fmt::Display;

  #[repr(transparent)]
  struct DisplayTraitObj(dyn Display);

  unsafe impl TransparentWrapper for DisplayTraitObj {
    type Wrapped = dyn Display;
  }

  impl Display for DisplayTraitObj {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
      self.0.fmt(f)
    }
  }

  #[test]
  fn test_vtabled() {
    let v: &DisplayTraitObj = wrap_ref(&5i32 as &dyn Display);
    let s = format!("{}", v);
    assert_eq!(s, "5");

    let mut x = 100i32;
    let v_mut: &mut DisplayTraitObj = wrap_mut(&mut x as &mut dyn Display);
    let s = format!("{}", v_mut);
    assert_eq!(s, "100");
  }
}
