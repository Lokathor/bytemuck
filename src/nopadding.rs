use crate::Pod;

/// Marker trait for "plain old data" types with no padding.
///
/// The requirements for this is very similar to [`Pod`],
/// except that it doesn't require that all bit patterns of the type are valid, i.e.
/// it does not require the type to be [`Zeroable`][crate::Zeroable].
/// This limits what you can do with a type of this kind, but also broadens the
/// included types to things like C-style enums. Notably, you can only cast from
/// *immutable* references to a [`NoPadding`] type into *immutable* references of any other
/// type, no casting of mutable references or mutable references to slices etc.
///
/// [`Pod`] is a superset of [`NoPadding`], meaning that any `T: Pod` is also
/// [`NoPadding`] but any `T: NoPadding` is not necessarily [`Pod`]. If possible,
/// prefer implementing [`Pod`] directly. To get more [`Pod`]-like functionality for
/// a type that is only [`NoPadding`], consider also implementing [`CheckedBitPattern`][crate::CheckedBitPattern].
///
/// # Derive
///
/// A `#[derive(NoPadding)]` macro is provided under the `derive` feature flag which will
/// automatically validate the requirements of this trait and implement the
/// trait for you for both enums and structs. This is the recommended method for
/// implementing the trait, however it's also possible to do manually. If you
/// implement it manually, you *must* carefully follow the below safety rules.
///
/// # Safety
///
/// The same as [`Pod`] except we disregard the rule about it must
/// allow any bit pattern (i.e. it does not need to be [`Zeroable`][crate::Zeroable]).
/// Still, this is a quite strong guarantee about a type, so *be careful* whem
/// implementing it manually.
///
/// * The type must be inhabited (eg: no
///   [Infallible](core::convert::Infallible)).
/// * The type must not contain any padding bytes, either in the middle or on
///   the end (eg: no `#[repr(C)] struct Foo(u8, u16)`, which has padding in the
///   middle, and also no `#[repr(C)] struct Foo(u16, u8)`, which has padding on
///   the end).
/// * Structs need to have all fields also be `NoPadding`.
/// * Structs need to be `repr(C)` or `repr(transparent)`. In the case of
///   `repr(C)`, the `packed` and `align` repr modifiers can be used as long as
///   all other rules end up being followed.
/// * Enums need to have an explicit `#[repr(Int)]`
/// * Enums must have only fieldless variants
pub unsafe trait NoPadding: Sized + Copy + 'static {}

unsafe impl<T: Pod> NoPadding for T {}

unsafe impl NoPadding for char {}

unsafe impl NoPadding for bool {}
