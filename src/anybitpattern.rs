use crate::{Pod, Zeroable};

/// Marker trait for "plain old data" types that are valid for any bit pattern.
///
/// The requirements for this is very similar to [`Pod`],
/// except that the type can allow uninit (or padding) bytes.
/// This limits what you can do with a type of this kind, but also broadens the
/// included types to `repr(C)` `struct`s that contain padding as well as `union`s. Notably, you can only cast
/// *immutable* references and *owned* values into [`AnyBitPattern`] types, not
/// *mutable* references.
///
/// [`Pod`] is a subset of [`AnyBitPattern`], meaning that any `T: Pod` is also
/// [`AnyBitPattern`] but any `T: AnyBitPattern` is not necessarily [`Pod`].
///
/// [`AnyBitPattern`] is a subset of [`Zeroable`], meaning that any `T: AnyBitPattern`
/// is also [`Zeroable`], but any `T: Zeroable` is not necessarily [`AnyBitPattern  ]
///
/// # Derive
///
/// A `#[derive(AnyBitPattern)]` macro is provided under the `derive` feature flag which will
/// automatically validate the requirements of this trait and implement the
/// trait for you for both structs and enums. This is the recommended method for
/// implementing the trait, however it's also possible to do manually. If you
/// implement it manually, you *must* carefully follow the below safety rules.
///
/// * *NOTE: even `C-style`, fieldless enums are intentionally **excluded** from
/// this trait, since it is **unsound** for an enum to have a discriminant value
/// that is not one of its defined variants.
/// 
/// # Safety
///
/// Similar to [`Pod`] except we disregard the rule about it must not contain uninit bytes.
/// Still, this is a quite strong guarantee about a type, so *be careful* when
/// implementing it manually.
///
/// * The type must be inhabited (eg: no
///   [Infallible](core::convert::Infallible)).
/// * The type must be valid for any bit pattern of its backing memory.
/// * Structs need to have all fields also be `AnyBitPattern`.
/// * The type must have no interior mutability. In the sense of RustBelt's separation logic, a
///   type is allowed to define a sharing predicate that holds for shared references. We require
///   this predicate to be trivial and permit only read-only access.
/// * There's probably more, don't mess it up (I mean it).
pub unsafe trait AnyBitPattern: Zeroable + Sized + Copy + 'static {}

unsafe impl<T: Pod> AnyBitPattern for T {}
