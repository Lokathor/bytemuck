use crate::Pod;

/// Marker trait for "plain old data" types that are valid for any bit pattern.
///
/// The requirements for this is very similar to [`Pod`],
/// except that it doesn't require that the type contains no padding bytes.
/// This limits what you can do with a type of this kind, but also broadens the
/// included types to `repr(C)` structs that contain padding. Notably, you can only cast
/// *immutable* references and *owned* values into [`AnyBitPattern`] types, not
/// *mutable* references.
///
/// [`Pod`] is a superset of [`AnyBitPattern`], meaning that any `T: Pod` is also
/// [`AnyBitPattern`] but any `T: AnyBitPattern` is not necessarily [`Pod`].
///
/// # Derive
///
/// A `#[derive(AnyBitPattern)]` macro is provided under the `derive` feature flag which will
/// automatically validate the requirements of this trait and implement the
/// trait for you for both structs. This is the recommended method for
/// implementing the trait, however it's also possible to do manually. If you
/// implement it manually, you *must* carefully follow the below safety rules.
///
/// * *NOTE: even `C-style`, fieldless enums are intentionally **excluded** from
/// this trait, since it is **unsound** for an enum to have a discriminant value
/// that is not one of its defined variants.
/// 
/// # Safety
///
/// Similar to [`Pod`] except we disregard the rule about it must not contain padding.
/// Still, this is a quite strong guarantee about a type, so *be careful* when
/// implementing it manually.
///
/// * The type must be inhabited (eg: no
///   [Infallible](core::convert::Infallible)).
/// * The type must be valid for any bit pattern of its backing memory.
/// * Structs need to have all fields also be `AnyBitPattern`.
pub unsafe trait AnyBitPattern: Sized + Copy + 'static {}

unsafe impl<T: Pod> AnyBitPattern for T {}
