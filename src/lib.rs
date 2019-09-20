#[cfg(target_arch = "x86")]
pub(crate) use core::arch::x86;
#[cfg(target_arch = "x86_64")]
pub(crate) use core::arch::x86_64;
//
pub(crate) use core::{marker::*, num::*, ptr::*};

macro_rules! impl_unsafe_marker_for_array {
  ( $marker:ident , $( $n:expr ),* ) => {
    $(unsafe impl<T> $marker for [T; $n] where T: $marker {})*
  }
}

macro_rules! impl_unsafe_marker_for_type {
  ( $marker:ident , $( $t:ty ),* ) => {
    $(unsafe impl<T> $marker for $t {})*
  }
}

mod zeroable;
pub use zeroable::*;
