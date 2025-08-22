#![forbid(unsafe_code)]

use core::{convert::TryFrom, marker::PhantomData};

use crate::Contiguous;

#[inline]
#[must_use]
fn contiguous_index<C>(c: C) -> u64
where
  C: Contiguous,
  <C as Contiguous>::Int: Into<u64>,
{
  let min64: u64 = C::MIN_VALUE.into();
  let i: u64 = c.into_integer().into();
  i - min64
}

/// A set of [Contiguous] values, encoded into a `u64`.
///
/// The trait bounds on the associated `Int` type allow for the set to convert
/// an `Int` value into a `u64` bit position. All of Rust's integer types of 64
/// bits or less will satisfy the bounds.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(transparent)]
pub struct ContigousBitset64<C>(u64, PhantomData<C>)
where
  C: Contiguous,
  <C as Contiguous>::Int: Into<u64>,
  <C as Contiguous>::Int: TryFrom<u64>;

impl<C> ContigousBitset64<C>
where
  C: Contiguous,
  <C as Contiguous>::Int: Into<u64>,
  <C as Contiguous>::Int: TryFrom<u64>,
{
  /// Makes a new bitset.
  ///
  /// ## Panics
  /// * `C::MAX_VALUE - C::MIN_VALUE` must be less than 64
  #[inline]
  #[must_use]
  pub fn new() -> Self {
    let c_max: u64 = C::MAX_VALUE.into();
    let c_min: u64 = C::MIN_VALUE.into();
    assert!((c_max - c_min) < 64);
    Self(0, PhantomData)
  }

  /// Inserts a value into the bitset, returning if the value was already
  /// present.
  #[inline]
  pub fn insert(&mut self, c: C) -> bool {
    let index = contiguous_index(c);
    let already_set = (self.0 & index) != 0;
    self.0 |= index;
    already_set
  }

  /// Removes a value from the bitset, returning if the value had been present.
  #[inline]
  pub fn remove(&mut self, c: C) -> bool {
    let index = contiguous_index(c);
    let already_set = (self.0 & index) != 0;
    self.0 &= !index;
    already_set
  }

  /// If the given element is contained in the set.
  #[inline]
  #[must_use]
  pub fn contains(&self, c: C) -> bool {
    let index = contiguous_index(c);
    (self.0 & index) != 0
  }

  /// Iterates the values of the bitset.
  #[inline]
  #[must_use]
  pub fn iter(&self) -> impl Iterator<Item = C> + Clone + '_ {
    let mut iter_index = 0;
    core::iter::from_fn(move || {
      while iter_index < 64 {
        let this_bit = self.0 & (1 << iter_index);
        iter_index += 1;
        if this_bit != 0 {
          let trailing: u32 = this_bit.trailing_zeros();
          let c_min: u64 = C::MIN_VALUE.into();
          let total: u64 = c_min + u64::from(trailing);
          let opt_int: Option<C::Int> = C::Int::try_from(total).ok();
          let opt_c: Option<C> = opt_int.and_then(C::from_integer);
          return opt_c;
        } else {
          continue;
        }
      }
      return None;
    })
  }
}
impl<C> Default for ContigousBitset64<C>
where
  C: Contiguous,
  <C as Contiguous>::Int: Into<u64>,
  <C as Contiguous>::Int: TryFrom<u64>,
{
  /// See the [new][Self::new] function.
  #[inline]
  fn default() -> Self {
    Self::new()
  }
}
