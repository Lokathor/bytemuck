use crate::{Pod, Zeroable};

/// A wrapper around an unaligned value with the bit-pattern of a `T`.
///
/// The type is modeled along the lines of its sibling, the [`Cell`](#core::cell::Cell). These are
/// similar in that the types disallow reference-access to their contents while preserving the
/// types' bit-representation. However, this types constraints come from a different reason. Its
/// trade-off is that the field containing the bit representation of the value is not aligned
/// properly. It is therefore not possible to create references to the inner value, either. Yet
/// some operations on the value are still sound and this type encapsulates those unsafe primitives
/// into a safe API.
///
/// The `Unaligned` container does not interact with `Send` and `Sync` and does not permit updates
/// through shared references. 
///
/// # Example
///
/// One primary use for this type is to use for attribute in larger POD structs, such as network
/// packets.
///
/// ```
/// use bytemuck::{Pod, Zeroable, Unaligned, from_bytes};
///
/// # unsafe impl Zeroable for Icmp {}
/// # unsafe impl Pod for Icmp {}
///
/// /// A structure describing the layout of an ICMPv4 header.
/// #[derive(Clone, Copy)]
/// struct Icmp {
///     pub icmp_type: u8,
///     pub code: u8,
///     pub checksum: u16,
/// }
///
/// # fn get_from_ethernet_zerocopy() -> &'static [u8] {
/// #     &[0; 12+2+4+4]
/// # }
///
/// # fn main() { inner().unwrap(); }
/// # fn inner() -> Option<Icmp> {
/// const OFFSET_NO_Q_TAG: usize = 14;
///
/// let data_frame: &[u8] = get_from_ethernet_zerocopy();
/// let payload = data_frame.get(OFFSET_NO_Q_TAG..)?.get(..4)?;
///
/// // Already checked the necessary size.
/// let icmp: &Unaligned<Icmp> = from_bytes(payload);
/// // Safety read the relevant data into a semantic value.
/// let header: Icmp = icmp.into_inner();
///
/// # Some(header)
/// # }
///
/// ```
///
#[repr(packed)]
pub struct Unaligned<T> {
    /// The value representation. It is unsafe to grab a reference to this field!
    inner: T,
}

impl<T> Unaligned<T> {
    /// Wrap a value as unaligned.
    pub const fn new(val: T) -> Self {
        Unaligned { inner: val }
    }

    /// Unwrap the contained value.
    pub fn into_inner(self) -> T {
        self.inner
    }

    /// Wrap an existing reference, forgetting its current place is aligned.
    pub fn from_ref(val: &T) -> &Self {
        unsafe { &*(val as *const T as *const Self) }
    }

    /// Wrap an existing mutable reference, forgetting its current place is aligned.
    pub fn from_mut(val: &mut T) -> &mut Self {
        unsafe { &mut *(val as *mut T as *mut Self) }
    }

    /// Get the underlying value, copied into a new place.
    pub fn get(&self) -> T
    where
        T: Copy,
    {
        self.inner
    }

    /// Overwrite the contained value.
    pub fn set(&mut self, val: T) {
        self.inner = val;
    }

    /// Swap the contained value with another one.
    ///
    /// This utility deviates from `Cell` in that the right-hand operand is a mutable reference,
    /// but not wrapped into an `Unaligned` value. The reason is that a swap of two unaligned
    /// containers is merely an ordinary [`core::mem::swap`] whereas the interoperability between
    /// the two types of values is not completely straightforward.
    pub fn swap(&mut self, val: &mut T) {
        let other = Self::from_mut(val);
        // Convince the compiler to use the unaligned, but efficient, intrinsic method for swapping
        // between those two non-overlapping places.
        core::mem::swap(self, other);
    }

    /// Replace the value, returning the old contained value.
    pub fn replace(&mut self, mut val: T) -> T {
        self.swap(&mut val);
        val
    }
}

#[cfg(feature = "min_const_generics")]
impl<T, const N: usize> Unaligned<[T; N]> {
    /// Reinterpret an unaligned array as an array of unaligned elements.
    ///
    /// This allows access to an unaligned part of a sequence of unaligned data, such as the
    /// payload within a larger packet. The inverse is [`Self::from_array_of_unaligned`].
    ///
    /// This is possible since the layout of arrays is determined to be precisely `N` elements of
    /// the underlying type.
    ///
    /// # Example
    ///
    /// ```
    /// # use bytemuck::{Unaligned, from_bytes};
    /// let raw_data: [u8; 8] = [0, 0, 0, 1, 0, 0, 0, 0xb];
    /// let val: &Unaligned<[u32; 2]> = from_bytes(&raw_data);
    ///
    /// let [_, b] = val.as_array_of_unaligned();
    /// assert_eq!(b.into_inner(), u32::from_ne_bytes([0, 0, 0, 0xb]));
    /// ```
    pub fn as_array_of_unaligned(&self) -> &[Unaligned<T>; N]
    where
        T: Pod,
    {
        // This _could_ be implemented unsafely without the T: Pod bound. Doing so would warrant
        // another safety review. For now this encapsulates an infallible downcast.
        use crate::{bytes_of, from_bytes};
        from_bytes(bytes_of(self))
    }

    /// The inverse is [`Self::as_array_of_unaligned`].
    ///
    /// # Example
    ///
    /// ```
    /// # use bytemuck::{Unaligned, from_bytes};
    /// let raw_data: [u8; 8] = [0, 0, 0, 1, 0, 0, 0, 0xb];
    /// let val: &[Unaligned<u32>; 2] = from_bytes(&raw_data);
    ///
    /// let val = Unaligned::from_array_of_unaligned(val);
    /// let [_, b] = val.into_inner();
    /// assert_eq!(b, u32::from_ne_bytes([0, 0, 0, 0xb]));
    /// ```
    pub fn from_array_of_unaligned(this: &[Unaligned<T>; N]) -> &Self
    where
        T: Pod,
    {
        // Similar comment as the inverse. T
        use crate::{bytes_of, from_bytes};
        from_bytes(bytes_of(this))
    }
}

/// Clones by copy, only if `T` itself is `Copy`.
///
/// This can not be implemented for non-copyable types since clone-by-reference would require the
/// inner value to be aligned, to make a valid reference.
impl<T: Copy> Clone for Unaligned<T> {
    fn clone(&self) -> Self {
        Unaligned { inner: self.inner }
    }
}

impl<T: Copy> Copy for Unaligned<T> {}

unsafe impl<T: Pod> Pod for Unaligned<T> {}
unsafe impl<T: Zeroable> Zeroable for Unaligned<T> {}
