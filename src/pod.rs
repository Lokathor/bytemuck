use super::*;

/// Marker trait for "plain old data".
///
/// The point of this trait is that once something is marked "plain old data"
/// you can really go to town with the bit fiddling and bit casting. Therefore,
/// it's a relatively strong claim to make about a type. Do not add this to your
/// type casually.
///
/// **Reminder:** The results of casting around bytes between data types are
/// _endian dependant_. Little-endian machines are the most common, but
/// big-endian machines do exist (and big-endian is also used for "network
/// order" bytes).
///
/// ## Safety
///
/// * The type must be inhabited (eg: no
///   [Infallible](core::convert::Infallible)).
/// * The type must allow any bit pattern (eg: no `bool` or `char`, which have
///   illegal bit patterns).
/// * The type must not contain any uninit (or padding) bytes, either in the
///   middle or on the end (eg: no `#[repr(C)] struct Foo(u8, u16)`, which has
///   padding in the middle, and also no `#[repr(C)] struct Foo(u16, u8)`, which
///   has padding on the end).
/// * The type needs to have all fields also be `Pod`.
/// * The type needs to be `repr(C)` or `repr(transparent)`. In the case of
///   `repr(C)`, the `packed` and `align` repr modifiers can be used as long as
///   all other rules end up being followed.
/// * It is disallowed for types to contain pointer types, `Cell`, `UnsafeCell`,
///   atomics, and any other forms of interior mutability.
/// * More precisely: A shared reference to the type must allow reads, and
///   *only* reads. RustBelt's separation logic is based on the notion that a
///   type is allowed to define a sharing predicate, its own invariant that must
///   hold for shared references, and this predicate is the reasoning that allow
///   it to deal with atomic and cells etc. We require the sharing predicate to
///   be trivial and permit only read-only access.
pub unsafe trait Pod: Zeroable + Copy + 'static {}

unsafe impl Pod for () {}
unsafe impl Pod for u8 {}
unsafe impl Pod for i8 {}
unsafe impl Pod for u16 {}
unsafe impl Pod for i16 {}
unsafe impl Pod for u32 {}
unsafe impl Pod for i32 {}
unsafe impl Pod for u64 {}
unsafe impl Pod for i64 {}
unsafe impl Pod for usize {}
unsafe impl Pod for isize {}
unsafe impl Pod for u128 {}
unsafe impl Pod for i128 {}
unsafe impl Pod for f32 {}
unsafe impl Pod for f64 {}
unsafe impl<T: Pod> Pod for Wrapping<T> {}

#[cfg(feature = "unsound_ptr_pod_impl")]
unsafe impl<T: 'static> Pod for *mut T {}
#[cfg(feature = "unsound_ptr_pod_impl")]
unsafe impl<T: 'static> Pod for *const T {}
#[cfg(feature = "unsound_ptr_pod_impl")]
unsafe impl<T: 'static> PodInOption for NonNull<T> {}

unsafe impl<T: Pod> Pod for PhantomData<T> {}
unsafe impl Pod for PhantomPinned {}
unsafe impl<T: Pod> Pod for ManuallyDrop<T> {}

// Note(Lokathor): MaybeUninit can NEVER be Pod.

#[cfg(feature = "min_const_generics")]
unsafe impl<T, const N: usize> Pod for [T; N] where T: Pod {}

#[cfg(not(feature = "min_const_generics"))]
impl_unsafe_marker_for_array!(
  Pod, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19,
  20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 48, 64, 96, 128, 256,
  512, 1024, 2048, 4096
);

#[cfg(all(target_arch = "wasm32", feature = "wasm_simd"))]
unsafe impl Pod for wasm32::v128 {}

#[cfg(all(target_arch = "aarch64", feature = "aarch64_simd"))]
unsafe impl Pod for aarch64::float32x2_t {}
#[cfg(all(target_arch = "aarch64", feature = "aarch64_simd"))]
unsafe impl Pod for aarch64::float32x2x2_t {}
#[cfg(all(target_arch = "aarch64", feature = "aarch64_simd"))]
unsafe impl Pod for aarch64::float32x2x3_t {}
#[cfg(all(target_arch = "aarch64", feature = "aarch64_simd"))]
unsafe impl Pod for aarch64::float32x2x4_t {}
#[cfg(all(target_arch = "aarch64", feature = "aarch64_simd"))]
unsafe impl Pod for aarch64::float32x4_t {}
#[cfg(all(target_arch = "aarch64", feature = "aarch64_simd"))]
unsafe impl Pod for aarch64::float32x4x2_t {}
#[cfg(all(target_arch = "aarch64", feature = "aarch64_simd"))]
unsafe impl Pod for aarch64::float32x4x3_t {}
#[cfg(all(target_arch = "aarch64", feature = "aarch64_simd"))]
unsafe impl Pod for aarch64::float32x4x4_t {}
#[cfg(all(target_arch = "aarch64", feature = "aarch64_simd"))]
unsafe impl Pod for aarch64::float64x1_t {}
#[cfg(all(target_arch = "aarch64", feature = "aarch64_simd"))]
unsafe impl Pod for aarch64::float64x1x2_t {}
#[cfg(all(target_arch = "aarch64", feature = "aarch64_simd"))]
unsafe impl Pod for aarch64::float64x1x3_t {}
#[cfg(all(target_arch = "aarch64", feature = "aarch64_simd"))]
unsafe impl Pod for aarch64::float64x1x4_t {}
#[cfg(all(target_arch = "aarch64", feature = "aarch64_simd"))]
unsafe impl Pod for aarch64::float64x2_t {}
#[cfg(all(target_arch = "aarch64", feature = "aarch64_simd"))]
unsafe impl Pod for aarch64::float64x2x2_t {}
#[cfg(all(target_arch = "aarch64", feature = "aarch64_simd"))]
unsafe impl Pod for aarch64::float64x2x3_t {}
#[cfg(all(target_arch = "aarch64", feature = "aarch64_simd"))]
unsafe impl Pod for aarch64::float64x2x4_t {}
#[cfg(all(target_arch = "aarch64", feature = "aarch64_simd"))]
unsafe impl Pod for aarch64::int16x4_t {}
#[cfg(all(target_arch = "aarch64", feature = "aarch64_simd"))]
unsafe impl Pod for aarch64::int16x4x2_t {}
#[cfg(all(target_arch = "aarch64", feature = "aarch64_simd"))]
unsafe impl Pod for aarch64::int16x4x3_t {}
#[cfg(all(target_arch = "aarch64", feature = "aarch64_simd"))]
unsafe impl Pod for aarch64::int16x4x4_t {}
#[cfg(all(target_arch = "aarch64", feature = "aarch64_simd"))]
unsafe impl Pod for aarch64::int16x8_t {}
#[cfg(all(target_arch = "aarch64", feature = "aarch64_simd"))]
unsafe impl Pod for aarch64::int16x8x2_t {}
#[cfg(all(target_arch = "aarch64", feature = "aarch64_simd"))]
unsafe impl Pod for aarch64::int16x8x3_t {}
#[cfg(all(target_arch = "aarch64", feature = "aarch64_simd"))]
unsafe impl Pod for aarch64::int16x8x4_t {}
#[cfg(all(target_arch = "aarch64", feature = "aarch64_simd"))]
unsafe impl Pod for aarch64::int32x2_t {}
#[cfg(all(target_arch = "aarch64", feature = "aarch64_simd"))]
unsafe impl Pod for aarch64::int32x2x2_t {}
#[cfg(all(target_arch = "aarch64", feature = "aarch64_simd"))]
unsafe impl Pod for aarch64::int32x2x3_t {}
#[cfg(all(target_arch = "aarch64", feature = "aarch64_simd"))]
unsafe impl Pod for aarch64::int32x2x4_t {}
#[cfg(all(target_arch = "aarch64", feature = "aarch64_simd"))]
unsafe impl Pod for aarch64::int32x4_t {}
#[cfg(all(target_arch = "aarch64", feature = "aarch64_simd"))]
unsafe impl Pod for aarch64::int32x4x2_t {}
#[cfg(all(target_arch = "aarch64", feature = "aarch64_simd"))]
unsafe impl Pod for aarch64::int32x4x3_t {}
#[cfg(all(target_arch = "aarch64", feature = "aarch64_simd"))]
unsafe impl Pod for aarch64::int32x4x4_t {}
#[cfg(all(target_arch = "aarch64", feature = "aarch64_simd"))]
unsafe impl Pod for aarch64::int64x1_t {}
#[cfg(all(target_arch = "aarch64", feature = "aarch64_simd"))]
unsafe impl Pod for aarch64::int64x1x2_t {}
#[cfg(all(target_arch = "aarch64", feature = "aarch64_simd"))]
unsafe impl Pod for aarch64::int64x1x3_t {}
#[cfg(all(target_arch = "aarch64", feature = "aarch64_simd"))]
unsafe impl Pod for aarch64::int64x1x4_t {}
#[cfg(all(target_arch = "aarch64", feature = "aarch64_simd"))]
unsafe impl Pod for aarch64::int64x2_t {}
#[cfg(all(target_arch = "aarch64", feature = "aarch64_simd"))]
unsafe impl Pod for aarch64::int64x2x2_t {}
#[cfg(all(target_arch = "aarch64", feature = "aarch64_simd"))]
unsafe impl Pod for aarch64::int64x2x3_t {}
#[cfg(all(target_arch = "aarch64", feature = "aarch64_simd"))]
unsafe impl Pod for aarch64::int64x2x4_t {}
#[cfg(all(target_arch = "aarch64", feature = "aarch64_simd"))]
unsafe impl Pod for aarch64::int8x16_t {}
#[cfg(all(target_arch = "aarch64", feature = "aarch64_simd"))]
unsafe impl Pod for aarch64::int8x16x2_t {}
#[cfg(all(target_arch = "aarch64", feature = "aarch64_simd"))]
unsafe impl Pod for aarch64::int8x16x3_t {}
#[cfg(all(target_arch = "aarch64", feature = "aarch64_simd"))]
unsafe impl Pod for aarch64::int8x16x4_t {}
#[cfg(all(target_arch = "aarch64", feature = "aarch64_simd"))]
unsafe impl Pod for aarch64::int8x8_t {}
#[cfg(all(target_arch = "aarch64", feature = "aarch64_simd"))]
unsafe impl Pod for aarch64::int8x8x2_t {}
#[cfg(all(target_arch = "aarch64", feature = "aarch64_simd"))]
unsafe impl Pod for aarch64::int8x8x3_t {}
#[cfg(all(target_arch = "aarch64", feature = "aarch64_simd"))]
unsafe impl Pod for aarch64::int8x8x4_t {}
#[cfg(all(target_arch = "aarch64", feature = "aarch64_simd"))]
unsafe impl Pod for aarch64::poly16x4_t {}
#[cfg(all(target_arch = "aarch64", feature = "aarch64_simd"))]
unsafe impl Pod for aarch64::poly16x4x2_t {}
#[cfg(all(target_arch = "aarch64", feature = "aarch64_simd"))]
unsafe impl Pod for aarch64::poly16x4x3_t {}
#[cfg(all(target_arch = "aarch64", feature = "aarch64_simd"))]
unsafe impl Pod for aarch64::poly16x4x4_t {}
#[cfg(all(target_arch = "aarch64", feature = "aarch64_simd"))]
unsafe impl Pod for aarch64::poly16x8_t {}
#[cfg(all(target_arch = "aarch64", feature = "aarch64_simd"))]
unsafe impl Pod for aarch64::poly16x8x2_t {}
#[cfg(all(target_arch = "aarch64", feature = "aarch64_simd"))]
unsafe impl Pod for aarch64::poly16x8x3_t {}
#[cfg(all(target_arch = "aarch64", feature = "aarch64_simd"))]
unsafe impl Pod for aarch64::poly16x8x4_t {}
#[cfg(all(target_arch = "aarch64", feature = "aarch64_simd"))]
unsafe impl Pod for aarch64::poly64x1_t {}
#[cfg(all(target_arch = "aarch64", feature = "aarch64_simd"))]
unsafe impl Pod for aarch64::poly64x1x2_t {}
#[cfg(all(target_arch = "aarch64", feature = "aarch64_simd"))]
unsafe impl Pod for aarch64::poly64x1x3_t {}
#[cfg(all(target_arch = "aarch64", feature = "aarch64_simd"))]
unsafe impl Pod for aarch64::poly64x1x4_t {}
#[cfg(all(target_arch = "aarch64", feature = "aarch64_simd"))]
unsafe impl Pod for aarch64::poly64x2_t {}
#[cfg(all(target_arch = "aarch64", feature = "aarch64_simd"))]
unsafe impl Pod for aarch64::poly64x2x2_t {}
#[cfg(all(target_arch = "aarch64", feature = "aarch64_simd"))]
unsafe impl Pod for aarch64::poly64x2x3_t {}
#[cfg(all(target_arch = "aarch64", feature = "aarch64_simd"))]
unsafe impl Pod for aarch64::poly64x2x4_t {}
#[cfg(all(target_arch = "aarch64", feature = "aarch64_simd"))]
unsafe impl Pod for aarch64::poly8x16_t {}
#[cfg(all(target_arch = "aarch64", feature = "aarch64_simd"))]
unsafe impl Pod for aarch64::poly8x16x2_t {}
#[cfg(all(target_arch = "aarch64", feature = "aarch64_simd"))]
unsafe impl Pod for aarch64::poly8x16x3_t {}
#[cfg(all(target_arch = "aarch64", feature = "aarch64_simd"))]
unsafe impl Pod for aarch64::poly8x16x4_t {}
#[cfg(all(target_arch = "aarch64", feature = "aarch64_simd"))]
unsafe impl Pod for aarch64::poly8x8_t {}
#[cfg(all(target_arch = "aarch64", feature = "aarch64_simd"))]
unsafe impl Pod for aarch64::poly8x8x2_t {}
#[cfg(all(target_arch = "aarch64", feature = "aarch64_simd"))]
unsafe impl Pod for aarch64::poly8x8x3_t {}
#[cfg(all(target_arch = "aarch64", feature = "aarch64_simd"))]
unsafe impl Pod for aarch64::poly8x8x4_t {}
#[cfg(all(target_arch = "aarch64", feature = "aarch64_simd"))]
unsafe impl Pod for aarch64::uint16x4_t {}
#[cfg(all(target_arch = "aarch64", feature = "aarch64_simd"))]
unsafe impl Pod for aarch64::uint16x4x2_t {}
#[cfg(all(target_arch = "aarch64", feature = "aarch64_simd"))]
unsafe impl Pod for aarch64::uint16x4x3_t {}
#[cfg(all(target_arch = "aarch64", feature = "aarch64_simd"))]
unsafe impl Pod for aarch64::uint16x4x4_t {}
#[cfg(all(target_arch = "aarch64", feature = "aarch64_simd"))]
unsafe impl Pod for aarch64::uint16x8_t {}
#[cfg(all(target_arch = "aarch64", feature = "aarch64_simd"))]
unsafe impl Pod for aarch64::uint16x8x2_t {}
#[cfg(all(target_arch = "aarch64", feature = "aarch64_simd"))]
unsafe impl Pod for aarch64::uint16x8x3_t {}
#[cfg(all(target_arch = "aarch64", feature = "aarch64_simd"))]
unsafe impl Pod for aarch64::uint16x8x4_t {}
#[cfg(all(target_arch = "aarch64", feature = "aarch64_simd"))]
unsafe impl Pod for aarch64::uint32x2_t {}
#[cfg(all(target_arch = "aarch64", feature = "aarch64_simd"))]
unsafe impl Pod for aarch64::uint32x2x2_t {}
#[cfg(all(target_arch = "aarch64", feature = "aarch64_simd"))]
unsafe impl Pod for aarch64::uint32x2x3_t {}
#[cfg(all(target_arch = "aarch64", feature = "aarch64_simd"))]
unsafe impl Pod for aarch64::uint32x2x4_t {}
#[cfg(all(target_arch = "aarch64", feature = "aarch64_simd"))]
unsafe impl Pod for aarch64::uint32x4_t {}
#[cfg(all(target_arch = "aarch64", feature = "aarch64_simd"))]
unsafe impl Pod for aarch64::uint32x4x2_t {}
#[cfg(all(target_arch = "aarch64", feature = "aarch64_simd"))]
unsafe impl Pod for aarch64::uint32x4x3_t {}
#[cfg(all(target_arch = "aarch64", feature = "aarch64_simd"))]
unsafe impl Pod for aarch64::uint32x4x4_t {}
#[cfg(all(target_arch = "aarch64", feature = "aarch64_simd"))]
unsafe impl Pod for aarch64::uint64x1_t {}
#[cfg(all(target_arch = "aarch64", feature = "aarch64_simd"))]
unsafe impl Pod for aarch64::uint64x1x2_t {}
#[cfg(all(target_arch = "aarch64", feature = "aarch64_simd"))]
unsafe impl Pod for aarch64::uint64x1x3_t {}
#[cfg(all(target_arch = "aarch64", feature = "aarch64_simd"))]
unsafe impl Pod for aarch64::uint64x1x4_t {}
#[cfg(all(target_arch = "aarch64", feature = "aarch64_simd"))]
unsafe impl Pod for aarch64::uint64x2_t {}
#[cfg(all(target_arch = "aarch64", feature = "aarch64_simd"))]
unsafe impl Pod for aarch64::uint64x2x2_t {}
#[cfg(all(target_arch = "aarch64", feature = "aarch64_simd"))]
unsafe impl Pod for aarch64::uint64x2x3_t {}
#[cfg(all(target_arch = "aarch64", feature = "aarch64_simd"))]
unsafe impl Pod for aarch64::uint64x2x4_t {}
#[cfg(all(target_arch = "aarch64", feature = "aarch64_simd"))]
unsafe impl Pod for aarch64::uint8x16_t {}
#[cfg(all(target_arch = "aarch64", feature = "aarch64_simd"))]
unsafe impl Pod for aarch64::uint8x16x2_t {}
#[cfg(all(target_arch = "aarch64", feature = "aarch64_simd"))]
unsafe impl Pod for aarch64::uint8x16x3_t {}
#[cfg(all(target_arch = "aarch64", feature = "aarch64_simd"))]
unsafe impl Pod for aarch64::uint8x16x4_t {}
#[cfg(all(target_arch = "aarch64", feature = "aarch64_simd"))]
unsafe impl Pod for aarch64::uint8x8_t {}
#[cfg(all(target_arch = "aarch64", feature = "aarch64_simd"))]
unsafe impl Pod for aarch64::uint8x8x2_t {}
#[cfg(all(target_arch = "aarch64", feature = "aarch64_simd"))]
unsafe impl Pod for aarch64::uint8x8x3_t {}
#[cfg(all(target_arch = "aarch64", feature = "aarch64_simd"))]
unsafe impl Pod for aarch64::uint8x8x4_t {}

#[cfg(target_arch = "x86")]
unsafe impl Pod for x86::__m128i {}
#[cfg(target_arch = "x86")]
unsafe impl Pod for x86::__m128 {}
#[cfg(target_arch = "x86")]
unsafe impl Pod for x86::__m128d {}
#[cfg(target_arch = "x86")]
unsafe impl Pod for x86::__m256i {}
#[cfg(target_arch = "x86")]
unsafe impl Pod for x86::__m256 {}
#[cfg(target_arch = "x86")]
unsafe impl Pod for x86::__m256d {}

#[cfg(target_arch = "x86_64")]
unsafe impl Pod for x86_64::__m128i {}
#[cfg(target_arch = "x86_64")]
unsafe impl Pod for x86_64::__m128 {}
#[cfg(target_arch = "x86_64")]
unsafe impl Pod for x86_64::__m128d {}
#[cfg(target_arch = "x86_64")]
unsafe impl Pod for x86_64::__m256i {}
#[cfg(target_arch = "x86_64")]
unsafe impl Pod for x86_64::__m256 {}
#[cfg(target_arch = "x86_64")]
unsafe impl Pod for x86_64::__m256d {}

#[cfg(feature = "nightly_portable_simd")]
unsafe impl<T, const N: usize> Pod for core::simd::Simd<T, N>
where
  T: core::simd::SimdElement + Pod,
  core::simd::LaneCount<N>: core::simd::SupportedLaneCount,
{
}
