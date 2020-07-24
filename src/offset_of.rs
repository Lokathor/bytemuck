#![forbid(unsafe_code)]

/// Find the offset in bytes of the given `$field` of `$Type`. Requires an
/// already initialized `$instance` value to work with.
///
/// This is similar to the macro from [`memoffset`](https://docs.rs/memoffset),
/// however it uses no `unsafe` code.
///
/// This macro has a 3-argument and 2-argument version.
/// * In the 3-arg version you specify an instance of the type, the type itself,
///   and the field name.
/// * In the 2-arg version the macro will call the [`default`](Default::default)
///   method to make a temporary instance of the type for you.
///
/// The output of this macro is the byte offset of the field (as a `usize`). The
/// calculations of the macro are fixed across the entire program, but if the
/// type used is `repr(Rust)` then they're *not* fixed across compilations or
/// compilers.
///
/// **CAUTION:** It is **unsound** to use this macro with a `repr(packed)` type.
/// Currently this will give a warning, and in the future it will become a hard
/// error.
/// * See [rust-lang/rust#27060](https://github.com/rust-lang/rust/issues/27060)
///   for more info and for status updates.
/// * Once this issue is resolved, a future version of this crate will use
///   `raw_ref` to correct the issue. For the duration of the `1.x` version of
///   this crate it will be an on-by-default cargo feature (to maintain minimum
///   rust version support).
///
/// ## Examples
///
/// ### 3-arg Usage
///
/// ```rust
/// # use bytemuck::offset_of;
/// // enums can't derive default, and for this example we don't pick one
/// enum MyExampleEnum {
///   A, B, C,
/// }
///
/// // so now our struct here doesn't have Default
/// #[repr(C)]
/// struct MyNotDefaultType {
///   pub counter: i32,
///   pub some_field: MyExampleEnum,
/// }
///
/// // but we provide an instance of the type and it's all good.
/// let val = MyNotDefaultType { counter: 5, some_field: MyExampleEnum::A };
/// assert_eq!(offset_of!(val, MyNotDefaultType, some_field), 4);
/// ```
///
/// ### 2-arg Usage
///
/// ```rust
/// # use bytemuck::offset_of;
/// #[derive(Default)]
/// #[repr(C)]
/// struct Vertex {
///   pub loc: [f32; 3],
///   pub color: [f32; 3],
/// }
/// // if the type impls Default the macro can make its own default instance.
/// assert_eq!(offset_of!(Vertex, loc), 0);
/// assert_eq!(offset_of!(Vertex, color), 12);
/// ```
#[macro_export]
macro_rules! offset_of {
  ($instance:expr, $Type:path, $field:tt) => {{
    // This helps us guard against field access going through a Deref impl.
    #[allow(clippy::unneeded_field_pattern)]
    let $Type { $field: _, .. };
    let reference: &$Type = &$instance;
    let address = reference as *const _ as usize;
    let field_pointer = &reference.$field as *const _ as usize;
    // These asserts/unwraps are compiled away at release, and defend against
    // the case where somehow a deref impl is still invoked.
    let result = field_pointer.checked_sub(address).unwrap();
    assert!(result <= $crate::__core::mem::size_of::<$Type>());
    result
  }};
  ($Type:path, $field:tt) => {{
    $crate::offset_of!(<$Type as Default>::default(), $Type, $field)
  }};
}
