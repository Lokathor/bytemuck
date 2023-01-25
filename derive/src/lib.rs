//! Derive macros for [bytemuck](https://docs.rs/bytemuck) traits.

extern crate proc_macro;

mod traits;

use proc_macro2::TokenStream;
use quote::quote;
use syn::{parse_macro_input, DeriveInput, Result};

use crate::traits::{
  AnyBitPattern, CheckedBitPattern, Contiguous, Derivable, NoUninit, Pod,
  TransparentWrapper, Zeroable,
};

/// Derive the `Pod` trait for a struct
///
/// The macro ensures that the struct follows all the the safety requirements
/// for the `Pod` trait.
///
/// The following constraints need to be satisfied for the macro to succeed
///
/// - All fields in the struct must implement `Pod`
/// - The struct must be `#[repr(C)]` or `#[repr(transparent)]`
/// - The struct must not contain any padding bytes
/// - The struct contains no generic parameters
///
/// ## Example
///
/// ```rust
/// # use bytemuck_derive::{Pod, Zeroable};
///
/// #[derive(Copy, Clone, Pod, Zeroable)]
/// #[repr(C)]
/// struct Test {
///   a: u16,
///   b: u16,
/// }
/// ```
#[proc_macro_derive(Pod)]
pub fn derive_pod(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
  let expanded =
    derive_marker_trait::<Pod>(parse_macro_input!(input as DeriveInput));

  proc_macro::TokenStream::from(expanded)
}

/// Derive the `AnyBitPattern` trait for a struct
///
/// The macro ensures that the struct follows all the the safety requirements
/// for the `AnyBitPattern` trait.
///
/// The following constraints need to be satisfied for the macro to succeed
///
/// - All fields in the struct must to implement `AnyBitPattern`
#[proc_macro_derive(AnyBitPattern)]
pub fn derive_anybitpattern(
  input: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
  let expanded = derive_marker_trait::<AnyBitPattern>(parse_macro_input!(
    input as DeriveInput
  ));

  proc_macro::TokenStream::from(expanded)
}

/// Derive the `Zeroable` trait for a struct
///
/// The macro ensures that the struct follows all the the safety requirements
/// for the `Zeroable` trait.
///
/// The following constraints need to be satisfied for the macro to succeed
///
/// - All fields in the struct must to implement `Zeroable`
///
/// ## Example
///
/// ```rust
/// # use bytemuck_derive::{Zeroable};
///
/// #[derive(Copy, Clone, Zeroable)]
/// #[repr(C)]
/// struct Test {
///   a: u16,
///   b: u16,
/// }
/// ```
#[proc_macro_derive(Zeroable)]
pub fn derive_zeroable(
  input: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
  let expanded =
    derive_marker_trait::<Zeroable>(parse_macro_input!(input as DeriveInput));

  proc_macro::TokenStream::from(expanded)
}

/// Derive the `NoUninit` trait for a struct or enum
///
/// The macro ensures that the type follows all the the safety requirements
/// for the `NoUninit` trait.
///
/// The following constraints need to be satisfied for the macro to succeed
/// (the rest of the constraints are guaranteed by the `NoUninit` subtrait
/// bounds, i.e. the type must be `Sized + Copy + 'static`):
///
/// If applied to a struct:
/// - All fields in the struct must implement `NoUninit`
/// - The struct must be `#[repr(C)]` or `#[repr(transparent)]`
/// - The struct must not contain any padding bytes
/// - The struct must contain no generic parameters
///
/// If applied to an enum:
/// - The enum must be explicit `#[repr(Int)]`
/// - All variants must be fieldless
/// - The enum must contain no generic parameters
#[proc_macro_derive(NoUninit)]
pub fn derive_no_uninit(
  input: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
  let expanded =
    derive_marker_trait::<NoUninit>(parse_macro_input!(input as DeriveInput));

  proc_macro::TokenStream::from(expanded)
}

/// Derive the `CheckedBitPattern` trait for a struct or enum.
///
/// The macro ensures that the type follows all the the safety requirements
/// for the `CheckedBitPattern` trait and derives the required `Bits` type
/// definition and `is_valid_bit_pattern` method for the type automatically.
///
/// The following constraints need to be satisfied for the macro to succeed
/// (the rest of the constraints are guaranteed by the `CheckedBitPattern`
/// subtrait bounds, i.e. are guaranteed by the requirements of the `NoUninit`
/// trait which `CheckedBitPattern` is a subtrait of):
///
/// If applied to a struct:
/// - All fields must implement `CheckedBitPattern`
///
/// If applied to an enum:
/// - All requirements already checked by `NoUninit`, just impls the trait
#[proc_macro_derive(CheckedBitPattern)]
pub fn derive_maybe_pod(
  input: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
  let expanded = derive_marker_trait::<CheckedBitPattern>(parse_macro_input!(
    input as DeriveInput
  ));

  proc_macro::TokenStream::from(expanded)
}

/// Derive the `TransparentWrapper` trait for a struct
///
/// The macro ensures that the struct follows all the the safety requirements
/// for the `TransparentWrapper` trait.
///
/// The following constraints need to be satisfied for the macro to succeed
///
/// - The struct must be `#[repr(transparent)]`
/// - The struct must contain the `Wrapped` type
///
/// If the struct only contains a single field, the `Wrapped` type will
/// automatically be determined if there is more then one field in the struct,
/// you need to specify the `Wrapped` type using `#[transparent(T)]`
///
/// ## Example
///
/// ```rust
/// # use bytemuck_derive::TransparentWrapper;
/// # use std::marker::PhantomData;
///
/// #[derive(Copy, Clone, TransparentWrapper)]
/// #[repr(transparent)]
/// #[transparent(u16)]
/// struct Test<T> {
///   inner: u16,
///   extra: PhantomData<T>,
/// }
/// ```
#[proc_macro_derive(TransparentWrapper, attributes(transparent))]
pub fn derive_transparent(
  input: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
  let expanded = derive_marker_trait::<TransparentWrapper>(parse_macro_input!(
    input as DeriveInput
  ));

  proc_macro::TokenStream::from(expanded)
}

/// Derive the `Contiguous` trait for an enum
///
/// The macro ensures that the enum follows all the the safety requirements
/// for the `Contiguous` trait.
///
/// The following constraints need to be satisfied for the macro to succeed
///
/// - The enum must be `#[repr(Int)]`
/// - The enum must be fieldless
/// - The enum discriminants must form a contiguous range
///
/// ## Example
///
/// ```rust
/// # use bytemuck_derive::{Contiguous};
///
/// #[derive(Copy, Clone, Contiguous)]
/// #[repr(u8)]
/// enum Test {
///   A = 0,
///   B = 1,
///   C = 2,
/// }
/// ```
#[proc_macro_derive(Contiguous)]
pub fn derive_contiguous(
  input: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
  let expanded =
    derive_marker_trait::<Contiguous>(parse_macro_input!(input as DeriveInput));

  proc_macro::TokenStream::from(expanded)
}

/// Derive the `PartialEq` and `Eq` trait for a type
///
/// The macro implements `PartialEq` and `Eq` by casting both sides of the
/// comparison to a byte slice and then compares those.
///
/// ## Warning
///
/// Since this implements a byte wise comparison, the behavior of floating point
/// numbers does not match their usual comparison behavior. Additionally other
/// custom comparison behaviors of the individual fields are also ignored. This
/// also does not implement `StructuralPartialEq` / `StructuralEq` like
/// `PartialEq` / `Eq` would. This means you can't pattern match on the values.
///
/// ## Example
///
/// ```rust
/// # use bytemuck_derive::{ByteEq, NoUninit};
/// #[derive(Copy, Clone, NoUninit, ByteEq)]
/// #[repr(C)]
/// struct Test {
///   a: u32,
///   b: char,
///   c: f32,
/// }
/// ```
#[proc_macro_derive(ByteEq)]
pub fn derive_byte_eq(
  input: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
  let input = parse_macro_input!(input as DeriveInput);
  let ident = input.ident;

  proc_macro::TokenStream::from(quote! {
    impl ::core::cmp::PartialEq for #ident {
      #[inline]
      #[must_use]
      fn eq(&self, other: &Self) -> bool {
        ::bytemuck::bytes_of(self) == ::bytemuck::bytes_of(other)
      }
    }
    impl ::core::cmp::Eq for #ident { }
  })
}

/// Derive the `Hash` trait for a type
///
/// The macro implements `Hash` by casting the value to a byte slice and hashing
/// that.
///
/// ## Warning
///
/// The hash does not match the standard library's `Hash` derive.
///
/// ## Example
///
/// ```rust
/// # use bytemuck_derive::{ByteHash, NoUninit};
/// #[derive(Copy, Clone, NoUninit, ByteHash)]
/// #[repr(C)]
/// struct Test {
///   a: u32,
///   b: char,
///   c: f32,
/// }
/// ```
#[proc_macro_derive(ByteHash)]
pub fn derive_byte_hash(
  input: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
  let input = parse_macro_input!(input as DeriveInput);
  let ident = input.ident;

  proc_macro::TokenStream::from(quote! {
    impl ::core::hash::Hash for #ident {
      #[inline]
      fn hash<H: ::core::hash::Hasher>(&self, state: &mut H) {
        ::core::hash::Hash::hash_slice(::bytemuck::bytes_of(self), state)
      }

      #[inline]
      fn hash_slice<H: ::core::hash::Hasher>(data: &[Self], state: &mut H) {
        ::core::hash::Hash::hash_slice(::bytemuck::cast_slice::<_, u8>(data), state)
      }
    }
  })
}

/// Basic wrapper for error handling
fn derive_marker_trait<Trait: Derivable>(input: DeriveInput) -> TokenStream {
  derive_marker_trait_inner::<Trait>(input)
    .unwrap_or_else(|err| err.into_compile_error())
}

fn derive_marker_trait_inner<Trait: Derivable>(
  mut input: DeriveInput,
) -> Result<TokenStream> {
  // Enforce Pod on all generic fields.
  let trait_ = Trait::ident(&input)?;
  add_trait_marker(&mut input.generics, &trait_);

  let name = &input.ident;

  let (impl_generics, ty_generics, where_clause) =
    input.generics.split_for_impl();

  Trait::check_attributes(&input.data, &input.attrs)?;
  let asserts = Trait::asserts(&input)?;
  let (trait_impl_extras, trait_impl) = Trait::trait_impl(&input)?;

  let implies_trait = if let Some(implies_trait) = Trait::implies_trait() {
    quote!(unsafe impl #impl_generics #implies_trait for #name #ty_generics #where_clause {})
  } else {
    quote!()
  };

  let where_clause = if Trait::requires_where_clause() {
    where_clause
  } else {
    None
  };

  Ok(quote! {
    #asserts

    #trait_impl_extras

    unsafe impl #impl_generics #trait_ for #name #ty_generics #where_clause {
      #trait_impl
    }

    #implies_trait
  })
}

/// Add a trait marker to the generics if it is not already present
fn add_trait_marker(generics: &mut syn::Generics, trait_name: &syn::Path) {
  // Get each generic type parameter.
  let type_params = generics
    .type_params()
    .map(|param| &param.ident)
    .map(|param| syn::parse_quote!(
      #param: #trait_name
    )).collect::<Vec<syn::WherePredicate>>();

  generics.make_where_clause().predicates.extend(type_params);
}
