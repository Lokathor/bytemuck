//! Derive macros for [bytemuck](https://docs.rs/bytemuck) traits.

extern crate proc_macro;

mod traits;

use proc_macro2::TokenStream;
use quote::quote;
use syn::{parse_macro_input, DeriveInput};

use crate::traits::{Contiguous, Derivable, Pod, TransparentWrapper, Zeroable};

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

/// Derive the `Zeroable` trait for a struct
///
/// The macro ensures that the struct follows all the the safety requirements
/// for the `Zeroable` trait.
///
/// The following constraints need to be satisfied for the macro to succeed
///
/// - All fields ind the struct must to implement `Zeroable`
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

/// Basic wrapper for error handling
fn derive_marker_trait<Trait: Derivable>(input: DeriveInput) -> TokenStream {
  derive_marker_trait_inner::<Trait>(input).unwrap_or_else(|err| {
    quote! {
      compile_error!(#err);
    }
  })
}

fn derive_marker_trait_inner<Trait: Derivable>(
  input: DeriveInput,
) -> Result<TokenStream, &'static str> {
  let name = &input.ident;

  let (impl_generics, ty_generics, where_clause) =
    input.generics.split_for_impl();

  let trait_ = Trait::ident();
  Trait::check_attributes(&input.attrs)?;
  let asserts = Trait::struct_asserts(&input)?;
  let trait_params = Trait::generic_params(&input)?;
  let trait_impl = Trait::trait_impl(&input)?;

  Ok(quote! {
    #asserts

    unsafe impl #impl_generics #trait_ #trait_params for #name #ty_generics #where_clause {
      #trait_impl
    }
  })
}
