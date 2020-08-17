mod traits;

use proc_macro2::TokenStream;
use quote::quote;
use syn::{parse_macro_input, spanned::Spanned, Data, DataStruct, DeriveInput};

use crate::traits::{Derivable, Pod, TransparentWrapper, Zeroable};

#[proc_macro_derive(Pod)]
pub fn derive_pod(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
  let expanded =
    derive_marker_trait::<Pod>(parse_macro_input!(input as DeriveInput));

  proc_macro::TokenStream::from(expanded)
}

#[proc_macro_derive(Zeroable)]
pub fn derive_zeroable(
  input: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
  let expanded =
    derive_marker_trait::<Zeroable>(parse_macro_input!(input as DeriveInput));

  proc_macro::TokenStream::from(expanded)
}

#[proc_macro_derive(TransparentWrapper, attributes(transparent))]
pub fn derive_transparent(
  input: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
  let expanded = derive_marker_trait::<TransparentWrapper>(parse_macro_input!(
    input as DeriveInput
  ));

  proc_macro::TokenStream::from(expanded)
}

fn derive_marker_trait<Trait: Derivable>(input: DeriveInput) -> TokenStream {
  derive_marker_trait_inner::<Trait>(input).unwrap_or_else(|err| err)
}

fn derive_marker_trait_inner<Trait: Derivable>(
  input: DeriveInput,
) -> Result<TokenStream, TokenStream> {
  let name = &input.ident;

  let (impl_generics, ty_generics, where_clause) =
    input.generics.split_for_impl();
  let span = input.span();

  let fields = if let Data::Struct(DataStruct { fields, .. }) = &input.data {
    fields
  } else {
    return Err(quote! {
      compile_error!("deriving this trait is only supported for structs");
    });
  };

  let trait_ = Trait::ident();
  Trait::check_attributes(&input.attrs)?;
  let asserts = Trait::struct_asserts(name, fields, &input.attrs, span)?;
  let trait_params = Trait::generic_params(&input)?;

  Ok(quote! {
    #asserts

    unsafe impl #impl_generics #trait_ #trait_params for #name #ty_generics #where_clause {}
  })
}
