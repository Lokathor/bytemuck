use proc_macro2::{Ident, Span, TokenStream, TokenTree};
use quote::{quote, quote_spanned};
use std::iter::empty;
use syn::{
  parse_macro_input, spanned::Spanned, AttrStyle, Attribute, Data, DataStruct,
  DeriveInput, Field, Fields, FieldsNamed, FieldsUnnamed, Type,
};

trait Derivable {
  fn ident() -> TokenStream;
  fn struct_asserts(
    struct_name: &Ident, fields: &Fields, span: Span,
  ) -> TokenStream;
  fn check_attributes(_attributes: &[Attribute]) -> Result<(), TokenStream> {
    Ok(())
  }
}

struct Pod;

impl Derivable for Pod {
  fn ident() -> TokenStream {
    quote!(bytemuck::Pod)
  }

  fn struct_asserts(
    struct_name: &Ident, fields: &Fields, span: Span,
  ) -> TokenStream {
    let assert_no_padding =
      generate_assert_no_padding(struct_name, fields, span);
    let assert_fields_are_pod =
      generate_fields_are_trait(&fields, Self::ident(), span);

    quote!(
      #assert_no_padding
      #assert_fields_are_pod
    )
  }

  fn check_attributes(attributes: &[Attribute]) -> Result<(), TokenStream> {
    for attr in attributes {
      if let (AttrStyle::Outer, Some(outer_ident), Some(inner_ident)) = (
        &attr.style,
        attr.path.get_ident(),
        get_ident_from_stream(attr.tokens.clone()),
      ) {
        if outer_ident.to_string() == "repr"
          && (inner_ident.to_string() == "C"
            || inner_ident.to_string() == "transparent")
        {
          return Ok(());
        }
      }
    }

    Err(quote! {
      compile_error!("Pod requires the struct to be #[repr(C)] or #[repr(transparent)]");
    })
  }
}

fn get_ident_from_stream(tokens: TokenStream) -> Option<Ident> {
  match tokens.into_iter().next() {
    Some(TokenTree::Group(group)) => get_ident_from_stream(group.stream()),
    Some(TokenTree::Ident(ident)) => Some(ident),
    _ => None,
  }
}

struct Zeroable;

impl Derivable for Zeroable {
  fn ident() -> TokenStream {
    quote!(bytemuck::Zeroable)
  }

  fn struct_asserts(
    _struct_name: &Ident, fields: &Fields, span: Span,
  ) -> TokenStream {
    generate_fields_are_trait(fields, Self::ident(), span)
  }
}

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

fn derive_marker_trait<Trait: Derivable>(input: DeriveInput) -> TokenStream {
  let name = &input.ident;

  let (impl_generics, ty_generics, where_clause) =
    input.generics.split_for_impl();
  let span = input.span();

  let fields = if let Data::Struct(DataStruct { fields, .. }) = &input.data {
    fields
  } else {
    return quote! {
      compile_error!("deriving this trait is only supported for structs");
    };
  };

  let trait_ = Trait::ident();
  if let Err(err) = Trait::check_attributes(&input.attrs) {
    return err;
  }
  let asserts = Trait::struct_asserts(name, fields, span);

  quote! {
    #asserts

    unsafe impl #impl_generics #trait_ for #name #ty_generics #where_clause {}
  }
}

fn get_fields<'a>(
  fields: &'a Fields,
) -> Box<dyn Iterator<Item = &'a Field> + 'a> {
  match fields {
    Fields::Unit => Box::new(empty()),
    Fields::Named(FieldsNamed { named, .. }) => Box::new(named.iter()),
    Fields::Unnamed(FieldsUnnamed { unnamed, .. }) => Box::new(unnamed.iter()),
  }
}

fn get_field_types<'a>(
  fields: &'a Fields,
) -> impl Iterator<Item = &'a Type> + 'a {
  get_fields(fields).map(|field| &field.ty)
}

/// Check that a struct has no padding by asserting that the size of the struct
/// is equal to the sum of the size of it's fields
fn generate_assert_no_padding(
  struct_type: &Ident, fields: &Fields, span: Span,
) -> TokenStream {
  let field_types = get_field_types(&fields);
  let struct_size = quote_spanned!(span => std::mem::size_of::<#struct_type>());
  let size_sum =
    quote_spanned!(span => 0 #( + std::mem::size_of::<#field_types>() )*);

  quote_spanned! {span => const _: fn() = || {
    let _ = core::mem::transmute::<[u8; #struct_size], [u8; #size_sum]>;
  };}
}

/// Check that all fields implement Pod
fn generate_fields_are_trait(
  fields: &Fields, trait_: TokenStream, span: Span,
) -> TokenStream {
  let field_types = get_field_types(&fields);
  quote_spanned! {span => #(const _: fn() = || {
      fn assert_impl<T: #trait_>() {}
      assert_impl::<#field_types>();
    };)*
  }
}
