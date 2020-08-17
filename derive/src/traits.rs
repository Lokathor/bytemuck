use proc_macro2::{Ident, Span, TokenStream, TokenTree};
use quote::{quote, quote_spanned, ToTokens};
use std::iter::empty;
use syn::{
  AttrStyle, Attribute, Data, DataStruct, DeriveInput, Field, Fields,
  FieldsNamed, FieldsUnnamed, Type,
};

pub trait Derivable {
  fn ident() -> TokenStream;
  fn generic_params(_input: &DeriveInput) -> Result<TokenStream, TokenStream> {
    Ok(quote!())
  }
  fn struct_asserts(
    struct_name: &Ident, fields: &Fields, attributes: &[Attribute], span: Span,
  ) -> Result<TokenStream, TokenStream>;
  fn check_attributes(_attributes: &[Attribute]) -> Result<(), TokenStream> {
    Ok(())
  }
}

pub struct Pod;

impl Derivable for Pod {
  fn ident() -> TokenStream {
    quote!(::bytemuck::Pod)
  }

  fn struct_asserts(
    struct_name: &Ident, fields: &Fields, _attributes: &[Attribute], span: Span,
  ) -> Result<TokenStream, TokenStream> {
    let assert_no_padding =
      generate_assert_no_padding(struct_name, fields, span);
    let assert_fields_are_pod =
      generate_fields_are_trait(&fields, Self::ident(), span);

    Ok(quote!(
      #assert_no_padding
      #assert_fields_are_pod
    ))
  }

  fn check_attributes(attributes: &[Attribute]) -> Result<(), TokenStream> {
    let repr = get_repr(attributes);
    match repr.as_ref().map(|repr| repr.as_str()) {
      Some("C") => Ok(()),
      Some("transparent") => Ok(()),
      _ => Err(quote! {
        compile_error!("Pod requires the struct to be #[repr(C)] or #[repr(transparent)]");
      }),
    }
  }
}

pub struct Zeroable;

impl Derivable for Zeroable {
  fn ident() -> TokenStream {
    quote!(::bytemuck::Zeroable)
  }

  fn struct_asserts(
    _struct_name: &Ident, fields: &Fields, _attributes: &[Attribute],
    span: Span,
  ) -> Result<TokenStream, TokenStream> {
    Ok(generate_fields_are_trait(fields, Self::ident(), span))
  }
}

pub struct TransparentWrapper;

impl TransparentWrapper {
  fn get_wrapper_type(
    attributes: &[Attribute], fields: &Fields,
  ) -> Option<TokenStream> {
    let transparent_param = get_simple_attr(attributes, "transparent");
    transparent_param.map(|ident| ident.to_token_stream()).or_else(|| {
      let mut types = get_field_types(fields);
      let first_type = types.next();
      if let Some(_) = types.next() {
        // can't guess param type if there is more than one field
        return None;
      } else {
        first_type.map(|ty| ty.to_token_stream())
      }
    })
  }
}

impl Derivable for TransparentWrapper {
  fn ident() -> TokenStream {
    quote!(::bytemuck::TransparentWrapper)
  }

  fn struct_asserts(
    _struct_name: &Ident, fields: &Fields, attributes: &[Attribute],
    _span: Span,
  ) -> Result<TokenStream, TokenStream> {
    let wrapped_type = match Self::get_wrapper_type(attributes, fields) {
      Some(wrapped_type) => wrapped_type.to_string(),
      None => return Err(quote!()), /* other code will already reject this
                                     * derive */
    };
    let mut wrapped_fields = get_fields(fields)
      .filter(|field| field.ty.to_token_stream().to_string() == wrapped_type);
    if let None = wrapped_fields.next() {
      return Err(quote! {
        compile_error!("TransparentWrapper must have one field of the wrapped type");
      });
    };
    if let Some(_) = wrapped_fields.next() {
      Err(quote! {
        compile_error!("TransparentWrapper can only have one field of the wrapped type");
      })
    } else {
      Ok(quote!())
    }
  }

  fn check_attributes(attributes: &[Attribute]) -> Result<(), TokenStream> {
    let repr = get_repr(attributes);

    match repr.as_ref().map(|repr| repr.as_str()) {
      Some("transparent") => Ok(()),
      _ => Err(quote! {
        compile_error!("TransparentWrapper requires the struct to be #[repr(transparent)]");
      }),
    }
  }

  fn generic_params(input: &DeriveInput) -> Result<TokenStream, TokenStream> {
    let fields = if let Data::Struct(DataStruct { fields, .. }) = &input.data {
      fields
    } else {
      return Err(quote! {
        compile_error!("deriving this trait is only supported for structs");
      });
    };

    Self::get_wrapper_type(&input.attrs, fields).map(|ty| quote!(<#ty>))
      .ok_or_else(|| quote! {
        compile_error!("when deriving TransparentWrapper for a struct with more than one field you need to specify the transparent field using #[transparent(T)]");
      })
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

fn get_ident_from_stream(tokens: TokenStream) -> Option<Ident> {
  match tokens.into_iter().next() {
    Some(TokenTree::Group(group)) => get_ident_from_stream(group.stream()),
    Some(TokenTree::Ident(ident)) => Some(ident),
    _ => None,
  }
}

/// get a simple #[foo(bar)] attribute, returning "bar"
fn get_simple_attr(attributes: &[Attribute], attr_name: &str) -> Option<Ident> {
  for attr in attributes {
    if let (AttrStyle::Outer, Some(outer_ident), Some(inner_ident)) = (
      &attr.style,
      attr.path.get_ident(),
      get_ident_from_stream(attr.tokens.clone()),
    ) {
      if outer_ident.to_string() == attr_name {
        return Some(inner_ident.clone());
      }
    }
  }

  None
}

fn get_repr(attributes: &[Attribute]) -> Option<String> {
  get_simple_attr(attributes, "repr").map(|ident| ident.to_string())
}
