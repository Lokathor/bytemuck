use proc_macro2::{Ident, TokenStream, TokenTree};
use quote::{quote, quote_spanned, ToTokens};
use syn::{
  spanned::Spanned, AttrStyle, Attribute, Data, DataEnum, DataStruct,
  DeriveInput, Expr, ExprLit, ExprUnary, Fields, Lit, LitInt, Type, UnOp,
  Variant,
};

pub trait Derivable {
  fn ident() -> TokenStream;
  fn generic_params(_input: &DeriveInput) -> Result<TokenStream, &'static str> {
    Ok(quote!())
  }
  fn struct_asserts(_input: &DeriveInput) -> Result<TokenStream, &'static str> {
    Ok(quote!())
  }
  fn check_attributes(_attributes: &[Attribute]) -> Result<(), &'static str> {
    Ok(())
  }
  fn trait_impl(_input: &DeriveInput) -> Result<TokenStream, &'static str> {
    Ok(quote!())
  }
}

pub struct Pod;

impl Derivable for Pod {
  fn ident() -> TokenStream {
    quote!(::bytemuck::Pod)
  }

  fn struct_asserts(input: &DeriveInput) -> Result<TokenStream, &'static str> {
    if !input.generics.params.is_empty() {
      return Err("Pod requires cannot be derived for structs containing generic parameters because the padding requirements can't be verified for generic structs");
    }

    let assert_no_padding = generate_assert_no_padding(input)?;
    let assert_fields_are_pod =
      generate_fields_are_trait(input, Self::ident())?;

    Ok(quote!(
      #assert_no_padding
      #assert_fields_are_pod
    ))
  }

  fn check_attributes(attributes: &[Attribute]) -> Result<(), &'static str> {
    let repr = get_repr(attributes);
    match repr.as_ref().map(|repr| repr.as_str()) {
      Some("C") => Ok(()),
      Some("transparent") => Ok(()),
      _ => {
        Err("Pod requires the struct to be #[repr(C)] or #[repr(transparent)]")
      }
    }
  }
}

pub struct Zeroable;

impl Derivable for Zeroable {
  fn ident() -> TokenStream {
    quote!(::bytemuck::Zeroable)
  }

  fn struct_asserts(input: &DeriveInput) -> Result<TokenStream, &'static str> {
    generate_fields_are_trait(input, Self::ident())
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

  fn generic_params(input: &DeriveInput) -> Result<TokenStream, &'static str> {
    let fields = get_struct_fields(input)?;

    Self::get_wrapper_type(&input.attrs, fields).map(|ty| quote!(<#ty>))
            .ok_or("when deriving TransparentWrapper for a struct with more than one field you need to specify the transparent field using #[transparent(T)]")
  }

  fn struct_asserts(input: &DeriveInput) -> Result<TokenStream, &'static str> {
    let fields = get_struct_fields(input)?;
    let wrapped_type = match Self::get_wrapper_type(&input.attrs, fields) {
      Some(wrapped_type) => wrapped_type.to_string(),
      None => unreachable!(), /* other code will already reject this derive */
    };
    let mut wrapped_fields = fields
      .iter()
      .filter(|field| field.ty.to_token_stream().to_string() == wrapped_type);
    if let None = wrapped_fields.next() {
      return Err("TransparentWrapper must have one field of the wrapped type");
    };
    if let Some(_) = wrapped_fields.next() {
      Err("TransparentWrapper can only have one field of the wrapped type")
    } else {
      Ok(quote!())
    }
  }

  fn check_attributes(attributes: &[Attribute]) -> Result<(), &'static str> {
    let repr = get_repr(attributes);

    match repr.as_ref().map(|repr| repr.as_str()) {
      Some("transparent") => Ok(()),
      _ => {
        Err("TransparentWrapper requires the struct to be #[repr(transparent)]")
      }
    }
  }
}

pub struct Contiguous;

impl Derivable for Contiguous {
  fn ident() -> TokenStream {
    quote!(::bytemuck::Contiguous)
  }

  fn trait_impl(input: &DeriveInput) -> Result<TokenStream, &'static str> {
    let repr = get_repr(&input.attrs)
      .ok_or("Contiguous requires the enum to be #[repr(Int)]")?;

    if !repr.starts_with('u') && !repr.starts_with('i') {
      return Err("Contiguous requires the enum to be #[repr(Int)]");
    }

    let variants = get_enum_variants(input)?;
    let mut variants_with_discriminator =
      VariantDiscriminantIterator::new(variants);

    let (min, max, count) = variants_with_discriminator.try_fold(
      (i64::max_value(), i64::min_value(), 0),
      |(min, max, count), res| {
        let discriminator = res?;
        Ok((
          i64::min(min, discriminator),
          i64::max(max, discriminator),
          count + 1,
        ))
      },
    )?;

    if max - min != count - 1 {
      return Err(
        "Contiguous requires the enum discriminants to be contiguous",
      );
    }

    let repr_ident = Ident::new(&repr, input.span());
    let min_lit = LitInt::new(&format!("{}", min), input.span());
    let max_lit = LitInt::new(&format!("{}", max), input.span());

    Ok(quote! {
        type Int = #repr_ident;
        const MIN_VALUE: #repr_ident = #min_lit;
        const MAX_VALUE: #repr_ident = #max_lit;
    })
  }
}

fn get_struct_fields(input: &DeriveInput) -> Result<&Fields, &'static str> {
  if let Data::Struct(DataStruct { fields, .. }) = &input.data {
    Ok(fields)
  } else {
    Err("deriving this trait is only supported for structs")
  }
}

fn get_enum_variants<'a>(
  input: &'a DeriveInput,
) -> Result<impl Iterator<Item = &'a Variant> + 'a, &'static str> {
  if let Data::Enum(DataEnum { variants, .. }) = &input.data {
    Ok(variants.iter())
  } else {
    Err("deriving this trait is only supported for enums")
  }
}

fn get_field_types<'a>(
  fields: &'a Fields,
) -> impl Iterator<Item = &'a Type> + 'a {
  fields.iter().map(|field| &field.ty)
}

/// Check that a struct has no padding by asserting that the size of the struct
/// is equal to the sum of the size of it's fields
fn generate_assert_no_padding(
  input: &DeriveInput,
) -> Result<TokenStream, &'static str> {
  let struct_type = &input.ident;
  let span = input.ident.span();
  let fields = get_struct_fields(input)?;

  let mut field_types = get_field_types(&fields);
  let size_sum = if let Some(first) = field_types.next() {
    let size_first = quote_spanned!(span => ::core::mem::size_of::<#first>());
    let size_rest =
      quote_spanned!(span => #( + ::core::mem::size_of::<#field_types>() )*);

    quote_spanned!(span => #size_first#size_rest)
  } else {
    quote_spanned!(span => 0)
  };

  Ok(quote_spanned! {span => const _: fn() = || {
    struct TypeWithoutPadding([u8; #size_sum]);
    let _ = ::core::mem::transmute::<#struct_type, TypeWithoutPadding>;
  };})
}

/// Check that all fields implement a given trait
fn generate_fields_are_trait(
  input: &DeriveInput, trait_: TokenStream,
) -> Result<TokenStream, &'static str> {
  let (impl_generics, _ty_generics, where_clause) =
    input.generics.split_for_impl();
  let fields = get_struct_fields(input)?;
  let span = input.span();
  let field_types = get_field_types(&fields);
  Ok(quote_spanned! {span => #(const _: fn() = || {
      fn check #impl_generics () #where_clause {
        fn assert_impl<T: #trait_>() {}
        assert_impl::<#field_types>();
      }
    };)*
  })
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
        return Some(inner_ident);
      }
    }
  }

  None
}

fn get_repr(attributes: &[Attribute]) -> Option<String> {
  get_simple_attr(attributes, "repr").map(|ident| ident.to_string())
}

struct VariantDiscriminantIterator<'a, I: Iterator<Item = &'a Variant> + 'a> {
  inner: I,
  last_value: i64,
}

impl<'a, I: Iterator<Item = &'a Variant> + 'a>
  VariantDiscriminantIterator<'a, I>
{
  fn new(inner: I) -> Self {
    VariantDiscriminantIterator { inner, last_value: -1 }
  }
}

impl<'a, I: Iterator<Item = &'a Variant> + 'a> Iterator
  for VariantDiscriminantIterator<'a, I>
{
  type Item = Result<i64, &'static str>;

  fn next(&mut self) -> Option<Self::Item> {
    let variant = self.inner.next()?;
    if !variant.fields.is_empty() {
      return Some(Err("Only fieldless enums are supported"));
    }

    if let Some((_, discriminant)) = &variant.discriminant {
      let discriminant_value = match parse_int_expr(discriminant) {
        Ok(value) => value,
        Err(e) => return Some(Err(e)),
      };
      self.last_value = discriminant_value;
    } else {
      self.last_value += 1;
    }

    Some(Ok(self.last_value))
  }
}

fn parse_int_expr(expr: &Expr) -> Result<i64, &'static str> {
  match expr {
    Expr::Unary(ExprUnary { op: UnOp::Neg(_), expr, .. }) => {
      parse_int_expr(expr).map(|int| -int)
    }
    Expr::Lit(ExprLit { lit: Lit::Int(int), .. }) => {
      int.base10_parse().map_err(|_| "Invalid integer expression")
    }
    _ => Err("Not an integer expression"),
  }
}
