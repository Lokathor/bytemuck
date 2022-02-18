use proc_macro2::{Ident, TokenStream, TokenTree};
use quote::{quote, quote_spanned, ToTokens, format_ident};
use syn::{
  spanned::Spanned, AttrStyle, Attribute, Data, DataEnum, DataStruct,
  DeriveInput, Expr, ExprLit, ExprUnary, Fields, Lit, LitInt, Type, UnOp,
  Variant, DataUnion,
};

pub trait Derivable {
  fn ident() -> TokenStream;
  fn implies_trait() -> Option<TokenStream> {
    None
  }
  fn generic_params(_input: &DeriveInput) -> Result<TokenStream, &'static str> {
    Ok(quote!())
  }
  fn asserts(_input: &DeriveInput) -> Result<TokenStream, &'static str> {
    Ok(quote!())
  }
  fn check_attributes(
    _ty: &Data, _attributes: &[Attribute],
  ) -> Result<(), &'static str> {
    Ok(())
  }
  fn trait_impl(
    _input: &DeriveInput,
  ) -> Result<(TokenStream, TokenStream), &'static str> {
    Ok((quote!(), quote!()))
  }
}

pub struct Pod;

impl Derivable for Pod {
  fn ident() -> TokenStream {
    quote!(::bytemuck::Pod)
  }

  fn asserts(input: &DeriveInput) -> Result<TokenStream, &'static str> {
    if !input.generics.params.is_empty() {
      return Err("Pod requires cannot be derived for types containing generic parameters because the padding requirements can't be verified for generic structs");
    }

    match &input.data {
      Data::Struct(_) => {
        let assert_no_padding = generate_assert_no_padding(input)?;
        let assert_fields_are_pod =
          generate_fields_are_trait(input, Self::ident())?;

        Ok(quote!(
          #assert_no_padding
          #assert_fields_are_pod
        ))
      },
      Data::Union(_) => NoPadding::asserts(input),
      Data::Enum(_) => Err("Deriving Pod is not supported for enums"),
    }
  }

  fn check_attributes(
    _ty: &Data, attributes: &[Attribute],
  ) -> Result<(), &'static str> {
    let repr = get_repr(attributes);
    match repr.as_ref().map(|repr| repr.as_str()) {
      Some("C") => Ok(()),
      Some("transparent") => Ok(()),
      _ => {
        Err("Pod requires the type to be #[repr(C)] or #[repr(transparent)]")
      }
    }
  }
}

pub struct AnyBitPattern;

impl Derivable for AnyBitPattern {
  fn ident() -> TokenStream {
    quote!(::bytemuck::AnyBitPattern)
  }

  fn implies_trait() -> Option<TokenStream> {
    Some(quote!(::bytemuck::Zeroable))
  }

  fn asserts(input: &DeriveInput) -> Result<TokenStream, &'static str> {
    match &input.data {
      Data::Union(_) => Ok(quote!()), // unions are always `AnyBitPattern`
      Data::Struct(_) => generate_fields_are_trait(input, Self::ident()),
      Data::Enum(_) => Err("Deriving AnyBitPattern is not supported for enums"),
    }
  }
}

pub struct Zeroable;

impl Derivable for Zeroable {
  fn ident() -> TokenStream {
    quote!(::bytemuck::Zeroable)
  }

  fn asserts(input: &DeriveInput) -> Result<TokenStream, &'static str> {
    match &input.data {
      Data::Union(_) => Ok(quote!()), // unions are always `Zeroable`
      Data::Struct(_) => generate_fields_are_trait(input, Self::ident()),
      Data::Enum(_) => Err("Deriving Zeroable is not supported for enums"),
    }
  }
}

pub struct NoPadding;

impl Derivable for NoPadding {
  fn ident() -> TokenStream {
    quote!(::bytemuck::NoPadding)
  }

  fn check_attributes(
    ty: &Data, attributes: &[Attribute],
  ) -> Result<(), &'static str> {
    let repr = get_repr(attributes);
    match ty {
      Data::Struct(_) | Data::Union(_) => match repr.as_deref() {
        Some("C" | "transparent") => Ok(()),
        _ => Err("NoPadding derive requires the type to be #[repr(C)] or #[repr(transparent)]"),
      },
      Data::Enum(_) => if repr.map(|repr| repr.starts_with('u') || repr.starts_with('i')) == Some(true) {
        Ok(())
      } else {
        Err("NoPadding requires the enum to be an explicit #[repr(Int)]")
      },
    }
  }

  fn asserts(input: &DeriveInput) -> Result<TokenStream, &'static str> {
    if !input.generics.params.is_empty() {
      return Err("NoPadding cannot be derived for structs containing generic parameters because the padding requirements can't be verified for generic structs");
    }

    match &input.data {
      Data::Struct(DataStruct { .. }) => {
        let assert_no_padding = generate_assert_no_padding(&input)?;
        let assert_fields_are_no_padding =
          generate_fields_are_trait(&input, Self::ident())?;

        Ok(quote!(
            #assert_no_padding
            #assert_fields_are_no_padding
        ))
      }
      Data::Enum(DataEnum { variants, .. }) => {
        if variants.iter().any(|variant| !variant.fields.is_empty()) {
          Err("Only fieldless enums are supported for NoPadding")
        } else {
          Ok(quote!())
        }
      }
      Data::Union(_) => {
        let assert_no_padding = generate_assert_no_padding_union(&input)?;
        let assert_fields_are_no_padding =
          generate_fields_are_trait(&input, Self::ident())?;

        Ok(quote!(
            #assert_no_padding
            #assert_fields_are_no_padding
        ))
      }
    }
  }

  fn trait_impl(
    _input: &DeriveInput,
  ) -> Result<(TokenStream, TokenStream), &'static str> {
    Ok((quote!(), quote!()))
  }
}

pub struct CheckedBitPattern;

impl Derivable for CheckedBitPattern {
  fn ident() -> TokenStream {
    quote!(::bytemuck::CheckedBitPattern)
  }

  fn check_attributes(
    ty: &Data, attributes: &[Attribute],
  ) -> Result<(), &'static str> {
    let repr = get_repr(attributes);
    match ty {
      Data::Struct(_) => match repr.as_deref() {
        Some("C" | "transparent") => Ok(()),
        _ => Err("CheckedBitPattern derive requires the struct to be #[repr(C)] or #[repr(transparent)]"),
      },
      Data::Enum(_) => if repr.map(|repr| repr.starts_with('u') || repr.starts_with('i')) == Some(true) {
        Ok(())
      } else {
        Err("CheckedBitPattern requires the enum to be an explicit #[repr(Int)]")
      },
      Data::Union(_) => Err("CheckedBitPattern can only be derived on enums and structs")
    }
  }

  fn asserts(input: &DeriveInput) -> Result<TokenStream, &'static str> {
    if !input.generics.params.is_empty() {
      return Err("CheckedBitPattern cannot be derived for structs containing generic parameters");
    }

    match &input.data {
      Data::Struct(DataStruct { .. }) => {
        let assert_fields_are_maybe_pod =
          generate_fields_are_trait(&input, Self::ident())?;

        Ok(assert_fields_are_maybe_pod)
      }
      Data::Enum(_) => Ok(quote!()), // nothing needed, already guaranteed OK by NoPadding
      Data::Union(_) => Err("Internal error in CheckedBitPattern derive"), // shouldn't be possible since we already error in attribute check for this case
    }
  }

  fn trait_impl(
    input: &DeriveInput,
  ) -> Result<(TokenStream, TokenStream), &'static str> {
    match &input.data {
      Data::Struct(DataStruct { fields, .. }) => {
        Ok(generate_checked_bit_pattern_struct(&input.ident, fields, &input.attrs))
      }
      Data::Enum(_) => generate_checked_bit_pattern_enum(input),
      Data::Union(_) => Err("Internal error in CheckedBitPattern derive"), // shouldn't be possible since we already error in attribute check for this case
    }
  }
}

pub struct TransparentWrapper;

impl TransparentWrapper {
  fn get_wrapper_type(
    attributes: &[Attribute], fields: &Fields,
  ) -> Option<TokenStream> {
    let transparent_param = get_simple_attr(attributes, "transparent");
    transparent_param.map(|ident| ident.to_token_stream()).or_else(|| {
      let mut types = get_field_types(&fields);
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

    Self::get_wrapper_type(&input.attrs, &fields).map(|ty| quote!(<#ty>))
            .ok_or("when deriving TransparentWrapper for a struct with more than one field you need to specify the transparent field using #[transparent(T)]")
  }

  fn asserts(input: &DeriveInput) -> Result<TokenStream, &'static str> {
    let fields = get_struct_fields(input)?;
    let wrapped_type = match Self::get_wrapper_type(&input.attrs, &fields) {
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

  fn check_attributes(
    _ty: &Data, attributes: &[Attribute],
  ) -> Result<(), &'static str> {
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

  fn trait_impl(
    input: &DeriveInput,
  ) -> Result<(TokenStream, TokenStream), &'static str> {
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

    Ok((
      quote!(),
      quote! {
          type Int = #repr_ident;
          const MIN_VALUE: #repr_ident = #min_lit;
          const MAX_VALUE: #repr_ident = #max_lit;
      },
    ))
  }
}

fn get_struct_fields(input: &DeriveInput) -> Result<&Fields, &'static str> {
  if let Data::Struct(DataStruct { fields, .. }) = &input.data {
    Ok(fields)
  } else {
    Err("deriving this trait is only supported for structs")
  }
}

fn get_fields(input: &DeriveInput) -> Result<Fields, &'static str> {
  match &input.data {
    Data::Struct(DataStruct { fields, .. }) => Ok(fields.clone()),
    Data::Union(DataUnion { fields, .. }) => Ok(Fields::Named(fields.clone())),
    Data::Enum(_) => Err("deriving this trait is not supported for enums")
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

fn generate_checked_bit_pattern_struct(
  input_ident: &Ident, fields: &Fields, attrs: &[Attribute],
) -> (TokenStream, TokenStream) {
  let bits_ty = Ident::new(&format!("{}Bits", input_ident), input_ident.span());

  let repr = get_simple_attr(attrs, "repr").unwrap(); // should be checked in attr check already

  let field_names = fields
    .iter()
    .enumerate()
    .map(|(i, field)| {
      field.ident.clone().unwrap_or_else(|| {
        Ident::new(&format!("field{}", i), input_ident.span())
      })
    })
    .collect::<Vec<_>>();
  let field_tys = fields.iter().map(|field| &field.ty).collect::<Vec<_>>();

  let field_name = &field_names[..];
  let field_ty = &field_tys[..];

  #[cfg(not(target_arch = "spirv"))]
  let derive_dbg = quote!(#[derive(Debug)]);
  #[cfg(target_arch = "spirv")]
  let derive_dbg = quote!();

  (
    quote! {
        #[repr(#repr)]
        #[derive(Clone, Copy, ::bytemuck::AnyBitPattern)]
        #derive_dbg
        pub struct #bits_ty {
            #(#field_name: <#field_ty as ::bytemuck::CheckedBitPattern>::Bits,)*
        }
    },
    quote! {
        type Bits = #bits_ty;

        #[inline]
        #[allow(clippy::double_comparisons)]
        fn is_valid_bit_pattern(bits: &#bits_ty) -> bool {
            #(<#field_ty as ::bytemuck::CheckedBitPattern>::is_valid_bit_pattern(&bits.#field_name) && )* true
        }
    },
  )
}

fn generate_checked_bit_pattern_enum(
  input: &DeriveInput,
) -> Result<(TokenStream, TokenStream), &'static str> {
  let span = input.span();
  let mut variants_with_discriminant =
    VariantDiscriminantIterator::new(get_enum_variants(input)?);

  let (min, max, count) = variants_with_discriminant.try_fold(
    (i64::max_value(), i64::min_value(), 0),
    |(min, max, count), res| {
      let discriminant = res?;
      Ok((i64::min(min, discriminant), i64::max(max, discriminant), count + 1))
    },
  )?;

  let check = if count == 0 {
    quote_spanned!(span => false)
  } else if max - min == count - 1 {
    // contiguous range
    let min_lit = LitInt::new(&format!("{}", min), span);
    let max_lit = LitInt::new(&format!("{}", max), span);

    quote!(*bits >= #min_lit && *bits <= #max_lit)
  } else {
    // not contiguous range, check for each
    let variant_lits =
      VariantDiscriminantIterator::new(get_enum_variants(input)?)
        .map(|res| {
          let variant = res?;
          Ok(LitInt::new(&format!("{}", variant), span))
        })
        .collect::<Result<Vec<_>, _>>()?;

    // count is at least 1
    let first = &variant_lits[0];
    let rest = &variant_lits[1..];

    quote!(matches!(*bits, #first #(| #rest )*))
  };

  let repr = get_simple_attr(&input.attrs, "repr").unwrap(); // should be checked in attr check already
  Ok((
    quote!(),
    quote! {
        type Bits = #repr;

        #[inline]
        #[allow(clippy::double_comparisons)]
        fn is_valid_bit_pattern(bits: &Self::Bits) -> bool {
            #check
        }
    },
  ))
}

/// Check that a struct has no padding by asserting that the size of the struct
/// is equal to the sum of the size of it's fields
fn generate_assert_no_padding(
  input: &DeriveInput,
) -> Result<TokenStream, &'static str> {
  let struct_type = &input.ident;
  let span = input.ident.span();
  let fields = get_fields(input)?;

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

fn generate_assert_no_padding_union(
  input: &DeriveInput,
) -> Result<TokenStream, &'static str> {
  let input_ident = &input.ident;
  let fields = get_fields(input)?;

  let max_field_size_ident = format_ident!("{}_MaxFieldSize", input_ident);

  let field_size_structs = fields.iter().map(|field| {
    let field_span = field.span();
    let field_ty = &field.ty;
    let field_ident = field.ident.as_ref().unwrap(); // unions only have named fields
    let ident = format_ident!("Field_{}_Size", field_ident);
    (
      ident.clone(),
      field_span,
      quote_spanned! { field_span=>
        #[allow(non_camel_case_types)]
        struct #ident([u8; ::core::mem::size_of::<#field_ty>()]);
      },
    )
  }).collect::<Vec<_>>();

  let asserts = field_size_structs.iter().map(|(field_size_struct, span, declaration)| {
    quote_spanned! {*span=>
      #declaration
      let _ = ::core::mem::transmute::<#max_field_size_ident, #field_size_struct>;
    }
  });

  Ok(quote_spanned! { input.span()=> const _: fn() = || {
    #[allow(non_camel_case_types)]
    struct #max_field_size_ident([u8; ::core::mem::size_of::<#input_ident>()]);

    #( #asserts )*
  };})
}

/// Check that all fields implement a given trait
fn generate_fields_are_trait(
  input: &DeriveInput, trait_: TokenStream,
) -> Result<TokenStream, &'static str> {
  let (impl_generics, _ty_generics, where_clause) =
    input.generics.split_for_impl();
  let fields = get_fields(input)?;
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
