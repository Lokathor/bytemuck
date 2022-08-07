use proc_macro2::{Ident, Span, TokenStream, TokenTree};
use quote::{quote, quote_spanned, ToTokens};
use syn::{
  spanned::Spanned, AttrStyle, Attribute, Data, DataEnum, DataStruct,
  DataUnion, DeriveInput, Expr, ExprLit, ExprUnary, Fields, Lit, LitInt, Meta,
  NestedMeta, Type, UnOp, Variant,
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
    let repr = get_repr(&input.attrs);

    let completly_packed = repr.packed == Some(1);

    if !completly_packed && !input.generics.params.is_empty() {
      return Err("Pod requires cannot be derived for non-packed types containing generic parameters because the padding requirements can't be verified for generic non-packed structs");
    }

    match &input.data {
      Data::Struct(_) => {
        let assert_no_padding = if !completly_packed {
          Some(generate_assert_no_padding(input)?)
        } else {
          None
        };
        let assert_fields_are_pod =
          generate_fields_are_trait(input, Self::ident())?;

        Ok(quote!(
          #assert_no_padding
          #assert_fields_are_pod
        ))
      }
      Data::Enum(_) => Err("Deriving Pod is not supported for enums"),
      Data::Union(_) => Err("Deriving Pod is not supported for unions"),
    }
  }

  fn check_attributes(
    _ty: &Data, attributes: &[Attribute],
  ) -> Result<(), &'static str> {
    let repr = get_repr(attributes);
    match repr.repr {
      Repr::C => Ok(()),
      Repr::Transparent => Ok(()),
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

pub struct NoUninit;

impl Derivable for NoUninit {
  fn ident() -> TokenStream {
    quote!(::bytemuck::NoUninit)
  }

  fn check_attributes(
    ty: &Data, attributes: &[Attribute],
  ) -> Result<(), &'static str> {
    let repr = get_repr(attributes);
    match ty {
      Data::Struct(_) => match repr.repr {
        Repr::C | Repr::Transparent => Ok(()),
        _ => Err("NoUninit requires the struct to be #[repr(C)] or #[repr(transparent)]"),
      },
      Data::Enum(_) => if repr.repr.is_integer() {
        Ok(())
      } else {
        Err("NoUninit requires the enum to be an explicit #[repr(Int)]")
      },
      Data::Union(_) => Err("NoUninit can only be derived on enums and structs")
    }
  }

  fn asserts(input: &DeriveInput) -> Result<TokenStream, &'static str> {
    if !input.generics.params.is_empty() {
      return Err("NoUninit cannot be derived for structs containing generic parameters because the padding requirements can't be verified for generic structs");
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
          Err("Only fieldless enums are supported for NoUninit")
        } else {
          Ok(quote!())
        }
      }
      Data::Union(_) => Err("NoUninit cannot be derived for unions"), // shouldn't be possible since we already error in attribute check for this case
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
      Data::Struct(_) => match repr.repr {
        Repr::C | Repr::Transparent => Ok(()),
        _ => Err("CheckedBitPattern derive requires the struct to be #[repr(C)] or #[repr(transparent)]"),
      },
      Data::Enum(_) => if repr.repr.is_integer() {
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
      Data::Enum(_) => Ok(quote!()), // nothing needed, already guaranteed OK by NoUninit
      Data::Union(_) => Err("Internal error in CheckedBitPattern derive"), // shouldn't be possible since we already error in attribute check for this case
    }
  }

  fn trait_impl(
    input: &DeriveInput,
  ) -> Result<(TokenStream, TokenStream), &'static str> {
    match &input.data {
      Data::Struct(DataStruct { fields, .. }) => Ok(
        generate_checked_bit_pattern_struct(&input.ident, fields, &input.attrs),
      ),
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

    match repr.repr {
      Repr::Transparent => Ok(()),
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
    let repr = get_repr(&input.attrs);

    let integer_ty = if let Some(integer_ty) = repr.repr.as_integer_type() {
      integer_ty
    } else {
      return Err("Contiguous requires the enum to be #[repr(Int)]");
    };

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

    let min_lit = LitInt::new(&format!("{}", min), input.span());
    let max_lit = LitInt::new(&format!("{}", max), input.span());

    Ok((
      quote!(),
      quote! {
          type Int = #integer_ty;
          const MIN_VALUE: #integer_ty = #min_lit;
          const MAX_VALUE: #integer_ty = #max_lit;
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
    Data::Enum(_) => Err("deriving this trait is not supported for enums"),
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

  let repr = get_repr(attrs);

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
        #repr
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

  let repr = get_repr(&input.attrs);
  let integer_ty = repr.repr.as_integer_type().unwrap(); // should be checked in attr check already
  Ok((
    quote!(),
    quote! {
        type Bits = #integer_ty;

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
      #[allow(clippy::missing_const_for_fn)]
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

#[derive(Clone, Copy)]
struct Representation {
  packed: Option<u32>,
  repr: Repr,
}

impl ToTokens for Representation {
  fn to_tokens(&self, tokens: &mut TokenStream) {
    let repr = match self.repr {
      Repr::Rust => None,
      Repr::C => Some("C"),
      Repr::Transparent => Some("transparent"),
      Repr::U8 => Some("u8"),
      Repr::I8 => Some("i8"),
      Repr::U16 => Some("u16"),
      Repr::I16 => Some("i16"),
      Repr::U32 => Some("u32"),
      Repr::I32 => Some("i32"),
      Repr::U64 => Some("u64"),
      Repr::I64 => Some("i64"),
      Repr::I128 => Some("i128"),
      Repr::U128 => Some("u128"),
    };
    if let Some(repr) = repr {
      let ident = Ident::new(repr, Span::call_site());
      tokens.extend(quote! {
        #[repr(#ident)]
      });
    }

    if let Some(packed) = self.packed {
      let lit = LitInt::new(&packed.to_string(), Span::call_site());
      tokens.extend(quote! {
        #[repr(packed(#lit))]
      });
    }
  }
}

#[derive(Clone, Copy)]
enum Repr {
  Rust,
  C,
  Transparent,
  U8,
  I8,
  U16,
  I16,
  U32,
  I32,
  U64,
  I64,
  I128,
  U128,
}

impl Repr {
  fn is_integer(&self) -> bool {
    match *self {
      Repr::Rust | Repr::C | Repr::Transparent => false,
      Repr::U8
      | Repr::I8
      | Repr::U16
      | Repr::I16
      | Repr::U32
      | Repr::I32
      | Repr::U64
      | Repr::I64
      | Repr::I128
      | Repr::U128 => true,
    }
  }

  fn as_integer_type(&self) -> Option<TokenStream> {
    match self {
      Repr::Rust | Repr::C | Repr::Transparent => None,
      Repr::U8 => Some(quote! { ::core::primitive::u8 }),
      Repr::I8 => Some(quote! { ::core::primitive::i8 }),
      Repr::U16 => Some(quote! { ::core::primitive::u16 }),
      Repr::I16 => Some(quote! { ::core::primitive::i16 }),
      Repr::U32 => Some(quote! { ::core::primitive::u32 }),
      Repr::I32 => Some(quote! { ::core::primitive::i32 }),
      Repr::U64 => Some(quote! { ::core::primitive::u64 }),
      Repr::I64 => Some(quote! { ::core::primitive::i64 }),
      Repr::I128 => Some(quote! { ::core::primitive::u128 }),
      Repr::U128 => Some(quote! { ::core::primitive::i128 }),
    }
  }
}

fn get_repr(attributes: &[Attribute]) -> Representation {
  let mut repr = Representation { packed: None, repr: Repr::Rust };

  for attr in attributes {
    let meta = if let Ok(meta) = attr.parse_meta() { meta } else { continue };
    if !meta.path().is_ident("repr") {
      continue;
    }
    let list = if let Meta::List(list) = meta {
      list
    } else {
      // The other `Meta` variants are illegal for `repr`.
      continue;
    };

    for item in list.nested {
      let meta = if let NestedMeta::Meta(meta) = item {
        meta
      } else {
        // Other nested items are illegal for `repr`.
        continue;
      };

      match meta.path() {
        path if path.is_ident("C") => {
          repr.repr = Repr::C;
        }
        path if path.is_ident("transparent") => {
          repr.repr = Repr::Transparent;
        }
        path if path.is_ident("u8") => {
          repr.repr = Repr::U8;
        }
        path if path.is_ident("i8") => {
          repr.repr = Repr::I8;
        }
        path if path.is_ident("u16") => {
          repr.repr = Repr::U16;
        }
        path if path.is_ident("i16") => {
          repr.repr = Repr::I16;
        }
        path if path.is_ident("u32") => {
          repr.repr = Repr::U32;
        }
        path if path.is_ident("i32") => {
          repr.repr = Repr::I32;
        }
        path if path.is_ident("u64") => {
          repr.repr = Repr::U64;
        }
        path if path.is_ident("i64") => {
          repr.repr = Repr::I64;
        }
        path if path.is_ident("u128") => {
          repr.repr = Repr::U128;
        }
        path if path.is_ident("i128") => {
          repr.repr = Repr::I128;
        }
        path if path.is_ident("packed") => {
          let packed_alignment = match meta {
            Meta::Path(_) => 1,
            Meta::List(list) => {
              if list.nested.len() != 1 {
                // `repr(packed(n))` must have exactly one nested item.
                continue;
              }

              let nested = &list.nested[0];
              let int_lit = if let NestedMeta::Lit(Lit::Int(int_lit)) = nested {
                int_lit
              } else {
                // The nested item must be an integer literal.
                continue;
              };

              let value = if let Ok(value) = int_lit.base10_parse::<u32>() {
                value
              } else {
                // The literal must be positive and less than 2^29.
                continue;
              };
              value
            }
            Meta::NameValue(_) => {
              // `repr(packed)` doesn't support name value syntax.
              continue;
            }
          };

          let new_packed_alignment = match repr.packed {
            Some(prev) => u32::min(prev, packed_alignment),
            None => packed_alignment,
          };
          repr.packed = Some(new_packed_alignment);
        }
        _ => {}
      }
    }
  }

  repr
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
