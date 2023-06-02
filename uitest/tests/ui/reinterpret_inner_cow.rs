use bytemuck::ReinterpretInner;
use std::borrow::Cow;

fn main() {
  let _: Cow<u16> = Cow::Borrowed(&[0u8; 2]).reinterpret_inner();
  let _: Cow<u32> = Cow::Borrowed(&[0u16; 2]).reinterpret_inner();
  let _: Cow<u64> = Cow::Borrowed(&[0u32; 2]).reinterpret_inner();

  let _: Cow<[u8; 2]> = Cow::Borrowed(&0u8).reinterpret_inner();
  let _: Cow<[u16; 2]> = Cow::Borrowed(&0u16).reinterpret_inner();
  let _: Cow<[u64; 2]> = Cow::Borrowed(&0u64).reinterpret_inner();

  let _: Cow<[u16]> =
    Cow::Borrowed([[0u8; 2]; 2].as_slice()).reinterpret_inner();
  let _: Cow<[u32]> =
    Cow::Borrowed([[0u16; 2]; 2].as_slice()).reinterpret_inner();
  let _: Cow<[[u32; 2]]> =
    Cow::Borrowed([0u64; 2].as_slice()).reinterpret_inner();

  let _: Cow<[()]> = Cow::Borrowed([0u8; 1].as_slice()).reinterpret_inner();
  let _: Cow<[[u8; 2]]> =
    Cow::Borrowed([[0u8; 3]; 3].as_slice()).reinterpret_inner();
  let _: Cow<[[u16; 2]]> =
    Cow::Borrowed([[0u16; 5]; 1].as_slice()).reinterpret_inner();
  let _: Cow<[[u32; 2]]> =
    Cow::Borrowed([[0u32; 1]; 1].as_slice()).reinterpret_inner();
}
