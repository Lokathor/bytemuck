use bytemuck::TryReinterpretInner;
use std::borrow::Cow;

fn main() {
  let _: Cow<[u8; 2]> = Cow::Borrowed(&0u8).try_reinterpret_inner().unwrap();
  let _: Cow<[u16; 2]> = Cow::Borrowed(&0u16).try_reinterpret_inner().unwrap();
  let _: Cow<[u64; 2]> = Cow::Borrowed(&0u64).try_reinterpret_inner().unwrap();

  let _: Cow<[u16]> =
    Cow::Borrowed([[0u8; 2]; 2].as_slice()).try_reinterpret_inner().unwrap();
  let _: Cow<[u32]> =
    Cow::Borrowed([[0u16; 2]; 2].as_slice()).try_reinterpret_inner().unwrap();
  let _: Cow<[[u32; 2]]> =
    Cow::Borrowed([0u64; 2].as_slice()).try_reinterpret_inner().unwrap();

  let _: Cow<[()]> =
    Cow::Borrowed([0u8; 1].as_slice()).try_reinterpret_inner().unwrap();
}
