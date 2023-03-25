use bytemuck::TryReinterpretInner;
use core::ptr::NonNull;

fn main() {
  let _: NonNull<[u8; 2]> =
    NonNull::from(&mut 0u8).try_reinterpret_inner().unwrap();
  let _: NonNull<[u16; 2]> =
    NonNull::from(&mut 0u16).try_reinterpret_inner().unwrap();
  let _: NonNull<[u64; 2]> =
    NonNull::from(&mut 0u64).try_reinterpret_inner().unwrap();

  let _: NonNull<[()]> =
    NonNull::from([0u8; 1].as_mut_slice()).try_reinterpret_inner().unwrap();
}
