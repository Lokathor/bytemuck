use bytemuck::ReinterpretInner;
use core::ptr::NonNull;

fn main() {
  let _: NonNull<u16> = NonNull::from(&mut [0u8; 2]).reinterpret_inner();
  let _: NonNull<u32> = NonNull::from(&mut [0u16; 2]).reinterpret_inner();
  let _: NonNull<u64> = NonNull::from(&mut [0u32; 2]).reinterpret_inner();

  let _: NonNull<[u8; 2]> = NonNull::from(&mut 0u8).reinterpret_inner();
  let _: NonNull<[u16; 2]> = NonNull::from(&mut 0u16).reinterpret_inner();
  let _: NonNull<[u64; 2]> = NonNull::from(&mut 0u64).reinterpret_inner();

  let _: NonNull<[u16]> =
    NonNull::from([[0u8; 2]; 2].as_mut_slice()).reinterpret_inner();
  let _: NonNull<[u32]> =
    NonNull::from([[0u16; 2]; 2].as_mut_slice()).reinterpret_inner();
  let _: NonNull<[u64]> =
    NonNull::from([[0u32; 2]; 2].as_mut_slice()).reinterpret_inner();

  let _: NonNull<[()]> =
    NonNull::from([0u8; 1].as_mut_slice()).reinterpret_inner();
  let _: NonNull<[[u8; 2]]> =
    NonNull::from([[0u8; 3]; 3].as_mut_slice()).reinterpret_inner();
  let _: NonNull<[[u16; 2]]> =
    NonNull::from([[0u16; 5]; 1].as_mut_slice()).reinterpret_inner();
  let _: NonNull<[[u32; 2]]> =
    NonNull::from([[0u32; 1]; 1].as_mut_slice()).reinterpret_inner();
}
