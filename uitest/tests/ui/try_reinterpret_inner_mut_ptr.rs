use bytemuck::TryReinterpretInner;

fn main() {
  let _: *mut [u8; 2] = (&mut 0u8 as *mut u8).try_reinterpret_inner().unwrap();
  let _: *mut [u16; 2] =
    (&mut 0u16 as *mut u16).try_reinterpret_inner().unwrap();
  let _: *mut [u64; 2] =
    (&mut 0u64 as *mut u64).try_reinterpret_inner().unwrap();
}
