use bytemuck::ReinterpretInner;

fn main() {
  let _: *mut u16 = (&mut [0u8; 2] as *mut [u8; 2]).reinterpret_inner();
  let _: *mut u32 = (&mut [0u16; 2] as *mut [u16; 2]).reinterpret_inner();
  let _: *mut u64 = (&mut [0u32; 2] as *mut [u32; 2]).reinterpret_inner();

  let _: *mut [u8; 2] = (&mut 0u8 as *mut u8).reinterpret_inner();
  let _: *mut [u16; 2] = (&mut 0u16 as *mut u16).reinterpret_inner();
  let _: *mut [u64; 2] = (&mut 0u64 as *mut u64).reinterpret_inner();
}
