use bytemuck::ReinterpretInner;

fn main() {
  let _: &mut u16 = (&mut [0u8; 2]).reinterpret_inner();
  let _: &mut u32 = (&mut [0u16; 2]).reinterpret_inner();
  let _: &mut u64 = (&mut [0u32; 2]).reinterpret_inner();

  let _: &mut [u8; 2] = (&mut 0u8).reinterpret_inner();
  let _: &mut [u16; 2] = (&mut 0u16).reinterpret_inner();
  let _: &mut [u64; 2] = (&mut 0u64).reinterpret_inner();

  let _: &mut [u16] = [[0u8; 2]; 2].as_mut_slice().reinterpret_inner();
  let _: &mut [u32] = [[0u16; 2]; 2].as_mut_slice().reinterpret_inner();
  let _: &mut [u64] = [[0u32; 2]; 2].as_mut_slice().reinterpret_inner();

  let _: &mut [()] = [0u8; 1].as_mut_slice().reinterpret_inner();
  let _: &mut [[u8; 2]] = [[0u8; 3]; 3].as_mut_slice().reinterpret_inner();
  let _: &mut [[u16; 2]] = [[0u16; 5]; 1].as_mut_slice().reinterpret_inner();
  let _: &mut [[u32; 2]] = [[0u32; 1]; 1].as_mut_slice().reinterpret_inner();
}
