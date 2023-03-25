use bytemuck::ReinterpretInner;

fn main() {
  let _: &u16 = (&[0u8; 2]).reinterpret_inner();
  let _: &u32 = (&[0u16; 2]).reinterpret_inner();
  let _: &u64 = (&[0u32; 2]).reinterpret_inner();

  let _: &[u8; 2] = (&0u8).reinterpret_inner();
  let _: &[u16; 2] = (&0u16).reinterpret_inner();
  let _: &[u64; 2] = (&0u64).reinterpret_inner();

  let _: &[u16] = [[0u8; 2]; 2].as_slice().reinterpret_inner();
  let _: &[u32] = [[0u16; 2]; 2].as_slice().reinterpret_inner();
  let _: &[u64] = [[0u32; 2]; 2].as_slice().reinterpret_inner();

  let _: &[()] = [0u8; 1].as_slice().reinterpret_inner();
  let _: &[[u8; 2]] = [[0u8; 3]; 3].as_slice().reinterpret_inner();
  let _: &[[u16; 2]] = [[0u16; 5]; 1].as_slice().reinterpret_inner();
  let _: &[[u32; 2]] = [[0u32; 1]; 1].as_slice().reinterpret_inner();
}
