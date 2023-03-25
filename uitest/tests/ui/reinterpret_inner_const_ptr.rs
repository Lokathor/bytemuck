use bytemuck::ReinterpretInner;

fn main() {
  let _: *const u16 = (&[0u8; 2] as *const [u8; 2]).reinterpret_inner();
  let _: *const u32 = (&[0u16; 2] as *const [u16; 2]).reinterpret_inner();
  let _: *const u64 = (&[0u32; 2] as *const [u32; 2]).reinterpret_inner();

  let _: *const [u8; 2] = (&0u8 as *const u8).reinterpret_inner();
  let _: *const [u16; 2] = (&0u16 as *const u16).reinterpret_inner();
  let _: *const [u64; 2] = (&0u64 as *const u64).reinterpret_inner();
}
