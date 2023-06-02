use bytemuck::TryReinterpretInner;

fn main() {
  let _: *const [u8; 2] = (&0u8 as *const u8).try_reinterpret_inner().unwrap();
  let _: *const [u16; 2] =
    (&0u16 as *const u16).try_reinterpret_inner().unwrap();
  let _: *const [u64; 2] =
    (&0u64 as *const u64).try_reinterpret_inner().unwrap();
}
