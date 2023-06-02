use bytemuck::TryReinterpretInner;

fn main() {
  let _: &[u8; 2] = (&0u8).try_reinterpret_inner().unwrap();
  let _: &[u16; 2] = (&0u16).try_reinterpret_inner().unwrap();
  let _: &[u64; 2] = (&0u64).try_reinterpret_inner().unwrap();
  let _: &[()] = [0u8; 1].as_slice().try_reinterpret_inner().unwrap();
}
