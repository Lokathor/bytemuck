use bytemuck::TryReinterpretInner;

fn main() {
  let _: Vec<u16> = vec![[0u8; 2]; 2].try_reinterpret_inner().unwrap();
  let _: Vec<u32> = vec![[0u16; 2]; 2].try_reinterpret_inner().unwrap();
  let _: Vec<[u32; 2]> = vec![0u64; 2].try_reinterpret_inner().unwrap();

  let _: Vec<()> = vec![[0u8; 1]].try_reinterpret_inner().unwrap();
}
