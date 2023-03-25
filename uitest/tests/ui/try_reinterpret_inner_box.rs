use bytemuck::TryReinterpretInner;

fn main() {
  let _: Box<u16> = Box::new([0u8; 2]).try_reinterpret_inner().unwrap();
  let _: Box<u32> = Box::new([0u16; 2]).try_reinterpret_inner().unwrap();
  let _: Box<[u32; 2]> = Box::new(0u64).try_reinterpret_inner().unwrap();

  let _: Box<[u8; 2]> = Box::new(0u8).try_reinterpret_inner().unwrap();
  let _: Box<[u16; 2]> = Box::new(0u16).try_reinterpret_inner().unwrap();
  let _: Box<[u64; 2]> = Box::new(0u64).try_reinterpret_inner().unwrap();

  let _: Box<[u16]> = Box::new([[0u8; 2]; 2]).try_reinterpret_inner().unwrap();
  let _: Box<[u32]> = Box::new([[0u16; 2]; 2]).try_reinterpret_inner().unwrap();
  let _: Box<[[u32; 2]]> = Box::new(0u64).try_reinterpret_inner().unwrap();

  let _: Box<[()]> = Box::new([0u8; 1]).try_reinterpret_inner().unwrap();
}
