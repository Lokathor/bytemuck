use bytemuck::TryReinterpretInner;
use std::sync::Arc;

fn main() {
  let _: Arc<u16> = Arc::new([0u8; 2]).try_reinterpret_inner().unwrap();
  let _: Arc<u32> = Arc::new([0u16; 2]).try_reinterpret_inner().unwrap();
  let _: Arc<[u32; 2]> = Arc::new(0u64).try_reinterpret_inner().unwrap();

  let _: Arc<[u8; 2]> = Arc::new(0u8).try_reinterpret_inner().unwrap();
  let _: Arc<[u16; 2]> = Arc::new(0u16).try_reinterpret_inner().unwrap();
  let _: Arc<[u64; 2]> = Arc::new(0u64).try_reinterpret_inner().unwrap();

  let _: Arc<[u16]> = Arc::new([[0u8; 2]; 2]).try_reinterpret_inner().unwrap();
  let _: Arc<[u32]> = Arc::new([[0u16; 2]; 2]).try_reinterpret_inner().unwrap();
  let _: Arc<[[u32; 2]]> = Arc::new(0u64).try_reinterpret_inner().unwrap();

  let _: Arc<[()]> = Arc::new([0u8; 1]).try_reinterpret_inner().unwrap();
}
