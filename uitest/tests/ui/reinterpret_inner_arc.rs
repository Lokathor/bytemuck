use bytemuck::ReinterpretInner;
use std::sync::Arc;

fn main() {
  let _: Arc<u16> = Arc::new([0u8; 2]).reinterpret_inner();
  let _: Arc<u32> = Arc::new([0u16; 2]).reinterpret_inner();
  let _: Arc<[u32; 2]> = Arc::new(0u64).reinterpret_inner();

  let _: Arc<[u8; 2]> = Arc::new(0u8).reinterpret_inner();
  let _: Arc<[u16; 2]> = Arc::new(0u16).reinterpret_inner();
  let _: Arc<[u64; 2]> = Arc::new(0u64).reinterpret_inner();

  let _: Arc<[u16]> = Arc::new([[0u8; 2]; 2]).reinterpret_inner();
  let _: Arc<[u32]> = Arc::new([[0u16; 2]; 2]).reinterpret_inner();
  let _: Arc<[[u32; 2]]> = Arc::new(0u64).reinterpret_inner();

  let _: Arc<[()]> = Arc::new([0u8; 1]).reinterpret_inner();
  let _: Arc<[[u8; 2]]> = Arc::new([[0u8; 3]; 3]).reinterpret_inner();
  let _: Arc<[[u16; 2]]> = Arc::new([[0u16; 5]; 1]).reinterpret_inner();
  let _: Arc<[[u32; 2]]> = Arc::new([[0u32; 1]; 1]).reinterpret_inner();
}
