use bytemuck::ReinterpretInner;
use std::sync::{Arc, Weak};

fn main() {
  let _: Weak<u16> = Arc::downgrade(&Arc::new([0u8; 2])).reinterpret_inner();
  let _: Weak<u32> = Arc::downgrade(&Arc::new([0u16; 2])).reinterpret_inner();
  let _: Weak<[u32; 2]> = Arc::downgrade(&Arc::new(0u64)).reinterpret_inner();

  let _: Weak<[u8; 2]> = Arc::downgrade(&Arc::new(0u8)).reinterpret_inner();
  let _: Weak<[u16; 2]> = Arc::downgrade(&Arc::new(0u16)).reinterpret_inner();
  let _: Weak<[u64; 2]> = Arc::downgrade(&Arc::new(0u64)).reinterpret_inner();
}
