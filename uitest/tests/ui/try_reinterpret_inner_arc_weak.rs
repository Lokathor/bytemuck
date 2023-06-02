use bytemuck::TryReinterpretInner;
use std::sync::{Arc, Weak};

fn main() {
  let _: Weak<u16> =
    Arc::downgrade(&Arc::new([0u8; 2])).try_reinterpret_inner().unwrap();
  let _: Weak<u32> =
    Arc::downgrade(&Arc::new([0u16; 2])).try_reinterpret_inner().unwrap();
  let _: Weak<[u32; 2]> =
    Arc::downgrade(&Arc::new(0u64)).try_reinterpret_inner().unwrap();

  let _: Weak<[u8; 2]> =
    Arc::downgrade(&Arc::new(0u8)).try_reinterpret_inner().unwrap();
  let _: Weak<[u16; 2]> =
    Arc::downgrade(&Arc::new(0u16)).try_reinterpret_inner().unwrap();
  let _: Weak<[u64; 2]> =
    Arc::downgrade(&Arc::new(0u64)).try_reinterpret_inner().unwrap();
}
