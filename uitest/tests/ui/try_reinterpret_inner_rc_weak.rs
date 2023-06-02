use bytemuck::TryReinterpretInner;
use std::rc::{Rc, Weak};

fn main() {
  let _: Weak<u16> =
    Rc::downgrade(&Rc::new([0u8; 2])).try_reinterpret_inner().unwrap();
  let _: Weak<u32> =
    Rc::downgrade(&Rc::new([0u16; 2])).try_reinterpret_inner().unwrap();
  let _: Weak<[u32; 2]> =
    Rc::downgrade(&Rc::new(0u64)).try_reinterpret_inner().unwrap();

  let _: Weak<[u8; 2]> =
    Rc::downgrade(&Rc::new(0u8)).try_reinterpret_inner().unwrap();
  let _: Weak<[u16; 2]> =
    Rc::downgrade(&Rc::new(0u16)).try_reinterpret_inner().unwrap();
  let _: Weak<[u64; 2]> =
    Rc::downgrade(&Rc::new(0u64)).try_reinterpret_inner().unwrap();
}
