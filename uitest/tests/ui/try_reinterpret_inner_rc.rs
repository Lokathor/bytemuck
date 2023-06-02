use bytemuck::TryReinterpretInner;
use std::rc::Rc;

fn main() {
  let _: Rc<u16> = Rc::new([0u8; 2]).try_reinterpret_inner().unwrap();
  let _: Rc<u32> = Rc::new([0u16; 2]).try_reinterpret_inner().unwrap();
  let _: Rc<[u32; 2]> = Rc::new(0u64).try_reinterpret_inner().unwrap();

  let _: Rc<[u8; 2]> = Rc::new(0u8).try_reinterpret_inner().unwrap();
  let _: Rc<[u16; 2]> = Rc::new(0u16).try_reinterpret_inner().unwrap();
  let _: Rc<[u64; 2]> = Rc::new(0u64).try_reinterpret_inner().unwrap();

  let _: Rc<[u16]> = Rc::new([[0u8; 2]; 2]).try_reinterpret_inner().unwrap();
  let _: Rc<[u32]> = Rc::new([[0u16; 2]; 2]).try_reinterpret_inner().unwrap();
  let _: Rc<[[u32; 2]]> = Rc::new(0u64).try_reinterpret_inner().unwrap();

  let _: Rc<[()]> = Rc::new([0u8; 1]).try_reinterpret_inner().unwrap();
}
