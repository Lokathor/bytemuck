use bytemuck::ReinterpretInner;
use std::rc::Rc;

fn main() {
  let _: Rc<u16> = Rc::new([0u8; 2]).reinterpret_inner();
  let _: Rc<u32> = Rc::new([0u16; 2]).reinterpret_inner();
  let _: Rc<[u32; 2]> = Rc::new(0u64).reinterpret_inner();

  let _: Rc<[u8; 2]> = Rc::new(0u8).reinterpret_inner();
  let _: Rc<[u16; 2]> = Rc::new(0u16).reinterpret_inner();
  let _: Rc<[u64; 2]> = Rc::new(0u64).reinterpret_inner();

  let _: Rc<[u16]> = Rc::new([[0u8; 2]; 2]).reinterpret_inner();
  let _: Rc<[u32]> = Rc::new([[0u16; 2]; 2]).reinterpret_inner();
  let _: Rc<[[u32; 2]]> = Rc::new(0u64).reinterpret_inner();

  let _: Rc<[()]> = Rc::new([0u8; 1]).reinterpret_inner();
  let _: Rc<[[u8; 2]]> = Rc::new([[0u8; 3]; 3]).reinterpret_inner();
  let _: Rc<[[u16; 2]]> = Rc::new([[0u16; 5]; 1]).reinterpret_inner();
  let _: Rc<[[u32; 2]]> = Rc::new([[0u32; 1]; 1]).reinterpret_inner();
}
