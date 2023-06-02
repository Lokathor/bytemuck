use bytemuck::ReinterpretInner;

fn main() {
  let _: Box<u16> = Box::new([0u8; 2]).reinterpret_inner();
  let _: Box<u32> = Box::new([0u16; 2]).reinterpret_inner();
  let _: Box<[u32; 2]> = Box::new(0u64).reinterpret_inner();

  let _: Box<[u8; 2]> = Box::new(0u8).reinterpret_inner();
  let _: Box<[u16; 2]> = Box::new(0u16).reinterpret_inner();
  let _: Box<[u64; 2]> = Box::new(0u64).reinterpret_inner();

  let _: Box<[u16]> = Box::new([[0u8; 2]; 2]).reinterpret_inner();
  let _: Box<[u32]> = Box::new([[0u16; 2]; 2]).reinterpret_inner();
  let _: Box<[[u32; 2]]> = Box::new(0u64).reinterpret_inner();

  let _: Box<[()]> = Box::new([0u8; 1]).reinterpret_inner();
  let _: Box<[[u8; 2]]> = Box::new([[0u8; 3]; 3]).reinterpret_inner();
  let _: Box<[[u16; 2]]> = Box::new([[0u16; 5]; 1]).reinterpret_inner();
  let _: Box<[[u32; 2]]> = Box::new([[0u32; 1]; 1]).reinterpret_inner();
}
