use bytemuck::ReinterpretInner;

fn main() {
  let _: Option<&[u8; 2]> = Some(&0u8).reinterpret_inner();
  let _: Option<&mut [u16; 2]> = Some(&mut 0u16).reinterpret_inner();
  let _: Option<&u64> = Some(&[0u32; 2]).reinterpret_inner();
  let _: Option<&mut [()]> = Some([0u8; 1].as_mut_slice().reinterpret_inner());
  let _: Option<Box<[u8; 4]>> = Some(Box::new([0u16; 2]).reinterpret_inner());
}
