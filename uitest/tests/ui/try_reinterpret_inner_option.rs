use bytemuck::TryReinterpretInner;

fn main() {
  let _: Option<&[u8; 2]> = Some(&0u8).try_reinterpret_inner().unwrap();
  let _: Option<&mut [u16; 2]> =
    Some(&mut 0u16).try_reinterpret_inner().unwrap();
  let _: Option<&mut [()]> =
    Some([0u8; 1].as_mut_slice().try_reinterpret_inner().unwrap());
  let _: Option<Box<[u8; 4]>> =
    Some(Box::new([0u16; 2]).try_reinterpret_inner().unwrap());
}
