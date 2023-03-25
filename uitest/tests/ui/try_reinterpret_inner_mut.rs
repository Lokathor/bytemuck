use bytemuck::TryReinterpretInner;

fn main() {
  let _: &mut [u8; 2] = (&mut 0u8).try_reinterpret_inner().unwrap();
  let _: &mut [u16; 2] = (&mut 0u16).try_reinterpret_inner().unwrap();
  let _: &mut [u64; 2] = (&mut 0u64).try_reinterpret_inner().unwrap();

  let _: &mut [()] = [0u8; 1].as_mut_slice().try_reinterpret_inner().unwrap();
}
