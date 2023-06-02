use bytemuck::ReinterpretInner;
use core::pin::Pin;

fn main() {
  let _: Pin<&[u8; 2]> = Pin::new(&0u8).reinterpret_inner();
  let _: Pin<&mut [u16; 2]> = Pin::new(&mut 0u16).reinterpret_inner();
  let _: Pin<&u64> = Pin::new(&[0u32; 2]).reinterpret_inner();
  let _: Pin<&mut [()]> = Pin::new([0u8; 1].as_mut_slice().reinterpret_inner());
  let _: Pin<Box<[u8; 4]>> = Pin::new(Box::new([0u16; 2]).reinterpret_inner());
}
