use bytemuck::TryReinterpretInner;
use core::pin::Pin;

fn main() {
  let _: Pin<&[u8; 2]> = Pin::new(&0u8).try_reinterpret_inner().unwrap();
  let _: Pin<&mut [u16; 2]> =
    Pin::new(&mut 0u16).try_reinterpret_inner().unwrap();
  let _: Pin<&mut [()]> =
    Pin::new([0u8; 1].as_mut_slice().try_reinterpret_inner().unwrap());
  let _: Pin<Box<[u8; 4]>> =
    Pin::new(Box::new([0u16; 2]).try_reinterpret_inner().unwrap());
}
