use bytemuck::ReinterpretInner;
use core::sync::atomic::AtomicPtr;

fn main() {
  let _: AtomicPtr<u16> = AtomicPtr::new(&mut [0u8; 2]).reinterpret_inner();
  let _: AtomicPtr<u32> = AtomicPtr::new(&mut [0u16; 2]).reinterpret_inner();
  let _: AtomicPtr<u64> = AtomicPtr::new(&mut [0u32; 2]).reinterpret_inner();

  let _: AtomicPtr<[u8; 2]> = AtomicPtr::new(&mut 0u8).reinterpret_inner();
  let _: AtomicPtr<[u16; 2]> = AtomicPtr::new(&mut 0u16).reinterpret_inner();
  let _: AtomicPtr<[u64; 2]> = AtomicPtr::new(&mut 0u64).reinterpret_inner();
}
