use bytemuck::TryReinterpretInner;
use core::sync::atomic::AtomicPtr;

fn main() {
  let _: AtomicPtr<[u8; 2]> =
    AtomicPtr::new(&mut 0u8).try_reinterpret_inner().unwrap();
  let _: AtomicPtr<[u16; 2]> =
    AtomicPtr::new(&mut 0u16).try_reinterpret_inner().unwrap();
  let _: AtomicPtr<[u64; 2]> =
    AtomicPtr::new(&mut 0u64).try_reinterpret_inner().unwrap();
}
