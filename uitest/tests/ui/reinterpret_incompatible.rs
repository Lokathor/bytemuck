use bytemuck::{
  Reinterpret, ReinterpretInner, TryReinterpret, TryReinterpretInner,
};
use core::num::NonZeroU32;

#[repr(C)]
struct Padded(u16, u8);

fn main() {
  let _: u32 = Padded(0, 0).reinterpret();
  let _: NonZeroU32 = 0u32.reinterpret();
  let _: u32 = Padded(0, 0).try_reinterpret().unwrap();
  let _: Padded = 0u32.try_reinterpret().unwrap();

  // let _: &u32 = (&Padded(0, 0)).reinterpret_inner();
  // let _: &NonZeroU32 = (&0u32).reinterpret_inner();
  // let _: &u32 = (&Padded(0, 0)).try_reinterpret_inner().unwrap();
  // let _: &Padded = (&0u32).try_reinterpret_inner().unwrap();

  // let _: &mut u32 = (&mut Padded(0, 0)).reinterpret_inner();
  // let _: &mut NonZeroU32 = (&mut 0u32).reinterpret_inner();
  // let _: &mut u32 = (&mut Padded(0, 0)).try_reinterpret_inner().unwrap();
  // let _: &mut Padded = (&mut 0u32).try_reinterpret_inner().unwrap();
}
