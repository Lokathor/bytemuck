use bytemuck_derive::{Pod, Zeroable};

#[derive(Copy, Clone, Pod, Zeroable)]
#[repr(C)]
struct Test {
  a: u16,
  b: u16,
}
