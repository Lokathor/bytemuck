#![cfg(feature = "derive")]

use bytemuck::{Zeroable, Pod};

#[derive(Copy, Clone, Pod, Zeroable)]
#[repr(C)]
struct Test {
  a: u16,
  b: u16,
}