use bytemuck::Reinterpret;

fn main() {
  let _: u16 = 0u8.reinterpret();
  let _: u32 = 0u16.reinterpret();
  let _: u32 = 0u64.reinterpret();
}
