use bytemuck::TryReinterpret;

fn main() {
  let _: u16 = 0u8.try_reinterpret().unwrap();
  let _: u32 = 0u16.try_reinterpret().unwrap();
  let _: u32 = 0u64.try_reinterpret().unwrap();
}
