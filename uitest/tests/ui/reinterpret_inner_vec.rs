use bytemuck::ReinterpretInner;

fn main() {
  let _: Vec<u16> = vec![[0u8; 2]; 2].reinterpret_inner();
  let _: Vec<u32> = vec![[0u16; 2]; 2].reinterpret_inner();
  let _: Vec<[u32; 2]> = vec![0u64].reinterpret_inner();

  let _: Vec<()> = vec![[0u8; 1]].reinterpret_inner();
  let _: Vec<[u8; 2]> = vec![0u8].reinterpret_inner();
  let _: Vec<[u16; 2]> = vec![0u16].reinterpret_inner();
  let _: Vec<[u64; 2]> = vec![0u64].reinterpret_inner();
}
