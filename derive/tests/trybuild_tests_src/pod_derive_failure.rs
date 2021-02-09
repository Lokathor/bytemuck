use bytemuck::Pod;

// pod should not work on types with generics
#[derive(Pod)]
#[repr(transparent)]
struct Generic<T>(T);


// should fail because it's not transparent or Repr(c)
#[derive(Zeroable, Pod)]
struct Bad(u16);

// Pod should not be derived by types that need padding
#[derive(Zeroable, Pod)]
#[repr(C)]
struct Padding {
    _byte_one: u8,
    _byte_two: u8,
    _int: u32,
    // this should induce 2 bytes of padding
}

fn main() {}
