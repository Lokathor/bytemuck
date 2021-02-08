use bytemuck::Contiguous;

#[derive(Contiguous, Copy, Clone)]
#[repr(u8)]
enum Fielded {
    A(u32),
}

#[derive(Contiguous, Copy, Clone)]
#[repr(u8)]
enum NonContiguous {
    A = 99,
    B = 300,
    C = 399,
}

fn main() {}
