use bytemuck::TransparentWrapper;

#[derive(TransparentWrapper)]
#[transparent(u8)]
struct NotReprTransparent(u8);

#[derive(TransparentWrapper)]
#[transparent(u8)]
#[repr(transparent)]
struct DifferentWrappedType(u32);

fn main() {}
