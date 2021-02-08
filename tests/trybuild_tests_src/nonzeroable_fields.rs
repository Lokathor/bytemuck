use bytemuck::Zeroable;

#[derive(Zeroable)]
#[repr(transparent)]
struct NonZeroableField { _bad_field: &'static str }

fn main() {}
