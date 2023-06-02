use trybuild::TestCases;

#[test]
fn ui() {
  let t = TestCases::new();
  t.compile_fail("tests/ui/*.rs");
  t.pass("src/main.rs");
}
