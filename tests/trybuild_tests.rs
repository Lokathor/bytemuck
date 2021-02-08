#[test]
#[cfg(feature = "derive")]
fn derive_tests() {
    let tester = trybuild::TestCases::new();
    tester.pass("tests/trybuild_tests_src/derive.rs");
    tester.compile_fail("tests/trybuild_tests_src/pod_derive_failure.rs");
    tester.compile_fail("tests/trybuild_tests_src/transparentwrapper_failure.rs");
    tester.compile_fail("tests/trybuild_tests_src/nonzeroable_fields.rs");
    tester.compile_fail("tests/trybuild_tests_src/contiguous_derive_failure.rs");
}
