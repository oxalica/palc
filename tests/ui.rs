#[test]
#[ignore = "unstable across compiler versions"]
fn ui() {
    let t = trybuild::TestCases::new();
    t.compile_fail("tests/ui/*.rs");
}
