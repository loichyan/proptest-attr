#[proptest_attr::property_test(s in "yyy")]
fn test_property_test() {
    let _ = s;
}

#[proptest_attr::test(s in "yyy")]
fn test_test() {
    let _ = s;
}

#[proptest_attr::compose(s in "yyy")]
fn test_compose() -> String {
    s
}
