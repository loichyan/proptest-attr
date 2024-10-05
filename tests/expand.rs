#[proptest_attr::property_test]
fn test_property_test(s: of!("yyy")) {
    let _ = s;
}

#[proptest_attr::test]
fn test_test(s: of!("yyy")) {
    let _ = s;
}

#[proptest_attr::compose]
fn test_compose(s: of!("yyy")) -> String {
    s
}
