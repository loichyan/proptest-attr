#[proptest_attr::test]
fn my_test_func(s: of!("yyy")) {
    let _ = s;
}
