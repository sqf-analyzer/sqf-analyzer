use sqf::{error::Error, parser1::*};

#[test]
fn tokens() {
    use std::fs;
    let path = "tests/integration/examples/basic_parenthesis.sqf";
    let case = fs::read_to_string(path).unwrap();

    let a = sqf::preprocessor::parse(sqf::preprocessor::pairs(&case).unwrap());
    let iter = sqf::preprocessor::AstIterator::new(a, Default::default());

    let (result, errors) = parse(iter);
    assert!(errors.is_empty());
    println!("{:#?}", result);
    let expected = r#"[
    1,
    {},
    (1 + 1),
    {},
    ((if a) then ({1;2;} else {1;2;})),
    ((if a) then ({1;} else {2;})),
    ((private a) = 1),
    (a = []),
    (a = [1,2,]),
    ((a + 1) * 2),
]"#;
    assert_eq!(format!("{:#?}", result), expected);
}

#[test]
fn errors() {
    let case = vec![
        (
            "[",
            vec![Error {
                inner: "\"[\" is not closed".to_string(),
                span: (0, 1),
            }],
        ),
        (
            "{",
            vec![Error {
                inner: "\"{\" is not closed".to_string(),
                span: (0, 1),
            }],
        ),
        (
            "(",
            vec![
                Error {
                    inner: "Un-expected end of file".to_string(),
                    span: (0, 0),
                },
                Error {
                    inner: "\"(\" is not closed".to_string(),
                    span: (0, 1),
                },
            ],
        ),
    ];

    for (case, expected) in case {
        let a = sqf::preprocessor::parse(sqf::preprocessor::pairs(case).unwrap());
        let iter = sqf::preprocessor::AstIterator::new(a, Default::default());

        let (_, errors) = parse(iter);
        assert_eq!(errors, expected);
    }
}
