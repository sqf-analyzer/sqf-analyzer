use sqf::{error::Error, parser::parse, preprocessor::tokens, span::Spanned};

fn check_parse(cases: &[&str]) {
    for case in cases {
        let pairs = tokens(case, Default::default(), Default::default());

        match pairs {
            Ok(r) => {
                println!("{case}");
                let (_, e) = parse(r);
                println!("{e:?}");
                assert!(e.is_empty());
            }
            Err(r) => {
                println!("{r:?}");
                panic!();
            }
        }
    }
}

#[test]
fn general() {
    let case = r#"
// Sets the value of the key of the dictionary. Use multiple keys for nested operation.
params ["_arguments", "_isGlobal"];

if (count _arguments < 3) exitWith {
    diag_log format ["DICT:set(%1):ERROR: requires 3 arguments", _arguments];
};

private _dictionary = _arguments select 0;
private _key = _arguments select (count _arguments - 2);
private _value = _arguments select (count _arguments - 1);

for "_i" from 1 to (count _arguments - 3) do {
    _dictionary = [_dictionary, _arguments select _i] call DICT_fnc_get;
    if isNil "_dictionary" exitWith {}; // the error was already emited by `get`, just quit
};
if not typeName _dictionary == "OBJECT" exitWith {
    diag_log format ["DICT:set(%1):ERROR: not an object.", _arguments];
};
_dictionary setVariable [toLower _key, _value, _isGlobal];

"#;
    parse(tokens(case, Default::default(), Default::default()).unwrap());
}

#[test]
fn parse_for() {
    parse(
        tokens(
            "for \"_i\" from 1 to (count _arguments - 3) do {}",
            Default::default(),
            Default::default(),
        )
        .unwrap(),
    );
}

#[test]
fn parse_macros() {
    parse(
        tokens(
            "#include \"macros.hpp\"\na = AA(a)",
            Default::default(),
            Default::default(),
        )
        .unwrap(),
    );
}

#[test]
fn parse_macros_call() {
    parse(tokens("a = (call AA(a))", Default::default(), Default::default()).unwrap());
}

#[test]
fn if_after_semi_colon() {
    assert_eq!(
        parse(tokens("; if !(isServer)", Default::default(), Default::default(),).unwrap(),).1,
        vec![]
    );
}

#[test]
fn semi_colon() {
    assert_eq!(
        parse(tokens("1;;;1", Default::default(), Default::default(),).unwrap(),).1,
        vec![]
    );
}

#[test]
fn two_signatures() {
    parse(tokens("call A; [] call A;", Default::default(), Default::default()).unwrap());
}

#[test]
fn assignment() {
    let cases = [
        "_dictionary = 1",
        "private _dictionary = 1",
        "private _dictionary = _arguments select 0",
        "private _key = _arguments select (count _arguments - 2)",
    ];

    check_parse(&cases);
}

#[test]
fn expr() {
    let cases = [
        "1",
        "(1)",
        // binary
        "1 + 1",
        "a + 1",
        "a + a",
        "1 + 1",
        "(1 + 1)",
        "a select 1",
        "1 select a",
        "1 select 1",
        "a select a",
        "(a select a)",
        "params []",
        "(params [])",
        "if _a",
        "if (_a > 1)",
        "params 1",
        "_dictionary setVariable [_key, _value, _isGlobal]",
        "not a && b",
        "-1",
        "private _key = _arguments select (count _arguments - 2);",
        "private _dict2 = 1;",               // variable with digit
        "private _results = +DICT_results;", // + as unary
    ];

    check_parse(&cases);
}

#[test]
fn expr_negative() {
    let cases = ["ormat [\"\", _arguments];", "_a cal [\"\", _arguments];"];
    let expected = [
        vec![
            Spanned {
                span: (6, 7),
                inner: "\"[\" is not a valid binary operator".to_string(),
            },
            Spanned {
                span: (11, 21),
                inner: "\"_arguments\" is not a valid binary operator".to_string(),
            },
        ],
        vec![Spanned {
            span: (3, 6),
            inner: "\"cal\" is not a valid binary operator".to_string(),
        }],
    ];

    for (case, expected) in cases.iter().zip(expected.iter()) {
        let r = tokens(case, Default::default(), Default::default());
        match r {
            Ok(r) => {
                println!("{case}");
                println!("{:#?}", r.clone().collect::<Vec<_>>());
                let (_, e) = parse(r);
                assert_eq!(&e, expected);
            }
            Err(r) => {
                println!("{r:?}");
                panic!();
            }
        }
    }
}

#[test]
fn f() {
    let e = r#"if _a then {
diag_log format ["a", _arguments];
};"#;
    let (_, error) = parse(tokens(e, Default::default(), Default::default()).unwrap());
    assert_eq!(error, vec![]);
}

#[test]
fn for_() {
    let case = "for \"_i\" from 1 to 10 do { 1+1; }";
    let (_, error) = parse(tokens(case, Default::default(), Default::default()).unwrap());
    assert_eq!(error, vec![]);
}

#[test]
fn macros_() {
    let case = "if not ISOBJECT(_dictionary) exitWith {}";
    parse(tokens(case, Default::default(), Default::default()).unwrap());
}

#[test]
fn no_panic() {
    let case = "params";
    parse(tokens(case, Default::default(), Default::default()).unwrap());
}

#[test]
fn parenthesis() {
    use std::fs;
    let path = "tests/integration/examples/basic_parenthesis.sqf";
    let case = fs::read_to_string(path).unwrap();

    let ast = tokens(&case, Default::default(), Default::default()).unwrap();

    let (_, errors) = parse(ast);
    assert_eq!(errors, vec![]);
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
        (
            "private a = \"_a",
            vec![Error {
                inner: "\"_a\" is not a valid binary operator".to_string(),
                span: (13, 15),
            }],
        ),
        (
            "ormat [\"\", _arguments]",
            vec![
                Error {
                    inner: "\"[\" is not a valid binary operator".to_string(),
                    span: (6, 7),
                },
                Error {
                    inner: "\"_arguments\" is not a valid binary operator".to_string(),
                    span: (11, 21),
                },
            ],
        ),
    ];

    for (case, expected) in case {
        let iter = tokens(case, Default::default(), Default::default()).unwrap();

        let (r, errors) = parse(iter);
        println!("{r:#?}");
        assert_eq!(errors, expected);
    }
}
