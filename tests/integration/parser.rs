use sqf::{parser::*, types::Spanned};

fn check_parse(cases: &[&str]) {
    for case in cases {
        let r = tokens(case);
        match r {
            Ok(r) => {
                println!("{case}");
                let (_, e) = parse(r);
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
    tokens(case).unwrap();
}

#[test]
fn parse_for() {
    parse(tokens("for \"_i\" from 1 to (count _arguments - 3) do {}").unwrap());
}

#[test]
fn parse_macros() {
    parse(tokens("#include \"macros.hpp\"\na = AA(a)").unwrap());
}

#[test]
fn parse_macros_call() {
    parse(tokens("a = (call AA(a))").unwrap());
}

#[test]
fn two_signatures() {
    parse(tokens("call A; [] call A;").unwrap());
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
        vec![Spanned {
            span: (6, 22),
            inner: "\"[\"\", _arguments]\" is not an operator".to_string(),
        }],
        vec![Spanned {
            span: (3, 6),
            inner: "\"cal\" is not an operator".to_string(),
        }],
    ];

    for (case, expected) in cases.iter().zip(expected.iter()) {
        let r = tokens(case);
        match r {
            Ok(r) => {
                println!("{case}");
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
    tokens(e).unwrap();
}

#[test]
fn for_() {
    let case = "for \"_i\" from 1 to 10 do { 1+1; }";
    tokens(case).unwrap();
}

#[test]
fn macros_() {
    let case = "if not ISOBJECT(_dictionary) exitWith {}";
    parse(tokens(case).unwrap());
}
