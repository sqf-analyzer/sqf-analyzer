use sqf::parser::*;

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
fn error_1() {
    let e = tokens("for \"_i\" do {}").unwrap_err();
    assert_eq!(e.span, (0, 0));
    assert_eq!(e.inner, "expected [define, include, expr]");
}
