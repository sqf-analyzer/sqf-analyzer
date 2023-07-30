pub use pest::iterators::{Pair, Pairs};
use pest::pratt_parser::PrattParser;
use pest::Parser;
use pest_derive::Parser;

use crate::error::Error;
use crate::types::*;

#[derive(Parser)]
#[grammar = "sqf.pest"]
pub struct SQFParser;

lazy_static::lazy_static! {
    static ref PRATT_PARSER: PrattParser<Rule> = {
        use pest::pratt_parser::{Assoc::*, Op};
        use Rule::*;

        // https://foxhound.international/precedence-arma-3-sqf.html
        PrattParser::new()
            .op(Op::infix(or, Left))
            .op(Op::infix(and, Left))
            .op(Op::infix(logical_operator, Left))
            .op(Op::infix(binary_operator, Left))
            .op(Op::infix(else_, Left))
            .op(Op::infix(add, Left) | Op::infix(subtract, Left))
            .op(Op::infix(min_max, Left))
            .op(Op::infix(multiply, Left) | Op::infix(divide, Left) | Op::infix(modulos, Left) | Op::infix(mod_, Left))
            .op(Op::infix(exp, Left))
            .op(Op::prefix(unary_operator))
    };
}

fn to_span(pair: &Pair<'_, Rule>) -> (usize, usize) {
    (pair.as_span().start(), pair.as_span().end())
}

impl From<Pair<'_, Rule>> for Span<String> {
    fn from(value: Pair<'_, Rule>) -> Self {
        Self {
            span: to_span(&value),
            inner: value.as_str().to_string(),
        }
    }
}

fn parse_assignment(pairs: Pairs<Rule>, pratt: &PrattParser<Rule>) -> Expr {
    let mut a = pairs.collect::<Vec<_>>();
    let expr = a.pop().expect("by pest definition");
    let variable = a.pop().expect("by pest definition");
    let is_private = !a.is_empty();
    Expr::Assignment {
        is_private,
        variable: variable.into(),
        expr: Box::new(parse_expr(expr, pratt)),
    }
}

fn parse_for(pairs: Pairs<Rule>, pratt: &PrattParser<Rule>) -> Expr {
    let mut a = pairs.collect::<Vec<_>>();
    let do_;
    let step;
    let to;
    let from;
    let variable;
    if a.len() == 5 {
        // variable, from, to, step, do
        do_ = a.pop().expect("by definition");
        step = Some(a.pop().expect("by definition"));
        to = a.pop().expect("by definition");
        from = a.pop().expect("by definition");
        variable = a.pop().expect("by definition");
    } else {
        // variable, from, to, do
        do_ = a.pop().expect("by definition");
        to = a.pop().expect("by definition");
        from = a.pop().expect("by definition");
        variable = a.pop().expect("by definition");
        step = None;
    }

    let do_ = do_
        .into_inner()
        .map(|pair| parse_expr(pair, pratt))
        .collect::<Vec<_>>();

    let variable = quoted_span(variable);

    Expr::For {
        variable,
        from: Box::new(parse_expr(from, pratt)),
        to: Box::new(parse_expr(to, pratt)),
        step: step.map(|step| Box::new(parse_expr(step, pratt))),
        do_,
    }
}

fn parse_macro(pairs: Pairs<Rule>) -> Expr {
    let mut a = pairs.collect::<Vec<_>>();
    let content = a.pop().expect("by pest definition");
    let name = a.pop().expect("by pest definition");
    Expr::Macro(name.into(), content.into())
}

fn no_quotes(v: &str) -> &str {
    &v[1..v.len() - 1]
}

fn quoted_span(pair: Pair<'_, Rule>) -> Span<String> {
    Span {
        inner: no_quotes(pair.as_str()).to_string(),
        span: to_span(&pair),
    }
}

pub fn parse_expr(pair: Pair<Rule>, pratt: &PrattParser<Rule>) -> Span<Expr> {
    pratt
        .map_primary(|primary| match primary.as_rule() {
            Rule::number => Span {
                inner: Expr::Value(Value::Number(primary.as_str().to_string())),
                span: to_span(&primary),
            },
            Rule::string => quoted_span(primary).map(Value::String).map(Expr::Value),
            Rule::boolean => Span {
                inner: Expr::Value(Value::Boolean(primary.as_str().to_string())),
                span: to_span(&primary),
            },
            Rule::array => {
                let span = to_span(&primary);
                let inner = primary
                    .into_inner()
                    .map(|pair| parse_expr(pair, pratt))
                    .collect::<Vec<_>>();
                Span {
                    inner: Expr::Value(Value::Array(inner)),
                    span,
                }
            }
            Rule::expr => parse_expr(primary, pratt),
            Rule::variable => Span {
                span: to_span(&primary),
                inner: Expr::Variable(primary.into()),
            },
            Rule::assignment => Span {
                span: to_span(&primary),
                inner: parse_assignment(primary.into_inner(), pratt),
            },
            Rule::code => {
                let span = to_span(&primary);
                let inner = primary
                    .into_inner()
                    .map(|pair| parse_expr(pair, pratt))
                    .collect::<Vec<_>>();
                Span {
                    inner: Expr::Value(Value::Code(inner)),
                    span,
                }
            }
            Rule::for_ => Span {
                span: to_span(&primary),
                inner: parse_for(primary.into_inner(), pratt),
            },
            Rule::macro_ => Span {
                span: to_span(&primary),
                inner: parse_macro(primary.into_inner()),
            },
            rule => unreachable!("Expr::parse expected atom, found {:?}", rule),
        })
        .map_prefix(|op: Pair<'_, Rule>, rhs| {
            let span = (to_span(&op).0, rhs.span.1);

            Span {
                inner: Expr::UnaryOp {
                    op: op.as_str().to_string(),
                    rhs: Box::new(rhs),
                },
                span,
            }
        })
        .map_infix(|lhs: Span<Expr>, op, rhs| {
            let span = (lhs.span.0, rhs.span.1);
            Span {
                inner: Expr::BinaryOp {
                    lhs: Box::new(lhs),
                    op: op.as_str().to_string(),
                    rhs: Box::new(rhs),
                },
                span,
            }
        })
        .parse(pair.into_inner())
}

pub fn tokens(data: &str) -> Result<Pairs<'_, Rule>, Error> {
    Ok(SQFParser::parse(Rule::program, data)?)
}

fn to_span_expr(pair: Pair<Rule>) -> Span<Expr> {
    match pair.as_rule() {
        Rule::include => {
            let pair = pair.into_inner().next().expect("by pest definition");

            Span {
                span: to_span(&pair),
                inner: Expr::Include(quoted_span(pair)),
            }
        }
        Rule::define => {
            let mut pairs = pair.into_inner();
            let name: Pair<'_, Rule> = pairs.next().expect("by pest definition");

            let mut arguments = vec![];
            let mut body = None;

            for pair in pairs {
                match pair.as_rule() {
                    Rule::define_argument => arguments.push(pair.as_str().to_string()),
                    Rule::define_body => body = Some(pair.as_str().to_string()),
                    _ => unreachable!(),
                }
            }

            Span {
                span: to_span(&name),
                inner: Expr::Define(Define {
                    name: name.as_str().to_string(),
                    arguments,
                    body,
                }),
            }
        }
        _ => parse_expr(pair, &PRATT_PARSER),
    }
}

pub fn parse<'a, I: Iterator<Item = Pair<'a, Rule>>>(iter: I) -> Vec<Span<Expr>> {
    iter.filter(|pair| !pair.as_str().is_empty())
        .map(to_span_expr)
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn string_positive() {
        let cases = ["\"a\"", "\"\"a\"\"", "\"\"ba\"aa\"\""];

        for case in cases {
            assert!(SQFParser::parse(Rule::string, case).is_ok());
        }
    }

    #[test]
    fn variable() {
        let cases = ["_a", "_args", "aa", "_arguments"];

        for case in cases {
            let r = SQFParser::parse(Rule::variable, case);
            if let Err(r) = r {
                println!("{r:?}");
                panic!();
            }
        }
    }

    #[test]
    fn array() {
        let cases = ["[1, 2]"];

        for case in cases {
            let r = SQFParser::parse(Rule::array, case);
            if let Err(r) = r {
                println!("{r:?}");
                panic!();
            }
        }
    }

    #[test]
    fn assignment() {
        let cases = [
            "private _dictionary = _arguments select 0",
            "private _key = _arguments select (count _arguments - 2)",
        ];

        for case in cases {
            let r = SQFParser::parse(Rule::assignment, case);
            if let Err(r) = r {
                println!("{r:?}");
                panic!();
            }
        }
    }

    #[test]
    fn expr() {
        let cases = [
            "1 + 1",
            "a select 1",
            "1 select 1",
            "_arguments select 0",
            "params [\"a\", \"b\"]",
            "diag_log format [\"DICT:set(%1):ERROR: requires 3 arguments\", _arguments]",
            "if _a",
            "if (_a > 1)",
            "1",
            "params 1",
            "(1)",
            "(params 1)",
            "(1 + 1)",
            "_dictionary setVariable [_key, _value, _isGlobal];",
            "not a && b",
        ];

        for case in cases {
            let r = SQFParser::parse(Rule::expr, case);
            if let Err(r) = r {
                println!("{r:?}");
                panic!();
            }
        }
    }

    #[test]
    fn if_() {
        let cases = ["if _a", "if (_a > 1)"];

        for case in cases {
            let r = SQFParser::parse(Rule::unary_operator, case);
            if let Err(r) = r {
                println!("{r:?}");
                panic!();
            }
        }
    }

    #[test]
    fn f() {
        let e = r#"if _a then {
    diag_log format ["a", _arguments];
};"#;
        SQFParser::parse(Rule::expr, e).unwrap();
    }

    #[test]
    fn for_() {
        let case = "for \"_i\" from 1 to 10 do { 1+1; }";
        SQFParser::parse(Rule::for_, case).unwrap();
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
        SQFParser::parse(Rule::program, case).unwrap();
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
    fn code_assign() {
        parse(tokens("a = {}").unwrap());
    }

    #[test]
    fn error_1() {
        let e = tokens("for \"_i\" do {}").unwrap_err();
        assert_eq!(e.span, (0, 0));
        assert_eq!(e.inner, "expected [define, include, expr]");
    }
}
