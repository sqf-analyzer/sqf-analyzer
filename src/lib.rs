pub mod analyzer;
pub mod database;
pub mod parser;
pub mod types;

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use super::*;

    use types::*;

    fn check_infer(case: &str, expected: HashMap<Span<String>, Option<Type>>) {
        let a = analyzer::analyze(parser::parse(case), "tests/dictionary/".into());
        assert_eq!(a, expected);
    }

    #[test]
    fn infer_number() {
        let case = "private _a = 1";

        let expected = HashMap::from([(
            Span {
                inner: "_a".to_string(),
                span: (8, 10),
            },
            Some(Type::Number),
        )]);
        check_infer(case, expected);
    }

    #[test]
    fn infer_string() {
        let case = "private _a = \"1\"";

        let expected = HashMap::from([(
            Span {
                inner: "_a".to_string(),
                span: (8, 10),
            },
            Some(Type::String),
        )]);
        check_infer(case, expected);
    }

    #[test]
    fn infer_variable() {
        let case = "private _a = 1";

        let expected = HashMap::from([(
            Span {
                inner: "_a".to_string(),
                span: (8, 10),
            },
            Some(Type::Number),
        )]);

        check_infer(case, expected);
    }

    #[test]
    fn infer_unary() {
        let case = "private _a = params [\"_a\"]";

        let expected = HashMap::from([(
            Span {
                inner: "_a".to_string(),
                span: (8, 10),
            },
            Some(Type::Boolean),
        )]);

        check_infer(case, expected);
    }

    #[test]
    fn infer_binary() {
        let case = "private _a = 1 + 1";

        let expected = HashMap::from([(
            Span {
                inner: "_a".to_string(),
                span: (8, 10),
            },
            Some(Type::Number),
        )]);

        check_infer(case, expected);
    }

    #[test]
    fn infer_nullary() {
        let case = "private _a = west";

        let expected = HashMap::from([(
            Span {
                inner: "_a".to_string(),
                span: (8, 10),
            },
            Some(Type::Side),
        )]);

        check_infer(case, expected);
    }

    #[test]
    fn infer_example() {
        use std::fs;
        let path = "tests/dictionary/addons/dictionary/";
        let case = fs::read_to_string(format!("{path}/fnc__set.sqf")).unwrap();

        let expected = HashMap::from([
            (
                Span {
                    inner: "_dictionary".to_string(),
                    span: (274, 285),
                },
                None,
            ),
            (
                Span {
                    inner: "_dictionary".to_string(),
                    span: (477, 488),
                },
                None,
            ),
            (
                Span {
                    inner: "_key".to_string(),
                    span: (317, 321),
                },
                None,
            ),
            (
                Span {
                    inner: "_value".to_string(),
                    span: (374, 380),
                },
                None,
            ),
            (
                Span {
                    inner: "_i".to_string(),
                    span: (430, 434),
                },
                Some(Type::Number),
            ),
        ]);

        let r = analyzer::analyze(parser::parse(&case), path.into());
        assert_eq!(r, expected);
    }

    #[test]
    fn infer_example2() {
        use std::fs;
        use std::path::PathBuf;
        let directory: PathBuf = "tests/dictionary/addons/dictionary/".into();

        let paths = fs::read_dir(directory.clone()).unwrap();
        for path in paths {
            let path = path.unwrap().path();
            if path.extension().is_none() {
                continue;
            }
            if !path.extension().unwrap().to_str().unwrap().contains(".sqf") {
                continue;
            }
            let case = fs::read_to_string(directory.clone().join(path)).unwrap();
            analyzer::analyze(parser::parse(&case), directory.clone());
        }
    }
}
