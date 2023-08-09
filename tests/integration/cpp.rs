use std::collections::HashMap;

use sqf::cpp::{analyze, analyze_addon};
use sqf::preprocessor::tokens;
use sqf::span::Spanned;

#[test]
fn basic() {
    // https://community.bistudio.com/wiki/Arma_3:_Functions_Library
    let case = r#"
class CfgFunctions {};
"#;
    let iter = tokens(case, Default::default(), Default::default()).unwrap();
    let (functions, errors) = analyze(iter);
    assert!(functions.is_empty());
    assert_eq!(errors, vec![]);
}

#[test]
fn general() {
    // https://community.bistudio.com/wiki/Arma_3:_Functions_Library
    let case = r#"
class CfgFunctions
{
    class TAG
    {
        class Category
        {
            class myFunction {};
        };

        class OtherCategory
        {
            file = "My\Category\Path";
            class myFunction1 {}; // file path will be <ROOT>\My\Category\Path\fn_myFunction.sqf";
        };

        class DataCategory
        {
            requiredAddons[] = { "A3_Data_F" }; // Optional requirements of CfgPatches classes. If some addons are missing, category functions will not be compiled.
            class myDataFunction {
                file = "My\Function\Filepath.sqf"; // file path will be <ROOT>\My\Function\Filepath.sqf", ignoring "Path\To\Category"
            };
        };
    };

    class other
    {
        tag = "SOME"
        class Category
        {
            class myFunction {};
        };
    };
};
"#;
    let iter = tokens(case, Default::default(), Default::default()).unwrap();
    let (functions, errors) = analyze(iter);
    assert_eq!(errors, vec![]);

    assert_eq!(
        functions,
        HashMap::from([
            (
                "TAG_fnc_myFunction".to_string().into(),
                Spanned::new("Category\\fn_myFunction.sqf".to_string(), (93, 103)),
            ),
            (
                "TAG_fnc_myFunction1".to_string().into(),
                Spanned::new(
                    "My\\Category\\Path\\fn_myFunction1.sqf".to_string(),
                    (215, 226)
                ),
            ),
            (
                "TAG_fnc_myDataFunction".to_string().into(),
                Spanned::new("My\\Function\\Filepath.sqf".to_string(), (528, 542)),
            ),
            (
                "SOME_fnc_myFunction".to_string().into(),
                Spanned::new("Category\\fn_myFunction.sqf".to_string(), (807, 817)),
            ),
        ])
    );
}

#[test]
fn general_1() {
    // https://community.bistudio.com/wiki/Arma_3:_Functions_Library
    let case = r#"
class CfgFunctions
{
    class TAG
    {
        class Category1
        {
            class myFunction {};
        };

        class Category2
        {
            file = "Path\To\Category";
            class myFunction
            {
                file = "My\Function\Filepath.sqf"; // file path will be <ROOT>\My\Function\Filepath.sqf", ignoring "Path\To\Category"
            };

            class myFSMFunction
            {
                preInit		= 1;
                postInit	= 1;
                ext			= ".fsm";
                preStart	= 1;
                recompile	= 1;
                a = {1};
            };
        };
    };
};
"#;
    let iter = tokens(case, Default::default(), Default::default()).unwrap();
    let (_, errors) = analyze(iter);
    assert_eq!(errors, vec![]);
}

#[test]
fn infer_addon() {
    use std::fs;
    let path = "tests/integration/dictionary/addons/dictionary/config.cpp";
    let case = fs::read_to_string(path).unwrap();

    let iter = tokens(&case, Default::default(), Default::default()).unwrap();
    let (_, errors) = analyze(iter);
    assert_eq!(errors, vec![]);
}

#[test]
fn addon_basic() {
    let directory = "tests/integration/dictionary/addons/dictionary";

    let (functions, errors) = analyze_addon(directory.into()).unwrap();
    assert_eq!(errors, vec![]);

    let names = [
        ("create".to_string()),
        ("keys".to_string()),
        ("_set".to_string()),
        ("set".to_string()),
        ("setGlobal".to_string()),
        ("_get".to_string()),
        ("get".to_string()),
        ("exists".to_string()),
        ("_del".to_string()),
        ("del".to_string()),
        ("delGlobal".to_string()),
        ("_copy".to_string()),
        ("copy".to_string()),
        ("copyGlobal".to_string()),
        ("_splitString".to_string()),
        ("_splitStringDelimited".to_string()),
        ("serialize".to_string()),
        ("deserialize".to_string()),
        ("test".to_string()),
    ];

    let functions = functions
        .into_iter()
        .map(|(k, v)| (k, v.inner))
        .collect::<HashMap<_, _>>();

    let expected = names
        .into_iter()
        .map(|x| {
            (
                format!("DICT_fnc_{x}").into(),
                format!("dictionary\\fnc_{x}.sqf"),
            )
        })
        .collect::<HashMap<_, _>>();

    assert_eq!(functions, expected);
}
