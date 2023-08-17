use std::collections::HashMap;

use sqf::cpp::{analyze, analyze_file};
use sqf::preprocessor::{tokens, Configuration};
use sqf::span::Spanned;

#[test]
fn basic() {
    // https://community.bistudio.com/wiki/Arma_3:_Functions_Library
    let case = r#"
class CfgFunctions {};
"#;
    let iter = tokens(case, Default::default()).unwrap();
    let (functions, errors) = analyze(iter);
    assert!(functions.is_empty());
    assert_eq!(errors, vec![]);
}

#[test]
fn with_expressions() {
    let case = r#"
    class set_params {
        idd=-1;
        movingenable=false;
    
        class controls {
            class HQ_frame: A3A_core_BattleMenuFrame
            {
                x = 0.254979 * safezoneW + safezoneX;
                y = 0.233941 * safezoneH + safezoneY;
            };
        };
};
"#;
    let iter = tokens(case, Default::default()).unwrap();
    let (functions, errors) = analyze(iter);
    assert!(functions.is_empty());
    assert_eq!(errors, vec![]);
}

#[test]
fn subclass_empty_class() {
    let case = r#"
    class flag_NATO;
    class a3a_flag_cdf: flag_NATO {};
"#;
    let iter = tokens(case, Default::default()).unwrap();
    let (functions, errors) = analyze(iter);
    assert!(functions.is_empty());
    assert_eq!(errors, vec![]);
}

#[test]
fn start_with_number() {
    let case = r#"
    class 3CBF_TKC {};
"#;
    let iter = tokens(case, Default::default()).unwrap();
    let (functions, errors) = analyze(iter);
    assert!(functions.is_empty());
    assert_eq!(errors, vec![]);
}

#[test]
fn negative_number() {
    let case = r#"
    class AA {a = -1};
"#;
    let iter = tokens(case, Default::default()).unwrap();
    let (functions, errors) = analyze(iter);
    assert!(functions.is_empty());
    assert_eq!(errors, vec![]);
}

#[test]
fn boolean() {
    let case = r#"
    class AA {a = false; b = true;};
"#;
    let iter = tokens(case, Default::default()).unwrap();
    let (functions, errors) = analyze(iter);
    assert!(functions.is_empty());
    assert_eq!(errors, vec![]);
}

#[test]
fn hexadecimal() {
    let case = r#"
    class AA {a = 0x10;};
"#;
    let iter = tokens(case, Default::default()).unwrap();
    let (functions, errors) = analyze(iter);
    assert!(functions.is_empty());
    assert_eq!(errors, vec![]);
}

#[test]
fn subclass() {
    let case = r#"
class CfgFunctions : A {};
"#;
    let iter = tokens(case, Default::default()).unwrap();
    let (functions, errors) = analyze(iter);
    assert!(functions.is_empty());
    assert_eq!(errors, vec![]);
}

#[test]
fn empty_class() {
    let case = r#"
class A;
"#;
    let iter = tokens(case, Default::default()).unwrap();
    let (functions, errors) = analyze(iter);
    assert!(functions.is_empty());
    assert_eq!(errors, vec![]);
}

#[test]
fn parenthesis() {
    let case = r#"
class A {a = (1 * 1);}
"#;
    let iter = tokens(case, Default::default()).unwrap();
    let (functions, errors) = analyze(iter);
    assert!(functions.is_empty());
    assert_eq!(errors, vec![]);
}

#[test]
fn coop_game_type() {
    let case = r#"
class A {a = COop;}
"#;
    let iter = tokens(case, Default::default()).unwrap();
    let (functions, errors) = analyze(iter);
    assert!(functions.is_empty());
    assert_eq!(errors, vec![]);
}

#[test]
fn empty_subclass() {
    let case = r#"
class A: B;
"#;
    let iter = tokens(case, Default::default()).unwrap();
    let (functions, errors) = analyze(iter);
    assert!(functions.is_empty());
    assert_eq!(errors, vec![]);
}

#[test]
fn error_class_no_name() {
    let case = r#"
class {};
"#;
    let iter = tokens(case, Default::default()).unwrap();
    let (_, errors) = analyze(iter);
    assert!(!errors.is_empty());
}

#[test]
fn error_assign_array() {
    let case = r#"
a = [];
"#;
    let iter = tokens(case, Default::default()).unwrap();
    let (_, errors) = analyze(iter);
    assert!(!errors.is_empty());
}

#[test]
fn error_assign_token() {
    let case = r#"
a = =;
"#;
    let iter = tokens(case, Default::default()).unwrap();
    let (_, errors) = analyze(iter);
    assert!(!errors.is_empty());
}

#[test]
fn error_assign_empty() {
    let case = r#"
a =;
"#;
    let iter = tokens(case, Default::default()).unwrap();
    let (_, errors) = analyze(iter);
    assert!(!errors.is_empty());
}

#[test]
fn error_assign_empty_neg() {
    let case = r#"
a = -;
"#;
    let iter = tokens(case, Default::default()).unwrap();
    let (_, errors) = analyze(iter);
    assert!(!errors.is_empty());
}

#[test]
fn error_subclass_no_name() {
    let case = r#"
class A : ;
"#;
    let iter = tokens(case, Default::default()).unwrap();
    let (_, errors) = analyze(iter);
    assert!(!errors.is_empty());
}

#[test]
fn error_missing_closing_array() {
    let case = r#"
a = [
"#;
    let iter = tokens(case, Default::default()).unwrap();
    let (_, errors) = analyze(iter);
    assert!(!errors.is_empty());
}

#[test]
fn error_missing_closing_par() {
    let case = "a = (";
    let iter = tokens(case, Default::default()).unwrap();
    let (_, errors) = analyze(iter);
    assert!(!errors.is_empty());
}

#[test]
fn error_missing_closing_brackets() {
    let case = "a = {";
    let iter = tokens(case, Default::default()).unwrap();
    let (_, errors) = analyze(iter);
    assert!(!errors.is_empty());
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
        tag = "SOME";
        class Category
        {
            class myFunction {};
        };
    };
};
"#;
    let iter = tokens(case, Default::default()).unwrap();
    let (functions, errors) = analyze(iter);
    assert_eq!(errors, vec![]);

    assert_eq!(
        functions,
        HashMap::from([
            (
                "TAG_fnc_myFunction".into(),
                Spanned::new("Category\\fn_myFunction.sqf".into(), (93, 103)),
            ),
            (
                "TAG_fnc_myFunction1".into(),
                Spanned::new("My\\Category\\Path\\fn_myFunction1.sqf".into(), (215, 226)),
            ),
            (
                "TAG_fnc_myDataFunction".into(),
                Spanned::new("My\\Function\\Filepath.sqf".into(), (528, 542)),
            ),
            (
                "SOME_fnc_myFunction".into(),
                Spanned::new("Category\\fn_myFunction.sqf".into(), (808, 818)),
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
    let iter = tokens(case, Default::default()).unwrap();
    let (_, errors) = analyze(iter);
    assert_eq!(errors, vec![]);
}

#[test]
fn infer_addon() {
    use std::fs;
    let path = "tests/integration/dictionary/addons/dictionary/config.cpp";
    let case = fs::read_to_string(path).unwrap();

    let iter = tokens(&case, Default::default()).unwrap();
    let (_, errors) = analyze(iter);
    assert_eq!(errors, vec![]);
}

#[test]
fn addon_basic() {
    let path = "tests/integration/dictionary/addons/dictionary/config.cpp";
    let configuration = Configuration::with_path(path.to_owned().into());

    let (functions, errors) = analyze_file(configuration).unwrap();
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
                format!("dictionary/fnc_{x}.sqf"),
            )
        })
        .collect::<HashMap<_, _>>();

    assert_eq!(functions, expected);
}

#[test]
fn versions() {
    let case = r#"
#define QUOTE(var1) #var1
#define MAJOR 3
#define MINOR 3
#define PATCHLVL 3
#define VERSION MAJOR.MINOR.PATCHLVL
#define VERSION_AR MAJOR,MINOR,PATCHLVL
#define VERSION_CONFIG version = VERSION; versionStr = QUOTE(VERSION); versionAr[] = {VERSION_AR}
class CfgPatches {
    class A{
        authorUrl = "";
        version = 3.1.1;
        version = VERSION; versionStr = QUOTE(VERSION); versionAr[] = {VERSION_AR}
};
};
"#;
    let iter = tokens(case, Default::default()).unwrap();
    let (functions, errors) = analyze(iter);
    assert!(functions.is_empty());
    assert_eq!(errors, vec![]);
}

#[test]
fn names_no_quotes() {
    let case = r#"file = \x\blabla\x.sqf"#;
    let iter = tokens(case, Default::default()).unwrap();
    let (_, errors) = analyze(iter);
    assert_eq!(errors, vec![]);
}

#[test]
fn empty_expressions() {
    let case = r#";;;;"#;
    let iter = tokens(case, Default::default()).unwrap();
    let (_, errors) = analyze(iter);
    assert_eq!(errors, vec![]);
}

#[test]
fn bls() {
    let case = r#"
    class CfgFunctions
    {
        class TAG
        {
            class Category1
            {
                class myFunction {
                    file = "aa";
                    compile = 1;
                };
            };
        };
    };
    "#;
    let iter = tokens(case, Default::default()).unwrap();
    let (functions, errors) = analyze(iter);
    assert_eq!(errors, vec![]);
    assert_eq!(
        functions,
        HashMap::from([(
            "TAG_fnc_myFunction".into(),
            Spanned::new("aa".to_string(), (122, 132))
        )])
    );
}
