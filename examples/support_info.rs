use std::collections::HashMap;

use pest::Parser;
use pest_derive::Parser;

#[derive(Parser)]
#[grammar_inline = r#"
string = @{("\"\"" | (!"\"" ~ ANY))*}
string_ = _{ ("\"" ~ string ~ "\"") }
member = _{string_ | array}
array = ${ "[]" | "[" ~ member ~ ("," ~ member)* ~ "]" }
"#]
struct PreprocessorParser;

fn main() {
    let types = vec![
        "HashMap",
        "Array",
        "Number",
        "Boolean",
        "Code",
        "String",
        "String",
        "Namespace",
        "Config",
        "Location",
        "Object",
        "Group",
        "TeamMember",
        "Control",
        "Display",
        "Exception",
        "For",
        "If",
        "Switch",
        "While",
        "With",
        "Side",
        "Task",
        "Script",
        "Number",
        "Nothing",
        "NetObject",
        "Anything",
        "DiaryRecord",
        "Exception",
    ];
    let mut map = HashMap::new();
    for t in types {
        map.insert(t.to_ascii_uppercase(), t);
    }
    map.insert("BOOL".to_string(), "Boolean");
    map.insert("SCALAR,NaN".to_string(), "Number");
    map.insert("TEXT".to_string(), "String");
    map.insert("SCALAR".to_string(), "Number");
    map.insert("NaN".to_string(), "Number");
    map.insert("TEAM_MEMBER".to_string(), "TeamMember");
    map.insert("ANY".to_string(), "Anything");
    map.insert("BOOL,NOTHING".to_string(), "For");
    map.insert("NetObject".to_string(), "NetObject");
    map.insert("DIARY_RECORD".to_string(), "DiaryRecord");

    let data = std::fs::read_to_string("supportinfo.txt").unwrap();

    let mut pairs = PreprocessorParser::parse(Rule::array, &data).unwrap();
    let pairs = pairs.next().unwrap().into_inner();

    let mut nullary = vec![];
    let mut unary = vec![];
    let mut binary = vec![];
    for pair in pairs {
        for pair in pair.into_inner() {
            let mut inner = pair.into_inner();
            let type_ = inner.next().unwrap().as_str();
            let op = inner.next().unwrap().as_str();
            inner.next().unwrap();
            let description = inner.next().unwrap().as_str();
            let _example = inner.next().unwrap().as_str();
            inner.next().unwrap();
            let return_type = inner.next().unwrap().as_str();
            let lhs_type = inner.next().unwrap().as_str();
            let rhs_type = inner.next().unwrap().as_str();

            match type_ {
                "u" => {
                    for rhs in rhs_type.split(',') {
                        for return_ in return_type.split(',') {
                            unary.push((
                                op.to_ascii_lowercase(),
                                *map.get(rhs).unwrap(),
                                *map.get(return_).unwrap(),
                                description.replace('\\', "\\\\").replace("\"\"", "\\\""),
                            ));
                        }
                    }
                }
                "b" => {
                    for rhs in rhs_type.split(',') {
                        for lhs in lhs_type.split(',') {
                            if lhs == "TEXT" || rhs == "TEXT" {
                                continue;
                            }
                            for return_ in return_type.split(',') {
                                binary.push((
                                    *map.get(lhs).unwrap(),
                                    op.to_ascii_lowercase(),
                                    *map.get(rhs).unwrap(),
                                    *map.get(return_).unwrap(),
                                    description.replace('\\', "\\\\").replace("\"\"", "\\\""),
                                ));
                            }
                        }
                    }
                }
                "n" => nullary.push((
                    op.to_ascii_lowercase(),
                    *map.get(return_type).unwrap(),
                    description.replace('\\', "\\\\").replace("\"\"", "\\\""),
                )),
                a => todo!("{a}"),
            };
        }
    }
    binary.push(("Switch", ":".to_string(), "Code", "Nothing", "".to_string()));
    binary.push((
        "Control",
        "ctrlsetstructuredtext".to_string(),
        "String",
        "Nothing",
        "Set the Text displayed in the control.".to_string(),
    ));

    nullary.sort();
    unary.sort();
    binary.sort();

    nullary.dedup_by(|x, y| (&x.0, x.1).eq(&(&y.0, y.1)));
    unary.dedup_by(|x, y| (&x.0, x.1, x.2).eq(&(&y.0, y.1, y.2)));
    binary.dedup_by(
        |(x_lhs, x_op, x_rhs, x_r, _), (y_lhs, y_op, y_rhs, y_r, _)| {
            x_lhs.as_bytes() == y_lhs.as_bytes()
                && x_op.as_str() == y_op.as_str()
                && x_rhs.as_bytes() == y_rhs.as_bytes()
                && x_r.as_bytes() == y_r.as_bytes()
        },
    );

    let mut result = String::new();
    result.push_str(&format!(
        r#"// This file is generated automatically by `build_database.py`. Change it there.
use crate::types::Signature::*;
use crate::types::Type::*;
use crate::types::*;
pub const NULLARY: [Signature; {}] = [
"#,
        nullary.len()
    ));
    nullary.iter().for_each(|x| {
        result.push_str(&format!(
            "    Nullary(\"{}\", {}, \"{}\"),\n",
            x.0, x.1, x.2
        ))
    });
    result.push(']');
    result.push_str(";\n\n");

    result.push_str(&format!(
        "pub const UNARY: [Signature; {}] = [\n",
        unary.len()
    ));
    unary.iter().for_each(|x| {
        result.push_str(&format!(
            "    Unary(\"{}\", {}, {}, \"{}\"),\n",
            x.0, x.1, x.2, x.3
        ))
    });
    result.push(']');
    result.push_str(";\n\n");

    result.push_str(&format!(
        "pub const BINARY: [Signature; {}] = [\n",
        binary.len()
    ));
    binary.iter().for_each(|x| {
        result.push_str(&format!(
            "    Binary({}, \"{}\", {}, {}, \"{}\"),\n",
            x.0, x.1, x.2, x.3, x.4
        ))
    });
    result.push(']');
    result.push(';');

    std::fs::write("src/database.rs", result).expect("Unable to write file");
}
