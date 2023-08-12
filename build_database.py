"""
This script is used to write `sqf/dababase.py`, that contains all valid SQF expressions.
It reads a file from here:

https://raw.githubusercontent.com/intercept/intercept/master/src/client/headers/client/sqf_pointers_declaration.hpp
"""
import urllib.request

# The mapping of SQF types to our types
STRING_TO_TYPE = {
    "hashmap": "HashMap",
    'array': "Array",
    'scalar': "Number",
    'bool': "Boolean",
    'code': "Code",
    'string': "String",
    'text': "String",
    'namespace': "Namespace",
    'config': "Config",
    'location': "Location",
    'object': "Object",
    'group': "Group",
    'member': "TeamMember",  # team_member gets split
    'control': "Control",
    'display': "Display",
    'exception': "TryType",
    'for': "ForType",
    'if': "IfType",
    'switch': "SwitchType",
    'while': "WhileType",
    'with': "WithType",
    'side': "Side",
    'task': "Task",
    'script': "Script",
    'nan': "Number",
    'nothing': "Nothing",
    'netobject': "NetObject",
    'any': "Anything",
    'diary': "DiaryReport",  # diary_record gets split
}

# the argument the type is initialized with
TYPE_TO_INIT_ARGS = {
    "Namespace": "'missionNamespace'",
}

# The return type "ANY" means that we do not know it, so it is Nothing()
STRING_TO_TYPE_RETURN = STRING_TO_TYPE.copy()
STRING_TO_TYPE_RETURN['any'] = "Anything"

WRONG_RETURN_TYPES = {
    'attachedto': "Object",
    'getclientstatenumber': "Number",
    'handgunmagazine': "Array",
    'ammoonpylon': "Anything"
}


def _parse_type_names(type_names):
    # Alternative types separated by _ char
    types_names = type_names.split('_')

    # Never care about NaN type (covered by scalar)
    if 'nan' in types_names:
        types_names.remove('nan')

    # Remove parts of types that also get split
    if 'team' in types_names:
        types_names.remove('team')
    if 'record' in types_names:
        types_names.remove('record')

    return types_names


def _parse_return_type_names(return_type_names):
    return_type_names = _parse_type_names(return_type_names)
    if len(return_type_names) > 1:
        return_type_name = 'any'

    if len(return_type_names) > 1 and 'nothing' in return_type_names:
        return_type_names.remove('nothing')

    if len(return_type_names) > 1:
        return_type_name = 'any'
    else:
        return_type_name = return_type_names[0]

    return STRING_TO_TYPE_RETURN[return_type_name]


url = 'https://raw.githubusercontent.com/intercept/intercept/master/src/client/headers/client/sqf_pointers_declaration.hpp'
data = urllib.request.urlopen(url).read().decode('utf-8').split('\n')

nullaries = []
unaries = [
    ("!", "Boolean", "Boolean"),
    ("+", "Number", "Number"),
    ("+", "Array", "Array"),
    ("-", "Number", "Number"),
    ("isnil", "Anything", "Boolean"),
]
binaries = [
    ("Array", "#", "Number", "Anything"),
    ("Number", "!=", "Number", "Boolean"),
    ("String", "!=", "String", "Boolean"),
    ("Object", "!=", "Object", "Boolean"),
    ("Group", "!=", "Group", "Boolean"),
    ("Side", "!=", "Side", "Boolean"),
    ("String", "!=", "String", "Boolean"),
    ("Config", "!=", "Config", "Boolean"),
    ("Display", "!=", "Display", "Boolean"),
    ("Control", "!=", "Control", "Boolean"),
    ("TeamMember", "!=", "TeamMember", "Boolean"),
    ("NetObject", "!=", "NetObject", "Boolean"),
    ("Task", "!=", "Task", "Boolean"),
    ("Location", "!=", "Location", "Boolean"),
    ("Number", "%", "Number", "Number"),
    ("Boolean", "&&", "Boolean", "Boolean"),
    ("Boolean", "&&", "Code", "Boolean"),
    ("Number", "*", "Number", "Number"),
    ("Number", "+", "Number", "Number"),
    ("String", "+", "String", "String"),
    ("Array", "+", "Array", "Array"),
    ("Number", "-", "Number", "Number"),
    ("Array", "-", "Array", "Array"),
    ("Number", "/", "Number", "Number"),
    ("Config", "/", "String", "Config"),
    ("SwitchType", ":", "Code", "Nothing"),
    ("Number", "<", "Number", "Boolean"),
    ("Number", "<=", "Number", "Boolean"),
    ("Number", "==", "Number", "Boolean"),
    ("String", "==", "String", "Boolean"),
    ("Object", "==", "Object", "Boolean"),
    ("Array", "==", "Array", "Boolean"),
    ("Group", "==", "Group", "Boolean"),
    ("Side", "==", "Side", "Boolean"),
    ("String", "==", "String", "Boolean"),
    ("Config", "==", "Config", "Boolean"),
    ("Display", "==", "Display", "Boolean"),
    ("Control", "==", "Control", "Boolean"),
    ("TeamMember", "==", "TeamMember", "Boolean"),
    ("NetObject", "==", "NetObject", "Boolean"),
    ("Task", "==", "Task", "Boolean"),
    ("Location", "==", "Location", "Boolean"),
    ("Number", ">", "Number", "Boolean"),
    ("Number", ">=", "Number", "Boolean"),
    ("Config", ">>", "String", "Config"),
    ("Number", "^", "Number", "Number"),
    ("Boolean", "||", "Boolean", "Boolean"),
    ("Boolean", "||", "Code", "Boolean"),
]

for t in ["Number", "Boolean", "String", "Code", "Side", "Config", "Namespace", "Array"]:
    binaries.append(("HashMap", "get", t, "Anything"))
    binaries.append(("HashMap", "deleteat", t, "Anything"))

expressions = []
for line in data:
    if not line.startswith('static '):
        continue

    sections = line[:-1].split(" ")[-1].split('__')
    num_sections = len(sections)

    if num_sections not in [4, 5, 6]:
        print('Could\'t read line: ', line)
        continue

    # Name always comes first
    op_type = sections[0]
    op_name = sections[1]

    # fix some return types
    if op_name in WRONG_RETURN_TYPES:
        return_type = WRONG_RETURN_TYPES[op_name]
    else:
        return_type = _parse_return_type_names(sections[num_sections-1])

    if op_type == "nular":
        nullaries.append((op_name, return_type))
    if op_type == "binary":
        for lhs in _parse_type_names(sections[2]):
            for rhs in _parse_type_names(sections[3]):
                binaries.append((STRING_TO_TYPE[lhs], op_name, STRING_TO_TYPE[rhs], return_type))
    if op_type == "unary":
        for argument in _parse_type_names(sections[2]):
            unaries.append((op_name, STRING_TO_TYPE[argument], return_type))

unaries = list(set(unaries))
binaries = list(set(binaries))
unaries.sort()
binaries.sort(key=lambda x: x[1])

def _write_rs():
    expressions = []
    for exp in nullaries:
        op_name, return_type = exp
        expression = 'Nullary(' \
                '"{keyword}", ' \
                '{return_type})'.format(
        keyword=op_name,
        return_type=return_type
        )
        expressions.append(expression)


    for exp in unaries:
        op_name, rhs_type, return_type = exp
        expression = 'Unary(' \
                        '"{keyword}", ' \
                        '{rhs_type}, {return_type})'.format(
            keyword=op_name,
            rhs_type=rhs_type,
            return_type=return_type,
        )
        expressions.append(expression)


    for exp in binaries:
        lhs_type, op_name, rhs_type, return_type = exp
        expression = 'Binary(' \
                        '{lhs_type}, ' \
                        '"{keyword}", ' \
                        '{rhs_type}, {return_type})'.format(
            lhs_type=lhs_type,
            keyword=op_name,
            rhs_type=rhs_type,
            return_type=return_type,
        )
        expressions.append(expression)

    preamble = r'''// This file is generated automatically by `build_database.py`. Change it there.
    use crate::types::Signature::*;
    use crate::types::Type::*;
    use crate::types::*;'''


    # Expressions that use symbols are hardcoded since they aren't present in the parsed file
    symbols = f'''pub const SIGNATURES: [Signature; {len(expressions)}] = [
    '''


    with open('src/database.rs', 'w') as f:
        f.write(preamble + '\n')
        f.write(symbols + '    ')
        f.write(',\n    '.join(expressions))
        f.write('\n];\n')

_write_rs()
