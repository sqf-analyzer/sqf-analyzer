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
    'any': "Type",
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

    if len(return_type_names) > 1 and 'nothing' in return_type_names:
        return_type_names.remove('nothing')

    if len(return_type_names) > 1:
        return_type_name = 'any'
    else:
        return_type_name = return_type_names[0]

    return STRING_TO_TYPE_RETURN[return_type_name]


url = 'https://raw.githubusercontent.com/intercept/intercept/master/src/' \
      'client/headers/client/sqf_pointers_declaration.hpp'
data = urllib.request.urlopen(url).read().decode('utf-8').split('\n')


expressions = []
for line in data:
    if not line.startswith('static '):
        continue

    sections = line.split('__')
    num_sections = len(sections)

    if num_sections not in [4, 5, 6]:
        print('Could\'t read line: ', line)
        continue

    # Name always comes first
    op_name = sections[1]

    # Return type always comes last (some operators have incorrect values for whatever reason)
    if op_name in WRONG_RETURN_TYPES:
        return_type = WRONG_RETURN_TYPES[op_name]
    else:
        return_type = _parse_return_type_names(sections[num_sections-1][:-1])

    # Adds any relevant initialization argument for the return type
    init_code = ''

    # Number of sections allows us to classify the operation
    if num_sections == 6:
        for lhs_type_name in _parse_type_names(sections[2]):
            lhs_type = STRING_TO_TYPE[lhs_type_name]
            for rhs_type_name in _parse_type_names(sections[3]):
                rhs_type = STRING_TO_TYPE[rhs_type_name]
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
    elif num_sections == 5:
        for rhs_type_name in _parse_type_names(sections[2]):
            rhs_type = STRING_TO_TYPE[rhs_type_name]
            expression = 'Unary(' \
                         '"{keyword}", ' \
                         '{rhs_type}, {return_type})'.format(
                keyword=op_name,
                rhs_type=rhs_type,
                return_type=return_type,
            )
            expressions.append(expression)
    else:
        expression = 'Nullary(' \
                     '"{keyword}", ' \
                     '{return_type})'.format(
                keyword=op_name,
                return_type=return_type
            )
        expressions.append(expression)

preamble = r'''// This file is generated automatically by `build_database.py`. Change it there.
use crate::types::Signature::*;
use crate::types::Type::*;
use crate::types::*;'''


# Expressions that use symbols are hardcoded since they aren't present in the parsed file
symbols = r'''
pub const SIGNATURES: [Signature; 3150] = [
    Binary(Array, "#", Number, Anything),
    Binary(Number, "!=", Number, Boolean),
    Binary(String, "!=", String, Boolean),
    Binary(Object, "!=", Object, Boolean),
    Binary(Group, "!=", Group, Boolean),
    Binary(Side, "!=", Side, Boolean),
    Binary(String, "!=", String, Boolean),
    Binary(Config, "!=", Config, Boolean),
    Binary(Display, "!=", Display, Boolean),
    Binary(Control, "!=", Control, Boolean),
    Binary(TeamMember, "!=", TeamMember, Boolean),
    Binary(NetObject, "!=", NetObject, Boolean),
    Binary(Task, "!=", Task, Boolean),
    Binary(Location, "!=", Location, Boolean),
    Binary(Number, "%", Number, Number),
    Binary(Boolean, "&&", Boolean, Boolean),
    Binary(Boolean, "&&", Code, Boolean),
    Binary(Number, "*", Number, Number),
    Binary(Number, "+", Number, Number),
    Binary(String, "+", String, String),
    Binary(Array, "+", Array, Array),
    Binary(Number, "-", Number, Number),
    Binary(Array, "-", Array, Array),
    Binary(Number, "/", Number, Number),
    Binary(Config, "/", String, Config),
    Binary(SwitchType, ":", Code, Nothing),
    Binary(Number, "<", Number, Boolean),
    Binary(Number, "<=", Number, Boolean),
    Binary(Number, "==", Number, Boolean),
    Binary(String, "==", String, Boolean),
    Binary(Object, "==", Object, Boolean),
    Binary(Group, "==", Group, Boolean),
    Binary(Side, "==", Side, Boolean),
    Binary(String, "==", String, Boolean),
    Binary(Config, "==", Config, Boolean),
    Binary(Display, "==", Display, Boolean),
    Binary(Control, "==", Control, Boolean),
    Binary(TeamMember, "==", TeamMember, Boolean),
    Binary(NetObject, "==", NetObject, Boolean),
    Binary(Task, "==", Task, Boolean),
    Binary(Location, "==", Location, Boolean),
    Binary(Number, ">", Number, Boolean),
    Binary(Number, ">=", Number, Boolean),
    Binary(Config, ">>", String, Config),
    Binary(Number, "^", Number, Number),
    Binary(Boolean, "||", Boolean, Boolean),
    Binary(Boolean, "||", Code, Boolean),
    Unary("!", Boolean, Boolean),
    Unary("+", Number, Number),
    Unary("+", Array, Array),
    Unary("-", Number, Number),
'''


with open('src/database.rs', 'w') as f:
    f.write(preamble + '\n')
    f.write(symbols + '    ')
    f.write(',\n    '.join(expressions))
    f.write('\n];\n')
