#define KEY_PARAM(KEY,NAME,DEF_VALUE) \
    private #NAME; \
    NAME = [toLower KEY, toUpper KEY, DEF_VALUE, RETNIL(_this)] call CBA_fnc_getArg; \
    TRACE_3("KEY_PARAM",KEY,NAME,DEF_VALUE)
