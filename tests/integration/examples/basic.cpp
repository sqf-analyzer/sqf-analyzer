#ifndef MAINPREFIX
    #define MAINPREFIX x
#endif

#define ADDON DOUBLES(PREFIX,COMPONENT)

#define RETDEF(VARIABLE,DEFAULT_VALUE) (if (isNil {VARIABLE}) then [{DEFAULT_VALUE}, {VARIABLE}])
