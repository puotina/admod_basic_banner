@echo off
setlocal EnableDelayedExpansion
setlocal EnableExtensions

rem Function call
set _1=World
set _0=Hello
call :func1 _6 0 _0 _1
echo | set /p ^=!_6!
rem Global and local variables
set v1=Global V1
set v2=Global V2
set v3=Global V3
set _2=Var
call :func2 _7 0 _2
echo | set /p ^=!_7!
echo !v1!
echo !v3!
rem Return value
set /a _3=4
call :func3 _8 0 _3
echo | set /p ^=!_8!
echo:
set /a _4=1
call :func3 _9 0 _4
set ret=!_9!
echo Returned: !ret!
rem Argument containing space
set _5=Param with space
call :f _10 0 _5
set test=!_10!
echo !test!

goto :EOF
: