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
ca