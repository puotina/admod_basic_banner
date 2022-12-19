@echo off
setlocal EnableDelayedExpansion
setlocal EnableExtensions

rem Function call
set _1=World
set _0=Hello
call :func1 _6 0 _0 _1
echo | set /p ^=!_6!
rem 