@echo off
setlocal EnableDelayedExpansion
setlocal EnableExtensions

echo BYVoid
echo Slash/
echo Backslash\
echo Quote^"^'
echo Tab	Tab
rem println("Newline\nLine2");
rem println("!");
echo http://www.byvoid.com
set /a _0=^(6 / 2^)
set /a _1=^(3 + 5^)
echo !_0!BYVoid!_1!
set _2=3
set /a _3=^(3 + !_2!^)
echo !_3!
set _4=3
set /a _5=^(3