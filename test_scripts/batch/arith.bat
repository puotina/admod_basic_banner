@echo off
setlocal EnableDelayedExpansion
setlocal EnableExtensions

echo 0
echo 1
echo 42
set /a _0=^(1 + ^(^(4 + 6^) * 3^)^)
echo !_0!
set /a _1=^(8 - ^(3 %% 2^)^)
echo !_1!