@echo off
setlocal EnableDelayedExpansion
setlocal EnableExtensions

set /a _0=^(1 + ^(^(4 + 6^) * 3^)^)
set a=Value: !_0!
echo