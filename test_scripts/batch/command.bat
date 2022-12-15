@echo off
setlocal EnableDelayedExpansion
setlocal EnableExtensions

echo Println Called
set cmd=echo
!cmd! Echo Called
for /f "delims=" %%i in ('e