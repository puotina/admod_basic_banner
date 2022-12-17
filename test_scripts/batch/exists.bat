@echo off
setlocal EnableDelayedExpansion
setlocal EnableExtensions

if exist Makefile (
  set /a ex=1
) else (
  set /a ex=0
)
echo !ex!
if e