@echo off
setlocal EnableDelayedExpansion
setlocal EnableExtensions

if 2 LSS 10 (
  echo Yes
)
if 1 EQU 1 (
  if 1 NEQ 1 (
    set /a v=^(4 + 1^)
  )