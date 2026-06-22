@echo off
setlocal EnableExtensions EnableDelayedExpansion
cd /d %~dp0

set "RSCRIPT="

for /f "delims=" %%D in ('dir /b /ad /o-n "C:\Program Files\R\R-*" 2^>nul') do (
  if exist "C:\Program Files\R\%%D\bin\Rscript.exe" (
    set "RSCRIPT=C:\Program Files\R\%%D\bin\Rscript.exe"
    goto :found_rscript
  )
)

for %%P in (Rscript.exe) do set "RSCRIPT=%%~$PATH:P"

:found_rscript
if not defined RSCRIPT (
  echo Rscript was not found.
  echo Install R from https://cran.r-project.org/ or add Rscript to PATH.
  pause
  exit /b 1
)

start "iapgrowthr" "http://127.0.0.1:3838"
"%RSCRIPT%" -e "shiny::runApp('.', host='127.0.0.1', port=3838, launch.browser=FALSE)"
