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
  echo.
  echo 1. Install R from https://cran.r-project.org/
  echo 2. Or add Rscript to PATH
  echo 3. Re-run this file
  pause
  exit /b 1
)

echo Using Rscript: %RSCRIPT%
echo Checking required R packages...
"%RSCRIPT%" -e "needed <- c('shiny','rsconnect'); inst <- rownames(installed.packages()); miss <- setdiff(needed, inst); if(length(miss)) install.packages(miss, repos='https://cloud.r-project.org')"
if errorlevel 1 (
  echo Failed to install or verify required packages.
  pause
  exit /b 1
)

echo Launching iapgrowthr...
call run_iapgrowthr.bat
