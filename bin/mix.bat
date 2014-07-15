@echo off

rem Set MIXCALLERDIR to the current directory for the life of the batch file
setlocal
set MIXCALLERDIR=%CD%

rem Change into the bin directory
pushd %~dp0

rem Call elixir, using relative path to bin\mix
call elixir.bat mix %*

rem Return to original directory
popd

endlocal
