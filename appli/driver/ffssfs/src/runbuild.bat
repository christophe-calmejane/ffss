@echo off

rem
rem This file makes it convenient to call the build program from Visual Studio
rem but it can also be used manually at the command line.
rem
rem syntax: runbuild nt4|w2k checked|free [-c] | runbuild all
rem

setlocal

set NT4_DDK_ROOT=e:\ntddk
set W2K_DDK_ROOT=e:\ntddk

if "%1"=="all" goto all

if "%2"=="checked" goto arg2ok
if "%2"=="free" goto arg2ok
goto usage

:arg2ok
if "%1"=="nt4" goto nt4
if "%1"=="w2k" goto w2k
goto usage

:nt4
if not exist %NT4_DDK_ROOT% goto noddk
pushd .
call %NT4_DDK_ROOT%\\bin\\setenv %NT4_DDK_ROOT% %2
popd
if "%DDKBUILDENV%"=="" goto noddk
build %3
goto exit

:w2k
if not exist %W2K_DDK_ROOT% goto noddk
pushd .
call %W2K_DDK_ROOT%\\bin\\setenv %W2K_DDK_ROOT% %2
popd
if "%DDKBUILDENV%"=="" goto noddk
build %3
goto exit

:all
call runbuild nt4 checked -c
call runbuild nt4 free -c
call runbuild w2k checked -c
call runbuild w2k free -c
goto exit

:usage
echo syntax: runbuild nt4^|w2k checked^|free [-c] ^| runbuild all
goto exit

:noddk
echo Can't find the DDK, check the path in runbuild.bat
goto exit

:exit
endlocal
