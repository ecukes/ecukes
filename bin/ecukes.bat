:: ecukes --- Cucumber for Emacs

:: Copyright (C) 2023 Jen-Chieh Shen

:: Author: Jen-Chieh Shen <jcs090218@gmail.com>
:: Maintainer: Jen-Chieh Shen <jcs090218@gmail.com>
:: Version: 0.6.15
:: Keywords: testing, cucumber
:: URL: http://ecukes.info

:: License:

:: This program is free software; you can redistribute it and/or modify
:: it under the terms of the GNU General Public License as published by
:: the Free Software Foundation; either version 3, or (at your option)
:: any later version.

:: This program is distributed in the hope that it will be useful,
:: but WITHOUT ANY WARRANTY; without even the implied warranty of
:: MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
:: GNU General Public License for more details.

:: You should have received a copy of the GNU General Public License
:: along with GNU Emacs; see the file COPYING.  If not, write to the
:: Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
:: Boston, MA 02110-1301, USA.

@echo off
setlocal enabledelayedexpansion

set ECUKES_HOME=%~dp0..\
set ECUKES_CLI=%ECUKES_HOME%ecukes-cli.el

IF "%INSIDE_EMACS%"=="" (
set ECUKES_EMACS=emacs
) ELSE (
set ECUKES_EMACS="%EMACS%"
)

set ECUKES_ARGS=%*

set HAS_OPTION_WIN=false
set HAS_OPTION_NO_WIN=false

for %%x in (%*) do (
if "%%x"=="--win"    set HAS_OPTION_WIN=true
if "%%x"=="--no-win" set HAS_OPTION_NO_WIN=true
)

set FLAG_WIN=false
IF %HAS_OPTION_WIN%=="true"    set FLAG_WIN=true
IF %HAS_OPTION_NO_WIN%=="true" set FLAG_WIN=true

IF %FLAG_WIN%=="true" (
:mktemp
set ECUKES_OUTFILE=%TMP%\ecukes.%RANDOM%
if exist "%ECUKES_OUTFILE%" goto :mktemp

IF HAS_OPTION_WIN=="true" (
%ECUKES_EMACS% --load %ECUKES_CLI% -Q
) ELSE (
%ECUKES_EMACS% -nw --load %ECUKES_CLI% -Q
)

set STATUS=%ERRORLEVEL%

type %ECUKES_OUTFILE%
del %ECUKES_OUTFILE%

exit %STATUS%
) ELSE (
%ECUKES_EMACS% --script %ECUKES_CLI% -Q
)
