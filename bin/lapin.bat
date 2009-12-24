@echo off

rem Copyright (C) 2009 Kenji Nozawa
rem This file is part of LAPIN.
rem
rem This program is free software; you can redistribute it and/or modify
rem it under the terms of the GNU General Public License as published by
rem the Free Software Foundation; either version 2 of the License, or
rem (at your option) any later version.
rem
rem This program is distributed in the hope that it will be useful,
rem but WITHOUT ANY WARRANTY; without even the implied warranty of
rem MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
rem GNU General Public License for more details.
rem
rem You should have received a copy of the GNU General Public License along
rem with this program; if not, write to the Free Software Foundation, Inc.,
rem 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.

set CURRENT_DIR=%cd%
rem echo CURRENT_DIR: "%CURRENT_DIR%"
cd %~dp0
cd ..
set LAPIN_HOME=%cd%
rem echo LAPIN_HOME: "%LAPIN_HOME%"
cd %CURRENT_DIR%

set LAPIN_CLASSPATH=.;%LAPIN_HOME%\classes
for %%i in (%LAPIN_HOME%\lib\*.jar) do call :addClasspath %%i
rem echo LAPIN_CLASSPATH: "%LAPIN_CLASSPATH%"

if "%1" == "-s" (
java -cp %LAPIN_CLASSPATH% lapin.tool.ReplServer %2
goto end
)
if "%1" == "-c" (
java -cp %LAPIN_CLASSPATH% lapin.tool.ReplClient %2 %3
goto end
)
java -cp %LAPIN_CLASSPATH% lapin.tool.Main %*

:end
exit /b

:addClasspath
rem echo addClasspath: arg: %1
set LAPIN_CLASSPATH=%LAPIN_CLASSPATH%;%1
