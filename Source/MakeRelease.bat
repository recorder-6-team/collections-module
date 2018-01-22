:: -----------------------------------------------------------------------------
:: Makes a release of the Collections Module in the specified directory.
::
:: MakeRelease [options] [destination]
::
::   destination  The directory in which to make the release. Defaults to a
::                test directory in the standard network location, with its
::                name taken from the full (four component) version number.
::
:: Options:
::
::   /verbose     Run the build script in verbose mode
::   /overwrite   Do not prompt for confirmation before deleting the contents
::                of an existing release directory
::   /norebuild   Do not rebuild the projects. This option is convenient when
::                working on the build scripts. It should not, however, be used
::                when making an actual release.
::   /help        Display this usage message (Short form: /?)
:: -----------------------------------------------------------------------------
@echo off
setlocal EnableExtensions EnableDelayedExpansion

:: Determine the directory containing this script.
set "SourceRootDir=%~dp0"
set "ScriptsDir=%SourceRootDir%Scripts\"
set "ProjectsDir=%SourceRootDir%Collections Module\Build Projects\"
set "TestDir=\\dorsetsoftware.com\dfs\Projects\Luxembourg NHM\Collections\Test\"
set "PATH="%ScriptsDir:~0,-1%";%PATH%"

:: Process command line arguments.
goto :GetOptions
:GotOptions

set "ReleaseDir=%~1"

if "%~2" neq "" (
    >&2 echo Too many arguments
    >&2 echo Run %~n0 /? to display the syntax of this command.
    exit /b 1
)

:: Preflight checks
call :CheckDelphiIsNotRunning || goto :end
call :CheckSQLUpgradeScripts  || goto :failed

:: Make the release
call :PrepareReleaseDir || goto :failed
call :AssembleRelease   || goto :failed

echo[
echo[*** SUCCESS ***
goto :end

:failed
echo[
echo[*** FAILED ***

:end
endlocal
goto :eof


:: -----------------------------------------------------------------------------
:: Processes the command line options to this script.
:: -----------------------------------------------------------------------------
:GetOptions
set ForceOverwrite=
set DoNotRebuild=
set Verbose=

:nextOption
set Arg=%1
if "!Arg:~0,1!" equ "/" (
    if /i "!Arg!" equ "/?" (
        call DisplayUsage "%~f0"
        exit /b 0
    ) else if /i "!Arg!" equ "/help" (
        call DisplayUsage "%~f0"
        exit /b 0
    ) else if /i "!Arg!" equ "/verbose" (
        set Verbose=1
    ) else if /i "!Arg!" equ "/overwrite" (
        set ForceOverwrite=1
    ) else if /i "!Arg!" equ "/norebuild" (
        set DoNotRebuild=1
    ) else (
        >&2 echo Invalid switch - !Arg!
        exit /b 1
    )
    shift /1
    goto :nextOption
)
goto :GotOptions


:: -----------------------------------------------------------------------------
:: CheckDelphiIsNotRunning
:: Checks that the Delphi IDE is not running.
:: -----------------------------------------------------------------------------
:CheckDelphiIsNotRunning
(2>nul tasklist /FI "IMAGENAME eq delphi32.exe" | 1>nul findstr /i /c:delphi32) && (
    >&2 echo Please close Delphi before running this script.
    exit /b 1
)
exit /b 0


:: -----------------------------------------------------------------------------
:: CheckSQLUpgradeScripts
:: Checks that none of the database upgrade scripts have Unicode byte-order
:: marks.
:: -----------------------------------------------------------------------------
:CheckSQLUpgradeScripts
setlocal
set "Script=%ScriptsDir%CheckUpgradeScripts.ps1"

powershell -ExecutionPolicy ByPass -Command "& '%Script%'" || exit /b 1

endlocal
goto :eof


:: -----------------------------------------------------------------------------
:: PrepareReleaseDir
:: Ensures that an empty directory is available in which to assemble the
:: release.
:: -----------------------------------------------------------------------------
:PrepareReleaseDir
setlocal

if not defined ReleaseDir (
    call :UseDefaultReleaseDir || exit /b 1
) else if "%ReleaseDir:~0,1%" neq "\" (
    set "ReleaseDir=%ReleaseDir%\"
)

if not exist "%ReleaseDir%*" (
    mkdir "%ReleaseDir%" || exit /b 1
) else (
    1>nul (dir /b /a "%ReleaseDir%" | findstr .) && (
        if defined ForceOverwrite (
            echo Clearing release directory...
            call :ClearDirectory "%ReleaseDir%" || exit /b 1
        ) else (
            echo The release directory is not empty.
            call :ConfirmationPrompt "Delete contents" && (
                call :ClearDirectory "%ReleaseDir%" || exit /b 1
            ) || (
                exit /b 1
            )
        )
    )
)

endlocal & set "ReleaseDir=%ReleaseDir%"
exit /b 0


:: -----------------------------------------------------------------------------
:: AssembleRelease
:: -----------------------------------------------------------------------------
:AssembleRelease
setlocal
set "Options=-Path '%ReleaseDir:~0,-1%'"
if defined Verbose (
    set "Options=%Options% -Verbose"
)
if defined DoNotRebuild (
    set "Options=%Options% -NoRebuild"
)
set "Command=& '%ScriptsDir%MakeRelease.ps1' %Options%"

powershell -ExecutionPolicy ByPass -Command "%Command%" || exit /b 1

endlocal
goto :eof

:: -----------------------------------------------------------------------------
:: UseDefaultReleaseDir
:: Sets ReleaseDir to the default path for the current version number.
:UseDefaultReleaseDir
set "ReleaseDir="
setlocal

for /f "usebackq tokens=*" %%l in ("%ProjectsDir%CollectionsBrowser.dof") do (
    set "Line=%%l"
    if "!Line:~0,12!" equ "FileVersion=" (
        set "ReleaseDir=!Line:~12!"
    )
)

if not defined ReleaseDir (
    >&2 echo Failed to determine release version number.
    exit /b 1
)

endlocal & set "ReleaseDir=%TestDir%%ReleaseDir%\"
goto :eof


:: -----------------------------------------------------------------------------
:: ConfirmationPrompt message
:: Prompts the user for a yes/no answer with the given message.
:: -----------------------------------------------------------------------------
:ConfirmationPrompt
setlocal
set "Message=%~1"
:showPrompt
set /P Response="%Message% (Y/N)? "
if /i "%Response%" equ "N" exit /b 1
if /i "%Response%" equ "Y" exit /b 0
goto :showPrompt


:: -----------------------------------------------------------------------------
:: ClearDirectory directory
:: Deletes all files and folders within the specified directory.
:: -----------------------------------------------------------------------------
:ClearDirectory
setlocal
set "Directory=%~1"

for /d %%f in ("%Directory%*") do (
    rmdir /s /q "%%f" || exit /b 1
)

for %%f in ("%Directory%*") do (
    del /f /q "%%f" || exit /b 1
)

endlocal
goto :eof