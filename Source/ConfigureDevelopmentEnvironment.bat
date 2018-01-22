:: -----------------------------------------------------------------------------
:: Configures the Delphi 7 IDE for Collections Module development.
::
:: ConfigureDevelopmentEnvironment [options]
::
:: Options:
::
::   /help  Display this usage message (Short form: /?)
:: -----------------------------------------------------------------------------
@echo off
setlocal EnableExtensions EnableDelayedExpansion

:: Determine the directory containing this script.
set SourceRootDir=%~dp0

set ScriptsDir=%SourceRootDir%Scripts\
set PackagesDir=%SourceRootDir%Binary Packages\
set BuildScript=%ScriptsDir%BuildProjectGroup.ps1
set ProjectGroup=%PackagesDir%\Packages.bpg
set RegistryData=%ScriptsDir%DelphiConfiguration.reg

set PATH="%ScriptsDir:~0,-1%";%PATH%

:: Process command line arguments.
goto :GetOptions
:GotOptions

(2>nul tasklist /FI "IMAGENAME eq delphi32.exe" | 1>nul findstr /i /c:delphi32) && (
    >&2 echo Please close Delphi before running this script.
    exit /b 1
)

if not exist "%BuildScript%" (
    >&2 echo Build script not found.
    exit /b 1
)

if not exist "%ProjectGroup%" (
    >&2 echo Project group not found.
    exit /b 1
)

if not exist "%RegistryData%" (
    >&2 echo Configuration data file not found.
    exit /b 1
)

:: Initial user configuration of the Delphi 7 IDE.
regedit /S "%RegistryData%" || exit /b 1

:: Build the custom packages required for Collections Module development.
powershell ^
    -ExecutionPolicy ByPass ^
    -Command "& '%BuildScript%' -ProjectGroupPath '%ProjectGroup%'" || exit /b 1

:: Install the custom packages.
call :RegisterPackage UnitLib7.bpl                   "In House package for units shared across several packages (Delphi 7)"            || exit /b 1
call :RegisterPackage InHouse7.bpl                   "In House Components - Design-time/Runtime (Delphi 7)"                            || exit /b 1
call :RegisterPackage InHouse7_Design.bpl            "In House Components - Design-time (Delphi 7)"                                    || exit /b 1
call :RegisterPackage InHouseAPI7.bpl                "In House Components - API Wrapper Components (Delphi 7)"                         || exit /b 1
call :RegisterPackage InHouseDB7.bpl                 "In House components  - Data Aware (Delphi 7)"                                    || exit /b 1
call :RegisterPackage trecold7.bpl                   "TTreeCollection for Delphi 7"                                                    || exit /b 1
call :RegisterPackage rapid.bpl                      "RapidTree Package IMCA SYSTEMS (c) 1998-2001"                                    || exit /b 1
call :RegisterPackage Jncc7.bpl                      "JNCC Related Components (Delphi 7)"                                              || exit /b 1
call :RegisterPackage RecorderAddin7.bpl             "Components for Recorder addins (Delphi 7)."                                      || exit /b 1
call :RegisterPackage Luxembourg7_Runtime.bpl        "Luxembourg NHM related components (Delphi 7 Runtime package)"                    || exit /b 1
call :RegisterPackage Luxembourg7_Design.bpl         "Luxembourg NHM related components (Delphi 7 Design-time package)"                || exit /b 1
call :RegisterPackage CollectionsBrowserControls.bpl "Collections Browser Controls"                                                    || exit /b 1
call :RegisterPackage SMImportD7.bpl                 "SMImport suite: data importing into dataset. Scalabium/Mike Shkolnik, 2000-2008" || exit /b 1
call :RegisterPackage FrameViewer7.bpl               "ThtmlViewer, TFrameViewer, and TFrameBrowser"                                    || exit /b 1

echo[
echo Delphi 7 has been configured for Collections Module development.

endlocal
goto :eof


:: -----------------------------------------------------------------------------
:: Processes the command line arguments to this script.
:: -----------------------------------------------------------------------------
:GetOptions

:nextOption
set Arg=%1
if "!Arg:~0,1!" equ "/" (
    if /i "!Arg!" equ "/?" (
        call DisplayUsage "%~f0"
        exit /b 0
    ) else if /i "!Arg!" equ "/help" (
        call DisplayUsage "%~f0"
        exit /b 0
    ) else (
        >&2 echo Invalid switch - !Arg!
        exit /b 1
    )
    shift /1
    goto :nextOption
)

if x%~1x neq xx (
    >&2 echo Too many arguments
    >&2 echo Run %~n0 /? to display the syntax of this command.
    exit /b 1
)
goto :GotOptions


:: -----------------------------------------------------------------------------
:: RegisterPackage packagename description
:: Registers a Delphi 7 package that has been built by this script.
:: -----------------------------------------------------------------------------
:RegisterPackage
setlocal
set PackageName=%~1
set Description=%~2

echo Registering %PackageName%...
1>nul reg add "HKCU\Software\Borland\Delphi\7.0\Known Packages" ^
    /v "%PackagesDir%BPL\%PackageName%" ^
    /d "%Description%" || exit /b 1

endlocal
goto :eof