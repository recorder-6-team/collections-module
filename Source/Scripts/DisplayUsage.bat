:: -----------------------------------------------------------------------------
:: Displays a message explaining the usage of a script.
::
:: DisplayUsage [options] script
::
::   script  The path of the script file.
::
:: Options:
::
::   /help   Displays this usage message (Short form: /?).
:: -----------------------------------------------------------------------------
@echo off
setlocal EnableExtensions EnableDelayedExpansion

:: Process any options.
goto :GetOptions
:GotOptions

set ScriptFile=%~1
if not defined ScriptFile goto :usageError

for /f "usebackq tokens=*" %%l in ("%ScriptFile%") do (
    set Line=%%l
    if "!Line:~0,2!" neq "::" (
        goto :eof
    ) else if "!Line:~3,10!" neq "----------" (
        set Text=!Line:~3!
        if "!Text!" equ "" (
            echo[
        ) else if "!Text: =!" equ "" (
            echo[
        ) else (
            echo !Text!
        )
    )
)

endlocal
goto :eof

:usageError
echo Invalid arguments. >&2
echo Run %~n0 /? to display the syntax of this command.>&2
exit /b 1

:: -----------------------------------------------------------------------------
:: Process command line switches.
:: -----------------------------------------------------------------------------
:GetOptions

:nextOption
set Arg=%1
if defined Arg (
    if "!Arg:~0,1!" equ "/" (
        if /i "!Arg!" equ "/?" (
            call "%~f0" "%~f0"
            exit /b 0
        ) else if /i "!Arg!" equ "/help" (
            call "%~f0" "%~f0"
            exit /b 0
        ) else (
            echo Invalid switch - !Arg!>&2
            exit /b 1
        )
        shift /1
        goto :nextOption
    )
)
goto :GotOptions
