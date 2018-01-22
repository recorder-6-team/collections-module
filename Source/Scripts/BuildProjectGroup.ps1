<#
    .SYNOPSIS
    Builds a list of Delphi projects that are read from a .BPG file.

    .DESCRIPTION
    The BuildProjectGroup.ps1 script builds the projects that are listed in
    a .BPG file (it assumes that they are all Delphi projects).
    
    Unlike Borland's MAKE command (as shipped with Delphi 7) this script
    correctly handles paths that contain spaces.

    .PARAMETER ProjectGroupPath
    Specifies the path of the project group (.BPG) file.
#>
param(
    [Parameter(Mandatory=$True)]
    [string]$ProjectGroupPath)

Set-StrictMode -Version 2.0
$ErrorActionPreference = "Stop"

# Compilation options
# -$C- = Assertions OFF
# -$O+ = Optimization ON
# -$W- = Stack frames OFF
# -$Y- = Reference information OFF
#
# Compiler mode options
# -B   = Build (as opposed to "Compile")
# -Q   = Quiet mode
$CompilerSwitches = ('-$C-', '-$O+', '-$W-', '-$Y-', '-B', '-Q')

if (-not (Test-Path $ProjectGroupPath)) {
    Write-Error "The project group file does not exist."
    exit 1
}

if (-not (Test-Path variable:PSScriptRoot)) {
    # $PSScriptRoot was not an automatic variable until Powershell 3
    $PSScriptRoot = Split-Path $script:MyInvocation.MyCommand.Path
}

<#
    .SYNOPSIS
    Lists the project targets in the specified project group file.
#>
function GetProjects()
{
    $MatchInfo = Get-Content $ProjectGroupPath | Out-String | Select-String "(?msx)

            # Find the variable listing the projects in the project group
            ^PROJECTS[^\r\n\S]*=

            # Capture project names from the first line of the definition
            ([^\r\n\S]+(?<project>[^\s\\]+))+

            # Continuation lines
            (\s*\\\r?\n
                # Capture project names from the continuation line
                ([^\r\n\S]+(?<project>[^\s\\]+))+
            )*"

    if ($MatchInfo) {
        $MatchInfo.Matches[0].Groups["project"] |
            foreach { $_.Captures } |
            foreach { $_.Value }
    }
}

<#
    .SYNOPSIS
    Returns the full path of the project file for the specified target.
#>
function GetProjectPath(
    [string]$TargetName)
{
    $Pattern = "^" +
        [System.Text.RegularExpressions.Regex]::Escape($TargetName) +
        ": (?<path>.*)"

    $MatchInfo = Get-Content $ProjectGroupPath | Select-String $Pattern
        
    if (-not $MatchInfo) {
        Write-Error "Failed to find target $TargetName."
        exit 1
    }

    $ProjectPath = $MatchInfo.Matches[0].Groups["path"].Captures[0].Value
    
    Join-Path (Split-Path $ProjectGroupPath) $ProjectPath -Resolve
}

<#
    .SYNOPSIS
    Builds the Delphi project corresponding to the specified target.
#>
function BuildProject(
    [string]$TargetName)
{
    $ProjectPath = & (Join-Path $PSScriptRoot "Get-ProjectPath") `
        -ProjectGroupPath $ProjectGroupPath `
        -TargetName $TargetName
    
    if (-not (Test-Path $ProjectPath)) {
        Write-Error "Failed to find project file $ProjectPath."
        exit 1
    }

    Push-Location (Split-Path $ProjectPath)
    try
    {
        & "C:\Program Files\Borland\Delphi7\Bin\dcc32.exe" $CompilerSwitches "$ProjectPath" | Write-Verbose

        if ($LASTEXITCODE -ne 0)
        {
            Write-Error "Failed to build $(Split-Path $ProjectPath -Leaf)."
            exit 1
        }

        $MadExceptSettings = [System.IO.Path]::ChangeExtension($ProjectPath, "mes")

        if (Test-Path $MadExceptSettings -PathType Leaf) {
            $OutputDir = & (Join-Path $PSScriptRoot "Get-OutputDir") `
                -ProjectPath $ProjectPath
                
            $TargetPath = Join-Path $OutputDir $TargetName
            
            & "C:\Program Files\madCollection\madExcept\Tools\madExceptPatch.exe" "$TargetPath" "$MadExceptSettings" | Write-Verbose

            if ($LASTEXITCODE -ne 0)
            {
                Write-Error "Failed to patch $(Split-Path $TargetPath -Leaf) for madExcept."
                exit 1
            }        
        }
    }
    finally
    {
        Pop-Location
    }
}

GetProjects $ProjectGroupPath | foreach {
    Write-Output "Building $_..."
    BuildProject $_
}
