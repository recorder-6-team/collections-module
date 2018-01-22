<#
    .SYNOPSIS
    Returns the full path of the output directory for the specified project.
#>
Param(
    [Parameter(Mandatory=$True)]
    [string]$ProjectPath)
    
Set-StrictMode -Version 2.0
$ErrorActionPreference = "Stop"

$ConfigPath = [System.IO.Path]::ChangeExtension($ProjectPath, "cfg")

if (-not (Test-Path $ConfigPath -Type "Leaf")) {
    throw "Build configuration file '$ConfigPath' not found."
}

$MatchInfo = Get-Content $ConfigPath | Select-String '^-E"(?<path>.*)"'
if (-not $MatchInfo) {
    throw "Output directory not found in build configuration file '$ConfigPath'."
}

$OutputDir = $MatchInfo.Matches[0].Groups["path"].Captures[0].Value
if (-not [System.IO.Path]::IsPathRooted($OutputDir)) {
    $OutputDir = Join-Path (Split-Path $ProjectPath) $OutputDir -Resolve
}    

Write-Output $OutputDir
