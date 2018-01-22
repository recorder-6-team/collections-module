<#
    .SYNOPSIS
    Returns the full path of the project file for the specified target.
#>
Param(
    [Parameter(Mandatory=$True)]
    [string]$ProjectGroupPath,
    
    [Parameter(Mandatory=$true)]
    [string]$TargetName)
    
Set-StrictMode -Version 2.0
$ErrorActionPreference = "Stop"    

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
