<#
    .SYNOPSIS
    Checks for database upgrade scripts that have Unicode byte-order marks.

    .DESCRIPTION
    Upgrade scripts that have been assembled using modern text editors may have
    been saved with a Unicode encoding (e.g. UTF-8 or UTF-16). The server
    upgrade tool (CollectionsUpgrade.exe) does not support Unicode, however,
    and in particular it will choke on byte-order marks.
    
    The CheckUpgradeScripts.ps1 script analyses all of the files in the
    'Collections Module\SQL Scripts\Upgrade Scripts' folder and checks that
    none of them begin with byte-order marks.
#>
Set-StrictMode -Version 2.0
$ErrorActionPreference = "Stop"

if (-not (Test-Path variable:PSScriptRoot)) {
    # $PSScriptRoot was not an automatic variable until Powershell 3
    $PSScriptRoot = Split-Path $script:MyInvocation.MyCommand.Path
}

$SourceRoot = Split-Path $PSScriptRoot -Parent
$UpgradeDir = Join-Path $SourceRoot "Collections Module\SQL Scripts\Upgrade Scripts" -Resolve

# Taken from https://en.wikipedia.org/wiki/Byte_order_mark
$BOMs = (
    (0xEF, 0xBB, 0xBF),             # UTF-8
    (0xFE, 0xFF),                   # UTF-16 (BE)
    (0xFF, 0xFE),                   # UTF-16 (LE)
    (0x00, 0x00, 0xFE, 0xFF),       # UTF-32 (BE)
    (0xFF, 0xFE, 0x00, 0x00),       # UTF-32 (LE)
    (0x2B, 0x2F, 0x76, 0x38),       # UTF-7
    (0x2B, 0x2F, 0x76, 0x39),       # 
    (0x2B, 0x2F, 0x76, 0x2B),       # 
    (0x2B, 0x2F, 0x76, 0x2F),       # 
    (0x2B, 0x2F, 0x76, 0x38, 0x2D), # 
    (0xF7, 0x64, 0x4C),             # UTF-1
    (0xDD, 0x73, 0x66, 0x73),       # UTF-EBCDIC
    (0x0E, 0xFE, 0xFF),             # SCSU
    (0xFB, 0xEE, 0x28),             # BOCU-1
    (0x84, 0x31, 0x95, 0x33)        # GB-18030
)

$BadUpgradeScripts = Get-ChildItem $UpgradeDir | Where-Object {
    $Prefix = Get-Content (Join-Path $UpgradeDir $_) -Encoding Byte -TotalCount 5
    $BOMs | Where-Object {
        -not (Compare-Object $_ $Prefix[0..($_.Length - 1)])
    }
}

if ($BadUpgradeScripts) {
    Write-Output "The following SQL upgrade scripts have Unicode byte-order marks."
    Write-Output "These files must be saved with the correct encoding before making a release."
    Write-Output ""
    
    $BadUpgradeScripts | foreach {
        Write-Output "  $_"
    }
    exit 1
}