<#
    .SYNOPSIS
    Prepares a release of the Collections Module.

    .DESCRIPTION
    The MakeRelease.ps1 script builds the Collections Module and associated
    installer projects, and then assembles a complete release in the specified
    directory.
#>
Param(
    # Specifies the path of the release directory. If the specified directory
    # already exists then it must be empty.
    [Parameter(Mandatory=$True)]
    [string]$Path,

    # Indicates that the build step is omitted. This assumes that the projects
    # have already been built.
    [switch]$NoRebuild)

Set-StrictMode -Version 2.0
$ErrorActionPreference = "Stop"

if (-not (Test-Path variable:PSScriptRoot)) {
    # $PSScriptRoot was not an automatic variable until Powershell 3
    $PSScriptRoot = Split-Path $script:MyInvocation.MyCommand.Path
}

$SourceRoot = Split-Path $PSScriptRoot -Parent
$BuildScript = Join-Path $PSScriptRoot "BuildProjectGroup.ps1"
$MainProjectGroup = Join-Path $SourceRoot "Collections Module\Build Projects\CollectionsModule.bpg"
$InstallerProjectGroup = Join-Path $SourceRoot "Install Kit\Installers.bpg"

<#
    .SYNOPSIS
    Ensures that the release directory exists, and is empty.
#>
function CheckReleaseDirectory
{
    Write-Verbose "Checking release directory..."
    
    if (Test-Path $Path -PathType Leaf) {
        throw "'$Path' is a file."
    } elseif (Test-Path $Path -PathType Container) {
        if (Get-ChildItem $Path) {
            throw "The release directory is not empty."
        }
    }
}

<#
    .SYNOPSIS
    Ensures that the release directory exists, and is empty.
#>
function BuildProjects
{    
    & "C:\Program Files\Borland\Delphi7\Bin\brcc32.exe" `
        (Join-Path $SourceRoot "Install Kit\Client Setup\Manifest.rc") | Write-Verbose

    if ($LASTEXITCODE -ne 0) {
        throw "Failed to compile Manifest.res for ClientSetup."
    }

    & $BuildScript $MainProjectGroup
    & $BuildScript $InstallerProjectGroup
}

function CopyProjectTarget(
    [string]$ProjectGroupPath,
    [string]$TargetName,
    [string]$Destination)
{
    Write-Verbose "Copying $TargetName to $Destination"

    $ProjectPath = & (Join-Path $PSScriptRoot "Get-ProjectPath") `
        -ProjectGroupPath $ProjectGroupPath `
        -TargetName $TargetName

    $OutputDir = & (Join-Path $PSScriptRoot "Get-OutputDir") `
        -ProjectPath $ProjectPath

    $TargetPath = Join-Path $OutputDir $TargetName

    if (-not (Test-Path $TargetPath -Type "Leaf")) {
        throw "Build product '$TargetPath' not found."
    }

    $Destination = Join-Path $Path $Destination

    if ([System.IO.Path]::GetExtension($Destination) -eq "" -and
        -not (Test-Path $Destination)) {
        New-Item $Destination -Type "directory" | Out-Null
    }
    
    Copy-Item $TargetPath $Destination
}

function AssembleRelease
{
    Write-Output "Assembling release..."
    
    Write-Verbose "Copying CD image..."    
    Get-ChildItem (Join-Path $SourceRoot "Install Kit\CD Image") |
        Copy-Item -Destination $Path -Recurse       
    Copy-Item `
            -Path (Join-Path $SourceRoot "Install Kit\CD Image\Server Install\NameServer Update.sql") `
            -Destination (Join-Path $Path "Server Upgrade") 
    Copy-Item `
            -Path (Join-Path $SourceRoot "Install Kit\Client Install\CollectionsModuleClientInstall.exe.manifest") `
            -Destination (Join-Path $Path "Client Install") 
        
    Write-Verbose "Copying collections reports..."
    Copy-Item `
            -Path (Join-Path $SourceRoot "Collections Module\Reports\Report Templates\Report Output Type") `
            -Destination (Join-Path $Path "Client Install\Files\Collections Reports") `
            -Recurse
        
    Write-Verbose "Copying upgrade scripts..."
    Copy-Item `
            -Path (Join-Path $SourceRoot "Collections Module\SQL Scripts\Upgrade Scripts") `
            -Destination (Join-Path $Path "Server Install\Scripts\Upgrades") `
            -Recurse
    Copy-Item `
            -Path (Join-Path $SourceRoot "Collections Module\SQL Scripts\Upgrade Scripts") `
            -Destination (Join-Path $Path "Server Upgrade\Scripts") `
            -Recurse

    Write-Verbose "Copying Thesaurus Editor files..."
    Get-ChildItem `
            -Path (Join-Path $SourceRoot "Collections Module\Thesaurus Editor\*") `
            -Include splash.ini, splash.bmp, splash.rgn, TermConstructs.xml |
        Copy-Item -Destination (Join-Path $Path "Thesaurus Editor")        
        
    CopyProjectTarget $MainProjectGroup CollectionsModuleManager.ocx "Client Install\Files"
    CopyProjectTarget $MainProjectGroup GeoAreasTab.ocx              "Client Install\Files"
    CopyProjectTarget $MainProjectGroup Occurrences.ocx              "Client Install\Files"
    CopyProjectTarget $MainProjectGroup CollectionsBrowser.ocx       "Client Install\Files\Collections Library Files"
    CopyProjectTarget $MainProjectGroup QuickEntry.ocx               "Client Install\Files\Collections Library Files"
    CopyProjectTarget $MainProjectGroup SpecimenFinderModule.ocx     "Client Install\Files\Collections Library Files"
    CopyProjectTarget $MainProjectGroup StandardReports.dll          "Client Install\Files\Collections Library Files"
    CopyProjectTarget $MainProjectGroup ThesaurusBrowser.ocx         "Client Install\Files\Collections Library Files"
    CopyProjectTarget $MainProjectGroup UserConfigExtended.ocx       "Client Install\Files\Collections Library Files"
    CopyProjectTarget $MainProjectGroup ThesaurusEditor.exe          "Thesaurus Editor"
    CopyProjectTarget $MainProjectGroup LoadThesaurusEditor.exe      "Thesaurus Editor\editor.exe"
}

CheckReleaseDirectory
if (-not $NoRebuild) {
    BuildProjects
}
AssembleRelease
