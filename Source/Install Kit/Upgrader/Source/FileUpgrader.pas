{===============================================================================
  Unit:           FileUpgrader

  Defines:

  Description:

  Created:

  Last revision information:
    $Revision: 2 $
    $Date: 27/08/04 14:13 $
    $Author: Johnvanbreda $

===============================================================================}
unit FileUpgrader;

interface

uses
  SysUtils, Windows, Messages, Classes, Forms, ComCtrls, Settings, VCLUnzip;

type
  TFileUpgrader = class (TObject)
  private
    FFolder: string;
    FProgress: TProgressBar;
    FUnzipper: TVCLUnzip;
    procedure ZipTotalPercentDone(Sender: TObject; Percent: LongInt);
  public
    constructor Create(ASettings: TSettings);
    destructor Destroy; override;
    procedure CopyFiles(AProgressBar: TProgressBar);
  end;
  
//==============================================================================
implementation

uses
  GeneralFunctions;

{-==============================================================================
    TFileUpgrader
===============================================================================}
{-------------------------------------------------------------------------------
}
constructor TFileUpgrader.Create(ASettings: TSettings);
begin
  inherited Create;
  
  FFolder := IncludeTrailingPathDelimiter(ASettings.InstallationPath);
  // Prepare Unzip component and set a few properties.
  FUnzipper := TVCLUnzip.Create(nil);
  with FUnzipper do begin
    OnTotalPercentDone := ZipTotalPercentDone;
    DestDir := FFolder;
    DoAll := True;
    RecreateDirs := True;
    RetainAttributes := False;
    OverwriteMode := Always;
  end;
end;  // TFileUpgrader.Create 

{-------------------------------------------------------------------------------
}
destructor TFileUpgrader.Destroy;
begin
  try
    FUnzipper.Free;
  except
    on Exception do;
  end;
  
  inherited;
end;  // TFileUpgrader.Destroy 

{-------------------------------------------------------------------------------
}
procedure TFileUpgrader.CopyFiles(AProgressBar: TProgressBar);
begin
  FProgress := AProgressBar;
  FProgress.Position := 2;
  // Now copy MapServer5 files across.
  with FUnzipper do begin
    ZipName := 'UpgradeFiles.zip';
    try
      UnZip;
    except
      on Exception do;
    end;
  end;
end;  // TFileUpgrader.CopyFiles 

{-------------------------------------------------------------------------------
}
procedure TFileUpgrader.ZipTotalPercentDone(Sender: TObject; Percent: LongInt);
begin
  AppStartCursor;  // Unzip component has hardcoded cursor values!!!
  if Assigned(FProgress) then
    FProgress.Position := Percent;
  Application.ProcessMessages;
end;  // TFileUpgrader.ZipTotalPercentDone 

end.
