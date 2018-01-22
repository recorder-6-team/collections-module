{===============================================================================
  Unit:        DatabaseInstallFrame

  Defines:     TfraDatabaseInstall

  Description: Install database and display progress

  Created:     September 2003

  Last revision information:
    $Revision: 6 $
    $Date: 9/01/06 13:07 $
    $Author: Johnvanbreda $

===============================================================================}

unit DatabaseInstallFrame;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, BaseStepFrame, ExtCtrls, StdCtrls, AdoDb, ExceptionForm;

type
  EDatabaseInstall = class(TExceptionPath);

  TfraDatabaseInstall = class(TfraBaseStep)
    Label3: TLabel;
    lblUpgrading: TLabel;
    lblCreating: TLabel;
    lblAdding: TLabel;
    lblImporting: TLabel;
    imgUpgrading: TImage;
    imgCreating: TImage;
    imgAdding: TImage;
    imgImporting: TImage;
    lblNext: TLabel;
    imgTick: TImage;
    imgArrow: TImage;
    lblViews: TLabel;
    imgViews: TImage;
    imgRecentUpdates: TImage;
    lblRecentUpdates: TLabel;
  private
    function CheckForCollectionsTables: Boolean;
    procedure RunScripts(ALabel: TLabel; AImage: TImage; const AText: String;
        AScripts: Array of String);
    procedure RunStoredProc(ALabel: TLabel; AImage: TImage; const AText: String;
      const AStoredProcName: String; AParams: array of Variant);
    procedure SetFeedBack(ALabel: TLabel; AImage: TImage;
      AActive: Boolean);
    function RunScriptsFromFolder(ALabel: TLabel; AImage: TImage; const AText: 
        String; AScriptsFolder: string): string;
    procedure InitCommandForQuery(AQuery: TADOCommand);
  protected
    function GetNext: TStepFrameClass; override;
  public
    function Execute: Boolean; override;
  end;

//==============================================================================
implementation

{$R *.dfm}

uses
  GeneralData, Settings, OrgDetailsFrame, GeneralFunctions;

{-==============================================================================
    TfraDatabaseInstall
===============================================================================}
{-------------------------------------------------------------------------------
}
function TfraDatabaseInstall.CheckForCollectionsTables: Boolean;
begin
  with TADOCommand.Create(nil) do
    try
      Connection := dmGeneral.Connection;
      CommandType := cmdText;
      CommandTimeout := 0;
      CommandText := 'SELECT * FROM SysObjects WHERE Name = ''Collection_Unit''';
      with Execute do begin
        Result := not Eof;
        Close;
      end;
    finally
      Free;
    end;
end;  // TfraDatabaseInstall.CheckForCollectionsTables

{-------------------------------------------------------------------------------
}
function TfraDatabaseInstall.Execute: Boolean;
var
  lLatestScript: string;
  lQuery: TADOCommand;
begin
  if CheckForCollectionsTables then begin
    MessageDlg('The Collections database has already been installed on the server.',
               mtInformation, [mbOk], 0);
    Result := True;
  end else begin
    Screen.Cursor := crHourglass;
    try
      // Include script for Import stored proc. Needs to run BEFORE constraints are in place.
      RunScripts(lblCreating, imgCreating, ' Creating...',
                 ['Collections Tables', 'Collections Functions', 'SystemSuppliedDataImport',
                 'Collections Constraint Drops']);
      // Run SysSupplied data import, before the constraints.
      RunStoredProc(lblImporting, imgImporting, ' Importing...',
                    'usp_SystemSuppliedData_BCP_Import', ['@InputPath',
                    ExtractFilePath(Application.ExeName) + 'System Supplied Data\']);
      RunScripts(lblAdding, imgAdding, ' Adding...',
                 ['Collections Constraints And Triggers']);
      RunScripts(lblUpgrading, imgUpgrading, ' Upgrading...',
                 ['NBNData Tables', 'Collections Stored Procedures']);
      RunScripts(lblViews, imgViews, ' Adding...', ['Collections Views']);
      lLatestScript := RunScriptsFromFolder(lblRecentUpdates, imgRecentUpdates, ' Upgrading...',
          ExtractFilePath(Application.Exename) + 'Scripts\Upgrades\*.sql');
      lQuery := TADOCommand.Create(nil);
      try
        InitCommandForQuery(lQuery);
        lQuery.CommandText := 'INSERT INTO Setting ([Name], Data) '+
            'VALUES (''CM DB Seq'', '''+lLatestScript + ''')';
        lQuery.Execute;
      finally
        lQuery.Free;
      end;
      lblNext.Visible := True;
    finally
      Screen.Cursor := crDefault;
      Result := True;
    end;
  end;
end;  // TfraDatabaseInstall.Execute

{-------------------------------------------------------------------------------
}
function TfraDatabaseInstall.GetNext: TStepFrameClass;
begin
  Result := TfraOrgDetails;
end;  // TfraDatabaseInstall.GetNext

{-------------------------------------------------------------------------------
}
procedure TfraDatabaseInstall.RunScripts(ALabel: TLabel; AImage: TImage; const
    AText: String; AScripts: Array of String);
var i, lLine: Integer;
    lScriptContent: TStringList;
    lScriptPart: TStringList;
    lQuery: TADOCommand;
    lCaption: String;

      procedure ExecuteScriptPart;
      begin
        lQuery.CommandText := 'SET ANSI_NULLS ON';
        lQuery.Execute;
        lQuery.CommandText := lScriptPart.Text;
        try
          lQuery.Execute;
        except
          on E:Exception do begin
            lScriptPart.SaveToFile(GetWindowsTempDir + 'ScriptPart.sql');
            raise EDatabaseInstall.Create('Error in file ' +  AScripts[i] + '.sql'
                + ' in script ' + lScriptPart.Text, E);
          end;
        end;
        lScriptPart.Clear;
      end;
begin
  // Update screen
  SetFeedback(ALabel, AImage, True);
  lCaption := ALabel.Caption;
  ALabel.Caption := lCaption + AText;
  // Run script(s)
  lScriptContent := TStringList.Create;
  lScriptPart := TStringList.Create;
  lQuery := TADOCommand.Create(nil);
  try
    InitCommandForQuery(lQuery);
    lQuery.CommandText := 'SET ARITHABORT ON';
    lQuery.Execute;
    for i := 0 to High(AScripts) do begin
      lScriptContent.LoadFromFile(ExtractFilePath(Application.ExeName) + 'Scripts\' +
                                  AScripts[i] + '.sql');
      lScriptPart.Clear;
      for lLine := 0 to lScriptContent.Count - 1 do
        if CompareText(Trim(lScriptContent[lLine]), 'GO') = 0 then begin
          ExecuteScriptPart;
          Application.ProcessMessages;
        end else
          lScriptPart.Add(lScriptContent[lLine]);
      if lScriptPart.Count > 0 then
        ExecuteScriptPart;
      Application.ProcessMessages;
    end;
  finally
    lQuery.Free;
    lScriptContent.Free;
    lScriptPart.Free;
  end;
  // Update screen
  SetFeedback(ALabel, AImage, False);
  ALabel.Caption := lCaption + ' Done.';
end;  // TfraDatabaseInstall.RunScripts

{-------------------------------------------------------------------------------
}
procedure TfraDatabaseInstall.RunStoredProc(ALabel: TLabel; AImage: TImage;
  const AText: String; const AStoredProcName: String; AParams: Array of Variant);
var
  lCaption: String;
begin
  // Update screen
  SetFeedback(ALabel, AImage, True);
  lCaption := ALabel.Caption;
  ALabel.Caption := lCaption + AText;

  dmGeneral.RunStoredProc(AStoredProcName, AParams);

  // Update screen
  SetFeedback(ALabel, AImage, False);
  ALabel.Caption := lCaption + ' Done.';
  Sleep(500);
end;

{-------------------------------------------------------------------------------
}
procedure TfraDatabaseInstall.SetFeedBack(ALabel: TLabel; AImage: TImage; AActive: Boolean);
begin
  if AActive then begin
    ALabel.Font.Style := [fsBold];
    AImage.Picture.Assign(imgArrow.Picture);
  end else begin
    ALabel.Font.Style := [];
    AImage.Picture.Assign(imgTick.Picture);
  end;
  Refresh;
end;  // TfraDatabaseInstall.SetFeedBack

{-------------------------------------------------------------------------------
  Run a batch of upgrade scripts from a folder, returns the latest run script
}
function TfraDatabaseInstall.RunScriptsFromFolder(ALabel: TLabel; AImage:
    TImage; const AText: String; AScriptsFolder: string): string;
var
  f: TSearchRec;
  lFiles: array of string;
  lFilesStrings: TStringList;
  i: integer;
begin
  lFilesStrings := TStringList.Create;
  try
    if FindFirst(AScriptsFolder, 0, f) = 0 then begin
      repeat
        lFilesStrings.Add(f.Name);
      until FindNext(f)<>0;
      FindClose(f);
      lFilesStrings.Sort;
      SetLength(lFiles, lFilesStrings.Count);
      for i := 0 to lFilesStrings.Count-1 do
        lFiles[i] := 'Upgrades\' + ExtractWithoutExt(lFilesStrings[i]);
      RunScripts(ALabel, AImage, AText, lFiles);
    end;
  finally
    Result := ExtractWithoutExt(lFilesStrings[lFilesStrings.Count-1]);
    lFilesStrings.Free;
  end;
end;

{-------------------------------------------------------------------------------
  Prepare a command object to run SQL text
}
procedure TfraDatabaseInstall.InitCommandForQuery(AQuery: TADOCommand);
begin
  AQuery.ParamCheck := False;
  AQuery.Connection := dmGeneral.Connection;
  AQuery.CommandType := cmdText;
  AQuery.CommandTimeout := 0;
  AQuery.CommandText := 'SET QUOTED_IDENTIFIER ON';
  AQuery.Execute;
end;

end.
