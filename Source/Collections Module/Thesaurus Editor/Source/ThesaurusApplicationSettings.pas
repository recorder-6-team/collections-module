{===============================================================================
  Unit:        ThesaurusApplicationSettings.pas

  Defines:     TThesaurusApplicationSettings

  Description: Application Settings for the Thesaurus Editor. Reads and writes
                data to and from the registry.

  Model:       ThesaurusEditor.mpb

  Created:     September 2004

  Last revision information:
    $Revision: 5 $
    $Date: 18/10/10 17:32 $
    $Author: Robertjohnson $

===============================================================================}

unit ThesaurusApplicationSettings;

interface

uses
  Registry, ExceptionForm, Dialogs;

type
  ELogDeletionError = class(TExceptionPath)
  end;
  

  TThesaurusApplicationSettings = class(TObject)
  private
    FLogDeletions: Boolean;
    FLogFilePath, FRecorderInstallationPath: string;
    FSyncTaxonDictDeletions: Boolean;
    procedure LogDeletionToFile(const AText, APath: string; AParams: Array of
            Variant);
    function ParamsToString(AParams: Array of Variant): string;
    function ReadBoolDefault(ARegistry: TRegistry; const AValue: string;
            ADefault: boolean): Boolean;
    procedure ReadRegistrySettings;
    procedure WriteRegistrySettings;
  public
    constructor Create;
    destructor Destroy; override;
    procedure LogDeletion(AText: String; AParams: Array of Variant;
            AWarningMessage: boolean=false);
    property LogDeletions: Boolean read FLogDeletions write FLogDeletions;
    property LogFilePath: string read FLogFilePath write FLogFilePath;
    property RecorderInstallationPath: string read FRecorderInstallationPath;
    property SyncTaxonDictDeletions: Boolean read FSyncTaxonDictDeletions write
            FSyncTaxonDictDeletions;
  end;
  
function ThesApplicationSettings: TThesaurusApplicationSettings;

//==============================================================================
implementation

uses
  LuxembourgConstants, Windows, SysUtils, Variants, DataTypes, SqlTimSt,
  Classes, GeneralFunctions, ResourceStrings;

var
  mThesaurusApplicationSettings: TThesaurusApplicationSettings;

function ThesApplicationSettings: TThesaurusApplicationSettings;
begin
  if not Assigned(mThesaurusApplicationSettings) then begin
    mThesaurusApplicationSettings := TThesaurusApplicationSettings.Create;
  end;
  Result := mThesaurusApplicationSettings;
end;

{-==============================================================================
    TThesaurusApplicationSettings
===============================================================================}
{-------------------------------------------------------------------------------
  Constructor. 
}
constructor TThesaurusApplicationSettings.Create;
begin
  ReadRegistrySettings;
end;  // TThesaurusApplicationSettings.Create 

{-------------------------------------------------------------------------------
  ThesaurusApplicationSettings is destroyed when the Thesaurus Editor is
          closed. Save the values contained in it to the registry at this point.
}
destructor TThesaurusApplicationSettings.Destroy;
begin
  WriteRegistrySettings;
  inherited;
end;  // TThesaurusApplicationSettings.Destroy 

{-------------------------------------------------------------------------------
  Log a deletion script to the file selected in deletion options.  If
          AWarningMessage is true then the user is warned about failures to
          write to the file.
}
procedure TThesaurusApplicationSettings.LogDeletion(AText: String; AParams:
        Array of Variant; AWarningMessage: boolean=false);
begin
  try
    LogDeletionToFile(AText, ThesApplicationSettings.LogFilePath, AParams);
  except
    on EInOutError do
    begin
      // We didn't have access rights to update the log.  Write to temp folder
      // instead
      try
        LogDeletionToFile(AText, GetWindowsTempDir + ExtractFileName(
            ThesApplicationSettings.LogFilePath), AParams);
        if aWarningMessage then
          MessageDlg(Format(ResStr_DeletionLogToTempDir,
              [ThesApplicationSettings.LogFilePath,
              ExpandLongPathName(GetWindowsTempDir) +
              ExtractFileName(ThesApplicationSettings.LogFilePath)]),
              mtWarning, [mbOk], 0);
      except on EInOutError do
        // raise error so that the deletion is rolled back
        raise ELogDeletionError.CreateNonCritical(Format(ResStr_NoAccessToDeletionLog,
              [ThesApplicationSettings.LogFilePath]));
      end;
    end;
  end; // try
end;  // TThesaurusApplicationSettings.LogDeletion 

{-------------------------------------------------------------------------------
  Logs deletion script to a specific file. 
}
procedure TThesaurusApplicationSettings.LogDeletionToFile(const AText, APath:
        string; AParams: Array of Variant);
var
  lTextFile: TextFile;
begin
  AssignFile(lTextFile, APath);
  try
    if FileExists(APath) then Append(lTextFile) else Rewrite(lTextFile);
    WriteLn(lTextFile, 'exec ' + AText + ' ' + ParamsToString(AParams));
    Flush(lTextFile);
  finally
    CloseFile(lTextFile);
  end;
end;  // TThesaurusApplicationSettings.LogDeletionToFile 

{-------------------------------------------------------------------------------
}
function TThesaurusApplicationSettings.ParamsToString(AParams: Array of
        Variant): string;
var
  lIdx: Integer;
  lVariableName, lValue: Variant;
begin
  Result := '';
  for lIdx := 0 to (Length(AParams) div 2) - 1 do begin
    lVariableName := VarToStr(AParams[(lIdx * 2)]);
    lValue        := VarToStr(AParams[(lIdx * 2) + 1]);
  
    // We aren't interested in the timestamp because it is difficult to convert
    // it into a string in the correct format.
    if lVariableName <> '@Timestamp' then begin
      if lIdx <> 0 then Result := Result + ', ';
      Result := Result + lVariableName + ' = ';
  
      if VarIsNumeric(lValue) then
        Result := Result + lValue
      else if LowerCase(lValue) = 'true' then
        Result := Result + '1'
      else if LowerCase(lValue) = 'false' then
        Result := Result + '0'
      else
        Result := Result + '''' + lValue + '''';
    end;
  end;
end;  // TThesaurusApplicationSettings.ParamsToString 

{-------------------------------------------------------------------------------
  When reading boolean values from the database, if no value exists then assume
          the value is false.
}
function TThesaurusApplicationSettings.ReadBoolDefault(ARegistry: TRegistry;
        const AValue: string; ADefault: boolean): Boolean;
begin
  if ARegistry.ValueExists(AValue) then
    Result := ARegistry.ReadBool(AValue)
  else
    Result := ADefault;
end;  // TThesaurusApplicationSettings.ReadBoolDefault 

{-------------------------------------------------------------------------------
  Read values from the registry 
}
procedure TThesaurusApplicationSettings.ReadRegistrySettings;
var
  lReg: TRegistry;
begin
  lReg := TRegistry.Create;
  
  try
    with lReg do begin
      RootKey := HKEY_CURRENT_USER;
      OpenKeyReadOnly(DELETION_OPTIONS_REG_PATH);
  
      FSyncTaxonDictDeletions := ReadBoolDefault(lReg, OPT_SYNC_TAX_DICT_DELETION, False);
      FLogDeletions := ReadBoolDefault(lReg, OPT_LOG_DELETIONS, False);
      FLogFilePath := ReadString(OPT_SCRIPT_LOG_DELETIONS);

      RootKey := HKEY_LOCAL_MACHINE;
      OpenKeyReadOnly(JNCC_REG_ROOT_PATH);
      if FLogFilePath = '' then begin
        FLogFilePath := ReadString(OPT_RECORDER_INSTALL_PATH) + 'User Files\Deletion Log.sql';
      end;
      FRecorderInstallationPath := ReadString(OPT_RECORDER_INSTALL_PATH);
    end;
  finally
    lReg.Free;
  end;
end;  // TThesaurusApplicationSettings.ReadRegistrySettings 

{-------------------------------------------------------------------------------
  Write values to the registry. 
}
procedure TThesaurusApplicationSettings.WriteRegistrySettings;
begin
  with TRegistry.Create do
    try
      RootKey := HKEY_CURRENT_USER;
      if OpenKey(DELETION_OPTIONS_REG_PATH, True) then begin
        WriteBool(OPT_SYNC_TAX_DICT_DELETION, FSyncTaxonDictDeletions);
        WriteBool(OPT_LOG_DELETIONS, FLogDeletions);
        WriteString(OPT_SCRIPT_LOG_DELETIONS, FLogFilePath);
      end;
    finally
      Free;
    end; // try
end;  // TThesaurusApplicationSettings.WriteRegistrySettings 

end.
