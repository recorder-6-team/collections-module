unit InstallFrame;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, ExceptionForm;

resourcestring
  ResStr_CopyingFile = 'Copying File: ';
  ResStr_AddinPathMissing = 'The Recorder addin path is missing from the registry'#13#10+
      'Installation cannot proceed.';
  ResStr_InsufficientPrivelegesToRegistry = 'You do not have the required access rights to '+
      'write to the local machine''s registry settings.'#13#10+
      'Installation cannot proceed.';
  ResStr_RegisteringAddins = 'Registering Addins';
  ResStr_AddinFailedToRegister =
      'The addin %s failed to register with Windows.  The error was described as:';

type
  EInstallError = class(TExceptionPath);

  TfraInstall = class(TFrame)
    Label2: TLabel;
    lblInstallPhase: TLabel;
    ProgressBar: TProgressBar;
  private
    FTotalBytes: integer;
    FAddinPath: string;
    FComLibraries: TStringList;
    procedure FileTransfer(AFileList: TStringList; const ADestPath: string);
    procedure CopyFiles;
    procedure RegisterCOMServers;
    procedure CreateAddinRegKeys;
    procedure CreateReportRegSetting;
    procedure InstallFile(const ASource, ADest: String; ABytesCopied: Integer);
    procedure GetFilesIn(AFileList: TStringList; const APath, ADestPath: String;
      const AFilter: String = '*.*');
    procedure CheckFileNotReadOnly(const ASource: string);
  public
    constructor Create(AOwner: TComponent); override;
    procedure DoInstallation;
  end;

//==============================================================================
implementation

{$R *.dfm}

uses
  Main, ComObj, Registry, GeneralFunctions, EasyShell;

const
  CHUNK_SIZE = 65536;

  REG_CMM_KEY = 'CollectionsModuleManager.Collections Module Addin';
  REG_OCC_KEY = 'Occurrences.Occurrences';
  REG_SPECTAB_KEY = 'Occurrences.SpecimenTab';
  REG_LOC_KEY = 'Occurrences.Locations';
  REG_DESCTAB_KEY = 'Occurrences.DescriptorsTab';
  REG_GEOAREA_KEY = 'GeoAreasTab.Geographic Areas Tab';

//==============================================================================
{-------------------------------------------------------------------------------
}
procedure TfraInstall.CopyFiles;
var
  lFileList: TStringList;
  lRegFile: TStringList;
  lRptPathIndex: integer;
begin
  lFileList := TStringList.Create;
  try
    // Count the bytes we are to copy, for progress info
    FTotalBytes := 0;
    GetFilesIn(lFileList, ExtractFilePath(Application.Exename) + 'Files', '');
    FileTransfer(lFileList, FAddinPath);
  finally
    lFileList.Free;
  end;
  // Update the client reg file to contain the correct paths
  if FileExists(ExtractFilePath(Application.Exename) + 'Client.reg')
      and (not FileExists(FAddinPath + 'Client.reg')) then
  begin
    lRegFile := TStringList.Create;
    try
      lRegFile.LoadFromFile(ExtractFilePath(Application.Exename) + 'Client.reg');
      lRptPathIndex := lRegFile.IndexOf('"Standard Report Template Path"="%s"');
      if lRptPathIndex > -1 then
        lRegFile[lRptPathIndex] := Format(lRegFile[lRptPathIndex],
            [DuplicateCharacters(ExpandLongPathName(FAddinPath) + 'Collections Reports', '\')]);
      lRegFile.SaveToFile(FAddinPath + 'Client.reg');
    finally
      lRegFile.Free;
    end;
  end;
end;

{-------------------------------------------------------------------------------
}
procedure TfraInstall.GetFilesIn(AFileList: TStringList; const APath, ADestPath: String;
  const AFilter: String = '*.*');
var
  lSearchRec: TSearchRec;
begin
  if FindFirst(APath + '\' + AFilter, faReadOnly + faDirectory, lSearchRec) = 0 then
    repeat
      // Ignore DOS directory maps
      if (lSearchRec.Name <> '.') and (lSearchRec.Name <> '..') then begin
        { for directories, recurse into them }
        if (lSearchRec.Attr and faDirectory) > 0 then begin
          if ADestPath='' then
            GetFilesIn(AFileList, APath + '\' + lSearchRec.Name, lSearchRec.Name)
          else
            GetFilesIn(AFileList, APath + '\' + lSearchRec.Name, ADestPath + '\' + lSearchRec.Name);
        end
        else begin
          { Record the file and size (string/object on stringlist) plus count total }
          FTotalBytes := FTotalBytes + lSearchRec.Size;
          AFileList.AddObject(APath + '\' + lSearchRec.Name + '=' + ADestPath + '\' + lSearchRec.Name,
                              Ptr(lSearchRec.Size));
          if (CompareText(ExtractFileExt(lSearchRec.Name), '.ocx')=0) or
             (CompareText(ExtractFileExt(lSearchRec.Name), '.dll')=0) then
            FComLibraries.Add(ADestPath + '\' + lSearchRec.Name);
        end;
      end;
    until FindNext(lSearchRec) <> 0;
  // Clean up.
  FindClose(lSearchRec);
end;

{-------------------------------------------------------------------------------
}
procedure TfraInstall.CreateAddinRegKeys;
var
  lReg: TRegistry;

  procedure SetRegistryKeyForAddin(const AKey, AGuid: String);
  begin
    if lReg.OpenKey(REG_ADDIN_PATH + '\' + AKey, True) then begin
      lReg.WriteString('ClsID', AGuid);
      lReg.WriteString('Installed', '1');
    end else
      raise EInstallError.Create(ResStr_InsufficientPrivelegesToRegistry);
  end;

begin
  // Perform Recorder registration
  lReg := TRegistry.Create;
  with lReg do
    try
      RootKey := HKEY_LOCAL_MACHINE;
      if OpenKey(REG_ADDIN_PATH, False) then begin
        SetRegistryKeyForAddin(REG_CMM_KEY, '{3739181F-25A2-4605-8835-80C423BABA71}');
        SetRegistryKeyForAddin(REG_OCC_KEY, '{4C975253-5FCF-411B-830B-EE2ECCDDF406}');
        SetRegistryKeyForAddin(REG_SPECTAB_KEY, '{EAF2C7CD-FD14-4599-B697-7FBD306C5209}');
        SetRegistryKeyForAddin(REG_LOC_KEY, '{3993345D-ADF9-411A-A331-FCEE45F3E0FB}');
        SetRegistryKeyForAddin(REG_DESCTAB_KEY, '{3A308973-7F2A-42CD-847F-E477B7CB350B}');
        SetRegistryKeyForAddin(REG_GEOAREA_KEY, '{B9558F7A-09C1-494C-AE5A-208FF9BE3480}');
      end else
    finally
      Free;
    end; // try
end;

{-------------------------------------------------------------------------------
}
procedure TfraInstall.CreateReportRegSetting;
begin
  with TRegistry.Create do
    try
      RootKey := HKEY_LOCAL_MACHINE;
      if OpenKey('Software\Dorset Software\Collections Module', True) then
        WriteString('Standard Report Template Path', FAddinPath + 'Collections Reports\')
      else
        raise EInstallError.Create(ResStr_InsufficientPrivelegesToRegistry);
    finally
      Free;
    end;  // try
end;

{-------------------------------------------------------------------------------
}
procedure TfraInstall.DoInstallation;
begin
  CopyFiles;
  CreateAddinRegKeys;
  CreateReportRegSetting;
  RegisterCOMServers;
end;

{-------------------------------------------------------------------------------
}
procedure TfraInstall.FileTransfer(AFileList: TStringList; const ADestPath: string);
var
  i: integer;
  lBytesCopied: integer;
begin
  lBytesCopied := 0;
  with AFileList do
    for i := 0 to Count - 1 do begin
      lblInstallPhase.Caption := ResStr_CopyingFile + ExtractFileName(Values[Names[i]]);
      ForceDirectories(ExtractFilePath(ADestPath + Values[Names[i]]));
      Application.ProcessMessages;
      InstallFile(Names[i], ADestPath + Values[Names[i]], lBytesCopied);
      Inc(lBytesCopied, Integer(Objects[i]));
      ProgressBar.Position := Round((lBytesCopied / FTotalBytes) * 100);
    end;
end;

{-------------------------------------------------------------------------------
}
procedure TfraInstall.InstallFile(const ASource, ADest: String; ABytesCopied: Integer);
var lSource, lDest: TFileStream;
begin
  lSource := TFileStream.Create(ASource, fmOpenRead);
  try
    if FileExists(ADest) then
      lDest := TFileStream.Create(ADest, fmOpenWrite + fmShareExclusive)
    else
      lDest := TFileStream.Create(ADest, fmCreate);
  except
    on E:EFOpenError do begin
      lSource.Free;
      raise EInstallError.Create('Error occurred copying file ' + ASource + #13#10 +
                                 E.Message);
    end;
  end;
  try
    if (FileGetDate( lSource.Handle ) > FileGetDate( lDest.Handle )) or
       (lSource.Size <> lDest.Size) then
    begin
      CheckFileNotReadOnly(ADest);
      while lSource.Size - lSource.Position > CHUNK_SIZE do begin
        lDest.Copyfrom(lSource, CHUNK_SIZE);
        ProgressBar.Position := Round(((ABytesCopied + lSource.Position) / FTotalBytes) * 100);
      end; // while
      // Copy the final incomplete chunk
      lDest.Copyfrom(lSource, lSource.Size - lSource.Position);
      lDest.Size := lDest.Position; // in case the original file was larger
    end; // if
  finally
    lSource.Free;
    lDest.Free;
  end; // try
end;

{-------------------------------------------------------------------------------
  Checks if the file is read only.  If so, sets it to read_write.
}
procedure TfraInstall.CheckFileNotReadOnly(const ASource: string);
var
  lAttr: integer;
begin
  lAttr := FileGetAttr(ASource);
  if lAttr and faReadOnly > 0 then begin
    lAttr := lAttr - faReadOnly;
    FileSetAttr(ASource, lAttr);
  end;
end;

{-------------------------------------------------------------------------------
}
procedure TfraInstall.RegisterCOMServers;
var
  lIdx: integer;
begin
  // Perform Windows registration
  lblInstallPhase.Caption := ResStr_RegisteringAddins;
  ProgressBar.Position := 0;
  for lIdx := 0 to FComLibraries.Count-1 do begin
    try
      RegisterComServer(FAddinPath + FComLibraries[lIdx]);
    except
      on E:Exception do
        ShowInformation(Format(ResStr_AddinFailedToRegister,
                        [FComLibraries[lIdx]]) + #13#10 + E.Classname + ': ' + E.Message);
    end; // try
    ProgressBar.Position := 100 * (lIdx+1) div FComLibraries.Count;
    Application.ProcessMessages;
  end;
end;

{-------------------------------------------------------------------------------
}
constructor TfraInstall.Create(AOwner: TComponent);
begin
  inherited;
  with TRegistry.Create do
    try
      RootKey := HKEY_LOCAL_MACHINE;
      if OpenKeyReadOnly('Software\Dorset Software\Recorder 6') then
        FAddinPath := ReadString('Addin Path')
      else
        raise EInstallError.Create(ResStr_AddinPathMissing);
    finally
      Free;
    end; // try
  FComLibraries := TStringList.Create;
end;

end.
