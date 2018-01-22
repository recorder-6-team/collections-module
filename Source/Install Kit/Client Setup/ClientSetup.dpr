{===============================================================================
  Program:     ClientSetup.exe

  Description: Registers the libraries implementing the Collections Module's
               Recorder Add-Ins, and installs them as add-ins in Recorder.

               This is necessary because Luxembourg NHM use
               CollectionsModuleClientInstall.exe *once* for each new release
               (to install the libraries into a shared network directory) and
               use this program on individual client machines to register the
               libraries from that shared directory.

               The idea is that when a new version is released they only need
               to upgrade the installation in the shared network directory,
               instead of having to remember to upgrade every client machine.

               (This seems like a Bad Idea, but is apparently the way that they
               have always worked. It used to be done with a .BAT script, but
               that caused problems once UAC showed up in modern versions of
               Windows.)

  Created:     December 2016

===============================================================================}
program ClientSetup;

{$APPTYPE CONSOLE}
{$R *.res}
{$R '..\Client Setup\Manifest.res' '..\Client Setup\Manifest.rc'}

uses
  SysUtils, ComObj, Registry, Windows;

var
  FilesDir: String;
  LibrariesDir: String;

{-------------------------------------------------------------------------------
  Registers the COM server implemented by the specified file.
}
procedure RegisterLibrary(const LibraryPath: String);
begin
  if ExitCode <> 0 then Exit;
  try
    WriteLn('Registering ' + ChangeFileExt(ExtractFileName(LibraryPath), '') + '...');
    RegisterComServer(LibraryPath);
  except
    on E: Exception do
    begin
      WriteLn(ErrOutput, 'Failed to register ' + ExtractFileName(LibraryPath) + '. ' + E.Message);
      ExitCode := 1;
    end;
  end;
end;

{-------------------------------------------------------------------------------
  Registers the specified add-in with Recorder.
}
procedure RegisterAddIn(const Name, ClsID: String);
var
  Registry: TRegistry;
  KeyName: String;
begin
  if ExitCode <> 0 then Exit;

  WriteLn('Registering ' + Name + '...');
  KeyName := 'SOFTWARE\Dorset Software\Recorder 6\Installed Addins\' + Name;

  Registry := TRegistry.Create;
  try
    try
      Registry.RootKey := HKEY_LOCAL_MACHINE;
      Registry.OpenKey(KeyName, True);
      Registry.WriteString('ClsID', ClsID);
      Registry.WriteString('Installed', '1');
    except
      on E: Exception do
      begin
        WriteLn(ErrOutput, 'Failed to register ' + Name + '. ' + E.Message);
        ExitCode := 1;
      end
    end
  finally
    Registry.Free;
  end;
end;

{-------------------------------------------------------------------------------
  Sets a value within the 'Collections Module' registry key.
}
procedure SetCollectionsRegistryValue(const Name, Value: String);
var
  Registry: TRegistry;
begin
  if ExitCode <> 0 then Exit;

  WriteLn('Setting ' + Name + '...');
  Registry := TRegistry.Create;
  try
    try
      Registry.RootKey := HKEY_LOCAL_MACHINE;
      Registry.OpenKey('SOFTWARE\Dorset Software\Collections Module', True);
      Registry.WriteString(Name, Value);
    except
      on E: Exception do
      begin
        WriteLn(ErrOutput, 'Failed to set ' + Name + '. ' + E.Message);
        ExitCode := 1;
      end
    end
  finally
    Registry.Free;
  end;
end;

{-------------------------------------------------------------------------------
  Reads a single character from the standard input stream.
}
function ReadKey: Char;
begin
  Read(Input, Result);
end;


begin
  if ParamCount = 0 then
    FilesDir:=ExtractFileDir(ParamStr(0))
  else
    FilesDir := ExcludeTrailingPathDelimiter(ParamStr(1));

  if not DirectoryExists(FilesDir) then
  begin
    WriteLn(ErrOutput, 'Directory "' + FilesDir + '" does not exist.');
    ExitCode := 1;
  end;

  LibrariesDir := FilesDir + '\Collections Library Files';

  RegisterLibrary(FilesDir + '\CollectionsModuleManager.ocx');
  RegisterLibrary(FilesDir + '\Occurrences.ocx');
  RegisterLibrary(LibrariesDir + '\CollectionsBrowser.ocx');
  RegisterLibrary(LibrariesDir + '\QuickEntry.ocx');
  RegisterLibrary(LibrariesDir + '\SpecimenFinderModule.ocx');
  RegisterLibrary(LibrariesDir + '\StandardReports.dll');
  RegisterLibrary(LibrariesDir + '\ThesaurusBrowser.ocx');
  RegisterLibrary(LibrariesDir + '\UserConfigExtended.ocx');

  RegisterAddIn('CollectionsModuleManager.Collections Module Addin', '{3739181F-25A2-4605-8835-80C423BABA71}');
  RegisterAddIn('Occurrences.DescriptorsTab',                        '{3A308973-7F2A-42CD-847F-E477B7CB350B}');
  RegisterAddIn('Occurrences.Locations',                             '{3993345D-ADF9-411A-A331-FCEE45F3E0FB}');
  RegisterAddIn('Occurrences.Occurrences',                           '{4C975253-5FCF-411B-830B-EE2ECCDDF406}');
  RegisterAddIn('Occurrences.SpecimenTab',                           '{EAF2C7CD-FD14-4599-B697-7FBD306C5209}');

  SetCollectionsRegistryValue('Standard Report Template Path', FilesDir + '\Collections Reports\');

  if ExitCode = 0 then
    WriteLn('Setup complete.')
  else
    WriteLn('Setup FAILED.');

  if ParamCount = 0 then
  begin
    // Assume not run from a command prompt
    WriteLn('Press any key to continue...');
    ReadKey;
  end;
end.
