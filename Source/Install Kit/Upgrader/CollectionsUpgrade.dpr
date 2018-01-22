program CollectionsUpgrade;

uses
  Forms,
  Windows,
  Dialogs,
  SysUtils,
  Messages,

  ApiUtils          in '..\..\Other Dependencies\DssVcl32\ApiUtils.pas',
  ExceptionForm     in '..\..\Other Dependencies\DssVcl32\ExceptionForm.pas' {frmException: TfrmException},
  GeneralFunctions  in '..\..\Other Dependencies\DssVcl32\GeneralFunctions.pas',
  ListDlls          in '..\..\Other Dependencies\DssVcl32\ListDlls.pas',
  VersionInfo       in '..\..\Other Dependencies\DssVcl32\VersionInfo.pas',

  Main              in 'Source\Main.pas' {frmMain},
  BaseFrameUnit     in 'Source\BaseFrameUnit.pas' {BaseFrame: TFrame},
  LoginFrame        in 'Source\LoginFrame.pas' {fraLogin: TFrame},
  UpgradeFrame      in 'Source\UpgradeFrame.pas' {fraUpgrade: TFrame},
  DBUpgrader        in 'Source\DBUpgrader.pas',
  FileUpgrader      in 'Source\FileUpgrader.pas',
  Settings          in 'Source\Settings.pas';

{$R *.res}

var
  MutHandle: THandle = 0;
begin
  // Check for previous instance of Recorder 2000
  MutHandle := OpenMutex(MUTEX_ALL_ACCESS, False, 'Recorder 2000');
  // If not running, all fine, else exit
  if MutHandle = 0 then
    MutHandle := CreateMutex(nil, True, 'Recorder 2000')
  else begin
    MessageDlg('Recorder is already running on this machine.  Please close the application'#13 +
               'before running the upgrade.', mtWarning, [mbOk], 0);
    if ParamCount >= 1 then
      PostMessage(StrToInt64(Copy(ParamStr(1), 2, 255)), WM_CLOSE, 0, 0);
    Exit;
  end;

  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
