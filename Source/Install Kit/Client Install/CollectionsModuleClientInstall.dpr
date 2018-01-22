program CollectionsModuleClientInstall;

uses
  Forms,
  Registry,
  Windows,

  ApiUtils         in '..\..\Other Dependencies\DssVcl32\ApiUtils.pas',
  EasyShell        in '..\..\Other Dependencies\DssVcl32\EasyShell.pas',
  ExceptionForm    in '..\..\Other Dependencies\DssVcl32\ExceptionForm.pas' {frmException: TfrmException},
  GeneralFunctions in '..\..\Other Dependencies\DssVcl32\GeneralFunctions.pas',
  ListDlls         in '..\..\Other Dependencies\DssVcl32\ListDlls.pas',
  VersionInfo      in '..\..\Other Dependencies\DssVcl32\VersionInfo.pas',

  main in 'Source\main.pas' {frmMain},
  InstallFrame in 'Source\InstallFrame.pas' {fraInstall: TFrame},
  WelcomeFrame in 'Source\WelcomeFrame.pas' {fraWelcome: TFrame},
  CompleteFrame in 'Source\CompleteFrame.pas' {fraComplete: TFrame},
  XPMenu in '..\..\Other Dependencies\Third-Party\XPMenu\XPMenu.pas';

{$R *.res}

resourcestring
  ResStr_RecorderNotInstalled = 'Recorder is not installed on this machine.  Collections' +
      ' Module installation cannot proceed.';

var
  lRecorderInstalled: boolean;

begin
  Application.Initialize;
  with TRegistry.Create do
    try
      RootKey := HKEY_LOCAL_MACHINE;
      lRecorderInstalled := OpenKey(REG_ADDIN_PATH, False);
    finally
      Free;
    end;
  if lRecorderInstalled then begin
    Application.Title := 'Collections Module Installation';
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
  end
  else
    ShowInformation(ResStr_RecorderNotInstalled);
end.
