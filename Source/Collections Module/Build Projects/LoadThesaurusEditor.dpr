program LoadThesaurusEditor;

uses
  Forms,

  ApiUtils         in '..\..\Other Dependencies\DssVcl32\ApiUtils.pas',
  GeneralFunctions in '..\..\Other Dependencies\DssVcl32\GeneralFunctions.pas',
  ListDlls         in '..\..\Other Dependencies\DssVcl32\ListDlls.pas',
  VersionInfo      in '..\..\Other Dependencies\DssVcl32\VersionInfo.pas',

  main          in '..\Thesaurus Editor Splash Screen\Source\main.pas' {frmMain},
  errormessages in '..\Thesaurus Editor Splash Screen\Source\errormessages.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
