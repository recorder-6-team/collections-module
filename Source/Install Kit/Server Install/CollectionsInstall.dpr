{===============================================================================
  Program:     CollectionsInstall

  Description: Install wizard for Collections Addin for Recorder.

  Created:     September 2003

===============================================================================}

program CollectionsInstall;

uses
  Forms,

  ADODB_TLB                    in '..\..\Other Dependencies\DssVcl32\ADODB_TLB.pas',
  ApiUtils                     in '..\..\Other Dependencies\DssVcl32\ApiUtils.pas',
  BaseADODataModule            in '..\..\Other Dependencies\DssVcl32\BaseADODataModule.pas' {dmBaseADO: TdmBaseADO},
  ExceptionForm                in '..\..\Other Dependencies\DssVcl32\ExceptionForm.pas' {frmException: TfrmException},
  GeneralFunctions             in '..\..\Other Dependencies\DssVcl32\GeneralFunctions.pas',
  ListDlls                     in '..\..\Other Dependencies\DssVcl32\ListDlls.pas',
  VersionInfo                  in '..\..\Other Dependencies\DssVcl32\VersionInfo.pas',

  VagueDate                    in '..\..\Other Dependencies\JNCC\Components\VagueDate.pas',
  GenFuncs                     in '..\..\Other Dependencies\JNCC\Components\Genfuncs.pas',
  Recorder2000_TLB             in '..\..\Other Dependencies\JNCC\Recorder2000_TLB.pas',

  CollectionsModuleManager_TLB in '..\..\Collections Module\Build Projects\CollectionsModuleManager_TLB.pas',
  ApplicationSettings          in '..\..\Collections Module\Common\ApplicationSettings.pas',
  DataTypes                    in '..\..\Collections Module\Common\DataTypes.pas',
  GeneralData                  in '..\..\Collections Module\Common\GeneralData.pas' {dmGeneral: TDataModule},
  LuxembourgConstants          in '..\..\Collections Module\Common\LuxembourgConstants.pas',
  ResourceStrings              in '..\..\Collections Module\Common\ResourceStrings.pas',
  BarCode                      in '..\..\Collections Module\Components\BarCode.pas',
  BcChkSum                     in '..\..\Collections Module\Components\BcChkSum.pas',

  BarcodeFrame                 in 'Source\BarcodeFrame.pas' {fraBarcode: TFrame},
  BaseStepFrame                in 'Source\BaseStepFrame.pas' {fraBaseStep: TFrame},
  CompleteFrame                in 'Source\CompleteFrame.pas' {fraComplete: TFrame},
  DatabaseFrame                in 'Source\DatabaseFrame.pas' {fraDatabase: TFrame},
  DatabaseInstallFrame         in 'Source\DatabaseInstallFrame.pas' {fraDatabaseInstall: TFrame},
  DataSetupFrame               in 'Source\DataSetupFrame.pas' {fraDataSetup: TFrame},
  JobSeedFrame                 in 'Source\JobSeedFrame.pas' {fraJobSeed: TFrame},
  LanguagesFrame               in 'Source\LanguagesFrame.pas' {fraLanguages: TFrame},
  OrgDetailsFrame              in 'Source\OrgDetailsFrame.pas' {fraOrgDetails: TFrame},
  Main                         in 'Source\Main.pas' {frmMain},
  Settings                     in 'Source\Settings.pas',
  ServerDetailsFrame           in 'Source\ServerDetailsFrame.pas' {fraServerDetails: TFrame},
  WelcomeFrame                 in 'Source\WelcomeFrame.pas' {fraWelcome: TFrame},
  
  XPMenu                       in '..\..\Other Dependencies\Third-Party\XPMenu\XPMenu.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.OnException := TfrmException.GlobalExceptionHandler;
  MstErrorMessage := 'An unhandled error has occurred in Recorder.  Don''t ' +
                     'worry.  Send this error message to recorder@jncc.gov.uk.';
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
