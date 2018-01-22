{===============================================================================
  Library:     StandardReports.dll

  Description:

  Created:     October 2003

  Last revision information:
    $Revision: $
    $Date: $
    $Author: $

===============================================================================}

library StandardReports;

uses
  madExcept,
  madLinkDisAsm,
  ComServ,
  ExceptionForm,
  ADODB_TLB                    in '..\..\Other Dependencies\DssVcl32\ADODB_TLB.pas',

  Recorder2000_TLB             in '..\..\Other Dependencies\JNCC\Recorder2000_TLB.pas',
  GenFuncs                     in '..\..\Other Dependencies\JNCC\Components\Genfuncs.pas',
  VagueDate                    in '..\..\Other Dependencies\JNCC\Components\VagueDate.pas',

  CollectionsModuleManager_TLB in 'CollectionsModuleManager_TLB.pas',

  ApplicationSettings          in '..\Common\ApplicationSettings.pas',
  DataTypes                    in '..\Common\DataTypes.pas',
  GeneralData                  in '..\Common\GeneralData.pas' {dmGeneral: TDataModule},
  InterfaceDataModule          in '..\Common\InterfaceDataModule.pas' {dmInterface: TDataModule},
  LuxembourgConstants          in '..\Common\LuxembourgConstants.pas',
  ResourceStrings              in '..\Common\ResourceStrings.pas',

  StandardReports_TLB          in 'StandardReports_TLB.pas',
  ReportConfiguration          in '..\Reports\Source\ReportConfiguration.pas' {dlgReportConfiguration},
  ReportObjects                in '..\Reports\Source\ReportObjects.pas';

exports
  DllGetClassObject,
  DllCanUnloadNow,
  DllRegisterServer,
  DllUnregisterServer;

{$R *.TLB}

{$R *.RES}

begin
  {$IFDEF madExcept}
  RegisterExceptionHandler(TfrmException.MadExceptionHandler, stTrySyncCallAlways);
  {$ELSE}
  Application.OnException := TfrmException.GlobalExceptionHandler;
  {$ENDIF}
  dmInterface := TdmInterface.Create(nil);
end.
