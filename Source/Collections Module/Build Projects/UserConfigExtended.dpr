{===============================================================================
  Library:     UserConfigExtended.ocx

  Description:

  Created:     January 2004

  Last revision information:
    $Revision: $
    $Date: $
    $Author: $

===============================================================================}

library UserConfigExtended;

uses
  madExcept, 
  madLinkDisAsm,
  ComServ,
  Forms,
  ExceptionForm,
  ADODB_TLB                    in '..\..\Other Dependencies\DssVcl32\ADODB_TLB.pas',

  Recorder2000_TLB             in '..\..\Other Dependencies\JNCC\Recorder2000_TLB.pas',

  DataClasses                  in '..\..\Other Dependencies\JNCC\Components\DataClasses.pas',
  GenFuncs                     in '..\..\Other Dependencies\JNCC\Components\Genfuncs.pas',
  JNCCDatasets                 in '..\..\Other Dependencies\JNCC\Components\JNCCDatasets.pas',
  SpatialRefFuncs              in '..\..\Other Dependencies\JNCC\Components\SpatialRefFuncs.pas',
  SQLConverter                 in '..\..\Other Dependencies\JNCC\Components\SQLConverter.pas',
  VagueDate                    in '..\..\Other Dependencies\JNCC\Components\VagueDate.pas',

  DSSDataTypes                 in '..\..\Other Dependencies\Recorder Addins\Components\DSSDataTypes.pas',
  
  CollectionsModuleManager_TLB in 'CollectionsModuleManager_TLB.pas',

  ApplicationSettings          in '..\Common\ApplicationSettings.pas',
  DataTypes                    in '..\Common\DataTypes.pas',
  GeneralData                  in '..\Common\GeneralData.pas' {dmGeneral: TDataModule},
  InterfaceDataModule          in '..\Common\InterfaceDataModule.pas' {dmInterface: TDataModule},
  LuxembourgConstants          in '..\Common\LuxembourgConstants.pas',
  ResourceStrings              in '..\Common\ResourceStrings.pas',

  UserConfigExtended_TLB       in 'UserConfigExtended_TLB.pas',
  UCEMain                      in '..\User Configuration Extended\Source\UCEMain.pas' {frmUCEMain: TActiveForm} {frmUCEMain: CoClass};

{$E ocx}

{$R *.TLB}

exports
  DllGetClassObject,
  DllCanUnloadNow,
  DllRegisterServer,
  DllUnregisterServer;

{$R *.RES}

begin
  {$IFDEF madExcept}
  RegisterExceptionHandler(TfrmException.MadExceptionHandler, stTrySyncCallAlways);
  {$ELSE}
  Application.OnException := TfrmException.GlobalExceptionHandler;
  {$ENDIF}
  Application.CreateForm(TdmInterface, dmInterface);
end.

