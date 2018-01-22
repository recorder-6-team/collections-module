{===============================================================================
  Library:     CollectionsModuleManager.ocx

  Description: ActiveX component managing other modules for Luxembourg Collections
               addin for Recorder.

  Created:     2003

  Last revision information:
    $Revision: $
    $Date: $
    $Author: $

===============================================================================}

library CollectionsModuleManager;

uses
  madExcept,
  madLinkDisAsm,
  ComServ,
  ExceptionForm,
  ADODB_TLB                    in '..\..\Other Dependencies\DssVcl32\ADODB_TLB.pas',
  BaseADODataModule            in '..\..\Other Dependencies\DssVcl32\BaseADODataModule.pas' {dmBaseADO: TDataModule},

  Recorder2000_TLB             in '..\..\Other Dependencies\JNCC\Recorder2000_TLB.pas',

  DataClasses                  in '..\..\Other Dependencies\JNCC\Components\DataClasses.pas',
  GenFuncs                     in '..\..\Other Dependencies\JNCC\Components\GenFuncs.pas',
  JNCCDatasets                 in '..\..\Other Dependencies\JNCC\Components\JNCCDatasets.pas',
  SpatialRefFuncs              in '..\..\Other Dependencies\JNCC\Components\SpatialRefFuncs.pas',
  SQLConverter                 in '..\..\Other Dependencies\JNCC\Components\SQLConverter.pas',
  VagueDate                    in '..\..\Other Dependencies\JNCC\Components\VagueDate.pas',

  DSSDataTypes                 in '..\..\Other Dependencies\Recorder Addins\Components\DSSDataTypes.pas',

  CollectionsBrowser_TLB       in 'CollectionsBrowser_TLB.pas',
  CollectionsModuleManager_TLB in 'CollectionsModuleManager_TLB.pas',
  QuickEntry_TLB               in 'QuickEntry_TLB.pas',
  SpecimenFinderModule_TLB     in 'SpecimenFinderModule_TLB.pas',
  ThesaurusBrowser_TLB         in 'ThesaurusBrowser_TLB.pas',
  UserConfigExtended_TLB       in 'UserConfigExtended_TLB.pas',

  ApplicationSettings          in '..\Common\ApplicationSettings.pas',
  DataTypes                    in '..\Common\DataTypes.pas',
  GeneralData                  in '..\Common\GeneralData.pas' {dmGeneral: TDataModule},
  InterfaceDataModule          in '..\Common\InterfaceDataModule.pas' {dmInterface: TDataModule},
  LuxembourgConstants          in '..\Common\LuxembourgConstants.pas',
  ResourceStrings              in '..\Common\ResourceStrings.pas',

  LuxIDComboBox                in '..\Components\LuxIDComboBox.pas',

  QENumberOfSpecimens          in '..\Quick Entry\Source\QENumberOfSpecimens.pas' {dlgQENumberOfSpecimens: TForm},
  
  CollectionsBrowserMenuItem   in '..\Collections Module Manager\Source\CollectionsBrowserMenuItem.pas' {CollectionsBrowserMenuItem: CoClass},
  CollectionsMenus             in '..\Collections Module Manager\Source\CollectionsMenus.pas' {CollectionsMenus: CoClass},
  CollectionsModuleSettings    in '..\Collections Module Manager\Source\CollectionsModuleSettings.pas',
  CollectionsOptionsPage       in '..\Collections Module Manager\Source\CollectionsOptionsPage.pas' {CollectionsOptionsPage: TActiveForm} {CollectionsOptionsPage: CoClass},
  ExtendedUserConfigMenuItem   in '..\Collections Module Manager\Source\ExtendedUserConfigMenuItem.pas' {ExtendedUserConfigMenuItem: CoClass},
  FilterOptionsFrame           in '..\Collections Module Manager\Source\FilterOptionsFrame.pas' {fraFilterOptions: TFrame},
  GeneralOptionsFrame          in '..\Collections Module Manager\Source\GeneralOptionsFrame.pas' {fraGeneralOptionsFrame: TFrame},
  LoginScreen                  in '..\Collections Module Manager\Source\LoginScreen.pas' {frmLoginScreen},
  MenuImages                   in '..\Collections Module Manager\Source\MenuImages.pas' {dmMenuImages: TDataModule},
  NumberMacroOptionsFrame      in '..\Collections Module Manager\Source\NumberMacroOptionsFrame.pas' {fraNumberMacroOptions: TFrame},
  QuickEntryMenuItem           in '..\Collections Module Manager\Source\QuickEntryMenuItem.pas' {QuickEntryMenuItem: CoClass},
  ReportSeparator              in '..\Collections Module Manager\Source\ReportSeparator.pas' {ReportSeparator: CoClass},
  SpecimenFinderMenuItem       in '..\Collections Module Manager\Source\SpecimenFinderMenuItem.pas' {SpecimenFinderMenuItem: CoClass},
  ThesaurusBrowserMenuItem     in '..\Collections Module Manager\Source\ThesaurusBrowserMenuItem.pas' {ThesaurusBrowserMenuItem: CoClass};

{$E ocx}

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
