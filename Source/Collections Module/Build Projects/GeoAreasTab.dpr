{===============================================================================
  Library:     GeoAreasTab.ocx

  Description: ActiveX addin for Recorder 6 for additional geographic areas tab
               on the Survey Event form.

  Created:     September 2007

  Last revision information:
    $Revision: $
    $Date: $
    $Author: $

===============================================================================}

library GeoAreasTab;

uses
  madExcept,
  madLinkDisAsm,
  ComServ,
  ADODB_TLB                    in '..\..\Other Dependencies\DssVcl32\ADODB_TLB.pas',

  Recorder2000_TLB             in '..\..\Other Dependencies\JNCC\Recorder2000_TLB.pas',

  DataClasses                  in '..\..\Other Dependencies\JNCC\Components\DataClasses.pas',
  DropSource                   in '..\..\Other Dependencies\JNCC\Components\DropSource.pas',
  DropStruct                   in '..\..\Other Dependencies\JNCC\Components\DropStruct.pas',
  DropTarget                   in '..\..\Other Dependencies\JNCC\Components\DropTarget.pas',
  GenFuncs                     in '..\..\Other Dependencies\JNCC\Components\GenFuncs.pas',
  JNCCDatasets                 in '..\..\Other Dependencies\JNCC\Components\JNCCDatasets.pas',
  JNCCGrid                     in '..\..\Other Dependencies\JNCC\Components\JNCCGrid.pas',
  SpatialRefFuncs              in '..\..\Other Dependencies\JNCC\Components\SpatialRefFuncs.pas',
  SQLConverter                 in '..\..\Other Dependencies\JNCC\Components\SQLConverter.pas',
  VagueDate                    in '..\..\Other Dependencies\JNCC\Components\VagueDate.pas',
  VagueDateEdit                in '..\..\Other Dependencies\JNCC\Components\VagueDateEdit.pas',

  AddinCompositeComponent      in '..\..\Other Dependencies\Recorder Addins\Components\AddinCompositeComponent.pas',
  AddinLinkedControls          in '..\..\Other Dependencies\Recorder Addins\Components\AddinLinkedControls.pas',
  BaseCompositeComponent       in '..\..\Other Dependencies\Recorder Addins\Components\BaseCompositeComponent.pas',
  DSSDataTypes                 in '..\..\Other Dependencies\Recorder Addins\Components\DSSDataTypes.pas',
  LinkedControls               in '..\..\Other Dependencies\Recorder Addins\Components\LinkedControls.pas',

  AddinConstants               in '..\..\Other Dependencies\Recorder Addins\Source\AddinConstants.pas',
  AddinFind                    in '..\..\Other Dependencies\Recorder Addins\Source\AddinFind.pas' {dlgAddinFind},
  AddinGeneralData             in '..\..\Other Dependencies\Recorder Addins\Source\AddinGeneralData.pas' {dmAddinGeneral: TDataModule},
  AddinInterfaceDataModule     in '..\..\Other Dependencies\Recorder Addins\Source\AddinInterfaceDataModule.pas' {dmAddinInterface: TDataModule},
  AddinResourceStrings         in '..\..\Other Dependencies\Recorder Addins\Source\AddinResourceStrings.pas',
  AddinSearchManager           in '..\..\Other Dependencies\Recorder Addins\Source\AddinSearchManager.pas',

  CollectionsModuleManager_TLB in 'CollectionsModuleManager_TLB.pas',

  ApplicationSettings          in '..\Common\ApplicationSettings.pas',
  DataTypes                    in '..\Common\DataTypes.pas',
  GeneralData                  in '..\Common\GeneralData.pas' {dmGeneral: TDataModule},
  LuxembourgConstants          in '..\Common\LuxembourgConstants.pas',
  LuxembourgDataClasses        in '..\Common\LuxembourgDataClasses.pas',
  ResourceStrings              in '..\Common\ResourceStrings.pas',

  GeoAreasTab_TLB              in 'GeoAreasTab_TLB.pas',
  DropControlAssistor          in '..\GeoAreasTab\Source\DropControlAssistor.pas',
  GeoAreasTabImpl              in '..\GeoAreasTab\Source\GeoAreasTabImpl.pas' {GeoAreasTabImpl: TActiveForm},
  ProjectSpecificAccess        in '..\GeoAreasTab\Source\ProjectSpecificAccess.pas';

{$E ocx}

exports
  DllGetClassObject,
  DllCanUnloadNow,
  DllRegisterServer,
  DllUnregisterServer;

{$R *.TLB}

{$R *.RES}

begin
end.
