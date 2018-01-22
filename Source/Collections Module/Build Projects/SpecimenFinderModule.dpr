{===============================================================================
  Library:     SpecimenFinderModule.ocx

  Description:

  Created:     October 2003
===============================================================================}

library SpecimenFinderModule;

uses
  madExcept,
  madLinkDisAsm,
  ComServ,
  ExceptionForm,

  ADODB_TLB                    in '..\..\Other Dependencies\DssVcl32\ADODB_TLB.pas',

  XPMenu                       in '..\..\Other Dependencies\Third-Party\XPMenu\XPMenu.pas',

  Recorder2000_TLB             in '..\..\Other Dependencies\JNCC\Recorder2000_TLB.pas',
  COMClasses                   in '..\..\Other Dependencies\JNCC\Components\COMClasses.pas',
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

  CollectionsModuleManager_TLB in 'CollectionsModuleManager_TLB.pas',

  ApplicationSettings          in '..\Common\ApplicationSettings.pas',
  BaseDragFrameUnit            in '..\Common\BaseDragFrameUnit.pas' {BaseDragFrame: TFrame},
  CommonNameFetchQueue         in '..\Common\CommonNameFetchQueue.pas',
  DataTypes                    in '..\Common\DataTypes.pas',
  DragDropControlAssistor      in '..\Common\DragDropControlAssistor.pas',
  GeneralData                  in '..\Common\GeneralData.pas' {dmGeneral: TDataModule},
  InterfaceDataModule          in '..\Common\InterfaceDataModule.pas' {dmInterface: TDataModule},
  LuxembourgConstants          in '..\Common\LuxembourgConstants.pas',
  ResourceStrings              in '..\Common\ResourceStrings.pas',
  UserMessages                 in '..\Common\UserMessages.pas',

  SpecimenFinderModule_TLB     in 'SpecimenFinderModule_TLB.pas',
  SpecimenFinder               in '..\Specimen Finder\Source\SpecimenFinder.pas' {frmSpecimenFinder: TActiveForm} {SpecimenFinder: CoClass},
  SpecimenFinderDragFrame      in '..\Specimen Finder\Source\SpecimenFinderDragFrame.pas' {fraSpecimenFinderDragFrame: TFrame},
  SpecimenFinderSQLData        in '..\Specimen Finder\Source\SpecimenFinderSQLData.pas',
  BatchAutoNumber              in '..\Specimen Finder\Source\BatchAutoNumber.pas' {dlgBatchAutoNumber: TForm},
  ICriterionUnit               in '..\Specimen Finder\Source\ICriterionUnit.pas',
  DragAndDropCriterion         in '..\Specimen Finder\Source\DragAndDropCriterion.pas',
  BooleanCriterionUnit         in '..\Specimen Finder\Source\BooleanCriterionUnit.pas',
  BooleanProperty              in '..\Specimen Finder\Source\BooleanProperty.pas',
  PreviousResultsCriterion     in '..\Specimen Finder\Source\PreviousResultsCriterion.pas',
  SpecimenFinderQueryMenu      in '..\Specimen Finder\Source\SpecimenFinderQueryMenu.pas';

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
