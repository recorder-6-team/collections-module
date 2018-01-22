{===============================================================================
  Library:     Occurrence.ocx

  Description: ActiveX addin for Recorder 6 extending Observations screen.

  Created:     October 2003

  Last revision information:
    $Revision: $
    $Date: $
    $Author: $

===============================================================================}

library Occurrences;

uses
  madExcept,
  madLinkDisAsm,
  ComServ,
  Forms,
  ExceptionForm,
  ADODB_TLB                    in '..\..\Other Dependencies\DssVcl32\ADODB_TLB.pas',
  BaseADODataModule            in '..\..\Other Dependencies\DssVcl32\BaseADODataModule.pas' {dmBaseADO: TDataModule},

  Constants                    in '..\..\Other Dependencies\JNCC\Constants.pas',
  Recorder2000_TLB             in '..\..\Other Dependencies\JNCC\Recorder2000_TLB.pas',

  COMClasses                   in '..\..\Other Dependencies\JNCC\Components\COMClasses.pas',
  CompositeComponent           in '..\..\Other Dependencies\JNCC\Components\CompositeComponent.pas',
  DataClasses                  in '..\..\Other Dependencies\JNCC\Components\DataClasses.pas',
  DBListCombo                  in '..\..\Other Dependencies\JNCC\Components\DBListCombo.pas',
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

  BaseCompositeComponent       in '..\..\Other Dependencies\Recorder Addins\Components\BaseCompositeComponent.pas',
  DSSDataTypes                 in '..\..\Other Dependencies\Recorder Addins\Components\DSSDataTypes.pas',
  LinkedControls               in '..\..\Other Dependencies\Recorder Addins\Components\LinkedControls.pas',

  CollectionsModuleManager_TLB in 'CollectionsModuleManager_TLB.pas',
  ThesaurusBrowser_TLB         in 'ThesaurusBrowser_TLB.pas',

  ApplicationSettings          in '..\Common\ApplicationSettings.pas',
  GeneralData                  in '..\Common\GeneralData.pas' {dmGeneral: TDataModule},
  InterfaceDataModule          in '..\Common\InterfaceDataModule.pas' {dmInterface: TDataModule},
  BaseDragFrameUnit            in '..\Common\BaseDragFrameUnit.pas' {BaseDragFrame: TFrame},
  BaseDetailFrameUnit          in '..\Common\BaseDetailFrameUnit.pas' {BaseDetailFrame: TFrame},
  BaseTabSheetFrameUnit        in '..\Common\BaseTabSheetFrameUnit.pas' {BaseTabSheetFrame: TFrame},
  BasePageControlFrameUnit     in '..\Common\BasePageControlFrameUnit.pas' {BasePageControlFrame: TFrame},
  DataTypes                    in '..\Common\DataTypes.pas',
  Find                         in '..\Common\Find.pas' {dlgFind: TForm},
  FrameMeasurementsGeneral     in '..\Common\FrameMeasurementsGeneral.pas' {fraMeasurementsGeneral: TFrame},
  FrameDescriptors             in '..\Common\FrameDescriptors.pas' {fraDescriptors: TFrame},
  FrameSources                 in '..\Common\FrameSources.pas' {fraSources: TFrame},
  LuxembourgConstants          in '..\Common\LuxembourgConstants.pas',
  LuxembourgDataClasses        in '..\Common\LuxembourgDataClasses.pas',
  LuxembourgFunctions          in '..\Common\LuxembourgFunctions.pas',
  RegisteredControls           in '..\Common\RegisteredControls.pas',
  ResourceStrings              in '..\Common\ResourceStrings.pas',
  SearchManager                in '..\Common\SearchManager.pas',
  UserMessages                 in '..\Common\UserMessages.pas',
  Validation                   in '..\Common\Validation.pas',

  ConceptGroupComboBox         in '..\Components\ConceptGroupComboBox.pas',
  LuxIDComboBox                in '..\Components\LuxIDComboBox.pas',
  SpatialRef                   in '..\Components\SpatialRef.pas',
  TermLabel                    in '..\Components\TermLabel.pas',

  FrameContainerUnit           in '..\Collections Browser\Source\FrameContainerUnit.pas' {fraContainer: TFrame},
  FrameDeterminationGeneral    in '..\Collections Browser\Source\FrameDeterminationGeneral.pas' {fraDeterminationGeneral: TFrame},
  UserEdit                     in '..\Collections Browser\Source\UserEdit.pas',

  Occurrences_TLB              in 'Occurrences_TLB.pas',
  ContainerFormLoc             in '..\Occurrences\Source\ContainerFormLoc.pas' {frmContainerLoc: TActiveForm} {frmContainerLoc: CoClass},
  ContainerFormOcc             in '..\Occurrences\Source\ContainerFormOcc.pas' {frmContainerOcc: TActiveForm} {frmContainerOcc: CoClass},
  DescriptorTab                in '..\Occurrences\Source\DescriptorTab.pas' {DescriptorTab: TActiveForm} {DescriptorTab: CoClass},
  FrameOccurrence              in '..\Occurrences\Source\FrameOccurrence.pas' {fraOccurrence: TFrame},
  FrameOccurrenceGeneral       in '..\Occurrences\Source\FrameOccurrenceGeneral.pas' {fraOccurrenceGeneral: TFrame},
  FrameRelatedOccurrences      in '..\Occurrences\Source\FrameRelatedOccurrences.pas' {fraRelatedOccurrences: TFrame},
  FrameSpecimens               in '..\Occurrences\Source\FrameSpecimens.pas' {fraSpecimens: TFrame},
  FrameDeterminationOccurrence in '..\Occurrences\Source\FrameDeterminationOccurrence.pas' {fraDetermination: TFrame},
  FrameMeasurementsLocation    in '..\Occurrences\Source\FrameMeasurementsLocation.pas' {fraMeasurementsLoc: TFrame},
  FrameMeasurementsOccurrence  in '..\Occurrences\Source\FrameMeasurementsOccurrence.pas' {fraMeasurementsOcc: TFrame},
  LocationNodes                in '..\Occurrences\Source\LocationNodes.pas' {OccurrenceNodes: CoClass},
  OccurrenceNodes              in '..\Occurrences\Source\OccurrenceNodes.pas' {OccurrenceNodes: CoClass},
  SpecimenTab                  in '..\Occurrences\Source\SpecimenTab.pas' {SpecimenTab: TActiveForm} {SpecimenTab: CoClass};

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
  Application.CreateForm(TdmInterface, dmInterface);
  MstErrorMessage := 'An unhandled error has occurred in Recorder %s.  Don''t ' +
                     'worry.  Send this error message to recorder@jncc.gov.uk.';
end.


