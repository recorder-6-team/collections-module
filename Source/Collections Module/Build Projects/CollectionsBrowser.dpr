{===============================================================================
  Library:     CollectionsBrowser.ocx

  Description: ActiveX component handling the Luxembourg Collections form.

  Created:     2003
===============================================================================}

library CollectionsBrowser;

uses
  madExcept,
  madLinkDisAsm,
  ComServ,
  Forms,
  Windows,

  ADODB_TLB                            in '..\..\Other Dependencies\DssVcl32\ADODB_TLB.pas',
  BaseADODataModule                    in '..\..\Other Dependencies\DssVcl32\BaseADODataModule.pas' {dmBaseADO: TDataModule},
  ExceptionForm                        in '..\..\Other Dependencies\DssVcl32\ExceptionForm.pas' {frmException: TfrmException},

  XPMenu                               in '..\..\Other Dependencies\Third-Party\XPMenu\XPMenu.pas',

  COMClasses                           in '..\..\Other Dependencies\JNCC\Components\COMClasses.pas',
  CompositeComponent                   in '..\..\Other Dependencies\JNCC\Components\CompositeComponent.pas',
  DatabaseUtilities                    in '..\..\Other Dependencies\JNCC\Components\DatabaseUtilities.pas',
  DropSource                           in '..\..\Other Dependencies\JNCC\Components\DropSource.pas',
  DropStruct                           in '..\..\Other Dependencies\JNCC\Components\DropStruct.pas',
  DropTarget                           in '..\..\Other Dependencies\JNCC\Components\DropTarget.pas',
  GenFuncs                             in '..\..\Other Dependencies\JNCC\Components\GenFuncs.pas',
  GridSquareItem                       in '..\..\Other Dependencies\JNCC\Components\GridSquareItem.pas',
  HTMLDisplayFuncs                     in '..\..\Other Dependencies\JNCC\Components\HTMLDisplayFuncs.pas',
  JNCCDatasets                         in '..\..\Other Dependencies\JNCC\Components\JNCCDatasets.pas',
  JNCCGrid                             in '..\..\Other Dependencies\JNCC\Components\JNCCGrid.pas',
  JNCCRelationships                    in '..\..\Other Dependencies\JNCC\Components\JNCCRelationships.pas',
  SpatialRefFuncs                      in '..\..\Other Dependencies\JNCC\Components\SpatialRefFuncs.pas',
  SQLConverter                         in '..\..\Other Dependencies\JNCC\Components\SQLConverter.pas',
  VagueDate                            in '..\..\Other Dependencies\JNCC\Components\VagueDate.pas',
  VagueDateEdit                        in '..\..\Other Dependencies\JNCC\Components\VagueDateEdit.pas',

  Constants                            in '..\..\Other Dependencies\JNCC\Constants.pas',
  DatabaseAccessADO                    in '..\..\Other Dependencies\JNCC\DatabaseAccessADO.pas' {dmDatabase: TDataModule},
  SQLConstants                         in '..\..\Other Dependencies\JNCC\SQLConstants.pas',
  ValidationData                       in '..\..\Other Dependencies\JNCC\ValidationData.pas' {dmValidation: TDataModule},

  DataClasses                          in '..\..\Other Dependencies\JNCC\Components\DataClasses.pas',
  Recorder2000_TLB                     in '..\..\Other Dependencies\JNCC\Recorder2000_TLB.pas',

  BaseCompositeComponent               in '..\..\Other Dependencies\Recorder Addins\Components\BaseCompositeComponent.pas',
  DSSDataTypes                         in '..\..\Other Dependencies\Recorder Addins\Components\DSSDataTypes.pas',
  LinkedControls                       in '..\..\Other Dependencies\Recorder Addins\Components\LinkedControls.pas',

  CollectionsModuleManager_TLB         in 'CollectionsModuleManager_TLB.pas',
  ThesaurusBrowser_TLB                 in 'ThesaurusBrowser_TLB.pas',
  StandardReports_TLB                  in 'StandardReports_TLB.pas',

  ApplicationSettings                  in '..\Common\ApplicationSettings.pas',
  BaseDetailFrameUnit                  in '..\Common\BaseDetailFrameUnit.pas' {BaseDetailFrame: TFrame},
  BaseDragFrameUnit                    in '..\Common\BaseDragFrameUnit.pas' {BaseDragFrame: TFrame},
  BaseFullScreenFrameUnit              in '..\Common\BaseFullScreenFrameUnit.pas' {BaseFullScreenFrame: TFrame},
  BasePageControlFrameUnit             in '..\Common\BasePageControlFrameUnit.pas' {BasePageControlFrame: TFrame},
  BaseTabSheetFrameUnit                in '..\Common\BaseTabSheetFrameUnit.pas' {BaseTabSheetFrame: TFrame},
  CommonNameFetchQueue                 in '..\Common\CommonNameFetchQueue.pas',
  ComUnit                              in '..\Common\ComUnit.pas',
  ConceptHTMLDetail                    in '..\Common\ConceptHTMLDetail.pas' {fraConceptHTMLDetail: TFrame},
  DataTypes                            in '..\Common\DataTypes.pas',
  Diagram                              in '..\Common\Diagram.pas',
  DiagramObjects                       in '..\Common\DiagramObjects.pas',
  DiagramSettings                      in '..\Common\DiagramSettings.pas' {dlgDiagramSettings: TForm},
  DiagramXMLConstants                  in '..\Common\DiagramXMLConstants.pas',
  Find                                 in '..\Common\Find.pas' {dlgFind},
  FrameDescriptors                     in '..\Common\FrameDescriptors.pas' {fraDescriptors: TFrame},
  FrameMeasurementsGeneral             in '..\Common\FrameMeasurementsGeneral.pas' {fraMeasurementsGeneral: TFrame},
  FrameMetadata                        in '..\Common\FrameMetadata.pas' {fraMetadata: TFrame},
  FrameSources                         in '..\Common\FrameSources.pas' {fraSources: TFrame},
  GeneralData                          in '..\Common\GeneralData.pas' {dmGeneral: TDataModule},
  HistoryManager                       in '..\Common\HistoryManager.pas',
  InterfaceDataModule                  in '..\Common\InterfaceDataModule.pas' {dmInterface: TDataModule},
  LuxembourgConstants                  in '..\Common\LuxembourgConstants.pas',
  LuxembourgDataClasses                in '..\Common\LuxembourgDataClasses.pas',
  LuxembourgFunctions                  in '..\Common\LuxembourgFunctions.pas',
  MacroGenerator                       in '..\Common\MacroGenerator.pas',
  RegisteredControls                   in '..\Common\RegisteredControls.pas',
  ResourceStrings                      in '..\Common\ResourceStrings.pas',
  SearchManager                        in '..\Common\SearchManager.PAS',
  UserMessages                         in '..\Common\UserMessages.pas',
  Validation                           in '..\Common\Validation.pas',

  Barcode                              in '..\Components\Barcode.pas',
  BcChkSum                             in '..\Components\BcChkSum.pas',
  ConceptGroupComboBox                 in '..\Components\ConceptGroupComboBox.pas',
  GIFImage                             in '..\Components\GIFImage.pas',
  LuxIDComboBox                        in '..\Components\LuxIDComboBox.pas',
  Memcheck                             in '..\Components\Memcheck.pas',
  SpatialRef                           in '..\Components\SpatialRef.pas',
  TermLabel                            in '..\Components\TermLabel.pas',

  CollectionsBrowser_TLB               in 'CollectionsBrowser_TLB.pas',
  ImageMagickObject_TLB                in '..\Collections Browser\ImageMagickObject_TLB.pas',
  BaseCollectionUnitsIncluded          in '..\Collections Browser\Source\BaseCollectionUnitsIncluded.pas' {BaseIncludedCollectionUnits: TFrame},
  BaseMaterialMovementDetailsFrameUnit in '..\Collections Browser\Source\BaseMaterialMovementDetailsFrameUnit.pas' {BaseMaterialMovementDetailsFrameUnit: TFrame},
  BrowserNodeCollectionUnits           in '..\Collections Browser\Source\BrowserNodeCollectionUnits.pas',
  BrowserNodeCommon                    in '..\Collections Browser\Source\BrowserNodeCommon.PAS',
  BrowserNodeConditionCheck            in '..\Collections Browser\Source\BrowserNodeConditionCheck.pas',
  BrowserNodeFramework                 in '..\Collections Browser\Source\BrowserNodeFramework.pas',
  BrowserNodeMovement                  in '..\Collections Browser\Source\BrowserNodeMovement.pas',
  BrowserNodeSpecimen                  in '..\Collections Browser\Source\BrowserNodeSpecimen.pas',
  BrowserViewTypes                     in '..\Collections Browser\Source\BrowserViewTypes.pas',
  CBMain                               in '..\Collections Browser\Source\CBMain.pas' {frmCBMain: TActiveForm} {frmCBMain: CoClass},
  CollectionsReportsMenu               in '..\Collections Browser\Source\CollectionsReportsMenu.pas' {CollectionsReportsMenu: CoClass},
  CollectionsReportsMenuItem           in '..\Collections Browser\Source\CollectionsReportsMenuItem.pas' {CollectionsReportsMenuItem: CoClass},
  CopyPasteMenuItem                    in '..\Collections Browser\Source\CopyPasteMenuItem.pas' {CopyPasteMenu: CoClass},
  EnquiryStats                         in '..\Collections Browser\Source\EnquiryStats.pas' {frmEnquiryStats},
  FrameAddSpecimenUnit                 in '..\Collections Browser\Source\FrameAddSpecimenUnit.pas' {FrameAddSpecimen},
  FrameCBNavigation                    in '..\Collections Browser\Source\FrameCBNavigation.pas' {fraCBNavigation: TFrame},
  FrameCollection                      in '..\Collections Browser\Source\FrameCollection.pas' {fraCollection: TFrame},
  FrameCollectionGeneral               in '..\Collections Browser\Source\FrameCollectionGeneral.pas' {fraCollectionGeneral: TFrame},
  FrameCollectionTimePeriod            in '..\Collections Browser\Source\FrameCollectionTimePeriod.pas' {fraCollectionTimePeriod: TFrame},
  FrameContainerUnit                   in '..\Collections Browser\Source\FrameContainerUnit.pas' {FrameContainer},
  FrameCondition                       in '..\Collections Browser\Source\FrameCondition.pas' {fraCondition: TFrame},
  FrameConditionGeneral                in '..\Collections Browser\Source\FrameConditionGeneral.pas' {fraConditionGeneral: TFrame},
  FrameEnquiry                         in '..\Collections Browser\Source\FrameEnquiry.pas' {fraEnquiry: TFrame},
  FrameEnquiryGeneral                  in '..\Collections Browser\Source\FrameEnquiryGeneral.pas' {fraEnquiryGeneral: TFrame},
  FrameEnquiryResponse                 in '..\Collections Browser\Source\FrameEnquiryResponse.pas' {fraEnquiryResponse: TFrame},
  FrameNumberHistory                   in '..\Collections Browser\Source\FrameNumberHistory.pas' {fraNumberHistory: TFrame},
  FrameListViewer                      in '..\Collections Browser\Source\FrameListViewer.pas' {fraListViewer: TFrame},
  FrameMovement                        in '..\Collections Browser\Source\FrameMovement.pas' {fraMovement: TFrame},
  FrameMovementCommunications          in '..\Collections Browser\Source\FrameMovementCommunications.pas' {fraMovementCommunications: TFrame},
  FrameMovementDetails                 in '..\Collections Browser\Source\FrameMovementDetails.pas' {fraMovementDetails: TFrame},
  FrameMovementDetailsCollectionUnits  in '..\Collections Browser\Source\FrameMovementDetailsCollectionUnits.pas' {fraMovementDetailsCollectionUnit: TFrame},
  FrameMovementGeneral                 in '..\Collections Browser\Source\FrameMovementGeneral.pas' {fraMovementGeneral: TFrame},
  FrameMaterialMovement                in '..\Collections Browser\Source\FrameMaterialMovement.pas' {fraMaterialMovement: TFrame},
  FrameMaterialToUnknownDestination    in '..\Collections Browser\Source\FrameMaterialToUnknownDestination.pas' {fraMaterialToUnknownDestination: TFrame},
  FrameOwnershipDetails                in '..\Collections Browser\Source\FrameOwnershipDetails.pas' {fraOwnershipDetails: TFrame},
  FramePeople                          in '..\Collections Browser\Source\FramePeople.pas' {fraPeople: TFrame},
  FramePeopleGeneral                   in '..\Collections Browser\Source\FramePeopleGeneral.pas' {fraPeopleGeneral: TFrame},
  FramePeopleSpCollected               in '..\Collections Browser\Source\FramePeopleSpCollected.pas' {fraPeopleSpCollected: TFrame},
  FramePeopleSpDetermined              in '..\Collections Browser\Source\FramePeopleSpDetermined.pas' {fraPeopleSpDetermined: TFrame},
  FrameProcess                         in '..\Collections Browser\Source\FrameProcess.pas' {fraProcess: TFrame},
  FrameProcessGeneral                  in '..\Collections Browser\Source\FrameProcessGeneral.pas' {fraProcessGeneral: TFrame},
  FrameSpecimen                        in '..\Collections Browser\Source\FrameSpecimen.pas' {fraSpecimen: TFrame},
  FrameSpecimenGeneral                 in '..\Collections Browser\Source\FrameSpecimenGeneral.pas' {fraSpecimenGeneral: TFrame},
  FrameStorage                         in '..\Collections Browser\Source\FrameStorage.pas' {fraStorage: TFrame},
  FrameStorageGeneral                  in '..\Collections Browser\Source\FrameStorageGeneral.pas' {fraStorageGeneral: TFrame},
  FrameTask                            in '..\Collections Browser\Source\FrameTask.pas' {fraTask: TFrame},
  FrameTaskGeneral                     in '..\Collections Browser\Source\FrameTaskGeneral.pas' {fraTaskGeneral: TFrame},
  FrameValuation                       in '..\Collections Browser\Source\FrameValuation.pas' {fraValuation: TFrame},
  FrameValuationGeneral                in '..\Collections Browser\Source\FrameValuationGeneral.pas' {fraValuationGeneral: TFrame},
  FrameFieldData                       in '..\Collections Browser\Source\FrameFieldData.pas' {fraFieldData: TFrame},
  FrameFieldDataGeneral                in '..\Collections Browser\Source\FrameFieldDataGeneral.pas' {fraFieldDataGeneral: TFrame},
  FrameFieldDataGeoAreas               in '..\Collections Browser\Source\FrameFieldDataGeoAreas.pas' {fraFieldDataGeoAreas: TFrame},
  FrameMaterials                       in '..\Collections Browser\Source\FrameMaterials.pas' {fraMaterials: TFrame},
  FrameMaterialsDocumented             in '..\Collections Browser\SOURCE\FrameMaterialsDocumented.pas' {fraMaterialsDocumented: TFrame},
  FrameDeterminationGeneral            in '..\Collections Browser\Source\FrameDeterminationGeneral.pas' {fraDeterminationGeneral: TFrame},
  FrameRelated                         in '..\Collections Browser\Source\FrameRelated.pas' {fraRelated: TFrame},
  FrameMultimedia                      in '..\Collections Browser\Source\FrameMultimedia.pas' {fraMultimedia: TFrame},
  FrameMaterialHostedMaterial          in '..\Collections Browser\Source\FrameMaterialHostedMaterial.pas' {fraMaterialHostedMaterial: TFrame},
  FrameInscription                     in '..\Collections Browser\Source\FrameInscription.pas' {fraInscription: TFrame},
  FrameInscriptionGeneral              in '..\Collections Browser\Source\FrameInscriptionGeneral.pas' {fraInscriptionGeneral: TFrame},
  FrameDetermination                   in '..\Collections Browser\Source\FrameDetermination.pas' {fraDetermination: TFrame},
  FrameTaskCollectionUnits             in '..\Collections Browser\Source\FrameTaskCollectionUnits.pas' {fraTaskCollectionUnits: TFrame},
  FrameJob                             in '..\Collections Browser\Source\FrameJob.pas' {fraJob: TFrame},
  FrameJobGeneral                      in '..\Collections Browser\Source\FrameJobGeneral.pas' {fraJobGeneral: TFrame},
  FrameFunding                         in '..\Collections Browser\Source\FrameFunding.pas' {fraFunding: TFrame},
  FrameFundingGeneral                  in '..\Collections Browser\Source\FrameFundingGeneral.pas' {fraFundingGeneral: TFrame},
  BaseNumberHistoryFrameUnit           in '..\Collections Browser\Source\BaseNumberHistoryFrameUnit.pas' {BaseNumberHistoryFrame: TFrame},
  FrameNumberHistoryReadOnly           in '..\Collections Browser\Source\FrameNumberHistoryReadOnly.pas' {fraNumberHistoryReadOnly: TFrame},
  FrameMeasurements                    in '..\Collections Browser\Source\FrameMeasurements.pas' {fraMeasurements: TFrame},
  FrameHistory                         in '..\Collections Browser\Source\FrameHistory.pas' {fraHistory: TFrame},
  FrameHistoryGeneral                  in '..\Collections Browser\Source\FrameHistoryGeneral.pas' {fraHistoryGeneral: TFrame},
  FrameStorageLayout                   in '..\Collections Browser\Source\FrameStorageLayout.pas' {fraStorageLayout: TFrame},
  SortOrderMenu                        in '..\Collections Browser\Source\SortOrderMenu.pas',
  SpecimenLabel                        in '..\Collections Browser\Source\SpecimenLabel.pas' {dlgSpecimenLabel},
  SpecimenMailMerge                    in '..\Collections Browser\Source\SpecimenMailMerge.pas' {dlgSpecimenMailMerge},
  SpecimenRelocation                   in '..\Collections Browser\Source\SpecimenRelocation.pas' {dlgSpecimenRelocation},
  StoreLayoutEditor                    in '..\Collections Browser\Source\StoreLayoutEditor.pas' {frmStorageLayout: TActiveForm},
  StoreLayoutDiagram                   in '..\Collections Browser\Source\StoreLayoutDiagram.pas',
  StoreDisplayProperties               in '..\Collections Browser\Source\StoreDisplayProperties.pas' {dlgStoreDisplayProperties},
  UserEdit                             in '..\Collections Browser\Source\UserEdit.pas',

  QuickEntryInsertTables               in '..\Quick Entry\Source\QuickEntryInsertTables.pas';

{$E ocx}

exports
  DllGetClassObject,
  DllCanUnloadNow,
  DllRegisterServer,
  DllUnregisterServer;

{$R *.RES}

{$R *.TLB}

var
  RecorderHandle: HWND;
  RecorderProcessId: DWORD;

begin
  RecorderHandle := FindWindow('TApplication', 'Recorder 6');
  if RecorderHandle <> 0 then
  begin
    // Recorder is running
    if GetWindowThreadProcessId(RecorderHandle, RecorderProcessId) <> 0 then
    begin
      if RecorderProcessId = GetCurrentProcessId then
      begin
        // We are running within Recorder
        Application.Handle := RecorderHandle;
        {$IFDEF madExcept}
        //MemChk;
        RegisterExceptionHandler(TfrmException.MadExceptionHandler, stTrySyncCallAlways);
        {$ELSE}
        Application.OnException := TfrmException.GlobalExceptionHandler;
        {$ENDIF}
        Application.CreateForm(TdmInterface, dmInterface);
        Application.CreateForm(TdmValidation, dmValidation);
        Application.CreateForm(TdmDatabase, dmDatabase);
      end;
    end;
  end;
end.

