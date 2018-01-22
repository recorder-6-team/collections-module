{===============================================================================
  Program:     ThesaurusEditor.exe

  Description:

  Created:

  Last revision information:
    $Revision: $
    $Date: $
    $Author: $

===============================================================================}

program ThesaurusEditor;

uses
  madExcept,
  madLinkDisAsm,
  Forms,
  ExceptionForm,
  MidasLib,
  Windows,
  Messages,
  Sysutils,

  ADODB_TLB                          in '..\..\Other Dependencies\DssVcl32\ADODB_TLB.pas',

  XPMenu                             in '..\..\Other Dependencies\Third-Party\XPMenu\XPMenu.pas',

  Recorder2000_TLB                   in '..\..\Other Dependencies\JNCC\Recorder2000_TLB.pas',
  Constants                          in '..\..\Other Dependencies\JNCC\Constants.pas',
  SQLConstants                       in '..\..\Other Dependencies\JNCC\SQLConstants.pas',
  DatabaseAccessADO                  in '..\..\Other Dependencies\JNCC\DatabaseAccessADO.pas',
  DefaultPaths                       in '..\..\Other Dependencies\JNCC\DefaultPaths.pas',

  DataClasses                        in '..\..\Other Dependencies\JNCC\Components\DataClasses.pas',
  DatabaseOutput                     in '..\..\Other Dependencies\JNCC\Components\DatabaseOutput.pas',
  DatabaseUtilities                  in '..\..\Other Dependencies\JNCC\Components\DatabaseUtilities.pas',
  DropSource                         in '..\..\Other Dependencies\JNCC\Components\DropSource.pas',
  DropStruct                         in '..\..\Other Dependencies\JNCC\Components\DropStruct.pas',
  DropTarget                         in '..\..\Other Dependencies\JNCC\Components\DropTarget.pas',
  GenFuncs                           in '..\..\Other Dependencies\JNCC\Components\GenFuncs.pas',
  JNCCDatasets                       in '..\..\Other Dependencies\JNCC\Components\JNCCDatasets.pas',
  JNCCGrid                           in '..\..\Other Dependencies\JNCC\Components\JNCCGrid.pas',
  JNCCRelationships                  in '..\..\Other Dependencies\JNCC\Components\JNCCRelationships.pas',
  SpatialRefFuncs                    in '..\..\Other Dependencies\JNCC\Components\SpatialRefFuncs.pas',
  SQLConverter                       in '..\..\Other Dependencies\JNCC\Components\SQLConverter.pas',
  VagueDate                          in '..\..\Other Dependencies\JNCC\Components\VagueDate.pas',
  VagueDateEdit                      in '..\..\Other Dependencies\JNCC\Components\VagueDateEdit.pas',

  BaseCompositeComponent             in '..\..\Other Dependencies\Recorder Addins\Components\BaseCompositeComponent.pas',
  DSSDataTypes                       in '..\..\Other Dependencies\Recorder Addins\Components\DSSDataTypes.pas',
  LinkedControls                     in '..\..\Other Dependencies\Recorder Addins\Components\LinkedControls.pas',

  CollectionsModuleManager_TLB       in 'CollectionsModuleManager_TLB.pas',

  ApplicationSettings                in '..\Common\ApplicationSettings.pas',
  BaseDetailFrameUnit                in '..\Common\BaseDetailFrameUnit.pas' {BaseDetailFrame: TFrame},
  BaseDragFrameUnit                  in '..\Common\BaseDragFrameUnit.pas' {BaseDragFrame: TFrame},
  BaseNavigatorFrame                 in '..\Common\BaseNavigatorFrame.pas' {fraBaseNavigator: TFrame},
  BasePageControlFrameUnit           in '..\Common\BasePageControlFrameUnit.pas' {BasePageControlFrame: TFrame},
  BaseTabSheetFrameUnit              in '..\Common\BaseTabSheetFrameUnit.pas' {BaseTabSheetFrame: TFrame},
  CommonNameFetchQueue               in '..\Common\CommonNameFetchQueue.pas',
  ConceptRankCache                   in '..\Common\ConceptRankCache.pas',
  DataTypes                          in '..\Common\DataTypes.pas',
  DomainConceptGroupSelector         in '..\Common\DomainConceptGroupSelector.pas' {fraDomainConceptGroupsSelector: TFrame},
  FilteredStringGrid                 in '..\Common\FilteredStringGrid.pas',
  Find                               in '..\Common\Find.pas',
  FrameMetadata                      in '..\Common\FrameMetadata.pas' {fraMetadata: TFrame},
  FrameSources                       in '..\Common\FrameSources.pas' {fraSources: TFrame},
  GeneralData                        in '..\Common\GeneralData.pas' {dmGeneral: TDataModule},
  HistoryManager                     in '..\Common\HistoryManager.pas',
  InterfaceDataModule                in '..\Common\InterfaceDataModule.pas' {dmInterface: TDataModule},
  LuxembourgConstants                in '..\Common\LuxembourgConstants.pas',
  LuxembourgDataClasses              in '..\Common\LuxembourgDataClasses.pas',
  LuxembourgFunctions                in '..\Common\LuxembourgFunctions.pas',
  RegisteredControls                 in '..\Common\RegisteredControls.pas',
  ResourceStrings                    in '..\Common\ResourceStrings.pas',
  SearchManager                      in '..\Common\SearchManager.pas',
  SelectLineage                      in '..\Common\SelectLineage.pas' {dlgSelectLineage: TForm},
  ThesaurusNavigator                 in '..\Common\ThesaurusNavigator.pas' {fraThesaurusNavigator: TFrame},
  UserMessages                       in '..\Common\UserMessages.pas',
  Validation                         in '..\Common\Validation.pas',

  SMI2TXT                            in '..\Components\SMI2TXT.pas',
  ConceptGroupComboBox               in '..\Components\ConceptGroupComboBox.pas',
  LuxIDComboBox                      in '..\Components\LuxIDComboBox.pas',
  TermLabel                          in '..\Components\TermLabel.pas',
  
  About                              in '..\Thesaurus Editor\Source\About.pas' {dlgAboutThesaurusEditor},
  BaseMDIChildEditorUnit             in '..\Thesaurus Editor\Source\BaseMDIChildEditorUnit.pas' {BaseMDIChildEditor},
  BaseMDIChildUnit                   in '..\Thesaurus Editor\Source\BaseMDIChildUnit.pas' {BaseMDIChild},
  ChecklistTransferForm              in '..\Thesaurus Editor\Source\ChecklistTransferForm.pas' {dlgChecklistTransferForm},
  Concept                            in '..\Thesaurus Editor\Source\Concept.pas' {fraConcept: TFrame},
  ConceptDetails                     in '..\Thesaurus Editor\Source\ConceptDetails.pas' {fraConceptDetails: TFrame},
  ConceptDetailsNodes                in '..\Thesaurus Editor\Source\ConceptDetailsNodes.pas',
  ConceptGeneral                     in '..\Thesaurus Editor\Source\ConceptGeneral.pas' {fraConceptGeneral: TFrame},
  ConceptGroup                       in '..\Thesaurus Editor\Source\ConceptGroup.pas' {fraConceptGroup: TFrame},
  ConceptGroupGeneral                in '..\Thesaurus Editor\Source\ConceptGroupGeneral.pas' {fraConceptGroupGeneral: TFrame},
  ConceptGroupQualityChecker         in '..\Thesaurus Editor\Source\ConceptGroupQualityChecker.pas' {frmConceptGroupQualityChecker},
  ConceptGroupQualityCheckerOptions  in '..\Thesaurus Editor\Source\ConceptGroupQualityCheckerOptions.pas' {dlgConceptGroupQualityCheckerOptions},
  ConceptGroupQualityCheckHistory    in '..\Thesaurus Editor\Source\ConceptGroupQualityCheckHistory.pas' {dlgConceptGroupQualityCheckHistory},
  ConceptGroupVersion                in '..\Thesaurus Editor\Source\ConceptGroupVersion.pas' {fraConceptGroupVersion: TFrame},
  ConceptGroupVersionGeneral         in '..\Thesaurus Editor\Source\ConceptGroupVersionGeneral.pas' {fraConceptGroupVersionGeneral: TFrame},
  ConceptHistory                     in '..\Thesaurus Editor\Source\ConceptHistory.pas' {fraConceptHistory: TFrame},
  ConceptHistoryDetails              in '..\Thesaurus Editor\Source\ConceptHistoryDetails.pas' {fraConceptHistoryDetails: TFrame},
  ConceptOrganiser                   in '..\Thesaurus Editor\Source\ConceptOrganiser.pas' {frmConceptOrganiser},
  ConfirmHistoryUpdate               in '..\Thesaurus Editor\Source\ConfirmHistoryUpdate.pas' {dlgConfirmHistoryUpdate},
  DeletionOptions                    in '..\Thesaurus Editor\Source\DeletionOptions.pas' {dlgDeletionOptions},
  Designation                        in '..\Thesaurus Editor\Source\Designation.pas' {fraDesignation: TFrame},
  DesignationGeneral                 in '..\Thesaurus Editor\Source\DesignationGeneral.pas' {fraDesignationGeneral: TFrame},
  Domain                             in '..\Thesaurus Editor\Source\Domain.pas' {fraDomain: TFrame},
  DomainGeneral                      in '..\Thesaurus Editor\Source\DomainGeneral.pas' {fraDomainGeneral: TFrame},
  DomainHyperlinks                   in '..\Thesaurus Editor\Source\DomainHyperlinks.pas' {fraDomainHyperlinks: TFrame},
  DomainRank                         in '..\Thesaurus Editor\Source\DomainRank.pas' {fraDomainRank: TFrame},
  ExportToDict                       in '..\Thesaurus Editor\Source\ExportToDict.pas' {dlgExportToDict},
  Fact                               in '..\Thesaurus Editor\Source\Fact.pas' {fraFact: TFrame},
  FactGeneral                        in '..\Thesaurus Editor\Source\FactGeneral.pas' {fraFactGeneral: TFrame},
  FrameBaseMaintainRelationships     in '..\Thesaurus Editor\Source\FrameBaseMaintainRelationships.pas' {fraBaseMaintainRelationships: TFrame},
  FrameMaintainRelationships         in '..\Thesaurus Editor\Source\FrameMaintainRelationships.pas' {fraMaintainRelationships: TFrame},
  FrameMaintainSemanticRelationships in '..\Thesaurus Editor\Source\FrameMaintainSemanticRelationships.pas' {fraMaintainSemanticRelationships: TFrame},
  FrameMergeData                     in '..\Thesaurus Editor\Source\FrameMergeData.pas' {fraMergeData: TFrame},
  ImportFromDict                     in '..\Thesaurus Editor\Source\ImportFromDict.pas' {dlgImportFromDict},
  ImportSpreadsheet                  in '..\Thesaurus Editor\Source\Importspreadsheet.pas' {dlgImportSpreadsheet},
  ListOrganiser                      in '..\Thesaurus Editor\Source\ListOrganiser.pas' {frmListOrganiser},
  ListOrganiserNodes                 in '..\Thesaurus Editor\Source\ListOrganiserNodes.pas',
  ListOrganiserTreeFrame             in '..\Thesaurus Editor\Source\ListOrganiserTreeFrame.pas' {fraListOrganiserTree: TFrame},
  LocalDomain                        in '..\Thesaurus Editor\Source\LocalDomain.pas' {fraLocalDomain: TFrame},
  LocalDomainGeneral                 in '..\Thesaurus Editor\Source\LocalDomainGeneral.pas' {fraLocalDomainGeneral: TFrame},
  MaintainRelationships              in '..\Thesaurus Editor\Source\MaintainRelationships.pas' {frmMaintainRelationships},
  MergeData                          in '..\Thesaurus Editor\Source\MergeData.pas' {frmMergeData: TForm},
  ProcessLongProcedureForm           in '..\Thesaurus Editor\Source\ProcessLongProcedureForm.pas' {frmProcessLongProcedure: TForm},
  Relationship                       in '..\Thesaurus Editor\Source\Relationship.pas' {fraRelationship: TFrame},
  RelationshipGeneral                in '..\Thesaurus Editor\Source\RelationshipGeneral.pas' {fraRelationshipGeneral: TFrame},
  SearchTerms                        in '..\Thesaurus Editor\Source\SearchTerms.pas' {fraSearchTerms: TFrame},
  SelectTaxonList                    in '..\Thesaurus Editor\Source\SelectTaxonList.pas' {dlgSelectTaxonList},
  SubjectArea                        in '..\Thesaurus Editor\Source\SubjectArea.pas' {fraSubjectArea: TFrame},
  SubjectAreaGeneral                 in '..\Thesaurus Editor\Source\SubjectAreaGeneral.pas' {fraSubjectAreaGeneral: TFrame},
  Synonym                            in '..\Thesaurus Editor\Source\Synonym.pas' {fraSynonym: TFrame},
  TermGenerationFunctions            in '..\Thesaurus Editor\Source\TermGenerationFunctions.pas',
  TermVersion                        in '..\Thesaurus Editor\Source\TermVersion.pas' {fraTermVersion: TFrame},
  TermVersionGeneral                 in '..\Thesaurus Editor\Source\TermVersionGeneral.pas' {fraTermVersionGeneral: TFrame},
  ThesaurusApplicationSettings       in '..\Thesaurus Editor\Source\ThesaurusApplicationSettings.pas',
  ThesaurusEditorData                in '..\Thesaurus Editor\Source\ThesaurusEditorData.pas' {dmMain: TDataModule},
  ThesaurusEditorMain                in '..\Thesaurus Editor\Source\ThesaurusEditorMain.pas' {frmThesaurusEditor},
  ThesaurusNavigatorEditable         in '..\Thesaurus Editor\Source\ThesaurusNavigatorEditable.pas' {fraThesaurusNavigatorEditable: TFrame},
  PublishedTermRuleSelector          in '..\Thesaurus Editor\Source\PublishedTermRuleSelector.pas' {fraPublishedTermRuleSelector: TFrame};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'Thesaurus Editor';
  {$IFDEF madExcept}
  RegisterExceptionHandler(TfrmException.MadExceptionHandler, stTrySyncCallAlways);
  {$ELSE}
  Application.OnException := TfrmException.GlobalExceptionHandler;
  {$ENDIF}
  Application.CreateForm(TfrmThesaurusEditor, frmThesaurusEditor);
  dmMain := TdmMain.Create(nil);
  dmInterface := TdmInterface.Create(nil);
  dmGeneral.AllowAllDomains := True; { force creation of datamodule to cause login prompt }
  // close the splash screen from the handle passed as a parameter
  if ParamCount>=1 then
    PostMessage(StrToInt64(Copy(ParamStr(1), 2, 255)), WM_CLOSE, 0, 0);
  Application.Run;
end.
