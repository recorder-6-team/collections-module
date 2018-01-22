{===============================================================================
  Library:     ThesaurusBrowser.ocx

  Description:

  Created:     September 2003
===============================================================================}

library ThesaurusBrowser;

uses
  madExcept, 
  madLinkDisAsm,
  ComServ,
  Forms,
  Windows,
  ExceptionForm,
  ADODB_TLB                               in '..\..\Other Dependencies\DssVcl32\ADODB_TLB.pas',
  BaseADODataModule                       in '..\..\Other Dependencies\DssVcl32\BaseADODataModule.pas' {dmBaseADO: TDataModule},

  XPMenu                                  in '..\..\Other Dependencies\Third-Party\XPMenu\XPMenu.pas',

  Recorder2000_TLB                        in '..\..\Other Dependencies\JNCC\Recorder2000_TLB.pas',

  ComClasses                              in '..\..\Other Dependencies\JNCC\Components\ComClasses.pas',
  DataClasses                             in '..\..\Other Dependencies\JNCC\Components\DataClasses.pas',
  DropSource                              in '..\..\Other Dependencies\JNCC\Components\DropSource.pas',
  DropStruct                              in '..\..\Other Dependencies\JNCC\Components\DropStruct.pas',
  DropTarget                              in '..\..\Other Dependencies\JNCC\Components\DropTarget.pas',
  GenFuncs                                in '..\..\Other Dependencies\JNCC\Components\GenFuncs.pas',
  HTMLDisplayFuncs                        in '..\..\Other Dependencies\JNCC\Components\HTMLDisplayFuncs.pas',
  JNCCDatasets                            in '..\..\Other Dependencies\JNCC\Components\JNCCDatasets.pas',
  JNCCGrid                                in '..\..\Other Dependencies\JNCC\Components\JNCCGrid.pas',
  SpatialRefFuncs                         in '..\..\Other Dependencies\JNCC\Components\SpatialRefFuncs.pas',
  SQLConverter                            in '..\..\Other Dependencies\JNCC\Components\SQLConverter.pas',
  VagueDate                               in '..\..\Other Dependencies\JNCC\Components\VagueDate.pas',
  VagueDateEdit                           in '..\..\Other Dependencies\JNCC\Components\VagueDateEdit.pas',

  BaseCompositeComponent                  in '..\..\Other Dependencies\Recorder Addins\Components\BaseCompositeComponent.pas',
  DSSDataTypes                            in '..\..\Other Dependencies\Recorder Addins\Components\DSSDataTypes.pas',
  LinkedControls                          in '..\..\Other Dependencies\Recorder Addins\Components\LinkedControls.pas',

  CollectionsModuleManager_TLB            in 'CollectionsModuleManager_TLB.pas',

  ApplicationSettings                     in '..\Common\ApplicationSettings.pas',
  BaseDetailFrameUnit                     in '..\Common\BaseDetailFrameUnit.pas' {BaseDetailFrame: TFrame},
  BaseDragFrameUnit                       in '..\Common\BaseDragFrameUnit.pas' {BaseDragFrame: TFrame},
  BasePageControlFrameUnit                in '..\Common\BasePageControlFrameUnit.pas'{BasePageControlFrame: TFrame},
  BaseTabSheetFrameUnit                   in '..\Common\BaseTabSheetFrameUnit.pas' {BaseTabSheetFrame: TFrame},
  BaseNavigatorFrame                      in '..\Common\BaseNavigatorFrame.pas' {fraBaseNavigator: TFrame},
  ConceptHTMLDetail                       in '..\Common\ConceptHTMLDetail.pas' {fraConceptHTMLDetail: TFrame},
  CommonNameFetchQueue                    in '..\Common\CommonNameFetchQueue.pas',
  ConceptRankCache                        in '..\Common\ConceptRankCache.pas',
  DataTypes                               in '..\Common\DataTypes.pas',
  Diagram                                 in '..\Common\Diagram.pas',
  DiagramObjects                          in '..\Common\DiagramObjects.pas',
  DiagramSettings                         in '..\Common\DiagramSettings.pas' {dlgDiagramSettings},
  DiagramXMLConstants                     in '..\Common\DiagramXMLConstants.pas',
  DomainConceptGroupSelector              in '..\Common\DomainConceptGroupSelector.pas' {fraDomainConceptGroupsSelector: TFrame},
  Find                                    in '..\Common\Find.pas' {dlgFind: TForm},
  GeneralData                             in '..\Common\GeneralData.pas' {dmGeneral: TDataModule},
  HistoryManager                          in '..\Common\HistoryManager.pas',
  InterfaceDataModule                     in '..\Common\InterfaceDataModule.pas' {dmInterface: TDataModule},
  LuxembourgConstants                     in '..\Common\LuxembourgConstants.pas',
  RegisteredControls                      in '..\Common\RegisteredControls.pas',
  ResourceStrings                         in '..\Common\ResourceStrings.pas',
  SearchManager                           in '..\Common\SearchManager.pas',
  SelectLineage                           in '..\Common\SelectLineage.pas' {dlgSelectLineage: TForm},
  ThesaurusNavigator                      in '..\Common\ThesaurusNavigator.pas' {fraThesaurusNavigator: TFrame},
  UserMessages                            in '..\Common\UserMessages.pas',
  Validation                              in '..\Common\Validation.pas',

  ConceptGroupComboBox                    in '..\Components\ConceptGroupComboBox.pas',
  GIFImage                                in '..\Components\GIFImage.pas',
  LuxIDComboBox                           in '..\Components\LuxIDComboBox.pas',

  ThesaurusBrowser_TLB                    in 'ThesaurusBrowser_TLB.pas',
  BaseDiagramObjectProperties             in '..\Thesaurus Browser\Source\BaseDiagramObjectProperties.pas' {fraBaseDiagramObjectProperties: TFrame},
  BaseDiagramPropertiesManager            in '..\Thesaurus Browser\Source\BaseDiagramPropertiesManager.pas' {dlgBaseDiagramPropertiesManager},
  ConceptRelationships                    in '..\Thesaurus Browser\Source\ConceptRelationships.pas' {fraConceptRelationships: TFrame},
  DiagramConceptProperties                in '..\Thesaurus Browser\Source\DiagramConceptProperties.pas' {fraDiagramConceptProperties: TFrame},
  DiagramConceptPropertiesForDiagram      in '..\Thesaurus Browser\Source\DiagramConceptPropertiesForDiagram.pas' {fraDiagramConceptPropertiesForDiagram: TFrame},
  DiagramConceptPropertiesForCG           in '..\Thesaurus Browser\Source\DiagramConceptPropertiesForCG.pas' {fraDiagramConceptPropertiesForCG: TFrame},
  DiagramConceptPropertiesForConcept      in '..\Thesaurus Browser\Source\DiagramConceptPropertiesForConcept.pas' {fraDiagramConceptPropertiesForConcept: TFrame},
  DiagramConceptPropertiesInherited       in '..\Thesaurus Browser\Source\DiagramConceptPropertiesInherited.pas' {fraDiagramConceptPropertiesInherited: TFrame},
  DiagramConceptPropertiesManager         in '..\Thesaurus Browser\Source\DiagramConceptPropertiesManager.pas' {dlgDiagramConceptPropertiesManager},
  DiagramPreview                          in '..\Thesaurus Browser\Source\DiagramPreview.pas' {frmDiagramPreview},
  DiagramRelationshipProperties           in '..\Thesaurus Browser\Source\DiagramRelationshipProperties.pas' {fraDiagramRelationshipProperties: TFrame},
  DiagramRelationshipPropertiesForDiagram in '..\Thesaurus Browser\Source\DiagramRelationshipPropertiesForDiagram.pas' {fraDiagramRelationshipPropertiesForDiagram: TFrame},
  DiagramRelationshipPropertiesForType    in '..\Thesaurus Browser\Source\DiagramRelationshipPropertiesForType.pas' {fraDiagramRelationshipPropertiesForType: TFrame},
  DiagramRelationshipPropertiesForRel     in '..\Thesaurus Browser\Source\DiagramRelationshipPropertiesForRel.pas' {fraDiagramRelationshipPropertiesForRel: TFrame},
  DiagramRelationshipPropertiesInherited  in '..\Thesaurus Browser\Source\DiagramRelationshipPropertiesInherited.pas' {fraDiagramRelationshipPropertiesInherited: TFrame},
  DiagramRelationshipPropertiesManager    in '..\Thesaurus Browser\Source\DiagramRelationshipPropertiesManager.pas' {dlgDiagramRelationshipPropertiesManager},
  DiagramsMenu                            in '..\Thesaurus Browser\Source\DiagramsMenu.pas',
  ThesaurusBrowserDetails                 in '..\Thesaurus Browser\Source\ThesaurusBrowserDetails.pas' {fraThesaurusBrowserDetails: TFrame},
  ThesaurusBrowserImpl                    in '..\Thesaurus Browser\Source\ThesaurusBrowserImpl.pas' {TfrmThesaurusBrowser: TActiveForm},
  ThesaurusDiagram                        in '..\Thesaurus Browser\Source\ThesaurusDiagram.pas',
  ThesaurusDiagramObjectLists             in '..\Thesaurus Browser\Source\ThesaurusDiagramObjectLists.pas',
  ThesaurusDiagramObjects                 in '..\Thesaurus Browser\Source\ThesaurusDiagramObjects.pas',
  ThesaurusNavigatorDiagramContainer      in '..\Thesaurus Browser\Source\ThesaurusNavigatorDiagramContainer.pas' {fraThesaurusNavigatorDiagramContainer: TFrame};

{$E ocx}

exports
  DllGetClassObject,
  DllCanUnloadNow,
  DllRegisterServer,
  DllUnregisterServer;

{$R *.TLB}

{$R *.RES}

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
        RegisterExceptionHandler(TfrmException.MadExceptionHandler, stTrySyncCallAlways);
        {$ELSE}
        Application.OnException := TfrmException.GlobalExceptionHandler;
        {$ENDIF}
        dmInterface := TdmInterface.Create(Application);
      end;
    end;
  end;
end.
