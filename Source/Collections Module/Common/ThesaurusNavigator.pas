{===============================================================================
  Unit:        ThesaurusNavigator

  Defines:     TfraThesaurusNavigator

  Description: A generic frame for navigating the thesaurus

  Model:       ThesaurusNavigator.mpb

  Created:     July 2003

===============================================================================}

unit ThesaurusNavigator;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, exgrid, RapTree, Treecoll, ComCtrls, Menus, Buttons,
  ImgList, ADODB, ApplicationSettings, BaseDragFrameUnit, ComboListID,
  CommonNameFetchQueue, ConceptRankCache, DropSource, ExceptionForm,
  GeneralFunctions, InterfaceDataModule, ResourceStrings, SearchManager,
  SelectLineage, ImageListButton, ToolWin, LuxIDComboBox, HistoryManager,
  BaseDetailFrameUnit, LuxembourgConstants, DataTypes, StrUtils,
  DomainConceptGroupSelector, DataClasses, DropTarget, ComObj, ExtCtrls,
  BaseNavigatorFrame, Types;

type

  EThesaurusNavigatorException = class(TExceptionPath)
  end;
  
  {-----------------------------------------------------------------------------
    TFlyNode descendant for managing details of a concept/term on the hierarchy.
  }
  TTermNode = class(TNavigatorNode, IAdditionalProperties)
  private
    FConceptKey: string;
    FConceptTerm: string;
    FPopulated: Boolean;
    FRank: TRank;
    FRefCount: Integer;
    function GetProperty(const AName: string): Variant;
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    procedure SetRank(Value: TRank);
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  public
    constructor Create(AOwner: TTreeCollection); override;
    destructor Destroy; override;
    procedure Initialise(const AConceptKey, ATerm: string);
    property ConceptKey: string read FConceptKey write FConceptKey;
    property ConceptTerm: string read FConceptTerm;
    property Rank: TRank read FRank write SetRank;
  end;
  
  {-----------------------------------------------------------------------------
    Frame component that facilitates the browsing of the thesaurus by subject area, domain
    and concept group.  Used in both the Thesaurus Editor and the Thesaurus Browser to
    browse concepts.
    This frame is able to receive a list of concepts from other systems and display them in
    tvHierarchy.  The existing nodes in tvHierarchy are cleared, and the new concepts added
    in Concept.Sort_Code order.  If the concepts all exist within 1 domain then the domain
    is selected in cmbDomains, otherwise cmbDomains is set to an empty value.  If the
    concepts all exist within 1 concept group then the concept group is selected in
    cmbConceptGroups, otherwise cmbDomains is set to an empty value.
  }
  TfraThesaurusNavigator = class(TfraBaseNavigator)
    btnGo: TButton;
    btnSearchMode: TBitBtn;
    eSearch: TEdit;
    fraDomainConceptGroupsSelector: TfraDomainConceptGroupsSelector;
    Label3: TLabel;
    pmNode: TPopupMenu;
    pmNodeCopy: TMenuItem;
    pmNodeRefreshNodeChildren: TMenuItem;
    pmNodeShowAncestorHierarchy: TMenuItem;
    pmSearchMode: TPopupMenu;
    pmSearchModeFind: TMenuItem;
    pmSearchModePopulateTopLevel: TMenuItem;
    pnlTop: TPanel;
    procedure btnGoClick(Sender: TObject);
    procedure btnSearchModeClick(Sender: TObject);
    procedure eSearchKeyPress(Sender: TObject; var Key: Char);
    procedure fraDomainConceptGroupsSelectorcmbConceptGroupsChange(Sender: TObject);
    procedure fraDomainConceptGroupsSelectorcmbConceptGroupsKeyDown(Sender: TObject; var Key:
        Word; Shift: TShiftState);
    procedure FrameEnter(Sender: TObject);
    procedure FrameResize(Sender: TObject);
    procedure pmNodeCopyClick(Sender: TObject);
    procedure pmNodeRefreshNodeChildrenClick(Sender: TObject);
    procedure pmNodeShowAncestorHierarchyClick(Sender: TObject);
    procedure pmSearchModeFindClick(Sender: TObject);
    procedure pmSearchModePopulateTopLevelClick(Sender: TObject);
    procedure tlbHistoryEnter(Sender: TObject);
    procedure tvHierarchyChange(Sender: TObject; Node: TFlyNode);
    procedure tvHierarchyDrawCell(Sender: TObject; aCanvas: TCanvas; ACol, ARow: Integer;
        Rect: TRect; State: TExGridDrawState);
    procedure tvHierarchyExpanding(Sender: TObject; Node: TFlyNode; var AllowExpansion:
        Boolean);
    procedure tvHierarchySelectCell(Sender: TObject; ACol, ARow: Integer; var CanSelect:
        Boolean);
  private
    FActivated: Boolean;
    FCommonNameFetchQueue: TCommonNameFetchQueue;
    FHistoryManager: THistoryManager;
    FIsCut: Boolean;
    FIsPasteAsSynonym: Boolean;
    FIsPasteAsTopLevel: Boolean;
    FShowHierarchy: Boolean;
    function AddChildNode(AParentNode: TFlyNode; const AConceptKey, AItemName,
        AConceptRankKey: string; AHasChildren: boolean): TTermNode; overload;
    function AddChildNode(
      const AParentNode: TFlyNode;
      const ARecordset: _Recordset;
      ADisplayChildren: Boolean = True): TTermNode; overload;
    procedure AddNodeWithAncestry(const ANonPreferredConceptKey: string);
    procedure AddTopLevelNode(
      ARecordset: _Recordset;
      ADisplayChildren: Boolean = True);
    procedure ClearTreeView;
    procedure EnableSearchControls(AEnabled: boolean);
    function GetCommonNameFetchQueue: TCommonNameFetchQueue;
    function GetConceptGroupKey: string;
    function GetConceptGroupVersionKey: string;
    function GetDomainKey: string;
    function GetHierarchyRelationTypeKey: string;
    procedure InternalDisplayConcept(const AConceptKey: string; AAddToHistory: boolean = True);
    procedure RunSearch;
    procedure SelectDomainAndConceptGroup(const AConceptKey: string);
    procedure SelectFirstNodeInList;
    procedure SetDefaultSelections;
    procedure SetNodeRank(ATermNode: TTermNode; const ARankKey: string);
    property CommonNameFetchQueue: TCommonNameFetchQueue read GetCommonNameFetchQueue;
  protected
    FConceptRankCache: TConceptRankCache;
    procedure PopulateNode(ANode: TNavigatorNode); overload; override;
    procedure CheckNodeCanDrop(APoint: TPoint; const ATable, AKeyField: string; var
        Accept: boolean); override;
    procedure DropTerm(const Sender: TObject; const iFormat : integer; const
        iSourceData: TKeyList; const iTextStrings : TstringList; const
        iIsPasteOperation: boolean; var ioHandled : boolean); override;
    procedure NodeCopy; virtual;
    procedure FetchConcepts(
      const AConceptSelectSP: String;
      AParams: array of Variant;
      ADisplayChildren: Boolean);
    procedure SetShowHierarchy(AValue: Boolean);
    function GetTableList : TStringDynArray; override;
    function CanDrag : Boolean; override;
    function CanDrop : Boolean; override; 
    procedure SetDropSourceData(var oDropSource: TJNCCDropSource;
        var node : TFlyNode); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure CreateNewNode(AParentNode : TTermNode = nil);
    procedure DisplayAncestorHierarchy(const AConceptKey: string; ALineageID: integer;
        AAddToHistory: boolean=true);
    procedure DisplaySelectedAncestorHierarchy(
      const ConceptKey, RankKey: string;
      Lineage: _Recordset);
    procedure DisplayConcept(const AConceptKey: string; AAddToHistory: boolean = True);
    procedure DisplayConceptGroup(const AKey: string);
    procedure DisplaySelectedConceptOnly;
    procedure RefreshNodeCaption(ANode: TFlyNode; AConceptsToUpdate: TStringList);
    procedure RefreshNodeRank(ATermNode: TTermNode);
    property ConceptGroupKey: string read GetConceptGroupKey;
    property ConceptGroupVersionKey: string read GetConceptGroupVersionKey;
    property DomainKey: string read GetDomainKey;
    property DraggedNode: TFlyNode read FDraggedNode write SetDraggedNode;
    property HierarchyRelationTypeKey: string read GetHierarchyRelationTypeKey;
    property HistoryManager: THistoryManager read FHistoryManager;
    property IsCut: Boolean read FIsCut write FIsCut;
    property IsPasteAsSynonym: Boolean read FIsPasteAsSynonym write FIsPasteAsSynonym;
    property IsPasteAsTopLevel: Boolean read FIsPasteAsTopLevel write FIsPasteAsTopLevel;
    property ShowHierarchy: Boolean read FShowHierarchy write SetShowHierarchy;
  end;
  
  {-----------------------------------------------------------------------------
    Base class for tracking the history information required for the Thesaurus Navigator.
  }
  TBaseThesaurusHistoryItem = class(TBaseHistoryItem)
  private
    FConceptKey: string;
    FThesaurusNavigator: TfraThesaurusNavigator;
    procedure SetConceptKey(const Value: string);
    procedure SetThesaurusNavigator(Value: TfraThesaurusNavigator);
  public
    property ConceptKey: string read FConceptKey write SetConceptKey;
    property ThesaurusNavigator: TfraThesaurusNavigator read FThesaurusNavigator write
        SetThesaurusNavigator;
  end;
  
  {-----------------------------------------------------------------------------
    Class for tracking history items that describe the display of a single concept.
  }
  TThesaurusAncestryHistoryItem = class(TBaseThesaurusHistoryItem)
  private
    FLineageID: Integer;
    procedure SetLineageID(Value: Integer);
  protected
    procedure Recall; override;
  public
    property LineageID: Integer read FLineageID write SetLineageID;
  end;
  
  {-----------------------------------------------------------------------------
    Class for tracking history items that describe the display of an ancestor hierarchy.
  }
  TThesaurusConceptHistoryItem = class(TBaseThesaurusHistoryItem)
  protected
    procedure Recall; override;
  end;
  

//==============================================================================
implementation

uses GeneralData;

{$R *.dfm}

{-==============================================================================
    TTermNode
===============================================================================}
{-------------------------------------------------------------------------------
  Constructor sets default property values. 
}
constructor TTermNode.Create(AOwner: TTreeCollection);
begin
  inherited Create(AOwner);
  
  FPopulated := False;
end;  // TTermNode.Create 

{-------------------------------------------------------------------------------
}
destructor TTermNode.Destroy;
begin
  inherited;
end;  // TTermNode.Destroy 

{-------------------------------------------------------------------------------
}
function TTermNode.GetProperty(const AName: string): Variant;
begin
  if AName = PROP_KEY then
    Result := FConceptKey
  else
    Result := Unassigned;
end;  // TTermNode.GetProperty 

{-------------------------------------------------------------------------------
  Initialises the node.  The concept key and term for the list preferred concept should be
      passed in.
}
procedure TTermNode.Initialise(const AConceptKey, ATerm: string);
begin
  FConceptKey := AConceptKey;
  FConceptTerm := ATerm;
  Caption := ATerm;
end;  // TTermNode.Initialise 

{-------------------------------------------------------------------------------
  Basic implementation of QueryInterface 
}
function TTermNode.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if GetInterface(IID, Obj) then
    Result := 0
  else
    Result := E_NOINTERFACE;
end;  // TTermNode.QueryInterface

{-------------------------------------------------------------------------------
}
procedure TTermNode.SetRank(Value: TRank);
begin
  
  FRank := Value;
end;  // TTermNode.SetRank 

{-------------------------------------------------------------------------------
  Reference counting is disabled, use standard Delphi object lifecycle. 
}
function TTermNode._AddRef: Integer;
begin
  Result := -1;
end;  // TTermNode._AddRef 

{-------------------------------------------------------------------------------
  Reference counting is disabled, use standard Delphi object lifecycle. 
}
function TTermNode._Release: Integer;
begin
  FRefCount := -1;
  Result := -1;
end;  // TTermNode._Release 

{-==============================================================================
    TfraThesaurusNavigator
===============================================================================}
{-------------------------------------------------------------------------------
}
constructor TfraThesaurusNavigator.Create(AOwner: TComponent);
begin
  inherited;
  FActivated := False;
  FHistoryManager := THistoryManager.Create;
  SetDefaultSelections;
  // A cache of concept ranks
  FConceptRankCache := TConceptRankCache.Create;
end;  // TfraThesaurusNavigator.Create

{-------------------------------------------------------------------------------
  Destructor frees objects and stops pending queries. 
}
destructor TfraThesaurusNavigator.Destroy;
begin
  FCommonNameFetchQueue.Free;
  FConceptRankCache.Free;
  FHistoryManager.Free;
  FreeAndNil(fraDomainConceptGroupsSelector);
  
  inherited Destroy;
end;  // TfraThesaurusNavigator.Destroy 

{-------------------------------------------------------------------------------
  Adds a child node to the node.  Input parameters are atomised rather than passing a
      recordset in the overloaded version of this.
}
function TfraThesaurusNavigator.AddChildNode(AParentNode: TFlyNode; const AConceptKey,
    AItemName, AConceptRankKey: string; AHasChildren: boolean): TTermNode;
begin
  Result := TTermNode(tvHierarchy.Items.AddTypedChild(AParentNode,
      TTermNode));
  with Result do begin
    Initialise(AConceptKey, AItemName);
    HasChildren := AHasChildren;
  end; // with
  // Check if a rank specified
  if AConceptRankKey<>'' then
    SetNodeRank(Result, AConceptRankKey);
  if AppSettings.DisplayCommonNames then
    CommonNameFetchQueue.Add(Result.ConceptKey, Result);
end;  // TfraThesaurusNavigator.AddChildNode 

{-------------------------------------------------------------------------------
  Adds a child node to the node.  The recordset should include the concept key for the list
      preferred concept key, the term and a flag for HasChildren.
}
function TfraThesaurusNavigator.AddChildNode(
  const AParentNode: TFlyNode;
  const ARecordset: _Recordset;
  ADisplayChildren: Boolean = True): TTermNode;
begin
  with ARecordset do
    if not EOF then begin
      if ADisplayChildren then
        ADisplayChildren := (Fields['HasChildren'].Value = 1);
      Result := AddChildNode(AParentNode, Fields['Concept_Key'].Value,
          Fields['Item_Name'].Value,
          VarToStr(Fields['Concept_Rank_Key'].Value),
          ADisplayChildren);
    end
    else
      raise EThesaurusNavigatorException.Create(Format(ResStr_InvalidMethodCall,
          ['TfraThesaurusNavigator.AddChildNode']));
end;  // TfraThesaurusNavigator.AddChildNode

{-------------------------------------------------------------------------------
  Adds a node, and its ancestry, if any, to the tree view. The supplied concept
  key is translated into the list preferred item.
}
procedure TfraThesaurusNavigator.AddNodeWithAncestry(
  const ANonPreferredConceptKey: string);
var
  lPreferredConcept: _Recordset;
  lConceptKey: string;
  lRankKey: string;
  lLineage: _Recordset;
begin
  if fraDomainConceptGroupsSelector.btnHistory.Down then
    lPreferredConcept := dmGeneral.GetRecordset(
        'usp_ConceptVersionListPreferred_Select_ForConcept', [
        '@ConceptGroupVersionKey', ConceptGroupVersionKey,
        '@ConceptKey', ANonPreferredConceptKey])
  else
    lPreferredConcept := dmGeneral.GetRecordset(
        'usp_ConceptListPreferred_Select_ForConcept', [
        '@ConceptGroupKey', ConceptGroupKey,
        '@ConceptKey', ANonPreferredConceptKey]);

  lConceptKey := lPreferredConcept.Fields['Concept_Key'].Value;
  lRankKey := VarToStr(lPreferredConcept.Fields['Concept_Rank_Key'].Value);

  lLineage := dmGeneral.GetRecordset('usp_ConceptLineage_Get', [
    '@ConceptKey', lConceptKey,
    '@IncludeSynonyms', AppSettings.IncludeHierarchySynonyms
  ]);

  if lLineage.RecordCount > 1 then
    DisplaySelectedAncestorHierarchy(lConceptKey, lRankKey, lLineage)
  else if lLineage.RecordCount = 1 then
    DisplayAncestorHierarchy(lConceptKey, lLineage.Fields['Lineage_ID'].Value)
  else begin
    AddTopLevelNode(lPreferredConcept);
    SelectFirstNodeInList;
  end;
end;  // TfraThesaurusNavigator.AddNodeWithAncestry

{-------------------------------------------------------------------------------
  Adds a top level node with details supplied in the recordset, containing concept_key,
      item_name and concept_rank_key.
}
procedure TfraThesaurusNavigator.AddTopLevelNode(
  ARecordset: _Recordset;
  ADisplayChildren: Boolean = True);
begin
  AddChildNode(nil, ARecordset, ADisplayChildren);
end;  // TfraThesaurusNavigator.AddTopLevelNode 

{-------------------------------------------------------------------------------
  Initiates a search. 
}
procedure TfraThesaurusNavigator.btnGoClick(Sender: TObject);
begin
  RunSearch;
  SetShowHierarchy(True);
end;  // TfraThesaurusNavigator.btnGoClick 

{-------------------------------------------------------------------------------
  Drops down a menu allowing the user to select a search, or population of the top level of
      the current concept group.
}
procedure TfraThesaurusNavigator.btnSearchModeClick(Sender: TObject);
var
  lMenuLocation: TPoint;
begin
  lMenuLocation := btnSearchMode.ClientToScreen(Point(0, btnSearchMode.Height));
  pmSearchMode.Popup(lMenuLocation.X, lMenuLocation.Y);
end;  // TfraThesaurusNavigator.btnSearchModeClick 

{-------------------------------------------------------------------------------
  Clears the tree view, and the common name fetch queue. 
}
procedure TfraThesaurusNavigator.ClearTreeView;
var
  lDummy: Boolean;
begin
  if Assigned(OnFrameNotification) then
    OnFrameNotification(Self, etTreeCleared, nil);

  if Assigned(tvHierarchy.OnChanging) then
    tvHierarchy.OnChanging(nil, nil, lDummy);
  tvHierarchy.Items.Clear;
  FDraggedNode := nil;
    // We need to ensure FCutItem is set to nil. FCutItem is used to remember
    // which node needs to be deleted when a cut node is pasted. However, if we
    // are clearing the treeview, the node will go anyway, but it won't
    // automatically be unassigned. Hence, an event is used to ensure it is nilled.
  //  if Assigned(OnFrameNotification) then
  //    OnFrameNotification(Self, etNilCutNode, nil);
  if Assigned(FCommonNameFetchQueue) then
    FCommonNameFetchQueue.Clear;
end;  // TfraThesaurusNavigator.ClearTreeView

{-------------------------------------------------------------------------------
}
procedure TfraThesaurusNavigator.CreateNewNode(AParentNode : TTermNode = nil);
begin
  tvHierarchy.Selected := AddChildNode(AParentNode, '', 'New Concept', '',
      False);
end;  // TfraThesaurusNavigator.CreateNewNode

{-------------------------------------------------------------------------------
  Builds the tree for the selected lineage, and moves the current node into the correct
      position in that tree.
}
procedure TfraThesaurusNavigator.DisplayAncestorHierarchy(const AConceptKey: string;
    ALineageID: integer; AAddToHistory: boolean=true);
var
  lRecordset: _Recordset;
  lLastNode: TFlyNode;
begin
  SelectDomainAndConceptGroup(AConceptKey);
  ClearTreeView;
  lRecordset := dmGeneral.GetRecordset('usp_ConceptAncestors_Select', [
      '@ConceptKey', AConceptKey,
      '@LineageID', ALineageID,
      '@HierarchyRelationTypeKey', HierarchyRelationTypeKey]);
  // Set the new hierarchy to start at the top level
  lLastNode := nil;
  with lRecordset do begin
    while not EOF do begin
      lLastNode := AddChildNode(lLastNode, lRecordset);
      TTermNode(lLastNode).Populated := True;
      MoveNext;
    end;
  end;
  // Now add the node at the bottom of the hierarchy
  lRecordset := dmGeneral.GetRecordset(
      'usp_ConceptListPreferred_Select_ForConcept', [
      '@ConceptGroupKey', ConceptGroupKey,
      '@ConceptKey', AConceptKey]);
  if not lRecordset.EOF then begin
    lLastNode := AddChildNode(lLastNode, lRecordset);
    lLastNode.MakeVisible;
    tvHierarchy.Selected := lLastNode;
  end;
  if AAddToHistory then
    with TThesaurusAncestryHistoryItem(FHistoryManager.Add(
         TThesaurusAncestryHistoryItem)) do begin
      Caption := 'Ancestry for ' + tvHierarchy.Selected.Text;
      ThesaurusNavigator := self;
      ConceptKey := AConceptKey;
      LineageID := ALineageID;
    end; // with
  CommonNameFetchQueue.ProcessWhenReady;
end;  // TfraThesaurusNavigator.DisplayAncestorHierarchy

{-------------------------------------------------------------------------------
  Prompts the user to select a lineage for the specified concept from the given
  set, and then builds the tree for that ancestry.
}
procedure TfraThesaurusNavigator.DisplaySelectedAncestorHierarchy(
  const ConceptKey, RankKey: string;
  Lineage: _Recordset);
var
  lDialog: TdlgSelectLineage;
begin
  lDialog := TdlgSelectLineage.Create(nil);
  try
    lDialog.ConceptKey := ConceptKey;
    lDialog.HierarchyRelationTypeKey := Self.HierarchyRelationTypeKey;
    lDialog.SelectedItemRank := RankKey;
    lDialog.SetRecordset(Lineage);

    if lDialog.ShowModal = mrOk then
    begin
      DisplayAncestorHierarchy(
          lDialog.OutputConceptKey,
          lDialog.OutputLineageID);
    end;
  finally
    lDialog.Free;
  end;
end;

{-------------------------------------------------------------------------------
  Clears the tree and displays a concept node.  If necessary the correct concept group and
      domain are selected.
}
procedure TfraThesaurusNavigator.DisplayConcept(const AConceptKey: string; AAddToHistory:
    boolean = True);
begin
  SelectDomainAndConceptGroup(AConceptKey);
  InternalDisplayConcept(AConceptKey, AAddToHistory);
end;  // TfraThesaurusNavigator.DisplayConcept 

{-------------------------------------------------------------------------------
  Displays a concept group. 
}
procedure TfraThesaurusNavigator.DisplayConceptGroup(const AKey: string);
begin
  fraDomainConceptGroupsSelector.SelectDomain(
      dmGeneral.GetStoredProcOutputParam('usp_ConceptGroup_GetDomain',
      ['@Key', AKey], '@Domain_Key'));
  fraDomainConceptGroupsSelector.btnHistory.Down := false;
  fraDomainConceptGroupsSelector.SelectConceptGroup(AKey);
end;  // TfraThesaurusNavigator.DisplayConceptGroup 

{-------------------------------------------------------------------------------
  Forces the treeview to discard all nodes except for the selected concept. 
}
procedure TfraThesaurusNavigator.DisplaySelectedConceptOnly;
var
  lKey: TKeyString;
begin
  lKey := TTermNode(tvHierarchy.Selected).FConceptKey;
  DisplayConcept(lKey);
end;  // TfraThesaurusNavigator.DisplaySelectedConceptOnly

{-------------------------------------------------------------------------------
  Disable or enable the search box and associated controls.  They are disabled until a concept
      group is selected.
}
procedure TfraThesaurusNavigator.EnableSearchControls(AEnabled: boolean);
begin
  eSearch.Enabled := AEnabled;
  btnGo.Enabled := AEnabled;
  btnSearchMode.Enabled := AEnabled;
end;  // TfraThesaurusNavigator.EnableSearchControls

{-------------------------------------------------------------------------------
  Sets the drop data of the drop source, call when a node begins being dragged.
}
procedure TfraThesaurusNavigator.SetDropSourceData(var oDropSource:
    TJNCCDropSource; var node : TFlyNode);
begin
  oDropSource.DropData.SetTable(TN_CONCEPT);
  // If the concept node that has been cut has a parent, we also want to store
  // the parent's concept key. With both of these concept keys we can establish
  // which Concept_Relation record needs to be altered.
  if Assigned(node.Parent) then
    oDropSource.DropData.AddItem(TTermNode(node).ConceptKey,
      TTermNode(node.Parent).ConceptKey)
  else
    oDropSource.DropData.AddItem(TTermNode(node).ConceptKey,
      '');
end;  // TfraBaseNavigator.SetDropSourceData

{-------------------------------------------------------------------------------
  If Return Key pressed, invoke the Find dialog 
}
procedure TfraThesaurusNavigator.eSearchKeyPress(Sender: TObject; var Key: Char);
begin
  if Key=#13 then begin
    Key:=#0;
    RunSearch;
  end;
end;  // TfraThesaurusNavigator.eSearchKeyPress 

{-------------------------------------------------------------------------------
  Perform the actual expansion of a node. 
}
procedure TfraThesaurusNavigator.PopulateNode(ANode: TNavigatorNode);
var
  lCursor: TCursor;
  lRecordset: _Recordset;
begin
  lCursor := HourglassCursor;
  tvHierarchy.Items.BeginUpdate;
  try
    if fraDomainConceptGroupsSelector.btnHistory.Down then
      lRecordset := dmGeneral.GetRecordset(
          'usp_Concept_Select_ForVersionParent', [
          '@ParentConceptKey', TTermNode(ANode).ConceptKey,
          '@ConceptGroupVersionKey', ConceptGroupVersionKey,
          '@HierarchyRelationTypeKey', HierarchyRelationTypeKey])
    else
      lRecordset := dmGeneral.GetRecordset('usp_Concept_Select_ForParent', [
          '@ParentConceptKey', TTermNode(ANode).ConceptKey,
          '@HierarchyRelationTypeKey', HierarchyRelationTypeKey]);
    with lRecordset do
      while not EOF do begin
        AddChildNode(ANode, lRecordset);
        MoveNext;
        if Application.Terminated then Break;
      end;
  finally
    tvHierarchy.Items.EndUpdate;
    DefaultCursor(lCursor);
  end;
  TTermNode(ANode).Populated := True;
  CommonNameFetchQueue.ProcessWhenReady;
end;  // TfraThesaurusNavigator.PopulateNode

{-------------------------------------------------------------------------------
  Enable the search controls only when there is a selected concept group or concept group
      version.
}
procedure TfraThesaurusNavigator.fraDomainConceptGroupsSelectorcmbConceptGroupsChange(Sender:
    TObject);
begin
  // If the selected item in the Concept Group combo box is changed, the
  // treeview and the Concept Details frame need to be cleared, or the
  // treeview's and the Concept Detail's Concept Group Key could go out of sync.
  ClearTreeView;
  SelectFirstNodeInList;
  EnableSearchControls(
      fraDomainConceptGroupsSelector.cmbConceptGroups.ItemIndex<>-1);
  // Persist the selection
  if fraDomainConceptGroupsSelector.btnHistory.Down then
    // Save last concept group version key - * is a tag indicating this.
    AppSettings.LastThesaurusConceptGroup :=
        '*' + fraDomainConceptGroupsSelector.ConceptGroupVersionKey
  else
    // save last concept group key
    AppSettings.LastThesaurusConceptGroup :=
        fraDomainConceptGroupsSelector.ConceptGroupKey;
end;  // TfraThesaurusNavigator.fraDomainConceptGroupsSelectorcmbConceptGroupsChange 

{-------------------------------------------------------------------------------
  If the delete button is pressed on the Concept Group selector, then the current selection
      should be cleared and the go button should be disabled.
}
procedure TfraThesaurusNavigator.fraDomainConceptGroupsSelectorcmbConceptGroupsKeyDown(Sender:
    TObject; var Key: Word; Shift: TShiftState);
begin
  inherited;
  if Key = VK_DELETE then begin
    fraDomainConceptGroupsSelector.cmbConceptGroups.ItemIndex := -1;
    fraDomainConceptGroupsSelectorcmbConceptGroupsChange(Self);
  end;
end;  // TfraThesaurusNavigator.fraDomainConceptGroupsSelectorcmbConceptGroupsKeyDown 

{-------------------------------------------------------------------------------
}
procedure TfraThesaurusNavigator.FrameEnter(Sender: TObject);
begin
  inherited;
  if not FActivated then begin
    FActivated := True;
    fraDomainConceptGroupsSelector.PopulateDomainCombo;
  end;
end;  // TfraThesaurusNavigator.FrameEnter 

{-------------------------------------------------------------------------------
  Ensure tvHierarchy column width stays consistent with the size of tvHierarchy. 
}
procedure TfraThesaurusNavigator.FrameResize(Sender: TObject);
begin
  tvHierarchy.ColWidths[0] := tvHierarchy.Width-17;
end;  // TfraThesaurusNavigator.FrameResize 

{-------------------------------------------------------------------------------
}
function TfraThesaurusNavigator.GetCommonNameFetchQueue: TCommonNameFetchQueue;
begin
  if not Assigned(FCommonNameFetchQueue) then begin
    // A queue of node common names we need to fetch
    FCommonNameFetchQueue := TCommonNameFetchQueue.Create(Self);
    FCommonNameFetchQueue.Parent := Self;
    FCommonNameFetchQueue.TreeView := tvHierarchy;
  end;
  Result := FCommonNameFetchQueue;
end;  // TfraThesaurusNavigator.GetCommonNameFetchQueue 

{-------------------------------------------------------------------------------
  Accessor method.  Returns the currently selected concept group key.  Raises an exception if
      concept groups not currently active.
}
function TfraThesaurusNavigator.GetConceptGroupKey: string;
begin
  Result := fraDomainConceptGroupsSelector.ConceptGroupKey;
end;  // TfraThesaurusNavigator.GetConceptGroupKey 

{-------------------------------------------------------------------------------
  Accessor method.  Returns the currently selected concept group version key.  Raises an
      exception if concept group versions not currently active.
}
function TfraThesaurusNavigator.GetConceptGroupVersionKey: string;
begin
  Result := fraDomainConceptGroupsSelector.ConceptGroupVersionKey;
end;  // TfraThesaurusNavigator.GetConceptGroupVersionKey 

{-------------------------------------------------------------------------------
}
function TfraThesaurusNavigator.GetDomainKey: string;
begin
  Result := fraDomainConceptGroupsSelector.DomainKey;
end;  // TfraThesaurusNavigator.GetDomainKey 

{-------------------------------------------------------------------------------
}
function TfraThesaurusNavigator.GetHierarchyRelationTypeKey: string;
begin
  Result := fraDomainConceptGroupsSelector.HierarchyRelationTypeKey;
end;  // TfraThesaurusNavigator.GetHierarchyRelationTypeKey

{-------------------------------------------------------------------------------
  Clears the tree, loads a concept.
}
procedure TfraThesaurusNavigator.InternalDisplayConcept(const AConceptKey: string;
    AAddToHistory: boolean = True);
begin
  ClearTreeView;
  AddNodeWithAncestry(AConceptKey);

  if AAddToHistory and Assigned(tvHierarchy.Selected) then begin
    // Record the item in the history
    with TThesaurusConceptHistoryItem(FHistoryManager.Add(
         TThesaurusConceptHistoryItem)) do begin
      Caption := tvHierarchy.Selected.Text;
      ThesaurusNavigator := self;
      ConceptKey := AConceptKey;
    end;
  end;
end;  // TfraThesaurusNavigator.InternalDisplayConcept

{-------------------------------------------------------------------------------
  Copies the selected node in the tree view.

  This method needs to be compiled without optimization, otherwise Self becomes
  nil after the call to ExecuteCopy, and then setting FIsCut fails with an
  access violation. I cannot explain this -- I assume it must be a compiler bug.

  See TFS bug #9831 (Thesaurus Editor) and the discussion in bug #11736 (in
  which the same issue occurs in Thesaurus Browser).
}
{$OPTIMIZATION OFF}
procedure TfraThesaurusNavigator.NodeCopy;
begin
  ExecuteCopy(tvHierarchy);
  FIsCut := False;
end;  // TfraThesaurusNavigator.NodeCopy
{$OPTIMIZATION ON}

{-------------------------------------------------------------------------------
}
procedure TfraThesaurusNavigator.pmNodeCopyClick(Sender: TObject);
begin
  inherited;
  NodeCopy;
end;  // TfraThesaurusNavigator.pmNodeCopyClick 

{-------------------------------------------------------------------------------
}
procedure TfraThesaurusNavigator.pmNodeRefreshNodeChildrenClick(Sender: TObject);
var
  lWasExpanded: boolean;
begin
  if Assigned(tvHierarchy.Selected) then begin
    // if the recorded dragged node is about to be destroyed, then clean it up
    if assigned(DraggedNode) then
      if DraggedNode.Parent=tvHierarchy.Selected then
        DraggedNode := nil;
    with TTermNode(tvHierarchy.Selected) do begin
      lWasExpanded := Expanded;
      DeleteChildren;
      Populated := False;
      // set HasChildren so that it will at least try to expand
      HasChildren := True;
      PopulateNode(TTermNode(tvHierarchy.Selected));
      HasChildren := Count>0;
      if lWasExpanded then
        Expand(false);
    end;
  end;
end;  // TfraThesaurusNavigator.pmNodeRefreshNodeChildrenClick

{-------------------------------------------------------------------------------
  When requested, show the current item's ancestors by building the tree above it.  If the
      item has more than one lineage, then display a dialog so the user can select which one
      to show.
}
procedure TfraThesaurusNavigator.pmNodeShowAncestorHierarchyClick(Sender: TObject);
var
  lRecordset: _Recordset;
  lNode: TTermNode;
  lRankKey: string;
begin
  if Assigned(tvHierarchy.Selected) then begin
    lNode := TTermNode(tvHierarchy.Selected);
    lRecordset := dmGeneral.GetRecordset('usp_ConceptLineage_Get', [
        '@ConceptKey', lNode.ConceptKey,
        '@IncludeSynonyms', AppSettings.IncludeHierarchySynonyms
        ]);
    with lRecordset do begin
      if (RecordCount=0) or ((RecordCount=1) and (Pos('\',
          Fields['Lineage'].Value)=0)) then
      begin
        ShowInformation(ResStr_NoAncestorHierarchy);
        ShowHierarchy := False;
      end
      else if RecordCount=1 then
        DisplayAncestorHierarchy(Fields['Concept_Key'].Value,
            Fields['Lineage_ID'].Value)
      else begin
        if Assigned(lNode.Rank) then
          lRankKey := lNode.Rank.RankKey
        else
          lRankKey := '';
          
        DisplaySelectedAncestorHierarchy(
            lNode.ConceptKey,
            lRankKey,
            lRecordset);
            
        ShowHierarchy := True;
      end;
    end; // with
  end; // if
end;  // TfraThesaurusNavigator.pmNodeShowAncestorHierarchyClick

{-------------------------------------------------------------------------------
}
procedure TfraThesaurusNavigator.pmSearchModeFindClick(Sender: TObject);
begin
  SetShowHierarchy(True);
  RunSearch;
end;  // TfraThesaurusNavigator.pmSearchModeFindClick

{-------------------------------------------------------------------------------
  Populate the top level of the hierarchy for the concept group. 
}
procedure TfraThesaurusNavigator.pmSearchModePopulateTopLevelClick(Sender: TObject);
begin
  SetShowHierarchy(True);
  if fraDomainConceptGroupsSelector.btnHistory.Down then
  begin
    FetchConcepts(
      'usp_Concept_Select_ForVersionTopLevel',
      ['@ConceptGroupVersionKey', ConceptGroupVersionKey,
       '@HierarchyRelationTypeKey', HierarchyRelationTypeKey],
      True);
  end
  else begin
    FetchConcepts(
      'usp_Concept_Select_ForTopLevel',
      ['@ConceptGroupKey', ConceptGroupKey,
        '@HierarchyRelationTypeKey', HierarchyRelationTypeKey],
      True);
  end;
end;  // TfraThesaurusNavigator.pmSearchModePopulateTopLevelClick

{-------------------------------------------------------------------------------
}
procedure TfraThesaurusNavigator.FetchConcepts(
  const AConceptSelectSP: String;
  AParams: array of Variant;
  ADisplayChildren: Boolean);
var
  lRecordset: _Recordset;
  lCursor: TCursor;
begin
  if fraDomainConceptGroupsSelector.cmbConceptGroups.ItemIndex > -1 then begin
    tvHierarchy.Items.BeginUpdate;
    lCursor := HourglassCursor;
    try
      ClearTreeView;
      lRecordset := dmGeneral.GetRecordset(AConceptSelectSP, AParams);

      while not lRecordset.EOF do begin
        AddTopLevelNode(lRecordset, ADisplayChildren);
        lRecordset.MoveNext;
        if Application.Terminated then Break;
      end;
      SelectFirstNodeInList;
    finally
      tvHierarchy.Items.EndUpdate;
      DefaultCursor(lCursor);
      CommonNameFetchQueue.ProcessWhenReady;
      // We need to fire an event at this point to get the Delete button correctly
      // disabled.
      if Assigned(OnFrameNotification) then
        OnFrameNotification(Self, etSetBrowseMode, nil);
    end; // try
  end; // if
end;

{-------------------------------------------------------------------------------
}
procedure TfraThesaurusNavigator.RefreshNodeCaption(
  ANode: TFlyNode;
  AConceptsToUpdate: TStringList);
var
  lKey, lCaption: String;
  lChildNode: TFlyNode;
begin
  if (ANode is TTermNode) then begin
    lKey := TTermNode(ANode).ConceptKey;
    if AConceptsToUpdate.IndexOf(lKey) <> -1 then
    begin
      lCaption := VarToStr(dmGeneral.GetStoredProcOutputParam('usp_Concept_Get',
                      ['@Key', lKey, '@GetPublishedTerm', True], '@Caption'));
      ANode.Caption := lCaption;
      if AppSettings.DisplayCommonNames then begin
        CommonNameFetchQueue.Add(lKey, ANode);
        CommonNameFetchQueue.ProcessWhenReady;
      end;
      dmInterface.RepaintNode(ANode, tvHierarchy);
      AConceptsToUpdate.Delete(AConceptsToUpdate.IndexOf(lKey));
    end;

    //Since changes to the selected rule affect all child nodes, have to update
    //the caption for each child node.
    if AConceptsToUpdate.Count > 0 then
    begin
      if ANode.HasChildren then
      begin
        lChildNode := ANode.getFirstChild;
        RefreshNodeCaption(lChildNode, AConceptsToUpdate);
        lChildNode := ANode.GetNextChild(lChildNode);
        while Assigned(lChildNode) do
        begin
          RefreshNodeCaption(lChildNode, AConceptsToUpdate);
          lChildNode := ANode.GetNextChild(lChildNode);
        end;
      end;
    end;
  end;
end;

{-------------------------------------------------------------------------------
  Searches the current concept group for a match on the item supplied in the search edit box.
      If a single hit is found, then show it in the hierarchy.  If multiple items found,
      display a Find dialog to identify the correct one.  If no items found, display a message
      and clear the hierarchy.
}
procedure TfraThesaurusNavigator.RunSearch;
var
  lSearchResult: string;
begin
  with TSearchManager.Create do
    try
      // select appropriate search type
      if fraDomainConceptGroupsSelector.btnHistory.Down then begin
        SearchType := stTermInConceptGroupVersion;
        SearchKey := ConceptGroupVersionKey;
      end
      else begin
        SearchType := stTermInConceptGroup;
        SearchKey := ConceptGroupKey;
      end;
      lSearchResult := RunSearch(eSearch.Text);
      if lSearchResult <> '' then
        InternalDisplayConcept(lSearchResult);
    finally
      Free;
    end;
  CommonNameFetchQueue.ProcessWhenReady;
end;  // TfraThesaurusNavigator.RunSearch 

{-------------------------------------------------------------------------------
  Sets up the domain combo box and the concept group combo box to point to the concept group
      containing the identified concept.
}
procedure TfraThesaurusNavigator.SelectDomainAndConceptGroup(const AConceptKey: string);
begin
  with dmGeneral.GetRecordset('usp_DomainAndConceptGroup_Select_ForConcept',
      ['@ConceptKey', AConceptKey]) do begin
    fraDomainConceptGroupsSelector.SelectDomain(Fields['Domain_Key'].Value);
    // If not a current concept, select the old version
    fraDomainConceptGroupsSelector.btnHistory.Down := not
        Fields['Is_Current'].Value;
    if fraDomainConceptGroupsSelector.btnHistory.Down then
      fraDomainConceptGroupsSelector.SelectConceptGroup(
          Fields['Concept_Group_Version_Key'].Value)
    else
      fraDomainConceptGroupsSelector.SelectConceptGroup(
          Fields['Concept_Group_Key'].Value)
  end;
end;  // TfraThesaurusNavigator.SelectDomainAndConceptGroup

{-------------------------------------------------------------------------------
  Selects the first node in the tree view.  If there are no nodes, takes no action. 
}
procedure TfraThesaurusNavigator.SelectFirstNodeInList;
begin
  if tvHierarchy.Items.Count>0 then begin
    if Assigned(tvHierarchy.OnChange) then
      tvHierarchy.OnChange(Self, tvHierarchy.items.Item[0]);
    tvHierarchy.items.Item[0].Selected := True;
    // When we select a node, we want to focus the treeview
    if tvHierarchy.CanFocus then
      tvHierarchy.SetFocus;
  end
  // Want to call OnChange event even if no nodes, because it will then clear
  // then remove the ConceptDetails frame.
  else if Assigned(tvHierarchy.OnChange) then
    tvHierarchy.OnChange(Self, nil);
end;  // TfraThesaurusNavigator.SelectFirstNodeInList 

{-------------------------------------------------------------------------------
  Set default domain and concept group (as used in last session), or the concept group version
      if btnHistory was down.
}
procedure TfraThesaurusNavigator.SetDefaultSelections;
var
  lDomainKey: string;
  lGroupKey: string;
  lProcName: string;
begin
  if AppSettings.LastThesaurusConceptGroup <> '' then begin
    if AppSettings.LastThesaurusConceptGroup[1]<>'*' then begin
      lGroupKey := AppSettings.LastThesaurusConceptGroup;
      lProcName := 'usp_ConceptGroup_GetDomain';
    end
    else begin
      // Concept group version key - extract without the *
      lGroupKey := Copy(AppSettings.LastThesaurusConceptGroup, 2, 16);
      fraDomainConceptGroupsSelector.btnHistory.Down := True;
      lProcName :='usp_ConceptGroupVersion_GetDomain';
    end;
    if lGroupKey<>'' then begin
      try
        lDomainKey := dmGeneral.GetStoredProcOutputParam(
            lProcName,
            ['@Key', lGroupKey],
            '@Domain_Key');
        try
          fraDomainConceptGroupsSelector.SelectDomain(lDomainKey);
          fraDomainConceptGroupsSelector.SelectConceptGroup(lGroupKey);
        except
          on EDomainConceptGroupSelectorException do
            ; // nothing - item no longer available
        end; // try
      except
        on EOleException do begin
          // If the user's previous selection no longer exists, ignore it
          lGroupKey := '';
          lDomainKey := '';
        end;
      end;
    end;
  end; // if
end;  // TfraThesaurusNavigator.SetDefaultSelections

{-------------------------------------------------------------------------------
}
procedure TfraThesaurusNavigator.RefreshNodeRank(ATermNode: TTermNode);
var
  lRankKey: String;
begin
  with dmGeneral.GetRecordset('usp_Concept_Select', ['@ConceptKey', ATermNode.ConceptKey]) do
    lRankKey := VarToStr(Fields['Concept_Rank_Key'].Value);

  // if already in cache, use it, else load a new one
  ATermNode.Rank := FConceptRankCache.GetRank(lRankKey);
end;  // TfraThesaurusNavigator.RefreshNodeRank

{-------------------------------------------------------------------------------
  Assigns the supplied rank to the term node.  The rank details are obtained from the cache if
      available.
}
procedure TfraThesaurusNavigator.SetNodeRank(ATermNode: TTermNode; const ARankKey: string);
begin
  // if already in cache, use it, else load a new one
  ATermNode.Rank := FConceptRankCache.GetRank(ARankKey);
end;  // TfraThesaurusNavigator.SetNodeRank

{-------------------------------------------------------------------------------
}
procedure TfraThesaurusNavigator.tlbHistoryEnter(Sender: TObject);
begin
  inherited;
  ShowMessage('Enter');
end;  // TfraThesaurusNavigator.tlbHistoryEnter 

{-------------------------------------------------------------------------------
}
procedure TfraThesaurusNavigator.tvHierarchyChange(Sender: TObject; Node: TFlyNode);
begin
  //  inherited;
  pmNodeShowAncestorHierarchy.Enabled := Assigned(Node);
  pmNodeCopy.Enabled := Assigned(Node);
end;  // TfraThesaurusNavigator.tvHierarchyChange 

{-------------------------------------------------------------------------------
  Draw the hierarchy cell.  This is responsible for rendering the rank image, and also
      rendering the formatting in the term (supports <i></i>).
}
procedure TfraThesaurusNavigator.tvHierarchyDrawCell(Sender: TObject; aCanvas: TCanvas; ACol,
    ARow: Integer; Rect: TRect; State: TExGridDrawState);
var
  lRect: TRect;
  lNode: TTermNode;
  lColor: TColor;
begin
  lNode := TTermNode(tvHierarchy.GetNodeAtRow(ARow));
  lRect := Rect;
  lColor := aCanvas.Brush.Color;
  with tvHierarchy do begin
    lRect.Left := lRect.Left + Indent * (lNode.Level + 1);
    // blank out the area to draw onto, using TreeView back colour, leaving the + buttons
    aCanvas.Brush.Color := Color;
    aCanvas.FillRect(lRect);
    // Restor colour for the rest of the item
    aCanvas.Brush.Color := lColor;
    lRect.Top := lRect.Top + (DefaultRowHeight - Canvas.TextHeight('A')) div 2;
  end; // with tvHierarchy

  // Draw the rank if there is one, that will update lRect dimensions too.
  if Assigned(lNode.Rank) then
    dmInterface.DrawRank(aCanvas, lNode.Rank, lRect, taLeftJustify);

  // Re-blank out the area to draw onto, but using correct colour this time.
  // lRect should have been updated to also skip the rank, if any.
  aCanvas.FillRect(lRect);

  if ACol = 0 then
    dmInterface.DrawTerm(aCanvas, lRect, lNode.Text, gdSelected in State);
end;  // TfraThesaurusNavigator.tvHierarchyDrawCell

{-------------------------------------------------------------------------------
}
procedure TfraThesaurusNavigator.DropTerm(const Sender: TObject; const
    iFormat : integer; const iSourceData: TKeyList; const iTextStrings :
    TstringList; const iIsPasteOperation: boolean; var ioHandled : boolean);
begin
  inherited;
end;

{-------------------------------------------------------------------------------
  Expanding a node populates the children from the database if required. 
}
procedure TfraThesaurusNavigator.tvHierarchyExpanding(Sender: TObject; Node: TFlyNode; var
    AllowExpansion: Boolean);
begin
  if not (Node is TTermNode) then
    raise EThesaurusNavigatorException.Create(ResStr_NodeNotTermNode);
  if not TTermNode(Node).Populated then begin
    PopulateNode(TTermNode(Node));
  end;
end;  // TfraThesaurusNavigator.tvHierarchyExpanding 

{-------------------------------------------------------------------------------
  Ensure Show Ancestor Hierarchy option disabled when nothing selected.
}
procedure TfraThesaurusNavigator.tvHierarchySelectCell(Sender: TObject; ACol, ARow: Integer;
    var CanSelect: Boolean);
begin
  //  pmNodeShowAncestorHierarchy.Enabled := Assigned(tvHierarchy.Selected)
end;  // TfraThesaurusNavigator.tvHierarchySelectCell

{-------------------------------------------------------------------------------
}
function TfraThesaurusNavigator.CanDrag : Boolean;
begin
  Result := True;
end;  // TfraThesaurusNavigator.CanDrag
                                                                                
{-------------------------------------------------------------------------------
}
function TfraThesaurusNavigator.CanDrop : Boolean;
begin
  Result := False;
end;  // TfraThesaurusNavigator.CanDrop

{-------------------------------------------------------------------------------
  Override in editable Navigator.
}
procedure TfraThesaurusNavigator.CheckNodeCanDrop(APoint: TPoint; const ATable,
        AKeyField: string; var Accept: boolean);
begin
  Accept := False;
end;

{-------------------------------------------------------------------------------
  Override in editable Navigator.
}
function TfraThesaurusNavigator.GetTableList : TStringDynArray;
begin
  Result := nil;
end;

{-------------------------------------------------------------------------------
  OnSetShowHierarchy event allows a frame containing Thesaurus Navigator can
  react whenever the concept hierarchy is not being displayed e.g. it makes
  little sense in such cases to paste a concept to the top level. Setting
  ShowHierarchy through this method ensures the event handler of the containing
  frame will be called.
}
procedure TfraThesaurusNavigator.SetShowHierarchy(AValue: Boolean);
begin
  FShowHierarchy := AValue;
end;

{-==============================================================================
    TBaseThesaurusHistoryItem
===============================================================================}
{-------------------------------------------------------------------------------
  Accessor method.  Sets the concept key to be recalled by this history item. 
}
procedure TBaseThesaurusHistoryItem.SetConceptKey(const Value: string);
begin
  FConceptKey := Value;
end;  // TBaseThesaurusHistoryItem.SetConceptKey 

{-------------------------------------------------------------------------------
}
procedure TBaseThesaurusHistoryItem.SetThesaurusNavigator(Value: TfraThesaurusNavigator);
begin
  FThesaurusNavigator := Value;
end;  // TBaseThesaurusHistoryItem.SetThesaurusNavigator 

{-==============================================================================
    TThesaurusAncestryHistoryItem
===============================================================================}
{-------------------------------------------------------------------------------
  Recalls the concept by displaying it in the browser. 
}
procedure TThesaurusAncestryHistoryItem.Recall;
begin
  FThesaurusNavigator.DisplayAncestorHierarchy(FConceptKey, FLineageID, False);
end;  // TThesaurusAncestryHistoryItem.Recall 

{-------------------------------------------------------------------------------
  Accessor method.  Sets the Lineage ID. 
}
procedure TThesaurusAncestryHistoryItem.SetLineageID(Value: Integer);
begin
  FLineageID := Value;
end;  // TThesaurusAncestryHistoryItem.SetLineageID 

{-==============================================================================
    TThesaurusConceptHistoryItem
===============================================================================}
{-------------------------------------------------------------------------------
  Recalls the concept and its ancestry by displaying it in the browser.
}
procedure TThesaurusConceptHistoryItem.Recall;
begin
  FThesaurusNavigator.DisplayConcept(FConceptKey, False);
end;  // TThesaurusConceptHistoryItem.Recall

end.




