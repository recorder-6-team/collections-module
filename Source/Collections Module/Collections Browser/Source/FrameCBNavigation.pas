{===============================================================================
  Unit:        FrameCBNavigation.pas

  Defines:     TfraCBNavigation

  Description: Container frame. Enables drag and drop for tvNav treeview.

  Model:       CollectionsBrowserGeneral.mpb

  Created:     Sept 2003

  Last revision information:
    $Revision: 114 $
    $Date: 13/03/13 9:40 $
    $Author: Alexanderpadley $

===============================================================================}

unit FrameCBNavigation;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, BaseDragFrameUnit, exgrid, RapTree, StdCtrls, ComCtrls, ToolWin,
  ExtCtrls, ADODb, Menus, ActnList, BrowserNodeFramework, DataClasses,
  CommonNameFetchQueue, DataTypes, InterfaceDataModule, UserMessages,
  DropSource, BrowserViewTypes, HistoryManager, BrowserNodeCommon, XPMenu,
  Recorder2000_TLB, ComboListID, LuxIDComboBox, ConceptGroupComboBox,
  ExceptionForm, DSSDataTypes;

type
  TOnSearchClick = procedure (Sender: TObject; const AViewType: TBaseViewType) of object;
  ECBNavigation = class(TExceptionPath)
  end;
  
  TfraCBNavigation = class(TBaseDragFrame, IRequestor)
    btnGo: TButton;
    btnSearchType: TButton;
    cmbSearch: TConceptGroupComboBox;
    cmbView: TComboBoxEx;
    DragFrame: TShape;
    eSearch: TEdit;
    Label1: TLabel;
    pmExpandList: TMenuItem;
    pmNavigate: TMenuItem;
    pmSearch: TPopupMenu;
    pmSep1: TMenuItem;
    pmTree: TPopupMenu;
    pmTreeBack: TMenuItem;
    pmTreeConvertSep: TMenuItem;
    pmTreeDuplicateValuation: TMenuItem;
    pmTreeForward: TMenuItem;
    pmTreeNewWindow: TMenuItem;
    pmTreeRefresh: TMenuItem;
    pnlFilter: TPanel;
    pnlView: TPanel;
    tbtnBack: TToolButton;
    tbtnForward: TToolButton;
    tlbTree: TToolBar;
    ToolButton1: TToolButton;
    tvNav: TRapidTree;
    procedure IRequestor.Update = ReturnDataToSearchBox;
    procedure btnGoClick(Sender: TObject);
    procedure btnSearchTypeClick(Sender: TObject);
    procedure cmbSearchPopulate(Sender: TObject);
    procedure cmbViewChange(Sender: TObject);
    procedure eSearchEnter(Sender: TObject);
    procedure eSearchExit(Sender: TObject);
    procedure eSearchKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure eSearchKeyPress(Sender: TObject; var Key: Char);
    procedure FrameResize(Sender: TObject);
    procedure pmNavigateClick(Sender: TObject);
    procedure pmTreeDuplicateValuationClick(Sender: TObject);
    procedure pmTreePopup(Sender: TObject);
    procedure tvNavChange(Sender: TObject; Node: TFlyNode);
    procedure tvNavClick(Sender: TObject);
    procedure tvNavDblClick(Sender: TObject);
    procedure tvNavDrawCell(Sender: TObject; aCanvas: TCanvas; ACol, ARow: Integer; Rect:
        TRect; State: TExGridDrawState);
    procedure tvNavExpanding(Sender: TObject; Node: TFlyNode; var AllowExpansion: Boolean);
    procedure tvNavMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y:
        Integer);
  private
    FAllowNavigation: Boolean;
    FCommonNameFetchQueue: TCommonNameFetchQueue;
    FDraggedNode: TFlyNode;
    FHistoryManager: THistoryManager;
    FLastHintNode: TBrowserNode;
    FLastHintPos: TPoint;
    FLastSearchType: String;
    FOnFrameNotification: TFrameNotificationEvent;
    FTreeExpandToggle: Boolean;
    FViewTypeManager: TViewTypeManager;
    FXPMenu: TXPMenu;
    FImageMagickInstalled: Boolean;
    FImageMagickInstallationChecked: Boolean;
    FFirstOfMultiSelect: TFlyNode;
    FGroupDeterminationsByDomain: Boolean;
    function CheckAcceptSearchKeylist(const KeyList: IKeyList; const ATableName: string = ''):
        Boolean;
    function CheckNodeAcceptsTable(ANode: TFlyNode; const ATable: string): Boolean;
    procedure CheckNodeCanDrop(APoint: TPoint; const ATable, AFieldKey: string; var Accept: boolean);
    procedure CheckNodeCanDropFile(APoint: TPoint; const AFiles: TStringList; var Accept: boolean);
    procedure DoExternalNavigation(ANode: TFlyNode);
    procedure DoNavigation(const AHyperlinkKey: string; ANodeContext: TNodeContext;
        const ACaption: string; AImageIndex: integer);
    procedure DragNode(const Sender: TObject; var dropSource: TJNCCDropSource);
    procedure DropFilesOnNode(ANode: TFlyNode; AFiles: TStringList);
    procedure DropNode(const Sender: TObject; const iFormat : integer; const iSourceData:
        TKeyList; const iTextStrings : TstringList; const iIsPasteOperation: boolean; var
        ioHandled : boolean);
    procedure DropNodeOnFolder(ANode: TFlyNode; const iSourceData: TKeyList);
    procedure DropNodeOnTopLevel(ANode: TFlyNode; const iSourceData: TKeyList);
    function FolderAcceptsChildrenOfType(ANode: TFolderNode; ATable: String): Boolean;
    procedure FormShowHint(var HintStr: String; var CanShow: Boolean; var HintInfo: THintInfo);
    function GetSearchText: String;
    function GetTargetNode(const iIsPasteOperation: Boolean): TFlyNode;
    function Get_KeyList(ANode: TFlyNode): IKeyList; safecall;
    procedure InternalPopulateTree(ANodeContext: TNodeContext; const AStoredProcName: string =
        ''; AParams: TVariantArray = nil; const AKey: string = ''; const AHistoryCaption:
        string = ''; AHistoryImageIndex: integer = -1; AAddToHistory: boolean = true);
    procedure RefreshXPMenu;
    procedure ReturnDataToSearchBox(const KeyList: IKeyList); safecall;
    procedure ReturnKeyListToSearchBox(AKeylist: TKeyList);
    function ReturnSearchPopup: TPopupMenu;
    procedure SearchDragOverCheck(APoint: TPoint; const ATable, AFieldKey: string; var Accept: boolean);
    procedure SearchDrop(const Sender: TObject; const iFormat : integer; const iSourceData:
        TKeyList; const iTextStrings : TstringList; const iIsPasteOperation: boolean; var
        ioHandled : boolean);
    procedure SearchPopupMenuHandler(Sender: TObject);
    procedure SearchTypeChange(Sender: TObject);
    procedure SetAllowNavigation(const Value: Boolean);
    procedure SetOnFrameNotification(Value: TFrameNotificationEvent);
    procedure SetSearchText(const Value: String);
    procedure UpdateBrowserNodePointer(APointer: integer);
    procedure WMNavigate(var Message: TMessage); message WM_NAVIGATE;
    procedure WMRefreshNode(var Message: TMessage); message WM_REFRESH_NODE;
    procedure WMRemoveDeletedNode(var Message: TMessage); message WM_REMOVE_DELETED_NODE;
    procedure WMRemoveDraggedNode(var Message: TMessage); message WM_REMOVE_DRAGGED_NODE;
    function ImageMagickInstalled: Boolean;
  protected
    function CheckTableSupported(ADropComponentIndex: integer; const ATableName: string):
        Boolean; override;
    procedure RegisterDragDropComponents; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AddItemsToTree(AKeyList: TKeyList);
    procedure AddItemToTree(const AKey: string);
    procedure ClearTreeView;
    procedure DeleteNode(ANode: TFlyNode);
    procedure DisplayCommonName(ADeterminationKey: string; ANode: TFlyNode);
    function GetHistoryCaption: String;
    function GetStoredProcName: String;
    function GetStoredProcParams: TVariantArray;
    procedure RepopulateTree(ANodeContext: TNodeContext; const AStoredProcName: string = '';
        AParams: TVariantArray = nil; const AKey: string = ''; const AHistoryCaption: string =
        ''; AHistoryImageIndex: integer = -1; AAddToHistory: boolean = true);
    procedure SetTreeMenuEnabled;
    function ViewTypeManager: TViewTypeManager;
    function ViewTypeManagerAllocated: Boolean;
    procedure ViewTypeManagerDiscard;
    property AllowNavigation: Boolean read FAllowNavigation write SetAllowNavigation;
    property DraggedNode: TFlyNode read FDraggedNode write FDraggedNode;
    property HistoryManager: THistoryManager read FHistoryManager;
    property OnFrameNotification: TFrameNotificationEvent read FOnFrameNotification write
        SetOnFrameNotification;
    property SearchText: String read GetSearchText write SetSearchText;
    property XPMenu: TXPMenu read FXPMenu write FXPMenu;
    property GroupDeterminationsByDomain: Boolean
        read FGroupDeterminationsByDomain;
    procedure SelectFolder(ATitle: String; AExpand: Boolean);
  end;

  TCBNavigationHistoryItem = class(TBaseHistoryItem)
  private
    FCBNavigation: TfraCBNavigation;
    FKey: String;
    FNodeContext: TNodeContext;
    FSearchText: String;
    FStoredProc: String;
    FStoredProcParams: TVariantArray;
  protected
    procedure Recall; override;
  public
    property CBNavigation: TfraCBNavigation read FCBNavigation write FCBNavigation;
    property Key: String read FKey write FKey;
    property NodeContext: TNodeContext read FNodeContext write FNodeContext;
    property SearchText: String read FSearchText write FSearchText;
    property StoredProc: String read FStoredProc write FStoredProc;
    property StoredProcParams: TVariantArray read FStoredProcParams write FStoredProcParams;
  end;
  
//==============================================================================
implementation

uses
  GeneralData, BrowserNodeCollectionUnits, BrowserNodeSpecimen, ApplicationSettings,
  ResourceStrings, VagueDate, SearchManager, LuxembourgConstants, Validation,
  Generalfunctions, DropStruct, Clipbrd, DropTarget, BrowserNodeMovement,
  BaseADODataModule, COMClasses, ThesaurusBrowser_TLB, BaseTabSheetFrameUnit,
  Treecoll, ComObj, StrUtils;

{$R *.dfm}

{-==============================================================================
    TfraCBNavigation
===============================================================================}
{-------------------------------------------------------------------------------
}
constructor TfraCBNavigation.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  
  Application.OnShowHint := FormShowHint;
  FHistoryManager := THistoryManager.Create;
  FHistoryManager.BackButton := tbtnBack;
  FHistoryManager.ForwardButton := tbtnForward;
  pmTreeBack.Action := FHistoryManager.BackAction;
  pmTreeForward.Action := FHistoryManager.ForwardAction;
  FHistoryManager.ImageList := dmInterface.ilBrowserNodes;
  
  FCommonNameFetchQueue := TCommonNameFetchQueue.Create(tvNav);
  FCommonNameFetchQueue.Parent := tvNav;
  FCommonNameFetchQueue.TreeView := tvNav;

  FGroupDeterminationsByDomain := AppSettings.GroupDeterminationsByDomain;
end;  // TfraCBNavigation.Create

{-------------------------------------------------------------------------------
}
destructor TfraCBNavigation.Destroy;
begin
  if Assigned(FCommonNameFetchQueue) then FCommonNameFetchQueue.Free;
  FHistoryManager.Free;
  
  inherited Destroy;
end;  // TfraCBNavigation.Destroy 

{-------------------------------------------------------------------------------
  Adds a list of items to the tree using a keylist to get the keys.  The View Type Manager
      should be already set to the correct context.
}
procedure TfraCBNavigation.AddItemsToTree(AKeyList: TKeyList);
var
  i: Integer;
begin
  for i := 0 to ViewTypeManager.ViewTypeCount - 1 do
    if SameText(
         AKeyList.Header.TableName,
         ViewTypeManager.ViewType[i].TopLevelNodeClass.ClassTableName) then
    begin
      ViewTypeManager.Selected := ViewTypeManager.ViewType[i];
      Break;
    end;
  // Prepare a temp table used to select the list of specimens to show
  dmGeneral.ExecuteSQL(SQL_CREATETEMPFILTER);
  try
    for i := 0 to AKeyList.Header.ItemCount - 1 do
      dmGeneral.ExecuteSQL(Format(SQL_INSERTTEMPFILTER, [AKeyList.Items[i].KeyField1]));
    // Show the specimens
    InternalPopulateTree(ViewTypeManager.Selected.NodeContext, '', nil, '');
  finally
    dmGeneral.ExecuteSQL(SQL_DROPTEMPFILTER);
  end;
end;  // TfraCBNavigation.AddItemsToTree

{-------------------------------------------------------------------------------
  Populate the top level of the tree
}
procedure TfraCBNavigation.AddItemToTree(const AKey: string);
begin
  InternalPopulateTree(ViewTypeManager.Selected.NodeContext, '', nil, AKey);
end;  // TfraCBNavigation.AddItemToTree

{-------------------------------------------------------------------------------
  Runs the default search if a search string is provided, else returns all records for the
      currently selected ViewType.
}
procedure TfraCBNavigation.btnGoClick(Sender: TObject);
var
  lCursor: TCursor;
  lDummy: Double;
begin
  lCursor := HourglassCursor;
  try
    try
      with ViewTypeManager.Selected do
        if SearchControlType[SearchDefaultIndex] = ctNumber then begin
          // Validate number searches only contain numbers
          if (SearchText <> '') and
             (not TextToFloat(PChar(SearchText), lDummy, fvExtended)) then
            Raise ECBNavigation.CreateValidation(ResStr_SuppyValidNumber, eSearch);
        end;
      RepopulateTree(
          ViewTypeManager.Selected.NodeContext,
          GetStoredProcName,
          GetStoredProcParams,
          '',
          GetHistoryCaption,
          ViewTypeManager.Selected.ImageIndex);
      with ViewTypeManager.Selected do
        FLastSearchType := SearchCaption[SearchDefaultIndex]
    except
      on EVagueDateError do
        MessageDlg(Trim(SearchText) + ResStr_IsNotAValidDate, mtInformation, [mbOk], 0);
      on EConvertError do
        MessageDlg(Trim(SearchText) + ResStr_IsNotAValidDate, mtInformation, [mbOk], 0);
    end; //try

    tvNav.SetFocus;
  finally
    DefaultCursor(lCursor);
  end;
end;  // TfraCBNavigation.btnGoClick 

{-------------------------------------------------------------------------------
  Uses the current ViewType to return a search popup menu containing all items that are
      allowed to be searched on.
}
procedure TfraCBNavigation.btnSearchTypeClick(Sender: TObject);
var
  lPosPopup: TPoint;
  lSearchPopup: TPopupMenu;
begin
  // Work out where to show the popupmenu so that it appears just under the button
  lPosPopup := ClientToScreen(Point(pnlFilter.Left + btnSearchType.Left,
                                    pnlFilter.Top + btnSearchType.Top +
                                    btnSearchType.Height));
  // For menu to appear at calculated position
  lSearchPopup := ReturnSearchPopup;
  if Assigned(lSearchPopup) then lSearchPopup.Popup(lPosPopup.X, lPosPopup.Y);
  tvNav.SetFocus;
end;  // TfraCBNavigation.btnSearchTypeClick 

{-------------------------------------------------------------------------------
  Checks if the keylist contains something valid for the current search type.  For example, if
      the search type is a location search, then checks the keylist contains location data.
      Either supply a keylist or a tablename string.
}
function TfraCBNavigation.CheckAcceptSearchKeylist(const KeyList: IKeyList; const ATableName:
    string = ''): Boolean;
var
  lTableName: String;
begin
  if (ATableName='') and Assigned(KeyList) then begin
    // Find the table name from the keylist
    if (KeyList.TableName = 'MIXED') and (KeyList.ItemCount > 0) then
      lTableName := KeyList.GetKeyItem(0).KeyField2
    else
      lTableName := KeyList.TableName;
  end else
    lTableName := ATableName;

  // Check if this is appropriate for the search type
  with FViewTypeManager.Selected do
    case SearchControlType[SearchDefaultIndex] of
      ctIndividual:
        Result := SameText(lTableName, TN_NAME) or
                  SameText(lTableName, TN_INDIVIDUAL);
      ctName:
        Result := SameText(lTableName, TN_NAME) or
                  SameText(lTableName, TN_INDIVIDUAL) or
                  SameText(lTableName, TN_ORGANISATION);
      ctLocation:
        Result := SameText(lTableName, TN_LOCATION);
      ctDetermination:
        Result := SameText(lTableName, TN_TAXON_LIST_ITEM) or
                  SameText(lTableName, TN_CONCEPT);
    else
      Result := False;
    end;
end;  // TfraCBNavigation.CheckAcceptSearchKeylist

{-------------------------------------------------------------------------------
  Check that the supplied node can accept drag and drop or paste operations for a table.
}
function TfraCBNavigation.CheckNodeAcceptsTable(ANode: TFlyNode; const ATable: string): Boolean;
var
  lNode: TFlyNode;
  
  // Method to see if folder accepts table. Put into local proc for code tidiness.
  function CheckFolderAcceptsTable(ANode: TFolderNode; ATable: String): Boolean;
  var
    lIdx: Integer;
  begin
    Result := False;
    // Only should be allowed to drop nodes on nodes where the add button is enabled.
    if TFolderNode(lNode).CanAdd then
      // Find if folder has a Link to exsiting option, otherwise no drop possible
      for lIdx := 0 to TFolderNode(lNode).AddButtonMenuCaptionsCount - 1 do
        Result := Result or (not TFolderNode(lNode).AddMenuIsAdd[lIdx]);
      // Find if the folder accepts children of the correct type.
      Result := Result and FolderAcceptsChildrenOfType(TFolderNode(lNode), ATable);
  end;

  // Method to see if top node accepts table. Put into local proc for code tidiness.
  function CheckTopLevelNodeAcceptsTable(ANode: THyperlinkTopLevelNode; ATable: String): Boolean;
  var
    lIdx: Integer;
  begin
    Result := False;
    // Only should be allowed to drop nodes on nodes where the add button is enabled.
    if THyperlinkTopLevelNode(lNode).CanAdd then
      // Find if folder has a Link to exsiting option, otherwise no drop possible
      for lIdx := 0 to THyperlinkTopLevelNode(lNode).AddButtonMenuCaptionsCount - 1 do
        Result := Result or (not THyperlinkTopLevelNode(lNode).AddMenuIsAdd[lIdx]);
      // Find if the folder accepts children of the correct type.
      Result := Result and SameText(THyperlinkTopLevelNode(ANode).ClassTableName, ATable);
  end;

begin
  Result := False;
  if ANode is TLeafNode and Assigned(ANode.Parent) then
    lNode := TLeafNode(ANode).Parent
  else
    lNode := ANode;

  //Some leaf nodes are contained beneath 2 folders
  if (lNode is TFolderNode) and Assigned(lNode.Parent) then
    if lNode.Parent is TFolderNode then
      if TFolderNode(lNode).ClassTableName = '' then
        lNode := TFolderNode(lNode).Parent;

  if Assigned(lNode) then
    if lNode is TFolderNode then
      Result := CheckFolderAcceptsTable(TFolderNode(lNode), ATable)
    else
      Result := CheckTopLevelNodeAcceptsTable(THyperlinkTopLevelNode(lNode), ATable);
end;  // TfraCBNavigation.CheckNodeAcceptsTable 

{-------------------------------------------------------------------------------
  Checks that the cursor is above a treenode that can accept a drop.
}
procedure TfraCBNavigation.CheckNodeCanDrop(APoint: TPoint; const ATable, AFieldKey: string;
    var Accept: boolean);
var
  lClickPoint: TPoint;
  lNode: TFlyNode;
begin
  Accept := False; // default
  // Find dragged over node
  lClickPoint := tvNav.ScreenToClient(APoint);
  lNode := tvNav.GetNodeAt(lClickPoint.X, lClickPoint.Y);
  Accept := CheckNodeAcceptsTable(lNode, ATable);
  // Don't allow drag onto self
  if (lNode is TBrowserNode) and Accept then
    Accept := (TBrowserNode(lNode).TableName <> ATable) or (TBrowserNode(lNode).Key <> AFieldKey);
end;  // TfraCBNavigation.CheckNodeCanDrop

{-------------------------------------------------------------------------------
  Checks that the cursor is above a treenode that can accept a file drop.
}
procedure TfraCBNavigation.CheckNodeCanDropFile(APoint: TPoint;
  const AFiles: TStringList; var Accept: boolean);
var
  lClickPoint: TPoint;
  lNode: TFlyNode;
begin
  Accept := False;  // default
  if AFiles.Count > 0 then begin
    if ImageMagickInstalled then begin
      lClickPoint := tvNav.ScreenToClient(APoint);
      lNode := tvNav.GetNodeAt(lClickPoint.X, lClickPoint.Y);
      if lNode is TBrowserNode then
        Accept := TBrowserNode(lNode).CanAcceptFiles(AFiles);
    end;    // if ImageMagickInstalled
  end;    // if AFiles.Count > 0
end;    // TfraCBNavigation.CheckNodeCanDropFile

{-------------------------------------------------------------------------------
  Uses default behaviour to check if a component supports the Table to be dropped from unless
      the component is he treeview then it checks with the node.
}
function TfraCBNavigation.CheckTableSupported(ADropComponentIndex: integer; const ATableName:
    string): Boolean;
var
  lActiveControl: TWinControl;
begin
  lActiveControl := GetActiveControl(self);

  if (lActiveControl = tvNav) and Assigned(tvNav.Selected) then
    Result := CheckNodeAcceptsTable(tvNav.Selected, ATableName)
  else
    Result :=
        TClipboardCapability(DropComponentList.Objects[ADropComponentIndex]).IsTableSupported(ATableName);
end;  // TfraCBNavigation.CheckTableSupported

{-------------------------------------------------------------------------------
  Clears the treeview.
}
procedure TfraCBNavigation.ClearTreeView;
var
  lDummy: Boolean;
  lArray: TVariantArray;
begin
  SetLength(lArray, 0);
  FCommonNameFetchQueue.Clear;
  if Assigned(tvNav.OnChanging) then
    tvNav.OnChanging(nil, nil, lDummy);
  tvNav.Items.Clear;
  FDraggedNode := nil;
  pmSearch.Items.Clear;
  pmExpandList.Enabled := false;
  if Assigned(FOnFrameNotification) then
    OnFrameNotification(self, etDestroyDetailsContainer, lArray);
end;  // TfraCBNavigation.ClearTreeView 

{-------------------------------------------------------------------------------
  Populate the search combo box when dropped down.  This is context sensitive to the search
      type.
}
procedure TfraCBNavigation.cmbSearchPopulate(Sender: TObject);
  
  // Populate the combo using a concept group
  procedure PopulateConceptGroup(const AKey: string);
  begin
    with dmGeneral.GetRecordset('usp_Concept_Select_ForConceptGroup', ['@ConceptGroupKey', AKey]) do
    begin
      while not EOF do begin
        cmbSearch.Add(VarToStr(Fields['Item_Name'].Value), VarToStr(Fields['Concept_Key'].Value));
        MoveNext;
      end; // while
    end; // with
  end;

begin
  inherited;
  with FViewTypeManager.Selected do
    case SearchControlType[SearchDefaultIndex] of
      ctConditionCheckConditionCombo:
        PopulateConceptGroup(CG_STORE_TYPE);
      ctStatusCombo:
        begin
          cmbSearch.Add(ResStr_Pending, 0);
          cmbSearch.Add(ResStr_Open, 1);
          cmbSearch.Add(ResStr_Closed, 2);
          cmbSearch.Add(ResStr_Postponed, 3);
          cmbSearch.Add(ResStr_Abandoned, 4);;
        end;
      ctSpecimenTypeCombo:
        with dmGeneral.GetRecordset('usp_SpecimenTypes_Select', ['@Mask', 0]) do begin
          while not EOF do begin
            cmbSearch.Add(VarToStr(Fields['PlainText'].Value), VarToStr(Fields['Concept_Key'].Value));
            MoveNext;
          end; // while
        end; // with
      ctStoreTypeCombo:
        PopulateConceptGroup(CG_STORE_TYPE);
      ctPriorityCombo: begin
          cmbSearch.Add(ResStr_Low, 0);
          cmbSearch.Add(ResStr_Medium, 1);
          cmbSearch.Add(ResStr_High, 2);
          cmbSearch.Add(ResStr_Urgent, 3);
        end;
      ctTaskTypeCombo:
        PopulateConceptGroup(CG_CONSERVATION_TASK_TYPES);
    end;
end;  // TfraCBNavigation.cmbSearchPopulate 

{-------------------------------------------------------------------------------
  Sets the enabled state of the search-menu drop-down button as well as clearing the treeview
      and search menu.
}
procedure TfraCBNavigation.cmbViewChange(Sender: TObject);
begin
  inherited;
  ClearTreeView;
  SearchTypeChange(nil);
end;  // TfraCBNavigation.cmbViewChange 

{-------------------------------------------------------------------------------
  Using the normal method of deletion will cause the top node of the treeview to be
      momentarily selected (this happens in TFlyNodes.Removed in the line FOwner.Row := 0).
      This will cause the OnChange and OnChanging events to be fired and a frame to be quickly
      loaded then removed. This can cause ASync problems. Hence, if you want to delete a node,
      use this method because it unlinks the relevant events, deletes the node and then
      relinks them.
}
procedure TfraCBNavigation.DeleteNode(ANode: TFlyNode);
var
  lEvent1: TFTVChangedEvent;
  lEvent2: TFTVChangingEvent;
begin
  with tvNav do begin
    lEvent1 := OnChange;
    lEvent2 := OnChanging;
    OnChange := nil;
    OnChanging := nil;
    try
      if FDraggedNode = ANode then FDraggedNode := nil;
      ANode.Delete;
    finally
      OnChange := lEvent1;
      OnChanging := lEvent2;
    end;
  end;
end;  // TfraCBNavigation.DeleteNode 

{-------------------------------------------------------------------------------
}
procedure TfraCBNavigation.DisplayCommonName(ADeterminationKey: string; ANode: TFlyNode);
begin
  if Assigned(ANode) then begin
    FCommonNameFetchQueue.Add(ADeterminationKey, ANode);
    FCommonNameFetchQueue.ProcessWhenReady;
  end;
end;  // TfraCBNavigation.DisplayCommonName 

{-------------------------------------------------------------------------------
  Handles double clicking on hyperlink leaf nodes that need to navigate outside the
      Collections Browser (e.g. to Names and Addresses module or Thesaurus Browser).
}
procedure TfraCBNavigation.DoExternalNavigation(ANode: TFlyNode);
var
  lClassTableName: String;
begin
  lClassTableName := TBrowserNode(ANode).ClassTableName;
  
  // Launch the Thesaurus Browser or use the current one.
  if lClassTableName = TN_CONCEPT then
    dmGeneral.Recorder.ShowActiveForm(CLASS_frmThesaurusBrowser);
  
  dmGeneral.Recorder.DisplayData(lClassTableName, Get_KeyList(ANode));
  
  SearchText := '';
end;  // TfraCBNavigation.DoExternalNavigation 

{-------------------------------------------------------------------------------
  Navigate to a particular Node type and Key
}
procedure TfraCBNavigation.DoNavigation(const AHyperlinkKey: string; ANodeContext:
    TNodeContext; const ACaption: string; AImageIndex: integer);
begin
  RepopulateTree(ANodeContext, '', nil, AHyperlinkKey, Trim(ACaption), AImageIndex);
  
  SearchText := '';
end;  // TfraCBNavigation.DoNavigation 

{-------------------------------------------------------------------------------
  Drag handler to retrieve a keylist for the selected node.
}
procedure TfraCBNavigation.DragNode(const Sender: TObject; var dropSource: TJNCCDropSource);
var
  node: TFlyNode;
  keyList: TEditableKeyList;
begin
  node := tvNav.Items.GetFirstSelectedNode;
  if Assigned(node) then begin
    keyList := TEditableKeyList.Create;
    KeyList.SetTable(TBrowserNode(node).TableName);

    while Assigned(node) do begin
      // Add key, plus a pointer to self so it can be refreshed after a drag operation
      keyList.AddItem(TBrowserNode(node).DragDropKey, IntToStr(Integer(Self)));
      node := tvNav.Items.GetNextSelectedNode(node);
    end;
    dropSource.AssignKeyList(keyList);
  end;
  
  FDraggedNode := tvNav.Selected;
end;  // TfraCBNavigation.DragNode 

{-------------------------------------------------------------------------------
  Handle dropping of a list of files onto a tree node.
}
procedure TfraCBNavigation.DropFilesOnNode(ANode: TFlyNode; AFiles: TStringList);
var
  lBrowserNode: TBrowserNode;

    function FindNodeThatAcceptsFiles: TBrowserNode;
    var
      i: Integer;
    begin
      Result := nil;
      if TBrowserNode(ANode).IsFileAcceptor(AFiles) then
        Result := TBrowserNode(ANode)
      else begin
        // Ensure that the node's items have been populated
        // But ONLY if it's not already expanded.
        if not ANode.Expanded then
          TBrowserNode(ANode).Refresh;

        for i := 0 to Pred(ANode.Count) do
          if (ANode.Items[i] is TBrowserNode) and
             TBrowserNode(ANode.Items[i]).IsFileAcceptor(AFiles) then
          begin
            Result := TBrowserNode(ANode.Items[i]);
            Break;
          end;
      end;
    end;
    
begin
  if ANode is TBrowserNode then begin
    lBrowserNode := FindNodeThatAcceptsFiles;
    if Assigned(lBrowserNode) then
      lBrowserNode.AddFiles(AFiles)
    else
      // Raising an exception is no use because it doesn't seem to be picked up
      //  by MadExcept and then prevents Recorder from shutting down cleanly.
      MessageDlg(ResStr_CannotDropFile, mtInformation, [mbOk], 0);
  end;    // if ANode is TBrowserNode
end;    // TfraCBNavigation.DropFilesOnNode

{-------------------------------------------------------------------------------
  Handle dropping of a node onto the tree view.
}
procedure TfraCBNavigation.DropNode(const Sender: TObject; const iFormat : integer; const
    iSourceData: TKeyList; const iTextStrings : TstringList; const iIsPasteOperation: boolean;
    var ioHandled : boolean);
var
  lNode: TFlyNode;
begin
  try
    lNode := GetTargetNode(iIsPasteOperation);
    ioHandled := True;
    if Assigned(lNode) then
      if iFormat = CF_HDROP then
        // Dropping a list of files onto the node
        DropFilesOnNode(lNode, iTextStrings)
      else
      if (lNode is TFolderNode) then
        DropNodeOnFolder(lNode, iSourceData)
      else
      if (lNode is THyperlinkTopLevelNode) then
        DropNodeOnTopLevel(lNode, iSourceData);
  except
    on E:TExceptionPath do
      // Since drop node could have been called from COM, non-critical errors
      // must be handled here, as COM exceptions are always critical
      if not TExceptionPath(E).Critical then
        ShowInformation(E.Message)
      else
        raise;
  end;
end;  // TfraCBNavigation.DropNode 

{-------------------------------------------------------------------------------
}
procedure TfraCBNavigation.DropNodeOnFolder(ANode: TFlyNode; const iSourceData: TKeyList);
var
  lAccept: Boolean;
  i: Integer;
  lNewNode: TBrowserNode;
  lSelectedNode: TFlyNode;
  lCaption: String;
begin
  lSelectedNode := tvNav.Selected;
  lNewNode := nil;
  // Find if folder has a Link to exsiting option, otherwise no drop possible
  lAccept := False;
  for i := 0 to TFolderNode(ANode).AddButtonMenuCaptionsCount-1 do
    lAccept := lAccept or (not TFolderNode(ANode).AddMenuIsAdd[i]);
  
  with iSourceData do begin
    // Find if the folder accepts children of the correct type.
    lAccept := lAccept and
               SameText(TFolderNode(ANode).ChildNodeType[0].ClassTableName, Header.TableName);

    if (Header.ItemCount > 1) and lAccept then
      lAccept := ConfirmYesNo(Format(ResStr_MultipleDrop,
                              [Header.ItemCount, TFolderNode(ANode).Text])) = mrYes;

    if lAccept then begin
      TFolderNode(ANode).ResetConfirmation;
      TFolderNode(ANode).ConfirmApplyToAllOption := Header.ItemCount > 1;

      for i := 0 to Header.ItemCount - 1 do
      begin
         { KeyField2 is pointer to Source node in Collections Browser, so use
           that to get the caption for the new node. Otherwise, use default 'new node'.}
        if IsInt(Items[i].KeyField2) and
           (TObject(StrToInt(Items[i].KeyField2)) is TBrowserNode) then
          lCaption := TBrowserNode(StrToInt(Items[i].KeyField2)).Caption
        else
        // Try to grab a decent caption if possible
        if TFolderNode(ANode).NodeContext = ncSpecimen then
          lCaption := ConvertSpecimenKeyToCaption(Items[i].KeyField1, TN_SPECIMEN_UNIT)
        else
          lCaption := ResStr_NewNode;

        if TFolderNode(ANode).ValidateNewNode(Items[i].KeyField1, lCaption) and
           TFolderNode(ANode).ConfirmNodeAction(Items[i].KeyField1, lCaption) then
        begin
          { Put in a try/except because the user could be attempting to move
            a Movement node to a Folder that also uses the Movement table, but
            doesn't permit this movement type. An critical EBrowserNodeError would
            be thrown, so catch it and display a friendlier non critical message. }
          try
            lNewNode := TFolderNode(ANode).LinkNode(Items[i].KeyField1, lCaption);
          except
            on E: EBrowserNodeError do
              raise EBrowserFrameError.CreateNonCritical(ResStr_NodeCannotBeMovedHere);
          end;

          if Assigned(lNewNode) then begin
            try
              // If the node is a Hyperlink Leaf Node, then the join key
              // should be given to the node.
              if lNewNode is THyperlinkLeafNode then
                THyperlinkLeafNode(lNewNode).JoinKey :=
                    TFolderNode(lNewNode.Parent).UpdateNodeRelationship(lNewNode.Key)
              // If not, just run UpdateNodeRelationship anyway.
              else
                TFolderNode(lNewNode.Parent).UpdateNodeRelationship(lNewNode.Key);
            except
              on EOleException do begin
                if dmGeneral.Connection.Errors.Count > 0 then begin
                  FreeAndNil(lNewNode);
                  raise EBrowserFrameError.CreateNonCritical(ResStr_NodeDragFailedForeignKey);
                end;
                raise; // raise error, assuming it wasn't raised as non critical above
              end;
            end; // try

            { Nodes dragged from the Thesaurus Browser use their KeyField2
              to store the key of the parent (not the pointer to the node,
              as is done in the Collections Browser. Hence, the caption
              has to be retrieved from the database. }
            if not IsInt(Items[i].KeyField2) then
              lNewNode.Caption := VarToStr(dmGeneral.GetStoredProcOutputParam
                                  ('usp_ConceptName_Get', ['@Key', Items[i].KeyField1],
                                   '@Caption'));
          end;
        end;
      end;

      try
        if Assigned(lNewNode) then
          if IsInt(Items[0].KeyField2) then
            tvNav.Selected := lNewNode
          else begin
            tvNav.Selected := lSelectedNode;
            if Assigned(FOnFrameNotification) then
              OnFrameNotification(Self, etRefreshDetailsContainer, nil);
            TFolderNode(lNewNode.Parent).Refresh;
          end;
      except
      end;

      // Refresh the source - doing the first item alone should be sufficient.
      if IsInt(Items[0].KeyField2) then
        UpdateBrowserNodePointer(StrToInt(Items[0].KeyField2));
    end;
  end;  // with
end;  // TfraCBNavigation.DropNodeOnFolder 

{-------------------------------------------------------------------------------
}
procedure TfraCBNavigation.DropNodeOnTopLevel(ANode: TFlyNode; const iSourceData: TKeyList);
var
  lAccept: Boolean;
  i: Integer;
  lNewNode: TBrowserNode;
  lSelectedNode: TFlyNode;
  lCaption: String;
begin
  lSelectedNode := tvNav.Selected;
  lNewNode := nil;
  // Find if folder has a Link to exsiting option, otherwise no drop possible
  lAccept := False;
  for i := 0 to THyperlinkTopLevelNode(ANode).AddButtonMenuCaptionsCount - 1 do
    lAccept := lAccept or (not THyperlinkTopLevelNode(ANode).AddMenuIsAdd[i]);

  with iSourceData do begin
    // Find if the folder accepts children of the correct type.
    lAccept := lAccept and
               SameText(THyperlinkTopLevelNode(ANode).ClassTableName, Header.TableName);

    if (Header.ItemCount > 1) and lAccept then
      lAccept := ConfirmYesNo(Format(ResStr_MultipleDrop,
                              [Header.ItemCount, THyperlinkTopLevelNode(ANode).Text])) = mrYes;

    if lAccept then begin
      for i := 0 to Header.ItemCount - 1 do begin
         { KeyField2 is pointer to Source node in Collections Browser, so use
           that to get the caption for the new node. Otherwise, use default 'new node'.}
        if IsInt(Items[i].KeyField2) and
           (TObject(StrToInt(Items[i].KeyField2)) is TBrowserNode) then
          lCaption := TBrowserNode(StrToInt(Items[i].KeyField2)).Caption
        else
          lCaption := ResStr_NewNode;

        if THyperlinkTopLevelNode(ANode).ValidateNewNode(Items[i].KeyField1, lCaption) then
        begin
          { Put in a try/except because the user could be attempting to move
            a Movement node to a Folder that also uses the Movement table, but
            doesn't permit this movement type. An critical EBrowserNodeError would
            be thrown, so catch it and display a friendlier non critical message. }
          try
            lNewNode := THyperlinkTopLevelNode(ANode).LinkNode(Items[i].KeyField1, lCaption);
          except
            on E: EBrowserNodeError do
              raise EBrowserFrameError.CreateNonCritical(ResStr_NodeCannotBeMovedHere);
          end;

          if Assigned(lNewNode) then begin
            try
              // If the node is a Hyperlink Leaf Node, then the join key
              // should be given to the node.
         (*     if lNewNode is THyperlinkLeafNode then
                THyperlinkLeafNode(lNewNode).JoinKey :=
                      TFolderNode(lNewNode.Parent).UpdateNodeRelationship(lNewNode.Key)
              // If not, just run UpdateNodeRelationship anyway.
              else
           *)     THyperlinkTopLevelNode(lNewNode.Parent).UpdateNodeRelationship(lNewNode.Key);
            except
              on EOleException do begin
                if dmGeneral.Connection.Errors.Count > 0 then begin
                  FreeAndNil(lNewNode);
                  raise EBrowserFrameError.CreateNonCritical(ResStr_NodeDragFailedForeignKey);
                end;
                raise; // raise error, assuming it wasn't raised as non critical above
              end;
            end; // try

            { Nodes dragged from the Thesaurus Browser use their KeyField2
              to store the key of the parent (not the pointer to the node,
              as is done in the Collections Browser. Hence, the caption
              has to be retrieved from the database. }
            if not IsInt(Items[i].KeyField2) then
              lNewNode.Caption := VarToStr(dmGeneral.GetStoredProcOutputParam
                                  ('usp_ConceptName_Get', ['@Key', Items[i].KeyField1],
                                   '@Caption'));
          end;
        end;
      end;
      try
        if Assigned(lNewNode) then
          if IsInt(Items[0].KeyField2) then
            tvNav.Selected := lNewNode
          else begin
            tvNav.Selected := lSelectedNode;
            if Assigned(FOnFrameNotification) then
              OnFrameNotification(Self, etRefreshDetailsContainer, nil);
            TFolderNode(lNewNode.Parent).Refresh;
          end;
      except
      end;
      // Refresh the source - doing the first item alone should be sufficient.
      if IsInt(Items[0].KeyField2) then
        UpdateBrowserNodePointer(StrToInt(Items[0].KeyField2));
    end;
  end;  // with
end;  // TfraCBNavigation.DropNodeOnTopLevel 

{-------------------------------------------------------------------------------
}
procedure TfraCBNavigation.eSearchEnter(Sender: TObject);
begin
  inherited;
  dmGeneral.Recorder.RecorderMainForm.StatusText := ResStr_PressReturnToPopulateAll;
end;  // TfraCBNavigation.eSearchEnter

{-------------------------------------------------------------------------------
}
procedure TfraCBNavigation.eSearchExit(Sender: TObject);
begin
  inherited;
  dmGeneral.Recorder.RecorderMainForm.StatusText := '';
end;  // TfraCBNavigation.eSearchExit

{-------------------------------------------------------------------------------
  For F2 keypresses on the search box, instigate a return data link if appropriate.
}
procedure TfraCBNavigation.eSearchKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  lDataType: String;
begin
  inherited;
  if Key = VK_F2 then begin
    with FViewTypeManager.Selected do
      case SearchControlType[SearchDefaultIndex] of
        ctIndividual, ctName: lDataType := 'Name';
        ctLocation: lDataType := 'Location';
      else
        lDataType := '';
      end;
    if lDataType<>'' then
      dmGeneral.Recorder.RequestData(Self as IRequestor, lDataType)
  end;
end;  // TfraCBNavigation.eSearchKeyDown

{-------------------------------------------------------------------------------
  Remove 'beep' from speaker due to Return character. Then run search if character was a
      return.
}
procedure TfraCBNavigation.eSearchKeyPress(Sender: TObject; var Key: Char);
begin
  inherited;
  if Key = #13 then begin
    Key := #0;
    btnGo.Click;
  end;
end;  // TfraCBNavigation.eSearchKeyPress

{-------------------------------------------------------------------------------
  We have to do something unusual for the Field Data Folder. This is the only
  place to do it - we can't put it in the ClassTableName method of
  FieldDataFolder because it is a class function and we need to use Self.
  Occurrence and Taxon_Occurrence nodes can be dragged from the Observations
  module to the Field Data folder node. However, the Folder can only have one
  Class Table Name. So depending on if the top level Specimen is an Earth/Life we
  need to change it.
}
function TfraCBNavigation.FolderAcceptsChildrenOfType(ANode: TFolderNode; ATable: String):
    Boolean;
var
  lClassTableName: String;
  lNode: TBrowserNode;
begin
  if ANode is TFieldDataFolderNode then begin
    lNode := TBrowserNode(ANode).TopLevelNode;
    if lNode is TSpecimenTopLevelNode then begin
      if TSpecimenTopLevelNode(lNode).LifeSciences then
        lClassTableName := TN_TAXON_OCCURRENCE
      else
        lClassTableName := TN_OCCURRENCE;
    end else
      raise EBrowserNodeError.Create(Format(ResStr_TopLevelNodeWrongType, [ResStr_Specimen]));
  end else
    // In all other cases we just use the Class Table Name stored in the folder.
    lClassTableName := TFolderNode(ANode).ChildNodeType[0].ClassTableName;

  // The result is decided.
  Result := SameText(lClassTableName, ATable);
end;  // TfraCBNavigation.FolderAcceptsChildrenOfType

{-------------------------------------------------------------------------------
  Generate the hint for each node on the treeview.  The hint is constructed from the caption,
      the search type and the value that was searched for and matched.
}
procedure TfraCBNavigation.FormShowHint(var HintStr: String; var CanShow: Boolean; var
    HintInfo: THintInfo);
var
  lBrowserNode: TBrowserNode;
  lFlyNode: TFlyNode;
  lPoint: TPoint;
  lHierarchies: TStringList;
  i: Integer;

  //Method to break a string into chunks. Method written due to bug
  //in Delphi's TStringList.DelimitedText property.
  procedure ParseDelimited(
    const list : TStringList;
    const value : string;
    const delimiter : string) ;
  var
    nextDelimiterPosition : integer;
    nextListEntry : string;
    remainingText : string;
    delimeterLength : integer;
  begin
    delimeterLength := Length(delimiter) ;
    remainingText := value + delimiter;
    list.BeginUpdate;
    list.Clear;
    try
      while Length(remainingText) > 0 do
      begin
        nextDelimiterPosition := Pos(delimiter, remainingText) ;
        nextListEntry := Copy(remainingText, 0, nextDelimiterPosition-1) ;
        list.Add(nextListEntry) ;
        remainingText := Copy(
                remainingText,
                nextDelimiterPosition + delimeterLength,
                MaxInt) ;
      end;
    finally
      list.EndUpdate;
    end;
  end;

  function InsertLinebreaks(
    lineLength : integer;
    value: string) : string;
  var
    noOfBreaks, i : integer;
    linebreak : string;
  begin
    linebreak := #13;

    noOfBreaks := Length(value) div lineLength;
    for i := noOfBreaks downto 1 do
    begin
      Insert(linebreak, value, (i * lineLength) - 1);
    end;
    Result := value;
  end;
begin
  //This proc is only for tvNav
  if HintInfo.HintControl <> tvNav then Exit;

  lPoint := HintInfo.CursorPos;

  //Find out which node
  lFlyNode := tvNav.GetNodeAt(lPoint.X, lPoint.Y);
  lBrowserNode := nil;
  if assigned(lFlyNode)then
    if lFlyNode is TBrowserNode then
      lBrowserNode := TBrowserNode(lFlyNode);

  if not assigned(lBrowserNode) then begin
    CanShow := false;
    Exit;
  end else
  if lBrowserNode.Hint = '' then begin
    CanShow := false;
    Exit;
  end;

  //Same node as last hint node?
  if lBrowserNode = FLastHintNode then
    //Yes, display at same position as last time
    HintInfo.HintPos := FLastHintPos
  else begin
    //No, save for later use
    FLastHintPos  := HintInfo.HintPos;
    FLastHintNode := lBrowserNode;
  end;

  HintInfo.HintStr := lBrowserNode.Caption;

  // If the node is a top level node, then include the search type in the hint
  if lBrowserNode is TTopLevelNode then
      HintInfo.HintStr := HintInfo.HintStr + ' - ' + FLastSearchType;
  // Only include match values for searches that have text to match
  if lBrowserNode.Hint <> '' then
  begin
    lHierarchies := TStringList.Create;
    ParseDelimited(lHierarchies, lBrowserNode.Hint, '**');
    HintInfo.HintStr := HintInfo.HintStr + ' - ' +
                        InsertLinebreaks(
                          100,
                          lHierarchies.Strings[0]);
    for i := 1 to lHierarchies.Count - 1 do
      HintInfo.HintStr := HintInfo.HintStr + #13 + lBrowserNode.Caption +
                          ' - ' + InsertLinebreaks(
                                    100,
                                    lHierarchies.Strings[i]);
  end;
  // Ensure hint does not show formatting characters
  HintInfo.HintStr := RemoveBrowserNodeTags(HintInfo.HintStr);
  //Refresh interval
  HintInfo.ReshowTimeout := 200;
end;  // TfraCBNavigation.FormShowHint

{-------------------------------------------------------------------------------
  Fix anchoring problem in drag frame.
}
procedure TfraCBNavigation.FrameResize(Sender: TObject);
begin
  inherited;
  // Ensure anchoring is done properly
  pnlFilter.Width := Width;
  btnSearchType.Left := pnlFilter.Width - btnSearchType.Width - 2;
  btnGo.Left := btnSearchType.Left - btnGo.Width;
  DragFrame.Width := btnGo.Left - DragFrame.Left - 1;
  eSearch.Width := DragFrame.Width-2;
  cmbSearch.Width := eSearch.Width;
  tvNav.Repaint;
end;  // TfraCBNavigation.FrameResize 

{-------------------------------------------------------------------------------
}
function TfraCBNavigation.GetHistoryCaption: String;
begin
  with ViewTypeManager.Selected do begin
    if Trim(SearchText) <> '' then
      Result := SearchCaption[SearchDefaultIndex] + ' - ' + Trim(SearchText)
    else
      Result := Name + ' [' + ResStr_All + ']';
  end
end;  // TfraCBNavigation.GetHistoryCaption 

{-------------------------------------------------------------------------------
}
function TfraCBNavigation.GetSearchText: String;
begin
  if cmbSearch.Visible then
    Result := cmbSearch.Text
  else
    Result := eSearch.Text
end;  // TfraCBNavigation.GetSearchText 

{-------------------------------------------------------------------------------
}
function TfraCBNavigation.GetStoredProcName: String;
begin
  with ViewTypeManager.Selected do begin
    if not SearchTextRequired[SearchDefaultIndex] then
      Result := SearchStoredProcName[SearchDefaultIndex]
    else
    if Length(Trim(SearchText)) > 0 then
      Result := SearchStoredProcName[SearchDefaultIndex]
    else
      Result := PopulateTopLevelStoredProcName;
  end;
end;  // TfraCBNavigation.GetStoredProcName

{-------------------------------------------------------------------------------
}
function TfraCBNavigation.GetStoredProcParams: TVariantArray;
begin
  with ViewTypeManager.Selected do begin
    Result := StoredProcParams;
    if Length(Trim(SearchText)) > 0 then begin
      SetLength(Result, Length(Result) + 2);
      Result[High(Result) -1] := '@SearchText';
      Result[High(Result)]    := TransformSearchText(SearchDefaultIndex, SearchText);
    end;
  end;
end;  // TfraCBNavigation.GetStoredProcParams

{-------------------------------------------------------------------------------
}
function TfraCBNavigation.GetTargetNode(const iIsPasteOperation: Boolean): TFlyNode;
var
  lClickPoint: TPoint;
begin
  if iIsPasteOperation then begin
    Result := tvNav.Selected;
    if not Assigned(Result) then begin
      MessageDlg(ResStr_SelectTargetNode, mtInformation, [mbOK], 0);
      Exit;
    end;
  end else begin
    lClickPoint := tvNav.ScreenToClient(Mouse.CursorPos);
    Result      := tvNav.GetNodeAt(lClickPoint.X, lClickPoint.Y);
  end;
  
  if Result is TLeafNode then
    Result := TLeafNode(Result).Parent;
end;  // TfraCBNavigation.GetTargetNode 

{-------------------------------------------------------------------------------
}
function TfraCBNavigation.Get_KeyList(ANode: TFlyNode): IKeyList;
var
  lKeylist: TKeyList;
  lComKeyList: TComKeyList;
begin
  if Assigned(ANode) then
    lKeyList := TBrowserNode(ANode).KeyList
  else
    // Create an empty key list as nothing selected.
    lKeyList := TEditableKeylist.Create;
  
  lComKeyList := TCOMKeyList.Create(lKeyList);
  Result      := lComKeyList as IKeyList;
end;  // TfraCBNavigation.Get_KeyList

{-------------------------------------------------------------------------------
  Tree population method used by both repopulate tree and AddItemToTree.
}
procedure TfraCBNavigation.InternalPopulateTree(ANodeContext: TNodeContext; const
    AStoredProcName: string = ''; AParams: TVariantArray = nil; const AKey: string = ''; const
    AHistoryCaption: string = ''; AHistoryImageIndex: integer = -1; AAddToHistory: boolean = true);
var
  lRecordset: _Recordset;
  lNewNode: TTopLevelNode;
  lBaseViewType: TBaseViewType;
  lStoredProcName: String;
  lParams, lArray: TVariantArray;
  lBeginUpdate: Boolean;
  lHintExists: Boolean;
begin
  lHintExists := true;

  lBaseViewType := ViewTypeManager.Selected;
  if AStoredProcName = '' then begin
    lStoredProcName := lBaseViewType.PopulateTopLevelStoredProcName ;
    lParams := lBaseViewType.StoredProcParams;
  end else begin
    lStoredProcName := AStoredProcName;
    lParams := AParams;
  end;

  if lParams = nil then SetLength(lParams, 0);

  if AKey <> '' then begin
    SetLength(lParams, Length(lParams) + 2);
    lParams[High(lParams) -1] := '@Key';
    lParams[High(lParams)] := AKey;
  end;

  //Add to history
  if AAddToHistory then
    with TCBNavigationHistoryItem(FHistoryManager.Add(TCBNavigationHistoryItem)) do
    begin
      CBNavigation     := Self;
      Caption          := AHistoryCaption;
      StoredProc       := lStoredProcName;
      StoredProcParams := lParams;
      NodeContext      := ANodeContext;
      Key              := AKey;
      ImageIndex       := AHistoryImageIndex;
      SearchText       := Self.SearchText;
    end; // with

  lRecordset := dmGeneral.GetRecordset(lStoredProcName, lParams);

  if lRecordset.RecordCount > 0 then
  begin
    lRecordSet.MoveFirst;
    while not lRecordset.EOF do
    begin
      lNewNode:= TTopLevelNode(tvNav.Items.AddTypedChild(nil, lBaseViewType.TopLevelNodeClass));
      lNewNode.Initialise(lRecordset); //Sets Key, Caption, ImageIndex...

      if AppSettings.DisplayCommonNames then
        if (lNewNode is TSpecimenTopLevelNode) then
        begin
          if lRecordset.Fields['Life_Sciences'].Value = 0 then
            if lRecordset.Fields['Det_Item_Key'].Value <> Null then
              FCommonNameFetchQueue.Add(lRecordset.Fields['Det_Item_Key'].Value, lNewNode);
        end;

      //If a search has taken place try to assign a hint. Not all search procs
      //use hints, so just try on the first record to stop the stack being thrown
      //multiple times.
      if lBaseViewType.SearchTextRequired[lBaseViewType.SearchDefaultIndex] and lHintExists then
        try
          lNewNode.Hint := VarToStr(lRecordset.Fields['Hint'].Value);
        except on E: exception do
          lHintExists := false;
        end;

      if (lNewNode is TSpecimenTopLevelNode) then
      begin

        if (lRecordset.Fields['NomenclaturalStatus'].Value <> Null)
          and (not (VarToStr(lRecordset.Fields['NomenclaturalStatus'].Value) = '')) then
          begin
            if (RightStr(Trim(lNewNode.Hint), 1) <> '-') then
            begin
              lNewNode.Hint := lNewNode.Hint + ' - '
            end;

            // Hint may have been overridden. But caption should be bold.
            lNewNode.Hint := lNewNode.Hint + 'NS: '
              + lRecordset.Fields['NomenclaturalStatus'].Value;
          end;
      end;

      lRecordset.MoveNext;
    end;
  end;

  //if Inscription/Labels are required for toplevel, then switch viewtype back
  //to Specimens after population
  if ANodeContext = ncInscriptionLabel then
    ViewTypeManager.Selected := ViewTypeManager.ViewTypeByNodeContext(ncSpecimen);

  SetLength(lArray, 0);

  //Ensure that the tree is not in BeginUpdate mode before selecting a node.
  if tvNav.Items.UpdateCount > 0 then begin
    lBeginUpdate := True;
    tvNav.Items.EndUpdate;
  end else
    lBeginUpdate := False;
  if tvNav.Items.Count > 0 then
    tvNav.Selected := tvNav.Items[0]
  else
  if Assigned(FOnFrameNotification) then //Set up the Add button menu anyway.
    OnFrameNotification(self, etInitializeButtons, lArray);

  if lBeginUpdate then
    tvNav.Items.BeginUpdate
  else
  if Assigned(FOnFrameNotification) then //Destroy Details panel if no nodes exist
    OnFrameNotification(self, etDestroyDetailsContainer, lArray);
  FCommonNameFetchQueue.ProcessWhenReady;
end;  // TfraCBNavigation.InternalPopulateTree 

{-------------------------------------------------------------------------------
  3.4.1.62 of the TSD says that store heirarchy leaf nodes should have a Navigate option in
      their pop-up menu. The reason for this is that these nodes are expandable/contractable,
      so double clicking on them will causes this behaviour, rather than navigating as is
      normal for other nodes.
}
procedure TfraCBNavigation.pmNavigateClick(Sender: TObject);
begin
  inherited;
  with TBrowserNode(tvNav.Selected) do begin
    DoNavigation(Key, ncStore, Caption, ImageIndex);
  end;
end;  // TfraCBNavigation.pmNavigateClick 

{-------------------------------------------------------------------------------
}
procedure TfraCBNavigation.pmTreeDuplicateValuationClick(Sender: TObject);
var
  lKeyString: TKeyString;
begin
  inherited;
  lKeyString := dmGeneral.RunInsertStoredProc(TN_VALUATION,
                            'usp_Valuation_Duplicate',
                            ['@Key', TBrowserNode(tvNav.Selected).Key],
                            '@NewKey');
  AddItemToTree(lKeyString);
end;  // TfraCBNavigation.pmTreeDuplicateValuationClick 

{-------------------------------------------------------------------------------
}
procedure TfraCBNavigation.pmTreePopup(Sender: TObject);
begin
  inherited;
  SetTreeMenuEnabled;
end;  // TfraCBNavigation.pmTreePopup 

{-------------------------------------------------------------------------------
}
procedure TfraCBNavigation.RefreshXPMenu;
begin
  if Assigned(FXPMenu) then begin
    FXPMenu.Active := not FXPMenu.Active;
    FXPMenu.Active := not FXPMenu.Active;
  end;
end;  // TfraCBNavigation.RefreshXPMenu

{-------------------------------------------------------------------------------
  Register the tree view for drag and drop operations.
}
procedure TfraCBNavigation.RegisterDragDropComponents;
begin
  RegisterDragComponent(tvNav, DragNode);
  if not (TUserAccessLevel(AppSettings.UserAccessLevel) = ualReadOnly) then
    RegisterDropComponent(tvNav, DropNode, [], [CF_JNCCDATA, CF_HDROP],
                          CheckNodeCanDrop, CheckNodeCanDropFile);
  RegisterDropComponent(eSearch, SearchDrop, [], [CF_JNCCDATA, CF_TEXT], SearchDragOverCheck);
end;  // TfraCBNavigation.RegisterDragDropComponents

{-------------------------------------------------------------------------------
  Populates the top level of the tree.
}
procedure TfraCBNavigation.RepopulateTree(ANodeContext: TNodeContext; const AStoredProcName:
    string = ''; AParams: TVariantArray = nil; const AKey: string = ''; const AHistoryCaption:
    string = ''; AHistoryImageIndex: integer = -1; AAddToHistory: boolean = true);
begin
  tvNav.Items.BeginUpdate;
  try
    ClearTreeView;
    ViewTypeManager.Selected := ViewTypeManager.ViewTypeByNodeContext(ANodeContext);
    InternalPopulateTree(
        ANodeContext,
        AStoredProcName,
        AParams,
        AKey,
        AHistoryCaption,
        AHistoryImageIndex,
        AAddToHistory);
  finally
    tvNav.Items.EndUpdate;
  end;
end;  // TfraCBNavigation.RepopulateTree

{-------------------------------------------------------------------------------
  Implements IRequestor.Update method.  When F2 is used in the search box, this handles the
      returning keylist and converts it to a search string.
}
procedure TfraCBNavigation.ReturnDataToSearchBox(const KeyList: IKeyList);
var
  lKeyList: TEditableKeylist;
begin
  try
    if CheckAcceptSearchKeylist(KeyList) then begin
      lKeylist := TEditableKeylist.Create(KeyList);
      try
        ReturnKeyListToSearchBox(lKeyList);
      finally
        lKeyList.Free;
      end;
    end;
  except
    on E:TExceptionPath do
      // For non-critical errors, display a non-critical box because this
      // method can be called across COM
      if TExceptionPath(E).Critical then
        raise
      else
        ShowInformation(E.Message);
  end; // try
end;  // TfraCBNavigation.ReturnDataToSearchBox 

{-------------------------------------------------------------------------------
  Adds an item in a keylist to the search box text.
}
procedure TfraCBNavigation.ReturnKeyListToSearchBox(AKeylist: TKeyList);
var
  lStoredProc: String;
begin
  with FViewTypeManager.Selected do
    case SearchControlType[SearchDefaultIndex] of
      ctIndividual, ctName:
        lStoredProc := 'usp_Name_Get';
      ctLocation:
        lStoredProc := 'usp_Location_Get';
      ctDetermination:
        if CompareText(AKeyList.Header.TableName, 'Concept') = 0 then
          lStoredProc := 'usp_Concept_Get'
        else
          lStoredProc := 'usp_TaxonListItem_Get'
    else
      lStoredProc := '';
    end; // case
  if (lStoredProc <> '') and (AKeyList.Header.ItemCount > 0) then
    SearchText := dmGeneral.GetStoredProcOutputParam(lStoredProc,
                      ['@Key', AKeyList.Items[0].KeyField1], '@Caption');
end;  // TfraCBNavigation.ReturnKeyListToSearchBox

{-------------------------------------------------------------------------------
  Uses the current ViewType to return a search popup menu containing all items that are
      allowed to be searched on.
}
function TfraCBNavigation.ReturnSearchPopup: TPopupMenu;
var
  i: Integer;
  lMenuItem: TMenuItem;
  lBaseViewType: TBaseViewType;
begin
  Result := pmSearch;
  lBaseViewType := ViewTypeManager.Selected;

  if pmSearch.Items.Count = 0 then
    for i := 0 to lBaseViewType.SearchCount - 1 do begin
      lMenuItem := TMenuItem.Create(pmSearch);
      lMenuItem.Caption := lBaseViewType.SearchCaption[i];
      lMenuItem.Default := i = lBaseViewType.SearchDefaultIndex;
      lMenuItem.OnClick := SearchPopupMenuHandler;
      pmSearch.Items.Add(lMenuItem);
    end;
  RefreshXPMenu;
end;  // TfraCBNavigation.ReturnSearchPopup

{-------------------------------------------------------------------------------
  Test if an item is allowed to be dropped onto the search box.  This is handled dynamically
      according to the selected search type.
}
procedure TfraCBNavigation.SearchDragOverCheck(APoint: TPoint; const ATable, AFieldKey: string;
    var Accept: boolean);
begin
  Accept := CheckAcceptSearchKeyList(nil, ATable);
end;  // TfraCBNavigation.SearchDragOverCheck

{-------------------------------------------------------------------------------
  Handle the dropping of an item on the search box.
}
procedure TfraCBNavigation.SearchDrop(const Sender: TObject; const iFormat : integer; const
    iSourceData: TKeyList; const iTextStrings : TstringList; const iIsPasteOperation: boolean;
    var ioHandled : boolean);
var
  lTableName: String;
begin
  if iFormat = CF_JNCCDATA then
    if Assigned(iSourceData) then begin
      // Find the table name from the keylist
      if (iSourceData.Header.TableName = 'MIXED') and (iSourceData.Header.ItemCount > 0) then
        lTableName := iSourceData.Items[0].KeyField2
      else
        lTableName := iSourceData.Header.TableName;
      if CheckAcceptSearchKeylist(nil, lTableName) then
        ReturnKeyListToSearchBox(iSourceData);
    end;
end;  // TfraCBNavigation.SearchDrop

{-------------------------------------------------------------------------------
  Handles the click event of the search popup menu.
}
procedure TfraCBNavigation.SearchPopupMenuHandler(Sender: TObject);
begin
  if Sender is TMenuItem then
    ViewTypeManager.Selected.SearchDefaultIndex := TMenuItem(Sender).MenuIndex;
end;  // TfraCBNavigation.SearchPopupMenuHandler

{-------------------------------------------------------------------------------
  Changing the search type causes the search box to be configured according to the type.  For
      example, it is replaced with a combo box in some cases.
}
procedure TfraCBNavigation.SearchTypeChange(Sender: TObject);
begin
  with FViewTypeManager.Selected do
    case SearchControlType[SearchDefaultIndex] of
      ctConditionCheckConditionCombo, ctStatusCombo,
      ctSpecimenTypeCombo, ctStoreTypeCombo,
      ctPriorityCombo, ctTaskTypeCombo:
        begin
          cmbSearch.Visible := True;
          eSearch.Visible   := False;
          DragFrame.Visible := False;
          cmbSearch.Clear;
        end;
    else
      begin
        cmbSearch.Visible := False;
        eSearch.Visible   := True;
        DragFrame.Visible := not (SearchControlType[SearchDefaultIndex] in [ctNormal, ctNumber]);
        if DragFrame.Visible then
          DragFrame.Pen.Color := dmGeneral.Recorder.CurrentSettings.DragDestinationColour;
      end;
    end; //case
end;  // TfraCBNavigation.SearchTypeChange 

{-------------------------------------------------------------------------------
}
procedure TfraCBNavigation.SetAllowNavigation(const Value: Boolean);
begin
  FAllowNavigation       := Value;
  HistoryManager.Enabled := FAllowNavigation;
  cmbView.Enabled        := FAllowNavigation;
  eSearch.Enabled        := FAllowNavigation;
  cmbSearch.Enabled      := FAllowNavigation;
  btnSearchType.Enabled  := FAllowNavigation;
  btnGo.Enabled          := FAllowNavigation;
  tvNav.Enabled          := FAllowNavigation;
end;  // TfraCBNavigation.SetAllowNavigation 

{-------------------------------------------------------------------------------
}
procedure TfraCBNavigation.SetOnFrameNotification(Value: TFrameNotificationEvent);
begin
  FOnFrameNotification := Value;
end;  // TfraCBNavigation.SetOnFrameNotification 

{-------------------------------------------------------------------------------
}
procedure TfraCBNavigation.SetSearchText(const Value: String);
begin
  if cmbSearch.Visible then
    cmbSearch.Text := Value
  else
    eSearch.Text := Value;
end;  // TfraCBNavigation.SetSearchText 

{-------------------------------------------------------------------------------
}
procedure TfraCBNavigation.SetTreeMenuEnabled;
var
  lNode: TFlyNode;
begin
  pmTreeRefresh.Enabled   := false;
  pmExpandList.Enabled    := false;
  pmTreeNewWindow.Enabled := false;
  pmTreeDuplicateValuation.Visible := false;
  lNode := tvNav.Selected;
  if Assigned(lNode) then
    if lNode.Selected then begin
      pmTreeRefresh.Enabled := lNode is TBrowserNode;
      if lNode is TTopLevelNode then begin
        pmExpandList.Enabled    := false;
        pmTreeNewWindow.Enabled := true;
      end else
      if lNode is TFolderNode then begin
        pmExpandList.Enabled    := (TContainerNode(lNode).NodeContext <> ncNone) and
                                   lNode.HasChildren;
        pmTreeNewWindow.Enabled := pmExpandList.Enabled;
      end else begin
        pmExpandList.Enabled    := false;
        pmTreeNewWindow.Enabled := lNode is THyperlinkLeafNode;
      end;
      pmTreeDuplicateValuation.Visible := lNode is TValuationTopLevelNode;
      // 'Navigate' pop-up menu option should be available in these cases.
      pmNavigate.Visible := (lNode is TStoreHierarchySubFolderNode) or
                            (lNode is TStoreHierarchyTopLevelNode);
    end;
end;  // TfraCBNavigation.SetTreeMenuEnabled

{-------------------------------------------------------------------------------
  Sets the state of the drop-down search-menu. Also populates the children of TFolderNode to
      allow TfraListViewer to have the correct content.
}
procedure TfraCBNavigation.tvNavChange(Sender: TObject; Node: TFlyNode);
begin
  //Populate the ListViewer if a TFolderNode is selected
  if Node is TFolderNode then
    if not TFolderNode(Node).Populated then
      TFolderNode(Node).Refresh;
  tvNav.PopupMenu := pmTree;
end;  // TfraCBNavigation.tvNavChange 

{-------------------------------------------------------------------------------
}
procedure TfraCBNavigation.tvNavClick(Sender: TObject);
var
  node, nextNode: TFlyNode;
  shiftDown, ctrlDown: Boolean;
begin
  inherited;

  shiftDown := GetKeyState(VK_SHIFT) < 0;
  ctrlDown  := GetKeyState(VK_CONTROL) < 0;

  if not (shiftDown or ctrlDown) then
    FFirstOfmultiSelect := nil;

  node := tvNav.Items.GetFirstSelectedNode;
  if Assigned(node) then begin
    if shiftDown or ctrlDown then begin
      // Proceed with extending the selection
      // while preventing different multi-level/node type selection.
      while Assigned(node) do begin
        nextNode := tvNav.Items.GetNextSelectedNode(node);
        node.Selected := not (node is TFolderNode)
                         and (node.ClassType = FFirstOfMultiSelect.ClassType);
        node := nextNode;
      end;
    end else
    // No SHIFT or CONTROL key down, get a new start node.
    if not (node is TFolderNode) then
      FFirstOfMultiSelect := node;
  end;

  // Always show at least one selected node, if possible.
  if Assigned(FFirstOfMultiSelect) then
    FFirstOfMultiSelect.Selected := True;
end;  // TfraCBNavigation.tvNavClick

{-------------------------------------------------------------------------------
  Navigates THyperlinkLeafNodes to become TTopLevelNodes.
}
procedure TfraCBNavigation.tvNavDblClick(Sender: TObject);
var
  node: TFlyNode;
begin
  // As This event is fired twice. Ignore half of them.
  FTreeExpandToggle := not FTreeExpandToggle;
  if FTreeExpandToggle then Exit;
  
  node := tvNav.Selected;
  if Assigned(node) then
    if ((node is THyperLinkTopLevelNode) and (THyperLinkTopLevelNode(node).DblClickNavigates)) or
       ((node is TStoreHierarchySubFolderNode) and (TStoreHierarchySubFolderNode(node).BottomLevel)) or
       ((node is TStoreHierarchyTopLevelNode) and (TStoreHierarchyTopLevelNode(node).BottomLevel)) or
       (node is THyperLinkLeafNode) then
      PostMessage(Handle, WM_NAVIGATE, Integer(node), 0)
    else
    if node is TContainerNode then
      if not node.Expanded then
        node.Expand(False)
      else
        node.Collapse(False);
end;  // TfraCBNavigation.tvNavDblClick

{-------------------------------------------------------------------------------
  Draw handler to allowing the drawing of Rank dots and formatted terms.
}
procedure TfraCBNavigation.tvNavDrawCell(Sender: TObject; aCanvas: TCanvas; ACol, ARow:
    Integer; Rect: TRect; State: TExGridDrawState);
var
  lRect: TRect;
  lNode: TFlyNode;
begin
  if ACol = 0 then begin
    lNode := tvNav.GetNodeAtRow(ARow);
  
    //Automatically expand TObjectFolderNodes
    if lNode is TObjectFolderNode then
      if not TContainerNode(lNode).Populated then
        TContainerNode(lNode).Expand(False);
  
    lRect := Rect;
    // Move rect over to leave the existing lines and images.
    // 2 Added so that TRapidTree icon/text spacing is the same as that for
    //TListBox and TMenuItems
    lRect.Left := lRect.Left + tvNav.Indent*(lNode.Level + 2) + 2;
    aCanvas.FillRect(lRect); // blank out the area to draw onto
  
    // Draw the rank if there is one
    if Assigned(lNode) then begin
      if lNode is TBaseDeterminationLeafNode then
        dmInterface.DrawRankDot(aCanvas, lRect, TDeterminationLeafNode(lNode).RankColor);
      if lNode is TStoreLeafNode then
        if TStoreLeafNode(lNode).DrawHierarchically then begin
          //Clear the canvas underneath where the icon would normally be drawn
          //This must be done as there is no imageindex, the caption is drawn there by the tree
          lRect.Left := lRect.Left - 20;
          aCanvas.FillRect(lRect);
          lRect.Left := lRect.Left + 20;
          //Indent each node according to its DrawSpacer
          lRect.Left := lRect.Left + TStoreLeafNode(lNode).HierarchyDrawSpacer;
          dmInterface.ilBrowserNodes.DrawOverlay(
              aCanvas, lRect.Left - 20, lRect.Top, 4, 0); //4 is the imageindex for a Store
        end;
    end;
  
    // Vertically centre the text
    lRect.Top := lRect.Top + (tvNav.DefaultRowHeight - aCanvas.TextHeight('A')) div 2;
    // Output the text.  Use the term text output handler for term nodes
    // since it manages italics
    if (lNode is TSpecimenTopLevelNode) or
       (lNode is TBaseDeterminationLeafNode) or
       (lNode is TSpecimenLeafNode) or
       (lNode is TTermLeafNode) then
      dmInterface.DrawTerm(aCanvas, lRect, lNode.Text, gdSelected in State)
    else
      aCanvas.TextOut(lRect.Left, lRect.Top, lNode.Text);
  end;
end;  // TfraCBNavigation.tvNavDrawCell 

{-------------------------------------------------------------------------------
  Forces TContainerNode to populate their children.
}
procedure TfraCBNavigation.tvNavExpanding(Sender: TObject; Node: TFlyNode; var AllowExpansion:
    Boolean);
begin
  if Node is TContainerNode then
    if not TContainerNode(Node).Populated then begin
      // We are assuming that the Container node is empty when we it hasn't
      // been populated. However, it is possible that is contains nodes that
      // have been dropped there. Therefore, it is best that we delete all
      // of these children nodes, or they will appear twice.
      TContainerNode(Node).DeleteChildren;
      TContainerNode(Node).Populate;
    end;
end;  // TfraCBNavigation.tvNavExpanding

{-------------------------------------------------------------------------------
}
procedure TfraCBNavigation.tvNavMouseDown(Sender: TObject; Button: TMouseButton; Shift:
    TShiftState; X, Y: Integer);

type
  TLocalMouseButton = (mbLocalLeft, mbLocalRight, mbLocalMiddle);

var
  lNode: TFlyNode;

begin
  inherited;
  // The following cast avoids a unit refernece problem in mbRight.
  if Button = TMouseButton(mbLocalRight) then begin
    lNode := tvNav.GetNodeAt(X, Y);
    if Assigned(lNode) then begin
      // select new node
      tvNav.Selected := lNode;
      tvNav.PopupMenu := pmTree;
    end
    else if Assigned(tvNav.Selected) then
      // Unlink popup menu if right click off tree
      tvNav.PopupMenu := nil;
  end;
end;  // TfraCBNavigation.tvNavMouseDown 

{-------------------------------------------------------------------------------
  Updates the browser node at a specific memory address, after a drag operation.
}
procedure TfraCBNavigation.UpdateBrowserNodePointer(APointer: integer);
begin
  if TObject(APointer) is TBrowserNode then begin
    if TObject(APointer) is TLeafNode then begin
      if TLeafNode(TObject(APointer)).Parent is TFolderNode then
        TFolderNode(TLeafNode(TObject(APointer)).Parent).Refresh;
    end else
      TBrowserNode(TObject(APointer)).Refresh
  end;
end;  // TfraCBNavigation.UpdateBrowserNodePointer 

{-------------------------------------------------------------------------------
}
function TfraCBNavigation.ViewTypeManager: TViewTypeManager;
begin
  if not Assigned(FViewTypeManager) then begin
    FViewTypeManager := TViewTypeManager.Create;
    FViewTypeManager.OnSearchTypeChange := SearchTypeChange;
  end;
  Result := FViewTypeManager;
end;  // TfraCBNavigation.ViewTypeManager 

{-------------------------------------------------------------------------------
}
function TfraCBNavigation.ViewTypeManagerAllocated: Boolean;
begin
  Result := Assigned(FViewTypeManager);
end;  // TfraCBNavigation.ViewTypeManagerAllocated 

{-------------------------------------------------------------------------------
}
procedure TfraCBNavigation.ViewTypeManagerDiscard;
begin
  FViewTypeManager.Free;
  FViewTypeManager := nil;
end;  // TfraCBNavigation.ViewTypeManagerDiscard 

{-------------------------------------------------------------------------------
}
procedure TfraCBNavigation.WMNavigate(var Message: TMessage);
var
  lBrowserNode: TBrowserNode;
begin
  // The node that will be navigated to is passed as a pointer
  lBrowserNode := TBrowserNode(TObject(Message.wParam));
  if lBrowserNode is THyperLinkLeafNode then
    with THyperLinkLeafNode(lBrowserNode) do
      if NodeContext <> ncRecorder then
        DoNavigation(HyperlinkKey, NodeContext, Caption, ImageIndex)
      else
        DoExternalNavigation(lBrowserNode)
  else
  if (lBrowserNode is THyperLinkTopLevelNode) or (lBrowserNode is TStoreHierarchySubFolderNode) then
    with lBrowserNode do
      if TopNodeContext in [ncStoreHierarchy, ncStore] then
        DoNavigation(Key, ncStore, Caption, ImageIndex);
end;  // TfraCBNavigation.WMNavigate 

{-------------------------------------------------------------------------------
}
procedure TfraCBNavigation.WMRefreshNode(var Message: TMessage);
begin
  // Reference to node to be refreshed is passed in the WParam field.
  if TObject(Message.WParam) is TBrowserNode then
    TBrowserNode(Message.WParam).Refresh;
end;  // TfraCBNavigation.WMRefreshNode

{-------------------------------------------------------------------------------
}
procedure TfraCBNavigation.WMRemoveDeletedNode(var Message: TMessage);
var
  lParentNode: TFlyNode;
  lDeletedLeafNode: TLeafNode;
begin
  // The node that will be navigated to is passed as a pointer
  lDeletedLeafNode := TLeafNode(TObject(Message.wParam));
  
  lParentNode := lDeletedLeafNode.Parent;
  //  tvNav.Items.Delete(lDeletedLeafNode);
  DeleteNode(lDeletedLeafNode);
  
  if Assigned(lParentNode) then
    tvNav.Selected := lParentNode
  else
  if tvNav.Items.GetFirstNode <> nil then
    tvNav.Selected := tvNav.Items.GetFirstNode;
end;  // TfraCBNavigation.WMRemoveDeletedNode 

{-------------------------------------------------------------------------------
}
procedure TfraCBNavigation.WMRemoveDraggedNode(var Message: TMessage);
begin
  if Assigned(DraggedNode) then begin
    if Assigned(FDraggedNode.Parent) then begin
      // Ensure that location hierarchy nodes become navigable if appropriate
      if FDraggedNode.Parent is TStoreHierarchyTopLevelNode then
        TStoreHierarchyTopLevelNode(FDraggedNode.Parent).BottomLevel :=
            TStoreHierarchyTopLevelNode(FDraggedNode.Parent).Count <= 1;
      if FDraggedNode.Parent is TStoreHierarchySubFolderNode then
        TStoreHierarchySubFolderNode(FDraggedNode.Parent).BottomLevel :=
            TStoreHierarchySubFolderNode(FDraggedNode.Parent).Count <= 1;
    end;
    if Assigned(FOnFrameNotification) then
      OnFrameNotification(Self, etClearInterfaceReferences, nil);
    DraggedNode.Free;
    DraggedNode := nil;
  end;
end;  // TfraCBNavigation.WMRemoveDraggedNode

{-------------------------------------------------------------------------------
}
function TfraCBNavigation.ImageMagickInstalled: Boolean;
var
  lImageFileMagick: Variant;
begin
  if not FImageMagickInstallationChecked then try
    FImageMagickInstallationChecked := True;
    lImageFileMagick := CreateOleObject('ImageMagickObject.MagickImage.1');
    FImageMagickInstalled := True;
  except
    on E: EOleSysError do
      // Assume that if instantiation fails, ImageMagick is not installed
      // Could test for specific error message, but would break in other versions of Windows
      FImageMagickInstalled := False;
  end;

  Result := FImageMagickInstalled;
end;

{-------------------------------------------------------------------------------
}
procedure TFraCBNavigation.SelectFolder(ATitle: String; AExpand: Boolean);
var
  child: TFlyNode;
begin
  if tvNav.Selected.HasChildren then
  begin
    tvNav.Selected.Expand(False);
    child := tvNav.Selected.getFirstChild;//.GetNext;//.GetNextChild(tvNav.Selected);
//    MessageDlg(tvNav.Selected.Caption, mtInformation, [mbOK], 0);
    while ((child <> nil) and (child.Caption <> ATitle)) do
    begin
      child := child.GetNextSibling;// tvNav.Selected.GetNextChild(child);
    end;

    if ((child <> nil) and child.HasChildren and AExpand) then
    begin
      child.Expand(False);
      tvNav.Selected := child;
    end;
  end;
end;

{-==============================================================================
    TCBNavigationHistoryItem
===============================================================================}
{-------------------------------------------------------------------------------
}
procedure TCBNavigationHistoryItem.Recall;
begin
  FCBNavigation.SearchText := SearchText;
  FCBNavigation.RepopulateTree(FNodeContext, FStoredProc, FStoredProcParams, FKey, '', -1, false);
end;  // TCBNavigationHistoryItem.Recall

end.

