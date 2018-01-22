{===============================================================================
  Unit:        CBMain

  Defines:     TfrmCBMain
               TAddinMenuItem

  Description: Collections Browser addin main form.

  Model:       CollectionBrowserGeneral.mpb

  Created:

===============================================================================}

unit CBMain;

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ActiveX, AxCtrls, CollectionsBrowser_TLB, StdVcl, StdCtrls, ActnList,
  ImgList, Menus, ToolWin, ImageListButton, ExtCtrls, exgrid, RapTree,
  InterfaceDataModule, Variants, ApplicationSettings, FrameContainerUnit,
  BaseDetailFrameUnit, BrowserNodeFramework, ComCtrls, DataTypes,
  BaseDragFrameUnit, FrameCBNavigation, BrowserViewTypes, DataClasses,
  BrowserNodeMovement, ResourceStrings, BrowserNodeSpecimen, UserMessages,
  BaseTabSheetFrameUnit, Recorder2000_TLB, CollectionsReportsMenu,
  DebuggerTracker, XPMenu, SearchManager, DSSDataTypes, ComUnit, GeneralData;

type
  {-----------------------------------------------------------------------------
    Subclass of TMenuItem used to create Add menu popup items.  They are bound to the
    appropriate option from the class that declared the add menu item by an interface
    pointer and an index.
  }
  TAddMenuItem = class(TMenuItem)
  private
    FAddMenuOptions: IAddMenuOptions;
    FIndex: Integer;
    FTree: TRapidTree;
  public
    procedure Initialise(AAddMenuOptions: IAddMenuOptions; AIndex: integer; ATree: TRapidTree);
    property AddMenuOptions: IAddMenuOptions read FAddMenuOptions;
    property Index: Integer read FIndex;
  end;
  
  {-----------------------------------------------------------------------------
    Main window of the Collections Browser. This screen is accessed from the Recorder Data
    Entry menu and split into browsing controls on the left, and viewing/editing controls
    on the right.  Other forms are embedded onto the screen when required.
  }
  TfrmCBMain = class(TActiveForm, IfrmCBMain, IFormCaption, IMergeMenuManager,
      IDynamicMenuList, IReportItemsProvider, ISortableScreen, IDisplayData, IKeyListSupplier,
      ICopyPasteActions, IMapWindowSelector, IRequestor)
    btnAdd: TImageListButton;
    btnDelete: TImageListButton;
    btnEdit: TImageListButton;
    fraCBNavigation: TfraCBNavigation;
    pmAdd: TPopupMenu;
    pmBack: TPopupMenu;
    pmForward: TPopupMenu;
    pnlButtons: TPanel;
    pnlDetails: TPanel;
    Splitter: TSplitter;
    procedure IRequestor.Update = RequestorUpdate;
    procedure ActiveFormCreate(Sender: TObject);
    procedure btnAddClick(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
    procedure btnEditClick(Sender: TObject);
    procedure fraCBNavigationcmbViewChange(Sender: TObject);
    procedure fraCBNavigationpmExpandListClick(Sender: TObject);
    procedure fraCBNavigationpmTreeNewWindowClick(Sender: TObject);
    procedure fraCBNavigationpmTreeRefreshClick(Sender: TObject);
    procedure fraCBNavigationtvNavChange(Sender: TObject; Node: TFlyNode);
    procedure fraCBNavigationtvNavChanging(Sender: TObject; Node: TFlyNode; var AllowChange:
        Boolean);
    procedure SplitterCanResize(Sender: TObject; var NewSize: Integer; var Accept: Boolean);
  private
    FContainer: TfraContainer;
    FCopyMenuItem: IDynamicMenu;
    FEvents: IfrmCBMainEvents;
    FLockedNode: TBrowserNode;
    FLockedPreviousSelectedNode: TBrowserNode;
    FNodeAdded: Boolean;
    FPasteMenuItem: IDynamicMenu;
    FXPMenu: TXPMenu;
    FComAddins: TComAddins;
    procedure ActivateEvent(Sender: TObject);
    procedure AddDeterminationNode(AIsLifeSciences: boolean; const AKey: TKeyString);
    procedure AddMenuAddItems(AIntf: IAddMenuOptions);
    procedure AddMenuOptionHandler(Sender: TObject);
    procedure ClearInterfaceReferences;
    procedure ClickEvent(Sender: TObject);
    procedure Copy; safecall;
    procedure CreateEvent(Sender: TObject);
    procedure DblClickEvent(Sender: TObject);
    procedure DeactivateEvent(Sender: TObject);
    procedure DestroyContainer;
    procedure DestroyEvent(Sender: TObject);
    procedure DisplayData(const AKeyList: IKeyList); safecall;
    procedure FocusFirstControl(AControl: TWinControl);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FrameContainerEditModeChange(Sender: TObject);
    procedure FrameNotification(Sender: TObject; AType: TEventType; const AParams:
        TVariantArray);
    function GetCollectionsReportsMenu: IDynamicMenu;
    function GetEditMenu(AType: TMenuType): IDynamicMenu;
    function GetSortByMenu: IDynamicMenu;
    function Get_Count: Integer; safecall;
    function Get_DynamicMenuList: IDynamicMenuList; safecall;
    function Get_Exportable: WordBool; safecall;
    function Get_FolderListCount: Integer; safecall;
    function Get_FolderListKey(AIndex: Integer): WideString; safecall;
    function Get_FolderListTable: WideString; safecall;
    function Get_FormCaption: widestring; safecall;
    function Get_ImageListHandle: Integer; safecall;
    function Get_InsertAfterMenu(AIndex: Integer): WideString; safecall;
    function Get_InsertBeforeMenu(AIndex: Integer): WideString; safecall;
    function Get_KeyList: IKeyList; safecall;
    function Get_MenuPath(AIndex: Integer): WideString; safecall;
    function Get_SelectedItemKey: WideString; safecall;
    function Get_SelectedItemTable: WideString; safecall;
    function Get_SortProvider: IDispatch; safecall;
    function Get_TopLevelListCount: Integer; safecall;
    function Get_TopLevelListKey(AIndex: Integer): WideString; safecall;
    function Get_TopLevelListTable: WideString; safecall;
    procedure InitAddButton(ANode: TBrowserNode);
    function Items(AIndex: Integer): IDynamicMenu; safecall;
    procedure KeyPressEvent(Sender: TObject; var Key: Char);
    procedure LinkNode(AParentFolderNode: TFolderNode; const AKey, ACaption: string); overload;
    procedure LinkNode(AParentTopLevelNode: THyperlinkTopLevelNode; const AKey, ACaption:
        string); overload;
    procedure LockDetailFrame(ALockedNode: TBrowserNode);
    procedure PaintEvent(Sender: TObject);
    procedure Paste; safecall;
    procedure RefreshNodeCaption(Sender: TObject; const ACaption: String);
    procedure RefreshNodeInfo(Sender: TObject; const AKey, ACaption: string);
    procedure RefreshXPMenu;
    procedure RequestorUpdate(const KeyList: IKeyList); safecall;
    function ReturnActiveBaseDragFrame(AControl: TWinControl): TBaseDragFrame;
    function ReturnParentContainerNode(ANode: TFlyNode): TContainerNode;
    procedure SetButtonStates(ANode: TBrowserNode);
    procedure SetDetailForm(ANode: TFlyNode; ReloadControls: Boolean = False);
    procedure ShowFindDialog(ASearchType: SearchManager.TSearchType; var oSearchResultKey:
        string; var oSearchResultText: string);
    function SupportsTable(const ATableName: WideString): WordBool; safecall;
    procedure UpdateMapWindowSelector; safecall;
    procedure WMAddDeterminationEarthScience(var Message: TMessage); message
        WM_ADD_DETERMINATION_EARTH_SCIENCE;
    procedure WMAddDeterminationLifeScience(var Message: TMessage); message
        WM_ADD_DETERMINATION_LIFE_SCIENCE;
    procedure WMAddSpecimen(var Message: TMessage); message WM_ADD_SPECIMEN;
    procedure WMNavigateSpecimen(var Message: TMessage); message WM_NAVIGATE_SPECIMEN;
    procedure WMNavigateStore(var Message: TMessage); message WM_NAVIGATE_STORE;
    procedure WMNavigateMovement(var Message: TMessage); message WM_NAVIGATE_MOVEMENT;
    procedure WMRefreshSpecimenCaption(var Message: TMessage); message
        WM_REFRESH_SPECIMEN_CAPTION;
    procedure WMNavigateConditionCheck(var Message: TMessage); message
        WM_NAVIGATE_CONDITION_CHECK;
    procedure WMNavigateJob(var Message: TMessage); message
        WM_NAVIGATE_JOB;
    procedure WMNavigateAccession(var Message: TMessage); message
        WM_NAVIGATE_ACCESSION;
    procedure WMNavigateLoan(var Message: TMessage); message
        WM_NAVIGATE_LOAN;
    procedure WMNavigateFolder(var Message: TMessage);
        message WM_NAVIGATE_FOLDER;
  protected
    procedure AboutBox; safecall;
    procedure DefinePropertyPages(DefinePropertyPage: TDefinePropertyPage); override;
    procedure EventSinkChanged(const EventSink: IUnknown); override;
    function Get_Active: WordBool; safecall;
    function Get_AlignDisabled: WordBool; safecall;
    function Get_AutoScroll: WordBool; safecall;
    function Get_AutoSize: WordBool; safecall;
    function Get_AxBorderStyle: TxActiveFormBorderStyle; safecall;
    function Get_Caption: WideString; safecall;
    function Get_Color: OLE_COLOR; safecall;
    function Get_DoubleBuffered: WordBool; safecall;
    function Get_DropTarget: WordBool; safecall;
    function Get_Enabled: WordBool; safecall;
    function Get_Font: IFontDisp; safecall;
    function Get_HelpFile: WideString; safecall;
    function Get_KeyPreview: WordBool; safecall;
    function Get_PixelsPerInch: Integer; safecall;
    function Get_PrintScale: TxPrintScale; safecall;
    function Get_Scaled: WordBool; safecall;
    function Get_ScreenSnap: WordBool; safecall;
    function Get_SnapBuffer: Integer; safecall;
    function Get_Visible: WordBool; safecall;
    function Get_VisibleDockClientCount: Integer; safecall;
    procedure Set_AutoScroll(Value: WordBool); safecall;
    procedure Set_AutoSize(Value: WordBool); safecall;
    procedure Set_AxBorderStyle(Value: TxActiveFormBorderStyle); safecall;
    procedure Set_Caption(const Value: WideString); safecall;
    procedure Set_Color(Value: OLE_COLOR); safecall;
    procedure Set_DoubleBuffered(Value: WordBool); safecall;
    procedure Set_DropTarget(Value: WordBool); safecall;
    procedure Set_Enabled(Value: WordBool); safecall;
    procedure Set_Font(const Value: IFontDisp); safecall;
    procedure Set_HelpFile(const Value: WideString); safecall;
    procedure Set_KeyPreview(Value: WordBool); safecall;
    procedure Set_PixelsPerInch(Value: Integer); safecall;
    procedure Set_PrintScale(Value: TxPrintScale); safecall;
    procedure Set_Scaled(Value: WordBool); safecall;
    procedure Set_ScreenSnap(Value: WordBool); safecall;
    procedure Set_SnapBuffer(Value: Integer); safecall;
    procedure Set_Visible(Value: WordBool); safecall;
    procedure WndProc(var Message: TMessage); override;
    procedure _Set_Font(var Value: IFontDisp); safecall;
  public
    destructor Destroy; override;
    procedure Initialize; override;
  end;
  
//==============================================================================
implementation

{$R *.DFM}

uses
  ComObj, ComServ, About, ADODB, FrameCollection, FrameListViewer,
  LuxembourgConstants, BrowserNodeCollectionUnits, BrowserNodeConditionCheck,
  BrowserNodeCommon, GeneralFunctions, Treecoll, COMClasses, BasePageControlFrameUnit,
  ThesaurusBrowser_TLB, ExceptionForm, FrameStorage;

{-==============================================================================
    TAddMenuItem
===============================================================================}
{-------------------------------------------------------------------------------
  Initialises the menu option to point to the add menu item stored in the interface at the
      supplied index.
}
procedure TAddMenuItem.Initialise(AAddMenuOptions: IAddMenuOptions; AIndex: integer; ATree:
    TRapidTree);
begin
  FAddMenuOptions := AAddMenuOptions;
  FIndex := AIndex;
  // Store the tree so items can add nodes when clicked
  FTree := ATree;
  Caption := FAddMenuOptions.AddButtonMenuCaption[AIndex];
end;  // TAddMenuItem.Initialise 

{-==============================================================================
    TfrmCBMain
===============================================================================}
{-------------------------------------------------------------------------------
}
destructor TfrmCBMain.Destroy;
var
  i: Integer;
begin
  // Destroy the add menu first as menu items have interface pointers to the
  // View types.
  FreeAndNil(FComAddins);
  FreeAndNil(pmAdd);
  with fraCBNavigation do
    if ViewTypeManagerAllocated then ViewTypeManagerDiscard;

  if Assigned(FContainer) then begin
    FContainer.UnloadFrames;
    FContainer.Free;
  end else
    for i := ComponentCount - 1 downto 0 do
      if Components[i] is TFrame then Components[i].Free;
  FContainer := nil;
  // Destroy things first to avoid possible handle problems later
  FreeAndNil(fraCBNavigation);

  DiscardAppSettings;
  if TdmGeneral.Allocated then TdmGeneral.Discard;
  inherited;
end;  // TfrmCBMain.Destroy 

{-------------------------------------------------------------------------------
}
procedure TfrmCBMain.AboutBox;
begin
end;  // TfrmCBMain.AboutBox 

{-------------------------------------------------------------------------------
}
procedure TfrmCBMain.ActivateEvent(Sender: TObject);
begin
  if FEvents <> nil then FEvents.OnActivate;
end;  // TfrmCBMain.ActivateEvent 

{-------------------------------------------------------------------------------
}
procedure TfrmCBMain.ActiveFormCreate(Sender: TObject);
var
  liCounter: Integer;
begin
  fraCBNavigation.OnFrameNotification := FrameNotification;
  
  
  fraCBNavigation.ViewTypeManager.ComboBox := fraCBNavigation.cmbView;
  for liCounter := 0 to fraCBNavigation.ViewTypeManager.ViewTypeCount -1 do
    fraCBNavigation.ViewTypeManager.ViewType[liCounter].OnFrameNotification :=
        FrameNotification;
  if fraCBNavigation.cmbView.ItemIndex=-1 then
    fraCBNavigation.cmbView.ItemIndex :=
        fraCBNavigation.cmbView.Items.IndexOf(ResStr_Collections);
  dmInterface.ilBrowserNodes.Overlay(0, 0);
  fraCBNavigationcmbViewChange(nil);
end;  // TfrmCBMain.ActiveFormCreate 

{-------------------------------------------------------------------------------
}
procedure TfrmCBMain.AddDeterminationNode(AIsLifeSciences: boolean; const AKey: TKeyString);
var
  lDeterminationLeafNode: TBaseDeterminationLeafNode;
  lNode: TFlyNode;
begin
  ShowInformation(ResStr_AskForDeterminationDetails);
  
  with fraCBNavigation.tvNav do begin
    if Selected is TSpecimenTopLevelNode then
      lNode := Selected
    else begin //Must be a TLeafNode
      fraCBNavigation.RepopulateTree(ncSpecimen, '', nil, Akey, '', -1, False);
      lNode := Items.GetFirstNode;
    end;
  
    if lNode is TSpecimenTopLevelNode then begin
      TTopLevelNode(lNode).Populate;
      lNode := lNode.GetFirstChild;
    end;
  
    // Loop through the folder nodes until we get to the TDeterminationFolderNode
    while Assigned(lNode) and not (lNode is TDeterminationFolderNode) do
      lNode := lNode.GetNextSibling;
  
    if not Assigned(lNode) then
      raise EBrowserFrameError.Create(Format(ResStr_FolderNodeNotFound,
                                             ['TDeterminationFolderNode']));
    // Need to expand the folder node before the new node is added, because
    // expanding folders causes the children to be deleted prior to repopulation.
    // This would delete the new node and cause a crash.
    lNode.Expanded := True;
  
    if AIsLifeSciences then
      lDeterminationLeafNode := TDeterminationRecorderLeafNode(Items.AddTypedChild(lNode,
                                                               TDeterminationRecorderLeafNode))
    else
      lDeterminationLeafNode := TDeterminationLeafNode(Items.AddTypedChild(lNode,
                                                       TDeterminationLeafNode));
  
    lDeterminationLeafNode.Key := AKey;
    lDeterminationLeafNode.InitialiseNewNode;

    FLockedPreviousSelectedNode := TBrowserNode(lNode);
    Selected := lDeterminationLeafNode;

    // Ensure the Determinations frame is in edit mode.
    FContainer.EditMode := emEdit;
  end;// with
  LockDetailFrame(lDeterminationLeafNode);
end;  // TfrmCBMain.AddDeterminationNode 

{-------------------------------------------------------------------------------
  Adds items listed by the interface to the Add popup menu.
}
procedure TfrmCBMain.AddMenuAddItems(AIntf: IAddMenuOptions);
var
  lIdx: Integer;
  lAddMenuItem: TAddMenuItem;
  lMenuItem: TMenuItem;
begin
  // If adding a different interface list to an existing add menu, show a eeparator
  if (AIntf.AddButtonMenuCaptionsCount>0) and (pmAdd.Items.Count>0) then begin
    lMenuItem := TMenuItem.Create(pmAdd);
    lMenuItem.Caption := '-'; // separator
    pmAdd.Items.Add(lMenuItem);
  end;
  for lIdx := 0 to AIntf.AddButtonMenuCaptionsCount-1 do begin
    lAddMenuItem := TAddMenuItem.Create(pmAdd);
    lAddMenuItem.Initialise(AIntf, lIdx, fraCBNavigation.tvNav);
    lAddMenuItem.OnClick := AddMenuOptionHandler;
    pmAdd.Items.Add(lAddMenuItem);
  end;
  RefreshXPMenu;
end;  // TfrmCBMain.AddMenuAddItems 

{-------------------------------------------------------------------------------
}
procedure TfrmCBMain.AddMenuOptionHandler(Sender: TObject);
var
  lNewNode: TBrowserNode;
  lSearchResultKey, lSearchResultText: String;
  lParentContainerNode: TContainerNode;
  lMenu: TAddMenuItem;
begin
  if not (Sender is TAddMenuItem) then Exit;
  
  lMenu := TAddMenuItem(Sender);
  if Assigned(fraCBNavigation.tvNav.Selected) then
    FLockedPreviousSelectedNode := TBrowserNode(fraCBNavigation.tvNav.Selected)
  else
    FLockedPreviousSelectedNode := nil;
  
  if lMenu.AddMenuOptions.AddMenuIsAdd[lMenu.Index] then
  begin
    // Adding a new item
    lNewNode := TBrowserNode(lMenu.AddMenuOptions.AddNode(fraCBNavigation.tvNav, lMenu.Index));
    //Force a new Frame Container to be created. This will cause all previously
    //initialised variables to be reset
    DestroyContainer;
    fraCBNavigation.tvNav.Selected := lNewNode; //Displays Associated BaseDetailFrame
    LockDetailFrame(lNewNode);
  end
  else begin
    lParentContainerNode := ReturnParentContainerNode(fraCBNavigation.tvNav.Selected);
    if Assigned(lParentContainerNode) then begin
      if (lParentContainerNode is TFolderNode) then begin
        // Linking an existing node
        if TFolderNode(lParentContainerNode).SearchType = SearchManager.stConcept then begin
          fraCBNavigation.AllowNavigation := False;
          dmGeneral.Recorder.RequestCOMData(Self as IRequestor, CLASS_frmThesaurusBrowser);
        end
        else begin
          ShowFindDialog(TFolderNode(lParentContainerNode).SearchType,
                         lSearchResultKey, lSearchResultText);
          if (lSearchResultKey <> EmptyStr) then
            LinkNode(TFolderNode(lParentContainerNode), lSearchResultKey, lSearchResultText);
        end;
      end else if (lParentContainerNode is THyperlinkTopLevelNode) then begin
        ShowFindDialog(THyperlinkTopLevelNode(lParentContainerNode).SearchType,
                       lSearchResultKey, lSearchResultText);
        if (lSearchResultKey <> EmptyStr) then
          LinkNode(THyperlinkTopLevelNode(lParentContainerNode), lSearchResultKey,
              lSearchResultText);
      end;
    end;
  end;
end;  // TfrmCBMain.AddMenuOptionHandler 

{-------------------------------------------------------------------------------
}
procedure TfrmCBMain.btnAddClick(Sender: TObject);
var
  lPosPopup: TPoint;
begin
  
  FNodeAdded := true;
  //Display Add popup if more than 1 menu item has been created
  if pmAdd.Items.Count > 1 then begin
    // Work out where to show the popupmenu so that it appears just under the button
    lPosPopup := btnAdd.ClientToScreen(Point(0, btnAdd.Height));
    pmAdd.Popup(lPosPopup.X, lPosPopup.Y);
  end
  else if pmAdd.Items.Count = 1 then
    // if only one item on the popup menu, don't show it, just click it
    pmAdd.Items[0].Click;
end;  // TfrmCBMain.btnAddClick 

{-------------------------------------------------------------------------------
}
procedure TfrmCBMain.btnDeleteClick(Sender: TObject);
var
  lParentNode: TFlyNode;
  lDeleteConfirmed: Boolean;
  lCaptionNoTags: String;
begin
  with fraCBNavigation.tvNav do
    if Assigned(Selected) then begin
      lParentNode := Selected.Parent;
      lDeleteConfirmed := False;
      // Remove the italic tags for the messages.
      lCaptionNoTags := RemoveBrowserNodeTags(Selected.Text);
      { If link node, delete only the link, not the whole data displayed.
        Special cases of leaf nodes that should be really treated as non-link:
        TFieldDataLeafNode:
            The Specimen_Field_Data is join table where data is kept.
        TInscriptionLabelLeafNode:
            Hyperlink node to get to associated multimedia.
        TTaskIdentifiedLeafNode:
            The Conservation_Task table is the join table, and so the link with condition
            check cannot be removed without actually removing the whole task record.
        TRelatedCollectionUnitLeafNode:
            The Collection_Unit_Relation table is join table where data is kept.
        TRelatedNameLeafNode:
            The Collection_Unit_Name table is join table where data is kept.
      }
      if (Selected is THyperlinkLeafNode) and not
         ((Selected is TFieldDataLeafNode) or (Selected is TInscriptionLabelLeafNode) or
          (Selected is TTaskIdentifiedLeafNode) or (Selected is TRelatedNameLeafNode) or
          (Selected is TRelatedCollectionUnitLeafNode)) then
      begin
        if MessageDlg(Format(ResStr_ConfirmLinkDelete, [lCaptionNoTags]),
                      mtWarning, [mbYes, mbNo], 0) = mrYes then
        begin
          lDeleteConfirmed := True;
          // Use the JoinKey of node, to get the right record.
          TFolderNode(Selected.Parent).DeleteLink(THyperlinkLeafNode(Selected).JoinKey);
        end;
      end else
      // TFieldData is a link but only one frame, and the Specimen_Field_Data table is the
      // link table too. So confirm link and delete content.
      if Selected is TFieldDataLeafNode then
      begin
        if MessageDlg(Format(ResStr_ConfirmLinkDelete, [lCaptionNoTags]),
                      mtWarning, [mbYes, mbNo], 0) = mrYes then
        begin
          lDeleteConfirmed := True;
          FContainer.ContainedFrame.DeleteContent;
        end;
      end else
      // Not a link node, or a special case one, go ahead and get rid of everything.
      if ConfirmDeletionYesNo(lCaptionNoTags) = mrYes then begin
        lDeleteConfirmed := True;
        FContainer.ContainedFrame.DeleteContent;
      end;
  
      // If data deleted, cleanup the details and the tree.
      if lDeleteConfirmed then begin
        FContainer.ContainedFrame.DropInterfaceReferences;
        // Actually delete the node itself.
        pmAdd.Items.Clear;
        fraCBNavigation.DeleteNode(Selected);
        Selected := lParentNode;
        // If nothing selected, default to very first node, if any.
        if Selected = nil then Selected := Items.GetFirstNode;
        // Make sure it shows as selected too.
        if Selected <> nil then Selected.Selected := True;
        // Now make sure the detail side is in sync.
        SetDetailForm(Selected);
      end;
    end;
end;  // TfrmCBMain.btnDeleteClick 

{-------------------------------------------------------------------------------
}
procedure TfrmCBMain.btnEditClick(Sender: TObject);
begin
  FNodeAdded := false;
  if fraCBNavigation.tvNav.Selected is TBrowserNode then
    LockDetailFrame(TBrowserNode(fraCBNavigation.tvNav.Selected));
end;  // TfrmCBMain.btnEditClick 

{-------------------------------------------------------------------------------
}
procedure TfrmCBMain.ClearInterfaceReferences;
begin
  pmAdd.Items.Clear;
  FContainer.ContainedFrame.DropInterfaceReferences;
end;  // TfrmCBMain.ClearInterfaceReferences 

{-------------------------------------------------------------------------------
}
procedure TfrmCBMain.ClickEvent(Sender: TObject);
begin
  if FEvents <> nil then FEvents.OnClick;
end;  // TfrmCBMain.ClickEvent 

{-------------------------------------------------------------------------------
}
procedure TfrmCBMain.Copy;
var
  lBaseDragFrame: TBaseDragFrame;
begin
  if Assigned(ActiveControl) then begin
    lBaseDragFrame := ReturnActiveBaseDragFrame(ActiveControl);
    if Assigned(lBaseDragFrame) then
      lBaseDragFrame.ExecuteCopy(ActiveControl);
  end;
end;  // TfrmCBMain.Copy 

{-------------------------------------------------------------------------------
}
procedure TfrmCBMain.CreateEvent(Sender: TObject);
begin
  if FEvents <> nil then FEvents.OnCreate;
end;  // TfrmCBMain.CreateEvent 

{-------------------------------------------------------------------------------
}
procedure TfrmCBMain.DblClickEvent(Sender: TObject);
begin
  if FEvents <> nil then FEvents.OnDblClick;
end;  // TfrmCBMain.DblClickEvent 

{-------------------------------------------------------------------------------
}
procedure TfrmCBMain.DeactivateEvent(Sender: TObject);
begin
  if FEvents <> nil then FEvents.OnDeactivate;
end;  // TfrmCBMain.DeactivateEvent 

{-------------------------------------------------------------------------------
}
procedure TfrmCBMain.DefinePropertyPages(DefinePropertyPage: TDefinePropertyPage);
begin
  { Define property pages here.  Property pages are defined by calling
    DefinePropertyPage with the class id of the page.  For example,
      DefinePropertyPage(Class_frmCBMainPage); }
end;  // TfrmCBMain.DefinePropertyPages 

{-------------------------------------------------------------------------------
  Unloads the container frames and frees it.
}
procedure TfrmCBMain.DestroyContainer;
begin
  if Assigned(FContainer) then begin
    FContainer.UnloadFrames;
    FContainer.Free;
    FContainer := nil;
  end;
end;  // TfrmCBMain.DestroyContainer 

{-------------------------------------------------------------------------------
}
procedure TfrmCBMain.DestroyEvent(Sender: TObject);
begin
  if FEvents <> nil then FEvents.OnDestroy;
end;  // TfrmCBMain.DestroyEvent 

{-------------------------------------------------------------------------------
  Display the nodes for a supplied keylist.  Implements IDisplayData.DisplayData allowing
      Recorder to display an item.
}
procedure TfrmCBMain.DisplayData(const AKeyList: IKeyList);
var
  lKeyList: TEditableKeyList;
begin
  lKeyList := TEditableKeyList.Create(AKeyList);
  try
    with fraCBNavigation do begin
      try
        tvNav.Items.BeginUpdate;
        ClearTreeView;
        AddItemsToTree(lKeyList);
      finally
        tvNav.Items.EndUpdate;
      end;
      // Ensure that the top item gets selected so that the frame on the right
      // gets loaded with data.
      if tvNav.Selected <> tvNav.TopItem then
        if Assigned(tvNav.TopItem) then
          tvNav.Selected := tvNav.TopItem;
    end;
  finally
    lKeyList.Free;
  end;
end;  // TfrmCBMain.DisplayData 

{-------------------------------------------------------------------------------
}
procedure TfrmCBMain.EventSinkChanged(const EventSink: IUnknown);
begin
  FEvents := EventSink as IfrmCBMainEvents;
  inherited EventSinkChanged(EventSink);
end;  // TfrmCBMain.EventSinkChanged 

{-------------------------------------------------------------------------------
  Focus the first control in the current container frame that receives input focus.
}
procedure TfrmCBMain.FocusFirstControl(AControl: TWinControl);
begin
  if AControl is TBasePageControlFrame then
    ActiveControl := TBasePageControlFrame(AControl).pcDetails.ActivePage
  else
    ActiveControl := AControl;
  Perform(WM_NEXTDLGCTL, 0, 0);
end;  // TfrmCBMain.FocusFirstControl 

{-------------------------------------------------------------------------------
}
procedure TfrmCBMain.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := True;
  if Assigned(FContainer) then
    if FContainer.EditMode <> emBrowse then
      case ConfirmYesNoCancel(ResStr_FrameToCloseInEditMode) of
        mrYes: FContainer.SaveChanges;
        mrNo : FContainer.CancelChanges;
      else
        CanClose := False;
      end;
end;  // TfrmCBMain.FormCloseQuery 

{-------------------------------------------------------------------------------
  Populate add menu for current view type.
}
procedure TfrmCBMain.fraCBNavigationcmbViewChange(Sender: TObject);
begin
  with fraCBNavigation do begin
    ViewTypeManager.Selected := ViewTypeManager.ViewType[cmbView.ItemIndex];
    pmAdd.Items.Clear;
    if Assigned(FContainer) then
      FContainer.ContainedFrame.DropInterfaceReferences;
    cmbViewChange(Sender);
    SetButtonStates(nil);
    InitAddButton(nil);
    AppSettings.DefaultViewType := ViewTypeManager.Selected.NodeContextText;
  end;
  RefreshXPMenu;
end;  // TfrmCBMain.fraCBNavigationcmbViewChange 

{-------------------------------------------------------------------------------
}
procedure TfrmCBMain.fraCBNavigationpmExpandListClick(Sender: TObject);
var
  lKeyList: IKeyList;
  lCursor: TCursor;
begin
  lCursor := HourglassCursor;
  try
    lKeyList := Get_KeyList;
    with fraCBNavigation do begin
      try
        tvNav.Items.BeginUpdate;
        ClearTreeView;
        DisplayData(lKeyList);
      finally
        tvNav.Items.EndUpdate;
      end;
    end;
  finally
    DefaultCursor(lCursor);
  end;
end;  // TfrmCBMain.fraCBNavigationpmExpandListClick 

{-------------------------------------------------------------------------------
  Display a new copy of the Collections Browser.
}
procedure TfrmCBMain.fraCBNavigationpmTreeNewWindowClick(Sender: TObject);
var
  lKeyList: IKeyList;
  lCursor: TCursor;
begin
  lCursor := HourglassCursor;
  try
    dmGeneral.Recorder.MenuOptionClick(ResStr_MnuDataEntry + ';' +
        ResStr_MnuCollectionsBrowser);
    lKeyList := Get_KeyList;
    dmGeneral.Recorder.DisplayData(lKeyList.TableName, lKeyList);
  finally
    DefaultCursor(lCursor);
  end;
end;  // TfrmCBMain.fraCBNavigationpmTreeNewWindowClick 

{-------------------------------------------------------------------------------
}
procedure TfrmCBMain.fraCBNavigationpmTreeRefreshClick(Sender: TObject);
var
  lCursor: TCursor;
begin
  lCursor := HourglassCursor;
  try
    if assigned(fraCBNavigation.tvNav.Selected) then begin
      if fraCBNavigation.tvNav.Selected is TBrowserNode then begin
        TBrowserNode(fraCBNavigation.tvNav.Selected).Refresh;
        SetDetailForm(fraCBNavigation.tvNav.Selected, False);
      end;
    end;
  finally
    DefaultCursor(lCursor);
  end;
end;  // TfrmCBMain.fraCBNavigationpmTreeRefreshClick 

{-------------------------------------------------------------------------------
}
procedure TfrmCBMain.fraCBNavigationtvNavChange(Sender: TObject; Node: TFlyNode);
  
  procedure SetDetails;
  begin
    if Assigned(fraCBNavigation.tvNav.Selected) then
      SetDetailForm(Node, not (TBrowserNode(Node).AssociatedFrame =
                    TBrowserNode(fraCBNavigation.tvNav.Selected).AssociatedFrame))
    else
      SetDetailForm(Node, True);
  end;
  
begin
  fraCBNavigation.tvNavChange(Sender, Node);
  //If the same node has been click on do not reload data.
  //Except for first node to allow fix for suspected bug in RapidTree
  with fraCBNavigation.tvNav do
    if Assigned(TRapidTree(Sender).Selected) and Assigned(Node) then
      if (Selected = Node) and (Selected.Index > 0) then Exit;
  
  if Assigned(Node) then begin
    if Node is TBrowserNode then
      if Assigned(FContainer) then begin
        if not (FContainer.EditMode in [emEdit]) then
          SetDetails;
      end else
        SetDetails;
  end else
    // clear the details as no node selected
    FreeAndNil(FContainer);
  
  InitAddButton(TBrowserNode(Node));
  SetButtonStates(TBrowserNode(Node));
  
  //Bomb-proofing to keep reference if TfraContainer has not been destroyed
  if pnlDetails.ControlCount = 1 then
    if pnlDetails.Controls[0] is TfraContainer then
      FContainer := TfraContainer(pnlDetails.Controls[0]);
end;  // TfrmCBMain.fraCBNavigationtvNavChange 

{-------------------------------------------------------------------------------
}
procedure TfrmCBMain.fraCBNavigationtvNavChanging(Sender: TObject; Node: TFlyNode; var
    AllowChange: Boolean);
begin
  if Assigned(FContainer) then
    if Assigned(FContainer.ContainedFrame) then
      // The following line used to check that Node was not equal to the selected
      // node in the treeview. However, this wasn't suitable because the rapidtree
      // code will often change the selected node to one for its own purposes, then
      // change it back so the user doesn't see the change. However, this change
      // will fire the OnChanging event, meaning the interface references get
      // dropped incorrectly. Hence, it is better for Node to be compared with
      // the node that last had SetAdditionalProperties called against it.
      // I understand that FContainer.ContainedFrame.AssociatedContainerNode
      // will meet this requirement.
      if FContainer.ContainedFrame.AssociatedContainerNode <> Node then
        ClearInterfaceReferences;
end;  // TfrmCBMain.fraCBNavigationtvNavChanging 

{-------------------------------------------------------------------------------
}
procedure TfrmCBMain.FrameContainerEditModeChange(Sender: TObject);
begin
  // EditMode has changed on FrameContainer.
  if Assigned(fraCBNavigation.tvNav.Selected) then
    SetButtonStates(TBrowserNode(fraCBNavigation.tvNav.Selected));
end;  // TfrmCBMain.FrameContainerEditModeChange 

{-------------------------------------------------------------------------------
}
procedure TfrmCBMain.FrameNotification(Sender: TObject; AType: TEventType; const AParams:
    TVariantArray);
var
  lData: PChar;
  lKey: PChar;
  lSelectedNode: TBrowserNode;
  lDummy: TVariantArray;
begin
  SetLength(lDummy, 0);
  lKey := '';
  
  if Length(AParams) > 0 then begin
    GetMem(lData, Length(string(AParams[0])));
    StrPCopy(lData, AParams[0]);
    if lData = 'Key' then begin
      GetMem(lKey, SZ_KEY_LENGTH);
      StrPCopy(lKey, AParams[1]);
    end;
  end;
  
  case AType of
    etAddSpecimen:
        PostMessage(Self.Handle, WM_ADD_SPECIMEN, integer(lKey), 0);
    etAddDeterminationLifeScience:
        PostMessage(Self.Handle, WM_ADD_DETERMINATION_LIFE_SCIENCE, integer(lKey), 0);
    etAddDeterminationEarthScience:
        PostMessage(Self.Handle, WM_ADD_DETERMINATION_EARTH_SCIENCE, integer(lKey), 0);
    etRefreshSpecimenCaption:
        SendMessage(Self.Handle, WM_REFRESH_SPECIMEN_CAPTION, integer(lKey), 0);
    etNavigateSpecimen:
        PostMessage(Self.Handle, WM_NAVIGATE_SPECIMEN, integer(lKey), 0);
    etNavigateStore:
        PostMessage(Self.Handle, WM_NAVIGATE_STORE, integer(lKey), 0);
    etNavigateMovement:
        PostMessage(Self.Handle, WM_NAVIGATE_MOVEMENT, integer(lKey), 0);
    etNavigateConditionCheck:
        PostMessage(Self.Handle, WM_NAVIGATE_CONDITION_CHECK, integer(lKey), 0);
    etNavigateJob:
        PostMessage(Self.Handle, WM_NAVIGATE_JOB, integer(lKey), 0);
    etNavigateAccession:
        PostMessage(Self.Handle, WM_NAVIGATE_ACCESSION, integer(lKey), 0);
    etNavigateLoan:
        PostMessage(Self.Handle, WM_NAVIGATE_LOAN, integer(lKey), 0);
    etSelectFolder:
        PostMessage(Self.Handle, WM_NAVIGATE_FOLDER, Integer(lKey), 0);
    etClearInterfaceReferences:
        ClearInterfaceReferences;
    etSortOrderChange:
      begin
        with fraCBNavigation do begin
          lSelectedNode := ReturnParentContainerNode(tvNav.Selected);
          if Assigned(lSelectedNode) then begin
            if lSelectedNode is TFolderNode then begin
              tvNav.Selected := lSelectedNode;
              lSelectedNode.Refresh; //Refresh with new sort order
            end
            else //must be TTopLevelNode
              RepopulateTree(ViewTypeManager.Selected.NodeContext,
                             GetStoredProcName, GetStoredProcParams, '', '', -1, False);
          end
          else
            RepopulateTree(ViewTypeManager.Selected.NodeContext,
                           GetStoredProcName, GetStoredProcParams, '', '', -1, False)
        end;
      end;
    etDestroyDetailsContainer:
      begin
        DestroyContainer;
        SetButtonStates(nil);
      end;
    etInitializeButtons:
      begin
        InitAddButton(nil);
        SetButtonStates(nil);
      end;
    etRefreshDetailsContainer:
      begin
        TBrowserNode(fraCBNavigation.tvNav.Selected).Refresh;
        SetDetailForm(fraCBNavigation.tvNav.Selected, False);
      end;
  end;
end;  // TfrmCBMain.FrameNotification 

{-------------------------------------------------------------------------------
  Accessor method for the Collection Reports menu implementation IDynamicMenu.  Instantiates
      the interface if required.
}
function TfrmCBMain.GetCollectionsReportsMenu: IDynamicMenu;
begin
  Result := CreateCOMObject(CLASS_CollectionsReportsMenu) as IDynamicMenu;
  (Result As ICollectionsReportsMenu).ItemsProvider := Self as IReportItemsProvider;
end;  // TfrmCBMain.GetCollectionsReportsMenu 

{-------------------------------------------------------------------------------
  Retrieve the dynamic menu instance for the edit clipboard operations.
}
function TfrmCBMain.GetEditMenu(AType: TMenuType): IDynamicMenu;
var
  lCopyPasteMenuItem: IDynamicMenu;
begin
  case AType of
    mtCopy: lCopyPasteMenuItem := FCopyMenuItem;
    mtPaste: lCopyPasteMenuItem := FPasteMenuItem
  end;
  
  if not Assigned(lCopyPasteMenuItem) then begin
    lCopyPasteMenuItem := CreateCOMObject(CLASS_CopyPasteMenu) as IDynamicMenu;
    (lCopyPasteMenuItem As ICopyPasteMenu).CopyPasteActions := Self as ICopyPasteActions;
    (lCopyPasteMenuItem As ICopyPasteMenu).MenuTypeIndex := integer(AType);
  end;
  Result := lCopyPasteMenuItem;
end;  // TfrmCBMain.GetEditMenu 

{-------------------------------------------------------------------------------
  Retrieve the dynamic menu instance for the sort by sub menu.
}
function TfrmCBMain.GetSortByMenu: IDynamicMenu;
begin
  Result := CreateCOMObject(CLASS_SortOrderMenu) as IDynamicMenu;
  (Result As ISortOrderMenu).SortableScreen := Self as ISortableScreen;
  // Set index to indicate it is the top level menu item
  (Result As ISortOrderMenu).Index := -1;
end;  // TfrmCBMain.GetSortByMenu 

{-------------------------------------------------------------------------------
}
function TfrmCBMain.Get_Active: WordBool;
begin
  Result := Active;
end;  // TfrmCBMain.Get_Active 

{-------------------------------------------------------------------------------
}
function TfrmCBMain.Get_AlignDisabled: WordBool;
begin
  Result := AlignDisabled;
end;  // TfrmCBMain.Get_AlignDisabled 

{-------------------------------------------------------------------------------
}
function TfrmCBMain.Get_AutoScroll: WordBool;
begin
  Result := AutoScroll;
end;  // TfrmCBMain.Get_AutoScroll 

{-------------------------------------------------------------------------------
}
function TfrmCBMain.Get_AutoSize: WordBool;
begin
  Result := AutoSize;
end;  // TfrmCBMain.Get_AutoSize 

{-------------------------------------------------------------------------------
}
function TfrmCBMain.Get_AxBorderStyle: TxActiveFormBorderStyle;
begin
  Result := Ord(AxBorderStyle);
end;  // TfrmCBMain.Get_AxBorderStyle 

{-------------------------------------------------------------------------------
}
function TfrmCBMain.Get_Caption: WideString;
begin
  Result := WideString(Caption);
end;  // TfrmCBMain.Get_Caption 

{-------------------------------------------------------------------------------
}
function TfrmCBMain.Get_Color: OLE_COLOR;
begin
  Result := OLE_COLOR(Color);
end;  // TfrmCBMain.Get_Color 

{-------------------------------------------------------------------------------
}
function TfrmCBMain.Get_Count: Integer;
begin
  Result := 4;
end;  // TfrmCBMain.Get_Count 

{-------------------------------------------------------------------------------
}
function TfrmCBMain.Get_DoubleBuffered: WordBool;
begin
  Result := DoubleBuffered;
end;  // TfrmCBMain.Get_DoubleBuffered 

{-------------------------------------------------------------------------------
}
function TfrmCBMain.Get_DropTarget: WordBool;
begin
  Result := DropTarget;
end;  // TfrmCBMain.Get_DropTarget 

{-------------------------------------------------------------------------------
}
function TfrmCBMain.Get_DynamicMenuList: IDynamicMenuList;
begin
  Result := Self as IDynamicMenuList;
end;  // TfrmCBMain.Get_DynamicMenuList 

{-------------------------------------------------------------------------------
}
function TfrmCBMain.Get_Enabled: WordBool;
begin
  Result := Enabled;
end;  // TfrmCBMain.Get_Enabled 

{-------------------------------------------------------------------------------
  Collections data can be exported.
}
function TfrmCBMain.Get_Exportable: WordBool;
begin
  Result := True;
end;  // TfrmCBMain.Get_Exportable 

{-------------------------------------------------------------------------------
}
function TfrmCBMain.Get_FolderListCount: Integer;
begin
  Result := 0; // default
  if Assigned(fraCBNavigation.tvNav.Selected) then
    if fraCBNavigation.tvNav.Selected.Level>0 then
      Result :=ReturnParentContainerNode(fraCBNavigation.tvNav.Selected).Count;
end;  // TfrmCBMain.Get_FolderListCount 

{-------------------------------------------------------------------------------
  Return the key of the requested item in the current folder list.  Returns '' if no available
      item.
}
function TfrmCBMain.Get_FolderListKey(AIndex: Integer): WideString;
begin
  Result := ''; // default
  if Assigned(fraCBNavigation.tvNav.Selected) then
    if fraCBNavigation.tvNav.Selected.Level>0 then
      Result := TBrowserNode(ReturnParentContainerNode(
          fraCBNavigation.tvNav.Selected).Items[AIndex]).Key;
end;  // TfrmCBMain.Get_FolderListKey 

{-------------------------------------------------------------------------------
  Return the table associated with the selected node's folder.
}
function TfrmCBMain.Get_FolderListTable: WideString;
begin
  Result := ''; // default
  if Assigned(fraCBNavigation.tvNav.Selected) then
    if fraCBNavigation.tvNav.Selected.Level>0 then
      if Assigned(ReturnParentContainerNode(fraCBNavigation.tvNav.Selected)) then
        if ReturnParentContainerNode(fraCBNavigation.tvNav.Selected).Count>0 then
          Result := ReturnParentContainerNode(fraCBNavigation.tvNav.Selected).TableName;
end;  // TfrmCBMain.Get_FolderListTable 

{-------------------------------------------------------------------------------
}
function TfrmCBMain.Get_Font: IFontDisp;
begin
  GetOleFont(Font, Result);
end;  // TfrmCBMain.Get_Font 

{-------------------------------------------------------------------------------
}
function TfrmCBMain.Get_FormCaption: widestring;
begin
  Result := ResStr_CollectionsBrowser;
end;  // TfrmCBMain.Get_FormCaption 

{-------------------------------------------------------------------------------
}
function TfrmCBMain.Get_HelpFile: WideString;
begin
  Result := WideString(HelpFile);
end;  // TfrmCBMain.Get_HelpFile 

{-------------------------------------------------------------------------------
}
function TfrmCBMain.Get_ImageListHandle: Integer;
begin
  Result := dmInterface.ilMenuItems.Handle;
end;  // TfrmCBMain.Get_ImageListHandle 

{-------------------------------------------------------------------------------
}
function TfrmCBMain.Get_InsertAfterMenu(AIndex: Integer): WideString;
begin
  case AIndex of
    0: Result := ResStr_MnuReports + '\' + ResStr_SpecimenFinder;
  else
    Result := '';
  end;
end;  // TfrmCBMain.Get_InsertAfterMenu 

{-------------------------------------------------------------------------------
}
function TfrmCBMain.Get_InsertBeforeMenu(AIndex: Integer): WideString;
begin
  case AIndex of
    1, 2, 3, 4, 5: Result := ResStr_MnuEdit + '\' + ResStr_MnuReturnData;
  else
    Result := '';
  end;
end;  // TfrmCBMain.Get_InsertBeforeMenu 

{-------------------------------------------------------------------------------
  Retrieve a keylist for the selected data - implements IKeyListSupplier.KeyList
}
function TfrmCBMain.Get_KeyList: IKeyList;
var
  lKeylist: TKeyList;
  lComKeyList: TComKeyList;
begin
  if Assigned(fraCBNavigation.tvNav.Selected) and Assigned(FContainer) then begin
    if (FContainer.ContainedFrame is TfraListViewer) then begin
      if fraCBNavigation.pmTree.PopupComponent is TfraContainer then
        lKeyList := TfraListViewer(FContainer.ContainedFrame).GetKeyList
      else
        lKeyList := TBrowserNode(fraCBNavigation.tvNav.Selected).KeyList;
    end
    else
      lKeyList := TBrowserNode(fraCBNavigation.tvNav.Selected).KeyList;
  end else
    // Create an empty key list as nothing selected.
    lKeyList := TEditableKeylist.Create;
  lComKeyList := TCOMKeyList.Create(lKeyList);
  Result := lComKeyList as IKeyList;
end;  // TfrmCBMain.Get_KeyList 

{-------------------------------------------------------------------------------
}
function TfrmCBMain.Get_KeyPreview: WordBool;
begin
  Result := KeyPreview;
end;  // TfrmCBMain.Get_KeyPreview 

{-------------------------------------------------------------------------------
}
function TfrmCBMain.Get_MenuPath(AIndex: Integer): WideString;
begin
  case AIndex of
    0: Result := ResStr_MnuReports + '\' + ResStr_MnuCollectionsReports;
    1: Result := ResStr_MnuEdit + '\' + ResStr_MnuSortBy;
    2: Result := ResStr_MnuEdit + '\' + ResStr_MnuCopy;
    3: Result := ResStr_MnuEdit + '\' + ResStr_MnuPaste;
  end;
end;  // TfrmCBMain.Get_MenuPath 

{-------------------------------------------------------------------------------
}
function TfrmCBMain.Get_PixelsPerInch: Integer;
begin
  Result := PixelsPerInch;
end;  // TfrmCBMain.Get_PixelsPerInch 

{-------------------------------------------------------------------------------
}
function TfrmCBMain.Get_PrintScale: TxPrintScale;
begin
  Result := Ord(PrintScale);
end;  // TfrmCBMain.Get_PrintScale 

{-------------------------------------------------------------------------------
}
function TfrmCBMain.Get_Scaled: WordBool;
begin
  Result := Scaled;
end;  // TfrmCBMain.Get_Scaled 

{-------------------------------------------------------------------------------
}
function TfrmCBMain.Get_ScreenSnap: WordBool;
begin
  Result := ScreenSnap;
end;  // TfrmCBMain.Get_ScreenSnap 

{-------------------------------------------------------------------------------
}
function TfrmCBMain.Get_SelectedItemKey: WideString;
begin
  if Assigned(fraCBNavigation.tvNav.Selected) then
    Result := TBrowserNode(fraCBNavigation.tvNav.Selected).Key
  else
    Result := '';
end;  // TfrmCBMain.Get_SelectedItemKey 

{-------------------------------------------------------------------------------
  Retrieve the table name for the selected node (or empty string if no selected node).  Used
      to provide information to the Reports menu item.
}
function TfrmCBMain.Get_SelectedItemTable: WideString;
begin
  if Assigned(fraCBNavigation.tvNav.Selected) then begin
    if fraCBNavigation.tvNav.Selected is TFolderNode then
      Result := ''
    else
      Result := TBrowserNode(fraCBNavigation.tvNav.Selected).TableName;
  end
  else
    Result := '';
end;  // TfrmCBMain.Get_SelectedItemTable 

{-------------------------------------------------------------------------------
}
function TfrmCBMain.Get_SnapBuffer: Integer;
begin
  Result := SnapBuffer;
end;  // TfrmCBMain.Get_SnapBuffer 

{-------------------------------------------------------------------------------
  Retrieves the current view type's sort information when requested by the sort menu.
}
function TfrmCBMain.Get_SortProvider: IDispatch;
var
  lContainerNode: TContainerNode;
begin
  with fraCBNavigation.tvNav do begin
    if Assigned(Selected) then begin
      if Selected is TBrowserNode then begin
        lContainerNode := ReturnParentContainerNode(Selected);
        if lContainerNode.NodeContext <> ncNone then
          Supports(fraCBNavigation.ViewTypeManager.ViewTypeByNodeContext(
              lContainerNode.NodeContext),
                  IID_ISortOrderProvider, Result)
        else
          Result := nil;
      end
      else
        Result := nil;
    end
    else
      Supports(fraCBNavigation.ViewTypeManager.Selected, IID_ISortOrderProvider, Result)
  end;
end;  // TfrmCBMain.Get_SortProvider 

{-------------------------------------------------------------------------------
  Return the count of items in the top level.
}
function TfrmCBMain.Get_TopLevelListCount: Integer;
begin
  Result := fraCBNavigation.tvNav.Items.Count;
end;  // TfrmCBMain.Get_TopLevelListCount 

{-------------------------------------------------------------------------------
}
function TfrmCBMain.Get_TopLevelListKey(AIndex: Integer): WideString;
begin
  Result := TBrowserNode(fraCBNavigation.tvNav.Items[AIndex]).Key;
end;  // TfrmCBMain.Get_TopLevelListKey 

{-------------------------------------------------------------------------------
  Return the table associated with top level.
}
function TfrmCBMain.Get_TopLevelListTable: WideString;
begin
  // Use first item in list to see table name as they will all be the same
  if fraCBNavigation.tvNav.Items.Count>0 then
    Result := TBrowserNode(fraCBNavigation.tvNav.Items[0]).TableName
  else
    Result := '';
end;  // TfrmCBMain.Get_TopLevelListTable 

{-------------------------------------------------------------------------------
}
function TfrmCBMain.Get_Visible: WordBool;
begin
  Result := Visible;
end;  // TfrmCBMain.Get_Visible 

{-------------------------------------------------------------------------------
}
function TfrmCBMain.Get_VisibleDockClientCount: Integer;
begin
  Result := VisibleDockClientCount;
end;  // TfrmCBMain.Get_VisibleDockClientCount 

{-------------------------------------------------------------------------------
  Sets up the Add button caption and drop down menu.
}
procedure TfrmCBMain.InitAddButton(ANode: TBrowserNode);
var
  lIntf: IAddMenuOptions;
begin
  pmAdd.Items.Clear;
  if not Assigned(ANode) then begin
    // No node selected, use View Type to get menu
    if fraCBNavigation.ViewTypeManager.Selected.CanAdd and
       Supports(fraCBNavigation.ViewTypeManager.Selected, IID_IAddMenuOptions, lIntf) then
      AddMenuAddItems(lIntf);
  end
  else if ANode is TTopLevelNode then begin
    // Top level node plus View Type node menus returned
    if Supports(fraCBNavigation.ViewTypeManager.Selected, IID_IAddMenuOptions, lIntf) then
      AddMenuAddItems(lIntf);
    if ANode.CanAdd and Supports(ANode, IID_IAddMenuOptions, lIntf) then
      AddMenuAddItems(lIntf);
  end
  else begin
    // Leaf node selected, use its parent folder node to get the add menu
    if ANode.CanAdd and ReturnParentContainerNode(ANode).CanAdd and
       Supports(ReturnParentContainerNode(ANode), IID_IAddMenuOptions, lIntf) then
      AddMenuAddItems(lIntf);
  end;
  // if only 1 item, then use it for the caption
  if pmAdd.Items.Count=1 then
    btnAdd.Caption := pmAdd.Items[0].Caption
  else
    btnAdd.Caption := ResStr_Add;
end;  // TfrmCBMain.InitAddButton 

{-------------------------------------------------------------------------------
}
procedure TfrmCBMain.Initialize;
begin
  inherited Initialize;
  OnActivate := ActivateEvent;
  OnClick := ClickEvent;
  OnCreate := CreateEvent;
  OnDblClick := DblClickEvent;
  OnDeactivate := DeactivateEvent;
  OnDestroy := DestroyEvent;
  OnKeyPress := KeyPressEvent;
  OnPaint := PaintEvent;
  FXPMenu := TXPMenu.Create(Self);
  FXPMenu.XPControls := [xcPopupMenu];
  FXPMenu.Active := True;
  // Tell the frame what to use for XPMenu, so the popups can be properly refreshed.
  fraCBNavigation.XPMenu := FXPMenu;
  OnCloseQuery := FormCloseQuery;
  FComAddins := TComAddins.Create;
end;  // TfrmCBMain.Initialize 

{-------------------------------------------------------------------------------
}
function TfrmCBMain.Items(AIndex: Integer): IDynamicMenu;
begin
  case AIndex of
    0: Result := GetCollectionsReportsMenu;
    1: Result := GetSortByMenu;
    2: Result := GetEditMenu(mtCopy);
    3: Result := GetEditMenu(mtPaste);
  end;
end;  // TfrmCBMain.Items 

{-------------------------------------------------------------------------------
}
procedure TfrmCBMain.KeyPressEvent(Sender: TObject; var Key: Char);
var
  TempKey: SmallInt;
begin
  TempKey := Smallint(Key);
  if FEvents <> nil then FEvents.OnKeyPress(TempKey);
  Key := Char(TempKey);
end;  // TfrmCBMain.KeyPressEvent 

{-------------------------------------------------------------------------------
}
procedure TfrmCBMain.LinkNode(AParentFolderNode: TFolderNode; const AKey, ACaption: string);
var
  lNewNode: TBrowserNode;
  lResult: String;
begin
  AParentFolderNode.ResetConfirmation;

  if AParentFolderNode.ValidateNewNode(AKey, ACaption) and
     AParentFolderNode.ConfirmNodeAction(AKey, ACaption) then
  begin
    lNewNode := AParentFolderNode.LinkNode(AKey, ACaption);
    // The Key returned by the find dialog isn't necessarily always the key
    // required by the new node. Hence, store it separately so they can
    // be compared where required in the UpdateNodeRelationship methods.
    TFolderNode(AParentFolderNode).FindDialogKey := AKey;
  
    //Update relationship between parent and new child
    if FNodeAdded then lResult := AParentFolderNode.UpdateNodeRelationship(lNewNode.Key);
  
    //Find a HyperLinkKey for the new node
    if lNewNode is THyperlinkLeafNode then begin
      THyperlinkLeafNode(lNewNode).FindAndSetDragDropKey;
      THyperlinkLeafNode(lNewNode).FindAndSetHyperlinkKey;
      THyperlinkLeafNode(lNewNode).JoinKey := lResult;
    end;
  
    fraCBNavigation.tvNav.Selected := lNewNode;
  end;
end;  // TfrmCBMain.LinkNode 

{-------------------------------------------------------------------------------
}
procedure TfrmCBMain.LinkNode(AParentTopLevelNode: THyperlinkTopLevelNode; const AKey,
    ACaption: string);
var
  lNewNode: TBrowserNode;
  lResult: String;
begin
  if AParentTopLevelNode.ValidateNewNode(AKey, ACaption) then begin
    lNewNode := AParentTopLevelNode.LinkNode(AKey, ACaption);
    // The Key returned by the find dialog isn't necessarily always the key
    // required by the new node. Hence, store it separately so they can
    // be compared where required in the UpdateNodeRelationship methods.
    THyperlinkTopLevelNode(AParentTopLevelNode).FindDialogKey := AKey;
  
    //Update relationship between parent and new child
    if FNodeAdded then lResult := AParentTopLevelNode.UpdateNodeRelationship(lNewNode.Key);
  
    //Find a HyperLinkKey for the new node
    if lNewNode is THyperlinkLeafNode then begin
      THyperlinkLeafNode(lNewNode).FindAndSetDragDropKey;
      THyperlinkLeafNode(lNewNode).FindAndSetHyperlinkKey;
      THyperlinkLeafNode(lNewNode).JoinKey := lResult;
    end;
  
    fraCBNavigation.tvNav.Selected := lNewNode;
  end;
end;  // TfrmCBMain.LinkNode 

{-------------------------------------------------------------------------------
}
procedure TfrmCBMain.LockDetailFrame(ALockedNode: TBrowserNode);
begin
  FLockedNode := ALockedNode;
  if Assigned(FContainer) then begin
    FContainer.EditMode := emEdit;
    FocusFirstControl(FContainer.ContainedFrame);
  end;
end;  // TfrmCBMain.LockDetailFrame 

{-------------------------------------------------------------------------------
}
procedure TfrmCBMain.PaintEvent(Sender: TObject);
begin
  if FEvents <> nil then FEvents.OnPaint;
end;  // TfrmCBMain.PaintEvent 

{-------------------------------------------------------------------------------
}
procedure TfrmCBMain.Paste;
var
  lBaseDragFrame: TBaseDragFrame;
begin
  if Assigned(ActiveControl) then begin
    lBaseDragFrame := ReturnActiveBaseDragFrame(ActiveControl);
    if Assigned(lBaseDragFrame) then
      if lBaseDragFrame.PasteEnabled then lBaseDragFrame.ExecutePaste(ActiveControl);
  end;
end;  // TfrmCBMain.Paste 

{-------------------------------------------------------------------------------
  This has been moved out of RefreshNodeInfo. This is there are cases where this is required.
      e.g. the Accession number can only be retrieved for a new Collection after
      UpdateNodeRelationship has been called and it has inserted the necessary join records.
      RefreshNodeInfo was previously taking a caption, running UpdateNodeRelationship, then
      setting the old caption. Now we can create joins, retrieve acc number, get correct
      caption and then assign it to the node.
}
procedure TfrmCBMain.RefreshNodeCaption(Sender: TObject; const ACaption: String);
begin
  if ACaption <> '' then FLockedNode.Text := ACaption; //Only update if req'd
  if FLockedNode is TBaseDeterminationLeafNode then
    TBaseDeterminationLeafNode(FLockedNode).RefreshRankColor
  else if FLockedNode is TStoreHierarchySubFolderNode then begin
    TStoreHierarchySubFolderNode(FLockedNode).HasChildren := False;
    FLockedNode.Parent.OverlayIndex := -1;
  end;
  FLockedNode := nil;
end;  // TfrmCBMain.RefreshNodeCaption 

{-------------------------------------------------------------------------------
  Event handler to trap changes to a node key or caption when the locked node is saved to the
      database.
}
procedure TfrmCBMain.RefreshNodeInfo(Sender: TObject; const AKey, ACaption: string);
var
  lDummy: Boolean;
  lTopLevelNode: TTopLevelNode;
  lSelectedNode: TFlyNode;
  lResult: String;
begin
  { TODO : Check if locked node still exists }
  if Assigned(FLockedNode) then begin
    // If node os cancelled, it doesn't matter, but if it's not cancelled, it really does.
    FLockedNode.DiscardOnCancel := False;
  
    if AKey = EmptyStr then begin  //ie Added Node has been cancelled
      // Ensure interfaces cleaned up
      fraCBNavigationtvNavChanging(nil, nil, lDummy);
      with fraCBNavigation.tvNav do begin
        if FLockedNode is TLeafNode then begin
          lTopLevelNode := TLeafNode(FLockedNode).TopLevelNode;
          try
            if Assigned(lTopLevelNode) and
               TLeafNode(FLockedNode).CanRemoveDependents then
            begin
              fraCBNavigation.DeleteNode(lTopLevelNode);
              FLockedPreviousSelectedNode := nil;
            end else
              fraCBNavigation.DeleteNode(FLockedNode);
          except
            on E:Exception do
              //TopLevel could not be removed because of other dependancies
              //so just remove the LeafNode.
              fraCBNavigation.DeleteNode(FLockedNode);
          end;// try
        end else
          //Remove the TopLevelNode
          fraCBNavigation.DeleteNode(FLockedNode);
  
        Selected := FLockedPreviousSelectedNode;
        if Selected = nil then DestroyContainer;
      end;// with
    end else begin
      FLockedNode.Key := AKey;
  
      fraCBNavigation.tvNav.Invalidate;
      //Update HyperlinkLeafNode Key
      if FLockedNode is THyperlinkLeafNode then begin
        THyperlinkLeafNode(FLockedNode).FindAndSetHyperLinkKey;
        THyperlinkLeafNode(FLockedNode).FindAndSetDragDropKey;
      end else
      if FLockedNode is TTopLevelNode then
        TTopLevelNode(FLockedNode).FindAndSetDragDropKey;
  
      //If LeafNode then Update relationship
      if Assigned(FLockedPreviousSelectedNode) then
        if FNodeAdded then begin
          if (FLockedPreviousSelectedNode is TFolderNode) then
            lResult := TFolderNode(FLockedPreviousSelectedNode).
                           UpdateNodeRelationship(FLockedNode.Key)
          else
          // Necessary in case previous node was a leaf node, and
          // UpdateNodeRelationship still needs to be called.
          if (FLockedPreviousSelectedNode is TLeafNode) then
            lResult := TFolderNode(FLockedPreviousSelectedNode.Parent).
                           UpdateNodeRelationship(FLockedNode.Key);
          // Update JoinKey for hyperlink nodes.
          if FLockedNode is THyperlinkLeafNode then
            THyperlinkLeafNode(FLockedNode).JoinKey := lResult;
        end;
    end;
  
    FLockedPreviousSelectedNode := nil;
  
    //Setup Button states again
    lSelectedNode := fraCBNavigation.tvNav.Selected;
    if Assigned(lSelectedNode) then begin
      InitAddButton(TBrowserNode(lSelectedNode));
      SetButtonStates(TBrowserNode(lSelectedNode));
    end
    else begin
      InitAddButton(nil);
      SetButtonStates(nil);
    end;
  end;
end;  // TfrmCBMain.RefreshNodeInfo 

{-------------------------------------------------------------------------------
}
procedure TfrmCBMain.RefreshXPMenu;
begin
  // Got some popup menus on some contained frames, want them handled too.
  if Assigned(FContainer) then
    if Assigned(FContainer.ContainedFrame) then
      FXPMenu.InitComponent(FContainer.ContainedFrame);

  if Assigned(FXPMenu) then begin
    FXPMenu.Active := not FXPMenu.Active;
    FXPMenu.Active := not FXPMenu.Active;
  end;
end;  // TfrmCBMain.RefreshXPMenu 

{-------------------------------------------------------------------------------
}
procedure TfrmCBMain.RequestorUpdate(const KeyList: IKeyList);
var
  lParentContainerNode: TContainerNode;
  lKey, lCaption: String;
begin
  try
    fraCBNavigation.AllowNavigation := true;
  
    if KeyList.ItemCount > 0 then
      lKey := KeyList.GetKeyItem(0).KeyField1
    else
      lKey := '';
  
    if lKey <> '' then begin
      lParentContainerNode := ReturnParentContainerNode(fraCBNavigation.tvNav.Selected);
      if Assigned(lParentContainerNode) then begin
        if (lParentContainerNode is TFolderNode) then begin
          lCaption := dmGeneral.GetStoredProcOutputParam('usp_Concept_Get',
                                                         ['@Key', lKey,
                                                          '@GetPublishedTerm', true], '@Caption');
          LinkNode(TFolderNode(lParentContainerNode), lKey, lCaption);
        end;
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
end;  // TfrmCBMain.RequestorUpdate 

{-------------------------------------------------------------------------------
}
function TfrmCBMain.ReturnActiveBaseDragFrame(AControl: TWinControl): TBaseDragFrame;
begin
  Result := nil;
  if not Assigned(AControl) then Exit;
  
  if AControl is TBaseDragFrame then
    Result := TBaseDragFrame(AControl)
  else if Assigned(AControl.Parent) then
    Result := ReturnActiveBaseDragFrame(AControl.Parent)
  else
    Result := nil;
end;  // TfrmCBMain.ReturnActiveBaseDragFrame 

{-------------------------------------------------------------------------------
}
function TfrmCBMain.ReturnParentContainerNode(ANode: TFlyNode): TContainerNode;
begin
  Result := nil;
  
  if Assigned(ANode) then begin
    if ANode is TContainerNode then
      Result := TContainerNode(ANode)
    else if (ANode is TLeafNode) and assigned(ANode.Parent) then begin
      if (ANode.Parent is TContainerNode) then
        Result := TContainerNode(ANode.Parent);
    end;
  end;
end;  // TfrmCBMain.ReturnParentContainerNode 

{-------------------------------------------------------------------------------
}
procedure TfrmCBMain.SetButtonStates(ANode: TBrowserNode);
var
  lInEditMode: Boolean;
begin
  //If Node has no key, must have just been added
  if Assigned(FContainer) then
    lInEditMode := (ANode.Key = '') or (FContainer.EditMode = emEdit)
  else
    lInEditMode := False;
  
  if lInEditMode then begin
    btnAdd.Enabled := False;
    btnEdit.Enabled := False;
    btnDelete.Enabled := False;
  end
  else if Assigned(ANode) then begin
    btnAdd.Enabled := ANode.CanAdd and (pmAdd.Items.Count>0);
    btnEdit.Enabled := ANode.CanEdit;
    btnDelete.Enabled := ANode.CanDelete;
  end
  else begin
    //Allow Add if user is allowed to add anything in the system
    btnAdd.Enabled := fraCBNavigation.ViewTypeManager.Selected.CanAdd;
    btnEdit.Enabled := False;
    btnDelete.Enabled := False;
  end;
  fraCBNavigation.AllowNavigation := not lInEditMode;
end;  // TfrmCBMain.SetButtonStates 

{-------------------------------------------------------------------------------
}
procedure TfrmCBMain.SetDetailForm(ANode: TFlyNode; ReloadControls: Boolean = False);
begin
  if not (ANode is TBrowserNode) then begin
    if Assigned(FContainer) then
      DestroyContainer;
    Exit;
  end;
  
  if ReloadControls and Assigned(FContainer) then
    DestroyContainer;
  
  if not Assigned(FContainer) then
  begin
    try
      FContainer := TfraContainer.Create(pnlDetails);
    except
      on E: EComponentError do
      begin //bomb-proofing to ensure that FrameContainer has been destroyed.
        if pnlDetails.ControlCount = 1 then
          pnlDetails.Controls[0].Free;
        FContainer := TfraContainer.Create(pnlDetails); //Re-create
      end;
    end; //try
  
    with FContainer do begin
      Parent  := pnlDetails;
      Align   := alClient;
      Visible := True;
      OnRefreshScreenInfo := RefreshNodeInfo;
      OnRefreshNodeCaption := RefreshNodeCaption;
      OnFrameNotification := FrameNotification;
      OnEditModeChange := FrameContainerEditModeChange;
    end;
  end;

  FContainer.lblName.Caption := ANode.Text;
  
  if ANode is TFolderNode then begin
    FContainer.LoadContent(TBrowserNode(ANode).AssociatedFrame, ANode, False, ANode);
    FContainer.PopupMenu := fraCBNavigation.tvNav.PopupMenu;
  end else
    FContainer.LoadContent(TBrowserNode(ANode).AssociatedFrame, ANode, False, ANode);
end;  // TfrmCBMain.SetDetailForm 

{-------------------------------------------------------------------------------
}
procedure TfrmCBMain.Set_AutoScroll(Value: WordBool);
begin
  AutoScroll := Value;
end;  // TfrmCBMain.Set_AutoScroll 

{-------------------------------------------------------------------------------
}
procedure TfrmCBMain.Set_AutoSize(Value: WordBool);
begin
  AutoSize := Value;
end;  // TfrmCBMain.Set_AutoSize 

{-------------------------------------------------------------------------------
}
procedure TfrmCBMain.Set_AxBorderStyle(Value: TxActiveFormBorderStyle);
begin
  AxBorderStyle := TActiveFormBorderStyle(Value);
end;  // TfrmCBMain.Set_AxBorderStyle 

{-------------------------------------------------------------------------------
}
procedure TfrmCBMain.Set_Caption(const Value: WideString);
begin
  Caption := TCaption(Value);
end;  // TfrmCBMain.Set_Caption 

{-------------------------------------------------------------------------------
}
procedure TfrmCBMain.Set_Color(Value: OLE_COLOR);
begin
  Color := TColor(Value);
end;  // TfrmCBMain.Set_Color 

{-------------------------------------------------------------------------------
}
procedure TfrmCBMain.Set_DoubleBuffered(Value: WordBool);
begin
  DoubleBuffered := Value;
end;  // TfrmCBMain.Set_DoubleBuffered 

{-------------------------------------------------------------------------------
}
procedure TfrmCBMain.Set_DropTarget(Value: WordBool);
begin
  DropTarget := Value;
end;  // TfrmCBMain.Set_DropTarget 

{-------------------------------------------------------------------------------
}
procedure TfrmCBMain.Set_Enabled(Value: WordBool);
begin
  Enabled := Value;
end;  // TfrmCBMain.Set_Enabled

{-------------------------------------------------------------------------------
}
procedure TfrmCBMain.Set_Font(const Value: IFontDisp);
begin
  SetOleFont(Font, Value);
end;  // TfrmCBMain.Set_Font 

{-------------------------------------------------------------------------------
}
procedure TfrmCBMain.Set_HelpFile(const Value: WideString);
begin
  HelpFile := String(Value);
end;  // TfrmCBMain.Set_HelpFile 

{-------------------------------------------------------------------------------
}
procedure TfrmCBMain.Set_KeyPreview(Value: WordBool);
begin
  KeyPreview := Value;
end;  // TfrmCBMain.Set_KeyPreview 

{-------------------------------------------------------------------------------
}
procedure TfrmCBMain.Set_PixelsPerInch(Value: Integer);
begin
  PixelsPerInch := Value;
end;  // TfrmCBMain.Set_PixelsPerInch 

{-------------------------------------------------------------------------------
}
procedure TfrmCBMain.Set_PrintScale(Value: TxPrintScale);
begin
  PrintScale := TPrintScale(Value);
end;  // TfrmCBMain.Set_PrintScale 

{-------------------------------------------------------------------------------
}
procedure TfrmCBMain.Set_Scaled(Value: WordBool);
begin
  Scaled := Value;
end;  // TfrmCBMain.Set_Scaled 

{-------------------------------------------------------------------------------
}
procedure TfrmCBMain.Set_ScreenSnap(Value: WordBool);
begin
  ScreenSnap := Value;
end;  // TfrmCBMain.Set_ScreenSnap 

{-------------------------------------------------------------------------------
}
procedure TfrmCBMain.Set_SnapBuffer(Value: Integer);
begin
  SnapBuffer := Value;
end;  // TfrmCBMain.Set_SnapBuffer 

{-------------------------------------------------------------------------------
}
procedure TfrmCBMain.Set_Visible(Value: WordBool);
begin
  Visible := Value;
end;  // TfrmCBMain.Set_Visible 

{-------------------------------------------------------------------------------
}
procedure TfrmCBMain.ShowFindDialog(ASearchType: SearchManager.TSearchType; var
    oSearchResultKey: string; var oSearchResultText: string);
begin
  with TSearchManager.Create do begin
    SearchType := ASearchType;
    oSearchResultKey := RunSearch;
    oSearchResultText := ResultText;
    Free;
  end;
end;  // TfrmCBMain.ShowFindDialog 

{-------------------------------------------------------------------------------
  Ensure right panel does not shrink below size of its constraints, to avoid flicker.
}
procedure TfrmCBMain.SplitterCanResize(Sender: TObject; var NewSize: Integer; var Accept:
    Boolean);
begin
  if NewSize < pnlDetails.Constraints.MinWidth then
    NewSize := pnlDetails.Constraints.MinWidth;
end;  // TfrmCBMain.SplitterCanResize 

{-------------------------------------------------------------------------------
}
function TfrmCBMain.SupportsTable(const ATableName: WideString): WordBool;
var
  lIdx: Integer;
begin
  Result := False;
  with fraCBNavigation.ViewTypeManager do
    // Tables supported for Display Data purposes are the top level ones only
    for lIdx := 0 to ViewTypeCount - 1 do begin
      // Return true if one of the top level nodes available matches the table
      Result := Result or
                (CompareText(ViewType[lIdx].TopLevelNodeClass.ClassTableName, ATableName) = 0);
    end;
end;  // TfrmCBMain.SupportsTable 

{-------------------------------------------------------------------------------
}
procedure TfrmCBMain.UpdateMapWindowSelector;
begin
  if Assigned(FContainer) then
    if Assigned(FContainer.ContainedFrame) then
      FContainer.ContainedFrame.UpdateMapWindowSelector;
end;  // TfrmCBMain.UpdateMapWindowSelector 

{-------------------------------------------------------------------------------
}
procedure TfrmCBMain.WMAddDeterminationEarthScience(var Message: TMessage);
begin
  AddDeterminationNode(False, PChar(Message.WParam));
  FreeMem(Pointer(Message.WParam), SZ_KEY_LENGTH);
end;  // TfrmCBMain.WMAddDeterminationEarthScience 

{-------------------------------------------------------------------------------
}
procedure TfrmCBMain.WMAddDeterminationLifeScience(var Message: TMessage);
begin
  AddDeterminationNode(True, PChar(Message.WParam));
  FreeMem(Pointer(Message.WParam), SZ_KEY_LENGTH);
end;  // TfrmCBMain.WMAddDeterminationLifeScience 

{-------------------------------------------------------------------------------
}
procedure TfrmCBMain.WMAddSpecimen(var Message: TMessage);
var
  lSpecimenTopLevelNode: TTopLevelNode;
begin
  //SetNavigationLock(true);
  with fraCBNavigation do begin
    ViewTypeManager.Selected := ViewTypeManager.ViewTypeByNodeContext(ncSpecimen);
  
    ShowInformation(ResStr_AskForSpecimenDetails);
  
    ClearTreeView;
    with tvNav do begin
      lSpecimenTopLevelNode := TTopLevelNode(Items.AddTypedChild(nil, TSpecimenTopLevelNode));
      lSpecimenTopLevelNode.InitialiseNewNode;
      lSpecimenTopLevelNode.Key := PChar(Message.WParam);
      // Set to True, so if user cancels, node will properly "vanish".
      lSpecimenTopLevelNode.DiscardOnCancel := True;
      Selected := lSpecimenTopLevelNode;
      FreeMem(Pointer(Message.WParam), SZ_KEY_LENGTH);
    end;
  end;
  LockDetailFrame(lSpecimenTopLevelNode);
end;  // TfrmCBMain.WMAddSpecimen 

{-------------------------------------------------------------------------------
  Repopulate the tree with a Specimen. This has been put into a message so that it can be
      posted - i.e. not executed straight away.
}
procedure TfrmCBMain.WMNavigateSpecimen(var Message: TMessage);
begin
  inherited;
  fraCBNavigation.RepopulateTree(ncSpecimen, '', nil, PChar(Message.WParam), '', -1, False);
end;  // TfrmCBMain.WMNavigateSpecimen

{-------------------------------------------------------------------------------
}
procedure TfrmCBMain.WMNavigateStore(var Message: TMessage);
begin
  fraCBNavigation.RepopulateTree(ncStore, '', nil, PChar(Message.WParam), '', -1, False);
  if FContainer.ContainedFrame is TfraStorage then begin
    TfraStorage(FContainer.ContainedFrame).pcDetails.ActivePageIndex := 4;
    TfraStorage(FContainer.ContainedFrame).pcDetails.OnChange(nil);
  end;
end;  // TfrmCBMain.WMNavigateStore

{-------------------------------------------------------------------------------
}
procedure TfrmCBMain.WMNavigateMovement(var Message: TMessage);
begin
  fraCBNavigation.RepopulateTree(
    ncMovement,
    '',
    nil,
    PChar(Message.WParam),
    '',
    -1,
    True);
end;  // TfrmCBMain.WMNavigateMovement

{-------------------------------------------------------------------------------
}
procedure TfrmCBMain.WMNavigateConditionCheck(var Message: TMessage);
begin
  fraCBNavigation.RepopulateTree(
    ncConditionCheck,
    '',
    nil,
    PChar(Message.WParam),
    '',
    -1,
    True);
end;  // TfrmCBMain.WMNavigateConditionCheck

{-------------------------------------------------------------------------------
}
procedure TfrmCBMain.WMNavigateJob(var Message: TMessage);
begin
  fraCBNavigation.RepopulateTree(
    ncJob,
    '',
    nil,
    PChar(Message.WParam),
    '',
    -1,
    True);
end;  // TfrmCBMain.WMNavigateJob

{-------------------------------------------------------------------------------
}
procedure TfrmCBMain.WMNavigateAccession(var Message: TMessage);
begin
  fraCBNavigation.RepopulateTree(
    ncAccession,
    '',
    nil,
    PChar(Message.WParam),
    '',
    -1,
    True);
end;  // TfrmCBMain.WMNavigateAccession

{-------------------------------------------------------------------------------
}
procedure TfrmCBMain.WMNavigateLoan(var Message: TMessage);
begin
  fraCBNavigation.RepopulateTree(
    ncLoan,
    '',
    nil,
    PChar(Message.WParam),
    '',
    -1,
    True);
end;  // TfrmCBMain.WMNavigateLoan

{-------------------------------------------------------------------------------
  Navigates to, and expands, the specified folder node.
}
procedure TfrmCBMain.WMNavigateFolder(var Message: TMessage);
begin
  fraCBNavigation.SelectFolder(PChar(Message.WParam), True);  
end;

{-------------------------------------------------------------------------------
}
procedure TfrmCBMain.WMRefreshSpecimenCaption(var Message: TMessage);
var
  lNode: TBrowserNode;
begin
  if Assigned(FlockedNode) then
    lNode := FLockedNode.TopLevelNode
  else
    lNode := TBrowserNode(fraCBNavigation.tvNav.Selected).TopLevelNode;

  if Assigned(lNode) then begin
    lNode.RefreshCaption;
    if StrLen(PChar(Message.WParam)) = SZ_KEY_LENGTH then begin
      fraCBNavigation.DisplayCommonName(PChar(Message.WParam), lNode);
      FreeMem(Pointer(Message.WParam), SZ_KEY_LENGTH);
    end;
  end;
end;  // TfrmCBMain.WMRefreshSpecimenCaption

{-------------------------------------------------------------------------------
}
procedure TfrmCBMain.WndProc(var Message: TMessage);
var
  msg: TMsg;
begin
  case Message.Msg of
    // TActiveXControl.TranslateAccelerator shall send navigation keys
    WM_GETDLGCODE:
        Message.Result := DLGC_WANTARROWS + DLGC_WANTTAB + DLGC_WANTCHARS;
    // dispatch navigation keys to currently active child control
    WM_KEYDOWN, WM_SYSKEYDOWN, WM_CHAR:
        if (activecontrol <> nil) then
        begin
          msg.hwnd := ActiveControl.handle;
          msg.message := Message.Msg;
          Msg.WParam := Message.WParam;
          Msg.LParam := Message.LParam;
          Msg.time := 0;
          Msg.pt := point(0,0);
          DispatchMessage(msg);
        end;
    else
      inherited; // standard messages
  end;
end;  // TfrmCBMain.WndProc

{-------------------------------------------------------------------------------
}
procedure TfrmCBMain._Set_Font(var Value: IFontDisp);
begin
  SetOleFont(Font, Value);
end;  // TfrmCBMain._Set_Font

initialization
  TActiveFormFactory.Create(
    ComServer,
    TActiveFormControl,
    TfrmCBMain,
    Class_frmCBMain,
    1,
    '',
    OLEMISC_SIMPLEFRAME or OLEMISC_ACTSLIKELABEL,
    tmApartment);
end.



