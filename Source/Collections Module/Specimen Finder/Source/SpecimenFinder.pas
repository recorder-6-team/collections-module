unit SpecimenFinder;

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ActiveX, AxCtrls, SpecimenFinderModule_TLB, StdVcl, Recorder2000_TLB, XPMenu,
  Grids, StdCtrls, ExtCtrls, ComCtrls, Menus, ImageListButton, BaseDragFrameUnit,
  SpecimenFinderDragFrame, exgrid, RapTree, CommonNameFetchQueue, ADODB, DataClasses,
  DropTarget, DragDropControlAssistor, DropSource, StrUtils, XMLDoc, XMLIntf, Math,
  ImgList, BooleanProperty, BooleanCriterionUnit, VagueDate, PreviousResultsCriterion,
  ICriterionUnit, DragAndDropCriterion, msxmldom, xmldom;

type
  {-----------------------------------------------------------------------------
    Form that allows the user to find specimens that are associated with
    specified criteria.  The form is available from the Recorder Reports menu.
    The user is able to use the results list to populate the Collections
    Browser, display a distribution map or populate the Observations screen
    hierarchy.
    The user specifies criteria by dragging and dropping associated items into
    a list.  For example, the user drags a location and an individual into the
    list to find specimens gathered at that location and gathered by or
    determined by the specified person.
  }
  TfrmSpecimenFinder = class(TActiveForm, ISpecimenFinder, IFormCaption,
      IfrmSpecimenFinder, IMapDropFormat, IMapPoint, IDisplayData,
      IMergeMenuManager, IDynamicMenuList)
    fraSpecimenFinderDragFrame: TfraSpecimenFinderDragFrame;
    pcFinder: TPageControl;
    pmAction: TPopupMenu;
    mnuActionSendToCollectionBrowser: TMenuItem;
    mnuActionSendToMap: TMenuItem;
    mnuActionSendToObservationsScreen: TMenuItem;
    tsFinder: TTabSheet;
    tsResults: TTabSheet;
    pmSelect: TPopupMenu;
    mnuActionAutoNumber: TMenuItem;
    mnuActionQuickReport: TMenuItem;
    mnuActionSaveSQLFile: TMenuItem;
    dlgActionSaveSQLFile: TSaveDialog;
    pnlSort: TPanel;
    cmbSortBy: TComboBox;
    lblSortBy: TLabel;
    pnlResults: TPanel;
    pnlResultButtons: TPanel;
    btnActionResults: TImageListButton;
    btnSelectResults: TImageListButton;
    btnRefresh: TImageListButton;
    tvResults: TRapidTree;
    pnlScratchpad: TPanel;
    pnlScratchpadButtons: TPanel;
    btnActionScratchpad: TImageListButton;
    btnSelectScratchpad: TImageListButton;
    btnClearScratchpad: TImageListButton;
    tvScratchpad: TRapidTree;
    pnlSelectionMove: TPanel;
    btnAdd: TImageListButton;
    btnRemove: TImageListButton;
    btnAddAll: TImageListButton;
    btnRemoveAll: TImageListButton;
    ilResults: TImageList;
    pnlCriterion: TPanel;
    lblFilterExplanation: TLabel;
    lblOperand2Caption: TLabel;
    cmbProperty: TComboBox;
    cmbOperator: TComboBox;
    btnAddCriterion: TButton;
    rgBooleanValue: TRadioGroup;
    eOperand1: TEdit;
    eOperand2: TEdit;
    pmActionSendToSpecimenFinder: TMenuItem;
    lblAnd: TLabel;
    lblResultCount: TLabel;
    procedure ActiveFormCreate(Sender: TObject);
    procedure btnActionResultsClick(Sender: TObject);
    procedure pcFinderChanging(Sender: TObject; var AllowChange: Boolean);
    procedure pcFinderResize(Sender: TObject);
    procedure pmActionPopup(Sender: TObject);
    procedure mnuActionSendToCollectionBrowserClick(Sender: TObject);
    procedure mnuActionSendToObservationsScreenClick(Sender: TObject);
    procedure TreeDrawCell(Sender: TObject; aCanvas: TCanvas; ACol, ARow:
        Integer; Rect: TRect; State: TExGridDrawState);
    procedure TreeGetEditText(Sender: TObject; ACol, ARow: Integer; var Value: String);
    procedure btnAddClick(Sender: TObject);
    procedure btnRemoveClick(Sender: TObject);
    procedure btnAddAllClick(Sender: TObject);
    procedure btnRemoveAllClick(Sender: TObject);
    procedure tvResultsSelectCell(Sender: TObject; ACol, ARow: Integer; var CanSelect: Boolean);
    procedure tvScratchpadSelectCell(Sender: TObject; ACol, ARow: Integer; var CanSelect: Boolean);
    procedure btnActionScratchpadClick(Sender: TObject);
    procedure btnSelectResultsClick(Sender: TObject);
    procedure btnSelectScratchpadClick(Sender: TObject);
    procedure btnClearScratchpadClick(Sender: TObject);
    procedure cmbSortByChange(Sender: TObject);
    procedure btnRefreshClick(Sender: TObject);
    procedure TreeDblClick(Sender: TObject);
    procedure TreeKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure TreeMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure mnuActionAutoNumberClick(Sender: TObject);
    procedure mnuActionSaveSQLFileClick(Sender: TObject);
    procedure cmbPropertyChange(Sender: TObject);
    procedure cmbOperatorChange(Sender: TObject);
    procedure eOperand1Change(Sender: TObject);
    procedure eOperand2Change(Sender: TObject);
    procedure btnAddCriterionClick(Sender: TObject);
    procedure cmbPropertySelect(Sender: TObject);
    procedure cmbPropertyKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure cmbPropertyDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure pmActionSendToSpecimenFinderClick(Sender: TObject);
    procedure TextBoxKeyPress(Sender: TObject; var Key: Char);
  private
    FCommonNameFetchQueue: TCommonNameFetchQueue;
    FEvents: IfrmSpecimenFinderEvents;
    FMapPointRecordNumber: Integer;
    FMapPoints: _Recordset;
    FXPMenu: TXPMenu;
    FCurrentTreeView: TRapidTree;
    FDragDropControlAssistor: TDragDropControlAssistor;
    FResultsGridChanged: Boolean;
    FReportList: IXmlReportList;
    FActionsSQL: TStringList;
    FSelectSQL: TStringList;
    FDoubleClicked: Boolean;
    FSortIndex: Integer;
    procedure ActivateEvent(Sender: TObject);
    procedure ApplySort(ASortIndex: Integer);
    procedure ReApplySort();
    procedure ClickEvent(Sender: TObject);
    procedure CreateEvent(Sender: TObject);
    procedure DblClickEvent(Sender: TObject);
    procedure DeactivateEvent(Sender: TObject);
    procedure DestroyEvent(Sender: TObject);
    function ConvertKeyList(AKeyList: IKeyList): TKeyList;
    procedure GetAutoGeneratedActions;
    procedure GetAutoGeneratedSelectors;
    function Get_KeyList(const ADestinationTable: string): IKeyList;
    function Get_SelectedKeyList(const ADestinationTable: string): IKeyList;
    procedure KeyPressEvent(Sender: TObject; var Key: Char);
    procedure MapMenuItemClick(Sender: TObject);
    procedure ReportMenuItemClick(Sender: TObject);
    procedure AutoActionClick(Sender: TObject);
    procedure AutoSelectorClick(Sender: TObject);
    procedure MoveItems(sourceTree, targetTree: TRapidTree; all: Boolean);
    procedure PaintEvent(Sender: TObject);
    procedure PopulateResults(ARecordset: _Recordset);
    procedure PopulateResultCount(AResultCount: Integer);
    procedure PopulateKeyListWithSamples(AKeyList: TEditableKeyList; Selected: Boolean);
    procedure DropSpecimens(ATreeView: TRapidTree; AKey: TKeyString);
    procedure UpdateQuickReportMenu;
    procedure UnselectTreeView(ATreeView: TRapidTree);
    procedure EnableButtons;
    procedure EnableMenus;
    procedure MoveNode(AFromTree, AToTree: TRapidTree; ANode: TFlyNode);
    procedure AddNodes(ATreeView: TRapidTree; ARecordset: _Recordset; ASelected: Boolean = False);
    function HasItemBeenAddedAlready(const AKey: String): Boolean;
    procedure AddOperatorToList(operator: TLogicalOperator);
    procedure EnableAddCriterion;
    function IsCriterionValid : Boolean;
    procedure DisplayInputFields(
            enableOperand1: Boolean;
            displayOperand2: Boolean;
            clearText: Boolean);
    procedure PopulateFilterPropertyList;
    procedure ResetFields;
    function SupportsTable(const ATableName: WideString): WordBool; safecall;
    procedure DisplayData(const AKeyList: IKeyList); safecall;
    procedure ReadXMLQuery(const FileName: string);
    function NodeNameCriterionType(const ANodeName: string): Integer;
    function WriteXMLQuery: TXMLDocument;
    function GetTableName(FinderType: TFinderType): string;
    procedure AddBooleanCriterion(BooleanCriterion: TBooleanCriterion);
    function Get_DynamicMenuList: IDynamicMenuList; safecall;
    function Get_Count: Integer; safecall;
    function Items(AIndex: Integer): IDynamicMenu; safecall;
    function Get_MenuPath(AIndex: Integer): WideString; safecall;
    function Get_InsertBeforeMenu(AIndex: Integer): WideString; safecall;
    function Get_InsertAfterMenu(AIndex: Integer): WideString; safecall;
    function Get_ImageListHandle: Integer; safecall;
  protected
    procedure WMSetDragDrop(var Message: TMessage); message WM_SETDRAGDROP;
    procedure DropResults(const Sender: TObject; const iFormat: integer;
        const iSourceData: TKeyList; const iTextStrings: TstringList; var ioHandled: boolean);
    procedure DropScratchpad(const Sender: TObject; const iFormat: integer;
        const iSourceData: TKeyList; const iTextStrings: TstringList; var ioHandled: boolean);
    procedure DropItems(targetTree: TRapidTree; sourceData: TKeyList);
    procedure DragResults(const Sender: TObject; var oDropSource: TJNCCDropSource);
    procedure DragScratchpad(const Sender: TObject; var oDropSource: TJNCCDropSource);

    procedure DefinePropertyPages(DefinePropertyPage: TDefinePropertyPage); override;
    procedure EventSinkChanged(const EventSink: IUnknown); override;
    function Get_Accuracy: Integer; safecall;
    function Get_Active: WordBool; safecall;
    function Get_AlignDisabled: WordBool; safecall;
    function Get_AutoScroll: WordBool; safecall;
    function Get_AutoSize: WordBool; safecall;
    function Get_AxBorderStyle: TxActiveFormBorderStyle; safecall;
    function Get_Caption: WideString; safecall;
    function Get_Color: OLE_COLOR; safecall;
    function Get_DatasetTableName: WideString; safecall;
    function Get_Date: TDateTime; safecall;
    function Get_DoubleBuffered: WordBool; safecall;
    function Get_DropTarget: WordBool; safecall;
    function Get_Enabled: WordBool; safecall;
    function Get_Font: IFontDisp; safecall;
    function Get_FormCaption: WideString; safecall;
    function Get_HelpFile: WideString; safecall;
    function Get_KeyPreview: WordBool; safecall;
    function Get_Lat: Double; safecall;
    function Get_Long: Double; safecall;
    function Get_MapPoint(Index: Integer): IMapPoint; safecall;
    function Get_MapPointCount: Integer; safecall;
    function Get_PixelsPerInch: Integer; safecall;
    function Get_PrintScale: TxPrintScale; safecall;
    function Get_RecordKey: WideString; safecall;
    function Get_Scaled: WordBool; safecall;
    function Get_ScreenSnap: WordBool; safecall;
    function Get_SnapBuffer: Integer; safecall;
    function Get_SupportedTable(Index: Integer): WideString; safecall;
    function Get_SupportedTableCount: Integer; safecall;
    function Get_Value: Integer; safecall;
    function Get_Visible: WordBool; safecall;
    function Get_VisibleDockClientCount: Integer; safecall;
    procedure InitialiseDataset(const KeyList: IKeyList); safecall;
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
    procedure OpenQuery;
    procedure SaveQuery;
    destructor Destroy; override;
    procedure Initialize; override;
  end;

//==============================================================================

implementation

uses
  ComObj, ComServ, ResourceStrings, ADOInt, Variants,
  InterfaceDataModule, GeneralData, LuxembourgConstants, COMClasses,
  GeneralFunctions, SpatialRefFuncs, Treecoll, ApplicationSettings,
  BaseADODataModule, CollectionsModuleManager_TLB, BatchAutoNumber,
  TypInfo, SpecimenFinderQueryMenu;

resourcestring
  ResStr_ConfirmRefresh =
    'Are you sure you want to refresh the data? You will lose all the changes ' +
    'made to the results grid.';
  ResStr_ConfirmScratchpadClear = 'Are you sure you want to delete all items from the scratchpad?';
  ResStr_NothingSelected = 'You have not selected any specimens. Do you want to select all of them?';
  ResStr_NoMacro = 'You must create a number generation macro before you can use this option.';

const
  MST_FILE_EXISTS = 'The file selected already exists.  Do you want to replace it?';
  ACTIONS_PATH    = 'User Files\Specimen Finder Actions\';
  SELECTORS_PATH  = 'User Files\Specimen Finder Selectors\';
  QUERIES_PATH  = 'User Files\Specimen Finder Queries\';

  COL_ITEM        = 0;
  COL_DOMAIN_NAME = 1;
  COL_LOC_CODE    = 2;
  COL_PREF_NUMBER = 3;
  COL_TYPE        = 4;
  COL_ITEM_NAME   = 5;
  COL_ITEM_KEY    = 6;

  CRITERION_TYPE_DRAG_AND_DROP = 0;
  CRITERION_TYPE_BOOLEAN = 1;
  CRITERION_TYPE_PREVIOUS_RESULTS = 2;

  CR_SPECIMENS = 'Specimens';
  CR_EXPRESSION = 'Expression';
  SPECIMEN_FINDER = 'SpecimenFinder';


{$R *.DFM}

{-==============================================================================
    TfrmSpecimenFinder
===============================================================================}
{-------------------------------------------------------------------------------
}
destructor TfrmSpecimenFinder.Destroy;
begin
  AppSettings.SpecimenFinderSortOrderIndex := cmbSortBy.ItemIndex;
  FActionsSQL.Free;
  FSelectSQL.Free;
  FDragDropControlAssistor.Free;
  fraSpecimenFinderDragFrame.CleanUp;

  if TdmGeneral.Allocated then TdmGeneral.Discard;
  inherited;
end;  // TfrmSpecimenFinder.Destroy

{-------------------------------------------------------------------------------
}
procedure TfrmSpecimenFinder.ActivateEvent(Sender: TObject);
begin
  if FEvents <> nil then FEvents.OnActivate;
end;  // TfrmSpecimenFinder.ActivateEvent

{-------------------------------------------------------------------------------
}
procedure TfrmSpecimenFinder.ActiveFormCreate(Sender: TObject);
begin
  FCommonNameFetchQueue := TCommonNameFetchQueue.Create(tvResults);
  FCommonNameFetchQueue.Parent := tvResults;
  FCommonNameFetchQueue.TreeView := TRapidTree(tvResults);

  // Hide the columns that need hiding
  tvResults.ColWidths[COL_ITEM_KEY]       := -1;
  tvResults.ColWidths[COL_DOMAIN_NAME]    := -1;
  tvResults.ColWidths[COL_PREF_NUMBER]    := -1;
  tvResults.ColWidths[COL_TYPE]           := -1;
  tvResults.ColWidths[COL_ITEM_NAME]      := -1;
  tvScratchpad.ColWidths[COL_ITEM_KEY]    := -1;
  tvScratchpad.ColWidths[COL_DOMAIN_NAME] := -1;
  tvScratchpad.ColWidths[COL_PREF_NUMBER] := -1;
  tvScratchpad.ColWidths[COL_TYPE]        := -1;
  tvScratchpad.ColWidths[COL_ITEM_NAME]   := -1;

  cmbSortBy.ItemIndex := AppSettings.SpecimenFinderSortOrderIndex;
  FSortIndex := cmbSortBy.ItemIndex;

  FCurrentTreeView := tvResults;
  
  mnuActionAutoNumber.Visible := AppSettings.UserAccessLevel > 2;
  // Add the auto-generated items to the menus
  FActionsSQL := TStringList.Create;
  if AppSettings.UserAccessLevel > 2 then GetAutoGeneratedActions;

  FSelectSQL := TStringList.Create;
  GetAutoGeneratedSelectors;
  
end;  // TfrmSpecimenFinder.ActiveFormCreate

{-------------------------------------------------------------------------------
}
procedure TfrmSpecimenFinder.btnActionResultsClick(Sender: TObject);
var
  lPosPopup: TPoint;
begin    
  FCurrentTreeView := tvResults;
  // Work out where to show the popupmenu so that it appears just under the button
  lPosPopup := btnActionResults.ClientToScreen(Point(0, btnActionResults.Height));
  // For menu to appear at calculated position
  pmAction.Popup(lPosPopup.X, lPosPopup.Y);
end;  // TfrmSpecimenFinder.btnActionResultsClick 

{-------------------------------------------------------------------------------
}
procedure TfrmSpecimenFinder.ClickEvent(Sender: TObject);
begin
  if FEvents <> nil then FEvents.OnClick;
end;  // TfrmSpecimenFinder.ClickEvent 

{-------------------------------------------------------------------------------
}
procedure TfrmSpecimenFinder.CreateEvent(Sender: TObject);
begin
  if FEvents <> nil then FEvents.OnCreate;
end;  // TfrmSpecimenFinder.CreateEvent 

{-------------------------------------------------------------------------------
}
procedure TfrmSpecimenFinder.DblClickEvent(Sender: TObject);
begin
  if FEvents <> nil then FEvents.OnDblClick;
end;  // TfrmSpecimenFinder.DblClickEvent

{-------------------------------------------------------------------------------
}
procedure TfrmSpecimenFinder.DeactivateEvent(Sender: TObject);
begin
  if FEvents <> nil then FEvents.OnDeactivate;
end;  // TfrmSpecimenFinder.DeactivateEvent

{-------------------------------------------------------------------------------
}
procedure TfrmSpecimenFinder.DefinePropertyPages(DefinePropertyPage:
    TDefinePropertyPage);
begin
  { Define property pages here.  Property pages are defined by calling
    DefinePropertyPage with the class id of the page.  For example,
      DefinePropertyPage(Class_frmSpecimenFinderPage); }
end;  // TfrmSpecimenFinder.DefinePropertyPages 

{-------------------------------------------------------------------------------
}
procedure TfrmSpecimenFinder.DestroyEvent(Sender: TObject);
begin
  if FEvents <> nil then FEvents.OnDestroy;
end;  // TfrmSpecimenFinder.DestroyEvent 

{-------------------------------------------------------------------------------
}
procedure TfrmSpecimenFinder.EventSinkChanged(const EventSink: IUnknown);
begin
  FEvents := EventSink as IfrmSpecimenFinderEvents;
  inherited EventSinkChanged(EventSink);
end;  // TfrmSpecimenFinder.EventSinkChanged 

{-------------------------------------------------------------------------------
  Retrieve the sample spatial reference accuracy for the map. 
}
function TfrmSpecimenFinder.Get_Accuracy: Integer;
var
  lSR: string;
  lSystem: string;
  lSpatialRef: string;
begin
  // Default to 0 for all.
  Result := 0;
  lSpatialRef := VarToStr(FMapPoints.Fields['Spatial_Ref'].Value);
  lSystem := VarToStr(FMapPoints.Fields['Spatial_Ref_System'].Value);

  // For UK and Irish spatial references then points have a cut-in and a cut out
  if (CompareText(lSystem, OS_GB) = 0) or (CompareText(lSystem, OS_NI) = 0) then begin
    lSR := UpperCase(StringReplace(lSpatialRef, ' ', '', [rfReplaceAll]));

    // Check for a TETRAD
    if ((lSystem = OS_GB) and (Length(lSR) = 5) and (lSR[5] in ['A'..'Z'])) or
       ((lSystem = OS_NI) and (Length(lSR) = 4) and (lSR[4] in ['A'..'Z'])) then
    begin
      Result := 2000;
      Exit;
    end;

    // Obviously not a tetrad then.
    if lSystem = OS_NI then
      // Add a supurious 'Z' so that it now has a 2 figure prefix, so its
      // length is equivalent to a UK Spatial Ref of the same accuracy
      lSR := 'Z' + lSR;

    // Now work out the accuracy.
    case Length(lSR) of
       2: Result := 100000;
       4: Result :=  10000;
       6: Result :=   1000;
       8: Result :=    100;
      10: Result :=     10;
      12: Result :=      1;
    end;
  end;
end;  // TfrmSpecimenFinder.Get_Accuracy

{-------------------------------------------------------------------------------
}
function TfrmSpecimenFinder.Get_Active: WordBool;
begin
  Result := Active;
end;  // TfrmSpecimenFinder.Get_Active

{-------------------------------------------------------------------------------
}
function TfrmSpecimenFinder.Get_AlignDisabled: WordBool;
begin
  Result := AlignDisabled;
end;  // TfrmSpecimenFinder.Get_AlignDisabled

{-------------------------------------------------------------------------------
}
function TfrmSpecimenFinder.Get_AutoScroll: WordBool;
begin
  Result := AutoScroll;
end;  // TfrmSpecimenFinder.Get_AutoScroll

{-------------------------------------------------------------------------------
}
function TfrmSpecimenFinder.Get_AutoSize: WordBool;
begin
  Result := AutoSize;
end;  // TfrmSpecimenFinder.Get_AutoSize

{-------------------------------------------------------------------------------
}
function TfrmSpecimenFinder.Get_AxBorderStyle: TxActiveFormBorderStyle;
begin
  Result := Ord(AxBorderStyle);
end;  // TfrmSpecimenFinder.Get_AxBorderStyle

{-------------------------------------------------------------------------------
}
function TfrmSpecimenFinder.Get_Caption: WideString;
begin
  Result := WideString(Caption);
end;  // TfrmSpecimenFinder.Get_Caption

{-------------------------------------------------------------------------------
}
function TfrmSpecimenFinder.Get_Color: OLE_COLOR;
begin
  Result := OLE_COLOR(Color);
end;  // TfrmSpecimenFinder.Get_Color

{-------------------------------------------------------------------------------
  Map points dropped are always a list of samples.
}
function TfrmSpecimenFinder.Get_DatasetTableName: WideString;
begin
  Result := TN_SAMPLE;
end;  // TfrmSpecimenFinder.Get_DatasetTableName

{-------------------------------------------------------------------------------
  Retrieve the sample date for the map.
}
function TfrmSpecimenFinder.Get_Date: TDateTime;
begin
  if not VarIsNull(FMapPoints.Fields['Vague_Date_Start'].Value) then
    Result := TDateTime(FMapPoints.Fields['Vague_Date_Start'].Value)
  else
    // use a very old date if start date not known
    Result := EncodeDate(1000, 1, 1);
end;  // TfrmSpecimenFinder.Get_Date

{-------------------------------------------------------------------------------
}
function TfrmSpecimenFinder.Get_DoubleBuffered: WordBool;
begin
  Result := DoubleBuffered;
end;  // TfrmSpecimenFinder.Get_DoubleBuffered

{-------------------------------------------------------------------------------
}
function TfrmSpecimenFinder.Get_DropTarget: WordBool;
begin
  Result := DropTarget;
end;  // TfrmSpecimenFinder.Get_DropTarget

{-------------------------------------------------------------------------------
}
function TfrmSpecimenFinder.Get_Enabled: WordBool;
begin
  Result := Enabled;
end;  // TfrmSpecimenFinder.Get_Enabled

{-------------------------------------------------------------------------------
}
function TfrmSpecimenFinder.Get_Font: IFontDisp;
begin
  GetOleFont(Font, Result);
end;  // TfrmSpecimenFinder.Get_Font

{-------------------------------------------------------------------------------
}
function TfrmSpecimenFinder.Get_FormCaption: WideString;
begin
  Result := ResStr_SpecimenFinder;
end;  // TfrmSpecimenFinder.Get_FormCaption

{-------------------------------------------------------------------------------
}
function TfrmSpecimenFinder.Get_HelpFile: WideString;
begin
  Result := WideString(HelpFile);
end;  // TfrmSpecimenFinder.Get_HelpFile

{-------------------------------------------------------------------------------
  Gets the keys of all the items in the grid
}
function TfrmSpecimenFinder.Get_KeyList(const ADestinationTable: string):
    IKeyList;
var
  lIdx: Integer;
  lKeylist: TEditableKeyList;
  lComKeyList: TComKeyList;
begin
  // Create an empty key list which will be used if nothing is selected.
  lKeylist := TEditableKeylist.Create;
  try
    lKeylist.SetTable(ADestinationTable);
    if FCurrentTreeView.Items.Count > 0 then begin
      if ADestinationTable = TN_SPECIMEN_UNIT then
        for lIdx := 0 to FCurrentTreeView.Items.Count -1 do
          // Add the keys
          lKeylist.AddItem(FCurrentTreeView.Items.Item[lIdx].Cells[COL_ITEM_KEY], '')
      else
      if ADestinationTable = TN_SAMPLE then
        PopulateKeyListWithSamples(lKeyList, False);
    end;
    lComKeyList := TCOMKeyList.Create(lKeyList);
    Result := lComKeyList as IKeyList;
  finally
    lKeyList.Free;
  end; // try
end;  // TfrmSpecimenFinder.Get_KeyList

{-------------------------------------------------------------------------------
}
function TfrmSpecimenFinder.Get_KeyPreview: WordBool;
begin
  Result := KeyPreview;
end;  // TfrmSpecimenFinder.Get_KeyPreview

{-------------------------------------------------------------------------------
  Retrieve the sample latitude for the map.
}
function TfrmSpecimenFinder.Get_Lat: Double;
begin
  Result := FMapPoints.Fields['Lat'].Value;
end;  // TfrmSpecimenFinder.Get_Lat

{-------------------------------------------------------------------------------
  Retrieve the sample longitude for the map.
}
function TfrmSpecimenFinder.Get_Long: Double;
begin
  Result := FMapPoints.Fields['Long'].Value;
end;  // TfrmSpecimenFinder.Get_Long

{-------------------------------------------------------------------------------
  Position the samples recordset at the correct record, then return self as
      IMapPoint.  This way only one IMapPoint class is needed.
  This assumes (correctly) that Recorder will work through the points from
      start to finish.  If it does anything else, then performance would be
      poor.
}
function TfrmSpecimenFinder.Get_MapPoint(Index: Integer): IMapPoint;
begin
  // Go back to beginning of recordset only if required
  if Index < FMapPointRecordNumber then begin
    FMapPoints.MoveFirst;
    FMapPointRecordNumber := 0;
  end;
  // Advance recordset to required row
  while FMapPointRecordNumber<Index do begin
    FMapPoints.MoveNext;
    Inc(FMapPointRecordNumber);
  end;
  Result := Self as IMapPoint;
end;  // TfrmSpecimenFinder.Get_MapPoint

{-------------------------------------------------------------------------------
  Return the number of map points found for the list of specimens, by returning
      the recordcount of the sample recordset.
}
function TfrmSpecimenFinder.Get_MapPointCount: Integer;
begin
  if assigned(FMapPoints) then
    Result := FMapPoints.RecordCount
  else
    Result := 0;
end;  // TfrmSpecimenFinder.Get_MapPointCount

{-------------------------------------------------------------------------------
}
function TfrmSpecimenFinder.Get_PixelsPerInch: Integer;
begin
  Result := PixelsPerInch;
end;  // TfrmSpecimenFinder.Get_PixelsPerInch

{-------------------------------------------------------------------------------
}
function TfrmSpecimenFinder.Get_PrintScale: TxPrintScale;
begin
  Result := Ord(PrintScale);
end;  // TfrmSpecimenFinder.Get_PrintScale

{-------------------------------------------------------------------------------
}
function TfrmSpecimenFinder.Get_RecordKey: WideString;
begin
  Result := FMapPoints.Fields['Sample_Key'].Value;
end;  // TfrmSpecimenFinder.Get_RecordKey

{-------------------------------------------------------------------------------
}
function TfrmSpecimenFinder.Get_Scaled: WordBool;
begin
  Result := Scaled;
end;  // TfrmSpecimenFinder.Get_Scaled

{-------------------------------------------------------------------------------
}
function TfrmSpecimenFinder.Get_ScreenSnap: WordBool;
begin
  Result := ScreenSnap;
end;  // TfrmSpecimenFinder.Get_ScreenSnap

{-------------------------------------------------------------------------------
}
function TfrmSpecimenFinder.Get_SnapBuffer: Integer;
begin
  Result := SnapBuffer;
end;  // TfrmSpecimenFinder.Get_SnapBuffer

{-------------------------------------------------------------------------------
  No additional tables are supported for map drops by this addin.
}
function TfrmSpecimenFinder.Get_SupportedTable(Index: Integer): WideString;
begin
  Result := '';
end;  // TfrmSpecimenFinder.Get_SupportedTable

{-------------------------------------------------------------------------------
  No additional tables are supported for map drops by this addin.
}
function TfrmSpecimenFinder.Get_SupportedTableCount: Integer;
begin
  Result := 0;
end;  // TfrmSpecimenFinder.Get_SupportedTableCount

{-------------------------------------------------------------------------------
  No value implemented for map points.  For future use?
}
function TfrmSpecimenFinder.Get_Value: Integer;
begin
  Result := 0;
end;  // TfrmSpecimenFinder.Get_Value

{-------------------------------------------------------------------------------
}
function TfrmSpecimenFinder.Get_Visible: WordBool;
begin
  Result := Visible;
end;  // TfrmSpecimenFinder.Get_Visible

{-------------------------------------------------------------------------------
}
function TfrmSpecimenFinder.Get_VisibleDockClientCount: Integer;
begin
  Result := VisibleDockClientCount;
end;  // TfrmSpecimenFinder.Get_VisibleDockClientCount

{-------------------------------------------------------------------------------
}
procedure TfrmSpecimenFinder.InitialiseDataset(const KeyList: IKeyList);
begin

end;  // TfrmSpecimenFinder.InitialiseDataset

{-------------------------------------------------------------------------------
}
procedure TfrmSpecimenFinder.Initialize;

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
  pcFinder.ActivePage := tsFinder;

  FXPMenu := TXPMenu.Create(Self);
  FXPMenu.XPControls := [xcPopupMenu];
  FXPMenu.Active := True;

  // This ensures the tabs are aligned properly.
  pcFinder.Align := alClient;

  rgBooleanValue.Columns := 2;
  rgBooleanValue.ItemIndex := 0;

  FDragDropControlAssistor := TDragDropControlAssistor.Create(Self);
  FResultsGridChanged := False;

  PopulateFilterPropertyList;
  ResetFields;
end;  // TfrmSpecimenFinder.Initialize

{-------------------------------------------------------------------------------
}
procedure TfrmSpecimenFinder.KeyPressEvent(Sender: TObject; var Key: Char);
var
  TempKey: SmallInt;
begin
  TempKey := Smallint(Key);
  if FEvents <> nil then FEvents.OnKeyPress(TempKey);
  Key := Char(TempKey);
end;  // TfrmSpecimenFinder.KeyPressEvent

{-------------------------------------------------------------------------------
  When a menu item is clicked for an available map, display the map window with
      the dataset loaded onto it.
}
procedure TfrmSpecimenFinder.MapMenuItemClick(Sender: TObject);
var
  lIdx: Integer;
begin
  // Create a temp table to hold the specimen list
  dmGeneral.ExecuteSQL(SQL_CREATETEMPFILTER);
  try
    // Populate the specimen list
    for lIdx := 0 to FCurrentTreeView.Items.Count - 1 do
      dmGeneral.ExecuteSQL(Format(SQL_INSERTTEMPFILTER,
                                  [FCurrentTreeView.Items.Item[lIdx].Cells[COL_ITEM_KEY]]));
    // Convert the specimen list to a sample list
    FMapPoints :=  dmGeneral.GetRecordset('usp_Samples_Select_ForSpecimenKeysMap', []);
    if not FMapPoints.Eof then begin
      FMapPoints.MoveFirst;
      FMapPointRecordNumber := 0;
      if Sender is TMenuItem then
        dmGeneral.Recorder.CurrentSettings.AvailableMap[TMenuItem(Sender).Tag].
              DisplayDistributionPoints(ResStr_Specimens, Self as IMapDropFormat);
    end else
      ShowInformation(ResStr_NoObservationsToMap);
  finally
    dmGeneral.ExecuteSQL(SQL_DROPTEMPFILTER);
  end;
end;  // TfrmSpecimenFinder.MapMenuItemClick

{-------------------------------------------------------------------------------
}
procedure TfrmSpecimenFinder.PaintEvent(Sender: TObject);
begin
  if FEvents <> nil then FEvents.OnPaint;
end;  // TfrmSpecimenFinder.PaintEvent

{-------------------------------------------------------------------------------
}
procedure TfrmSpecimenFinder.pcFinderChanging(Sender: TObject; var AllowChange: Boolean);
var
  lCursor: TCursor;
  lRS: _Recordset;
begin
  if pcFinder.ActivePageIndex <> 0 then Exit;

  if fraSpecimenFinderDragFrame.RefreshResults then begin
    lRS := fraSpecimenFinderDragFrame.GetResults;

    if Assigned(lRS) then begin
      lCursor := HourglassCursor;
      try
        PopulateResults(lRS);
        pcFinderResize(nil);
        // Initialise the columns to be visible. User can resize afterwards.
        tvResults.ColWidths[COL_ITEM]        := tvResults.Width div 2 - 3;
        tvResults.ColWidths[COL_LOC_CODE]    := tvResults.ColWidths[COL_ITEM];
        tvScratchpad.ColWidths[COL_ITEM]     := tvResults.ColWidths[COL_ITEM];
        tvScratchpad.ColWidths[COL_LOC_CODE] := tvResults.ColWidths[COL_ITEM];
        ReApplySort;
      finally
        DefaultCursor(lCursor);
      end;
    end;
  end;
  EnableButtons;
end;  // TfrmSpecimenFinder.pcFinderChanging

{-------------------------------------------------------------------------------
  Keep the grids equal width whatever size the from is, and then make sure all
  the controls are aligned correctly
}
procedure TfrmSpecimenFinder.pcFinderResize(Sender: TObject);
begin
  pnlResults.Width := (tsResults.Width - pnlSelectionMove.Width) div 2;
  pnlScratchpad.Width := pnlResults.Width;
  tvResults.UpdateScrollRange;
  tvScratchpad.UpdateScrollRange;
end;  // TfrmSpecimenFinder.pcFinderResize

{-------------------------------------------------------------------------------
  When the output menu is popped up, reload the list of available maps. 
}
procedure TfrmSpecimenFinder.pmActionPopup(Sender: TObject);
var
  i: Integer;
begin
  // Add the specimen reports to the quick report menu item
  with (CreateOleObject('Recorder2000.AutoApplicationSettings') as IRecorder6) do
    FReportList := GetAvailableXmlReports(ResStr_Specimen);
  UpdateQuickReportMenu;
  mnuActionQuickReport.Visible := FReportList.ReportCount > 0;

  dmGeneral.UpdateMapMenu(nil, mnuActionSendToMap, True, MapMenuItemClick);
  // Ensure the map subitems have the correct image
  for i := 0 to mnuActionSendToMap.Count - 1 do
    mnuActionSendToMap.Items[i].ImageIndex := 10;
  // Hide the SendToMap option if no maps available
  mnuActionSendToMap.Visible := dmGeneral.Recorder.CurrentSettings.AvailableMapCount > 0;

  // Make sure the sub-menu is "XPMenu-ed" too.
  if Assigned(FXPMenu) then
    FXPMenu.InitComponent(Self);

  EnableMenus;
end;  // TfrmSpecimenFinder.pmActionPopup

{-------------------------------------------------------------------------------
}
procedure TfrmSpecimenFinder.mnuActionSendToCollectionBrowserClick(Sender: TObject);
var
  lKeyList: IKeyList;
  lNode: TFlyNode;
begin
  lNode := FCurrentTreeView.Items.GetFirstSelectedNode;
  if Assigned(lNode) then begin
    lKeyList := Get_SelectedKeyList(TN_SPECIMEN_UNIT);
  end else
  if FCurrentTreeView.Items.Count = 0 then
      ShowInformation(ResStr_NoSpecimensToDisplay)
  else
  if MessageDlg(ResStr_NothingSelected, mtConfirmation, [mbYes, mbNo], 0) = mrYes then begin
    lKeyList := Get_KeyList(TN_SPECIMEN_UNIT);
  end;

  if Assigned(lKeyList) then begin
    if lKeyList.ItemCount > 0 then begin
      dmGeneral.Recorder.MenuOptionClick(ResStr_MnuDataEntry + ';' + ResStr_MnuCollectionsBrowser);
      dmGeneral.Recorder.DisplayData(TN_SPECIMEN_UNIT, lKeyList);
      if TdmGeneral.Allocated then TdmGeneral.Discard;
    end;
  end;
end;  // TfrmSpecimenFinder.mnuActionSendToCollectionBrowserClick

{-------------------------------------------------------------------------------
}
procedure TfrmSpecimenFinder.mnuActionSendToObservationsScreenClick(Sender: TObject);
var
  lKeyList: IKeyList;
  lNode: TFlyNode;
begin
  lNode := FCurrentTreeView.Items.GetFirstSelectedNode;
  if Assigned(lNode) then begin
    lKeyList := Get_SelectedKeyList(TN_SAMPLE);
  end else
  if MessageDlg(ResStr_NothingSelected, mtConfirmation, [mbYes, mbNo], 0) = mrYes then begin
    lKeyList := Get_KeyList(TN_SAMPLE);
  end;
  if (Assigned(lKeyList)) then begin
    if (lKeyList.ItemCount > 0) then begin
      dmGeneral.Recorder.MenuOptionClick(ResStr_MnuDataEntry + ';' +
                                         ResStr_MnuObservations);
      dmGeneral.Recorder.DisplayData(TN_SAMPLE, lKeyList);
    end else
      ShowInformation(ResStr_NoObservationsToDisplay);
  end;
end;  // TfrmSpecimenFinder.mnuActionSendToObservationsScreenClick

{-------------------------------------------------------------------------------
}
procedure TfrmSpecimenFinder.PopulateResults(ARecordset: _Recordset);
begin
  try
    tvResults.Items.BeginUpdate;
    tvResults.Items.Clear;
    if ARecordset.RecordCount > 0 then begin
      AddNodes(tvResults, ARecordset);
      FCommonNameFetchQueue.ProcessWhenReady;
    end;
    PopulateResultCount(tvResults.Items.Count);
  finally
    tvResults.Items.EndUpdate;
  end;
end;  // TfrmSpecimenFinder.PopulateResults

{-------------------------------------------------------------------------------
 Populates the Result Count label with the value held in FNodeCount
}
procedure TfrmSpecimenFinder.PopulateResultCount(AResultCount: Integer);
begin
  if(AResultCount = 1) then
    lblResultCount.Caption := IntToStr(AResultCount) + ' Result'
  else
    lblResultCount.Caption := IntToStr(AResultCount) + ' Results';
end;

{-------------------------------------------------------------------------------
  Populates the keylist with the list of samples associated with the specimens. 
}
procedure TfrmSpecimenFinder.PopulateKeyListWithSamples(AKeyList:
    TEditableKeyList; Selected: Boolean);
var
  i: Integer;
  lNode: TFlyNode;
begin
  // Create a temp table to hold the specimen list
  dmGeneral.ExecuteSQL(SQL_CREATETEMPFILTER);
  try
    // Populate the specimen list
    if Selected then begin
      lNode := FCurrentTreeView.Items.GetFirstSelectedNode;
      while Assigned(lNode) do begin
        dmGeneral.ExecuteSQL(Format(SQL_INSERTTEMPFILTER, [lNode.Cells[COL_ITEM_KEY]]));
        lNode := FCurrentTreeView.Items.GetNextSelectedNode(lNode)
      end;
    end else
      for i := 0 to FCurrentTreeView.Items.Count -1 do
        dmGeneral.ExecuteSQL(
            Format(SQL_INSERTTEMPFILTER,
                [FCurrentTreeView.Items.Item[i].Cells[COL_ITEM_KEY]]));

    // Convert the specimen list to a sample list
    with dmGeneral.GetRecordset('usp_SampleKeys_Select_ForSpecimenKeys', []) do begin
      if not Eof then begin
        MoveFirst;
        while not Eof do begin
          AKeylist.AddItem(Fields['Sample_Key'].Value, '');
          MoveNext;
        end;
      end;
      Close;
    end;
  finally
    dmGeneral.ExecuteSQL(SQL_DROPTEMPFILTER);
  end;
end;  // TfrmSpecimenFinder.PopulateKeylistWithSamples

{-------------------------------------------------------------------------------
}
procedure TfrmSpecimenFinder.Set_AutoScroll(Value: WordBool);
begin
  AutoScroll := Value;
end;  // TfrmSpecimenFinder.Set_AutoScroll 

{-------------------------------------------------------------------------------
}
procedure TfrmSpecimenFinder.Set_AutoSize(Value: WordBool);
begin
  AutoSize := Value;
end;  // TfrmSpecimenFinder.Set_AutoSize 

{-------------------------------------------------------------------------------
}
procedure TfrmSpecimenFinder.Set_AxBorderStyle(Value: TxActiveFormBorderStyle);
begin
  AxBorderStyle := TActiveFormBorderStyle(Value);
end;  // TfrmSpecimenFinder.Set_AxBorderStyle 

{-------------------------------------------------------------------------------
}
procedure TfrmSpecimenFinder.Set_Caption(const Value: WideString);
begin
  Caption := TCaption(Value);
end;  // TfrmSpecimenFinder.Set_Caption 

{-------------------------------------------------------------------------------
}
procedure TfrmSpecimenFinder.Set_Color(Value: OLE_COLOR);
begin
  Color := TColor(Value);
end;  // TfrmSpecimenFinder.Set_Color 

{-------------------------------------------------------------------------------
}
procedure TfrmSpecimenFinder.Set_DoubleBuffered(Value: WordBool);
begin
  DoubleBuffered := Value;
end;  // TfrmSpecimenFinder.Set_DoubleBuffered 

{-------------------------------------------------------------------------------
}
procedure TfrmSpecimenFinder.Set_DropTarget(Value: WordBool);
begin
  DropTarget := Value;
end;  // TfrmSpecimenFinder.Set_DropTarget 

{-------------------------------------------------------------------------------
}
procedure TfrmSpecimenFinder.Set_Enabled(Value: WordBool);
begin
  Enabled := Value;
end;  // TfrmSpecimenFinder.Set_Enabled 

{-------------------------------------------------------------------------------
}
procedure TfrmSpecimenFinder.Set_Font(const Value: IFontDisp);
begin
  SetOleFont(Font, Value);
end;  // TfrmSpecimenFinder.Set_Font 

{-------------------------------------------------------------------------------
}
procedure TfrmSpecimenFinder.Set_HelpFile(const Value: WideString);
begin
  HelpFile := String(Value);
end;  // TfrmSpecimenFinder.Set_HelpFile 

{-------------------------------------------------------------------------------
}
procedure TfrmSpecimenFinder.Set_KeyPreview(Value: WordBool);
begin
  KeyPreview := Value;
end;  // TfrmSpecimenFinder.Set_KeyPreview

{-------------------------------------------------------------------------------
}
procedure TfrmSpecimenFinder.Set_PixelsPerInch(Value: Integer);
begin
  PixelsPerInch := Value;
end;  // TfrmSpecimenFinder.Set_PixelsPerInch 

{-------------------------------------------------------------------------------
}
procedure TfrmSpecimenFinder.Set_PrintScale(Value: TxPrintScale);
begin
  PrintScale := TPrintScale(Value);
end;  // TfrmSpecimenFinder.Set_PrintScale 

{-------------------------------------------------------------------------------
}
procedure TfrmSpecimenFinder.Set_Scaled(Value: WordBool);
begin
  Scaled := Value;
end;  // TfrmSpecimenFinder.Set_Scaled 

{-------------------------------------------------------------------------------
}
procedure TfrmSpecimenFinder.Set_ScreenSnap(Value: WordBool);
begin
  ScreenSnap := Value;
end;  // TfrmSpecimenFinder.Set_ScreenSnap 

{-------------------------------------------------------------------------------
}
procedure TfrmSpecimenFinder.Set_SnapBuffer(Value: Integer);
begin
  SnapBuffer := Value;
end;  // TfrmSpecimenFinder.Set_SnapBuffer 

{-------------------------------------------------------------------------------
}
procedure TfrmSpecimenFinder.Set_Visible(Value: WordBool);
begin
  Visible := Value;
end;  // TfrmSpecimenFinder.Set_Visible 

{-------------------------------------------------------------------------------
}
procedure TfrmSpecimenFinder.TreeDrawCell(Sender: TObject; aCanvas:
    TCanvas; ACol, ARow: Integer; Rect: TRect; State: TExGridDrawState);
var
  lRect: TRect;
  lNode: TFlyNode;
  tree: TRapidTree;
begin
  tree := Sender as TRapidTree;
  if (ACol = 0) and (ARow > 0) then begin
    lNode := tree.GetNodeAtRow(ARow);
    if Assigned(lNode) then begin
      lRect := Rect;
      // Move rect over to leave the existing lines and images.
      // 2 Added so that TRapidTree icon/text spacing is the same as that for
      //TListBox and TMenuItems
      lRect.Left := lRect.Left + tree.Indent*(lNode.Level + 1) + 2;
      aCanvas.FillRect(lRect); // blank out the area to draw onto

      // Vertically centre the text
      lRect.Top := lRect.Top + (tree.DefaultRowHeight - aCanvas.TextHeight('A')) div 2;
      // Output the text.  Use the term text output handler since it manages italics
      dmInterface.DrawTerm(aCanvas, lRect, lNode.Text, gdSelected in State);
    end; // if
  end;
end;  // TfrmSpecimenFinder.TreeDrawCell 

{-------------------------------------------------------------------------------
}
procedure TfrmSpecimenFinder.TreeGetEditText(Sender: TObject; ACol, ARow:
    Integer; var Value: String);
begin
  if (ARow = 0) then begin
    case ACol of
      COL_ITEM    : Value := ResStr_Specimens;
      COL_LOC_CODE: Value := ResStr_CurrentLocationCode
    end; // case
  end;
end;  // TfrmSpecimenFinder.TreeGetEditText

{-------------------------------------------------------------------------------
}
procedure TfrmSpecimenFinder.WndProc(var Message: TMessage);
var
  msg: TMsg;
begin
  case Message.Msg of
    // TActiveXControl.TranslateAccelerator shall send navigation keys
    WM_GETDLGCODE:
      Message.Result := DLGC_WANTArrows + DLGC_Wanttab;
    // dispatch navigation keys to currently active child control
  else
    if (Message.Msg < WM_KEYFIRST) or (Message.Msg > WM_KEYLAST) then
      inherited
    else
    begin
      // This code used to execute only on WM_KEYDOWN or WM_SYSKEYDOWN, for
      // the arrow, tab, home and end keys. However, due to an apparent bug
      // in Delphi's TActiveXControl.TranslateAccelerator, the handling for
      // special keys is also applied to WM_CHAR messages, for which WParam
      // is a character, not a virtual key code; so it was treating, for
      // example, character 39 = $27 = '''' as VK_RIGHT = $27. The upshot of
      // this is that we need to dispatch all keyboard messages to the active
      // control so that the user can still type things like ' and ( into text
      // boxes.
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
    end;
  end;
end;  // TfrmSpecimenFinder.WndProc

{-------------------------------------------------------------------------------
}
procedure TfrmSpecimenFinder._Set_Font(var Value: IFontDisp);
begin
  SetOleFont(Font, Value);
end;  // TfrmSpecimenFinder._Set_Font

{-------------------------------------------------------------------------------
}
procedure TfrmSpecimenFinder.MoveItems(sourceTree, targetTree: TRapidTree; all: Boolean);
var
  lNode: TFlyNode;
  lCursor: TCursor;
  i: Integer;
begin
  UnselectTreeView(targetTree);
  sourceTree.Items.BeginUpdate;
  targetTree.Items.BeginUpdate;
  lCursor := HourglassCursor;
  try
    if all then
      for i := sourceTree.Items.Count - 1 downto 0 do begin
        lNode := sourceTree.Items[i];
        MoveNode(sourceTree, targetTree, lNode);
      end
    else begin
      lNode := sourceTree.Items.GetFirstSelectedNode;
      while Assigned(lNode) do begin
        MoveNode(sourceTree, targetTree, lNode);
        lNode := sourceTree.Items.GetNextSelectedNode(lNode);
      end;
    end;
  finally
    sourceTree.Items.EndUpdate;
    targetTree.Items.EndUpdate;
    DefaultCursor(lCursor);
  end;
  EnableButtons;
  ReApplySort;
end;  // TfrmSpecimenFinder.MoveItems

{-------------------------------------------------------------------------------
}
procedure TfrmSpecimenFinder.btnAddClick(Sender: TObject);
begin
  MoveItems(tvResults, tvScratchPad, False);
  PopulateResultCount(tvResults.Items.Count);
end;  // TfrmSpecimenFinder.btnAddClick

{-------------------------------------------------------------------------------
}
procedure TfrmSpecimenFinder.btnRemoveClick(Sender: TObject);
begin
  MoveItems(tvscratchPad, tvResults, False);
  PopulateResultCount(tvResults.Items.Count);
end;  // TfrmSpecimenFinder.btnRemoveClick

{-------------------------------------------------------------------------------
}
procedure TfrmSpecimenFinder.btnAddAllClick(Sender: TObject);
begin
  MoveItems(tvResults, tvScratchPad, True);
  PopulateResultCount(0);
end;  // TfrmSpecimenFinder.btnAddAllClick

{-------------------------------------------------------------------------------
}
procedure TfrmSpecimenFinder.btnRemoveAllClick(Sender: TObject);
begin
  MoveItems(tvScratchPad, tvResults, True);
  PopulateResultCount(tvResults.Items.Count);
end;  // TfrmSpecimenFinder.btnRemoveAllClick

{-------------------------------------------------------------------------------
}
procedure TfrmSpecimenFinder.tvResultsSelectCell(Sender: TObject; ACol,
  ARow: Integer; var CanSelect: Boolean);
begin
  FCurrentTreeView := tvResults;
  btnAdd.Enabled := Assigned(tvResults.Selected);
end;  // TfrmSpecimenFinder.tvResultsSelectCell

{-------------------------------------------------------------------------------
}
procedure TfrmSpecimenFinder.tvScratchpadSelectCell(Sender: TObject; ACol,
  ARow: Integer; var CanSelect: Boolean);
begin
  FCurrentTreeView := tvScratchpad;
  btnRemove.Enabled := Assigned(tvScratchpad.Selected);
end;  // TfrmSpecimenFinder.tvScratchpadSelectCell

{-------------------------------------------------------------------------------
}
procedure TfrmSpecimenFinder.btnActionScratchpadClick(Sender: TObject);
var
  lPosPopup: TPoint;
begin
  FCurrentTreeView := tvScratchpad;
  // Work out where to show the popupmenu so that it appears just under the button
  lPosPopup := btnActionScratchpad.ClientToScreen(Point(0, btnActionScratchpad.Height));
  // For menu to appear at calculated position
  pmAction.Popup(lPosPopup.X, lPosPopup.Y);
end;  // TfrmSpecimenFinder.btnActionScratchpadClick
              
{-------------------------------------------------------------------------------
}
procedure TfrmSpecimenFinder.btnSelectResultsClick(Sender: TObject);
var
  lPosPopup: TPoint;
begin        
  FCurrentTreeView := tvResults;
  // Work out where to show the popupmenu so that it appears just under the button
  lPosPopup := btnSelectResults.ClientToScreen(Point(0, btnSelectResults.Height));
  // For menu to appear at calculated position
  pmSelect.Popup(lPosPopup.X, lPosPopup.Y);
end;  // TfrmSpecimenFinder.btnSelectResultsClick

{-------------------------------------------------------------------------------
}
procedure TfrmSpecimenFinder.btnSelectScratchpadClick(Sender: TObject);
var
  lPosPopup: TPoint;
begin
  FCurrentTreeView := tvScratchpad;
  // Work out where to show the popupmenu so that it appears just under the button
  lPosPopup := btnSelectScratchpad.ClientToScreen(Point(0, btnSelectScratchpad.Height));
  // For menu to appear at calculated position
  pmSelect.Popup(lPosPopup.X, lPosPopup.Y);
end;  // TfrmSpecimenFinder.btnSelectScratchpadClick

{-------------------------------------------------------------------------------
}
procedure TfrmSpecimenFinder.btnClearScratchpadClick(Sender: TObject);
begin
  if tvScratchpad.Items.Count > 0 then
    if MessageDlg(ResStr_ConfirmScratchpadClear, mtConfirmation, [mbYes, mbNo], 0) = mrYes then
      tvScratchpad.Items.Clear;
  EnableButtons;
end;  // TfrmSpecimenFinder.btnClearScratchpadClick

{-------------------------------------------------------------------------------
}
procedure TfrmSpecimenFinder.cmbSortByChange(Sender: TObject);
begin
  ApplySort(cmbSortBy.ItemIndex);
end;

procedure TfrmSpecimenFinder.ReApplySort();
begin
  ApplySort(FSortIndex);
end;

procedure TfrmSpecimenFinder.ApplySort(ASortIndex: Integer);

  procedure Sort(Tree: TRapidTree);
  var
    i, j: Integer;
    lSortingList: TStringList;
    lNode, lNewNode: TFlyNode;
    lNodes: TFlyNodes;
  begin
    lSortingList := TStringList.Create;
    lNodes := TFlyNodes.Create(nil);
    // Create string list associating the attribute we want to sort by with its node
    for i := 0 to Tree.Items.Count - 1 do begin
      lSortingList.AddObject(
          Tree.Items[i].Cells[FSortIndex] + ' ' + Tree.Items[i].Cells[0],
          Tree.Items[i]);
    end;
    // Sorts alphabetically by the attribute, now has nodes in the right order
    lSortingList.Sort;
    for i := 0 to lSortingList.Count - 1 do begin
      // Reassign node properties that were lost when casting to object and back
      lNode := TFlyNode(lSortingList.Objects[i]);
      lNewNode := lNodes.AddChild(nil, lNode.Caption);
      lNewNode.ImageIndex    := IMG_SPECIMEN;
      lNewNode.SelectedIndex := IMG_SPECIMEN;
      for j := 0 to Tree.ColCount - 1 do
        lNewNode.Cells[j] := lNode.Cells[j];
    end;
    // Refresh the tree to have the nodes in the correct order
    Tree.Items.Clear;
    Tree.Items.Assign(lNodes);
    lNodes.Free;
    lSortingList.Free;
  end;  // Sort

begin
  FSortIndex := ASortIndex;
  Sort(tvResults);
  Sort(tvScratchpad);
end;  // TfrmSpecimenFinder.cmbSortByChange
  
{-------------------------------------------------------------------------------
}
procedure TfrmSpecimenFinder.btnRefreshClick(Sender: TObject);
var
  rs: _Recordset;
  cursor: TCursor;
begin
  if not FResultsGridChanged or
     (MessageDlg(ResStr_ConfirmRefresh, mtConfirmation, [mbYes, mbNo], 0) = mrYes) then
  begin
    rs := fraSpecimenFinderDragFrame.GetResults;
    if Assigned(rs) then begin
      cursor := HourglassCursor;
      try
        PopulateResults(rs);
        FResultsGridChanged := False;
        ReApplySort;
      finally
        DefaultCursor(cursor);
      end;
    end;
  end;
  EnableButtons;
end;  // TfrmSpecimenFinder.btnRefreshClick
  
{-------------------------------------------------------------------------------
}
procedure TfrmSpecimenFinder.TreeDblClick(Sender: TObject);
begin
  // Double clicking the grid calls this method twice for some reason, and
  // we only want one Collections Browser window to open
  if FDoubleClicked then
    FDoubleClicked := False
  else begin
    mnuActionSendToCollectionBrowserClick(Sender);
    FDoubleClicked := True;
  end;
end;  // TfrmSpecimenFinder.TreeDblClick

{-------------------------------------------------------------------------------
  Initialise controls that specimens can be dragged from or dropped onto.
}
procedure TfrmSpecimenFinder.WMSetDragDrop(var Message: TMessage);
begin
  FDragDropControlAssistor.RegisterDropComponent(tvResults, DropResults, nil,
              [TN_SPECIMEN_UNIT], [CF_JNCCDATA], nil);
  FDragDropControlAssistor.RegisterDropComponent(tvScratchpad, DropScratchpad, nil,
              [TN_SPECIMEN_UNIT], [CF_JNCCDATA], nil);
  FDragDropControlAssistor.RegisterDragComponent(tvResults, DragResults, nil);
  FDragDropControlAssistor.RegisterDragComponent(tvScratchpad, DragScratchpad, nil);
end;  // TfrmSpecimenFinder.WMSetDragDrop

{-------------------------------------------------------------------------------
}
procedure TfrmSpecimenFinder.DropSpecimens(ATreeView: TRapidTree; AKey: TKeyString);
var
  lRecordset: _Recordset;
begin
  UnselectTreeView(ATreeView);
  lRecordset := dmGeneral.GetRecordset('usp_Specimen_GetFinderData',
                      ['@CollectionUnitKey', AKey,
                       '@ShowCommonNames', AppSettings.DisplayCommonNames,
                       '@UserDomainMask', IntToStr(AppSettings.DomainMask),
                       '@SessionID', AppSettings.SessionID]);
  AddNodes(ATreeView, lRecordset, True);
  EnableButtons;    
  FResultsGridChanged := ATreeView = tvResults;
end;  // TfrmSpecimenFinder.DropSpecimens

{-------------------------------------------------------------------------------
}
procedure TfrmSpecimenFinder.DropItems(targetTree: TRapidTree; sourceData: TKeyList);
var
  i: Integer;
  node: TFlyNode;
begin
  if sourceData.Header.ItemCount > 0 then begin
    // If we're dragging between the two grids on this form, we want to remove
    // the dragged items from the grid they came from
    if sourceData.Items[0].KeyField2 = FCurrentTreeView.Name then
    begin
      node := FCurrentTreeView.Items.GetFirstSelectedNode;
      while Assigned(node) do begin
        FCurrentTreeView.Items.Delete(node);
        node := FCurrentTreeView.Items.GetNextSelectedNode(node);
      end;
    end;

    for i := 0 to sourceData.Header.ItemCount - 1 do
      if SameText(sourceData.ItemTable[i], TN_SPECIMEN_UNIT) then
        DropSpecimens(targetTree, sourceData.Items[i].KeyField1);
  end;
  ReApplySort;
end;  // TfrmSpecimenFinder.DropItems

{-------------------------------------------------------------------------------
}
procedure TfrmSpecimenFinder.DropResults(const Sender: TObject; const iFormat: integer;
    const iSourceData: TKeyList; const iTextStrings: TStringList; var ioHandled: boolean);
begin
  DropItems(tvResults, iSourceData);
end;  // TfrmSpecimenFinder.DropResults

{-------------------------------------------------------------------------------
}
procedure TfrmSpecimenFinder.DropScratchpad(const Sender: TObject; const iFormat: integer;
    const iSourceData: TKeyList; const iTextStrings: TStringList; var ioHandled: boolean);
begin
  DropItems(tvScratchpad, iSourceData);
end;  // TfrmSpecimenFinder.DropScratchpad
     
{-------------------------------------------------------------------------------
}
procedure TfrmSpecimenFinder.DragResults(const Sender: TObject; var oDropSource: TJNCCDropSource);
begin
  if Assigned(tvResults.Selected) then begin
    FCurrentTreeView := tvResults;
    oDropSource.AssignKeyList(ConvertKeyList(Get_SelectedKeyList(TN_SPECIMEN_UNIT)));
  end;
end;  // TfrmSpecimenFinder.DragResults
    
{-------------------------------------------------------------------------------
}
procedure TfrmSpecimenFinder.DragScratchpad(const Sender: TObject; var oDropSource: TJNCCDropSource);
begin
  if Assigned(tvScratchpad.Selected) then begin
    FCurrentTreeView := tvScratchpad;
    oDropSource.AssignKeyList(ConvertKeyList(Get_SelectedKeyList(TN_SPECIMEN_UNIT)));
  end;
end;  // TfrmSpecimenFinder.DragScratchpad

{------------------------------------------------------------------------------- 
  Get_KeyList and Get_SelectedKeyList both return an IKeyList, but sometimes
  we want a TKeyList instead
}
function TfrmSpecimenFinder.ConvertKeyList(AKeyList: IKeyList): TKeyList;
var
  lKeyList: TEditableKeyList;
  i: Integer;
begin
  lKeyList := TEditableKeyList.Create;
  for i := 0 to AKeyList.ItemCount - 1 do
    lKeyList.AddItem(AKeyList.GetKeyItem(i).KeyField1, AKeyList.GetKeyItem(i).KeyField2);
  lKeyList.SetTable(AKeyList.TableName);
  Result := lKeyList;
end;  // TfrmSpecimenFinder.ConvertKeyList

{-------------------------------------------------------------------------------
  This gets the keys of only the selected items in the grid
}
function TfrmSpecimenFinder.Get_SelectedKeyList(const ADestinationTable: string):
    IKeyList;
var
  lKeyList: TEditableKeyList;
  lComKeyList: TCOMKeyList;
  lNode: TFlyNode;
begin
  lNode := FCurrentTreeView.Items.GetFirstSelectedNode;
  lKeyList := TEditableKeyList.Create;
  lKeyList.SetTable(ADestinationTable); 
  if ADestinationTable = TN_SPECIMEN_UNIT then begin
    while Assigned(lNode) do begin
      lKeyList.AddItem(lNode.Cells[COL_ITEM_KEY], FCurrentTreeView.Name);
      lNode := FCurrentTreeView.Items.GetNextSelectedNode(lNode);
    end;
  end else if ADestinationTable = TN_SAMPLE then
    PopulateKeyListWithSamples(lKeyList, True);
  lComKeyList := TCOMKeyList.Create(lKeyList);
  Result := lComKeyList as IKeyList;
end;  // TfrmSpecimenFinder.Get_SelectedKeyList

{-------------------------------------------------------------------------------
}
procedure TfrmSpecimenFinder.TreeKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  tree: TRapidTree;
  node: TFlyNode;
begin
  tree := Sender as TRapidTree;
  case Key of
    VK_DELETE:
      if Assigned(tree.Selected) then
        if MessageDlg(Format(ResStr_ConfirmRowDelete, [ResStr_Specimens]),
                      mtConfirmation, [mbYes, mbNo], 0) = mrYes then
        begin
          node := tree.Items.GetFirstSelectedNode;
          while Assigned(node) do begin
            tree.Items.Delete(node);
            // First selected is now gone. Get the "new" first selected and carry on.
            node := tree.items.GetFirstSelectedNode;
          end;
        end;
    end;
end;  // TfrmSpecimenFinder.TreeKeyDown

{-------------------------------------------------------------------------------
}
procedure TfrmSpecimenFinder.TreeMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  lNode, lLastSelectedNode: TFlyNode;
  i, j, k: Integer;
begin
  FCurrentTreeView := Sender as TRapidTree;
  lLastSelectedNode := FCurrentTreeView.Selected;
  if (Button=TMouseButton(mbRight)) then begin
    lNode := FCurrentTreeView.GetNodeAt(X, Y);
    if Assigned(lNode) then begin
      if ssShift in Shift then begin
        FCurrentTreeView.FlushSelection;
        i := lNode.GetRow;
        j := lLastSelectedNode.GetRow;
        if j < i then begin  
          j := lNode.GetRow;
          i := lLastSelectedNode.GetRow;
        end;
        for k := i to j do
          FCurrentTreeView.Items.Item[k-1].Selected := True;
      end else if ssCtrl in Shift then begin
        lNode.Selected := True;
      end else
        FCurrentTreeView.Selected := lNode;
    end;
  end;
end;  // TfrmSpecimenFinder.TreeMouseDown

{-------------------------------------------------------------------------------
  Gives all items without a preferred number a new preferred number.
  Currently hardcoded to be a registration number.
}
procedure TfrmSpecimenFinder.mnuActionAutoNumberClick(Sender: TObject);
var
  i: Integer;
  lNode: TFlyNode;
  AutoNumberForm: TdlgBatchAutoNumber;

  // Checks if a macro currently exists to generate a new number
  function MacroExists: Boolean;
  var
    lRS: _Recordset;
    lExists: Boolean;
  begin
    lExists := False;
    lRS := dmGeneral.GetRecordset('usp_Macros_Select', []);
    if lRS.RecordCount > 0 then begin
      lRS.MoveFirst;
      while not lRS.EOF do begin
        if lRS.Fields['Number_Type'].Value = 'Registration' then
          if VarToStr(lRS.Fields['Macro'].Value) <> '' then begin
            lExists := True;
            break;
          end;
        lRS.MoveNext;
      end;
    end;
    Result := lExists;
  end;  // MacroExists

  {=============================================================================
  Function Name: GetNewRegNumber
        Purpose: Generates a new number for the specimen and sets it to the
                 specified status.
     Parameters: AKey: Key of the item for which a number should be generated.
                 Status: The desired status of the item being assigned a number.
                 "Previous" = 0; "Preferred" = 1; "New" = 2.
  -----------------------------------------------------------------------------}
  function GetNewRegNumber(const AKey: String;
      const Status: TNumberStatus): String;
  var
    lNumber, lDepartment, lNumberKey: String;
  begin
    lDepartment := dmGeneral.GetStoredProcOutputParam(
        'usp_CollectionUnitDepartment_Get', ['@Key', AKey], '@Department');

    // Hard-coding registration number, this may change in the future.
    lNumber := dmGeneral.GetStoredProcOutputParam('usp_Macro_Get',
        ['@NumberType', 'Registration', '@Dept', lDepartment], '@Macro');

    lNumberKey := dmGeneral.GetStoredProcOutputParam(
        'usp_CollectionUnitNumber_Insert', ['@CollectionUnitKey', AKey,
        '@Number', lNumber, '@TypeConceptKey', REGISTRATION_NUMBER,
        '@Preferred', Ord(Status), '@Notes', '', '@SessionID',
        dmGeneral.Recorder.CurrentSettings.SessionID], '@Key');
    Result := lNumber;
  end;  // GetNewRegNumber

begin

  if MacroExists then
  begin
    AutoNumberForm := TdlgBatchAutoNumber.Create(nil);

    lNode := FCurrentTreeView.Items.GetFirstSelectedNode;
    if Assigned(lNode) then
    begin
      if AutoNumberForm.ShowModal = mrOK then
      begin
        while Assigned(lNode) do
        begin
          lNode.Cells[COL_PREF_NUMBER] :=
              GetNewRegNumber(lNode.Cells[COL_ITEM_KEY],
              AutoNumberForm.SelectedNumberStatus);
          lNode.Caption :=
              lNode.Cells[COL_ITEM_NAME] + ' - ' + lNode.Cells[COL_PREF_NUMBER];
          lNode := FCurrentTreeView.Items.GetNextSelectedNode(lNode);
        end;
      end;
    end else
    begin
      if MessageDlg(ResStr_NothingSelected, mtConfirmation,
          [mbYes, mbNo], 0) = mrYes then
      begin
        if AutoNumberForm.ShowModal = mrOK then
        begin
          for i := 0 to FCurrentTreeView.Items.Count - 1 do
            with FCurrentTreeView.Items[i] do
            begin
              Cells[COL_PREF_NUMBER] := GetNewRegNumber(Cells[COL_ITEM_KEY],
                  AutoNumberForm.SelectedNumberStatus);
              Caption := Cells[COL_ITEM_NAME] + ' - ' + Cells[COL_PREF_NUMBER];
            end;
        end;
      end;
      tvResults.Invalidate;
      tvScratchpad.Invalidate;
    end;

    AutoNumberForm.Free;
  end else
    MessageDlg(ResStr_NoMacro, mtWarning, [mbOK], 0);
end;  // TfrmSpecimenFinder.mnuActionAutoNumberClick

{-------------------------------------------------------------------------------
  Saves a SQL statement that selects the preferred number and item name of
  all items in the grid
}
procedure TfrmSpecimenFinder.mnuActionSaveSQLFileClick(Sender: TObject);
var
  lFile: TextFile;
  lKeys, lShowCommonNames: String;
  lKeyList: IKeyList;
  i: Integer;
begin
  if dlgActionSaveSQLFile.Execute then begin
    if FileExists(dlgActionSaveSQLFile.FileName) then
      if MessageDlg(MST_FILE_EXISTS, mtConfirmation, [mbYes, mbNo], 0) = mrNo then
      begin
        ModalResult := mrNone;
        Exit; // from procedure without doing anything
      end;
    lShowCommonNames := '0';
    if AppSettings.DisplayCommonNames then
      lShowCommonNames := '1';
    lKeys := '';
    lKeyList := Get_KeyList(TN_SPECIMEN_UNIT);
    // Convert the keylist into a comma separated string of SQL strings
    for i := 0 to lKeyList.ItemCount - 1 do
      if i = 0 then
        lKeys := '''' + lKeyList.GetKeyItem(i).KeyField1 + ''''
      else
        lKeys := lKeys + ', ''' + lKeyList.GetKeyItem(i).KeyField1 + '''';
    // Save the file
    AssignFile(lFile, dlgActionSaveSQLFile.FileName);
    Rewrite(lFile);
    WriteLn(lFile, 'SELECT dbo.ufn_GetPrefNumber(SpecUnit.Collection_Unit_Key) as PrefNumber,');
    WriteLn(lFile, 'CASE WHEN SpecUnit.Life_Sciences = 0 THEN TP.Item_Name');
    WriteLn(lFile, 'ELSE dbo.ufn_GetFormattedTaxonNameByParams(ITN.Actual_Name, ' +
        'ITN.Actual_Name_Italic, ITN.Common_Name, ITN.Common_Name_Italic, ITN.Authority, ' +
        lShowCommonNames + ') END AS Item_Name');
    WriteLn(lFile, 'FROM SPECIMEN_UNIT SpecUnit');
    WriteLn(lFile, 'LEFT JOIN DETERMINATION D ON SpecUnit.Preferred_Determination_Key = D.Determination_Key');
    WriteLn(lFile, 'LEFT JOIN Concept C ON D.Concept_Key = C.Concept_Key');
    WriteLn(lFile, 'LEFT JOIN Concept CP ON CP.Meaning_Key = C.Meaning_Key AND CP.Concept_Group_Key = C.Concept_Group_Key AND CP.List_Preferred = 1');
    WriteLn(lFile, 'LEFT JOIN Term TP ON CP.Term_Key = TP.Term_Key');
    WriteLn(lFile, 'LEFT JOIN TAXON_DETERMINATION TD ON SpecUnit.Preferred_Taxon_Determination_Key = TD.Taxon_Determination_Key');
    WriteLn(lFile, 'LEFT JOIN INDEX_TAXON_NAME ITN ON TD.Taxon_List_Item_Key = ITN.Taxon_List_Item_Key');
    WriteLn(lFile, 'WHERE SpecUnit.Collection_Unit_Key in (' + lKeys + ')');
    CloseFile(lFile);
  end;
end;  // TfrmSpecimenFinder.mnuActionSaveSQLFileClick

{-------------------------------------------------------------------------------
}
procedure TfrmSpecimenFinder.ReportMenuItemClick(Sender: TObject);
var
  lNode: TFlyNode;
begin
  if Sender is TMenuItem then begin
    lNode := FCurrentTreeview.Items.GetFirstSelectedNode;
    if Assigned(lNode) then
      FReportList.Report[TMenuItem(Sender).Tag].Execute(Get_SelectedKeyList(TN_SPECIMEN_UNIT))
    else
    if MessageDlg(ResStr_NothingSelected, mtConfirmation, [mbYes, mbNo], 0) = mrYes then
      FReportList.Report[TMenuItem(Sender).Tag].Execute(Get_KeyList(TN_SPECIMEN_UNIT));
  end;
end;  // TfrmSpecimenFinder.ReportMenuItemClick

{-------------------------------------------------------------------------------
  Put all the reports that take Specimen Unti Keys as parameters on the Actions menu
}
procedure TfrmSpecimenFinder.UpdateQuickReportMenu;
var
  i: Integer;
  lMenu, lParentItem: TMenuItem;

  // Create the correct submenu structure for the report
  // Returns the menu item at the lowest level
  function CreateFolders(APath: String; AParent: TMenuItem): TMenuItem;
  var
    lPathName, lRemainingPath: String;
    j: Integer;  
    lFolderExists: Boolean;
  begin
    lFolderExists := False;
    // Split path on first \
    lPathName := LeftStr(APath, Pos('\', APath)-1);
    if lPathName = '' then
      lPathName := APath;
    lRemainingPath := Copy(APath, Length(lPathName)+2, Length(APath));

    // Add menu item with folder icon for the left string, if it doesn't already exist
    for j := 0 to AParent.Count - 1 do
      if AParent.Items[j].Caption = lPathName then begin
        lFolderExists := true;
        lMenu := AParent.Items[j];
        break;
      end;
    if not lFolderExists then begin
      lMenu := TMenuItem.Create(AParent);
      lMenu.Caption := lPathName;
      lMenu.ImageIndex := 8;
      AParent.Add(lMenu);
    end;

    // If the right string isn't empty, recurse until it is
    if lRemainingPath <> '' then
      Result := CreateFolders(lRemainingPath, lMenu)
    else
      Result := lMenu;
  end;

begin
  mnuActionQuickReport.Clear;

  for i := 0 to FReportList.ReportCount - 1 do begin
    // If there's no report path, the report goes immediately under "Quick Reports"
    if FReportList.Report[i].ReportPath = '' then begin
      lMenu := TMenuItem.Create(mnuActionQuickReport);
      lMenu.Caption    := FReportList.Report[i].ReportTitle;
      lMenu.ImageIndex := 9;
      lMenu.Tag        := i;
      lMenu.OnClick    := ReportMenuItemClick;
      mnuActionQuickReport.Add(lMenu);
    // Otherwise create the correct folder structure and put the report there
    end else begin
      lParentItem := CreateFolders(FReportList.Report[i].ReportPath, mnuActionQuickReport);

      lMenu := TMenuItem.Create(lParentItem);
      lMenu.Caption    := FReportList.Report[i].ReportTitle;
      lMenu.ImageIndex := 9;
      lMenu.Tag        := i;
      lMenu.OnClick    := ReportMenuItemClick;
      lParentItem.Add(lMenu);
    end;
  end;
end;  // TfrmSpecimenFinder.UpdateQuickReportMenu

{-------------------------------------------------------------------------------
  Read the XML files for the custom actions, and add them to the actions menu
}
procedure TfrmSpecimenFinder.GetAutoGeneratedActions;
var
  lSr: TSearchRec;
  lMenu: TMenuItem;
  lXMLDoc: IXMLDocument;
  lNode: IXMLNode;
  i: Integer;
  path: String;
begin
  i := 0;
  path := ExtractFilePath(Application.ExeName) + ACTIONS_PATH;
  if FindFirst(path + '*.xml', faAnyFile, lSr) = 0 then begin
    repeat
      lXMLDoc := TXMLDocument.Create(path + lSr.Name);
      lNode := lXMLDoc.DocumentElement;
      lMenu := TMenuItem.Create(pmAction);
      lMenu.Caption := lNode.Attributes['caption'];
      lMenu.Tag := i;
      lMenu.OnClick := AutoActionClick;
      pmAction.Items.Insert(4 + i, lMenu);
      FActionsSQL.Add(lNode.Text);
      i := i + 1;
    until FindNext(lSr) <> 0;
    FindClose(lSr);
  end;
end;  // TfrmSpecimenFinder.GetAutoGeneratedActions

{-------------------------------------------------------------------------------
  Perform a custom action on the selected specimens
}
procedure TfrmSpecimenFinder.AutoActionClick(Sender: TObject);
var
  lNode: TFlyNode;
  lKeyList: IKeyList;
  lKeys: String;
  i: Integer;
  lCursor: TCursor;
begin
  if Sender is TMenuItem then begin
    lNode := FCurrentTreeview.Items.GetFirstSelectedNode;
    if Assigned(lNode) then
      lKeyList := Get_SelectedKeyList(TN_SPECIMEN_UNIT)
    else
    if MessageDlg(ResStr_NothingSelected, mtConfirmation, [mbYes, mbNo], 0) = mrYes then
      lKeyList := Get_KeyList(TN_SPECIMEN_UNIT);

    if Assigned(lKeyList) then begin
      lKeys := lKeyList.GetKeyItem(0).KeyField1;
      for i := 1 to lKeyList.ItemCount - 1 do
        lKeys := lKeys + ''', ''' + lKeyList.GetKeyItem(i).KeyField1;

      lCursor := HourglassCursor;
      try
        dmGeneral.ExecuteSQL(Format(FActionsSQL[TMenuItem(Sender).Tag], [lKeys]));
      finally
        DefaultCursor(lCursor);
      end;
    end;
  end;
end;  // TfrmSpecimenFinder.AutoActionClick

{-------------------------------------------------------------------------------
  read the XMl files for the custom select statements and add them to the Select menu
}
procedure TfrmSpecimenFinder.GetAutoGeneratedSelectors;
var
  lSr: TSearchRec;
  lMenu: TMenuItem;
  lXMLDoc: IXMLDocument;
  lNode: IXMLNode;
  i: Integer;
  path: String;
begin
  i := 0;
  path := ExtractFilePath(Application.ExeName) + SELECTORS_PATH;
  if FindFirst(path + '*.xml', faAnyFile, lSr) = 0 then begin
    repeat
      lXMLDoc := TXMLDocument.Create(path + lSr.Name);
      lNode := lXMLDoc.DocumentElement;
      lMenu := TMenuItem.Create(pmAction);
      lMenu.Caption := lNode.Attributes['caption'];
      lMenu.Tag := i;
      lMenu.OnClick := AutoSelectorClick;
      pmSelect.Items.Add(lMenu);
      FSelectSQL.Add(lNode.Text);
      i := i + 1;
    until FindNext(lSr) <> 0;
    FindClose(lSr);
  end else begin
    // No custom selectors means no items in the select menu, so disable the select buttons
    btnSelectResults.Enabled := False;
    btnSelectScratchpad.Enabled := False;
  end;
end;  // TfrmSpecimenFinder.GetAutoGeneratedSelectors

{-------------------------------------------------------------------------------
  Flter the currently selected specimens down to those returned by the custom selector
}
procedure TfrmSpecimenFinder.AutoSelectorClick(Sender: TObject);
var
  lNode: TFlyNode;
  lKeyList: IKeyList;
  lKeys: String;
  i: Integer;
  lRS: _Recordset;
begin
  if Sender is TMenuItem then begin
    lNode := FCurrentTreeview.Items.GetFirstSelectedNode;
    if Assigned(lNode) then
      lKeyList := Get_SelectedKeyList(TN_SPECIMEN_UNIT)
    else
    if MessageDlg(ResStr_NothingSelected, mtConfirmation, [mbYes, mbNo], 0) = mrYes then
      lKeyList := Get_KeyList(TN_SPECIMEN_UNIT);

    if Assigned(lKeyList) then begin
      lKeys := '''' + lKeyList.GetKeyItem(0).KeyField1;
      for i := 1 to lKeyList.ItemCount - 1 do
        lKeys := lKeys + ''', ''' + lKeyList.GetKeyItem(i).KeyField1;
      lKeys := lKeys + '''';

      lRS := dmGeneral.ExecuteSQL(Format(FSelectSQL[TMenuItem(Sender).Tag], [lKeys]), True);

      // Unselect all currently selected items, then reselect those the SQL statement returned
      UnselectTreeView(FCurrentTreeView);
      if lRS.RecordCount > 0 then begin
        lRS.MoveFirst;
        while not lRS.EOF do begin
          for i := 0 to FCurrentTreeView.Items.Count - 1 do begin
            if FCurrentTreeView.Items[i].Cells[COL_ITEM_KEY] = lRS.Fields[0].Value then begin
              FCurrentTreeView.Items[i].Selected := True;
            end;
          end;
          lRS.MoveNext;
        end;
      end;
    end;
  end;
  EnableButtons;
end;  // TfrmSpecimenFinder.AutoSelectorClick
                  
{-------------------------------------------------------------------------------
  Unselects all items in the grid
}
procedure TfrmSpecimenFinder.UnselectTreeView(ATreeView: TRapidTree);
var
  lNode: TFlyNode;
begin
  lNode := ATreeView.Items.GetFirstSelectedNode;
  while Assigned(lNode) do begin
    lNode.Selected := False;
    lNode := ATreeView.Items.GetNextSelectedNode(lNode);
  end;
end;  // TfrmSpecimenFinder.UnselectTreeView

{-------------------------------------------------------------------------------
  Update the enabled state of all the buttons depending on the current state of the
  two grids
}
procedure TfrmSpecimenFinder.EnableButtons;
var
  itemsInResults, itemsInScratchpad: Boolean;
begin
  itemsInResults    := tvResults.Items.Count > 0;
  itemsInScratchpad := tvScratchpad.Items.Count > 0;

  btnAddAll.Enabled        := itemsInResults;
  btnActionResults.Enabled := itemsInResults;
  btnSelectResults.Enabled := itemsInResults and (FSelectSQL.Count > 0);

  btnRemoveAll.Enabled        := itemsInScratchpad;
  btnActionScratchpad.Enabled := itemsInScratchpad;
  btnSelectScratchpad.Enabled := itemsInScratchpad and (FSelectSQL.Count > 0);

  btnAdd.Enabled    := Assigned(tvResults.Items.GetFirstSelectedNode());
  btnRemove.Enabled := Assigned(tvScratchpad.Items.GetFirstSelectedNode());
  
  EnableMenus;
end;  // TfrmSpecimenFinder.EnableButtons

{-------------------------------------------------------------------------------
  Update the enabled state of all the popup menus depending on the current state of the
  selected tree.
}
procedure TfrmSpecimenFinder.EnableMenus;
var
  i: Integer;
  itemsInTree: Boolean;
begin
  itemsInTree := FCurrentTreeView.Items.Count > 0;

  for i := 0 to pmAction.Items.Count - 1 do
    pmAction.Items.Items[i].Enabled := itemsInTree;
  for i := 0 to pmSelect.Items.Count - 1 do pmSelect.Items.Items[i].Enabled := itemsInTree;
  if mnuActionAutoNumber.Visible then begin
    mnuActionAutoNumber.Enabled := itemsInTree;
    for i := 0 to FActionsSQL.Count - 1 do pmAction.Items.Items[4 + i].Enabled := itemsInTree;
  end;
end;  // TfrmSpecimenFinder.EnableMenus

{-------------------------------------------------------------------------------
  Moves a specimen from one grid to the other
}
procedure TfrmSpecimenFinder.MoveNode(AFromTree, AToTree: TRapidTree; ANode: TFlyNode);
var
  lNewNode: TFlyNode;
  i: Integer;
begin
  lNewNode := AToTree.Items.AddChild(nil, ANode.Caption);
  lNewNode.ImageIndex    := IMG_SPECIMEN;
  lNewNode.SelectedIndex := IMG_SPECIMEN;
  for i := 0 to AFromTree.ColCount - 1 do
    lNewNode.Cells[i] := ANode.Cells[i];
  AFromTree.Items.Delete(ANode);
  lNewNode.Selected := True;
end;  // TfrmSpecimenFinder.MoveNode

{===============================================================================
Function Name: HasItemBeenAddedAlready
      Purpose: Determines whether the specimen unit item identified by the given
               key has already been added to either the results tree or the
               scratch pad tree.
-------------------------------------------------------------------------------}
function TfrmSpecimenFinder.HasItemBeenAddedAlready(const AKey: String): Boolean;
var
  i: Integer;
begin
  Result := false;

  if Assigned(tvResults) then
  begin
    for i := 0 to tvResults.Items.Count - 1 do
    begin
      if tvResults.Items.Item[i].Cells[COL_ITEM_KEY] = AKey then
      begin
        Result := true;
        break;
      end;
    end;
  end;

  if Assigned(tvScratchPad) and not Result then
  begin
    for i := 0 to tvScratchPad.Items.Count - 1 do
    begin
      if tvScratchPad.Items.Item[i].Cells[COL_ITEM_KEY] = AKey then
      begin
        Result := true;
        break;
      end;
    end;
  end;
end;

{-------------------------------------------------------------------------------
  Add the specimens in the recordset to the grid
}
procedure TfrmSpecimenFinder.AddNodes(ATreeView: TRapidTree; ARecordset: _Recordset;
    ASelected: Boolean = False);
var
  newNode: TFlyNode;
  nodeCaption: String;
begin
  if ARecordset.RecordCount > 0 then
  begin
    ARecordset.MoveFirst;
    while not ARecordset.Eof do
    begin
      if not HasItemBeenAddedAlready(ARecordset.Fields['Item_Key'].Value) then
      begin
        if VarIsNull(ARecordset.Fields['Item_Name'].Value) then
          nodeCaption := ResStr_NoDetermination
        else
          nodeCaption := ARecordset.Fields['Item_Name'].Value;

        newNode := ATreeView.Items.AddChild(nil, nodeCaption);
        newNode.ImageIndex    := IMG_SPECIMEN;
        newNode.SelectedIndex := IMG_SPECIMEN;
        // Hold unaltered caption, so it can be easily reused when the caption needs to be refreshed.
        newNode.Cells[COL_ITEM_NAME] := nodeCaption;
        // Store the key and sample key in invisible columns
        newNode.Cells[COL_ITEM_KEY] := ARecordset.Fields['Item_Key'].Value;

        if not VarIsNull(ARecordset.Fields['Domain_Name'].Value) then begin
          newNode.Cells[COL_DOMAIN_NAME] := ARecordset.Fields['Domain_Name'].Value;
          if not VarIsNull(ARecordset.Fields['List_Code'].Value) then
            newNode.Cells[COL_DOMAIN_NAME] :=
                newNode.Cells[COL_DOMAIN_NAME] + ' - ' + ARecordset.Fields['List_Code'].Value;
        end;

        if not VarIsNull(ARecordset.Fields['Current_Location_Code'].Value) then
          newNode.Cells[COL_LOC_CODE] := ARecordset.Fields['Current_Location_Code'].Value;

        if not VarIsNull(ARecordset.Fields['Number'].Value) and
           (ARecordset.Fields['Number'].Value <> '') then
        begin
          newNode.Caption := nodeCaption + ' - ' + ARecordset.Fields['Number'].Value;
          newNode.Cells[COL_PREF_NUMBER] := ARecordset.Fields['Number'].Value;
        end;

        newNode.Cells[COL_TYPE] := ARecordset.Fields['Specimen_Type'].Value;

        // Add specimen nodes to the common name fetch queue
        if (ARecordset.Fields['Life_Sciences'].Value = 0) and
           (not VarIsNull(ARecordset.Fields['Det_Item_Key'].Value)) then
          FCommonNameFetchQueue.Add(ARecordset.Fields['Det_Item_Key'].Value, newNode);

        newNode.Selected := ASelected;
      end;

      ARecordset.MoveNext;
    end;
  end;
end;  // TfrmSpecimenFinder.AddNodes

{-------------------------------------------------------------------------------
  Populates the operator drop down list with the relevant operators for the
  selected filter property.
}
procedure TfrmSpecimenFinder.cmbPropertyChange(Sender: TObject);
var
  lOperator: TLogicalOperator;
begin
  ResetFields;
  if cmbProperty.ItemIndex > 0 then
  begin
    for lOperator := Low(TLogicalOperator) to High(TLogicalOperator) do
    begin
      if lOperator in TBooleanProperty(cmbProperty.Items.Objects[cmbProperty.ItemIndex]).ApplicableOperators then
      begin
        AddOperatorToList(lOperator);
      end;
    end;

    if TBooleanProperty(cmbProperty.Items.Objects[cmbProperty.ItemIndex]).PropertyType = ptBoolean then
    begin
      rgBooleanValue.Visible := true;
      eOperand1.Visible := false;
    end;

    case TBooleanProperty(cmbProperty.Items.Objects[cmbProperty.ItemIndex]).PropertyEnum of
      bpeAppliesTo, bpeAnyNumber, bpeAccessionNumber, bpePreferredNumber,
      bpePreferredAccessionNumber, bpeAssembledBy:
      begin
        eOperand1.MaxLength := 30;
        eOperand2.MaxLength := 30;
      end;
    else
      begin
        eOperand1.MaxLength := 75;
        eOperand2.MaxLength := 75;
      end;
    end;
  end;
end;

{-------------------------------------------------------------------------------
  Resets input fields to initial values
}
procedure TfrmSpecimenFinder.ResetFields;

begin
  cmbOperator.Items.Clear;
  cmbOperator.Items.Add('[please select]');
  cmbOperator.ItemIndex := 0;


  rgBooleanValue.Visible := false;
  eOperand1.Visible := true;

  DisplayInputFields(false, false, true);
end;

{-------------------------------------------------------------------------------
  Add an operator enum-name pair to the operator drop-down list
}
procedure TfrmSpecimenFinder.AddOperatorToList(operator: TLogicalOperator);
var
  newOperator: TOperatorPair;
begin
  newOperator := TOperatorPair.Create(operator);
  cmbOperator.AddItem(newOperator.OperatorName, newOperator);
end;

{-------------------------------------------------------------------------------
   Displays the appropriate input fields according to which filter operator is
   selected
}
procedure TfrmSpecimenFinder.cmbOperatorChange(Sender: TObject);
begin
  if (cmbOperator.ItemIndex = 0) then
    begin
      DisplayInputFields(false, false, true);
    end
  else
  begin
    case TOperatorPair(cmbOperator.Items.Objects[cmbOperator.ItemIndex]).OperatorEnum of
    loEqualTo, loGreaterThan, loLessThan, loStartsWith, loContains, loNotEqualTo:
      begin
        DisplayInputFields(true, false, false);
      end;
    loBetween:
      begin
        DisplayInputFields(true, true, false);
      end;
    end;
  end;
  EnableAddCriterion;
end;

{-------------------------------------------------------------------------------
  Displays the appropriate criterion input fields
}
procedure TfrmSpecimenFinder.DisplayInputFields(
            enableOperand1: Boolean;
            displayOperand2: Boolean;
            clearText: Boolean);
begin
  eOperand1.Enabled := enableOperand1;
  eOperand2.Visible := displayOperand2;
  lblAnd.Visible := displayOperand2;
  if clearText then
  begin
    eOperand1.Text := '';
    eOperand2.Text := '';
  end;
end;

{-------------------------------------------------------------------------------
  Enables the add filter criterion button if necessary fields are completed
}
procedure TfrmSpecimenFinder.EnableAddCriterion;
begin
  if cmbOperator.ItemIndex = 0 then
    btnAddCriterion.Enabled := False
  else
  if not eOperand1.Visible then
    btnAddCriterion.Enabled := true
  else
    begin
      case TOperatorPair(cmbOperator.Items.Objects[cmbOperator.ItemIndex]).OperatorEnum of
      loEqualTo, loGreaterThan, loLessThan, loStartsWith, loContains, loNotEqualTo:
        begin
          if Length(eOperand1.Text) > 0 then
          begin
            btnAddCriterion.Enabled := True;
          end
          else
            btnAddCriterion.Enabled := False;
        end;
      loBetween:
        begin
          if (Length(eOperand1.Text) > 0) and (Length(eOperand2.Text) > 0) then
            btnAddCriterion.Enabled := True
          else
            btnAddCriterion.Enabled := False

        end;
      end;
    end
end;

{-------------------------------------------------------------------------------
  Checks if add criterion button should be enabled
}
procedure TfrmSpecimenFinder.eOperand1Change(Sender: TObject);
begin
  EnableAddCriterion;
end;

{-------------------------------------------------------------------------------
  Checks if add criterion button should be enabled
}
procedure TfrmSpecimenFinder.eOperand2Change(Sender: TObject);
begin
  EnableAddCriterion;
end;

{-------------------------------------------------------------------------------
  Adds a new filtering criterion, provided the fields entered are valid.
}
procedure TfrmSpecimenFinder.btnAddCriterionClick(Sender: TObject);
var
  lBooleanCriterion: TBooleanCriterion;
  lBooleanProperty: TBooleanProperty;
  lOperator: TLogicalOperator;
  lOperand1: string;
  lOperand2: string;
begin
  if IsCriterionValid then
  begin
    lBooleanProperty := TBooleanProperty(cmbProperty.Items.Objects[cmbProperty.ItemIndex]);
    lOperand1 := eOperand1.Text;
    lOperand2 := eOperand2.Text;
    lOperator := TOperatorPair(cmbOperator.Items.Objects[cmbOperator.ItemIndex]).OperatorEnum;
    lBooleanCriterion := TBooleanCriterion.Create(
                            lBooleanProperty,
                            lOperator,
                            lOperand1,
                            lOperand2,
                            rgBooleanValue.ItemIndex = 0);
    AddBooleanCriterion(lBooleanCriterion);
    cmbProperty.SetFocus;
  end;

end;

procedure TfrmSpecimenFinder.AddBooleanCriterion(BooleanCriterion: TBooleanCriterion);
var
  lListCaption: string;
  OperatorPair: TOperatorPair;
begin
  lListCaption := '%s %s';
  OperatorPair := TOperatorPair.Create(BooleanCriterion.Operator);
  if BooleanCriterion.PropertyType.IsMetadata then
    lListCaption := Format(lListCaption,
      [BooleanCriterion.PropertyType.MetadataName,
      OperatorPair.OperatorName])
  else
    lListCaption := Format(lListCaption,
      [BooleanCriterion.PropertyType.PropertyName,
      OperatorPair.OperatorName]);
  if BooleanCriterion.PropertyType.PropertyType = ptBoolean then
  begin
    if rgBooleanValue.ItemIndex = 0 then
      lListCaption := lListCaption + ' "yes"'
    else
      lListCaption := lListCaption + ' "no"';
  end
  else
  begin
    lListCaption := lListCaption + ' %s';
    if (OperatorPair.OperatorEnum = loBetween) then
      lListCaption := lListCaption + ' and %s';
    case BooleanCriterion.PropertyType.PropertyType of
      ptString, ptMeasurementValue:
        lListCaption := Format(lListCaption,
                          ['"' + BooleanCriterion.Operand1 + '"', '"' + BooleanCriterion.Operand2 + '"']);
      ptVagueDate:
        lListCaption := Format(lListCaption,
                          [BooleanCriterion.Operand1, BooleanCriterion.Operand2]);
    end;
  end;

  fraSpecimenFinderDragFrame.lbFinder.AddItem(lListCaption, BooleanCriterion);
  cmbProperty.ItemIndex := 0;
  ResetFields;

  fraSpecimenFinderDragFrame.RefreshResults := True;
end;

{-------------------------------------------------------------------------------
  Returns true or false depending on whether the fields entered for a new Boolean
  criterion are valid.
}
function TfrmSpecimenFinder.IsCriterionValid : Boolean;
begin
  Result := True;
  if cmbProperty.ItemIndex = 0 then
  begin
    ShowMessage('Please select a property to filter on.');
    Result := False;
  end
  else if cmbOperator.ItemIndex = 0 then
  begin
    ShowMessage('Please select a filtering operator.');
    Result := False;
  end
  else
  begin
    if TBooleanProperty(cmbProperty.Items.Objects[cmbProperty.ItemIndex]).PropertyType = ptVagueDate then
    begin
      if not IsVagueDate(eOperand1.Text) then
      begin
        MessageDlg(eOperand1.Text + 'is not a valid vague date.', mtInformation, [mbOK], 0);
        FocusControl(eOperand1);
        Result := False;
      end
      else if (TOperatorPair(cmbOperator.Items.Objects[cmbOperator.ItemIndex]).OperatorEnum = loBetween)
              and not IsVagueDate(eOperand2.Text) then
      begin
        ShowMessage(eOperand2.Text + ' is not a valid vague date.');
        FocusControl(eOperand2);
        Result := False;
      end;
    end
    else if not (TBooleanProperty(cmbProperty.Items.Objects[cmbProperty.ItemIndex]).PropertyType = ptBoolean) then
    begin
      if Length(eOperand1.Text) = 0 then
      begin
        ShowMessage(eOperand1.Text + 'is a required field.');
        FocusControl(eOperand1);
        Result := False;
      end
      else if (TOperatorPair(cmbOperator.Items.Objects[cmbOperator.ItemIndex]).OperatorEnum = loBetween)
          and (Length(eOperand2.Text) = 0) then
      begin
        ShowMessage(eOperand2.Text + 'is a required field.');
        FocusControl(eOperand2);
        Result := False;
      end;
    end;
  end;
end;

{-------------------------------------------------------------------------------
  Populates cmbProperty with a list of filtering properties as well as section
  headings
}
procedure TfrmSpecimenFinder.PopulateFilterPropertyList;
var
  lCategory: TBooleanPropertyCategory;
  lCategoryPair: TCategoryPair;
  lNewBooleanProperty : TBooleanProperty;
  lBoolProperty: TBooleanPropertyEnum;
  lMetadataProperty : TBooleanProperty;
  i: Integer;
begin
  cmbProperty.Items.Add('[please select]');
  cmbProperty.ItemIndex := 0;
  for lCategory := Low(TBooleanPropertyCategory) to High(TBooleanPropertyCategory) do
  begin
    lCategoryPair := TCategoryPair.Create(lCategory);
    cmbProperty.AddItem(lCategoryPair.CategoryName, lCategoryPair);
    for lBoolProperty := Low(TBooleanPropertyEnum) to High(TBooleanPropertyEnum) do
    begin
      lNewBooleanProperty := TBooleanProperty.Create(lBoolProperty);
      if lNewBooleanProperty.Category = lCategory then
      begin
        if (lBoolProperty = bpeSpecimenMetadata) or (lBoolProperty = bpeCollectionMetadata) then
        begin
          lNewBooleanProperty.HasMetadata := True;
          lNewBooleanProperty.SetMetadata;
          for i := 0 to lNewBooleanProperty.Metadata.Count - 1 do
          begin
            lMetadataProperty := TBooleanProperty.Create(lBoolProperty);
            lMetadataProperty.IsMetadata := true;
            lMetadataProperty.MetadataName := lNewBooleanProperty.Metadata[i];
            cmbProperty.AddItem(lMetadataProperty.MetadataName, lMetadataProperty);
          end;
        end
        else
          cmbProperty.AddItem(lNewBooleanProperty.PropertyName, lNewBooleanProperty);
      end;
    end;
  end;
end;

{-------------------------------------------------------------------------------
  Ensures sections headings in cmbProperty cannot be selected
}
procedure TfrmSpecimenFinder.cmbPropertySelect(Sender: TObject);
begin
  if cmbProperty.Items.Objects[cmbProperty.ItemIndex] is TCategoryPair then
    cmbProperty.ItemIndex := cmbProperty.ItemIndex + 1;

  cmbPropertyChange(Sender);
end;

{-------------------------------------------------------------------------------
   Skips the section headings when keying up through property list
}
procedure TfrmSpecimenFinder.cmbPropertyKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  if (Key = VK_UP) and (cmbProperty.ItemIndex > 1) then
  begin
    if cmbProperty.Items.Objects[cmbProperty.ItemIndex - 1] is TCategoryPair then
    begin
      Key := 0;
      cmbProperty.ItemIndex := cmbProperty.ItemIndex - 2;
    end;
  end;
  cmbPropertyChange(Sender);
end;

{-------------------------------------------------------------------------------
  Draws list of filtering properties
}
procedure TfrmSpecimenFinder.cmbPropertyDrawItem(Control: TWinControl;
  Index: Integer; Rect: TRect; State: TOwnerDrawState);
var
  lIndent: Integer;
  lImageIndex: Integer;
begin
  if Index > 0 then
  begin
    if cmbProperty.Items.Objects[Index] is TCategoryPair then begin
      lIndent := 0;
      case TCategoryPair(cmbProperty.Items.Objects[Index]).CategoryEnum of
      bpcCollections:
        lImageIndex := 2;//index for collections icon in ilBrowserNodes
      bpcSpecimens, bpcSpecMeasurements:
        lImageIndex := 3;//index for specimens icon in ilBrowserNodes
      else
        lImageIndex := -1;
      end;
      cmbProperty.Canvas.Font.Name  := 'Arial';  //MS Sans Serif font looks rubbish in italic
      cmbProperty.Canvas.Font.Style := [fsItalic];
      cmbProperty.Canvas.Font.Color := GetContrastColour(clGrayText);
      // Don't draw a highlight background for subject area as user cannot select it
      cmbProperty.Canvas.Brush.Color := clGrayText;
    end
    else
    begin
      lIndent := 5;
      lImageIndex := -1;
      cmbProperty.Canvas.Font.Style := [];
      if odSelected in State then
        cmbProperty.Canvas.Font.Color := clHighlightText
      else
        cmbProperty.Canvas.Font.Color := clWindowText;
    end;
    cmbProperty.Canvas.FillRect(Rect);
    cmbProperty.Canvas.TextOut(Rect.Left+lIndent + 18, Rect.Top + 1, cmbProperty.Items[Index]);
    dmInterface.ilBrowserNodes.Draw(cmbProperty.Canvas, Rect.Left + lIndent, Rect.Top, lImageIndex);
  end
  else
  begin
    lIndent := 0;
    lImageIndex := -1;
    cmbProperty.Canvas.Font.Style := [];
    if odSelected in State then
      cmbProperty.Canvas.Font.Color := clHighlightText
    else
      cmbProperty.Canvas.Font.Color := clWindowText;
     cmbProperty.Canvas.FillRect(Rect);
    cmbProperty.Canvas.TextOut(Rect.Left+lIndent, Rect.Top + 1, cmbProperty.Items[Index]);
    dmInterface.ilOtherNodes.Draw(cmbProperty.Canvas, Rect.Left + lIndent, Rect.Top, lImageIndex);
  end;

end;

{-------------------------------------------------------------------------------
  Displays a new specimen finder window with the results included as a search
  criterion.
}
procedure TfrmSpecimenFinder.pmActionSendToSpecimenFinderClick(
  Sender: TObject);
var
  lKeyList: IKeyList;
  lNode: TFlyNode;
begin
  lNode := FCurrentTreeView.Items.GetFirstSelectedNode;
  if Assigned(lNode) then begin
    lKeyList := Get_SelectedKeyList(TN_SPECIMEN_UNIT);
  end else
  if FCurrentTreeView.Items.Count = 0 then
      ShowInformation(ResStr_NoSpecimensToDisplay)
  else
  if MessageDlg(ResStr_NothingSelected, mtConfirmation, [mbYes, mbNo], 0) = mrYes then begin
    lKeyList := Get_KeyList(TN_SPECIMEN_UNIT);
  end;

  if Assigned(lKeyList) then begin
    if lKeyList.ItemCount > 0 then begin

      dmGeneral.Recorder.MenuOptionClick(ResStr_MnuReports + ';' + ResStr_MnuSpecimenFinder);
      dmGeneral.Recorder.DisplayData(TN_SPECIMEN_UNIT, lKeyList);
      if TdmGeneral.Allocated then TdmGeneral.Discard;
    end;
  end;
end;

{-------------------------------------------------------------------------------
  Implementation of function in IDisplayData.
}
function TfrmSpecimenFinder.SupportsTable(const ATableName: WideString): WordBool;
begin
  Result := (CompareText(ATableName, TN_SPECIMEN_UNIT) = 0);
end;

{-------------------------------------------------------------------------------
  Creates a new PreviousSearchCriterion
}
procedure TfrmSpecimenFinder.DisplayData(const AKeyList: IKeyList); safecall;
var
  lKeys: string;
  i: Integer;
  lPreviousResults: TPreviousResultsCriterion;
  lCaption: string;
begin
  if Assigned(AKeyList) then
  begin
    lKeys := '''' + AKeyList.GetKeyItem(0).KeyField1 + '''';
    for i := 1 to AKeyList.ItemCount - 1 do
      lKeys := lKeys + ', ''' + AKeyList.GetKeyItem(i).KeyField1 + '''';
    lPreviousResults := TPreviousResultsCriterion.Create(lKeys);
    lCaption := IntToStr(AKeyList.ItemCount) + ' Specimen';
    if AKeyList.ItemCount > 1 then
      lCaption := lCaption + 's';
    fraSpecimenFinderDragFrame.lbFinder.AddItem(lCaption, lPreviousResults);
  end;

  fraSpecimenFinderDragFrame.RefreshResults := True;
end;

{-------------------------------------------------------------------------------
  Parses a specified SFQ file and populates the search panel with the relevant
  criteria.
}
procedure TfrmSpecimenFinder.ReadXMLQuery(const FileName: string);
var
  lXmlDoc: IXMLDocument;
  lCurrentNode: IXMLNode;
  i: Integer;
  path, Operand1, Operand2: string;
  HasProperty: Boolean;
  Operator: TLogicalOperator;
  PropertyType: TBooleanProperty;
  lDragAndDropCriterion: TDragAndDropCriterion;
  lBooleanCriterion: TBooleanCriterion;
  lPreviousResultsCriterion: TPreviousResultsCriterion;
  ErrorMessage: string;
  function CountKeys(keyString: string): integer;
  var
    nextComma: integer;
  begin
    Result := 1;
    nextComma := AnsiPos(',', keyString) ;
    while nextComma <> 0 do
    begin
      Result := Result + 1;
      keyString := Copy(keyString, nextComma + 1, Length(keyString) - nextComma);
      nextComma := AnsiPos(',', keyString);
    end;
  end;
begin
  path := ExtractFilePath(Application.ExeName) + QUERIES_PATH;

  try
    lXmlDoc := TXMLDocument.Create(FileName);
    ErrorMessage := '';
    PropertyType := TBooleanProperty.Create;
    Operator := loBetween;

    if CompareStr(lXmlDoc.DocumentElement.NodeName, 'SpecimenFinder') = 0 then
    begin
      for i := 0 to lXmlDoc.DocumentElement.ChildNodes.Count - 1 do
      begin
        lCurrentNode := lXmlDoc.DocumentElement.ChildNodes[i];
        if NodeNameCriterionType(lCurrentNode.NodeName)
             = CRITERION_TYPE_DRAG_AND_DROP then
        begin
          if lCurrentNode.HasAttribute('key') then
          begin
            lDragAndDropCriterion := TDragAndDropCriterion.Create(lCurrentNode.NodeName,
                                lCurrentNode.Attributes['key']);
            fraSpecimenFinderDragFrame.AddFinderDragAndDropItem(lDragAndDropCriterion);
          end
          else
          begin
            ErrorMessage := 'Attribute ''key'' is required by node '
                            + lCurrentNode.NodeName;
            break;
          end;
        end
        else if NodeNameCriterionType(lCurrentNode.NodeName)
             = CRITERION_TYPE_BOOLEAN then
        begin
          if (lCurrentNode.HasAttribute('property') and
             lCurrentNode.HasAttribute('operand1') and
             lCurrentNode.HasAttribute('operator')) then
          begin
            PropertyType := TBooleanProperty.Create(TBooleanPropertyEnum(lCurrentNode.Attributes['property']));
            Operand1 := lCurrentNode.Attributes['operand1'];
            Operator := TLogicalOperator(lCurrentNode.Attributes['operator']);
            if lCurrentNode.HasAttribute('isMetadata') then
            begin
              if lCurrentNode.Attributes['isMetadata'] = '1' then
              begin
                PropertyType.IsMetadata := true;
                PropertyType.MetadataName := lCurrentNode.Attributes['metadataName'];
              end;
            end
            else
            begin
              ErrorMessage := 'Attribute ''isMetadata'' is required by node '
                                + lCurrentNode.NodeName;
              break;
            end;
          end
          else
          begin
            ErrorMessage := 'Attributes ''property'', ''operand1'' and '
                            + '''operator'' are required by node '
                            + lCurrentNode.NodeName;
            break;
          end;
          if lCurrentNode.HasAttribute('operand2') then
            Operand2 := lCurrentNode.Attributes['operand2']
          else
            Operand2 := '';

          if lCurrentNode.HasAttribute('hasProperty') then
          begin
            if lCurrentNode.Attributes['hasProperty'] = '1' then
              HasProperty := true
            else
              HasProperty := false;
          end
          else
            HasProperty := true;

          lBooleanCriterion := TBooleanCriterion.Create(
                                  PropertyType,
                                  Operator,
                                  Operand1,
                                  Operand2,
                                  HasProperty);
          AddBooleanCriterion(lBooleanCriterion);
        end
        else if NodeNameCriterionType(lCurrentNode.NodeName)
             = CRITERION_TYPE_PREVIOUS_RESULTS then
        begin
          if lCurrentNode.HasAttribute('keys') then
          begin
            lPreviousResultsCriterion := TPreviousResultsCriterion.Create(
                                            lCurrentNode.Attributes['keys']);
            fraSpecimenFinderDragFrame.lbFinder.AddItem(
                        IntToStr(CountKeys(lCurrentNode.Attributes['keys']))
                         + ' Specimens',
                        lPreviousResultsCriterion);
            fraSpecimenFinderDragFrame.RefreshResults := true;
          end
          else
          begin
            ErrorMessage := 'Attribute ''keys'' is required by node '
                                + lCurrentNode.NodeName;
            break;
          end;
        end
        else
        begin
          ErrorMessage := 'File contains an invalid non-root node.';
          break;
        end;
      end;
    end
    else
      ErrorMessage := 'Root node is not of the correct type.';
    if ErrorMessage <> '' then
    begin
      fraSpecimenFinderDragFrame.lbFinder.Clear;
      fraSpecimenFinderDragFrame.RefreshResults := true;
      MessageDlg(ErrorMessage, mtError, [mbOk], 0);
    end;
  except
    on E: EDOMParseError do
    begin
      fraSpecimenFinderDragFrame.lbFinder.Clear;
      fraSpecimenFinderDragFrame.RefreshResults := true;
      MessageDlg('XML could not be parsed - please check file is valid XML.',
                  mtError, [mbOk], 0);
    end;
    on E: Exception do
    begin
      fraSpecimenFinderDragFrame.lbFinder.Clear;
      fraSpecimenFinderDragFrame.RefreshResults := true;
      MessageDlg('XML contains some attributes with invalid values.', mtError, [mbOk], 0);
    end;
  end;
end;

{-------------------------------------------------------------------------------
  Returns an integer denoting the search criterion type for a given node name
}
function TfrmSpecimenFinder.NodeNameCriterionType(const ANodeName: string): Integer;
begin
  if CompareStr(ANodeName, TN_LOCATION) = 0 then
    Result := CRITERION_TYPE_DRAG_AND_DROP
  else if CompareStr(ANodeName, TN_CONCEPT) = 0 then
    Result := CRITERION_TYPE_DRAG_AND_DROP
  else if CompareStr(ANodeName, TN_COLLECTION) = 0 then
    Result := CRITERION_TYPE_DRAG_AND_DROP
  else if CompareStr(ANodeName, TN_NAME) = 0 then
    Result := CRITERION_TYPE_DRAG_AND_DROP
  else if CompareStr(ANodeName, TN_SURVEY) = 0 then
    Result := CRITERION_TYPE_DRAG_AND_DROP
  else if CompareStr(ANodeName, TN_STORE) = 0 then
    Result := CRITERION_TYPE_DRAG_AND_DROP
  else if CompareStr(ANodeName, TN_MOVEMENT) = 0 then
    Result := CRITERION_TYPE_DRAG_AND_DROP
  else if CompareStr(ANodeName, TN_TAXON_LIST_ITEM) = 0 then
    Result := CRITERION_TYPE_DRAG_AND_DROP
  else if CompareStr(ANodeName, CR_EXPRESSION) = 0 then
    Result := CRITERION_TYPE_BOOLEAN
  else if CompareStr(ANodeName, CR_SPECIMENS) = 0 then
    Result := CRITERION_TYPE_PREVIOUS_RESULTS
  else Result := -1;
end;

{-------------------------------------------------------------------------------
  Opens an open file dialog and calls ReadXMLQuery for the selected file
}
procedure TfrmSpecimenFinder.OpenQuery;
var
  lOpenDialog: TOpenDialog;
begin
  fraSpecimenFinderDragFrame.lbFinder.Items.Clear;
  lOpenDialog := TOpenDialog.Create(self);
  lOpenDialog.Options := lOpenDialog.Options + [ofFileMustExist];
  try
    lOpenDialog.Filter := 'SFQ Files|*.sfq';
    if lOpenDialog.Execute then
    begin
      fraSpecimenFinderDragFrame.RefreshResults := true;
      pcFinder.ActivePage := tsFinder;
      ReadXMLQuery(lOpenDialog.FileName);
    end;
  finally
    lOpenDialog.Free;
  end;
end;

{-------------------------------------------------------------------------------
  Opens a dialog and tries to save criteria to an XML file
}
procedure TfrmSpecimenFinder.SaveQuery;
var
  lSaveDialog: TSaveDialog;
  lXmlDoc: TXMLDocument;
begin
  lSaveDialog := TSaveDialog.Create(self);
  try
    lSaveDialog.Filter := 'SFQ Files| *.sfq';
    lSaveDialog.DefaultExt := 'sfq';
    lSaveDialog.Options := lSaveDialog.Options + [ofOverwritePrompt];
    if lSaveDialog.Execute then
    begin
      lXmlDoc := WriteXMLQuery;
      DeleteFile(lSaveDialog.FileName);
      lXmlDoc.SaveToFile(lSaveDialog.FileName);
    end;
  finally
    lSaveDialog.Free;
  end;
end;

{-------------------------------------------------------------------------------
  Returns a TXMLDocument object representing the search criteria currently in
  the specimen finder window.
}
function TfrmSpecimenFinder.WriteXMLQuery: TXMLDocument;
var
  i: Integer;
  lRootNode: IXMLNode;
  lCriterionNode: IXMLNode;
  lTableName: string;
begin
  Result := TXMLDocument.Create(self);
  Result.Active := true;
  lRootNode := Result.AddChild(SPECIMEN_FINDER);
  for i := 0 to fraSpecimenFinderDragFrame.lbFinder.Items.Count - 1 do
  begin
    if fraSpecimenFinderDragFrame.lbFinder.Items.Objects[i] is TDragAndDropCriterion then
    begin
      lTableName := GetTableName(
          TDragAndDropCriterion(fraSpecimenFinderDragFrame.lbFinder.Items.Objects[i]).SearchType);
      lCriterionNode := lRootNode.AddChild(lTableName);
      lCriterionNode.Attributes['key'] :=
        TDragAndDropCriterion(fraSpecimenFinderDragFrame.lbFinder.Items.Objects[i]).SearchValue;
    end
    else if fraSpecimenFinderDragFrame.lbFinder.Items.Objects[i] is TBooleanCriterion then
    begin
      lCriterionNode := lRootNode.AddChild(CR_EXPRESSION);
      lCriterionNode.Attributes['property'] :=
        integer(TBooleanCriterion(fraSpecimenFinderDragFrame.lbFinder.Items.Objects[i]).PropertyType.PropertyEnum);
      lCriterionNode.Attributes['operand1'] :=
        TBooleanCriterion(fraSpecimenFinderDragFrame.lbFinder.Items.Objects[i]).Operand1;
      lCriterionNode.Attributes['operand2'] :=
        TBooleanCriterion(fraSpecimenFinderDragFrame.lbFinder.Items.Objects[i]).Operand2;
      lCriterionNode.Attributes['operator'] :=
        integer(TBooleanCriterion(fraSpecimenFinderDragFrame.lbFinder.Items.Objects[i]).Operator);
      if TBooleanCriterion(fraSpecimenFinderDragFrame.lbFinder.Items.Objects[i]).HasProperty then
        lCriterionNode.Attributes['hasProperty'] := '1'
      else
        lCriterionNode.Attributes['hasProperty'] := '0';
      if TBooleanCriterion(fraSpecimenFinderDragFrame.lbFinder.Items.Objects[i]).PropertyType.IsMetadata then
      begin
        lCriterionNode.Attributes['isMetadata'] := '1';
        lCriterionNode.Attributes['metadataName'] :=
           TBooleanCriterion(
           fraSpecimenFinderDragFrame.lbFinder.Items.Objects[i]).PropertyType.MetadataName;
      end
      else
        lCriterionNode.Attributes['isMetadata'] := '0';
    end
    else if fraSpecimenFinderDragFrame.lbFinder.Items.Objects[i] is TPreviousResultsCriterion then
    begin
      lCriterionNode := lRootNode.AddChild(CR_SPECIMENS);
      lCriterionNode.Attributes['keys'] :=
        TPreviousResultsCriterion(
          fraSpecimenFinderDragFrame.lbFinder.Items.Objects[i]).Keys;
    end;

  end;
end;

{-------------------------------------------------------------------------------
  Returns the table name for each finder type.
}
function TfrmSpecimenFinder.GetTableName(FinderType: TFinderType): string;
begin
  case FinderType of
    ftCollection:     Result := TN_COLLECTION;
    ftStore:          Result := TN_STORE;
    ftSurvey:         Result := TN_SURVEY;
    ftTaxonListItem:  Result := TN_TAXON_LIST_ITEM;
    ftName:           Result := TN_NAME;
    ftMovement:       Result := TN_MOVEMENT;
    ftConcept:        Result := TN_CONCEPT;
    ftLocation:       Result := TN_LOCATION;
  else
    Result := '';
  end;
end;

{-------------------------------------------------------------------------------
  Implementation of method in IMergeMenuManager
}
function TfrmSpecimenFinder.Get_DynamicMenuList: IDynamicMenuList;
begin
  Result := Self as IDynamicMenuList;
end;

{-------------------------------------------------------------------------------
  Gets the number of dynamic menu items in TfrmSpecimenFinder
}
function TfrmSpecimenFinder.Get_Count: Integer;
begin
  Result := 1;
end;

{-------------------------------------------------------------------------------
  Gets the dynamic menu items in TfrmSpecimenFinder
}
function TfrmSpecimenFinder.Items(AIndex: Integer): IDynamicMenu;
begin
  case AIndex of
    0:
    begin
      Result := CreateComObject(CLASS_SpecimenFinderQueryMenu) as IDynamicMenu;
      (Result as ISpecimenFinderQueryMenu).ItemType := MNU_SPECIMEN_FINDER;
      // pass a pointer to the object so it can find the diagram
      (Result as ISpecimenFinderQueryMenu).SpecimenFinder := Integer(Self);
    end;
    else
      Result := nil;
  end;
end;

{-------------------------------------------------------------------------------
  Sets the path of the new menu items.
}
function TfrmSpecimenFinder.Get_MenuPath(AIndex: Integer): WideString;
begin
  case AIndex of
    0: Result := ResStr_MnuFile + '\' + ResStr_MnuSpecimenFinderQuery;
  end;
end;

{-------------------------------------------------------------------------------
  Sets the path of the menu item after the new specimen finder items
}
function TfrmSpecimenFinder.Get_InsertBeforeMenu(AIndex: Integer): WideString;
begin 
  Result := '';
end;

{-------------------------------------------------------------------------------
  Sets the path of the menu item before the new specimen finder items
}
function TfrmSpecimenFinder.Get_InsertAfterMenu(AIndex: Integer): WideString;
begin
  case AIndex of
    0: Result := ResStr_MnuFile + '\' + ResStr_MnuPrint;
  end;
end;

{-------------------------------------------------------------------------------
  Sets the image list used for the specimen finder menu items.
}
function TfrmSpecimenFinder.Get_ImageListHandle: Integer;
begin
  Result := dmInterface.ilMenuItems.Handle;
end;

{-------------------------------------------------------------------------------
  Fires the Add Criterion button if 'Enter' key is pressed
}
procedure TfrmSpecimenFinder.TextBoxKeyPress(Sender: TObject;
  var Key: Char);
var
  ValidInput: Boolean;
  EmptyTextbox: TEdit;
begin
  ValidInput := true;
  EmptyTextbox := nil;
  if Length(eOperand1.Text) = 0 then
      begin
        ValidInput := False;
        EmptyTextbox := eOperand1;
      end
      else if (TOperatorPair(cmbOperator.Items.Objects[cmbOperator.ItemIndex]).OperatorEnum = loBetween)
          and (Length(eOperand2.Text) = 0) then
      begin
        ValidInput := False;
        EmptyTextbox := eOperand2;
      end;
  if (Key = Char(13)) then
  begin
    if ValidInput then
      btnAddCriterion.Click
    else
      FocusControl(EmptyTextbox);
  end;
end;

initialization
  TActiveFormFactory.Create(
    ComServer,
    TActiveFormControl,
    TfrmSpecimenFinder,
    Class_frmSpecimenFinder,
    2,
    '',
    OLEMISC_SIMPLEFRAME or OLEMISC_ACTSLIKELABEL,
    tmApartment);
end.
