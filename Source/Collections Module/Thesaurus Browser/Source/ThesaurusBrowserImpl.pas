{===============================================================================
  Unit:        ThesaurusBrowserImpl

  Defines:     TfrmThesaurusBrowser

  Description: Main implementation screen for the Thesaurus Browser

  Created:     September 2003

  Last revision information:
    $Revision: 19 $
    $Date: 19/01/05 14:06 $
    $Author: Johnvanbreda $

===============================================================================}
unit ThesaurusBrowserImpl;

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ActiveX, AxCtrls, ThesaurusBrowser_TLB, StdVcl, ExtCtrls,
  BaseDragFrameUnit, ThesaurusNavigator, GeneralData, ApplicationSettings,
  InterfaceDataModule, ComCtrls, ToolWin, Recorder2000_TLB, HistoryManager,
  VersionInfo, BaseDetailFrameUnit, BasePageControlFrameUnit, RapTree,
  ThesaurusBrowserDetails, StdCtrls, BaseTabSheetFrameUnit, ImgList, DataTypes,
  ConceptHTMLDetail, Menus, ThesaurusNavigatorDiagramContainer,
  ThesaurusDiagram, XPMenu;

type
  {-----------------------------------------------------------------------------
    Screen that allows the user to browse the entire thesaurus, by subject area,
    domain and concept group.  The user is able to navigate through the
    thesaurus by relationships between terms.  Contains the thesaurus navigator
    and details frames, plus optional diagrams.
    When the form is active, a menu item is added to the Recorder File menu,
    Diagrams using the IMergeMenuManager and IDynamicMenu interfaces.  The
    contents of this menu are disabled unless a diagram tab is visible.  The
    Diagrams menu item contains the following options:
    Open - displays a File Open dialog allowing a diagram to be loaded.
    Diagrams are saved to disk using a custom file format with the extension
    *.tdg.
    Save - if the diagram has been previously saved to disk, then updates the
    file.  Otherwise, this option is always disabled.
    Save As - displays a File Save dialog allowing the user to specify a
    location into which to save the *.tdg file.
    Save As Image - displays a File Save dialog with *.bmp, *.gif and *.jpg
    extension options.  If the user selects Ok on the dialog, then the diagram
    image is rendered as the appropriate format and saved to the file.
    Print - Displays a Print dialog.  This allows the user to print the diagram.
  }
  TfrmThesaurusBrowser = class(TActiveForm, IfrmThesaurusBrowser, IFormCaption,
      IKeyListSupplier, IMergeMenuManager, IDynamicMenuList, IDisplayData)
    fraThesaurusBrowserDetails: TfraThesaurusBrowserDetails;
    fraThesaurusNavigatorDiagramContainer:
        TfraThesaurusNavigatorDiagramContainer;
    ilRecorderMenu: TImageList;
    pnlBrowse: TPanel;
    Splitter: TSplitter;
    tbtnBack: TToolButton;
    tbtnDivider: TToolButton;
    tbtnForward: TToolButton;
    tbtnNewWindow: TToolButton;
    tlbTree: TToolBar;
    procedure ActiveFormKeyDown(Sender: TObject; var Key: Word; Shift:
        TShiftState);
    procedure fraThesaurusNavigatortvHierarchyChange(Sender: TObject; Node:
        TFlyNode);
    procedure fraThesaurusNavigatortvHierarchyChanging(Sender: TObject; Node:
        TFlyNode; var AllowChange: Boolean);
    procedure tbtnNewWindowClick(Sender: TObject);
  private
    FEvents: IfrmThesaurusBrowserEvents;
    FXPMenu: TXPMenu;
    procedure ActivateEvent(Sender: TObject);
    procedure ClickEvent(Sender: TObject);
    procedure CreateEvent(Sender: TObject);
    procedure DblClickEvent(Sender: TObject);
    procedure DeactivateEvent(Sender: TObject);
    procedure DestroyEvent(Sender: TObject);
    procedure FrameNotification(Sender: TObject; AType: TEventType; const
        AParams: TVariantArray);
    function Get_Exportable: WordBool; safecall;
    function Get_FormCaption: Widestring; safecall;
    function Get_KeyList: IKeyList; safecall;
    procedure KeyPressEvent(Sender: TObject; var Key: Char);
    procedure LinkToNewDiagram(Sender: TObject; ADiagram: TThesaurusDiagram);
    procedure PaintEvent(Sender: TObject);
    procedure SelectDiagramConcept(Sender: TObject; const AConceptKey: string);
  protected
    procedure DefinePropertyPages(DefinePropertyPage: TDefinePropertyPage);
        override;
    procedure DisplayConcept(const AKey: WideString); safecall;
    procedure DisplayConceptGroup(const AKey: WideString); safecall;
    procedure DisplayData(const AKeyList: IKeyList); safecall;
    procedure EventSinkChanged(const EventSink: IUnknown); override;
    function Get_Active: WordBool; safecall;
    function Get_AlignDisabled: WordBool; safecall;
    function Get_AutoScroll: WordBool; safecall;
    function Get_AutoSize: WordBool; safecall;
    function Get_AxBorderStyle: TxActiveFormBorderStyle; safecall;
    function Get_Caption: WideString; safecall;
    function Get_Color: OLE_COLOR; safecall;
    function Get_Count: Integer; safecall;
    function Get_DoubleBuffered: WordBool; safecall;
    function Get_DropTarget: WordBool; safecall;
    function Get_DynamicMenuList: IDynamicMenuList; safecall;
    function Get_Enabled: WordBool; safecall;
    function Get_Font: IFontDisp; safecall;
    function Get_HelpFile: WideString; safecall;
    function Get_ImageListHandle: Integer; safecall;
    function Get_InsertAfterMenu(AIndex: Integer): WideString; safecall;
    function Get_InsertBeforeMenu(AIndex: Integer): WideString; safecall;
    function Get_KeyPreview: WordBool; safecall;
    function Get_MenuPath(AIndex: Integer): WideString; safecall;
    function Get_PixelsPerInch: Integer; safecall;
    function Get_PrintScale: TxPrintScale; safecall;
    function Get_Scaled: WordBool; safecall;
    function Get_ScreenSnap: WordBool; safecall;
    function Get_SnapBuffer: Integer; safecall;
    function Get_Visible: WordBool; safecall;
    function Get_VisibleDockClientCount: Integer; safecall;
    function Items(AIndex: Integer): IDynamicMenu; safecall;
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
    function SupportsTable(const ATableName: WideString): WordBool; safecall;
    procedure WndProc(var Message: TMessage); override;
    procedure _Set_Font(var Value: IFontDisp); safecall;
  public
    destructor Destroy; override;
    function CurrentDiagram: TThesaurusDiagram;
    procedure FocusDiagram(Sender: TObject);
    procedure Initialize; override;
    procedure OpenDiagram;
  end;
  
implementation

uses ComObj, ComServ, DataClasses, ComClasses, ResourceStrings,
     LuxembourgConstants, DiagramsMenu, Diagram;

{$R *.DFM}

{ TfrmThesaurusBrowser }

{-==============================================================================
    TfrmThesaurusBrowser
===============================================================================}
{-------------------------------------------------------------------------------
  Clean up objects.  Although frames are owned, free frames first so that items
      requiring a parent window can destroy properly
}
destructor TfrmThesaurusBrowser.Destroy;
begin
  // Free frames first, so that items requiring a parent window can destroy properly
  FreeAndNil(fraThesaurusNavigatorDiagramContainer);
  FreeAndNil(fraThesaurusBrowserDetails);
  DiscardAppSettings;
  if TdmGeneral.Allocated then TdmGeneral.Discard;  
  inherited;
end;  // TfrmThesaurusBrowser.Destroy 

{-------------------------------------------------------------------------------
}
procedure TfrmThesaurusBrowser.ActivateEvent(Sender: TObject);
begin
  if FEvents <> nil then FEvents.OnActivate;
end;  // TfrmThesaurusBrowser.ActivateEvent

{-------------------------------------------------------------------------------
  Because of a quirk that I don't understand, sometime the key down messages
      destined for the diagram are passed to the form.  Relay the delete key
      strokes to the diagram.
}
procedure TfrmThesaurusBrowser.ActiveFormKeyDown(Sender: TObject; var Key: Word;
    Shift: TShiftState);
begin
  if (ActiveControl is TThesaurusDiagram) and (Key=VK_DELETE) then
    TThesaurusDiagram(ActiveControl).DeleteSelectedConcept
  else if (ActiveControl is TInternalDiagram) and (Key=VK_DELETE) then
    TThesaurusDiagram(TInternalDiagram(ActiveControl).DiagramContainer).DeleteSelectedConcept;
end;  // TfrmThesaurusBrowser.ActiveFormKeyDown 

{-------------------------------------------------------------------------------
}
procedure TfrmThesaurusBrowser.ClickEvent(Sender: TObject);
begin
  if FEvents <> nil then FEvents.OnClick;
end;  // TfrmThesaurusBrowser.ClickEvent 

{-------------------------------------------------------------------------------
}
procedure TfrmThesaurusBrowser.CreateEvent(Sender: TObject);
begin
  if FEvents <> nil then FEvents.OnCreate;
end;  // TfrmThesaurusBrowser.CreateEvent 

{-------------------------------------------------------------------------------
  Returns the currently visible diagram, or nil if none visible. 
}
function TfrmThesaurusBrowser.CurrentDiagram: TThesaurusDiagram;
begin
  with fraThesaurusNavigatorDiagramContainer.pcNavigatorAndDiagrams do
    if ActivePage.Controls[0] is TThesaurusDiagram then
      Result := TThesaurusDiagram(ActivePage.Controls[0])
    else
      Result := nil;
end;  // TfrmThesaurusBrowser.CurrentDiagram 

{-------------------------------------------------------------------------------
}
procedure TfrmThesaurusBrowser.DblClickEvent(Sender: TObject);
begin
  if FEvents <> nil then FEvents.OnDblClick;
end;  // TfrmThesaurusBrowser.DblClickEvent 

{-------------------------------------------------------------------------------
}
procedure TfrmThesaurusBrowser.DeactivateEvent(Sender: TObject);
begin
  if FEvents <> nil then FEvents.OnDeactivate;
end;  // TfrmThesaurusBrowser.DeactivateEvent 

{-------------------------------------------------------------------------------
}
procedure TfrmThesaurusBrowser.DefinePropertyPages(DefinePropertyPage:
    TDefinePropertyPage);
begin
  { Define property pages here.  Property pages are defined by calling
    DefinePropertyPage with the class id of the page.  For example,
      DefinePropertyPage(Class_frmThesaurusBrowserPage); }
end;  // TfrmThesaurusBrowser.DefinePropertyPages 

{-------------------------------------------------------------------------------
}
procedure TfrmThesaurusBrowser.DestroyEvent(Sender: TObject);
begin
  if FEvents <> nil then FEvents.OnDestroy;
end;  // TfrmThesaurusBrowser.DestroyEvent

{-------------------------------------------------------------------------------
}
procedure TfrmThesaurusBrowser.DisplayConcept(const AKey: WideString);
begin
  fraThesaurusNavigatorDiagramContainer.fraThesaurusNavigator.DisplayConcept(
      AKey, True);
end;  // TfrmThesaurusBrowser.DisplayConcept 

{-------------------------------------------------------------------------------
  Display data requested through COM (originally a call to
      Recorder2000.DisplayData).  Only the first concept is displayed.
}
procedure TfrmThesaurusBrowser.DisplayData(const AKeyList: IKeyList);
begin
  if AKeyList.ItemCount>0 then
    with fraThesaurusNavigatorDiagramContainer.fraThesaurusNavigator do begin
      // Ensure objects are created
      FrameEnter(nil);
      DisplayConcept(AKeyList.GetKeyItem(0).KeyField1);
    end;
end;  // TfrmThesaurusBrowser.DisplayData 

{-------------------------------------------------------------------------------
}
procedure TfrmThesaurusBrowser.EventSinkChanged(const EventSink: IUnknown);
begin
  FEvents := EventSink as IfrmThesaurusBrowserEvents;
  inherited EventSinkChanged(EventSink);
end;  // TfrmThesaurusBrowser.EventSinkChanged 

{-------------------------------------------------------------------------------
  Event handler that allows the diagram to make itself focused. 
}
procedure TfrmThesaurusBrowser.FocusDiagram(Sender: TObject);
begin
  if Sender is TWinControl then
    ActiveControl := TWinControl(Sender);
end;  // TfrmThesaurusBrowser.FocusDiagram 

{-------------------------------------------------------------------------------
  Handle notification events from the details frames that trigger navigations. 
}
procedure TfrmThesaurusBrowser.FrameNotification(Sender: TObject; AType:
    TEventType; const AParams: TVariantArray);
begin
  case AType of
    etNavigation:
      if CompareText(AParams[0], 'Concept')=0 then
        with fraThesaurusNavigatorDiagramContainer.fraThesaurusNavigator do
            begin
        DisplayConcept(AParams[1]);
      end;
  end; // case
end;  // TfrmThesaurusBrowser.FrameNotification

{-------------------------------------------------------------------------------
}
procedure TfrmThesaurusBrowser.fraThesaurusNavigatortvHierarchyChange(Sender:
    TObject; Node: TFlyNode);
var
  lUpdateRequired: boolean;
begin
  fraThesaurusNavigatorDiagramContainer.fraThesaurusNavigator.
      tvHierarchyChange(Sender, Node);
  fraThesaurusBrowserDetails.pcDetails.Enabled := Assigned(Node);
  if Assigned(Node) then
    if Node is TTermNode then begin
      lUpdateRequired := fraThesaurusBrowserDetails.Key<>TTermNode(Node).ConceptKey;
      fraThesaurusBrowserDetails.SetAdditionalProperties(Node);
      if lUpdateRequired then
        fraThesaurusBrowserDetails.LoadContent;
    end;
end;  // TfrmThesaurusBrowser.fraThesaurusNavigatortvHierarchyChange 

{-------------------------------------------------------------------------------
  Ensure pointers to IAdditionalProperties are dropped before the selected node
      ever changes.
}
procedure TfrmThesaurusBrowser.fraThesaurusNavigatortvHierarchyChanging(Sender:
    TObject; Node: TFlyNode; var AllowChange: Boolean);
begin
  if Assigned(fraThesaurusBrowserDetails) then
    if fraThesaurusNavigatorDiagramContainer.fraThesaurusNavigator.tvHierarchy.Selected<>Node then
      fraThesaurusBrowserDetails.DropInterfaceReferences;
end;  // TfrmThesaurusBrowser.fraThesaurusNavigatortvHierarchyChanging 

{-------------------------------------------------------------------------------
}
function TfrmThesaurusBrowser.Get_Active: WordBool;
begin
  Result := Active;
end;  // TfrmThesaurusBrowser.Get_Active 

{-------------------------------------------------------------------------------
}
function TfrmThesaurusBrowser.Get_AlignDisabled: WordBool;
begin
  Result := AlignDisabled;
end;  // TfrmThesaurusBrowser.Get_AlignDisabled 

{-------------------------------------------------------------------------------
}
function TfrmThesaurusBrowser.Get_AutoScroll: WordBool;
begin
  Result := AutoScroll;
end;  // TfrmThesaurusBrowser.Get_AutoScroll 

{-------------------------------------------------------------------------------
}
function TfrmThesaurusBrowser.Get_AutoSize: WordBool;
begin
  Result := AutoSize;
end;  // TfrmThesaurusBrowser.Get_AutoSize 

{-------------------------------------------------------------------------------
}
function TfrmThesaurusBrowser.Get_AxBorderStyle: TxActiveFormBorderStyle;
begin
  Result := Ord(AxBorderStyle);
end;  // TfrmThesaurusBrowser.Get_AxBorderStyle 

{-------------------------------------------------------------------------------
}
function TfrmThesaurusBrowser.Get_Caption: WideString;
begin
  Result := Caption;
end;  // TfrmThesaurusBrowser.Get_Caption 

{-------------------------------------------------------------------------------
}
function TfrmThesaurusBrowser.Get_Color: OLE_COLOR;
begin
  Result := OLE_COLOR(Color);
end;  // TfrmThesaurusBrowser.Get_Color 

{-------------------------------------------------------------------------------
  Single Diagrams item on the File Menu. 
}
function TfrmThesaurusBrowser.Get_Count: Integer;
begin
  Result := 1;
end;  // TfrmThesaurusBrowser.Get_Count 

{-------------------------------------------------------------------------------
}
function TfrmThesaurusBrowser.Get_DoubleBuffered: WordBool;
begin
  Result := DoubleBuffered;
end;  // TfrmThesaurusBrowser.Get_DoubleBuffered 

{-------------------------------------------------------------------------------
}
function TfrmThesaurusBrowser.Get_DropTarget: WordBool;
begin
  Result := DropTarget;
end;  // TfrmThesaurusBrowser.Get_DropTarget 

{-------------------------------------------------------------------------------
  Returns the Dynamic Menu interface. 
}
function TfrmThesaurusBrowser.Get_DynamicMenuList: IDynamicMenuList;
begin
  Result := Self as IDynamicMenuList;
end;  // TfrmThesaurusBrowser.Get_DynamicMenuList 

{-------------------------------------------------------------------------------
}
function TfrmThesaurusBrowser.Get_Enabled: WordBool;
begin
  Result := Enabled;
end;  // TfrmThesaurusBrowser.Get_Enabled 

{-------------------------------------------------------------------------------
  Concepts are exportable. 
}
function TfrmThesaurusBrowser.Get_Exportable: WordBool;
begin
  Result := True;
end;  // TfrmThesaurusBrowser.Get_Exportable 

{-------------------------------------------------------------------------------
}
function TfrmThesaurusBrowser.Get_Font: IFontDisp;
begin
  GetOleFont(Font, Result);
end;  // TfrmThesaurusBrowser.Get_Font 

{-------------------------------------------------------------------------------
}
function TfrmThesaurusBrowser.Get_FormCaption: Widestring;
begin
  Result := ResStr_ThesaurusBrowser;
end;  // TfrmThesaurusBrowser.Get_FormCaption 

{-------------------------------------------------------------------------------
}
function TfrmThesaurusBrowser.Get_HelpFile: WideString;
begin
  Result := WideString(HelpFile);
end;  // TfrmThesaurusBrowser.Get_HelpFile 

{-------------------------------------------------------------------------------
}
function TfrmThesaurusBrowser.Get_ImageListHandle: Integer;
begin
  result := ilRecorderMenu.Handle;
end;  // TfrmThesaurusBrowser.Get_ImageListHandle 

{-------------------------------------------------------------------------------
  Position the menu item after the Print item. 
}
function TfrmThesaurusBrowser.Get_InsertAfterMenu(AIndex: Integer): WideString;
begin
  Result := ResStr_MnuFile + '\' + ResStr_MnuPrint;
end;  // TfrmThesaurusBrowser.Get_InsertAfterMenu 

{-------------------------------------------------------------------------------
  Method not used. 
}
function TfrmThesaurusBrowser.Get_InsertBeforeMenu(AIndex: Integer): WideString;
begin
  Result := '';
end;  // TfrmThesaurusBrowser.Get_InsertBeforeMenu 

{-------------------------------------------------------------------------------
  Return a keylist for the selected item to Recorder when requested. 
}
function TfrmThesaurusBrowser.Get_KeyList: IKeyList;
var
  lKeylist: TEditableKeylist;
  lComKeyList: TComKeyList;
begin
  lKeylist := TEditableKeylist.Create;
  try
    lKeylist.SetTable('CONCEPT');
    if Assigned(
        fraThesaurusNavigatorDiagramContainer.fraThesaurusNavigator.tvHierarchy.Selected) then
      lKeylist.AddItem(TTermNode(
          fraThesaurusNavigatorDiagramContainer.fraThesaurusNavigator.tvHierarchy.Selected).ConceptKey, '');
    lComKeyList := TCOMKeyList.Create(lKeyList);
    Result := lComKeyList as IKeyList;
  finally
    lKeylist.Free;
  end; // try
end;  // TfrmThesaurusBrowser.Get_KeyList 

{-------------------------------------------------------------------------------
}
function TfrmThesaurusBrowser.Get_KeyPreview: WordBool;
begin
  Result := KeyPreview;
end;  // TfrmThesaurusBrowser.Get_KeyPreview 

{-------------------------------------------------------------------------------
  Return the path to the menu item - File\Diagrams 
}
function TfrmThesaurusBrowser.Get_MenuPath(AIndex: Integer): WideString;
begin
  case AIndex of
    0: Result := ResStr_MnuFile + '\' + ResStr_MnuDiagrams;
  end; // case
end;  // TfrmThesaurusBrowser.Get_MenuPath 

{-------------------------------------------------------------------------------
}
function TfrmThesaurusBrowser.Get_PixelsPerInch: Integer;
begin
  Result := PixelsPerInch;
end;  // TfrmThesaurusBrowser.Get_PixelsPerInch 

{-------------------------------------------------------------------------------
}
function TfrmThesaurusBrowser.Get_PrintScale: TxPrintScale;
begin
  Result := Ord(PrintScale);
end;  // TfrmThesaurusBrowser.Get_PrintScale 

{-------------------------------------------------------------------------------
}
function TfrmThesaurusBrowser.Get_Scaled: WordBool;
begin
  Result := Scaled;
end;  // TfrmThesaurusBrowser.Get_Scaled 

{-------------------------------------------------------------------------------
}
function TfrmThesaurusBrowser.Get_ScreenSnap: WordBool;
begin
  Result := ScreenSnap;
end;  // TfrmThesaurusBrowser.Get_ScreenSnap 

{-------------------------------------------------------------------------------
}
function TfrmThesaurusBrowser.Get_SnapBuffer: Integer;
begin
  Result := SnapBuffer;
end;  // TfrmThesaurusBrowser.Get_SnapBuffer 

{-------------------------------------------------------------------------------
}
function TfrmThesaurusBrowser.Get_Visible: WordBool;
begin
  Result := Visible;
end;  // TfrmThesaurusBrowser.Get_Visible 

{-------------------------------------------------------------------------------
}
function TfrmThesaurusBrowser.Get_VisibleDockClientCount: Integer;
begin
  Result := VisibleDockClientCount;
end;  // TfrmThesaurusBrowser.Get_VisibleDockClientCount 

{-------------------------------------------------------------------------------
}
procedure TfrmThesaurusBrowser.Initialize;
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
  //Setup handler for navigation events in the details frames
  fraThesaurusBrowserDetails.OnFrameNotification := FrameNotification;
  // and handler for linking up diagrams
  fraThesaurusNavigatorDiagramContainer.OnNewDiagram := LinkToNewDiagram;
  //Point history manager at our toolbuttons.
  fraThesaurusNavigatorDiagramContainer.fraThesaurusNavigator.HistoryManager.BackButton := tbtnBack;
  fraThesaurusNavigatorDiagramContainer.fraThesaurusNavigator.HistoryManager.ForwardButton := tbtnForward;

  FXPMenu := TXPMenu.Create(Self);
  FXPMenu.XPControls := [xcPopupMenu];
  FXPMenu.Active := True;

  fraThesaurusNavigatorDiagramContainer.XPMenu := FXPMenu;
  FXPMenu.InitComponent(fraThesaurusNavigatorDiagramContainer);
end;  // TfrmThesaurusBrowser.Initialize

{-------------------------------------------------------------------------------
}
function TfrmThesaurusBrowser.Items(AIndex: Integer): IDynamicMenu;
begin
  case AIndex of
    0: begin
      Result := CreateComObject(CLASS_DiagramsMenu) as IDynamicMenu;
      (Result as IDiagramsMenu).ItemType := MNU_DIAGRAM;
      // pass a pointer to the object so it can find the diagram
      (Result as IDiagramsMenu).ThesaurusBrowser := Integer(Self);
    end;
  end;
end;  // TfrmThesaurusBrowser.Items 

{-------------------------------------------------------------------------------
}
procedure TfrmThesaurusBrowser.KeyPressEvent(Sender: TObject; var Key: Char);
var
  TempKey: SmallInt;
begin
  TempKey := Smallint(Key);
  if FEvents <> nil then FEvents.OnKeyPress(TempKey);
  Key := Char(TempKey);
end;  // TfrmThesaurusBrowser.KeyPressEvent 

{-------------------------------------------------------------------------------
  Link the SelectDiagramConcept method up to each new diagram. 
}
procedure TfrmThesaurusBrowser.LinkToNewDiagram(Sender: TObject; ADiagram:
    TThesaurusDiagram);
begin
  ADiagram.OnSelectConcept := SelectDiagramConcept;
end;  // TfrmThesaurusBrowser.LinkToNewDiagram 

{-------------------------------------------------------------------------------
  Opens a diagram file into a new diagram tab. 
}
procedure TfrmThesaurusBrowser.OpenDiagram;
begin
  with TOpenDialog.Create(nil) do begin
    try
      Filter := ResStr_DiagramFileFilters;
      if Execute then
        fraThesaurusNavigatorDiagramContainer.CreateNewDiagram(FileName);
    finally
      Free;
    end; // try
  end;
end;  // TfrmThesaurusBrowser.OpenDiagram

{-------------------------------------------------------------------------------
}
procedure TfrmThesaurusBrowser.PaintEvent(Sender: TObject);
begin
  if FEvents <> nil then FEvents.OnPaint;
end;  // TfrmThesaurusBrowser.PaintEvent 

{-------------------------------------------------------------------------------
  When a concept is selected in a diagram, show its details. 
}
procedure TfrmThesaurusBrowser.SelectDiagramConcept(Sender: TObject; const
    AConceptKey: string);
begin
  fraThesaurusBrowserDetails.pcDetails.Enabled := AConceptKey<>'';
  if Sender is TThesaurusDiagram then begin
    fraThesaurusBrowserDetails.SetAdditionalProperties(Sender);
    fraThesaurusBrowserDetails.LoadContent;
  end;
end;  // TfrmThesaurusBrowser.SelectDiagramConcept

{-------------------------------------------------------------------------------
}
procedure TfrmThesaurusBrowser.Set_AutoScroll(Value: WordBool);
begin
  AutoScroll := Value;
end;  // TfrmThesaurusBrowser.Set_AutoScroll 

{-------------------------------------------------------------------------------
}
procedure TfrmThesaurusBrowser.Set_AutoSize(Value: WordBool);
begin
  AutoSize := Value;
end;  // TfrmThesaurusBrowser.Set_AutoSize 

{-------------------------------------------------------------------------------
}
procedure TfrmThesaurusBrowser.Set_AxBorderStyle(Value:
    TxActiveFormBorderStyle);
begin
  AxBorderStyle := TActiveFormBorderStyle(Value);
end;  // TfrmThesaurusBrowser.Set_AxBorderStyle 

{-------------------------------------------------------------------------------
}
procedure TfrmThesaurusBrowser.Set_Caption(const Value: WideString);
begin
  Caption := TCaption(Value);
end;  // TfrmThesaurusBrowser.Set_Caption 

{-------------------------------------------------------------------------------
}
procedure TfrmThesaurusBrowser.Set_Color(Value: OLE_COLOR);
begin
  Color := TColor(Value);
end;  // TfrmThesaurusBrowser.Set_Color 

{-------------------------------------------------------------------------------
}
procedure TfrmThesaurusBrowser.Set_DoubleBuffered(Value: WordBool);
begin
  DoubleBuffered := Value;
end;  // TfrmThesaurusBrowser.Set_DoubleBuffered 

{-------------------------------------------------------------------------------
}
procedure TfrmThesaurusBrowser.Set_DropTarget(Value: WordBool);
begin
  DropTarget := Value;
end;  // TfrmThesaurusBrowser.Set_DropTarget 

{-------------------------------------------------------------------------------
}
procedure TfrmThesaurusBrowser.Set_Enabled(Value: WordBool);
begin
  Enabled := Value;
end;  // TfrmThesaurusBrowser.Set_Enabled 

{-------------------------------------------------------------------------------
}
procedure TfrmThesaurusBrowser.Set_Font(const Value: IFontDisp);
begin
  SetOleFont(Font, Value);
end;  // TfrmThesaurusBrowser.Set_Font 

{-------------------------------------------------------------------------------
}
procedure TfrmThesaurusBrowser.Set_HelpFile(const Value: WideString);
begin
  HelpFile := String(Value);
end;  // TfrmThesaurusBrowser.Set_HelpFile 

{-------------------------------------------------------------------------------
}
procedure TfrmThesaurusBrowser.Set_KeyPreview(Value: WordBool);
begin
  KeyPreview := Value;
end;  // TfrmThesaurusBrowser.Set_KeyPreview 

{-------------------------------------------------------------------------------
}
procedure TfrmThesaurusBrowser.Set_PixelsPerInch(Value: Integer);
begin
  PixelsPerInch := Value;
end;  // TfrmThesaurusBrowser.Set_PixelsPerInch 

{-------------------------------------------------------------------------------
}
procedure TfrmThesaurusBrowser.Set_PrintScale(Value: TxPrintScale);
begin
  PrintScale := TPrintScale(Value);
end;  // TfrmThesaurusBrowser.Set_PrintScale 

{-------------------------------------------------------------------------------
}
procedure TfrmThesaurusBrowser.Set_Scaled(Value: WordBool);
begin
  Scaled := Value;
end;  // TfrmThesaurusBrowser.Set_Scaled 

{-------------------------------------------------------------------------------
}
procedure TfrmThesaurusBrowser.Set_ScreenSnap(Value: WordBool);
begin
  ScreenSnap := Value;
end;  // TfrmThesaurusBrowser.Set_ScreenSnap 

{-------------------------------------------------------------------------------
}
procedure TfrmThesaurusBrowser.Set_SnapBuffer(Value: Integer);
begin
  SnapBuffer := Value;
end;  // TfrmThesaurusBrowser.Set_SnapBuffer 

{-------------------------------------------------------------------------------
}
procedure TfrmThesaurusBrowser.Set_Visible(Value: WordBool);
begin
  Visible := Value;
end;  // TfrmThesaurusBrowser.Set_Visible 

{-------------------------------------------------------------------------------
}
function TfrmThesaurusBrowser.SupportsTable(const ATableName: WideString):
    WordBool;
begin
  Result := CompareText(ATableName, 'Concept')=0
end;  // TfrmThesaurusBrowser.SupportsTable 

{-------------------------------------------------------------------------------
  Creates a new instance of TfrmThesaurusBrowser.  Selects the currently
      selected concept.
}
procedure TfrmThesaurusBrowser.tbtnNewWindowClick(Sender: TObject);
begin
  dmGeneral.Recorder.MenuOptionClick(ResStr_MnuDictionaries + ';' +
      ResStr_MnuThesaurusBrowser);
  with fraThesaurusNavigatorDiagramContainer.fraThesaurusNavigator.tvHierarchy
      do begin
    if Assigned(Selected) then
      dmGeneral.Recorder.DisplayData('Concept', Get_KeyList);
  end;
end;  // TfrmThesaurusBrowser.tbtnNewWindowClick 

{-------------------------------------------------------------------------------
  ActiveX controls in Delphi 5 and later do not respond to navigation key
      strokes by default.  This is a knock on effect of a fix to an ActiveX
      control bug introduced in IE5.
  To fix this, messages are handled by the active form.  WM_GETDLGCODE is
      responded to to request that navigation key strokes are sent.  Other
      navigation messages are routed to the current active control.
}
procedure TfrmThesaurusBrowser.WndProc(var Message: TMessage);
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
end;  // TfrmThesaurusBrowser.WndProc 

{-------------------------------------------------------------------------------
}
procedure TfrmThesaurusBrowser._Set_Font(var Value: IFontDisp);
begin
  SetOleFont(Font, Value);
end;  // TfrmThesaurusBrowser._Set_Font 


procedure TfrmThesaurusBrowser.DisplayConceptGroup(
  const AKey: WideString);
begin
  fraThesaurusNavigatorDiagramContainer.fraThesaurusNavigator.DisplayConceptGroup(AKey);
end;

initialization
  TActiveFormFactory.Create(
    ComServer,
    TActiveFormControl,
    TfrmThesaurusBrowser,
    Class_frmThesaurusBrowser,
    1,
    '',
    OLEMISC_SIMPLEFRAME or OLEMISC_ACTSLIKELABEL,
    tmApartment);

end.

