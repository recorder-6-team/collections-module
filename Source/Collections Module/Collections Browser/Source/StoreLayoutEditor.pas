{===============================================================================
  Unit:        StoreLayoutEditor.pas

  Defines:     TfrmStorageLayout

  Description: Editor form for a store layout diagram

  Model:       StoreLayoutDiagram

  Created:     September 2004

  Last revision information:
    $Revision: 9 $
    $Date: 26/02/07 16:11 $
    $Author: Davidkelly $

===============================================================================}
unit StoreLayoutEditor;

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ActiveX, AxCtrls, CollectionsBrowser_TLB, StdVcl, StoreLayoutDiagram,
  StdCtrls, ImageListButton, ExtCtrls, Recorder2000_TLB, ComboListID, Variants,
  DataTypes;

resourcestring
  ResStr_StoreLayoutEditor = 'Store Layout Editor';
  ResStr_SomeStoresRemovedFromDiagram = 'The following stores were removed from the diagram '+
      'as they have either been moved from this store or no longer exist:';
  ResStr_SaveDiagramChanges = 'The diagram has been edited.  Do you want to save your '+
      'changes before closing the window?';

type
  {-----------------------------------------------------------------------------
    MDI Child window that allows viewing and editing of a store diagram on a larger window
    than the tab page allows.
    Each diagram has a one to one relationship with a store.  The diagram displayed is the
    diagram for the store that was selected when this screen was called.  The stores
    available for display on the diagram are those which have the store identified as the
    current location, plus those which have the store identified as the usual location, but
    have not been lost, destroyed or disposed of.
    When closing the form, if changes have been made to the diagram which have not yet been
    saved, then the user is prompted 'Do you want to save changes to the diagram?' with yes,
    no and cancel buttons.  Clicking yes saves the diagram and closes the form, clicking no
    closes the form, clicking cancel takes no further action (the form is not closed).
  }
  TfrmStorageLayout = class(TActiveForm, IfrmStorageLayout, IFormCaption)
    btnApply: TImageListButton;
    Label1: TLabel;
    lbAvailableStores: TIDListBox;
    pnlButton: TPanel;
    pnlDiagram: TPanel;
    pnlStoreList: TPanel;
    Splitter1: TSplitter;
    procedure ActiveFormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure btnApplyClick(Sender: TObject);
    procedure lbAvailableStoresDblClick(Sender: TObject);
    procedure lbAvailableStoresDrawItem(Control: TWinControl;
      Index: Integer; Rect: TRect; State: TOwnerDrawState);
  private
    FCaller: IRefreshTab;
    FDiagram: TStoreLayoutDiagram;
    FEvents: IfrmStorageLayoutEvents;
    FStoreKey: String;
    FTimestamp: TSQLSvrTimestamp;
    FNonCurrentStores: TStringList;
    procedure ActivateEvent(Sender: TObject);
    procedure ClickEvent(Sender: TObject);
    procedure CreateEvent(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure DblClickEvent(Sender: TObject);
    procedure DeactivateEvent(Sender: TObject);
    procedure DestroyEvent(Sender: TObject);
    procedure KeyPressEvent(Sender: TObject; var Key: Char);
    procedure PaintEvent(Sender: TObject);
  protected
    procedure DefinePropertyPages(DefinePropertyPage: TDefinePropertyPage); override;
    procedure EventSinkChanged(const EventSink: IUnknown); override;
    function Get_Active: WordBool; safecall;
    function Get_AlignDisabled: WordBool; safecall;
    function Get_AutoScroll: WordBool; safecall;
    function Get_AutoSize: WordBool; safecall;
    function Get_AxBorderStyle: TxActiveFormBorderStyle; safecall;
    function Get_Caller: IRefreshTab; safecall;
    function Get_Caption: WideString; safecall;
    function Get_Color: OLE_COLOR; safecall;
    function Get_DoubleBuffered: WordBool; safecall;
    function Get_DropTarget: WordBool; safecall;
    function Get_Enabled: WordBool; safecall;
    function Get_Font: IFontDisp; safecall;
    function Get_FormCaption: WideString; safecall;
    function Get_HelpFile: WideString; safecall;
    function Get_KeyPreview: WordBool; safecall;
    function Get_PixelsPerInch: Integer; safecall;
    function Get_PrintScale: TxPrintScale; safecall;
    function Get_Scaled: WordBool; safecall;
    function Get_ScreenSnap: WordBool; safecall;
    function Get_SnapBuffer: Integer; safecall;
    function Get_StoreKey: WideString; safecall;
    function Get_Visible: WordBool; safecall;
    function Get_VisibleDockClientCount: Integer; safecall;
    procedure Set_AutoScroll(Value: WordBool); safecall;
    procedure Set_AutoSize(Value: WordBool); safecall;
    procedure Set_AxBorderStyle(Value: TxActiveFormBorderStyle); safecall;
    procedure Set_Caller(const Value: IRefreshTab); safecall;
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
    procedure Set_StoreKey(const Value: WideString); safecall;
    procedure Set_Visible(Value: WordBool); safecall;
    procedure WndProc(var Message: TMessage); override;
    procedure _Set_Font(var Value: IFontDisp); safecall;
  public
    procedure Initialize; override;
    destructor Destroy; override;
  end;

implementation

uses ComObj, ComServ, InterfaceDataModule, GeneralData, GeneralFunctions,
     Diagram;

{$R *.DFM}

{ TfrmStorageLayout }

{-==============================================================================
    TfrmStorageLayout
===============================================================================}
{-------------------------------------------------------------------------------
}
procedure TfrmStorageLayout.ActivateEvent(Sender: TObject);
begin
  if FEvents <> nil then FEvents.OnActivate;
end;  // TfrmStorageLayout.ActivateEvent

{-------------------------------------------------------------------------------
}
procedure TfrmStorageLayout.ActiveFormKeyDown(Sender: TObject; var Key: Word; Shift:
    TShiftState);
begin
  if ((ActiveControl is TStoreLayoutDiagram) or
      (ActiveControl is TInternalDiagram)) and (Key=VK_DELETE) then
    FDiagram.DeleteSelectedConcept;
end;  // TfrmStorageLayout.ActiveFormKeyDown

{-------------------------------------------------------------------------------
}
procedure TfrmStorageLayout.btnApplyClick(Sender: TObject);
var
  lXml: String;
  lParams: Array of Variant;
begin
  lXML := FDiagram.WriteXML;
  lParams := VarArrayOf(['@Key', FStoreKey,
                         '@Xml', lXML,
                         '@Timestamp', FTimestamp
                         ]);
  dmGeneral.RunUpdateStoredProc('usp_StoreDiagram_Update', lParams);
  // reload the timestamp
  with dmGeneral.GetRecordset('usp_Store_LayoutXML_Select', ['@Key', FStoreKey]) do
    FTimeStamp := Fields['TimeStamp'].Value;
  if Assigned(FCaller) then
    FCaller.RefreshTab;
  FDiagram.Dirty := False;
end;  // TfrmStorageLayout.btnApplyClick 

{-------------------------------------------------------------------------------
}
procedure TfrmStorageLayout.ClickEvent(Sender: TObject);
begin
  if FEvents <> nil then FEvents.OnClick;
end;  // TfrmStorageLayout.ClickEvent 

{-------------------------------------------------------------------------------
}
procedure TfrmStorageLayout.CreateEvent(Sender: TObject);
begin
  if FEvents <> nil then FEvents.OnCreate;
end;  // TfrmStorageLayout.CreateEvent 

{-------------------------------------------------------------------------------
}
procedure TfrmStorageLayout.DblClickEvent(Sender: TObject);
begin
  if FEvents <> nil then FEvents.OnDblClick;
end;  // TfrmStorageLayout.DblClickEvent 

{-------------------------------------------------------------------------------
}
procedure TfrmStorageLayout.DeactivateEvent(Sender: TObject);
begin
  if FEvents <> nil then FEvents.OnDeactivate;
end;  // TfrmStorageLayout.DeactivateEvent 

{-------------------------------------------------------------------------------
}
procedure TfrmStorageLayout.DefinePropertyPages(DefinePropertyPage: TDefinePropertyPage);
begin
  { Define property pages here.  Property pages are defined by calling
    DefinePropertyPage with the class id of the page.  For example,
      DefinePropertyPage(Class_frmStorageLayoutPage); }
end;  // TfrmStorageLayout.DefinePropertyPages 

{-------------------------------------------------------------------------------
}
procedure TfrmStorageLayout.DestroyEvent(Sender: TObject);
begin
  if FEvents <> nil then FEvents.OnDestroy;
end;  // TfrmStorageLayout.DestroyEvent 

{-------------------------------------------------------------------------------
}
procedure TfrmStorageLayout.EventSinkChanged(const EventSink: IUnknown);
begin
  FEvents := EventSink as IfrmStorageLayoutEvents;
  inherited EventSinkChanged(EventSink);
end;  // TfrmStorageLayout.EventSinkChanged 

{-------------------------------------------------------------------------------
}
function TfrmStorageLayout.Get_Active: WordBool;
begin
  Result := Active;
end;  // TfrmStorageLayout.Get_Active 

{-------------------------------------------------------------------------------
}
function TfrmStorageLayout.Get_AlignDisabled: WordBool;
begin
  Result := AlignDisabled;
end;  // TfrmStorageLayout.Get_AlignDisabled 

{-------------------------------------------------------------------------------
}
function TfrmStorageLayout.Get_AutoScroll: WordBool;
begin
  Result := AutoScroll;
end;  // TfrmStorageLayout.Get_AutoScroll 

{-------------------------------------------------------------------------------
}
function TfrmStorageLayout.Get_AutoSize: WordBool;
begin
  Result := AutoSize;
end;  // TfrmStorageLayout.Get_AutoSize 

{-------------------------------------------------------------------------------
}
function TfrmStorageLayout.Get_AxBorderStyle: TxActiveFormBorderStyle;
begin
  Result := Ord(AxBorderStyle);
end;  // TfrmStorageLayout.Get_AxBorderStyle 

{-------------------------------------------------------------------------------
  Accessor method. 
}
function TfrmStorageLayout.Get_Caller: IRefreshTab;
begin
  Result := FCaller;
end;  // TfrmStorageLayout.Get_Caller 

{-------------------------------------------------------------------------------
}
function TfrmStorageLayout.Get_Caption: WideString;
begin
  Result := WideString(Caption);
end;  // TfrmStorageLayout.Get_Caption 

{-------------------------------------------------------------------------------
}
function TfrmStorageLayout.Get_Color: OLE_COLOR;
begin
  Result := OLE_COLOR(Color);
end;  // TfrmStorageLayout.Get_Color 

{-------------------------------------------------------------------------------
}
function TfrmStorageLayout.Get_DoubleBuffered: WordBool;
begin
  Result := DoubleBuffered;
end;  // TfrmStorageLayout.Get_DoubleBuffered 

{-------------------------------------------------------------------------------
}
function TfrmStorageLayout.Get_DropTarget: WordBool;
begin
  Result := DropTarget;
end;  // TfrmStorageLayout.Get_DropTarget 

{-------------------------------------------------------------------------------
}
function TfrmStorageLayout.Get_Enabled: WordBool;
begin
  Result := Enabled;
end;  // TfrmStorageLayout.Get_Enabled 

{-------------------------------------------------------------------------------
}
function TfrmStorageLayout.Get_Font: IFontDisp;
begin
  GetOleFont(Font, Result);
end;  // TfrmStorageLayout.Get_Font 

{-------------------------------------------------------------------------------
  Return the caption for the form to Recorder. 
}
function TfrmStorageLayout.Get_FormCaption: WideString;
begin
  Result := ResStr_StoreLayoutEditor;
end;  // TfrmStorageLayout.Get_FormCaption 

{-------------------------------------------------------------------------------
}
function TfrmStorageLayout.Get_HelpFile: WideString;
begin
  Result := WideString(HelpFile);
end;  // TfrmStorageLayout.Get_HelpFile 

{-------------------------------------------------------------------------------
}
function TfrmStorageLayout.Get_KeyPreview: WordBool;
begin
  Result := KeyPreview;
end;  // TfrmStorageLayout.Get_KeyPreview 

{-------------------------------------------------------------------------------
}
function TfrmStorageLayout.Get_PixelsPerInch: Integer;
begin
  Result := PixelsPerInch;
end;  // TfrmStorageLayout.Get_PixelsPerInch 

{-------------------------------------------------------------------------------
}
function TfrmStorageLayout.Get_PrintScale: TxPrintScale;
begin
  Result := Ord(PrintScale);
end;  // TfrmStorageLayout.Get_PrintScale 

{-------------------------------------------------------------------------------
}
function TfrmStorageLayout.Get_Scaled: WordBool;
begin
  Result := Scaled;
end;  // TfrmStorageLayout.Get_Scaled 

{-------------------------------------------------------------------------------
}
function TfrmStorageLayout.Get_ScreenSnap: WordBool;
begin
  Result := ScreenSnap;
end;  // TfrmStorageLayout.Get_ScreenSnap 

{-------------------------------------------------------------------------------
}
function TfrmStorageLayout.Get_SnapBuffer: Integer;
begin
  Result := SnapBuffer;
end;  // TfrmStorageLayout.Get_SnapBuffer 

{-------------------------------------------------------------------------------
}
function TfrmStorageLayout.Get_StoreKey: WideString;
begin
  Result := FStoreKey;
end;  // TfrmStorageLayout.Get_StoreKey 

{-------------------------------------------------------------------------------
}
function TfrmStorageLayout.Get_Visible: WordBool;
begin
  Result := Visible;
end;  // TfrmStorageLayout.Get_Visible 

{-------------------------------------------------------------------------------
}
function TfrmStorageLayout.Get_VisibleDockClientCount: Integer;
begin
  Result := VisibleDockClientCount;
end;  // TfrmStorageLayout.Get_VisibleDockClientCount 

{-------------------------------------------------------------------------------
  Set up the form and the diagram. 
}
procedure TfrmStorageLayout.Initialize;
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
  FDiagram := TStoreLayoutDiagram.Create(Self);
  with FDiagram do begin
    Parent := pnlDiagram;
    Align := alClient;
    Zoom := 1;
    StoreListBox := lbAvailableStores;
  end;
  // Correct anchoring
  lbAvailableStores.Height := pnlStoreList.Height - 25;
  FNonCurrentStores := TStringList.Create;
  OnCloseQuery := FormCloseQuery;
end;  // TfrmStorageLayout.Initialize

{-------------------------------------------------------------------------------
}
procedure TfrmStorageLayout.KeyPressEvent(Sender: TObject; var Key: Char);
var
  TempKey: SmallInt;
begin
  TempKey := Smallint(Key);
  if FEvents <> nil then FEvents.OnKeyPress(TempKey);
  Key := Char(TempKey);
end;  // TfrmStorageLayout.KeyPressEvent 

{-------------------------------------------------------------------------------
}
procedure TfrmStorageLayout.PaintEvent(Sender: TObject);
begin
  if FEvents <> nil then FEvents.OnPaint;
end;  // TfrmStorageLayout.PaintEvent 

{-------------------------------------------------------------------------------
}
procedure TfrmStorageLayout.Set_AutoScroll(Value: WordBool);
begin
  AutoScroll := Value;
end;  // TfrmStorageLayout.Set_AutoScroll 

{-------------------------------------------------------------------------------
}
procedure TfrmStorageLayout.Set_AutoSize(Value: WordBool);
begin
  AutoSize := Value;
end;  // TfrmStorageLayout.Set_AutoSize 

{-------------------------------------------------------------------------------
}
procedure TfrmStorageLayout.Set_AxBorderStyle(Value: TxActiveFormBorderStyle);
begin
  AxBorderStyle := TActiveFormBorderStyle(Value);
end;  // TfrmStorageLayout.Set_AxBorderStyle 

{-------------------------------------------------------------------------------
  Allows the caller (i.e. the store layout tab) to identify itself, so that any updates may be
      applied back to the tab.
}
procedure TfrmStorageLayout.Set_Caller(const Value: IRefreshTab);
begin
  FCaller := Value;
end;  // TfrmStorageLayout.Set_Caller 

{-------------------------------------------------------------------------------
}
procedure TfrmStorageLayout.Set_Caption(const Value: WideString);
begin
  Caption := TCaption(Value);
end;  // TfrmStorageLayout.Set_Caption 

{-------------------------------------------------------------------------------
}
procedure TfrmStorageLayout.Set_Color(Value: OLE_COLOR);
begin
  Color := TColor(Value);
end;  // TfrmStorageLayout.Set_Color 

{-------------------------------------------------------------------------------
}
procedure TfrmStorageLayout.Set_DoubleBuffered(Value: WordBool);
begin
  DoubleBuffered := Value;
end;  // TfrmStorageLayout.Set_DoubleBuffered 

{-------------------------------------------------------------------------------
}
procedure TfrmStorageLayout.Set_DropTarget(Value: WordBool);
begin
  DropTarget := Value;
end;  // TfrmStorageLayout.Set_DropTarget 

{-------------------------------------------------------------------------------
}
procedure TfrmStorageLayout.Set_Enabled(Value: WordBool);
begin
  Enabled := Value;
end;  // TfrmStorageLayout.Set_Enabled 

{-------------------------------------------------------------------------------
}
procedure TfrmStorageLayout.Set_Font(const Value: IFontDisp);
begin
  SetOleFont(Font, Value);
end;  // TfrmStorageLayout.Set_Font 

{-------------------------------------------------------------------------------
}
procedure TfrmStorageLayout.Set_HelpFile(const Value: WideString);
begin
  HelpFile := String(Value);
end;  // TfrmStorageLayout.Set_HelpFile 

{-------------------------------------------------------------------------------
}
procedure TfrmStorageLayout.Set_KeyPreview(Value: WordBool);
begin
  KeyPreview := Value;
end;  // TfrmStorageLayout.Set_KeyPreview 

{-------------------------------------------------------------------------------
}
procedure TfrmStorageLayout.Set_PixelsPerInch(Value: Integer);
begin
  PixelsPerInch := Value;
end;  // TfrmStorageLayout.Set_PixelsPerInch 

{-------------------------------------------------------------------------------
}
procedure TfrmStorageLayout.Set_PrintScale(Value: TxPrintScale);
begin
  PrintScale := TPrintScale(Value);
end;  // TfrmStorageLayout.Set_PrintScale 

{-------------------------------------------------------------------------------
}
procedure TfrmStorageLayout.Set_Scaled(Value: WordBool);
begin
  Scaled := Value;
end;  // TfrmStorageLayout.Set_Scaled 

{-------------------------------------------------------------------------------
}
procedure TfrmStorageLayout.Set_ScreenSnap(Value: WordBool);
begin
  ScreenSnap := Value;
end;  // TfrmStorageLayout.Set_ScreenSnap 

{-------------------------------------------------------------------------------
}
procedure TfrmStorageLayout.Set_SnapBuffer(Value: Integer);
begin
  SnapBuffer := Value;
end;  // TfrmStorageLayout.Set_SnapBuffer 

{-------------------------------------------------------------------------------
}
procedure TfrmStorageLayout.Set_StoreKey(const Value: WideString);
var
  lXML: String;
  i, lIdx: Integer;
  lDeletedStores: String;
begin
  FStoreKey := Value;
  lbAvailableStores.Clear;
  with dmGeneral.GetRecordset(
      'usp_Store_PossibleChildStores_Select', ['@Key', FStoreKey]) do begin
    while not EOF do begin
      lbAvailableStores.Add(
          Fields['Item_Name'].Value, VarToStr(Fields['Collection_Unit_Key'].Value));
      if Fields['IsCurrent'].Value = False then
        FNonCurrentStores.Add(VarToStr(Fields['Collection_Unit_Key'].Value));
      MoveNext;
    end; // while
  end;
  with dmGeneral.GetRecordset('usp_Store_LayoutXML_Select', ['@Key', FStoreKey]) do begin
    if RecordCount > 0 then begin
      lXML := VarToStr(Fields['Diagram_XML'].Value);
      FTimeStamp := Fields['TimeStamp'].Value;
    end;
  end;
  FDiagram.ReadXML(lXML);
  // remove used stores from the list, and remove non-existant or moved
  // stores from the diagram
  lDeletedStores := '';
  for i := FDiagram.Stores.Count-1 downto 0 do begin
    lIdx := lbAvailableStores.IDIndexOf(TStore(FDiagram.Stores[i]).Key);
    if lIdx>-1 then
      lbAvailableStores.Delete(lIdx)
    else begin
      lDeletedStores := lDeletedStores + #13#10 + TStore(FDiagram.Stores[i]).Caption;
      FDiagram.Stores.Delete(i);
    end;
  end;
  if lDeletedStores<>'' then
    ShowInformation(ResStr_SomeStoresRemovedFromDiagram + lDeletedStores);
end;  // TfrmStorageLayout.Set_StoreKey 

{-------------------------------------------------------------------------------
}
procedure TfrmStorageLayout.Set_Visible(Value: WordBool);
begin
  Visible := Value;
end;  // TfrmStorageLayout.Set_Visible 

{-------------------------------------------------------------------------------
}
procedure TfrmStorageLayout.WndProc(var Message: TMessage);
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
end;  // TfrmStorageLayout.WndProc 

{-------------------------------------------------------------------------------
}
procedure TfrmStorageLayout._Set_Font(var Value: IFontDisp);
begin
  SetOleFont(Font, Value);
end;  // TfrmStorageLayout._Set_Font 

{-------------------------------------------------------------------------------
}
procedure TfrmStorageLayout.lbAvailableStoresDblClick(Sender: TObject);
begin
  FDiagram.DoDragDrop(Sender, FDiagram.InternalWidth div 2, FDiagram.InternalHeight div 2);
end;

destructor TfrmStorageLayout.Destroy;
begin
  FNonCurrentStores.Free;
  inherited;
end;

{-------------------------------------------------------------------------------
}
procedure TfrmStorageLayout.lbAvailableStoresDrawItem(Control: TWinControl;
  Index: Integer; Rect: TRect; State: TOwnerDrawState);
begin
  with lbAvailableStores.Canvas do begin
    FillRect(Rect);
    if (FNonCurrentStores.IndexOf(lbAvailableStores.StrID[Index])<>-1) then begin
      if not (odSelected in State) then
        Font.Color := clGrayText;
      Font.Style := [fsItalic];
    end
    else
      Font.Style := [];
    TextOut(Rect.Left + 2, Rect.Top, lbAvailableStores.Items[Index]);
  end;
end;

{-------------------------------------------------------------------------------
}
procedure TfrmStorageLayout.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
var
  lResult: TModalResult;
begin
  if FDiagram.Dirty then begin
    lResult := ConfirmYesNoCancel(ResStr_SaveDiagramChanges);
    CanClose := lResult <> mrCancel;
    if lResult = mrYes then
      btnApplyClick(nil);
  end
  else
    CanClose := True;
end;

initialization
  TActiveFormFactory.Create(
    ComServer,
    TActiveFormControl,
    TfrmStorageLayout,
    Class_frmStorageLayout,
    1,
    '',
    OLEMISC_SIMPLEFRAME or OLEMISC_ACTSLIKELABEL,
    tmApartment);
end.


