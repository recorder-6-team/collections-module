{===============================================================================
  Unit:        QuickEntryForm

  Defines:     TQuickEntryForm

  Description: Form in which quick entry data is entered.

  Model:       QuickEntry

  Created:     August 2003

  Last revision information:
    $Revision: 8 $
    $Date: 18/03/14 15:53 $
    $Author: Christopherknight $

===============================================================================}
unit QuickEntryForm;

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ActiveX, AxCtrls, QuickEntry_TLB, StdVcl, Recorder2000_TLB, QuickEntryFrame,
  BaseDragFrameUnit, BaseDetailFrameUnit, XPMenu, ComUnit;

type
  {-----------------------------------------------------------------------------
    Screen allowing Quick Data Entry templates to be created. These templates 
    are then used to enter Specimen/Storage/Collection data into the system.
    This screen operates in 2 modes, Browse (default) and Edit.  In Browse mode,
    lbTemplates, btnAdd, btnEdit and btnDelete are enabled and all other 
    controls are disabled.  In Edit mode, lbTemplates, btnAdd, btnEdit and 
    btnDelete are disabled and all other controls are enabled.
    If the user closes the screen whilst editing data, then a message is 
    displayed 'Do you want to save changes to the selected template?' with Yes, 
    No, Cancel option buttons.  If the user selects Yes, then saves the changes 
    and closes the screen.  If the user clicks No, then discards the changes 
    and closes the screen.  If the user selects Cancel, then the screen does 
    not close.
  }
  TQuickEntryForm = class (TActiveForm, IQuickEntryForm, IFormCaption)
    fraQuickEntry: TfraQuickEntry;
    procedure fraQuickEntrybtnDiscardSessionClick(Sender: TObject);
  private
    FEvents: IQuickEntryFormEvents;
    FtfInitialised: Boolean;
    FXPMenu: TXPMenu;
    FComAddins: TComAddins;
    FIsNewSession : Boolean;
    procedure ActivateEvent(Sender: TObject);
    procedure ClickEvent(Sender: TObject);
    procedure CreateEvent(Sender: TObject);
    procedure DblClickEvent(Sender: TObject);
    procedure DeactivateEvent(Sender: TObject);
    procedure DestroyEvent(Sender: TObject);
    procedure KeyPressEvent(Sender: TObject; var Key: Char);
    procedure PaintEvent(Sender: TObject);
    procedure ShowEvent(Sender:TObject);
  protected
    procedure DefinePropertyPages(DefinePropertyPage: TDefinePropertyPage); 
        override;
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
    function Get_FormCaption: WideString; safecall;
    function Get_HelpFile: WideString; safecall;
    function Get_IsNewSession: WordBool; safecall;
    function Get_KeyPreview: WordBool; safecall;
    function Get_Name: WideString; safecall;
    function Get_PixelsPerInch: Integer; safecall;
    function Get_PrintScale: TxPrintScale; safecall;
    function Get_Scaled: WordBool; safecall;
    function Get_ScreenSnap: WordBool; safecall;
    function Get_SessionKey: LongInt; safecall;
    function Get_SnapBuffer: Integer; safecall;
    function Get_Visible: WordBool; safecall;
    function Get_VisibleDockClientCount: Integer; safecall;
    function Get_VisibleDockClientCount1: Integer; safecall;
    procedure Set_AutoScroll(Value: WordBool); safecall;
    procedure Set_AutoSize(Value: WordBool); safecall;
    procedure Set_AxBorderStyle(Value: TxActiveFormBorderStyle); safecall;
    procedure Set_Caption(const Value: WideString); safecall;
    procedure Set_Color(Value: OLE_COLOR); safecall;
    procedure Set_DoubleBuffered(Value: WordBool); safecall;
    procedure Set_DoubleBuffered1(Value: WordBool); safecall;
    procedure Set_DropTarget(Value: WordBool); safecall;
    procedure Set_Enabled(Value: WordBool); safecall;
    procedure Set_Font(const Value: IFontDisp); safecall;
    procedure Set_HelpFile(const Value: WideString); safecall;
    procedure Set_IsNewSession(Value: WordBool); safeCall;
    procedure Set_KeyPreview(Value: WordBool); safecall;
    procedure Set_PixelsPerInch(Value: Integer); safecall;
    procedure Set_PrintScale(Value: TxPrintScale); safecall;
    procedure Set_Scaled(Value: WordBool); safecall;
    procedure Set_ScreenSnap(Value: WordBool); safecall;
    procedure Set_SessionKey(Value: Integer); safecall;
    procedure Set_SnapBuffer(Value: Integer); safecall;
    procedure Set_Visible(Value: WordBool); safecall;
    procedure WndProc(var Message: TMessage); override;
    procedure _Set_Font(var Value: IFontDisp); safecall;
  public
    destructor Destroy; override;
    procedure Initialize; override;
  end;

implementation

uses ComObj, ComServ, GeneralData, ResourceStrings;

{$R *.DFM}

{ TQuickEntryForm }

{-==============================================================================
    TQuickEntryForm
===============================================================================}
{-------------------------------------------------------------------------------
}
destructor TQuickEntryForm.Destroy;
begin
  // Discard frame manually before we get problems with missing handles.
  fraQuickEntry.Free;
  FComAddins.Free;
  inherited Destroy;
end;  // TQuickEntryForm.Destroy 

{-------------------------------------------------------------------------------
}
procedure TQuickEntryForm.ActivateEvent(Sender: TObject);
begin
  if FEvents <> nil then FEvents.OnActivate;
end;  // TQuickEntryForm.ActivateEvent

{-------------------------------------------------------------------------------
}
procedure TQuickEntryForm.ClickEvent(Sender: TObject);
begin
  if FEvents <> nil then FEvents.OnClick;
end;  // TQuickEntryForm.ClickEvent 

{-------------------------------------------------------------------------------
}
procedure TQuickEntryForm.CreateEvent(Sender: TObject);
begin
  if FEvents <> nil then FEvents.OnCreate;
end;  // TQuickEntryForm.CreateEvent 

{-------------------------------------------------------------------------------
}
procedure TQuickEntryForm.DblClickEvent(Sender: TObject);
begin
  if FEvents <> nil then FEvents.OnDblClick;
end;  // TQuickEntryForm.DblClickEvent 

{-------------------------------------------------------------------------------
}
procedure TQuickEntryForm.DeactivateEvent(Sender: TObject);
begin
  if FEvents <> nil then FEvents.OnDeactivate;
end;  // TQuickEntryForm.DeactivateEvent 

{-------------------------------------------------------------------------------
}
procedure TQuickEntryForm.DefinePropertyPages(DefinePropertyPage: 
    TDefinePropertyPage);
begin
  // No property pages required
end;  // TQuickEntryForm.DefinePropertyPages 

{-------------------------------------------------------------------------------
}
procedure TQuickEntryForm.DestroyEvent(Sender: TObject);
begin
  if FEvents <> nil then FEvents.OnDestroy;
end;  // TQuickEntryForm.DestroyEvent 

{-------------------------------------------------------------------------------
}
procedure TQuickEntryForm.EventSinkChanged(const EventSink: IUnknown);
begin
  FEvents := EventSink as IQuickEntryFormEvents;
  inherited EventSinkChanged(EventSink);
end;  // TQuickEntryForm.EventSinkChanged 

{-------------------------------------------------------------------------------
}
function TQuickEntryForm.Get_Active: WordBool;
begin
  Result := Active;
end;  // TQuickEntryForm.Get_Active 

{-------------------------------------------------------------------------------
}
function TQuickEntryForm.Get_AlignDisabled: WordBool;
begin
  Result := AlignDisabled;
end;  // TQuickEntryForm.Get_AlignDisabled 

{-------------------------------------------------------------------------------
}
function TQuickEntryForm.Get_AutoScroll: WordBool;
begin
  Result := AutoScroll;
end;  // TQuickEntryForm.Get_AutoScroll 

{-------------------------------------------------------------------------------
}
function TQuickEntryForm.Get_AutoSize: WordBool;
begin
  Result := AutoSize;
end;  // TQuickEntryForm.Get_AutoSize 

{-------------------------------------------------------------------------------
}
function TQuickEntryForm.Get_AxBorderStyle: TxActiveFormBorderStyle;
begin
  Result := Ord(AxBorderStyle);
end;  // TQuickEntryForm.Get_AxBorderStyle 

{-------------------------------------------------------------------------------
}
function TQuickEntryForm.Get_Caption: WideString;
begin
  Result := WideString(Caption);
end;  // TQuickEntryForm.Get_Caption 

{-------------------------------------------------------------------------------
}
function TQuickEntryForm.Get_Color: OLE_COLOR;
begin
  Result := OLE_COLOR(Color);
end;  // TQuickEntryForm.Get_Color 

{-------------------------------------------------------------------------------
}
function TQuickEntryForm.Get_DoubleBuffered: WordBool;
begin
  Result := DoubleBuffered;
end;  // TQuickEntryForm.Get_DoubleBuffered 

{-------------------------------------------------------------------------------
}
function TQuickEntryForm.Get_DropTarget: WordBool;
begin
  Result := DropTarget;
end;  // TQuickEntryForm.Get_DropTarget 

{-------------------------------------------------------------------------------
}
function TQuickEntryForm.Get_Enabled: WordBool;
begin
  Result := Enabled;
end;  // TQuickEntryForm.Get_Enabled 

{-------------------------------------------------------------------------------
}
function TQuickEntryForm.Get_Font: IFontDisp;
begin
  GetOleFont(Font, Result);
end;  // TQuickEntryForm.Get_Font 

{-------------------------------------------------------------------------------
}
function TQuickEntryForm.Get_FormCaption: WideString;
begin
  if FIsNewSession then
    Result := ResStr_QuickEntry + ' - ' + fraQuickEntry.SessionName + ' (' +
        fraQuickEntry.TemplateName + ')'
  else
    Result := ResStr_QuickEntry + ' - ' + fraQuickEntry.SessionName + ' (' +
        fraQuickEntry.TemplateName + ' - ' + fraQuickEntry.LastUser + ')';
end;  // TQuickEntryForm.Get_FormCaption 

{-------------------------------------------------------------------------------
}
function TQuickEntryForm.Get_HelpFile: WideString;
begin
  Result := WideString(HelpFile);
end;  // TQuickEntryForm.Get_HelpFile 

{-------------------------------------------------------------------------------
}
function TQuickEntryForm.Get_IsNewSession: WordBool;
begin
  Result := FIsNewSession;
end;  // TQuickEntryForm.Get_IsNewSession

{-------------------------------------------------------------------------------
}
function TQuickEntryForm.Get_KeyPreview: WordBool;
begin
  Result := KeyPreview;
end;  // TQuickEntryForm.Get_KeyPreview 

{-------------------------------------------------------------------------------
}
function TQuickEntryForm.Get_Name: WideString;
begin
  result :=Caption;
end;  // TQuickEntryForm.Get_Name 

{-------------------------------------------------------------------------------
}
function TQuickEntryForm.Get_PixelsPerInch: Integer;
begin
  Result := PixelsPerInch;
end;  // TQuickEntryForm.Get_PixelsPerInch 

{-------------------------------------------------------------------------------
}
function TQuickEntryForm.Get_PrintScale: TxPrintScale;
begin
  Result := Ord(PrintScale);
end;  // TQuickEntryForm.Get_PrintScale 

{-------------------------------------------------------------------------------
}
function TQuickEntryForm.Get_Scaled: WordBool;
begin
  Result := Scaled;
end;  // TQuickEntryForm.Get_Scaled 

{-------------------------------------------------------------------------------
}
function TQuickEntryForm.Get_ScreenSnap: WordBool;
begin
  Result := ScreenSnap;
end;  // TQuickEntryForm.Get_ScreenSnap 

{-------------------------------------------------------------------------------
}
function TQuickEntryForm.Get_SessionKey: LongInt;
begin
  Result := fraQuickEntry.SessionKey;
end;  // TQuickEntryForm.Get_SessionKey 

{-------------------------------------------------------------------------------
}
function TQuickEntryForm.Get_SnapBuffer: Integer;
begin
  Result := SnapBuffer;
end;  // TQuickEntryForm.Get_SnapBuffer 

{-------------------------------------------------------------------------------
}
function TQuickEntryForm.Get_Visible: WordBool;
begin
  Result := Visible;
end;  // TQuickEntryForm.Get_Visible 

{-------------------------------------------------------------------------------
}
function TQuickEntryForm.Get_VisibleDockClientCount: Integer;
begin
  Result := VisibleDockClientCount;
end;  // TQuickEntryForm.Get_VisibleDockClientCount 

{-------------------------------------------------------------------------------
}
function TQuickEntryForm.Get_VisibleDockClientCount1: Integer;
begin
  Result := VisibleDockClientCount;
end;  // TQuickEntryForm.Get_VisibleDockClientCount1 

{-------------------------------------------------------------------------------
}
procedure TQuickEntryForm.Initialize;
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
  OnShow := ShowEvent;
  FtfInitialised := False; //because it hasn't been shown yet

  FXPMenu := TXPMenu.Create(Self);  // Self gives XPMenu to form to get rid of.
  FXPMenu.XPControls := [xcPopupMenu];
  FXPMenu.Active := True;
  fraQuickEntry.XPMenu := FXPMenu;
  FComAddins := TComAddins.Create;
  FIsNewSession := false;
end;  // TQuickEntryForm.Initialize

{-------------------------------------------------------------------------------
}
procedure TQuickEntryForm.KeyPressEvent(Sender: TObject; var Key: Char);
var
  TempKey: SmallInt;
begin
  TempKey := Smallint(Key);
  if FEvents <> nil then FEvents.OnKeyPress(TempKey);
  Key := Char(TempKey);
end;  // TQuickEntryForm.KeyPressEvent 

{-------------------------------------------------------------------------------
}
procedure TQuickEntryForm.PaintEvent(Sender: TObject);
begin
  if FEvents <> nil then FEvents.OnPaint;
end;  // TQuickEntryForm.PaintEvent 

{-------------------------------------------------------------------------------
}
procedure TQuickEntryForm.Set_AutoScroll(Value: WordBool);
begin
  AutoScroll := Value;
end;  // TQuickEntryForm.Set_AutoScroll 

{-------------------------------------------------------------------------------
}
procedure TQuickEntryForm.Set_AutoSize(Value: WordBool);
begin
  AutoSize := Value;
end;  // TQuickEntryForm.Set_AutoSize 

{-------------------------------------------------------------------------------
}
procedure TQuickEntryForm.Set_AxBorderStyle(Value: TxActiveFormBorderStyle);
begin
  AxBorderStyle := TActiveFormBorderStyle(Value);
end;  // TQuickEntryForm.Set_AxBorderStyle 

{-------------------------------------------------------------------------------
}
procedure TQuickEntryForm.Set_Caption(const Value: WideString);
begin
  Caption := TCaption(Value);
end;  // TQuickEntryForm.Set_Caption 

{-------------------------------------------------------------------------------
}
procedure TQuickEntryForm.Set_Color(Value: OLE_COLOR);
begin
  Color := TColor(Value);
end;  // TQuickEntryForm.Set_Color 

{-------------------------------------------------------------------------------
}
procedure TQuickEntryForm.Set_DoubleBuffered(Value: WordBool);
begin
  DoubleBuffered := Value;
end;  // TQuickEntryForm.Set_DoubleBuffered 

{-------------------------------------------------------------------------------
}
procedure TQuickEntryForm.Set_DoubleBuffered1(Value: WordBool);
begin
  DoubleBuffered := Value;
end;  // TQuickEntryForm.Set_DoubleBuffered1 

{-------------------------------------------------------------------------------
}
procedure TQuickEntryForm.Set_DropTarget(Value: WordBool);
begin
  DropTarget := Value;
end;  // TQuickEntryForm.Set_DropTarget 

{-------------------------------------------------------------------------------
}
procedure TQuickEntryForm.Set_Enabled(Value: WordBool);
begin
  Enabled := Value;
end;  // TQuickEntryForm.Set_Enabled 

{-------------------------------------------------------------------------------
}
procedure TQuickEntryForm.Set_Font(const Value: IFontDisp);
begin
  SetOleFont(Font, Value);
end;  // TQuickEntryForm.Set_Font 

{-------------------------------------------------------------------------------
}
procedure TQuickEntryForm.Set_HelpFile(const Value: WideString);
begin
  HelpFile := String(Value);
end;  // TQuickEntryForm.Set_HelpFile 

{-------------------------------------------------------------------------------
}
procedure TQuickEntryForm.Set_IsNewSession(Value: WordBool);
begin
  FIsNewSession := Value;
end;  // TQuickEntryForm.Set_IsNewSession

{-------------------------------------------------------------------------------
}
procedure TQuickEntryForm.Set_KeyPreview(Value: WordBool);
begin
  KeyPreview := Value;
end;  // TQuickEntryForm.Set_KeyPreview 

{-------------------------------------------------------------------------------
}
procedure TQuickEntryForm.Set_PixelsPerInch(Value: Integer);
begin
  PixelsPerInch := Value;
end;  // TQuickEntryForm.Set_PixelsPerInch 

{-------------------------------------------------------------------------------
}
procedure TQuickEntryForm.Set_PrintScale(Value: TxPrintScale);
begin
  PrintScale := TPrintScale(Value);
end;  // TQuickEntryForm.Set_PrintScale 

{-------------------------------------------------------------------------------
}
procedure TQuickEntryForm.Set_Scaled(Value: WordBool);
begin
  Scaled := Value;
end;  // TQuickEntryForm.Set_Scaled 

{-------------------------------------------------------------------------------
}
procedure TQuickEntryForm.Set_ScreenSnap(Value: WordBool);
begin
  ScreenSnap := Value;
end;  // TQuickEntryForm.Set_ScreenSnap 

{-------------------------------------------------------------------------------
  Accessor method, sets the frame's session key. 
}
procedure TQuickEntryForm.Set_SessionKey(Value: Integer);
begin
  fraQuickEntry.SessionKey := Value;
end;  // TQuickEntryForm.Set_SessionKey 

{-------------------------------------------------------------------------------
}
procedure TQuickEntryForm.Set_SnapBuffer(Value: Integer);
begin
  SnapBuffer := Value;
end;  // TQuickEntryForm.Set_SnapBuffer 

{-------------------------------------------------------------------------------
}
procedure TQuickEntryForm.Set_Visible(Value: WordBool);
begin
  Visible := Value;
end;  // TQuickEntryForm.Set_Visible 

{-------------------------------------------------------------------------------
}
procedure TQuickEntryForm.ShowEvent(Sender:TObject);
begin
  if not FtfInitialised then
    begin
      fraQuickEntry.Initialise;
      FtfInitialised := True;
    end;
end;  // TQuickEntryForm.ShowEvent 

{-------------------------------------------------------------------------------
}
procedure TQuickEntryForm.WndProc(var Message: TMessage);
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
end;  // TQuickEntryForm.WndProc 

{-------------------------------------------------------------------------------
}
procedure TQuickEntryForm._Set_Font(var Value: IFontDisp);
begin
  SetOleFont(Font, Value);
end;  // TQuickEntryForm._Set_Font 

procedure TQuickEntryForm.fraQuickEntrybtnDiscardSessionClick(
  Sender: TObject);
begin
  fraQuickEntry.btnDiscardSessionClick(Sender);
end;

initialization
  TActiveFormFactory.Create(
    ComServer,
    TActiveFormControl,
    TQuickEntryForm,
    Class_QuickEntryForm,
    4,
    '',
    OLEMISC_SIMPLEFRAME or OLEMISC_ACTSLIKELABEL,
    tmApartment);
end.






