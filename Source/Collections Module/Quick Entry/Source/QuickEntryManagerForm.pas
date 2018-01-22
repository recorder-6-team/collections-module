{===============================================================================
  Unit:        QuickEntryManagerForm

  Defines:     TfrmQuickEntryManager

  Description: Main form for the Quick Entry Manager

  Created:     Sept 2003

  Last revision information:
    $Revision: 6 $
    $Date: 19/06/07 10:40 $
    $Author: Davidkelly $

===============================================================================}
unit QuickEntryManagerForm;

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ActiveX, AxCtrls, QuickEntry_TLB, StdVcl, QuickEntryManagerFrame,
  Recorder2000_TLB, BaseDragFrameUnit, BaseDetailFrameUnit;

type
  {-----------------------------------------------------------------------------
    Template maintenance screen for the Quick Entry System.
  }
  TfrmQuickEntryManager = class (TActiveForm, IfrmQuickEntryManager, 
      IFormCaption)
    fraQuickEntryManager: TfraQuickEntryManager;
  private
    FEvents: IfrmQuickEntryManagerEvents;
    FtfInitialised: Boolean;
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
    function CloseQuery: Boolean; override;
    procedure Initialize; override;
  end;
  
implementation

uses ComObj, ComServ, GeneralData, ResourceStrings;

{$R *.DFM}

{ TTfrmQuickEntryManager }

{-==============================================================================
    TfrmQuickEntryManager
===============================================================================}
{-------------------------------------------------------------------------------
  Object destruction.  Free the frame first to avoid windows handle problems. 
}
destructor TfrmQuickEntryManager.Destroy;
begin
  FreeAndNil(fraQuickEntryManager);
  
  inherited Destroy;
end;  // TfrmQuickEntryManager.Destroy 

{-------------------------------------------------------------------------------
}
procedure TfrmQuickEntryManager.ActivateEvent(Sender: TObject);
begin
  if FEvents <> nil then FEvents.OnActivate;
end;  // TfrmQuickEntryManager.ActivateEvent 

{-------------------------------------------------------------------------------
}
procedure TfrmQuickEntryManager.ClickEvent(Sender: TObject);
begin
  if FEvents <> nil then FEvents.OnClick;
end;  // TfrmQuickEntryManager.ClickEvent 

{-------------------------------------------------------------------------------
  Override CloseQuery to prevent the user closing the form with unsaved changes 
      without being warned first. 
}
function TfrmQuickEntryManager.CloseQuery: Boolean;
begin
  // default - allow close operation
  Result := True;
  if fraQuickEntryManager.EditMode = emEdit then begin
    case MessageDlg(Format(ResStr_ConfirmSaveOutstandingChanges, 
        [ResStr_Template]),
        mtConfirmation, [mbYes, mbNo, mbCancel], 0) of
      mrYes:
      begin
        try
          fraQuickEntryManager.btnSave.Click;
        except
          on Exception do
            Result := False;
        end; // try
      end;
      mrNo:     fraQuickEntryManager.btnCancel.Click;
      mrCancel: Result := False;
    end;
  end;
end;  // TfrmQuickEntryManager.CloseQuery 

{-------------------------------------------------------------------------------
}
procedure TfrmQuickEntryManager.CreateEvent(Sender: TObject);
begin
  if FEvents <> nil then FEvents.OnCreate;
end;  // TfrmQuickEntryManager.CreateEvent 

{-------------------------------------------------------------------------------
}
procedure TfrmQuickEntryManager.DblClickEvent(Sender: TObject);
begin
  if FEvents <> nil then FEvents.OnDblClick;
end;  // TfrmQuickEntryManager.DblClickEvent 

{-------------------------------------------------------------------------------
}
procedure TfrmQuickEntryManager.DeactivateEvent(Sender: TObject);
begin
  if FEvents <> nil then FEvents.OnDeactivate;
end;  // TfrmQuickEntryManager.DeactivateEvent 

{-------------------------------------------------------------------------------
}
procedure TfrmQuickEntryManager.DefinePropertyPages(DefinePropertyPage: 
    TDefinePropertyPage);
begin
  // No property pages required.
end;  // TfrmQuickEntryManager.DefinePropertyPages 

{-------------------------------------------------------------------------------
}
procedure TfrmQuickEntryManager.DestroyEvent(Sender: TObject);
begin
  if FEvents <> nil then FEvents.OnDestroy;
end;  // TfrmQuickEntryManager.DestroyEvent 

{-------------------------------------------------------------------------------
}
procedure TfrmQuickEntryManager.EventSinkChanged(const EventSink: IUnknown);
begin
  FEvents := EventSink as IfrmQuickEntryManagerEvents;
  inherited EventSinkChanged(EventSink);
end;  // TfrmQuickEntryManager.EventSinkChanged 

{-------------------------------------------------------------------------------
}
function TfrmQuickEntryManager.Get_Active: WordBool;
begin
  Result := Active;
end;  // TfrmQuickEntryManager.Get_Active 

{-------------------------------------------------------------------------------
}
function TfrmQuickEntryManager.Get_AlignDisabled: WordBool;
begin
  Result := AlignDisabled;
end;  // TfrmQuickEntryManager.Get_AlignDisabled 

{-------------------------------------------------------------------------------
}
function TfrmQuickEntryManager.Get_AutoScroll: WordBool;
begin
  Result := AutoScroll;
end;  // TfrmQuickEntryManager.Get_AutoScroll 

{-------------------------------------------------------------------------------
}
function TfrmQuickEntryManager.Get_AutoSize: WordBool;
begin
  Result := AutoSize;
end;  // TfrmQuickEntryManager.Get_AutoSize 

{-------------------------------------------------------------------------------
}
function TfrmQuickEntryManager.Get_AxBorderStyle: TxActiveFormBorderStyle;
begin
  Result := Ord(AxBorderStyle);
end;  // TfrmQuickEntryManager.Get_AxBorderStyle 

{-------------------------------------------------------------------------------
}
function TfrmQuickEntryManager.Get_Caption: WideString;
begin
  Result := WideString(Caption);
end;  // TfrmQuickEntryManager.Get_Caption 

{-------------------------------------------------------------------------------
}
function TfrmQuickEntryManager.Get_Color: OLE_COLOR;
begin
  Result := OLE_COLOR(Color);
end;  // TfrmQuickEntryManager.Get_Color 

{-------------------------------------------------------------------------------
}
function TfrmQuickEntryManager.Get_DoubleBuffered: WordBool;
begin
  Result := DoubleBuffered;
end;  // TfrmQuickEntryManager.Get_DoubleBuffered 

{-------------------------------------------------------------------------------
}
function TfrmQuickEntryManager.Get_DropTarget: WordBool;
begin
  Result := DropTarget;
end;  // TfrmQuickEntryManager.Get_DropTarget 

{-------------------------------------------------------------------------------
}
function TfrmQuickEntryManager.Get_Enabled: WordBool;
begin
  Result := Enabled;
end;  // TfrmQuickEntryManager.Get_Enabled 

{-------------------------------------------------------------------------------
}
function TfrmQuickEntryManager.Get_Font: IFontDisp;
begin
  GetOleFont(Font, Result);
end;  // TfrmQuickEntryManager.Get_Font 

{-------------------------------------------------------------------------------
  Retrieve the caption that should be displayed on Recorder's container form. 
}
function TfrmQuickEntryManager.Get_FormCaption: WideString;
begin
  Result := StringReplace(ResStr_MnuQuickEntryManager, '&', '', [rfReplaceAll]);
end;  // TfrmQuickEntryManager.Get_FormCaption 

{-------------------------------------------------------------------------------
}
function TfrmQuickEntryManager.Get_HelpFile: WideString;
begin
  Result := WideString(HelpFile);
end;  // TfrmQuickEntryManager.Get_HelpFile 

{-------------------------------------------------------------------------------
}
function TfrmQuickEntryManager.Get_KeyPreview: WordBool;
begin
  Result := KeyPreview;
end;  // TfrmQuickEntryManager.Get_KeyPreview 

{-------------------------------------------------------------------------------
}
function TfrmQuickEntryManager.Get_PixelsPerInch: Integer;
begin
  Result := PixelsPerInch;
end;  // TfrmQuickEntryManager.Get_PixelsPerInch 

{-------------------------------------------------------------------------------
}
function TfrmQuickEntryManager.Get_PrintScale: TxPrintScale;
begin
  Result := Ord(PrintScale);
end;  // TfrmQuickEntryManager.Get_PrintScale 

{-------------------------------------------------------------------------------
}
function TfrmQuickEntryManager.Get_Scaled: WordBool;
begin
  Result := Scaled;
end;  // TfrmQuickEntryManager.Get_Scaled 

{-------------------------------------------------------------------------------
}
function TfrmQuickEntryManager.Get_ScreenSnap: WordBool;
begin
  Result := ScreenSnap;
end;  // TfrmQuickEntryManager.Get_ScreenSnap 

{-------------------------------------------------------------------------------
}
function TfrmQuickEntryManager.Get_SnapBuffer: Integer;
begin
  Result := SnapBuffer;
end;  // TfrmQuickEntryManager.Get_SnapBuffer 

{-------------------------------------------------------------------------------
}
function TfrmQuickEntryManager.Get_Visible: WordBool;
begin
  Result := Visible;
end;  // TfrmQuickEntryManager.Get_Visible 

{-------------------------------------------------------------------------------
}
function TfrmQuickEntryManager.Get_VisibleDockClientCount: Integer;
begin
  Result := VisibleDockClientCount;
end;  // TfrmQuickEntryManager.Get_VisibleDockClientCount 

{-------------------------------------------------------------------------------
  ActiveX instance initialisation. 
}
procedure TfrmQuickEntryManager.Initialize;
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
  FtfInitialised := false;//this means that fraQuickEntryManager hasn't been
                          //initialised
end;  // TfrmQuickEntryManager.Initialize 

{-------------------------------------------------------------------------------
}
procedure TfrmQuickEntryManager.KeyPressEvent(Sender: TObject; var Key: Char);
var
  TempKey: SmallInt;
begin
  TempKey := Smallint(Key);
  if FEvents <> nil then FEvents.OnKeyPress(TempKey);
  Key := Char(TempKey);
end;  // TfrmQuickEntryManager.KeyPressEvent 

{-------------------------------------------------------------------------------
}
procedure TfrmQuickEntryManager.PaintEvent(Sender: TObject);
begin
  if FEvents <> nil then FEvents.OnPaint;
end;  // TfrmQuickEntryManager.PaintEvent 

{-------------------------------------------------------------------------------
}
procedure TfrmQuickEntryManager.Set_AutoScroll(Value: WordBool);
begin
  AutoScroll := Value;
end;  // TfrmQuickEntryManager.Set_AutoScroll 

{-------------------------------------------------------------------------------
}
procedure TfrmQuickEntryManager.Set_AutoSize(Value: WordBool);
begin
  AutoSize := Value;
end;  // TfrmQuickEntryManager.Set_AutoSize 

{-------------------------------------------------------------------------------
}
procedure TfrmQuickEntryManager.Set_AxBorderStyle(Value: 
    TxActiveFormBorderStyle);
begin
  AxBorderStyle := TActiveFormBorderStyle(Value);
end;  // TfrmQuickEntryManager.Set_AxBorderStyle 

{-------------------------------------------------------------------------------
}
procedure TfrmQuickEntryManager.Set_Caption(const Value: WideString);
begin
  Caption := TCaption(Value);
end;  // TfrmQuickEntryManager.Set_Caption 

{-------------------------------------------------------------------------------
}
procedure TfrmQuickEntryManager.Set_Color(Value: OLE_COLOR);
begin
  Color := TColor(Value);
end;  // TfrmQuickEntryManager.Set_Color 

{-------------------------------------------------------------------------------
}
procedure TfrmQuickEntryManager.Set_DoubleBuffered(Value: WordBool);
begin
  DoubleBuffered := Value;
end;  // TfrmQuickEntryManager.Set_DoubleBuffered 

{-------------------------------------------------------------------------------
}
procedure TfrmQuickEntryManager.Set_DropTarget(Value: WordBool);
begin
  DropTarget := Value;
end;  // TfrmQuickEntryManager.Set_DropTarget 

{-------------------------------------------------------------------------------
}
procedure TfrmQuickEntryManager.Set_Enabled(Value: WordBool);
begin
  Enabled := Value;
end;  // TfrmQuickEntryManager.Set_Enabled 

{-------------------------------------------------------------------------------
}
procedure TfrmQuickEntryManager.Set_Font(const Value: IFontDisp);
begin
  SetOleFont(Font, Value);
end;  // TfrmQuickEntryManager.Set_Font 

{-------------------------------------------------------------------------------
}
procedure TfrmQuickEntryManager.Set_HelpFile(const Value: WideString);
begin
  HelpFile := String(Value);
end;  // TfrmQuickEntryManager.Set_HelpFile 

{-------------------------------------------------------------------------------
}
procedure TfrmQuickEntryManager.Set_KeyPreview(Value: WordBool);
begin
  KeyPreview := Value;
end;  // TfrmQuickEntryManager.Set_KeyPreview 

{-------------------------------------------------------------------------------
}
procedure TfrmQuickEntryManager.Set_PixelsPerInch(Value: Integer);
begin
  PixelsPerInch := Value;
end;  // TfrmQuickEntryManager.Set_PixelsPerInch 

{-------------------------------------------------------------------------------
}
procedure TfrmQuickEntryManager.Set_PrintScale(Value: TxPrintScale);
begin
  PrintScale := TPrintScale(Value);
end;  // TfrmQuickEntryManager.Set_PrintScale 

{-------------------------------------------------------------------------------
}
procedure TfrmQuickEntryManager.Set_Scaled(Value: WordBool);
begin
  Scaled := Value;
end;  // TfrmQuickEntryManager.Set_Scaled 

{-------------------------------------------------------------------------------
}
procedure TfrmQuickEntryManager.Set_ScreenSnap(Value: WordBool);
begin
  ScreenSnap := Value;
end;  // TfrmQuickEntryManager.Set_ScreenSnap 

{-------------------------------------------------------------------------------
}
procedure TfrmQuickEntryManager.Set_SnapBuffer(Value: Integer);
begin
  SnapBuffer := Value;
end;  // TfrmQuickEntryManager.Set_SnapBuffer 

{-------------------------------------------------------------------------------
}
procedure TfrmQuickEntryManager.Set_Visible(Value: WordBool);
begin
  Visible := Value;
end;  // TfrmQuickEntryManager.Set_Visible 

{-------------------------------------------------------------------------------
  Ensure that the frame isn't filled with data until the form is actually 
      shown. 
}
procedure TfrmQuickEntryManager.ShowEvent(Sender:TObject);
begin
  if not FtfInitialised then
  begin
    fraQuickEntryManager.Initialise;
    FtfInitialised := True;
  end;
end;  // TfrmQuickEntryManager.ShowEvent 

{-------------------------------------------------------------------------------
  Override WndProc to allow the ActiveX to receive navigation key presses. 
}
procedure TfrmQuickEntryManager.WndProc(var Message: TMessage);
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
end;  // TfrmQuickEntryManager.WndProc 

{-------------------------------------------------------------------------------
}
procedure TfrmQuickEntryManager._Set_Font(var Value: IFontDisp);
begin
  SetOleFont(Font, Value);
end;  // TfrmQuickEntryManager._Set_Font

initialization

  TActiveFormFactory.Create(
    ComServer,
    TActiveFormControl,
    TfrmQuickEntryManager,
    Class_frmQuickEntryManager,
    5,
    '',
    OLEMISC_SIMPLEFRAME or OLEMISC_ACTSLIKELABEL,
    tmApartment);
end.

