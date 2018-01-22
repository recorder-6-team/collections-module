{===============================================================================
  Unit:        CollectionsOptionsPage

  Defines:     TCollectionsOptionsPage

  Description: Provides the options page required for the Collections module

  Model:       CollectionsModuleManager.mpb

  Created:     Nov 2003

  Last revision information:
    $Revision: 11 $
    $Date: 25/08/08 10:51 $
    $Author: Ericsalmon $

===============================================================================}
unit CollectionsOptionsPage;

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ActiveX, AxCtrls, CollectionsModuleManager_TLB, StdVcl, Recorder2000_TLB,
  StdCtrls, ComCtrls, FilterOptionsFrame, NumberMacroOptionsFrame,
  GeneralOptionsFrame;

type
  TCollectionsOptionsPage = class (TActiveForm, ICollectionsOptionsPage, IOptionsPage)
    fraFilterOptions: TfraFilterOptions;
    pcOptions: TPageControl;
    tsDomainFilters: TTabSheet;
    tsNumberGenerationMacros: TTabSheet;
    fraNumberMacroOptions: TfraNumberMacroOptions;
    tsGeneralOptions: TTabSheet;
    fraGeneralOptions: TfraGeneralOptions;
  private
    FEvents: ICollectionsOptionsPageEvents;
    procedure ActivateEvent(Sender: TObject);
    procedure ClickEvent(Sender: TObject);
    procedure CreateEvent(Sender: TObject);
    procedure DblClickEvent(Sender: TObject);
    procedure DeactivateEvent(Sender: TObject);
    procedure Default; safecall;
    procedure DestroyEvent(Sender: TObject);
    function Get_Title: WideString; safecall;
    procedure KeyPressEvent(Sender: TObject; var Key: Char);
    procedure Load; safecall;
    procedure PaintEvent(Sender: TObject);
    procedure Save; safecall;
  protected
    procedure WndProc(var Message: TMessage); override;
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
    procedure _Set_Font(var Value: IFontDisp); safecall;
  public
    procedure Initialize; override;
    destructor Destroy; override;
  end;
  
//==============================================================================
implementation

uses 
  ComObj, ComServ, ResourceStrings, ApplicationSettings, DSSDataTypes;

{$R *.DFM}

{-==============================================================================
    TCollectionsOptionsPage
===============================================================================}
{-------------------------------------------------------------------------------
}
procedure TCollectionsOptionsPage.ActivateEvent(Sender: TObject);
begin
  if FEvents <> nil then FEvents.OnActivate;
end;  // TCollectionsOptionsPage.ActivateEvent 

{-------------------------------------------------------------------------------
}
procedure TCollectionsOptionsPage.ClickEvent(Sender: TObject);
begin
  if FEvents <> nil then FEvents.OnClick;
end;  // TCollectionsOptionsPage.ClickEvent 

{-------------------------------------------------------------------------------
}
procedure TCollectionsOptionsPage.CreateEvent(Sender: TObject);
begin
  if FEvents <> nil then FEvents.OnCreate;
end;  // TCollectionsOptionsPage.CreateEvent 

{-------------------------------------------------------------------------------
}
procedure TCollectionsOptionsPage.DblClickEvent(Sender: TObject);
begin
  if FEvents <> nil then FEvents.OnDblClick;
end;  // TCollectionsOptionsPage.DblClickEvent 

{-------------------------------------------------------------------------------
}
procedure TCollectionsOptionsPage.DeactivateEvent(Sender: TObject);
begin
  if FEvents <> nil then FEvents.OnDeactivate;
end;  // TCollectionsOptionsPage.DeactivateEvent 

{-------------------------------------------------------------------------------
}
procedure TCollectionsOptionsPage.Default;
begin
end;  // TCollectionsOptionsPage.Default 

{-------------------------------------------------------------------------------
}
procedure TCollectionsOptionsPage.DefinePropertyPages(DefinePropertyPage: TDefinePropertyPage);
begin
  { Define property pages here.  Property pages are defined by calling
    DefinePropertyPage with the class id of the page.  For example,
      DefinePropertyPage(Class_CollectionsOptionsPagePage); }
end;  // TCollectionsOptionsPage.DefinePropertyPages 

{-------------------------------------------------------------------------------
}
destructor TCollectionsOptionsPage.Destroy;
begin
  FreeAndNil(fraNumberMacroOptions);
  inherited;
end;

procedure TCollectionsOptionsPage.DestroyEvent(Sender: TObject);
begin
  if FEvents <> nil then FEvents.OnDestroy;
end;  // TCollectionsOptionsPage.DestroyEvent 

{-------------------------------------------------------------------------------
}
procedure TCollectionsOptionsPage.EventSinkChanged(const EventSink: IUnknown);
begin
  FEvents := EventSink as ICollectionsOptionsPageEvents;
  inherited EventSinkChanged(EventSink);
end;  // TCollectionsOptionsPage.EventSinkChanged 

{-------------------------------------------------------------------------------
}
function TCollectionsOptionsPage.Get_Active: WordBool;
begin
  Result := Active;
end;  // TCollectionsOptionsPage.Get_Active 

{-------------------------------------------------------------------------------
}
function TCollectionsOptionsPage.Get_AlignDisabled: WordBool;
begin
  Result := AlignDisabled;
end;  // TCollectionsOptionsPage.Get_AlignDisabled 

{-------------------------------------------------------------------------------
}
function TCollectionsOptionsPage.Get_AutoScroll: WordBool;
begin
  Result := AutoScroll;
end;  // TCollectionsOptionsPage.Get_AutoScroll 

{-------------------------------------------------------------------------------
}
function TCollectionsOptionsPage.Get_AutoSize: WordBool;
begin
  Result := AutoSize;
end;  // TCollectionsOptionsPage.Get_AutoSize 

{-------------------------------------------------------------------------------
}
function TCollectionsOptionsPage.Get_AxBorderStyle: TxActiveFormBorderStyle;
begin
  Result := Ord(AxBorderStyle);
end;  // TCollectionsOptionsPage.Get_AxBorderStyle 

{-------------------------------------------------------------------------------
}
function TCollectionsOptionsPage.Get_Caption: WideString;
begin
  Result := WideString(Caption);
end;  // TCollectionsOptionsPage.Get_Caption 

{-------------------------------------------------------------------------------
}
function TCollectionsOptionsPage.Get_Color: OLE_COLOR;
begin
  Result := OLE_COLOR(Color);
end;  // TCollectionsOptionsPage.Get_Color 

{-------------------------------------------------------------------------------
}
function TCollectionsOptionsPage.Get_DoubleBuffered: WordBool;
begin
  Result := DoubleBuffered;
end;  // TCollectionsOptionsPage.Get_DoubleBuffered 

{-------------------------------------------------------------------------------
}
function TCollectionsOptionsPage.Get_DropTarget: WordBool;
begin
  Result := DropTarget;
end;  // TCollectionsOptionsPage.Get_DropTarget 

{-------------------------------------------------------------------------------
}
function TCollectionsOptionsPage.Get_Enabled: WordBool;
begin
  Result := Enabled;
end;  // TCollectionsOptionsPage.Get_Enabled 

{-------------------------------------------------------------------------------
}
function TCollectionsOptionsPage.Get_Font: IFontDisp;
begin
  GetOleFont(Font, Result);
end;  // TCollectionsOptionsPage.Get_Font 

{-------------------------------------------------------------------------------
}
function TCollectionsOptionsPage.Get_HelpFile: WideString;
begin
  Result := WideString(HelpFile);
end;  // TCollectionsOptionsPage.Get_HelpFile 

{-------------------------------------------------------------------------------
}
function TCollectionsOptionsPage.Get_KeyPreview: WordBool;
begin
  Result := KeyPreview;
end;  // TCollectionsOptionsPage.Get_KeyPreview 

{-------------------------------------------------------------------------------
}
function TCollectionsOptionsPage.Get_PixelsPerInch: Integer;
begin
  Result := PixelsPerInch;
end;  // TCollectionsOptionsPage.Get_PixelsPerInch 

{-------------------------------------------------------------------------------
}
function TCollectionsOptionsPage.Get_PrintScale: TxPrintScale;
begin
  Result := Ord(PrintScale);
end;  // TCollectionsOptionsPage.Get_PrintScale 

{-------------------------------------------------------------------------------
}
function TCollectionsOptionsPage.Get_Scaled: WordBool;
begin
  Result := Scaled;
end;  // TCollectionsOptionsPage.Get_Scaled 

{-------------------------------------------------------------------------------
}
function TCollectionsOptionsPage.Get_ScreenSnap: WordBool;
begin
  Result := ScreenSnap;
end;  // TCollectionsOptionsPage.Get_ScreenSnap 

{-------------------------------------------------------------------------------
}
function TCollectionsOptionsPage.Get_SnapBuffer: Integer;
begin
  Result := SnapBuffer;
end;  // TCollectionsOptionsPage.Get_SnapBuffer 

{-------------------------------------------------------------------------------
  Result := ResStr_Collections; 
}
function TCollectionsOptionsPage.Get_Title: WideString;
begin
  Result := ResStr_Collections;
end;  // TCollectionsOptionsPage.Get_Title

{-------------------------------------------------------------------------------
}
function TCollectionsOptionsPage.Get_Visible: WordBool;
begin
  Result := Visible;
end;  // TCollectionsOptionsPage.Get_Visible 

{-------------------------------------------------------------------------------
}
function TCollectionsOptionsPage.Get_VisibleDockClientCount: Integer;
begin
  Result := VisibleDockClientCount;
end;  // TCollectionsOptionsPage.Get_VisibleDockClientCount 

{-------------------------------------------------------------------------------
}
procedure TCollectionsOptionsPage.Initialize;
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
end;  // TCollectionsOptionsPage.Initialize 

{-------------------------------------------------------------------------------
}
procedure TCollectionsOptionsPage.KeyPressEvent(Sender: TObject; var Key: Char);
var
  TempKey: SmallInt;
begin
  TempKey := Smallint(Key);
  if FEvents <> nil then FEvents.OnKeyPress(TempKey);
  Key := Char(TempKey);
end;  // TCollectionsOptionsPage.KeyPressEvent 

{-------------------------------------------------------------------------------
}
procedure TCollectionsOptionsPage.Load;
begin
  pcOptions.ActivePageIndex := 0;
  fraFilterOptions.Load;
  fraNumberMacroOptions.Load;
  fraGeneralOptions.Load;
end;  // TCollectionsOptionsPage.Load

{-------------------------------------------------------------------------------
}
procedure TCollectionsOptionsPage.PaintEvent(Sender: TObject);
begin
  if FEvents <> nil then FEvents.OnPaint;
end;  // TCollectionsOptionsPage.PaintEvent

{-------------------------------------------------------------------------------
}
procedure TCollectionsOptionsPage.Save;
begin
  fraFilterOptions.Save;
  if TUserAccessLevel(AppSettings.UserAccessLevel) > ualReadOnly then
    fraNumberMacroOptions.Save;
  fraGeneralOptions.Save;
end;  // TCollectionsOptionsPage.Save 

{-------------------------------------------------------------------------------
}
procedure TCollectionsOptionsPage.Set_AutoScroll(Value: WordBool);
begin
  AutoScroll := Value;
end;  // TCollectionsOptionsPage.Set_AutoScroll 

{-------------------------------------------------------------------------------
}
procedure TCollectionsOptionsPage.Set_AutoSize(Value: WordBool);
begin
  AutoSize := Value;
end;  // TCollectionsOptionsPage.Set_AutoSize 

{-------------------------------------------------------------------------------
}
procedure TCollectionsOptionsPage.Set_AxBorderStyle(Value: TxActiveFormBorderStyle);
begin
  AxBorderStyle := TActiveFormBorderStyle(Value);
end;  // TCollectionsOptionsPage.Set_AxBorderStyle 

{-------------------------------------------------------------------------------
}
procedure TCollectionsOptionsPage.Set_Caption(const Value: WideString);
begin
  Caption := TCaption(Value);
end;  // TCollectionsOptionsPage.Set_Caption 

{-------------------------------------------------------------------------------
}
procedure TCollectionsOptionsPage.Set_Color(Value: OLE_COLOR);
begin
  Color := TColor(Value);
end;  // TCollectionsOptionsPage.Set_Color 

{-------------------------------------------------------------------------------
}
procedure TCollectionsOptionsPage.Set_DoubleBuffered(Value: WordBool);
begin
  DoubleBuffered := Value;
end;  // TCollectionsOptionsPage.Set_DoubleBuffered 

{-------------------------------------------------------------------------------
}
procedure TCollectionsOptionsPage.Set_DropTarget(Value: WordBool);
begin
  DropTarget := Value;
end;  // TCollectionsOptionsPage.Set_DropTarget 

{-------------------------------------------------------------------------------
}
procedure TCollectionsOptionsPage.Set_Enabled(Value: WordBool);
begin
  Enabled := Value;
end;  // TCollectionsOptionsPage.Set_Enabled 

{-------------------------------------------------------------------------------
}
procedure TCollectionsOptionsPage.Set_Font(const Value: IFontDisp);
begin
  SetOleFont(Font, Value);
end;  // TCollectionsOptionsPage.Set_Font 

{-------------------------------------------------------------------------------
}
procedure TCollectionsOptionsPage.Set_HelpFile(const Value: WideString);
begin
  HelpFile := String(Value);
end;  // TCollectionsOptionsPage.Set_HelpFile 

{-------------------------------------------------------------------------------
}
procedure TCollectionsOptionsPage.Set_KeyPreview(Value: WordBool);
begin
  KeyPreview := Value;
end;  // TCollectionsOptionsPage.Set_KeyPreview 

{-------------------------------------------------------------------------------
}
procedure TCollectionsOptionsPage.Set_PixelsPerInch(Value: Integer);
begin
  PixelsPerInch := Value;
end;  // TCollectionsOptionsPage.Set_PixelsPerInch 

{-------------------------------------------------------------------------------
}
procedure TCollectionsOptionsPage.Set_PrintScale(Value: TxPrintScale);
begin
  PrintScale := TPrintScale(Value);
end;  // TCollectionsOptionsPage.Set_PrintScale 

{-------------------------------------------------------------------------------
}
procedure TCollectionsOptionsPage.Set_Scaled(Value: WordBool);
begin
  Scaled := Value;
end;  // TCollectionsOptionsPage.Set_Scaled 

{-------------------------------------------------------------------------------
}
procedure TCollectionsOptionsPage.Set_ScreenSnap(Value: WordBool);
begin
  ScreenSnap := Value;
end;  // TCollectionsOptionsPage.Set_ScreenSnap 

{-------------------------------------------------------------------------------
}
procedure TCollectionsOptionsPage.Set_SnapBuffer(Value: Integer);
begin
  SnapBuffer := Value;
end;  // TCollectionsOptionsPage.Set_SnapBuffer 

{-------------------------------------------------------------------------------
}
procedure TCollectionsOptionsPage.Set_Visible(Value: WordBool);
begin
  Visible := Value;
end;  // TCollectionsOptionsPage.Set_Visible 

{-------------------------------------------------------------------------------
}
procedure TCollectionsOptionsPage._Set_Font(var Value: IFontDisp);
begin
  SetOleFont(Font, Value);
end;  // TCollectionsOptionsPage._Set_Font

{-------------------------------------------------------------------------------
}
procedure TCollectionsOptionsPage.WndProc(var Message: TMessage);
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

initialization
  TActiveFormFactory.Create(
    ComServer,
    TActiveFormControl,
    TCollectionsOptionsPage,
    Class_CollectionsOptionsPage,
    1,
    '',
    OLEMISC_SIMPLEFRAME or OLEMISC_ACTSLIKELABEL,
    tmApartment);
end.


