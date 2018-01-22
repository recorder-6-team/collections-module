{===============================================================================
  Unit:        DescriptorTab

  Defines:     TDescriptorTab

  Description: Additional tab on Location Feature screen.

  Model:

  Created:     August 2004

  Last revision information:
    $Revision: 5 $
    $Date: 19/12/06 14:36 $
    $Author: Ericsalmon $

===============================================================================}

unit DescriptorTab;

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ActiveX, AxCtrls, Occurrences_TLB, StdVcl, Recorder2000_TLB, BaseDragFrameUnit,
  BaseDetailFrameUnit, BaseTabSheetFrameUnit, FrameDescriptors, DataTypes, Variants,
  LuxembourgConstants, ExceptionForm, DSSDataTypes;

type
  TDescriptorTab = class(TActiveForm, IDescriptorTab, IRecorderAddin, IAdditionalPage,
      IRecorderFormEvents, IAdditionalProperties)
    fraDescriptors: TfraDescriptors;
  private
    FEvents: IDescriptorTabEvents;
    FMasterKey: String;
    procedure ActivateEvent(Sender: TObject);
    procedure ClickEvent(Sender: TObject);
    procedure CreateEvent(Sender: TObject);
    procedure DblClickEvent(Sender: TObject);
    procedure DeactivateEvent(Sender: TObject);
    procedure DestroyEvent(Sender: TObject);
    procedure KeyPressEvent(Sender: TObject; var Key: Char);
    procedure PaintEvent(Sender: TObject);
    // IRecorderAddin
    function Get_Description: WideString; safecall;
    function Get_ImageFileName: WideString; safecall;
    function Get_Name: WideString; safecall;
    procedure Install(const iInstalledFilePath: WideString); safecall;
    // IAdditionalPage
    function Get_CurrentKey: WideString; safecall;
    function Get_CurrentTable: WideString; safecall;
    function Get_Form: WideString; safecall;
    function Get_PageCaption: WideString; safecall;
    procedure Set_CurrentKey(const Value: WideString); safecall;
    procedure Set_CurrentTable(const Value: WideString); safecall;
    // IRecorderFormEvents
    function CheckCanSave: WordBool; safecall;
    procedure DoAdd; safecall;
    procedure DoCancel; safecall;
    procedure DoDelete; safecall;
    procedure DoEditMode; safecall;
    procedure DoItemChange(const iTableName: WideString; const iKeyValue: WideString); safecall;
    procedure DoSave; safecall;
    function Get_FormName: WideString; safecall;
    procedure SetForm(const iForm: IRecorderForm); safecall;
    // IAdditionalProperties
    function GetProperty(const AName: string): Variant;
  protected
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
    procedure WndProc(var Message: TMessage); override;
  public
    destructor Destroy; override;
    procedure Initialize; override;
  end;
  
//==============================================================================
implementation

uses
  ComObj, ComServ;

{$R *.DFM}

{-==============================================================================
    TDescriptorTab
===============================================================================}
{-------------------------------------------------------------------------------
}
destructor TDescriptorTab.Destroy;
begin
  fraDescriptors.Free;
  
  inherited;
end;  // TDescriptorTab.Destroy 

{-------------------------------------------------------------------------------
}
procedure TDescriptorTab.ActivateEvent(Sender: TObject);
begin
  if FEvents <> nil then FEvents.OnActivate;
end;  // TDescriptorTab.ActivateEvent 

{-------------------------------------------------------------------------------
}
procedure TDescriptorTab.ClickEvent(Sender: TObject);
begin
  if FEvents <> nil then FEvents.OnClick;
end;  // TDescriptorTab.ClickEvent

{-------------------------------------------------------------------------------
}
procedure TDescriptorTab.CreateEvent(Sender: TObject);
begin
  if FEvents <> nil then FEvents.OnCreate;
end;  // TDescriptorTab.CreateEvent

{-------------------------------------------------------------------------------
}
procedure TDescriptorTab.DblClickEvent(Sender: TObject);
begin
  if FEvents <> nil then FEvents.OnDblClick;
end;  // TDescriptorTab.DblClickEvent 

{-------------------------------------------------------------------------------
}
procedure TDescriptorTab.DeactivateEvent(Sender: TObject);
begin
  if FEvents <> nil then FEvents.OnDeactivate;
end;  // TDescriptorTab.DeactivateEvent 

{-------------------------------------------------------------------------------
}
procedure TDescriptorTab.DefinePropertyPages(DefinePropertyPage: TDefinePropertyPage);
begin
  { Define property pages here.  Property pages are defined by calling
    DefinePropertyPage with the class id of the page.  For example,
      DefinePropertyPage(Class_DescriptorTabPage); }
end;  // TDescriptorTab.DefinePropertyPages

{-------------------------------------------------------------------------------
}
procedure TDescriptorTab.DestroyEvent(Sender: TObject);
begin
  if FEvents <> nil then FEvents.OnDestroy;
end;  // TDescriptorTab.DestroyEvent

{-------------------------------------------------------------------------------
}
procedure TDescriptorTab.EventSinkChanged(const EventSink: IUnknown);
begin
  FEvents := EventSink as IDescriptorTabEvents;
  inherited EventSinkChanged(EventSink);
end;  // TDescriptorTab.EventSinkChanged 

{-------------------------------------------------------------------------------
}
function TDescriptorTab.Get_Active: WordBool;
begin
  Result := Active;
end;  // TDescriptorTab.Get_Active 

{-------------------------------------------------------------------------------
}
function TDescriptorTab.Get_AlignDisabled: WordBool;
begin
  Result := AlignDisabled;
end;  // TDescriptorTab.Get_AlignDisabled 

{-------------------------------------------------------------------------------
}
function TDescriptorTab.Get_AutoScroll: WordBool;
begin
  Result := AutoScroll;
end;  // TDescriptorTab.Get_AutoScroll 

{-------------------------------------------------------------------------------
}
function TDescriptorTab.Get_AutoSize: WordBool;
begin
  Result := AutoSize;
end;  // TDescriptorTab.Get_AutoSize 

{-------------------------------------------------------------------------------
}
function TDescriptorTab.Get_AxBorderStyle: TxActiveFormBorderStyle;
begin
  Result := Ord(AxBorderStyle);
end;  // TDescriptorTab.Get_AxBorderStyle 

{-------------------------------------------------------------------------------
}
function TDescriptorTab.Get_Caption: WideString;
begin
  Result := WideString(Caption);
end;  // TDescriptorTab.Get_Caption 

{-------------------------------------------------------------------------------
}
function TDescriptorTab.Get_Color: OLE_COLOR;
begin
  Result := OLE_COLOR(Color);
end;  // TDescriptorTab.Get_Color 

{-------------------------------------------------------------------------------
}
function TDescriptorTab.Get_DoubleBuffered: WordBool;
begin
  Result := DoubleBuffered;
end;  // TDescriptorTab.Get_DoubleBuffered

{-------------------------------------------------------------------------------
}
function TDescriptorTab.Get_DropTarget: WordBool;
begin
  Result := DropTarget;
end;  // TDescriptorTab.Get_DropTarget

{-------------------------------------------------------------------------------
}
function TDescriptorTab.Get_Enabled: WordBool;
begin
  Result := Enabled;
end;  // TDescriptorTab.Get_Enabled

{-------------------------------------------------------------------------------
}
function TDescriptorTab.Get_Font: IFontDisp;
begin
  GetOleFont(Font, Result);
end;  // TDescriptorTab.Get_Font

{-------------------------------------------------------------------------------
}
function TDescriptorTab.Get_HelpFile: WideString;
begin
  Result := WideString(HelpFile);
end;  // TDescriptorTab.Get_HelpFile

{-------------------------------------------------------------------------------
}
function TDescriptorTab.Get_KeyPreview: WordBool;
begin
  Result := KeyPreview;
end;  // TDescriptorTab.Get_KeyPreview

{-------------------------------------------------------------------------------
}
function TDescriptorTab.Get_PixelsPerInch: Integer;
begin
  Result := PixelsPerInch;
end;  // TDescriptorTab.Get_PixelsPerInch

{-------------------------------------------------------------------------------
}
function TDescriptorTab.Get_PrintScale: TxPrintScale;
begin
  Result := Ord(PrintScale);
end;  // TDescriptorTab.Get_PrintScale

{-------------------------------------------------------------------------------
}
function TDescriptorTab.Get_Scaled: WordBool;
begin
  Result := Scaled;
end;  // TDescriptorTab.Get_Scaled

{-------------------------------------------------------------------------------
}
function TDescriptorTab.Get_ScreenSnap: WordBool;
begin
  Result := ScreenSnap;
end;  // TDescriptorTab.Get_ScreenSnap

{-------------------------------------------------------------------------------
}
function TDescriptorTab.Get_SnapBuffer: Integer;
begin
  Result := SnapBuffer;
end;  // TDescriptorTab.Get_SnapBuffer

{-------------------------------------------------------------------------------
}
function TDescriptorTab.Get_Visible: WordBool;
begin
  Result := Visible;
end;  // TDescriptorTab.Get_Visible

{-------------------------------------------------------------------------------
}
function TDescriptorTab.Get_VisibleDockClientCount: Integer;
begin
  Result := VisibleDockClientCount;
end;  // TDescriptorTab.Get_VisibleDockClientCount 

{-------------------------------------------------------------------------------
}
procedure TDescriptorTab.Initialize;
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

  fraDescriptors.SetAdditionalProperties(Self);
  fraDescriptors.MasterFrameType := mftLocationFeature;
  fraDescriptors.EditMode := emBrowse;
end;  // TDescriptorTab.Initialize

{-------------------------------------------------------------------------------
}
procedure TDescriptorTab.KeyPressEvent(Sender: TObject; var Key: Char);
var
  TempKey: SmallInt;
begin
  TempKey := Smallint(Key);
  if FEvents <> nil then FEvents.OnKeyPress(TempKey);
  Key := Char(TempKey);
end;  // TDescriptorTab.KeyPressEvent 

{-------------------------------------------------------------------------------
}
procedure TDescriptorTab.PaintEvent(Sender: TObject);
begin
  if FEvents <> nil then FEvents.OnPaint;
end;  // TDescriptorTab.PaintEvent 

{-------------------------------------------------------------------------------
}
procedure TDescriptorTab.Set_AutoScroll(Value: WordBool);
begin
  AutoScroll := Value;
end;  // TDescriptorTab.Set_AutoScroll 

{-------------------------------------------------------------------------------
}
procedure TDescriptorTab.Set_AutoSize(Value: WordBool);
begin
  AutoSize := Value;
end;  // TDescriptorTab.Set_AutoSize

{-------------------------------------------------------------------------------
}
procedure TDescriptorTab.Set_AxBorderStyle(Value: TxActiveFormBorderStyle);
begin
  AxBorderStyle := TActiveFormBorderStyle(Value);
end;  // TDescriptorTab.Set_AxBorderStyle 

{-------------------------------------------------------------------------------
}
procedure TDescriptorTab.Set_Caption(const Value: WideString);
begin
  Caption := TCaption(Value);
end;  // TDescriptorTab.Set_Caption 

{-------------------------------------------------------------------------------
}
procedure TDescriptorTab.Set_Color(Value: OLE_COLOR);
begin
  Color := TColor(Value);
end;  // TDescriptorTab.Set_Color 

{-------------------------------------------------------------------------------
}
procedure TDescriptorTab.Set_DoubleBuffered(Value: WordBool);
begin
  DoubleBuffered := Value;
end;  // TDescriptorTab.Set_DoubleBuffered 

{-------------------------------------------------------------------------------
}
procedure TDescriptorTab.Set_DropTarget(Value: WordBool);
begin
  DropTarget := Value;
end;  // TDescriptorTab.Set_DropTarget 

{-------------------------------------------------------------------------------
}
procedure TDescriptorTab.Set_Enabled(Value: WordBool);
begin
  Enabled := Value;
end;  // TDescriptorTab.Set_Enabled 

{-------------------------------------------------------------------------------
}
procedure TDescriptorTab.Set_Font(const Value: IFontDisp);
begin
  SetOleFont(Font, Value);
end;  // TDescriptorTab.Set_Font 

{-------------------------------------------------------------------------------
}
procedure TDescriptorTab.Set_HelpFile(const Value: WideString);
begin
  HelpFile := String(Value);
end;  // TDescriptorTab.Set_HelpFile 

{-------------------------------------------------------------------------------
}
procedure TDescriptorTab.Set_KeyPreview(Value: WordBool);
begin
  KeyPreview := Value;
end;  // TDescriptorTab.Set_KeyPreview 

{-------------------------------------------------------------------------------
}
procedure TDescriptorTab.Set_PixelsPerInch(Value: Integer);
begin
  PixelsPerInch := Value;
end;  // TDescriptorTab.Set_PixelsPerInch 

{-------------------------------------------------------------------------------
}
procedure TDescriptorTab.Set_PrintScale(Value: TxPrintScale);
begin
  PrintScale := TPrintScale(Value);
end;  // TDescriptorTab.Set_PrintScale 

{-------------------------------------------------------------------------------
}
procedure TDescriptorTab.Set_Scaled(Value: WordBool);
begin
  Scaled := Value;
end;  // TDescriptorTab.Set_Scaled 

{-------------------------------------------------------------------------------
}
procedure TDescriptorTab.Set_ScreenSnap(Value: WordBool);
begin
  ScreenSnap := Value;
end;  // TDescriptorTab.Set_ScreenSnap 

{-------------------------------------------------------------------------------
}
procedure TDescriptorTab.Set_SnapBuffer(Value: Integer);
begin
  SnapBuffer := Value;
end;  // TDescriptorTab.Set_SnapBuffer 

{-------------------------------------------------------------------------------
}
procedure TDescriptorTab.Set_Visible(Value: WordBool);
begin
  Visible := Value;
end;  // TDescriptorTab.Set_Visible 

{-------------------------------------------------------------------------------
}
procedure TDescriptorTab._Set_Font(var Value: IFontDisp);
begin
  SetOleFont(Font, Value);
end;  // TDescriptorTab._Set_Font

{-------------------------------------------------------------------------------
}
procedure TDescriptorTab.WndProc(var Message: TMessage);
var
  lMsg: TMsg;
begin
  case Message.Msg of
    // TActiveXControl.TranslateAccelerator shall send navigation keys
    WM_GETDLGCODE:
        Message.Result := DLGC_WANTARROWS + DLGC_WANTTAB + DLGC_WANTCHARS;
    // dispatch navigation keys to currently active child control
    WM_KEYDOWN, WM_SYSKEYDOWN, WM_CHAR:
        if GetParentForm(Self).ActiveControl <> nil then
        begin
          lMsg.HWnd := GetParentForm(Self).ActiveControl.Handle;
          lMsg.Message := Message.Msg;
          lMsg.WParam := Message.WParam;
          lMsg.LParam := Message.LParam;
          lMsg.Time := 0;
          lMsg.Pt := Point(0, 0);
          DispatchMessage(lMsg);
        end;
    else
      inherited; // standard messages
  end;
end;  // TDescriptorTab.WndProc

{===============================================================================
  IRecorderAddin implementation
===============================================================================}
{-------------------------------------------------------------------------------
}
function TDescriptorTab.Get_Description: WideString;
begin
  Result := 'Descriptors tab for Location Feature screen.'
end;  // TDescriptorTab.Get_Description

{-------------------------------------------------------------------------------
}
function TDescriptorTab.Get_ImageFileName: WideString;
begin
  Result := '';
end;  // TDescriptorTab.Get_ImageFileName

{-------------------------------------------------------------------------------
}
function TDescriptorTab.Get_Name: WideString;
begin
  Result := 'Descriptors';
end;  // TDescriptorTab.Get_Name

{-------------------------------------------------------------------------------
}
procedure TDescriptorTab.Install(const iInstalledFilePath: WideString);
begin

end;  // TDescriptorTab.Install

{===============================================================================
  IAdditionalPage implementation
===============================================================================}
{-------------------------------------------------------------------------------
}
function TDescriptorTab.Get_CurrentKey: WideString;
begin
  Result := fraDescriptors.Key;
end;  // TDescriptorTab.Get_CurrentKey

{-------------------------------------------------------------------------------
}
function TDescriptorTab.Get_CurrentTable: WideString;
begin
  Result := '';
end;  // TDescriptorTab.Get_CurrentTable

{-------------------------------------------------------------------------------
}
function TDescriptorTab.Get_Form: WideString;
begin
  Result := 'TfrmFeatureDetails';
end;  // TDescriptorTab.Get_Form

{-------------------------------------------------------------------------------
}
function TDescriptorTab.Get_PageCaption: WideString;
begin
  Result := 'Descriptors';
end;  // TDescriptorTab.Get_PageCaption

{-------------------------------------------------------------------------------
}
procedure TDescriptorTab.Set_CurrentKey(const Value: WideString);
begin
  fraDescriptors.Key := Value;
end;  // TDescriptorTab.Set_CurrentKey

{-------------------------------------------------------------------------------
}
procedure TDescriptorTab.Set_CurrentTable(const Value: WideString);
begin

end;  // TDescriptorTab.Set_CurrentTable

{===============================================================================
  IRecorderFormEvents implementation
===============================================================================}
{-------------------------------------------------------------------------------
}
function TDescriptorTab.CheckCanSave: WordBool;
begin
  Result := False;
  try
    fraDescriptors.ValidateContent;
    Result := True;
  except
    // Do nothing on Abort.
    on EAbort do;

    // Raise is critical only, otherwise, show message only.
    on E: TExceptionPath do
      if E.Critical then raise else MessageDlg(E.Message, mtWarning, [mbOk], 0);

    // Anything else goes through.
    on E:Exception do
      raise;
  end;
end;  // TDescriptorTab.CheckCanSave

{-------------------------------------------------------------------------------
}
procedure TDescriptorTab.DoAdd;
begin
  FMasterKey := '';
  fraDescriptors.LoadContent;
  fraDescriptors.EditMode := emEdit;
end;  // TDescriptorTab.DoAdd

{-------------------------------------------------------------------------------
}
procedure TDescriptorTab.DoCancel;
begin
  fraDescriptors.EditMode := emBrowse;
  fraDescriptors.ReloadContent;
end;  // TDescriptorTab.DoCancel

{-------------------------------------------------------------------------------
}
procedure TDescriptorTab.DoDelete;
begin
  fraDescriptors.DeleteContent;
end;  // TDescriptorTab.DoDelete

{-------------------------------------------------------------------------------
}
procedure TDescriptorTab.DoEditMode;
begin
  fraDescriptors.EditMode := emEdit;
end;  // TDescriptorTab.DoEditMode 

{-------------------------------------------------------------------------------
}
procedure TDescriptorTab.DoItemChange(const iTableName: WideString; const iKeyValue:
    WideString);
begin
  if fraDescriptors.EditMode = emBrowse then begin
    FMasterKey := iKeyValue;
    fraDescriptors.LoadContent;
  end;
end;  // TDescriptorTab.DoItemChange 

{-------------------------------------------------------------------------------
}
procedure TDescriptorTab.DoSave;
begin
  fraDescriptors.SaveContent;
  fraDescriptors.EditMode := emBrowse;
end;  // TDescriptorTab.DoSave 

{-------------------------------------------------------------------------------
}
function TDescriptorTab.Get_FormName: WideString;
begin
  Result := 'TfrmFeatureDetails';
end;  // TDescriptorTab.Get_FormName

{-------------------------------------------------------------------------------
}
procedure TDescriptorTab.SetForm(const iForm: IRecorderForm);
begin

end;  // TDescriptorTab.SetForm

{===============================================================================
  IAdditionalProperties implementation
===============================================================================}
{-------------------------------------------------------------------------------
}
function TDescriptorTab.GetProperty(const AName: string): Variant;
begin
  // Only one property matters, disregard all others.
  if AName = PROP_KEY then
    Result := FMasterKey
  else
    Result := Null;
end;  // TDescriptorTab.GetProperty

initialization
  TActiveFormFactory.Create(
    ComServer,
    TActiveFormControl,
    TDescriptorTab,
    Class_DescriptorTab,
    9,
    '',
    OLEMISC_SIMPLEFRAME or OLEMISC_ACTSLIKELABEL,
    tmApartment);
end.

