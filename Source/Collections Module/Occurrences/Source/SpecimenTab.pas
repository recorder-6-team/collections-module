{===============================================================================
  Unit:        SpecimenTab

  Defines:     TSpecimenTab

  Description: Replacement tab for Specimen on Taxon Occurrence screen.

  Model:

  Created:     February 2004

  Last revision information:
    $Revision: 4 $
    $Date: 15/09/10 10:29 $
    $Author: Robertjohnson $

===============================================================================}

unit SpecimenTab;

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, ActiveX, AxCtrls,
  Occurrences_TLB, StdVcl, Recorder2000_TLB, BaseDragFrameUnit, BaseDetailFrameUnit,
  BaseTabSheetFrameUnit, FrameSpecimens, DataTypes, LuxembourgConstants, Variants, DSSDataTypes;

type
  TSpecimenTab = class(TActiveForm, ISpecimenTab, IRecorderAddin, IAdditionalPage,
    IRecorderFormEvents, IAdditionalProperties)
    fraSpecimens: TfraSpecimens;
  private
    FEvents: ISpecimenTabEvents;
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
    procedure _Set_Font(var Value: IFontDisp); safecall;
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
  public
    destructor Destroy; override;
    procedure Initialize; override;
  end;

//==============================================================================
implementation

uses
  ComObj, ComServ, ApplicationSettings;

{$R *.DFM}

{===============================================================================
  TSpecimenTab
===============================================================================}
destructor TSpecimenTab.Destroy;
begin
  fraSpecimens.Free;

  inherited;
end;  // TSpecimenTab.Destroy

procedure TSpecimenTab.DefinePropertyPages(DefinePropertyPage: TDefinePropertyPage);
begin
  { Define property pages here.  Property pages are defined by calling
    DefinePropertyPage with the class id of the page.  For example,
      DefinePropertyPage(Class_SpecimenTabPage); }
end;

procedure TSpecimenTab.EventSinkChanged(const EventSink: IUnknown);
begin
  FEvents := EventSink as ISpecimenTabEvents;
  inherited EventSinkChanged(EventSink);
end;

procedure TSpecimenTab.Initialize;
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

  fraSpecimens.SetAdditionalProperties(Self);
  fraSpecimens.IsTaxonOccurrence := True;
  fraSpecimens.EditMode := emBrowse;
end;

function TSpecimenTab.Get_Active: WordBool;
begin
  Result := Active;
end;

function TSpecimenTab.Get_AlignDisabled: WordBool;
begin
  Result := AlignDisabled;
end;

function TSpecimenTab.Get_AutoScroll: WordBool;
begin
  Result := AutoScroll;
end;

function TSpecimenTab.Get_AutoSize: WordBool;
begin
  Result := AutoSize;
end;

function TSpecimenTab.Get_AxBorderStyle: TxActiveFormBorderStyle;
begin
  Result := Ord(AxBorderStyle);
end;

function TSpecimenTab.Get_Caption: WideString;
begin
  Result := WideString(Caption);
end;

function TSpecimenTab.Get_Color: OLE_COLOR;
begin
  Result := OLE_COLOR(Color);
end;

function TSpecimenTab.Get_DoubleBuffered: WordBool;
begin
  Result := DoubleBuffered;
end;

function TSpecimenTab.Get_DropTarget: WordBool;
begin
  Result := DropTarget;
end;

function TSpecimenTab.Get_Enabled: WordBool;
begin
  Result := Enabled;
end;

function TSpecimenTab.Get_Font: IFontDisp;
begin
  GetOleFont(Font, Result);
end;

function TSpecimenTab.Get_HelpFile: WideString;
begin
  Result := WideString(HelpFile);
end;

function TSpecimenTab.Get_KeyPreview: WordBool;
begin
  Result := KeyPreview;
end;

function TSpecimenTab.Get_PixelsPerInch: Integer;
begin
  Result := PixelsPerInch;
end;

function TSpecimenTab.Get_PrintScale: TxPrintScale;
begin
  Result := Ord(PrintScale);
end;

function TSpecimenTab.Get_Scaled: WordBool;
begin
  Result := Scaled;
end;

function TSpecimenTab.Get_ScreenSnap: WordBool;
begin
  Result := ScreenSnap;
end;

function TSpecimenTab.Get_SnapBuffer: Integer;
begin
  Result := SnapBuffer;
end;

function TSpecimenTab.Get_Visible: WordBool;
begin
  Result := Visible;
end;

function TSpecimenTab.Get_VisibleDockClientCount: Integer;
begin
  Result := VisibleDockClientCount;
end;

procedure TSpecimenTab._Set_Font(var Value: IFontDisp);
begin
  SetOleFont(Font, Value);
end;

procedure TSpecimenTab.ActivateEvent(Sender: TObject);
begin
  if FEvents <> nil then FEvents.OnActivate;
end;

procedure TSpecimenTab.ClickEvent(Sender: TObject);
begin
  if FEvents <> nil then FEvents.OnClick;
end;

procedure TSpecimenTab.CreateEvent(Sender: TObject);
begin
  if FEvents <> nil then FEvents.OnCreate;
end;

procedure TSpecimenTab.DblClickEvent(Sender: TObject);
begin
  if FEvents <> nil then FEvents.OnDblClick;
end;

procedure TSpecimenTab.DeactivateEvent(Sender: TObject);
begin
  if FEvents <> nil then FEvents.OnDeactivate;
end;

procedure TSpecimenTab.DestroyEvent(Sender: TObject);
begin
  if FEvents <> nil then FEvents.OnDestroy;
end;

procedure TSpecimenTab.KeyPressEvent(Sender: TObject; var Key: Char);
var
  TempKey: Smallint;
begin
  TempKey := Smallint(Key);
  if FEvents <> nil then FEvents.OnKeyPress(TempKey);
  Key := Char(TempKey);
end;

procedure TSpecimenTab.PaintEvent(Sender: TObject);
begin
  if FEvents <> nil then FEvents.OnPaint;
end;

procedure TSpecimenTab.Set_AutoScroll(Value: WordBool);
begin
  AutoScroll := Value;
end;

procedure TSpecimenTab.Set_AutoSize(Value: WordBool);
begin
  AutoSize := Value;
end;

procedure TSpecimenTab.Set_AxBorderStyle(Value: TxActiveFormBorderStyle);
begin
  AxBorderStyle := TActiveFormBorderStyle(Value);
end;

procedure TSpecimenTab.Set_Caption(const Value: WideString);
begin
  Caption := TCaption(Value);
end;

procedure TSpecimenTab.Set_Color(Value: OLE_COLOR);
begin
  Color := TColor(Value);
end;

procedure TSpecimenTab.Set_DoubleBuffered(Value: WordBool);
begin
  DoubleBuffered := Value;
end;

procedure TSpecimenTab.Set_DropTarget(Value: WordBool);
begin
  DropTarget := Value;
end;

procedure TSpecimenTab.Set_Enabled(Value: WordBool);
begin
  Enabled := Value;
end;

procedure TSpecimenTab.Set_Font(const Value: IFontDisp);
begin
  SetOleFont(Font, Value);
end;

procedure TSpecimenTab.Set_HelpFile(const Value: WideString);
begin
  HelpFile := String(Value);
end;

procedure TSpecimenTab.Set_KeyPreview(Value: WordBool);
begin
  KeyPreview := Value;
end;

procedure TSpecimenTab.Set_PixelsPerInch(Value: Integer);
begin
  PixelsPerInch := Value;
end;

procedure TSpecimenTab.Set_PrintScale(Value: TxPrintScale);
begin
  PrintScale := TPrintScale(Value);
end;

procedure TSpecimenTab.Set_Scaled(Value: WordBool);
begin
  Scaled := Value;
end;

procedure TSpecimenTab.Set_ScreenSnap(Value: WordBool);
begin
  ScreenSnap := Value;
end;

procedure TSpecimenTab.Set_SnapBuffer(Value: Integer);
begin
  SnapBuffer := Value;
end;

procedure TSpecimenTab.Set_Visible(Value: WordBool);
begin
  Visible := Value;
end;

{-------------------------------------------------------------------------------
}
procedure TSpecimenTab.WndProc(var Message: TMessage);
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
function TSpecimenTab.Get_Description: WideString;
begin
  Result := 'Specimen tab replacement for Taxon Occurrence screen.'
end;  // TSpecimenTab.Get_Description

function TSpecimenTab.Get_ImageFileName: WideString;
begin
  Result := '';
end;  // TSpecimenTab.Get_ImageFileName

function TSpecimenTab.Get_Name: WideString;
begin
  Result := 'Specimen';
end;  // TSpecimenTab.Get_Name

procedure TSpecimenTab.Install(const iInstalledFilePath: WideString);
begin

end;  // TSpecimenTab.Install

{===============================================================================
  IAdditionalPage implementation
===============================================================================}
function TSpecimenTab.Get_CurrentKey: WideString;
begin
  Result := fraSpecimens.Key;
end;  // TSpecimenTab.Get_CurrentKey

function TSpecimenTab.Get_CurrentTable: WideString;
begin
  Result := '';
end;  // TSpecimenTab.Get_CurrentTable

function TSpecimenTab.Get_Form: WideString;
begin
  Result := 'TfrmTaxonOccurrences';
end;  // TSpecimenTab.Get_Form

function TSpecimenTab.Get_PageCaption: WideString;
begin
  Result := 'Specimens';
end;  // TSpecimenTab.Get_PageCaption

procedure TSpecimenTab.Set_CurrentKey(const Value: WideString);
begin
  fraSpecimens.Key := Value;
end;  // TSpecimenTab.Set_CurrentKey

procedure TSpecimenTab.Set_CurrentTable(const Value: WideString);
begin

end;  // TSpecimenTab.Set_CurrentTable

{===============================================================================
  IRecorderFormEvents implementation
===============================================================================}
function TSpecimenTab.CheckCanSave: WordBool;
begin
  Result := True;
end;  // TSpecimenTab.CheckCanSave

procedure TSpecimenTab.DoAdd;
begin
  FMasterKey := '';
  fraSpecimens.LoadContent;
  fraSpecimens.EditMode := emEdit;
end;  // TSpecimenTab.DoAdd

procedure TSpecimenTab.DoCancel;
begin
  fraSpecimens.EditMode := emBrowse;
  fraSpecimens.ReloadContent;
end;  // TSpecimenTab.DoCancel

procedure TSpecimenTab.DoDelete;
begin
  fraSpecimens.DeleteContent;
end;  // TSpecimenTab.DoDelete

procedure TSpecimenTab.DoEditMode;
begin
  fraSpecimens.EditMode := emEdit;
end;  // TSpecimenTab.DoEditMode

procedure TSpecimenTab.DoItemChange(const iTableName: WideString; const iKeyValue: WideString);
begin
  if fraSpecimens.EditMode = emBrowse then begin
    FMasterKey := iKeyValue;
    fraSpecimens.LoadContent;
  end;
end;  // TSpecimenTab.DoItemChange

procedure TSpecimenTab.DoSave;
begin
  fraSpecimens.SaveContent;
  fraSpecimens.EditMode := emBrowse;
end;  // TSpecimenTab.DoSave

function TSpecimenTab.Get_FormName: WideString;
begin
  Result := 'TfrmTaxonOccurrences';
end;  // TSpecimenTab.Get_FormName

procedure TSpecimenTab.SetForm(const iForm: IRecorderForm);
var
  lControl: IInterface;
begin
  // Find original Specimens tab.
  lControl := iForm.Control['tsSpecimens'];
  if Assigned(lControl) then
    if Supports(lControl, IID_ITabPageControl) then
      // If the 'Show Recorder Specimens Tab' option has been selected,
      // then rename the original specimens tab to disguish it from the
      // new tab. Otherwise, hide it.
      if AppSettings.ShowRecorderSpecimensTab then
        (lControl as ITabPageControl).Caption := 'Recorder Specimens'
      else
        (lControl as IRecorderControl).Visible := False;
end;  // TSpecimenTab.SetForm

{===============================================================================
  IAdditionalProperties implementation
===============================================================================}
function TSpecimenTab.GetProperty(const AName: string): Variant;
begin
  // Only one property matters, disregard all others.
  if AName = PROP_KEY then
    Result := FMasterKey
  else
    Result := Null;
end;  // TSpecimenTab.GetProperty

initialization
  TActiveFormFactory.Create(
    ComServer,
    TActiveFormControl,
    TSpecimenTab,
    Class_SpecimenTab,
    8,
    '',
    OLEMISC_SIMPLEFRAME or OLEMISC_ACTSLIKELABEL,
    tmApartment);
end.
