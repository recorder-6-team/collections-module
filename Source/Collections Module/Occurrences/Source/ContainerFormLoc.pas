{===============================================================================
  Unit:        ContainerFormLoc

  Defines:     TfrmContainerLoc

  Description: Container form for Location Feature detail screens.

  Model:       Occurrences.mpb

  Created:     September 2004

  Last revision information:
    $Revision: 2 $
    $Date: 19/12/06 14:36 $
    $Author: Ericsalmon $

===============================================================================}

unit ContainerFormLoc;

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ActiveX, AxCtrls, Occurrences_TLB, StdVcl, Recorder2000_TLB, DataTypes,
  FrameContainerUnit, LuxembourgConstants, Variants, DSSDataTypes;

type
  TfrmContainerLoc = class(TActiveForm, IfrmContainerLoc, IRecorderDetailScreen,
      IAdditionalProperties)
    fraContainer: TfraContainer;
  private
    FCurrentItemKey: String;
    FCurrentParentKey: String;
    FCurrentTableName: String;
    FDetailScreenEvents: IRecorderDetailScreenEvents;
    FEvents: IfrmContainerLocEvents;
    FTypeName: String;
    procedure ActivateEvent(Sender: TObject);
    procedure ClickEvent(Sender: TObject);
    procedure CreateEvent(Sender: TObject);
    procedure DblClickEvent(Sender: TObject);
    procedure DeactivateEvent(Sender: TObject);
    procedure DeleteRecord; safecall;
    procedure DestroyEvent(Sender: TObject);
    procedure EditModeChange(Sender: TObject);
    procedure EditRecord; safecall;
    function GetProperty(const AName: string): Variant;
    function Get_CanClose: WordBool; safecall;
    function Get_Editing: WordBool; safecall;
    function Get_Events: IRecorderDetailScreenEvents; safecall;
    function Get_SelectedItemCaption: WideString; safecall;
    procedure KeyPressEvent(Sender: TObject; var Key: Char);
    procedure LoadContent(const TypeName: WideString; const ParentItemKey: WideString; const
        ItemKey: WideString); safecall;
    procedure PaintEvent(Sender: TObject);
    procedure RefreshScreenInfo(Sender: TObject; const AKey, ACaption: String);
    procedure Set_Events(const Value: IRecorderDetailScreenEvents); safecall;
    procedure Set_SelectedItemCaption(const Value: WideString); safecall;
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
  public
    destructor Destroy; override;
    procedure Initialize; override;
  end;
  
//==============================================================================
implementation

uses
  ComObj, ComServ, BaseDetailFrameUnit, FrameMeasurementsLocation, GeneralData,
  ResourceStrings;

{$R *.DFM}

{-==============================================================================
    TfrmContainerLoc
===============================================================================}
{-------------------------------------------------------------------------------
}
destructor TfrmContainerLoc.Destroy;
begin
  if Assigned(fraContainer) then fraContainer.UnloadFrames;
  
  inherited;
end;  // TfrmContainerLoc.Destroy 

{-------------------------------------------------------------------------------
}
procedure TfrmContainerLoc.ActivateEvent(Sender: TObject);
begin
  if FEvents <> nil then FEvents.OnActivate;
end;  // TfrmContainerLoc.ActivateEvent 

{-------------------------------------------------------------------------------
}
procedure TfrmContainerLoc.ClickEvent(Sender: TObject);
begin
  if FEvents <> nil then FEvents.OnClick;
end;  // TfrmContainerLoc.ClickEvent 

{-------------------------------------------------------------------------------
}
procedure TfrmContainerLoc.CreateEvent(Sender: TObject);
begin
  if FEvents <> nil then FEvents.OnCreate;
end;  // TfrmContainerLoc.CreateEvent 

{-------------------------------------------------------------------------------
}
procedure TfrmContainerLoc.DblClickEvent(Sender: TObject);
begin
  if FEvents <> nil then FEvents.OnDblClick;
end;  // TfrmContainerLoc.DblClickEvent 

{-------------------------------------------------------------------------------
}
procedure TfrmContainerLoc.DeactivateEvent(Sender: TObject);
begin
  if FEvents <> nil then FEvents.OnDeactivate;
end;  // TfrmContainerLoc.DeactivateEvent 

{-------------------------------------------------------------------------------
}
procedure TfrmContainerLoc.DefinePropertyPages(DefinePropertyPage: TDefinePropertyPage);
begin
  { Define property pages here.  Property pages are defined by calling
    DefinePropertyPage with the class id of the page.  For example,
      DefinePropertyPage(Class_frmContainerLocPage); }
end;  // TfrmContainerLoc.DefinePropertyPages 

{-------------------------------------------------------------------------------
}
procedure TfrmContainerLoc.DeleteRecord;
begin
  dmGeneral.Connection.BeginTrans;
  try
    fraContainer.ContainedFrame.DeleteContent;
    dmGeneral.Connection.CommitTrans;
  except
    on Exception do begin
      dmGeneral.Connection.RollbackTrans;
      Raise;
    end;
  end;
end;  // TfrmContainerLoc.DeleteRecord 

{-------------------------------------------------------------------------------
}
procedure TfrmContainerLoc.DestroyEvent(Sender: TObject);
begin
  if FEvents <> nil then FEvents.OnDestroy;
end;  // TfrmContainerLoc.DestroyEvent 

{-------------------------------------------------------------------------------
}
procedure TfrmContainerLoc.EditModeChange(Sender: TObject);
begin
  if Assigned(FDetailScreenEvents) then FDetailScreenEvents.OnEditModeChange;
end;  // TfrmContainerLoc.EditModeChange 

{-------------------------------------------------------------------------------
}
procedure TfrmContainerLoc.EditRecord;
begin
  fraContainer.EditMode := emEdit;
end;  // TfrmContainerLoc.EditRecord 

{-------------------------------------------------------------------------------
}
procedure TfrmContainerLoc.EventSinkChanged(const EventSink: IUnknown);
begin
  FEvents := EventSink as IfrmContainerLocEvents;
  inherited EventSinkChanged(EventSink);
end;  // TfrmContainerLoc.EventSinkChanged 

{-------------------------------------------------------------------------------
}
function TfrmContainerLoc.GetProperty(const AName: string): Variant;
begin
  if AName = PROP_KEY then
    Result := FCurrentItemKey
  else if AName = PROP_PARENT_KEY then
    Result := FCurrentParentKey
  else if AName = PROP_TABLE_NAME then
    Result := FCurrentTableName
  else if AName = PROP_TOP_NODE_CONTEXT then
    Result := ncNone
  else
    Result := Null;
end;  // TfrmContainerLoc.GetProperty 

{-------------------------------------------------------------------------------
}
function TfrmContainerLoc.Get_Active: WordBool;
begin
  Result := Active;
end;  // TfrmContainerLoc.Get_Active 

{-------------------------------------------------------------------------------
}
function TfrmContainerLoc.Get_AlignDisabled: WordBool;
begin
  Result := AlignDisabled;
end;  // TfrmContainerLoc.Get_AlignDisabled 

{-------------------------------------------------------------------------------
}
function TfrmContainerLoc.Get_AutoScroll: WordBool;
begin
  Result := AutoScroll;
end;  // TfrmContainerLoc.Get_AutoScroll 

{-------------------------------------------------------------------------------
}
function TfrmContainerLoc.Get_AutoSize: WordBool;
begin
  Result := AutoSize;
end;  // TfrmContainerLoc.Get_AutoSize 

{-------------------------------------------------------------------------------
}
function TfrmContainerLoc.Get_AxBorderStyle: TxActiveFormBorderStyle;
begin
  Result := Ord(AxBorderStyle);
end;  // TfrmContainerLoc.Get_AxBorderStyle 

{-------------------------------------------------------------------------------
}
function TfrmContainerLoc.Get_CanClose: WordBool;
begin
  if not Get_Editing then
    Result := True
  else
  if Get_Editing then
    case MessageDlg(ResStr_FrameToCloseInEditMode, mtWarning, [mbYes, mbNo, mbCancel], 0) of
      mrYes:
        begin
          fraContainer.SaveChanges;
          Result := True;
        end;
  
      mrNo:
        begin
          fraContainer.CancelChanges;
          Result := True;
        end;
  
      mrCancel: Result := False;
    end;
end;  // TfrmContainerLoc.Get_CanClose 

{-------------------------------------------------------------------------------
}
function TfrmContainerLoc.Get_Caption: WideString;
begin
  Result := WideString(Caption);
end;  // TfrmContainerLoc.Get_Caption 

{-------------------------------------------------------------------------------
}
function TfrmContainerLoc.Get_Color: OLE_COLOR;
begin
  Result := OLE_COLOR(Color);
end;  // TfrmContainerLoc.Get_Color 

{-------------------------------------------------------------------------------
}
function TfrmContainerLoc.Get_DoubleBuffered: WordBool;
begin
  Result := DoubleBuffered;
end;  // TfrmContainerLoc.Get_DoubleBuffered 

{-------------------------------------------------------------------------------
}
function TfrmContainerLoc.Get_DropTarget: WordBool;
begin
  Result := DropTarget;
end;  // TfrmContainerLoc.Get_DropTarget 

{-------------------------------------------------------------------------------
}
function TfrmContainerLoc.Get_Editing: WordBool;
begin
  Result := fraContainer.EditMode = emEdit;
end;  // TfrmContainerLoc.Get_Editing 

{-------------------------------------------------------------------------------
}
function TfrmContainerLoc.Get_Enabled: WordBool;
begin
  Result := Enabled;
end;  // TfrmContainerLoc.Get_Enabled 

{-------------------------------------------------------------------------------
}
function TfrmContainerLoc.Get_Events: IRecorderDetailScreenEvents;
begin
  Result := FDetailScreenEvents;
end;  // TfrmContainerLoc.Get_Events 

{-------------------------------------------------------------------------------
}
function TfrmContainerLoc.Get_Font: IFontDisp;
begin
  GetOleFont(Font, Result);
end;  // TfrmContainerLoc.Get_Font 

{-------------------------------------------------------------------------------
}
function TfrmContainerLoc.Get_HelpFile: WideString;
begin
  Result := WideString(HelpFile);
end;  // TfrmContainerLoc.Get_HelpFile 

{-------------------------------------------------------------------------------
}
function TfrmContainerLoc.Get_KeyPreview: WordBool;
begin
  Result := KeyPreview;
end;  // TfrmContainerLoc.Get_KeyPreview 

{-------------------------------------------------------------------------------
}
function TfrmContainerLoc.Get_PixelsPerInch: Integer;
begin
  Result := PixelsPerInch;
end;  // TfrmContainerLoc.Get_PixelsPerInch 

{-------------------------------------------------------------------------------
}
function TfrmContainerLoc.Get_PrintScale: TxPrintScale;
begin
  Result := Ord(PrintScale);
end;  // TfrmContainerLoc.Get_PrintScale 

{-------------------------------------------------------------------------------
}
function TfrmContainerLoc.Get_Scaled: WordBool;
begin
  Result := Scaled;
end;  // TfrmContainerLoc.Get_Scaled 

{-------------------------------------------------------------------------------
}
function TfrmContainerLoc.Get_ScreenSnap: WordBool;
begin
  Result := ScreenSnap;
end;  // TfrmContainerLoc.Get_ScreenSnap 

{-------------------------------------------------------------------------------
}
function TfrmContainerLoc.Get_SelectedItemCaption: WideString;
begin
  
end;  // TfrmContainerLoc.Get_SelectedItemCaption 

{-------------------------------------------------------------------------------
}
function TfrmContainerLoc.Get_SnapBuffer: Integer;
begin
  Result := SnapBuffer;
end;  // TfrmContainerLoc.Get_SnapBuffer 

{-------------------------------------------------------------------------------
}
function TfrmContainerLoc.Get_Visible: WordBool;
begin
  Result := Visible;
end;  // TfrmContainerLoc.Get_Visible 

{-------------------------------------------------------------------------------
}
function TfrmContainerLoc.Get_VisibleDockClientCount: Integer;
begin
  Result := VisibleDockClientCount;
end;  // TfrmContainerLoc.Get_VisibleDockClientCount 

{-------------------------------------------------------------------------------
}
procedure TfrmContainerLoc.Initialize;
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
  // Link events all the way down to the frame.
  fraContainer.OnEditModeChange := EditModeChange;
  fraContainer.OnRefreshScreenInfo := RefreshScreenInfo;
end;  // TfrmContainerLoc.Initialize 

{-------------------------------------------------------------------------------
}
procedure TfrmContainerLoc.KeyPressEvent(Sender: TObject; var Key: Char);
var
  TempKey: SmallInt;
begin
  TempKey := Smallint(Key);
  if FEvents <> nil then FEvents.OnKeyPress(TempKey);
  Key := Char(TempKey);
end;  // TfrmContainerLoc.KeyPressEvent 

{-------------------------------------------------------------------------------
}
procedure TfrmContainerLoc.LoadContent(const TypeName: WideString; const ParentItemKey:
    WideString; const ItemKey: WideString);
begin
  // If exactly same frame and record, just reload.
  if (FCurrentParentKey = ParentItemKey) and (FCurrentItemKey = ItemKey) and
     (FTypeName = TypeName) and Assigned(fraContainer.ContainedFrame) then
    fraContainer.ContainedFrame.ReloadContent
  else begin
    // Setup for IAdditinalProperties.GetProperty
    FCurrentParentKey := ParentItemKey;
    FCurrentItemKey   := ItemKey;
    FTypeName         := TypeName;
  
    if TypeName = 'Measurement' then begin
      FCurrentTableName := TN_LOCATION_FEATURE_DATA;
      fraContainer.LoadContent(TfraMeasurementsLoc, Self);
    end;
  end;
end;  // TfrmContainerLoc.LoadContent 

{-------------------------------------------------------------------------------
}
procedure TfrmContainerLoc.PaintEvent(Sender: TObject);
begin
  if FEvents <> nil then FEvents.OnPaint;
end;  // TfrmContainerLoc.PaintEvent 

{-------------------------------------------------------------------------------
}
procedure TfrmContainerLoc.RefreshScreenInfo(Sender: TObject; const AKey, ACaption: String);
begin
  if Assigned(FDetailScreenEvents) then
    FDetailScreenEvents.OnRefreshScreenInfo(AKey, ACaption);
  // Update current item key, especially for new items.
  FCurrentItemKey := AKey;
end;  // TfrmContainerLoc.RefreshScreenInfo 

{-------------------------------------------------------------------------------
}
procedure TfrmContainerLoc.Set_AutoScroll(Value: WordBool);
begin
  AutoScroll := Value;
end;  // TfrmContainerLoc.Set_AutoScroll 

{-------------------------------------------------------------------------------
}
procedure TfrmContainerLoc.Set_AutoSize(Value: WordBool);
begin
  AutoSize := Value;
end;  // TfrmContainerLoc.Set_AutoSize 

{-------------------------------------------------------------------------------
}
procedure TfrmContainerLoc.Set_AxBorderStyle(Value: TxActiveFormBorderStyle);
begin
  AxBorderStyle := TActiveFormBorderStyle(Value);
end;  // TfrmContainerLoc.Set_AxBorderStyle 

{-------------------------------------------------------------------------------
}
procedure TfrmContainerLoc.Set_Caption(const Value: WideString);
begin
  Caption := TCaption(Value);
end;  // TfrmContainerLoc.Set_Caption 

{-------------------------------------------------------------------------------
}
procedure TfrmContainerLoc.Set_Color(Value: OLE_COLOR);
begin
  Color := TColor(Value);
end;  // TfrmContainerLoc.Set_Color 

{-------------------------------------------------------------------------------
}
procedure TfrmContainerLoc.Set_DoubleBuffered(Value: WordBool);
begin
  DoubleBuffered := Value;
end;  // TfrmContainerLoc.Set_DoubleBuffered 

{-------------------------------------------------------------------------------
}
procedure TfrmContainerLoc.Set_DropTarget(Value: WordBool);
begin
  DropTarget := Value;
end;  // TfrmContainerLoc.Set_DropTarget 

{-------------------------------------------------------------------------------
}
procedure TfrmContainerLoc.Set_Enabled(Value: WordBool);
begin
  Enabled := Value;
end;  // TfrmContainerLoc.Set_Enabled 

{-------------------------------------------------------------------------------
}
procedure TfrmContainerLoc.Set_Events(const Value: IRecorderDetailScreenEvents);
begin
  FDetailScreenEvents := Value;
end;  // TfrmContainerLoc.Set_Events 

{-------------------------------------------------------------------------------
}
procedure TfrmContainerLoc.Set_Font(const Value: IFontDisp);
begin
  SetOleFont(Font, Value);
end;  // TfrmContainerLoc.Set_Font 

{-------------------------------------------------------------------------------
}
procedure TfrmContainerLoc.Set_HelpFile(const Value: WideString);
begin
  HelpFile := String(Value);
end;  // TfrmContainerLoc.Set_HelpFile 

{-------------------------------------------------------------------------------
}
procedure TfrmContainerLoc.Set_KeyPreview(Value: WordBool);
begin
  KeyPreview := Value;
end;  // TfrmContainerLoc.Set_KeyPreview 

{-------------------------------------------------------------------------------
}
procedure TfrmContainerLoc.Set_PixelsPerInch(Value: Integer);
begin
  PixelsPerInch := Value;
end;  // TfrmContainerLoc.Set_PixelsPerInch 

{-------------------------------------------------------------------------------
}
procedure TfrmContainerLoc.Set_PrintScale(Value: TxPrintScale);
begin
  PrintScale := TPrintScale(Value);
end;  // TfrmContainerLoc.Set_PrintScale 

{-------------------------------------------------------------------------------
}
procedure TfrmContainerLoc.Set_Scaled(Value: WordBool);
begin
  Scaled := Value;
end;  // TfrmContainerLoc.Set_Scaled 

{-------------------------------------------------------------------------------
}
procedure TfrmContainerLoc.Set_ScreenSnap(Value: WordBool);
begin
  ScreenSnap := Value;
end;  // TfrmContainerLoc.Set_ScreenSnap 

{-------------------------------------------------------------------------------
}
procedure TfrmContainerLoc.Set_SelectedItemCaption(const Value: WideString);
begin
  fraContainer.lblName.Caption := Value;
end;  // TfrmContainerLoc.Set_SelectedItemCaption 

{-------------------------------------------------------------------------------
}
procedure TfrmContainerLoc.Set_SnapBuffer(Value: Integer);
begin
  SnapBuffer := Value;
end;  // TfrmContainerLoc.Set_SnapBuffer 

{-------------------------------------------------------------------------------
}
procedure TfrmContainerLoc.Set_Visible(Value: WordBool);
begin
  Visible := Value;
end;  // TfrmContainerLoc.Set_Visible 

{-------------------------------------------------------------------------------
}
procedure TfrmContainerLoc._Set_Font(var Value: IFontDisp);
begin
  SetOleFont(Font, Value);
end;  // TfrmContainerLoc._Set_Font 

initialization
  TActiveFormFactory.Create(
    ComServer,
    TActiveFormControl,
    TfrmContainerLoc,
    Class_frmContainerLoc,
    10,
    '',
    OLEMISC_SIMPLEFRAME or OLEMISC_ACTSLIKELABEL,
    tmApartment);
end.



