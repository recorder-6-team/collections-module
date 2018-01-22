{===============================================================================
  Unit:        ContainerFormOcc

  Defines:     TfrmContainerOcc

  Description: Container form for Occurrence detail screens.

  Model:       Occurrences.mpb

  Created:     October 2003

  Last revision information:
    $Revision: 10 $
    $Date: 19/12/06 14:36 $
    $Author: Ericsalmon $

===============================================================================}

unit ContainerFormOcc;

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ActiveX, AxCtrls, Occurrences_TLB, StdVcl, Recorder2000_TLB, DataTypes,
  FrameContainerUnit, LuxembourgConstants, Variants, DSSDataTypes;

type
  TfrmContainerOcc = class(TActiveForm, IfrmContainerOcc, IRecorderDetailScreen,
      IAdditionalProperties)
    fraContainer: TfraContainer;
  private
    FCurrentItemKey: String;
    FCurrentParentKey: String;
    FCurrentTableName: String;
    FDetailScreenEvents: IRecorderDetailScreenEvents;
    FEvents: IfrmContainerOccEvents;
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
    procedure WndProc(var Message: TMessage); override;
    procedure _Set_Font(var Value: IFontDisp); safecall;
  public
    destructor Destroy; override;
    procedure Initialize; override;
  end;
  
//==============================================================================
implementation

uses
  ComObj, ComServ, BaseDetailFrameUnit, FrameOccurrence, FrameDeterminationOccurrence,
  FrameMeasurementsOccurrence, GeneralData, ResourceStrings;

{$R *.DFM}

{-==============================================================================
    TfrmContainerOcc
===============================================================================}
{-------------------------------------------------------------------------------
}
destructor TfrmContainerOcc.Destroy;
begin
  if Assigned(fraContainer) then fraContainer.UnloadFrames;
  
  inherited;
end;  // TfrmContainerOcc.Destroy 

{-------------------------------------------------------------------------------
}
procedure TfrmContainerOcc.ActivateEvent(Sender: TObject);
begin
  if FEvents <> nil then FEvents.OnActivate;
end;  // TfrmContainerOcc.ActivateEvent 

{-------------------------------------------------------------------------------
}
procedure TfrmContainerOcc.ClickEvent(Sender: TObject);
begin
  if FEvents <> nil then FEvents.OnClick;
end;  // TfrmContainerOcc.ClickEvent 

{-------------------------------------------------------------------------------
}
procedure TfrmContainerOcc.CreateEvent(Sender: TObject);
begin
  if FEvents <> nil then FEvents.OnCreate;
end;  // TfrmContainerOcc.CreateEvent 

{-------------------------------------------------------------------------------
}
procedure TfrmContainerOcc.DblClickEvent(Sender: TObject);
begin
  if FEvents <> nil then FEvents.OnDblClick;
end;  // TfrmContainerOcc.DblClickEvent 

{-------------------------------------------------------------------------------
}
procedure TfrmContainerOcc.DeactivateEvent(Sender: TObject);
begin
  if FEvents <> nil then FEvents.OnDeactivate;
end;  // TfrmContainerOcc.DeactivateEvent 

{-------------------------------------------------------------------------------
}
procedure TfrmContainerOcc.DefinePropertyPages(DefinePropertyPage: TDefinePropertyPage);
begin
  { Define property pages here.  Property pages are defined by calling
    DefinePropertyPage with the class id of the page.  For example,
      DefinePropertyPage(Class_frmContainerPage); }
end;  // TfrmContainerOcc.DefinePropertyPages 

{-------------------------------------------------------------------------------
}
procedure TfrmContainerOcc.DeleteRecord;
var
  lContainer: TfraContainer;
  lCurrentParentKey: String;
  lCurrentItemKey: String;
  lCurrentTableName: String;
  
  procedure DeleteRecords(const AStoredProcName: String; AFrameClass: TBaseDetailFrameClass);
  var
    lKeys: TStringList;
    i:  Integer;
  begin
    lKeys := TStringList.Create;
    try
      // Get keys first, no "dangling" recordset while going through the delete sequence.
      with dmGeneral.GetRecordset(AStoredProcName, ['@OccurrenceKey', FCurrentParentKey]) do
      begin
        while not Eof do begin
          lKeys.Add(VarToStr(Fields['Item_Key'].Value));
          MoveNext;
        end;
        Close;
      end;
      // Now for each key, go and delete the corresponding data from the database.
      for i := 0 to lKeys.Count - 1 do begin
        FCurrentItemKey := lKeys[i];
        lContainer.LoadContent(AFrameClass, Self);
        lContainer.ContainedFrame.DeleteContent;
       end;
     finally
       FreeAndNil(lKeys);
     end;
  end;
  
begin
  lCurrentParentKey := FCurrentParentKey;
  lCurrentItemKey   := FCurrentItemKey;
  lCurrentTableName := FCurrentTableName;
  
  dmGeneral.Connection.BeginTrans;
  try
    // Delete Occurrence record. Have to go through all Measurements/Determinations
    if CompareText(FCurrentTableName, TN_OCCURRENCE) = 0 then begin
      lContainer := TfraContainer.Create(Owner);
      try
        lContainer.Visible := False;  // Don't want to see it!
        lContainer.Parent  := Self;
        lContainer.SendToBack;        // Hide it behind current one
        lContainer.Visible := True;   // Have to "show" it now to have everything
                                      // created properly!
        // Set Parent Key to be the Occurrence's key
        FCurrentParentKey := lCurrentItemKey;
  
        // Start with Measurements
        FCurrentTableName := TN_OCCURRENCE_DATA;
        DeleteRecords('usp_Measurements_Select_ForOccurrence', TfraMeasurementsOcc);

        // Then do the determinations
        FCurrentTableName := TN_DETERMINATION;
        DeleteRecords('usp_Determinations_Select_ForOccurrence', TfraDetermination);
  
        // Restore values
        FCurrentParentKey := lCurrentParentKey;
        FCurrentItemKey   := lCurrentItemKey;
        FCurrentTableName := lCurrentTableName;
      finally
        FreeAndNil(lContainer);
      end;
    end;
    // Main record
    fraContainer.ContainedFrame.DeleteContent;
    dmGeneral.Connection.CommitTrans;
  except
    on Exception do begin
      dmGeneral.Connection.RollbackTrans;
      Raise;
    end;
  end;
end;  // TfrmContainerOcc.DeleteRecord 

{-------------------------------------------------------------------------------
}
procedure TfrmContainerOcc.DestroyEvent(Sender: TObject);
begin
  if FEvents <> nil then FEvents.OnDestroy;
end;  // TfrmContainerOcc.DestroyEvent 

{-------------------------------------------------------------------------------
}
procedure TfrmContainerOcc.EditModeChange(Sender: TObject);
begin
  if Assigned(FDetailScreenEvents) then FDetailScreenEvents.OnEditModeChange;
end;  // TfrmContainerOcc.EditModeChange 

{-------------------------------------------------------------------------------
}
procedure TfrmContainerOcc.EditRecord;
begin
  fraContainer.EditMode := emEdit;
end;  // TfrmContainerOcc.EditRecord 

{-------------------------------------------------------------------------------
}
procedure TfrmContainerOcc.EventSinkChanged(const EventSink: IUnknown);
begin
  FEvents := EventSink as IfrmContainerOccEvents;
  inherited EventSinkChanged(EventSink);
end;  // TfrmContainerOcc.EventSinkChanged 

{-------------------------------------------------------------------------------
}
function TfrmContainerOcc.GetProperty(const AName: string): Variant;
begin
  if AName = PROP_KEY then
    Result := FCurrentItemKey
  else if AName = PROP_PARENT_KEY then
    Result := FCurrentParentKey
  else if AName = PROP_TABLE_NAME then
    Result := FCurrentTableName
  else if AName = PROP_SPECIMEN_IS_LIFESCIENCES then
    Result := 0
  else if AName = PROP_TOP_NODE_CONTEXT then
    Result := ncNone
  else
    Result := Null;
end;  // TfrmContainerOcc.GetProperty 

{-------------------------------------------------------------------------------
}
function TfrmContainerOcc.Get_Active: WordBool;
begin
  Result := Active;
end;  // TfrmContainerOcc.Get_Active 

{-------------------------------------------------------------------------------
}
function TfrmContainerOcc.Get_AlignDisabled: WordBool;
begin
  Result := AlignDisabled;
end;  // TfrmContainerOcc.Get_AlignDisabled 

{-------------------------------------------------------------------------------
}
function TfrmContainerOcc.Get_AutoScroll: WordBool;
begin
  Result := AutoScroll;
end;  // TfrmContainerOcc.Get_AutoScroll 

{-------------------------------------------------------------------------------
}
function TfrmContainerOcc.Get_AutoSize: WordBool;
begin
  Result := AutoSize;
end;  // TfrmContainerOcc.Get_AutoSize 

{-------------------------------------------------------------------------------
}
function TfrmContainerOcc.Get_AxBorderStyle: TxActiveFormBorderStyle;
begin
  Result := Ord(AxBorderStyle);
end;  // TfrmContainerOcc.Get_AxBorderStyle 

{-------------------------------------------------------------------------------
}
function TfrmContainerOcc.Get_CanClose: WordBool;
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
end;  // TfrmContainerOcc.Get_CanClose 

{-------------------------------------------------------------------------------
}
function TfrmContainerOcc.Get_Caption: WideString;
begin
  Result := WideString(Caption);
end;  // TfrmContainerOcc.Get_Caption 

{-------------------------------------------------------------------------------
}
function TfrmContainerOcc.Get_Color: OLE_COLOR;
begin
  Result := OLE_COLOR(Color);
end;  // TfrmContainerOcc.Get_Color 

{-------------------------------------------------------------------------------
}
function TfrmContainerOcc.Get_DoubleBuffered: WordBool;
begin
  Result := DoubleBuffered;
end;  // TfrmContainerOcc.Get_DoubleBuffered 

{-------------------------------------------------------------------------------
}
function TfrmContainerOcc.Get_DropTarget: WordBool;
begin
  Result := DropTarget;
end;  // TfrmContainerOcc.Get_DropTarget 

{-------------------------------------------------------------------------------
}
function TfrmContainerOcc.Get_Editing: WordBool;
begin
  Result := fraContainer.EditMode = emEdit;
end;  // TfrmContainerOcc.Get_Editing

{-------------------------------------------------------------------------------
}
function TfrmContainerOcc.Get_Enabled: WordBool;
begin
  Result := Enabled;
end;  // TfrmContainerOcc.Get_Enabled 

{-------------------------------------------------------------------------------
}
function TfrmContainerOcc.Get_Events: IRecorderDetailScreenEvents;
begin
  Result := FDetailScreenEvents;
end;  // TfrmContainerOcc.Get_Events 

{-------------------------------------------------------------------------------
}
function TfrmContainerOcc.Get_Font: IFontDisp;
begin
  GetOleFont(Font, Result);
end;  // TfrmContainerOcc.Get_Font 

{-------------------------------------------------------------------------------
}
function TfrmContainerOcc.Get_HelpFile: WideString;
begin
  Result := WideString(HelpFile);
end;  // TfrmContainerOcc.Get_HelpFile 

{-------------------------------------------------------------------------------
}
function TfrmContainerOcc.Get_KeyPreview: WordBool;
begin
  Result := KeyPreview;
end;  // TfrmContainerOcc.Get_KeyPreview 

{-------------------------------------------------------------------------------
}
function TfrmContainerOcc.Get_PixelsPerInch: Integer;
begin
  Result := PixelsPerInch;
end;  // TfrmContainerOcc.Get_PixelsPerInch 

{-------------------------------------------------------------------------------
}
function TfrmContainerOcc.Get_PrintScale: TxPrintScale;
begin
  Result := Ord(PrintScale);
end;  // TfrmContainerOcc.Get_PrintScale 

{-------------------------------------------------------------------------------
}
function TfrmContainerOcc.Get_Scaled: WordBool;
begin
  Result := Scaled;
end;  // TfrmContainerOcc.Get_Scaled 

{-------------------------------------------------------------------------------
}
function TfrmContainerOcc.Get_ScreenSnap: WordBool;
begin
  Result := ScreenSnap;
end;  // TfrmContainerOcc.Get_ScreenSnap 

{-------------------------------------------------------------------------------
}
function TfrmContainerOcc.Get_SelectedItemCaption: WideString;
begin

end;  // TfrmContainerOcc.Get_SelectedItemCaption 

{-------------------------------------------------------------------------------
}
function TfrmContainerOcc.Get_SnapBuffer: Integer;
begin
  Result := SnapBuffer;
end;  // TfrmContainerOcc.Get_SnapBuffer 

{-------------------------------------------------------------------------------
}
function TfrmContainerOcc.Get_Visible: WordBool;
begin
  Result := Visible;
end;  // TfrmContainerOcc.Get_Visible 

{-------------------------------------------------------------------------------
}
function TfrmContainerOcc.Get_VisibleDockClientCount: Integer;
begin
  Result := VisibleDockClientCount;
end;  // TfrmContainerOcc.Get_VisibleDockClientCount 

{-------------------------------------------------------------------------------
}
procedure TfrmContainerOcc.Initialize;
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
end;  // TfrmContainerOcc.Initialize 

{-------------------------------------------------------------------------------
}
procedure TfrmContainerOcc.KeyPressEvent(Sender: TObject; var Key: Char);
var
  TempKey: SmallInt;
begin
  TempKey := Smallint(Key);
  if FEvents <> nil then FEvents.OnKeyPress(TempKey);
  Key := Char(TempKey);
end;  // TfrmContainerOcc.KeyPressEvent 

{-------------------------------------------------------------------------------
}
procedure TfrmContainerOcc.LoadContent(const TypeName: WideString; const ParentItemKey:
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
  
    if TypeName = 'Determination' then begin
      FCurrentTableName := TN_DETERMINATION;
      fraContainer.LoadContent(TfraDetermination, Self);
    end else
    if TypeName = 'Measurement' then begin
      FCurrentTableName := TN_OCCURRENCE_DATA;
      fraContainer.LoadContent(TfraMeasurementsOcc, Self);
    end else begin
      FCurrentTableName := TN_OCCURRENCE;
      fraContainer.LoadContent(TfraOccurrence, Self);
    end;
  end;
end;  // TfrmContainerOcc.LoadContent 

{-------------------------------------------------------------------------------
}
procedure TfrmContainerOcc.PaintEvent(Sender: TObject);
begin
  if FEvents <> nil then FEvents.OnPaint;
end;  // TfrmContainerOcc.PaintEvent 

{-------------------------------------------------------------------------------
}
procedure TfrmContainerOcc.RefreshScreenInfo(Sender: TObject; const AKey, ACaption: String);
begin
  if Assigned(FDetailScreenEvents) then
    FDetailScreenEvents.OnRefreshScreenInfo(AKey, ACaption);
  // Update current item key, especially for new items.
  FCurrentItemKey := AKey;
end;  // TfrmContainerOcc.RefreshScreenInfo 

{-------------------------------------------------------------------------------
}
procedure TfrmContainerOcc.Set_AutoScroll(Value: WordBool);
begin
  AutoScroll := Value;
end;  // TfrmContainerOcc.Set_AutoScroll 

{-------------------------------------------------------------------------------
}
procedure TfrmContainerOcc.Set_AutoSize(Value: WordBool);
begin
  AutoSize := Value;
end;  // TfrmContainerOcc.Set_AutoSize 

{-------------------------------------------------------------------------------
}
procedure TfrmContainerOcc.Set_AxBorderStyle(Value: TxActiveFormBorderStyle);
begin
  AxBorderStyle := TActiveFormBorderStyle(Value);
end;  // TfrmContainerOcc.Set_AxBorderStyle 

{-------------------------------------------------------------------------------
}
procedure TfrmContainerOcc.Set_Caption(const Value: WideString);
begin
  Caption := TCaption(Value);
end;  // TfrmContainerOcc.Set_Caption 

{-------------------------------------------------------------------------------
}
procedure TfrmContainerOcc.Set_Color(Value: OLE_COLOR);
begin
  Color := TColor(Value);
end;  // TfrmContainerOcc.Set_Color 

{-------------------------------------------------------------------------------
}
procedure TfrmContainerOcc.Set_DoubleBuffered(Value: WordBool);
begin
  DoubleBuffered := Value;
end;  // TfrmContainerOcc.Set_DoubleBuffered 

{-------------------------------------------------------------------------------
}
procedure TfrmContainerOcc.Set_DropTarget(Value: WordBool);
begin
  DropTarget := Value;
end;  // TfrmContainerOcc.Set_DropTarget 

{-------------------------------------------------------------------------------
}
procedure TfrmContainerOcc.Set_Enabled(Value: WordBool);
begin
  Enabled := Value;
end;  // TfrmContainerOcc.Set_Enabled 

{-------------------------------------------------------------------------------
}
procedure TfrmContainerOcc.Set_Events(const Value: IRecorderDetailScreenEvents);
begin
  FDetailScreenEvents := Value;
end;  // TfrmContainerOcc.Set_Events 

{-------------------------------------------------------------------------------
}
procedure TfrmContainerOcc.Set_Font(const Value: IFontDisp);
begin
  SetOleFont(Font, Value);
end;  // TfrmContainerOcc.Set_Font 

{-------------------------------------------------------------------------------
}
procedure TfrmContainerOcc.Set_HelpFile(const Value: WideString);
begin
  HelpFile := String(Value);
end;  // TfrmContainerOcc.Set_HelpFile 

{-------------------------------------------------------------------------------
}
procedure TfrmContainerOcc.Set_KeyPreview(Value: WordBool);
begin
  KeyPreview := Value;
end;  // TfrmContainerOcc.Set_KeyPreview 

{-------------------------------------------------------------------------------
}
procedure TfrmContainerOcc.Set_PixelsPerInch(Value: Integer);
begin
  PixelsPerInch := Value;
end;  // TfrmContainerOcc.Set_PixelsPerInch 

{-------------------------------------------------------------------------------
}
procedure TfrmContainerOcc.Set_PrintScale(Value: TxPrintScale);
begin
  PrintScale := TPrintScale(Value);
end;  // TfrmContainerOcc.Set_PrintScale 

{-------------------------------------------------------------------------------
}
procedure TfrmContainerOcc.Set_Scaled(Value: WordBool);
begin
  Scaled := Value;
end;  // TfrmContainerOcc.Set_Scaled 

{-------------------------------------------------------------------------------
}
procedure TfrmContainerOcc.Set_ScreenSnap(Value: WordBool);
begin
  ScreenSnap := Value;
end;  // TfrmContainerOcc.Set_ScreenSnap 

{-------------------------------------------------------------------------------
}
procedure TfrmContainerOcc.Set_SelectedItemCaption(const Value: WideString);
begin
  fraContainer.lblName.Caption := Value;
end;  // TfrmContainerOcc.Set_SelectedItemCaption 

{-------------------------------------------------------------------------------
}
procedure TfrmContainerOcc.Set_SnapBuffer(Value: Integer);
begin
  SnapBuffer := Value;
end;  // TfrmContainerOcc.Set_SnapBuffer 

{-------------------------------------------------------------------------------
}
procedure TfrmContainerOcc.Set_Visible(Value: WordBool);
begin
  Visible := Value;
end;  // TfrmContainerOcc.Set_Visible 

{-------------------------------------------------------------------------------
}
procedure TfrmContainerOcc.WndProc(var Message: TMessage);
var
  msg: TMsg;
begin
  case Message.Msg of
    // TActiveXControl.TranslateAccelerator shall send navigation keys
    WM_GETDLGCODE:
      Message.Result := DLGC_WANTArrows + DLGC_Wanttab;
    // dispatch navigation keys to currently active child control
    WM_KEYDOWN, WM_SYSKEYDOWN:
      case Message.WParam of
        VK_LEFT,VK_RIGHT, VK_UP, VK_DOWN, VK_TAB, VK_HOME, VK_END:
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
    else
      inherited; // standard messages
  end;
end;  // TfrmContainerOcc.WndProc 

{-------------------------------------------------------------------------------
}
procedure TfrmContainerOcc._Set_Font(var Value: IFontDisp);
begin
  SetOleFont(Font, Value);
end;  // TfrmContainerOcc._Set_Font 

initialization
  TActiveFormFactory.Create(
    ComServer,
    TActiveFormControl,
    TfrmContainerOcc,
    Class_frmContainerOcc,
    7,
    '',
    OLEMISC_SIMPLEFRAME or OLEMISC_ACTSLIKELABEL,
    tmApartment);
end.


