{===============================================================================
  Unit:        UCEMain

  Defines:     TfrmUCEMain

  Description: Extended user configuration screen.

  Model:       UserConfigExtended.mpb

  Created:     December 2003

  Last revision information:
    $Revision: 9 $
    $Date: 26/01/07 14:50 $
    $Author: Johnvanbreda $

===============================================================================}

unit UCEMain;

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, ActiveX, AxCtrls,
  UserConfigExtended_TLB, StdVcl, Menus, StdCtrls, Grids, ExtCtrls, ComCtrls, ImageListButton,
  DataTypes, Contnrs, DataClasses, ADOInt, LuxembourgConstants, Variants, InterfaceDataModule,
  DSSDataTypes;

type
  TfrmUCEMain = class (TActiveForm, IfrmUCEMain)
    btnCancel: TImageListButton;
    btnEdit: TImageListButton;
    btnSave: TImageListButton;
    chkFinance: TCheckBox;
    chkMovements: TCheckBox;
    chkProcessQuickEntry: TCheckBox;
    chkQuickEntry: TCheckBox;
    Label1: TLabel;
    Label2: TLabel;
    Label4: TLabel;
    lblUserName: TLabel;
    lvUsers: TListView;
    pnlDetails: TPanel;
    sgPermissions: TStringGrid;
    procedure btnCancelClick(Sender: TObject);
    procedure btnEditClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure lvUsersChange(Sender: TObject; Item: TListItem; Change: TItemChange);
    procedure sgPermissionsClick(Sender: TObject);
    procedure sgPermissionsDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect; State:
        TGridDrawState);
  private
    FEditMode: TEditMode;
    FEvents: IfrmUCEMainEvents;
    FUserInfoList: TObjectList;
    procedure ActivateEvent(Sender: TObject);
    procedure AlignFormControls;
    procedure ClickEvent(Sender: TObject);
    procedure CreateEvent(Sender: TObject);
    procedure DblClickEvent(Sender: TObject);
    procedure DeactivateEvent(Sender: TObject);
    procedure DestroyEvent(Sender: TObject);
    procedure KeyPressEvent(Sender: TObject; var Key: Char);
    procedure LoadData;
    procedure LoadGrid;
    procedure LoadUsers;
    procedure PaintEvent(Sender: TObject);
    procedure RefreshDetails;
    procedure SaveDetails;
    procedure SetEditMode(Value: TEditMode);
    procedure UMLoaded(var Message: TMessage); message UM_LOADED;
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
    property EditMode: TEditMode read FEditMode write SetEditMode;
  end;

//==============================================================================
implementation

uses
  ComObj, ComServ, GeneralFunctions, ApplicationSettings, GeneralData, ResourceStrings,
  CollectionsModuleManager_TLB;

{$R *.DFM}

const
  CELL_UNCHECKED = '0';
  CELL_CHECKED   = '1';

  CHECKED_VALUES: Array[Boolean] of String = (CELL_UNCHECKED, CELL_CHECKED);

  COL_DOMAIN = 0;
  COL_BROWSE = 1;
  COL_QENTRY = 2;
  COL_ADD    = 3;
  COL_EDIT   = 4;

type
  TDomainAccess = class (TObject)
  private
    FAllowAdd: Boolean;
    FAllowBrowse: Boolean;
    FAllowEdit: Boolean;
    FAllowQuickEntry: Boolean;
    FDomainKey: TKeyString;
    FItemKey: TKeyString;
    FModified: Boolean;
    FTimestamp: TSQLSvrTimestamp;
    FDomainMask: LongWord;
    procedure SetAllowAdd(const Value: Boolean);
    procedure SetAllowBrowse(const Value: Boolean);
    procedure SetAllowEdit(const Value: Boolean);
    procedure SetAllowQuickEntry(const Value: Boolean);
  public
    property AllowAdd: Boolean read FAllowAdd write SetAllowAdd;
    property AllowBrowse: Boolean read FAllowBrowse write SetAllowBrowse;
    property AllowEdit: Boolean read FAllowEdit write SetAllowEdit;
    property AllowQuickEntry: Boolean read FAllowQuickEntry write SetAllowQuickEntry;
    property DomainKey: TKeyString read FDomainKey write FDomainKey;
    property ItemKey: TKeyString read FItemKey write FItemKey;
    property Modified: Boolean read FModified;
    property Timestamp: TSQLSvrTimestamp read FTimestamp write FTimestamp;
    property DomainMask: LongWord read FDomainMask write FDomainMask;
  end;

{-==============================================================================
    TfrmUCEMain
===============================================================================}
{-------------------------------------------------------------------------------
}
destructor TfrmUCEMain.Destroy;
var
  i: Integer;
begin
  DiscardAppSettings;
  if TdmGeneral.Allocated then TdmGeneral.Discard;

  with sgPermissions do
    for i := 0 to RowCount - 1 do
      if Assigned(Objects[COL_DOMAIN, i]) then Objects[COL_DOMAIN, i].Free;

  FUserInfoList.Free;

  inherited;
end;  // TfrmUCEMain.Destroy

{-------------------------------------------------------------------------------
}
procedure TfrmUCEMain.ActivateEvent(Sender: TObject);
begin
  if FEvents <> nil then FEvents.OnActivate;
end;  // TfrmUCEMain.ActivateEvent 

{-------------------------------------------------------------------------------
}
procedure TfrmUCEMain.AlignFormControls;
begin
  lvUsers.Height := Height - btnEdit.Height - 16;
  btnEdit.Top    := Height - btnEdit.Height - 4;
  
  pnlDetails.SetBounds(lvUsers.Left + lvUsers.Width + 4, 8,
                       Width - pnlDetails.Left - 4, Height - 12);
  sgPermissions.Width  := pnlDetails.Width - 16;
  sgPermissions.Height := pnlDetails.Height - 32 - sgPermissions.Top - 4;
  btnCancel.Top        := pnlDetails.Height - 32;
  btnCancel.Left       := pnlDetails.Width - 8 - btnCancel.Width;
  btnSave.Top          := btnCancel.Top;
  btnSave.Left         := btnCancel.Left - 8 - btnSave.Width;
  
  lvUsers.Anchors    := [akLeft,akTop,akRight,akBottom];
  btnEdit.Anchors    := [akLeft,akBottom];
  pnlDetails.Anchors := [akTop,akRight,akBottom];
end;  // TfrmUCEMain.AlignFormControls

{-------------------------------------------------------------------------------
}
procedure TfrmUCEMain.btnCancelClick(Sender: TObject);
begin
  RefreshDetails;
  EditMode := emBrowse;
end;  // TfrmUCEMain.btnCancelClick

{-------------------------------------------------------------------------------
}
procedure TfrmUCEMain.btnEditClick(Sender: TObject);
begin
  EditMode := emEdit;
end;  // TfrmUCEMain.btnEditClick 

{-------------------------------------------------------------------------------
}
procedure TfrmUCEMain.btnSaveClick(Sender: TObject);
begin
  SaveDetails;
  EditMode := emBrowse;
  RefreshDetails;
end;  // TfrmUCEMain.btnSaveClick 

{-------------------------------------------------------------------------------
}
procedure TfrmUCEMain.ClickEvent(Sender: TObject);
begin
  if FEvents <> nil then FEvents.OnClick;
end;  // TfrmUCEMain.ClickEvent 

{-------------------------------------------------------------------------------
}
procedure TfrmUCEMain.CreateEvent(Sender: TObject);
begin
  if FEvents <> nil then FEvents.OnCreate;
end;  // TfrmUCEMain.CreateEvent 

{-------------------------------------------------------------------------------
}
procedure TfrmUCEMain.DblClickEvent(Sender: TObject);
begin
  if FEvents <> nil then FEvents.OnDblClick;
end;  // TfrmUCEMain.DblClickEvent 

{-------------------------------------------------------------------------------
}
procedure TfrmUCEMain.DeactivateEvent(Sender: TObject);
begin
  if FEvents <> nil then FEvents.OnDeactivate;
end;  // TfrmUCEMain.DeactivateEvent 

{-------------------------------------------------------------------------------
}
procedure TfrmUCEMain.DefinePropertyPages(DefinePropertyPage: TDefinePropertyPage);
begin
  { Define property pages here.  Property pages are defined by calling
    DefinePropertyPage with the class id of the page.  For example,
      DefinePropertyPage(Class_frmExtendedUserConfigPage); }
end;  // TfrmUCEMain.DefinePropertyPages 

{-------------------------------------------------------------------------------
}
procedure TfrmUCEMain.DestroyEvent(Sender: TObject);
begin
  if FEvents <> nil then FEvents.OnDestroy;
end;  // TfrmUCEMain.DestroyEvent 

{-------------------------------------------------------------------------------
}
procedure TfrmUCEMain.EventSinkChanged(const EventSink: IUnknown);
begin
  FEvents := EventSink as IfrmUCEMainEvents;
  inherited EventSinkChanged(EventSink);
end;  // TfrmUCEMain.EventSinkChanged 

{-------------------------------------------------------------------------------
}
function TfrmUCEMain.Get_Active: WordBool;
begin
  Result := Active;
end;  // TfrmUCEMain.Get_Active 

{-------------------------------------------------------------------------------
}
function TfrmUCEMain.Get_AlignDisabled: WordBool;
begin
  Result := AlignDisabled;
end;  // TfrmUCEMain.Get_AlignDisabled 

{-------------------------------------------------------------------------------
}
function TfrmUCEMain.Get_AutoScroll: WordBool;
begin
  Result := AutoScroll;
end;  // TfrmUCEMain.Get_AutoScroll

{-------------------------------------------------------------------------------
}
function TfrmUCEMain.Get_AutoSize: WordBool;
begin
  Result := AutoSize;
end;  // TfrmUCEMain.Get_AutoSize 

{-------------------------------------------------------------------------------
}
function TfrmUCEMain.Get_AxBorderStyle: TxActiveFormBorderStyle;
begin
  Result := Ord(AxBorderStyle);
end;  // TfrmUCEMain.Get_AxBorderStyle 

{-------------------------------------------------------------------------------
}
function TfrmUCEMain.Get_Caption: WideString;
begin
  Result := WideString(Caption);
end;  // TfrmUCEMain.Get_Caption 

{-------------------------------------------------------------------------------
}
function TfrmUCEMain.Get_Color: OLE_COLOR;
begin
  Result := OLE_COLOR(Color);
end;  // TfrmUCEMain.Get_Color 

{-------------------------------------------------------------------------------
}
function TfrmUCEMain.Get_DoubleBuffered: WordBool;
begin
  Result := DoubleBuffered;
end;  // TfrmUCEMain.Get_DoubleBuffered 

{-------------------------------------------------------------------------------
}
function TfrmUCEMain.Get_DropTarget: WordBool;
begin
  Result := DropTarget;
end;  // TfrmUCEMain.Get_DropTarget 

{-------------------------------------------------------------------------------
}
function TfrmUCEMain.Get_Enabled: WordBool;
begin
  Result := Enabled;
end;  // TfrmUCEMain.Get_Enabled 

{-------------------------------------------------------------------------------
}
function TfrmUCEMain.Get_Font: IFontDisp;
begin
  GetOleFont(Font, Result);
end;  // TfrmUCEMain.Get_Font

{-------------------------------------------------------------------------------
}
function TfrmUCEMain.Get_HelpFile: WideString;
begin
  Result := WideString(HelpFile);
end;  // TfrmUCEMain.Get_HelpFile 

{-------------------------------------------------------------------------------
}
function TfrmUCEMain.Get_KeyPreview: WordBool;
begin
  Result := KeyPreview;
end;  // TfrmUCEMain.Get_KeyPreview 

{-------------------------------------------------------------------------------
}
function TfrmUCEMain.Get_PixelsPerInch: Integer;
begin
  Result := PixelsPerInch;
end;  // TfrmUCEMain.Get_PixelsPerInch

{-------------------------------------------------------------------------------
}
function TfrmUCEMain.Get_PrintScale: TxPrintScale;
begin
  Result := Ord(PrintScale);
end;  // TfrmUCEMain.Get_PrintScale 

{-------------------------------------------------------------------------------
}
function TfrmUCEMain.Get_Scaled: WordBool;
begin
  Result := Scaled;
end;  // TfrmUCEMain.Get_Scaled 

{-------------------------------------------------------------------------------
}
function TfrmUCEMain.Get_ScreenSnap: WordBool;
begin
  Result := ScreenSnap;
end;  // TfrmUCEMain.Get_ScreenSnap 

{-------------------------------------------------------------------------------
}
function TfrmUCEMain.Get_SnapBuffer: Integer;
begin
  Result := SnapBuffer;
end;  // TfrmUCEMain.Get_SnapBuffer 

{-------------------------------------------------------------------------------
}
function TfrmUCEMain.Get_Visible: WordBool;
begin
  Result := Visible;
end;  // TfrmUCEMain.Get_Visible

{-------------------------------------------------------------------------------
}
function TfrmUCEMain.Get_VisibleDockClientCount: Integer;
begin
  Result := VisibleDockClientCount;
end;  // TfrmUCEMain.Get_VisibleDockClientCount 

{-------------------------------------------------------------------------------
}
procedure TfrmUCEMain.Initialize;
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
  
  AlignFormControls;
  LoadData;
end;  // TfrmUCEMain.Initialize 

{-------------------------------------------------------------------------------
}
procedure TfrmUCEMain.KeyPressEvent(Sender: TObject; var Key: Char);
var
  TempKey: SmallInt;
begin
  TempKey := Smallint(Key);
  if FEvents <> nil then FEvents.OnKeyPress(TempKey);
  Key := Char(TempKey);
end;  // TfrmUCEMain.KeyPressEvent 

{-------------------------------------------------------------------------------
}
procedure TfrmUCEMain.LoadData;
begin
  FUserInfoList := TObjectList.Create;

  LoadGrid;
  LoadUsers;
  PostMessage(Handle, UM_LOADED, 0, 0);
end;  // TfrmUCEMain.LoadData 

{-------------------------------------------------------------------------------
}
procedure TfrmUCEMain.LoadGrid;
var
  lFirst: Boolean;
  lAccess: TDomainAccess;
begin
  lFirst := True;

  with sgPermissions do begin
    Cells[COL_DOMAIN, 0] := ResStr_Domain;
    Cells[COL_BROWSE, 0] := ResStr_Browse;
    Cells[COL_QENTRY, 0] := ResStr_QuickEntry;
    Cells[COL_ADD,    0] := ResStr_Add;
    Cells[COL_EDIT,   0] := ResStr_Edit;

    // Populate the grid
    with dmGeneral.GetRecordset('usp_Domains_Select', []) do begin
      while not Eof do begin
        if lFirst then lFirst := False
                  else RowCount := RowCount + 1;

        Cells[COL_DOMAIN, RowCount - 1]   := Fields['Item_Name'].Value;
        // Make sure each domain in list has a Domain Access object.
        lAccess := TDomainAccess.Create;
        lAccess.DomainKey := Fields['Domain_Key'].Value;
        lAccess.DomainMask := Fields['Domain_Mask'].Value;
        Objects[COL_DOMAIN, RowCount - 1] := lAccess;
        MoveNext;
      end;
      Close;
    end;
    Enabled := Assigned(Objects[COL_DOMAIN, FixedRows]);
  end;
end;  // TfrmUCEMain.LoadGrid

{-------------------------------------------------------------------------------
}
procedure TfrmUCEMain.LoadUsers;
begin
  // Populate the users list
  with dmGeneral.GetRecordset('usp_Users_Select', []) do begin
    while not Eof do begin
      with lvUsers.Items.Add do begin
        Caption := Fields['User_Name'].Value;
        Data := TKeyData.Create;
        TKeyData(Data).ItemKey := Fields['Name_Key'].Value;
        TKeyData(Data).ItemAdditional := IntToStr(Fields['Security_Level'].Value - 1);
        // And to easily clear all objects, use an ObjectList.
        FUserInfoList.Add(TKeyData(Data));
      end;
      MoveNext;
    end;
    Close;
  end;
end;  // TfrmUCEMain.LoadUsers

{-------------------------------------------------------------------------------
}
procedure TfrmUCEMain.lvUsersChange(Sender: TObject; Item: TListItem; Change: TItemChange);
begin
  with lvUsers do begin
    btnEdit.Enabled := TUserAccessLevel(AppSettings.UserAccessLevel) = ualAdmin;

    if Selected <> nil then begin
      RefreshDetails;
      if Assigned(Selected.Data) then
        btnEdit.Enabled := btnEdit.Enabled or
                           (TKeyData(Selected.Data).ItemKey = AppSettings.UserID);
    end;
  end;
end;  // TfrmUCEMain.lvUsersChange

{-------------------------------------------------------------------------------
}
procedure TfrmUCEMain.PaintEvent(Sender: TObject);
begin
  if FEvents <> nil then FEvents.OnPaint;
end;  // TfrmUCEMain.PaintEvent

{-------------------------------------------------------------------------------
}
procedure TfrmUCEMain.RefreshDetails;
var
  i: Integer;
  lAccess: TDomainAccess;
begin
  // Reset grid, clear all checkboxes as UserDomainAccess table might not contain
  // a record for each listed domain for the selected user.
  with sgPermissions do
    if Enabled then
      for i := 1 to RowCount - 1 do begin
        lAccess := TDomainAccess(Objects[COL_DOMAIN, i]);
        if Assigned(lAccess) then begin
          lAccess.ItemKey   := '';
          lAccess.TimeStamp := Null;
        end;
        Cells[COL_BROWSE, i] := CELL_UNCHECKED;
        Cells[COL_QENTRY, i] := CELL_UNCHECKED;
        Cells[COL_ADD,    i] := CELL_UNCHECKED;
        Cells[COL_EDIT,   i] := CELL_UNCHECKED;
      end;

  if Assigned(lvUsers.Selected) then begin
    lblUserName.Caption := lvUsers.Selected.Caption;
    // Fields behind checkboxes come from User table, therefore always have a value.
    with dmGeneral.GetRecordset('usp_User_Select',
                                ['@NameKey', TKeyData(lvUsers.Selected.Data).ItemKey]) do
    begin
      chkQuickEntry.Checked        := Fields['Allow_Quick_Entry'].Value;
      chkMovements.Checked         := Fields['Allow_Movement_Edit'].Value;
      chkProcessQuickEntry.Checked := Fields['Allow_Quick_Entry_Processing'].Value;
      chkFinance.Checked           := Fields['Allow_Finance'].Value;
      Close;
    end;

    if sgPermissions.Enabled then
      with sgPermissions,
           dmGeneral.GetRecordset('usp_UserDomainAccess_Select_ForNameKey',
                                  ['@NameKey', TKeyData(lvUsers.Selected.Data).ItemKey]) do
      begin
        while not Eof do begin
          // Get the appropriate object to update.
          i := Cols[COL_DOMAIN].IndexOf(Fields['Domain_Name'].Value);
          // if found (it might not be if the user was granted access to a domain
          // that is subsequently made not occurrences
          if i>-1 then begin
            lAccess := TDomainAccess(Objects[COL_DOMAIN, i]);
            with lAccess do begin
              ItemKey     := Fields['User_Domain_Access_Key'].Value;
              Timestamp   := Fields['Timestamp'].Value;
              Cells[COL_BROWSE, i] := CHECKED_VALUES[Boolean(Fields['Allow_Browse'].Value)];
              Cells[COL_QENTRY, i] := CHECKED_VALUES[Boolean(Fields['Allow_Quick_Entry'].Value)];
              Cells[COL_ADD,    i] := CHECKED_VALUES[Boolean(Fields['Allow_Add'].Value)];
              Cells[COL_EDIT,   i] := CHECKED_VALUES[Boolean(Fields['Allow_Edit'].Value)];
            end;
          end;
          MoveNext;
        end;
        Close;
      end;
  end;
end;  // TfrmUCEMain.RefreshDetails 

{-------------------------------------------------------------------------------
}
procedure TfrmUCEMain.SaveDetails;
var
  lNameKey: TKeyString;
  lParams: Array of Variant;
  i: Integer;
  lAccess: TDomainAccess;
  lMask: LongWord;
begin
  lParams := nil;
  dmGeneral.Connection.BeginTrans;
  try
    lNameKey := TKeyData(lvUsers.Selected.Data).ItemKey;

    dmGeneral.RunUpdateStoredProc('usp_User_Update_ForSystemAccess',
                                  ['@NameKey', lNameKey,
                                   '@QuickEntry', Ord(chkQuickEntry.Checked),
                                   '@Movements', Ord(chkMovements.Checked),
                                   '@ProcessQuickEntry', Ord(chkProcessQuickEntry.Checked),
                                   '@Finance', Ord(chkFinance.Checked)]);
    lMask := 0;
    if sgPermissions.Enabled then
      for i := 1 to sgPermissions.RowCount - 1 do begin
        lAccess := TDomainAccess(sgPermissions.Objects[COL_DOMAIN, i]);
        lParams := VarArrayOf(['@Key', lAccess.ItemKey,         // for update only
                               '@NameKey', lNameKey,            // for insert only
                               '@DomainKey', lAccess.DomainKey, // for insert only
                               '@Browse', StrToInt(sgPermissions.Cells[COL_BROWSE, i]),
                               '@QuickEntry', StrToInt(sgPermissions.Cells[COL_QENTRY, i]),
                               '@Add', StrToInt(sgPermissions.Cells[COL_ADD, i]),
                               '@Edit', StrToInt(sgPermissions.Cells[COL_EDIT, i]),
                               '@Timestamp', lAccess.Timestamp]);   // for update only

        if lAccess.ItemKey = '' then
          dmGeneral.RunInsertStoredProc('User_Domain_Access', 'usp_UserDomainAccess_Insert',
                                        lParams, '@Key')
        else
          dmGeneral.RunUpdateStoredProc('usp_UserDomainAccess_Update', lParams);

        if sgPermissions.Cells[COL_BROWSE, i] = CELL_CHECKED then
          lMask := lMask or lAccess.DomainMask;
      end;

    dmGeneral.Connection.CommitTrans;

    // Ensure that user info up to date, if user is editing self
    if lNameKey = AppSettings.UserID then begin
      AppSettings.DomainMask := lMask;
      AppSettings.Refresh;
    end;
  except
    dmGeneral.Connection.RollbackTrans;
    Raise;
  end;
end;  // TfrmUCEMain.SaveDetails 

{-------------------------------------------------------------------------------
}
procedure TfrmUCEMain.SetEditMode(Value: TEditMode);
var
  lEditing: Boolean;
  lUserAccess: TUserAccessLevel;
begin
  lUserAccess := TUserAccessLevel(StrToInt(TKeyData(lvUsers.Selected.Data).ItemAdditional));
  FEditMode := Value;
  lEditing  := FEditMode = emEdit;
  lvUsers.Enabled := not lEditing;
  btnEdit.Enabled := not lEditing;
  chkQuickEntry.Enabled := lEditing and (lUserAccess <> ualReadOnly);
  chkMovements.Enabled  := lEditing and (lUserAccess in [ualAddOnly, ualFullUser, ualAdmin]);
  chkFinance.Enabled    := lEditing and (lUserAccess in [ualFullUser, ualAdmin]);
  chkProcessQuickEntry.Enabled := lEditing and (lUserAccess in [ualFullUser, ualAdmin]);
  btnSave.Enabled       := lEditing;
  btnCancel.Enabled     := lEditing;
end;  // TfrmUCEMain.SetEditMode

{-------------------------------------------------------------------------------
}
procedure TfrmUCEMain.Set_AutoScroll(Value: WordBool);
begin
  AutoScroll := Value;
end;  // TfrmUCEMain.Set_AutoScroll 

{-------------------------------------------------------------------------------
}
procedure TfrmUCEMain.Set_AutoSize(Value: WordBool);
begin
  AutoSize := Value;
end;  // TfrmUCEMain.Set_AutoSize 

{-------------------------------------------------------------------------------
}
procedure TfrmUCEMain.Set_AxBorderStyle(Value: TxActiveFormBorderStyle);
begin
  AxBorderStyle := TActiveFormBorderStyle(Value);
end;  // TfrmUCEMain.Set_AxBorderStyle 

{-------------------------------------------------------------------------------
}
procedure TfrmUCEMain.Set_Caption(const Value: WideString);
begin
  Caption := TCaption(Value);
end;  // TfrmUCEMain.Set_Caption

{-------------------------------------------------------------------------------
}
procedure TfrmUCEMain.Set_Color(Value: OLE_COLOR);
begin
  Color := TColor(Value);
end;  // TfrmUCEMain.Set_Color 

{-------------------------------------------------------------------------------
}
procedure TfrmUCEMain.Set_DoubleBuffered(Value: WordBool);
begin
  DoubleBuffered := Value;
end;  // TfrmUCEMain.Set_DoubleBuffered 

{-------------------------------------------------------------------------------
}
procedure TfrmUCEMain.Set_DropTarget(Value: WordBool);
begin
  DropTarget := Value;
end;  // TfrmUCEMain.Set_DropTarget 

{-------------------------------------------------------------------------------
}
procedure TfrmUCEMain.Set_Enabled(Value: WordBool);
begin
  Enabled := Value;
end;  // TfrmUCEMain.Set_Enabled 

{-------------------------------------------------------------------------------
}
procedure TfrmUCEMain.Set_Font(const Value: IFontDisp);
begin
  SetOleFont(Font, Value);
end;  // TfrmUCEMain.Set_Font 

{-------------------------------------------------------------------------------
}
procedure TfrmUCEMain.Set_HelpFile(const Value: WideString);
begin
  HelpFile := String(Value);
end;  // TfrmUCEMain.Set_HelpFile 

{-------------------------------------------------------------------------------
}
procedure TfrmUCEMain.Set_KeyPreview(Value: WordBool);
begin
  KeyPreview := Value;
end;  // TfrmUCEMain.Set_KeyPreview

{-------------------------------------------------------------------------------
}
procedure TfrmUCEMain.Set_PixelsPerInch(Value: Integer);
begin
  PixelsPerInch := Value;
end;  // TfrmUCEMain.Set_PixelsPerInch 

{-------------------------------------------------------------------------------
}
procedure TfrmUCEMain.Set_PrintScale(Value: TxPrintScale);
begin
  PrintScale := TPrintScale(Value);
end;  // TfrmUCEMain.Set_PrintScale 

{-------------------------------------------------------------------------------
}
procedure TfrmUCEMain.Set_Scaled(Value: WordBool);
begin
  Scaled := Value;
end;  // TfrmUCEMain.Set_Scaled 

{-------------------------------------------------------------------------------
}
procedure TfrmUCEMain.Set_ScreenSnap(Value: WordBool);
begin
  ScreenSnap := Value;
end;  // TfrmUCEMain.Set_ScreenSnap 

{-------------------------------------------------------------------------------
}
procedure TfrmUCEMain.Set_SnapBuffer(Value: Integer);
begin
  SnapBuffer := Value;
end;  // TfrmUCEMain.Set_SnapBuffer 

{-------------------------------------------------------------------------------
}
procedure TfrmUCEMain.Set_Visible(Value: WordBool);
begin
  Visible := Value;
end;  // TfrmUCEMain.Set_Visible 

{-------------------------------------------------------------------------------
}
procedure TfrmUCEMain.sgPermissionsClick(Sender: TObject);
var
  lUserAccess: TUserAccessLevel;
begin
  lUserAccess := TUserAccessLevel(StrToInt(TKeyData(lvUsers.Selected.Data).ItemAdditional));
  if EditMode = emBrowse then Exit;

  with sgPermissions do
    if (Col = COL_BROWSE) or
       ((Col = COL_QENTRY) and (lUserAccess <> ualReadOnly)) or
       ((Col = COL_ADD) and (lUserAccess in [ualAddOnly, ualFullUser, ualAdmin])) or
       ((Col = COL_EDIT) and (lUserAccess in [ualFullUser, ualAdmin])) then
    begin
      if Cells[Col, Row] = CELL_CHECKED then Cells[Col, Row] := CELL_UNCHECKED
                                        else Cells[Col, Row] := CELL_CHECKED;
      case Col of
        COL_BROWSE:
            if Cells[COL_BROWSE, Row] = CELL_UNCHECKED then begin
              Cells[COL_EDIT, Row] := CELL_UNCHECKED;
              Cells[COL_ADD,  Row] := CELL_UNCHECKED;
            end;
        COL_ADD:
            if Cells[COL_ADD, Row] = CELL_CHECKED then
              Cells[COL_BROWSE, Row] := CELL_CHECKED;
        COL_EDIT:
            if Cells[COL_EDIT, Row] = CELL_CHECKED then
              Cells[COL_BROWSE, Row] := CELL_CHECKED;
      end;
    end;
end;  // TfrmUCEMain.sgPermissionsClick

{-------------------------------------------------------------------------------
}
procedure TfrmUCEMain.sgPermissionsDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect; 
    State: TGridDrawState);
var
  lUserAccess: TUserAccessLevel;
begin
  lUserAccess := TUserAccessLevel(StrToInt(TKeyData(lvUsers.Selected.Data).ItemAdditional));
  with sgPermissions do
    if (ACol > 0) and (ARow > 0) then begin
      if not Enabled then
        Canvas.Brush.Color := clBtnFace;

      Canvas.FillRect(Rect);
      DrawCheckBox(Canvas,
                   Rect.Left + (ColWidths[ACol] - 13) div 2,
                   Rect.Top + (RowHeights[ARow] - 13) div 2,
                   (Cells[ACol, ARow] = CELL_CHECKED) and Enabled,
                   Enabled and
                   ((ACol = COL_BROWSE) or
                    ((ACol = COL_QENTRY) and (lUserAccess <> ualReadOnly)) or
                    ((ACol = COL_ADD) and (lUserAccess in [ualAddOnly, ualFullUser, ualAdmin])) or
                    ((ACol = COL_EDIT) and (lUserAccess in [ualFullUser, ualAdmin]))
                   ));
    end else begin
      Canvas.FillRect(Rect);
      DrawChoppedText(Cells[ACol, ARow], Canvas, Rect, 2);
    end;
end;  // TfrmUCEMain.sgPermissionsDrawCell 

{-------------------------------------------------------------------------------
}
procedure TfrmUCEMain.UMLoaded(var Message: TMessage);
begin
  lvUsers.ItemIndex := 0;
  lvUsersChange(nil, nil, ctState);
  lvUsers.SetFocus;
end;  // TfrmUCEMain.UMLoaded 

{-------------------------------------------------------------------------------
}
procedure TfrmUCEMain.WndProc(var Message: TMessage);
var
  lMsg: TMsg;
begin
  case Message.Msg of
    // TActiveXControl.TranslateAccelerator shall send navigation keys
    WM_GETDLGCODE:
        Message.Result := DLGC_WANTARROWS + DLGC_WANTTAB + DLGC_WANTCHARS;
    // dispatch navigation keys to currently active child control
    WM_KEYDOWN, WM_SYSKEYDOWN, WM_CHAR:
        if (ActiveControl <> nil) then
        begin
          lMsg.hwnd := ActiveControl.Handle;
          lMsg.Message := Message.Msg;
          lMsg.WParam := Message.WParam;
          lMsg.LParam := Message.LParam;
          lMsg.Time := 0;
          lMsg.pt := Point(0,0);
          DispatchMessage(lMsg);
        end;
    else
      inherited; // standard messages
  end;
end;  // TfrmUCEMain.WndProc 

{-------------------------------------------------------------------------------
}
procedure TfrmUCEMain._Set_Font(var Value: IFontDisp);
begin
  SetOleFont(Font, Value);
end;  // TfrmUCEMain._Set_Font 

{-==============================================================================
    TDomainAccess
===============================================================================}
{-------------------------------------------------------------------------------
}
procedure TDomainAccess.SetAllowAdd(const Value: Boolean);
begin
  if FAllowAdd <> Value then begin
    FAllowAdd := Value;
    FModified := True;
  end;
end;  // TDomainAccess.SetAllowAdd 

{-------------------------------------------------------------------------------
}
procedure TDomainAccess.SetAllowBrowse(const Value: Boolean);
begin
  if FAllowBrowse <> Value then begin
    FAllowBrowse := Value;
    FModified := True;
  end;
end;  // TDomainAccess.SetAllowBrowse 

{-------------------------------------------------------------------------------
}
procedure TDomainAccess.SetAllowEdit(const Value: Boolean);
begin
  if FAllowEdit <> Value then begin
    FAllowEdit := Value;
    FModified := True;
  end;
end;  // TDomainAccess.SetAllowEdit 

{-------------------------------------------------------------------------------
}
procedure TDomainAccess.SetAllowQuickEntry(const Value: Boolean);
begin
  if FAllowQuickEntry <> Value then begin
    FAllowQuickEntry := Value;
    FModified := True;
  end;
end;  // TDomainAccess.SetAllowQuickEntry 

initialization
  TActiveFormFactory.Create(
    ComServer,
    TActiveFormControl,
    TfrmUCEMain,
    Class_frmUCEMain,
    1,
    '',
    OLEMISC_SIMPLEFRAME or OLEMISC_ACTSLIKELABEL,
    tmApartment);
end.
