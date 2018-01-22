{===============================================================================
  Unit:        FrameSources

  Defines:     TfraSources

  Description:

  Model:       CollectionsCommonAndGeneral.mpb

  Created:    May 2003

  Last revision information:
    $Revision: 23 $
    $Date: 15/12/06 10:37 $
    $Author: Ericsalmon $

===============================================================================}

unit FrameSources;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, BaseTabSheetFrameUnit, StdCtrls, Grids, ImageListButton, ExtCtrls,
  InterfaceDataModule, DssStringGrid, LuxembourgDataClasses, ADOInt, DropTarget,
  DataClasses, ComboListID, LuxIDComboBox, EasyShell, Validation, SearchManager,
  DSSDataTypes, Recorder2000_TLB, ExceptionForm;

type
  TInternalRefItem = class (TLuxGridDataItem)
  private
    FDocument: String;
    FOriginal: Boolean;
    FRefType: String;
    FSourceKey: TKeyString;
    procedure SetOriginal(Value: Boolean);
  protected
    procedure GetData(const Column: Integer; var AText: String; var AKey: TKeyString); 
        override;
    procedure InitFromRecord(AFields: Fields); override;
    procedure SetData(const Column: Integer; const AText: String; const AKey: TKeyString); 
        override;
    procedure ValidateData; override;
  public
    property Document: String read FDocument write FDocument;
    property Original: Boolean read FOriginal write SetOriginal;
    property RefType: String read FRefType write FRefType;
    property SourceKey: TKeyString read FSourceKey write FSourceKey;
  end;
  
  TInternalRefList = class (TLuxGridDataList)
  private
    FTableName: String;
  protected
    function AllowedAddOnKeyDown: Boolean; override;
    procedure DoAddition(AItem: TLuxCachedDataItem); override;
    procedure DoDeletion(AItem: TLuxCachedDataItem); override;
    procedure DoModification(AItem: TLuxCachedDataItem); override;
    function GetRecordset: _Recordset; override;
  public
    property TableName: String read FTableName write FTableName;
  end;
  
  TExternalRefItem = class (TLuxStringDataItem)
  private
    FFileName: String;
    procedure SetFileName(const Value: String);
  protected
    procedure InitFromRecord(AFields: Fields); override;
  public
    property FileName: String read FFileName write SetFileName;
  end;
  
  TExternalRefList = class (TLuxStringDataList)
  private
    FTableName: String;
  protected
    procedure DoAddition(AItem: TLuxCachedDataItem); override;
    procedure DoDeletion(AItem: TLuxCachedDataItem); override;
    function GetRecordset: _Recordset; override;
    function GetText(AItem: TLuxCachedDataItem): String; override;
  public
    property TableName: String read FTableName write FTableName;
  end;
  
  {-----------------------------------------------------------------------------
    Tab page component that displays and allows editing of the sources associated with a 
    specific record in the database.  The screen is divided into 2 halves, the top half 
    refering to sources in the Documents screen, and the bottom half refering to external file 
    and internet links.
    The component is linked to a specific record in any table by filtering on the Record_Key 
    and Table_Name fields in the Source_Join to identify the sources for the record.
  }
  TfraSources = class (TBaseTabSheetFrame)
    btnAddExternalRef: TImageListButton;
    btnAddInternalRef: TImageListButton;
    btnAddURL: TImageListButton;
    btnGetInternalRef: TImageListButton;
    btnRemoveExternalRef: TImageListButton;
    btnRemoveInternalRef: TImageListButton;
    btnViewExternalRef: TButton;
    cmbOriginal: TLuxIDComboBox;
    dlgOpenFile: TOpenDialog;
    Label8: TLabel;
    lbExternalRefs: TListBox;
    lblExternalRefs: TLabel;
    sgInternalRefs: TDSSStringGrid;
    shpGrid: TShape;
    shpList: TShape;
    procedure btnAddExternalRefClick(Sender: TObject);
    procedure btnAddInternalRefClick(Sender: TObject);
    procedure btnAddURLClick(Sender: TObject);
    procedure btnGetInternalRefClick(Sender: TObject);
    procedure btnRemoveExternalRefClick(Sender: TObject);
    procedure btnRemoveInternalRefClick(Sender: TObject);
    procedure btnViewExternalRefClick(Sender: TObject);
    procedure cmbOriginalPopulate(Sender: TObject);
    procedure FrameResize(Sender: TObject);
    procedure lbExternalRefsClick(Sender: TObject);
    procedure lbExternalRefsMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure sgInternalRefsDblClick(Sender: TObject);
  private
    FExternalRefsList: TExternalRefList;
    FInternalRefsList: TInternalRefList;
    function AddExternalReference(AName: String): Integer;
    function AddInternalReference(ASourceKey: TKeyString; var AText: String): Integer;
    procedure DropExternalRefs(const Sender: TObject; const AFormat: Integer; const 
        ASourceData: TKeyList; const ATextStrings: TStringList; var AHandled: Boolean);
    procedure DropInternalRefs(const Sender: TObject; const AFormat: Integer; const 
        ASourceData: TKeyList; const ATextStrings: TStringList; var AHandled: Boolean);
    procedure EnableBtnViewExternalRef;
    procedure ReturnReferenceData(const AKeyList: IKeyList);
  protected
    procedure DeleteData; override;
    procedure EnableControls(AEnabled: Boolean); override;
    function GetHasData: Boolean; virtual;
    procedure LoadData; override;
    procedure RegisterControls; override;
    procedure RegisterDragDropcomponents; override;
    procedure SaveData; override;
    procedure SetKey(Value: TKeyString); override;
    procedure ValidateData; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property HasData: Boolean read GetHasData;
  end;
  
//==============================================================================
implementation

uses
  BaseDetailFrameUnit, GeneralData, LuxembourgConstants, ResourceStrings,
  GeneralFunctions, ComObj, LuxembourgFunctions;

{$R *.dfm}

type
  TSingleDocumentList = class(TInterfacedObject, IKeyList, IKeyItem)
  private
    FDocumentKey: WideString;
  protected { IDispatch }
    function GetTypeInfoCount(out Count: Integer):
        HResult; stdcall;
    function GetTypeInfo(Index, LocaleID: Integer; out TypeInfo):
        HResult; stdcall;
    function GetIDsOfNames(const IID: TGUID; Names: Pointer;
        NameCount, LocaleID: Integer; DispIDs: pointer):
        HResult; stdcall;
    function Invoke(DispID: Integer; const IID: TGUID;
        LocaleID: Integer; Flags: Word; var Params;
        varResult, ExepInfo, ArgErr: Pointer):
        HResult; stdcall;
  protected {IKeyList}
    function Get_ItemCount: Integer; safecall;
    function Get_TableName: WideString; safecall;
    function GetKeyItem(iIndex: Integer): IKeyItem; safecall;
    property ItemCount: Integer read Get_ItemCount;
    property TableName: WideString read Get_TableName;
  protected {IKeyItem}  
    function Get_KeyField1: WideString; safecall;
    function Get_KeyField2: WideString; safecall;
    property KeyField1: WideString read Get_KeyField1;
    property KeyField2: WideString read Get_KeyField2;
  public
    constructor Create(const DocumentKey: WideString);
  end;
  
{-==============================================================================
    TfraSources
===============================================================================}
{-------------------------------------------------------------------------------
  Create the lists and set up the string grids. 
}
constructor TfraSources.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  
  FInternalRefsList := TInternalRefList.Create(TInternalRefItem, sgInternalRefs);
  FExternalRefsList := TExternalRefList.Create(TExternalRefItem, lbExternalRefs.Items);
  
  sgInternalRefs.DrawRichText := False;
  sgInternalRefs.Rows[0].CommaText := ResStr_Document + ',' +
                                      ResStr_Original + ',' +
                                      ResStr_Type;
  sgInternalRefs.ColumnsInfo[0].ReadOnly := True;
  sgInternalRefs.ColumnsInfo[0].RichTextContent := True;
  sgInternalRefs.ColumnsInfo[1].WinControl := cmbOriginal;
  sgInternalRefs.ColumnsInfo[2].ReadOnly := True;
  // Hide return data button as this won't work outside Recorder
  if CompareText(Application.Title, 'Thesaurus Editor')=0 then
    btnGetInternalRef.Visible := False;
end;  // TfraSources.Create 

{-------------------------------------------------------------------------------
  Destructor frees the lists. 
}
destructor TfraSources.Destroy;
begin
  FInternalRefsList.Free;
  FExternalRefsList.Free;
  
  inherited Destroy;
end;  // TfraSources.Destroy 

{-------------------------------------------------------------------------------
  Adds an external reference. 
}
function TfraSources.AddExternalReference(AName: String): Integer;
var
  i: Integer;
  lItem: TExternalRefItem;
begin
  with lbExternalRefs do
    for i := 0 to Items.Count - 1 do
      if CompareText(Items[i], AName) = 0 then begin
        Result := i;
        Exit;
      end;
  
  lItem := TExternalRefItem.CreateNew(FExternalRefsList);
  lItem.FileName := AName;
  FExternalRefsList.AddNew(lItem);
  EnableBtnViewExternalRef;
  ResetListBoxExtent(lbExternalRefs);
  Result := lbExternalRefs.Items.Count - 1;
end;  // TfraSources.AddExternalReference 

{-------------------------------------------------------------------------------
  Adds an internal reference. 
}
function TfraSources.AddInternalReference(ASourceKey: TKeyString; var AText: String): Integer;
var
  lRecordset: _Recordset;
  lItem: TInternalRefItem;
  i: Integer;
  lText: String;
begin
  // Get recordset, so the name of the ref can be displayed in the "no duplicates" message.
  lRecordset := dmGeneral.GetRecordset('usp_Reference_Select_ForInternalSources',
                                      ['@Key', ASourceKey]);
  
  // Check the item isn't already in the grid.
  for i := 1 to sgInternalRefs.RowCount - 1 do
    if TInternalRefItem(sgInternalRefs.Objects[0, i]).SourceKey = ASourceKey then begin
      lText := RichTextToText(Self, lRecordset.Fields['Document'].Value);
      lText := StringReplace(lText, #13#10, ' ', [rfReplaceAll]);
      MessageDlg(Format(ResStr_DuplicateReference, [lText]), mtInformation, [mbOk], 0);
      Result := i;
      Exit;
    end;
  
  // Add the item to the list. However, if the grid is empty there will still
  // already be an item in the list. Hence, make the new item point to the
  // empty item that is already there.
  if TInternalRefItem(sgInternalRefs.Objects[0, 1]).Document = '' then
    lItem := TInternalRefItem(sgInternalRefs.Objects[0, 1])
  else begin
    lItem := TInternalRefItem.CreateNew(FInternalRefsList);
    FInternalRefsList.AddNew(lItem);
  end;
  
  // Fill the lDocumentItem object with data
  with lItem do begin
    SetItemKey(VarToStr(lRecordset.Fields['Item_Key'].Value));
    SourceKey := ASourceKey;
    Document  := lRecordset.Fields['Document'].Value;
    RefType   := lRecordset.Fields['Ref_Type'].Value;
    if VarToStr(lRecordset.Fields['Original'].Value) = '' then
      Original := False
    else
      Original := lRecordset.Fields['Original'].Value;
  end;
  
  // Need to refresh the item or nothing will be shown
  FInternalRefsList.RefreshItemDisplay(lItem);
  Result := sgInternalRefs.RowCount - 1;
end;  // TfraSources.AddInternalReference 

{-------------------------------------------------------------------------------
  Handles a click on the add external ref button. 
}
procedure TfraSources.btnAddExternalRefClick(Sender: TObject);
begin
  inherited;
  
  if dlgOpenFile.Execute then
    lbExternalRefs.ItemIndex := AddExternalReference(dlgOpenFile.FileName);
  EnableControls(True);
end;  // TfraSources.btnAddExternalRefClick 

{-------------------------------------------------------------------------------
  Handles a click on the add internal ref button. 
}
procedure TfraSources.btnAddInternalRefClick(Sender: TObject);
var
  lSourceKey, lText: String;
begin
  inherited;
  
  // Shows the find dialog to search for all references. lKey and lText will
  // come back containing values if successful.
  if DoCheck(lSourceKey, lText, stInternalReferences, '', '') then
    sgInternalRefs.Row := AddInternalReference(lSourceKey, lText);
  EnableControls(True);
end;  // TfraSources.btnAddInternalRefClick 

{-------------------------------------------------------------------------------
  Handles the click on the Add URL button. 
}
procedure TfraSources.btnAddURLClick(Sender: TObject);
var
  lHyperlink: String;
begin
  inherited;
  
  if InputQuery(ResStr_EnterSource, ResStr_AskForURL, lHyperlink) = True then
    lbExternalRefs.ItemIndex := AddExternalReference(lHyperLink);
  EnableControls(True);
end;  // TfraSources.btnAddURLClick 

{-------------------------------------------------------------------------------
  Handles a click on the get external ref button. 
}
procedure TfraSources.btnGetInternalRefClick(Sender: TObject);
begin
  inherited;
  InitReturnData(ReturnReferenceData, 'Document');
  EnableControls(True);
end;  // TfraSources.btnGetInternalRefClick 

{-------------------------------------------------------------------------------
  Handles a click on the remove external ref button. 
}
procedure TfraSources.btnRemoveExternalRefClick(Sender: TObject);
var
  lItemIndex: Integer;
begin
  inherited;
  lItemIndex := lbExternalRefs.ItemIndex;
  if MessageDlg(ResStr_ConfirmExternalDocumentReferenceDelete,
                mtWarning, [mbYes, mbNo], 0) = mrYes then begin
    FExternalRefsList.DeleteItem(lItemIndex);
    if lItemIndex > 0 then Dec(lItemIndex);
    lbExternalRefs.ItemIndex := lItemIndex;
    ResetListBoxExtent(lbExternalRefs);
  end;
  EnableControls(True);
end;  // TfraSources.btnRemoveExternalRefClick 

{-------------------------------------------------------------------------------
  Handles a click on the remove internal ref button. 
}
procedure TfraSources.btnRemoveInternalRefClick(Sender: TObject);
var
  lItemIndex: Integer;
begin
  inherited;
  lItemIndex := sgInternalRefs.Row;
  if MessageDlg(ResStr_ConfirmDocumentReferenceDelete,
                mtWarning, [mbYes, mbNo], 0) = mrYes then begin
    FInternalRefsList.DeleteItem(lItemIndex);
    if lItemIndex > 1 then Dec(lItemIndex);
    sgInternalRefs.Row := lItemIndex;
  end;
  EnableControls(True);
end;  // TfraSources.btnRemoveInternalRefClick 

{-------------------------------------------------------------------------------
  Handles a click on the view external ref button. 
}
procedure TfraSources.btnViewExternalRefClick(Sender: TObject);
begin
  inherited;
  with lbExternalRefs do
    if ItemIndex <> -1 then ShellFile(Items[ItemIndex]);
end;  // TfraSources.btnViewExternalRefClick 

{-------------------------------------------------------------------------------
  Populate cmbOriginal. 
}
procedure TfraSources.cmbOriginalPopulate(Sender: TObject);
begin
  inherited;
  
  cmbOriginal.Add(ResStr_No, '0');
  cmbOriginal.Add(ResStr_Yes, '1');
end;  // TfraSources.cmbOriginalPopulate 

{-------------------------------------------------------------------------------
  Run the delete stored proc for the frame. 
}
procedure TfraSources.DeleteData;
begin
  dmGeneral.RunDeleteStoredProc('usp_References_Delete_ForSources',
                     ['@TableName', AdditionalProperties.GetProperty(PROP_TABLE_NAME),
                      '@RecordKey', Key]);
end;  // TfraSources.DeleteData 

{-------------------------------------------------------------------------------
  Handles the dragging and dropping onto external refs grid. 
}
procedure TfraSources.DropExternalRefs(const Sender: TObject; const AFormat: Integer; const 
    ASourceData: TKeyList; const ATextStrings: TStringList; var AHandled: Boolean);
var
  i: Integer;
begin
  if Editmode = emBrowse then
    AHandled := True
  else
  if AFormat = CF_HDROP then begin
    for i := 0 to ATextStrings.Count - 1 do
      lbExternalRefs.ItemIndex := AddExternalReference(ATextStrings[i]);
    AHandled := True;
    EnableControls(True);
  end else
    AHandled := False;
end;  // TfraSources.DropExternalRefs 

{-------------------------------------------------------------------------------
  Handles the dragging and dropping onto internal refs grid. 
}
procedure TfraSources.DropInternalRefs(const Sender: TObject; const AFormat: Integer; const 
    ASourceData: TKeyList; const ATextStrings: TStringList; var AHandled: Boolean);
var
  i: Integer;
  lText: String;
begin
  if EditMode = emBrowse then
    AHandled := True
  else
  if (AFormat = CF_JNCCDATA) and
     (CompareText(ASourceData.Header.TableName, TN_REFERENCE) = 0) then
  begin
    for i := 0 to ASourceData.Header.ItemCount - 1 do
      sgInternalRefs.Row := AddInternalReference(ASourceData.Items[i].KeyField1, lText);
    AHandled := True;
    EnableControls(True);
  end else
    AHandled := False;
end;  // TfraSources.DropInternalRefs 

{-------------------------------------------------------------------------------
  Method that toggles whether btnViewExternalRef is enabled/disabled. 
}
procedure TfraSources.EnableBtnViewExternalRef;
begin
  btnViewExternalRef.Enabled := lbExternalRefs.ItemIndex <> -1;
end;  // TfraSources.EnableBtnViewExternalRef 

{-------------------------------------------------------------------------------
  Enable the controls. 
}
procedure TfraSources.EnableControls(AEnabled: Boolean);
begin
  inherited;
  sgInternalRefs.ReadOnly := not AEnabled;
  // To ensure btnRemoveInternalRef is disabled when there aren't any items in
  // sgInternalRefs, we need to check the contents of the first row of the
  // string grid. This can't be done with the number of items in the list,
  // or the number of rows in the grid because if the grid is empty both of these
  // would be set to 1, and if there was one row, it would still be set to 1.
  with sgInternalRefs do
    btnRemoveInternalRef.Enabled := (EditMode = emEdit) and
          ((Cells[0, 1] <> '') or (Cells[1, 1] <> '') or (Cells[2, 1] <> ''));
  btnRemoveExternalRef.Enabled := (EditMode = emEdit) and (FExternalRefsList.ItemCount > 0);
  EnableBtnViewExternalRef;
end;  // TfraSources.EnableControls 

{-------------------------------------------------------------------------------
  Resize the frame and the contained components to make use of the available room as best as 
      possible. 
}
procedure TfraSources.FrameResize(Sender: TObject);
var
  lNewHeight: Integer;
  
  const FIXED_GAP  = 86;  // For other controls around grid and listbox
        GRID_EXTRA = 17;  // The grid is slightly bigger than the listbox
        BOTTOM_GAP = 33;  // Room needed for buttons below listbox
  
begin
  inherited;
  // Work out how much room there is available for grid and list
  lNewHeight := (Height - FIXED_GAP - GRID_EXTRA) div 2;
  // Set grid size, and realign surrounding controls
  sgInternalRefs.Height    := lNewHeight + GRID_EXTRA;
  shpGrid.Height           := sgInternalRefs.Height + 2;
  btnGetInternalRef.Top    := sgInternalRefs.Top + sgInternalRefs.Height + 2;
  btnAddInternalRef.Top    := btnGetInternalRef.Top;
  btnRemoveInternalRef.Top := btnGetInternalRef.Top;
  // Set listbox size and realign surrounding controls
  lbExternalRefs.Height := lNewHeight;
  lbExternalRefs.Top    := Height - lNewHeight - BOTTOM_GAP;
  lblExternalRefs.Top   := lbExternalRefs.Top - 16;
  shpList.Height        := lNewHeight + 2;
  shpList.Top           := lbExternalRefs.Top - 1;
end;  // TfraSources.FrameResize 

{-------------------------------------------------------------------------------
}
function TfraSources.GetHasData: Boolean;
begin
  with sgInternalRefs do
    Result := (Cells[0, 1] <> '') 
        or (Cells[1, 1] <> '')  
        or (Cells[2, 1] <> '');

  if not Result then 
    Result := lbExternalRefs.Count > 0;
end;  // TfraSources.GetHasData

{-------------------------------------------------------------------------------
  Handles a click on the external refs grid. 
}
procedure TfraSources.lbExternalRefsClick(Sender: TObject);
begin
  inherited;
  EnableBtnViewExternalRef;
end;  // TfraSources.lbExternalRefsClick 

{-------------------------------------------------------------------------------
  Handles a mouse movement over external refs.  
}
procedure TfraSources.lbExternalRefsMouseMove(Sender: TObject; Shift: TShiftState; X, Y: 
    Integer);
begin
  inherited;
  SetListBoxHintIfTooWide(lbExternalRefs, X, Y);
end;  // TfraSources.lbExternalRefsMouseMove 

{-------------------------------------------------------------------------------
  Load the lists for the string grids with the values they require. 
}
procedure TfraSources.LoadData;
begin
  inherited;
  cmbOriginal.ItemIndex := -1;
  
  FInternalRefsList.MasterKey := Key;
  FInternalRefsList.TableName := AdditionalProperties.GetProperty(PROP_TABLE_NAME);
  FInternalRefsList.Refresh;
  
  FExternalRefsList.MasterKey := Key;
  FExternalRefsList.TableName := AdditionalProperties.GetProperty(PROP_TABLE_NAME);
  FExternalRefsList.Refresh;
  ResetListBoxExtent(lbExternalRefs);
  if lbExternalRefs.Items.Count > 0 then lbExternalRefs.ItemIndex := 0;
end;  // TfraSources.LoadData 

{-------------------------------------------------------------------------------
  Register the recordset and the controls. 
}
procedure TfraSources.RegisterControls;
begin
  inherited RegisterControls;
end;  // TfraSources.RegisterControls 

{-------------------------------------------------------------------------------
  Register the drag and drop components. 
}
procedure TfraSources.RegisterDragDropcomponents;
begin
  RegisterDropcomponent(sgInternalRefs, DropInternalRefs, [TN_REFERENCE], [CF_JNCCDATA]);
  RegisterDropComponent(lbExternalRefs, DropExternalRefs, [], [CF_HDROP]);
end;  // TfraSources.RegisterDragDropcomponents 

{-------------------------------------------------------------------------------
  Return reference data. 
}
procedure TfraSources.ReturnReferenceData(const AKeyList: IKeyList);
var
  lText: String;
  i: Integer;
begin
  if CompareText(AKeyList.TableName, TN_REFERENCE) = 0 then
    for i := 0 to AKeyList.ItemCount - 1 do
      sgInternalRefs.Row := AddInternalReference(AKeylist.GetKeyItem(i).KeyField1, lText);
end;  // TfraSources.ReturnReferenceData 

{-------------------------------------------------------------------------------
  Save modified data to the database. 
}
procedure TfraSources.SaveData;
begin
  FInternalRefsList.Update;
  FExternalRefsList.Update;
end;  // TfraSources.SaveData 

{-------------------------------------------------------------------------------
  Sets the master keys of the lists. 
}
procedure TfraSources.SetKey(Value: TKeyString);
begin
  inherited SetKey(Value);
  
  FInternalRefsList.MasterKey := Key;
  FExternalRefsList.MasterKey := Key;
end;  // TfraSources.SetKey 

{-------------------------------------------------------------------------------
}
procedure TfraSources.sgInternalRefsDblClick(Sender: TObject);
var
  lDocList : IKeyList;
begin
  inherited;
  with sgInternalRefs do
    if (Row > 0) and Assigned(Objects[0, Row]) and dmGeneral.RecorderRunning then
      if TInternalRefItem(Objects[0, Row]).SourceKey = '' then
        Exit
      else begin
        // Get a reference to a keylist interface so it doesn't destroy itself to early.
        lDocList := TSingleDocumentList.Create(
                TInternalRefItem(Objects[0, Row]).SourceKey);
        // Now get Recorder to show us the Documents screen.
        dmGeneral.Recorder.DisplayData(
            'Document',
            lDocList);
      end;
end;  // TfraSources.sgInternalRefsDblClick

{-------------------------------------------------------------------------------
  All tab pages are validated before anything is actually saved. The list used by the string 
      grid needs to be told to validate. 
}
procedure TfraSources.ValidateData;
begin
  inherited;
  FInternalRefsList.ValidateContent;
end;  // TfraSources.ValidateData 

{-==============================================================================
    TInternalRefItem
===============================================================================}
{-------------------------------------------------------------------------------
}
procedure TInternalRefItem.GetData(const Column: Integer; var AText: String; var AKey: 
    TKeyString);
begin
  AKey := '';
  AText := '';
  case Column of
    0: AText := FDocument;
    1: if FOriginal then begin
         AText := ResStr_Yes;
         AKey := '1';
       end else begin
         AText := ResStr_No;
         AKey := '0';
       end;
    2: AText := FRefType;
  end;
end;  // TInternalRefItem.GetData 

{-------------------------------------------------------------------------------
}
procedure TInternalRefItem.InitFromRecord(AFields: Fields);
begin
  SetItemKey(VarToStr(AFields['Item_Key'].Value));
  FDocument  := AFields['Document'].Value;
  FOriginal  := AFields['Original'].Value;
  FRefType   := AFields['Ref_Type'].Value;
  FSourceKey := AFields['Source_Key'].Value;
end;  // TInternalRefItem.InitFromRecord 

{-------------------------------------------------------------------------------
}
procedure TInternalRefItem.SetData(const Column: Integer; const AText: String; const AKey: 
    TKeyString);
var
  lNewOriginal: Boolean;
begin
  // Can only ever change the Original flag, so deal only with that one.
  case Column of
    1: begin
         lNewOriginal := (AText = ResStr_Yes);
         if Original <> lNewOriginal then Original := lNewOriginal;
       end;
  end;
end;  // TInternalRefItem.SetData 

{-------------------------------------------------------------------------------
}
procedure TInternalRefItem.SetOriginal(Value: Boolean);
begin
  FOriginal := Value;
  SetModified;
end;  // TInternalRefItem.SetOriginal 

{-------------------------------------------------------------------------------
}
procedure TInternalRefItem.ValidateData;
begin
  ValidateValue(FDocument <> '', Format(ResStr_MissingData, [ResStr_Document]));
  ValidateValue(FRefType <> '', Format(ResStr_MissingData, [ResStr_Type]));
end;  // TInternalRefItem.ValidateData 

{-==============================================================================
    TInternalRefList
===============================================================================}
{-------------------------------------------------------------------------------
}
function TInternalRefList.AllowedAddOnKeyDown: Boolean;
begin
  Result := False;
end;  // TInternalRefList.AllowedAddOnKeyDown 

{-------------------------------------------------------------------------------
}
procedure TInternalRefList.DoAddition(AItem: TLuxCachedDataItem);
var
  lSourceJoinKey: TKeyString;
begin
  with TInternalRefItem(AItem) do begin
    lSourceJoinKey := VarToStr(dmGeneral.RunInsertStoredProc(TN_SOURCE_JOIN,
                        'usp_SourceJoin_Insert',
                        ['@TableName', FTableName,
                         '@RecordKey', MasterKey,
                         '@SourceKey', SourceKey,
                         '@Original', Original],
                        '@Key'));
    SetItemKey(lSourceJoinKey);
  end;
end;  // TInternalRefList.DoAddition 

{-------------------------------------------------------------------------------
}
procedure TInternalRefList.DoDeletion(AItem: TLuxCachedDataItem);
begin
  with TInternalRefItem(AItem) do
    dmGeneral.RunDeleteStoredProc('usp_SourceJoin_Delete',
                                  ['@Key', ItemKey,
                                   '@Timestamp', Timestamp]);
end;  // TInternalRefList.DoDeletion 

{-------------------------------------------------------------------------------
}
procedure TInternalRefList.DoModification(AItem: TLuxCachedDataItem);
begin
  with TInternalRefItem(AItem) do begin
    dmGeneral.RunUpdateStoredProc('usp_SourceJoin_Update',
                                  ['@Key', ItemKey,
                                   '@Original', Original,
                                   '@Timestamp', Timestamp]);
  end;
end;  // TInternalRefList.DoModification 

{-------------------------------------------------------------------------------
}
function TInternalRefList.GetRecordset: _Recordset;
begin
  Result := dmGeneral.GetRecordset('usp_References_Select_ForInternalSources',
                                  ['@Key', MasterKey, '@TableName', FTableName]);
end;  // TInternalRefList.GetRecordset 

{-==============================================================================
    TExternalRefItem
===============================================================================}
{-------------------------------------------------------------------------------
}
procedure TExternalRefItem.InitFromRecord(AFields: Fields);
begin
  SetItemKey(VarToStr(AFields['Item_Key'].Value));
  FFileName  := AFields['Item_Name'].Value;
end;  // TExternalRefItem.InitFromRecord 

{-------------------------------------------------------------------------------
}
procedure TExternalRefItem.SetFileName(const Value: String);
begin
  FFileName := Value;
  SetModified;
end;  // TExternalRefItem.SetFileName 

{-==============================================================================
    TExternalRefList
===============================================================================}
{-------------------------------------------------------------------------------
}
procedure TExternalRefList.DoAddition(AItem: TLuxCachedDataItem);
begin
  inherited;
  
  dmGeneral.RunInsertStoredProc(TN_SOURCE_FILE, 'usp_Multimedia_Insert',
                                ['@RecordKey', MasterKey,
                                 '@Filename', TExternalRefItem(AItem).FileName,
                                 '@TableName', TableName,
                                 '@Preferred', 0],
                                '@Key');
end;  // TExternalRefList.DoAddition 

{-------------------------------------------------------------------------------
}
procedure TExternalRefList.DoDeletion(AItem: TLuxCachedDataItem);
begin
  inherited;
  
  dmGeneral.RunDeleteStoredProc('usp_Multimedia_Delete',
                                ['@Key', TExternalRefItem(AItem).ItemKey,
                                 '@RecordKey', MasterKey]);
end;  // TExternalRefList.DoDeletion 

{-------------------------------------------------------------------------------
}
function TExternalRefList.GetRecordset: _Recordset;
begin
  Result := dmGeneral.GetRecordset('usp_References_Select_ForExternalSources',
                                   ['@Key', MasterKey, '@TableName', FTableName]);
end;  // TExternalRefList.GetRecordset

{-------------------------------------------------------------------------------
}
function TExternalRefList.GetText(AItem: TLuxCachedDataItem): String;
begin
  Result := TExternalRefItem(AItem).FileName;
end;  // TExternalRefList.GetText 

{-==============================================================================
    TSingleDocumentList
===============================================================================}
{-------------------------------------------------------------------------------
}
constructor TSingleDocumentList.Create(const DocumentKey: WideString);
begin
  FDocumentKey := DocumentKey;
end;

{-------------------------------------------------------------------------------
}
function TSingleDocumentList.Get_ItemCount: Integer;
begin
  Result := 1;
end;  // TSingleDocumentList.Get_ItemCount

{-------------------------------------------------------------------------------
}
function TSingleDocumentList.Get_KeyField1: WideString;
begin
  Result := FDocumentKey;
end;  // TSingleDocumentList.Get_KeyField1

{-------------------------------------------------------------------------------
}
function TSingleDocumentList.Get_KeyField2: WideString;
begin
  Result := '';
end;  // TSingleDocumentList.Get_KeyField2

{-------------------------------------------------------------------------------
}
function TSingleDocumentList.Get_TableName: WideString;
begin
  Result := 'Document';
end;  // TSingleDocumentList.Get_TableName

{-------------------------------------------------------------------------------
}
function TSingleDocumentList.GetIDsOfNames(const IID: TGUID;
  Names: Pointer; NameCount, LocaleID: Integer; DispIDs: Pointer):
  HResult;
begin
  Result := E_NOTIMPL;
end;  // TSingleDocumentList.GetIDsOfNames

{-------------------------------------------------------------------------------
}
function TSingleDocumentList.GetKeyItem(iIndex: Integer): IKeyItem;
begin
  Result := Self;
end;  // TSingleDocumentList.GetKeyItem

{-------------------------------------------------------------------------------
}
function TSingleDocumentList.GetTypeInfo(Index, LocaleID: Integer;
  out TypeInfo): HResult;
begin
  Result := E_NOTIMPL;
end;  // TSingleDocumentList.GetTypeInfo

{-------------------------------------------------------------------------------
}
function TSingleDocumentList.GetTypeInfoCount(out Count: Integer):
  HResult;
begin
  Result := E_NOTIMPL;
end;  // TSingleDocumentList.GetTypeInfoCount

{-------------------------------------------------------------------------------
}
function TSingleDocumentList.Invoke(DispID: Integer; const IID: TGUID;
  LocaleID: Integer; Flags: Word; var Params; varResult, ExepInfo,
  ArgErr: Pointer): HResult;
begin
  Result := E_NOTIMPL;
end;  // TSingleDocumentList.Invoke

end.
