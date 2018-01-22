{===============================================================================
  Unit:        FrameMergeData

  Defines:     TfraMergeData

  Description:

  Model:

  Created:     january 2007

  Last revision information:
    $Revision: 4 $
    $Date: 21/12/07 10:06 $
    $Author: Johnvanbreda $

===============================================================================}

unit FrameMergeData;

interface

uses
  Windows, Messages, SysUtils, Forms, StdCtrls, Dialogs, ImageListButton, ComCtrls, Controls,
  Graphics, ExtCtrls, Menus, Classes, BaseDragFrameUnit, DropStruct, DropTarget, DataClasses,
  ExceptionForm, Constants, ImgList, Relationships_ADO, JNCCRelationships, BaseADODataModule,
  ComObj;

type
  EMergeDataError = class(TExceptionPath);
  EGeneralDataError = class(TExceptionPath);
  ERecordMissingError = class(EGeneralDataError);
  EMultiFieldKey = class(EGeneralDataError);

  { Class to stick in the data of treenodes to record the info. }
  TRecordDataNode = class
    FDataValue: String;
    FFieldName: String;
    FTableName: String;
  end;

	TfraMergeData = class(TBaseDragFrame)
    pnlInstruct: TPanel;
    lblInstruct: TLabel;
    Label1: TLabel;
    lblDestItem: TLabel;
    lblInstruct2: TLabel;
    pnlSource: TPanel;
    tvSourceItem: TTreeView;
    pnlDest: TPanel;
    tvTargetItem: TTreeView;
    pnlButtons: TPanel;
    btnMerge: TImageListButton;
    btnClear: TImageListButton;
    ilTreeImages: TImageList;
    procedure FrameResize(Sender: TObject);
    procedure ItemClick(Sender: TObject);
    procedure CustomDrawItem(Sender: TCustomTreeView;
      Node: TTreeNode; State: TCustomDrawState; var DefaultDraw: Boolean);
    procedure btnClearClick(Sender: TObject);
    procedure btnMergeClick(Sender: TObject);
  private
    FWorkToDo: Integer;
    FWorkDone: Integer;
    FSourceTable: String;
    FTargetTable: String;
    FSourceKey  : TKeyString;
    FTargetKey: TKeyString;
    FDeleteSources: Boolean;
    FUnsetPreferred: Boolean;     // default behaviour is to uncheck preferred flag but mustn't do during import
    FPreferredLinks: TStringList; // identify joins on which the preferred flag operates
    procedure CheckForHierarchicalMerge;
    procedure ClearTreeView(ATree: TTreeView);
    procedure FixupJoin(const ADetailTable, ADetailName: String; const ASourceKey,
        ATargetKey: TKeyString);
    function IsPrimaryKey(const AReferencingTable, AReferencingField: String): Boolean;
    function IsSystemSupplied(const ATableName: String; AKey: TKeyString): Boolean;
    function GetPrimaryKey(const ATableName: String;
      AWantMultiField: Boolean): String;
    procedure GetRecordStrings(ARecordList: TStrings; const ATableName: String;
      const AKey: TKeyString; ADelimiter: String = '''');
    procedure GetRecordStruct(AKeyList: TKeyList; ATree: TTreeView; ANode: TTreenode);
    procedure LoadPreferredLinks;
    function OneToOneJoin(const ATable1, ATable2: String): Boolean;
    procedure PopulateRelationship(ARelIndex: Integer; const AMasterTableName: String;
      ANode: TTreeNode);
    procedure PrepareMerge(const ATable, ASourceKey, ATargetKey: String);
    procedure ProcessMerge(const ATable, ASourceKey, ATargetKey: String);
    procedure ProcessSingleTableMerge(const ATable: String; const ASourceKey,
        ATargetKey: TKeyString; ATaskStart, ATaskMax: Integer);
    procedure ProcessThreeTableMerge(const ATable1, ATable2, ATable3: String;
      const ASourceKey, ATargetKey: TKeyString);
    procedure RegisterCanDropAnything(AControl: TWinControl; AEvent: TDataDroppedEvent);
    procedure SetSourceItem(const Sender: TObject; const AFormat: Integer;
      const ASourceData: TKeyList; const ATextStrings: TStringList;
      var AHandled: Boolean);
    procedure SetTargetItem(const Sender: TObject; const AFormat: Integer;
      const ASourceData: TKeyList; const ATextStrings: TStringList;
      var AHandled: Boolean);
    procedure SetTreeViewItem(ATree: TTreeView; ASourceData: TKeyList);
    function TableExists(const ATableName: String): Boolean;
    procedure UpdateNameTable(const ASourceKey, ATargetKey: String; ATaskStart, ATaskMax: Integer);
  protected
    procedure RegisterDragDropComponents; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
	end;

//==============================================================================
implementation

{$R *.dfm}

uses
  GeneralData, ADOInt, Variants, GeneralFunctions, InterfaceDataModule;

const
  MST_ITEMS_SAME = 'You cannot merge two data items which are the same.  ' +
                   'Please select a different source or destination item.';
  MST_MERGE_OK   = 'The data merge is complete.';

  EST_MERGEREC_MISSING = 'Cannot merge data items that are not in the local database';
  EST_COMPOUNDKEY = 'Items cannot be merged in a table with a compound key: ';
  EST_FIXUP_FAILURE = 'Failure occurred whilst translating keys in table ';
  EST_MERGE_ROLLBACK = 'A problem occurred during the merge.  All data changes have been undone.';

{-------------------------------------------------------------------------------
}
constructor TfraMergeData.Create(AOwner: TComponent);
begin
  inherited;

  // Frame dropped on form "loses" events. So re-link and all is fine. All I can say is: WTF!?
  tvSourceItem.OnClick := ItemClick;
  tvSourceItem.OnCustomDrawItem := CustomDrawItem;
  tvTargetItem.OnClick := ItemClick;
  tvTargetItem.OnCustomDrawItem := CustomDrawItem;

  FDeleteSources := True;
  FUnsetPreferred := True;
  LoadPreferredLinks;

  Resize;
end;

{-------------------------------------------------------------------------------
  Load PREFERRED_LINKS into a String list so we can rapidly scan }
procedure TfraMergeData.LoadPreferredLinks;
begin
  FPreferredLinks := TStringList.Create;
  FPreferredLinks.Sorted := True; // Improves IndexOf performance.
  with dmGeneral.ExecuteSQL('Select TABLE_NAME, PREFERRED_FIELD from PREFERRED_LINKS', True) do
    while not Eof do begin
      FPreferredLinks.Add(
          VarToStr(Fields['Table_Name'].Value)
          + '-'
          + VarToStr(Fields['Preferred_Field'].Value));
      MoveNext;
    end;
end;

{-------------------------------------------------------------------------------
}
destructor TfraMergeData.Destroy;
begin
  ClearTreeView(tvSourceItem);
  ClearTreeView(tvTargetItem);
  inherited;
end;

{-------------------------------------------------------------------------------
}
procedure TfraMergeData.ClearTreeView(ATree: TTreeView);
var
  i: Integer;
begin
  with ATree do begin
    for i := 0 to Items.Count - 1 do
      if Items[i].Data <> nil then begin
        TObject(Items[i].Data).Free;
        Items[i].Data := nil;
      end;
    Items.Clear;
  end;
end;

{-------------------------------------------------------------------------------
}
procedure TfraMergeData.FrameResize(Sender: TObject);
begin
  inherited;
  tvSourceItem.Width := Width div 2 - 4;
  lblDestItem.Left   := Width div 2 + 4;

  lblInstruct.Left   := (pnlInstruct.Width - lblInstruct.Width) div 2;
  lblInstruct2.Left  := (pnlInstruct.Width - lblInstruct2.Width) div 2;

  btnMerge.Left := (pnlButtons.Width - btnClear.Width - btnMerge.Width - 16) div 2;
  btnClear.Left := btnMerge.Left + btnMerge.Width + 16;
end;

{-------------------------------------------------------------------------------
}
procedure TfraMergeData.ItemClick(Sender: TObject);
var
  idxRel: Integer;
  nodeSelected: TTreeNode;
  relationships: TJNCCRelationshipList;
begin
  inherited;
  if TTreeView(Sender).Selected <> nil then
  begin
    nodeSelected := TTreeView(Sender).Selected;
    if (nodeSelected.Data <> nil) and (not nodeSelected.HasChildren) then
    begin
      relationships := TJNCCRelationshipList.Create(dmGeneral.Connection);
      try
        idxRel := relationships.FindRelationship(
              TRecordDataNode(nodeSelected.Data).FTableName,
              TRecordDataNode(nodeSelected.Data).FFieldName);
        if idxRel <> NO_RELATIONSHIP then
          PopulateRelationship(
              idxRel,
              relationships.Relationship[idxRel].MasterTable,
              nodeSelected);
      finally
        relationships.Free;
      end;

    end;
  end;
end;

{-------------------------------------------------------------------------------
  Nodes which are not data items should be displayed bold - detected simply
  because the data items are nil.  Other nodes are in a different font
  colour for clarity - these are the data nodes }
procedure TfraMergeData.CustomDrawItem(
    Sender: TCustomTreeView;
    Node: TTreeNode;
    State: TCustomDrawState;
    var DefaultDraw: Boolean);
begin
  inherited;
  DefaultDraw := True;
  if (Node.Data = nil) then
    Sender.Canvas.Font.Style := [fsBold]
  else begin
    Sender.Canvas.Font.Style := [];
    // Can't draw selected colour on selected background!
    if not Node.Selected then
      Sender.Canvas.Font.Color := clHighlight;
  end;
end;

{-------------------------------------------------------------------------------
}
procedure TfraMergeData.RegisterDragDropComponents;
begin
  RegisterCanDropAnything(tvSourceItem, SetSourceItem);
  RegisterCanDropAnything(tvTargetItem, SetTargetItem);
end;

{-------------------------------------------------------------------------------
}
procedure TfraMergeData.RegisterCanDropAnything(
    AControl: TWinControl;
    AEvent: TDataDroppedEvent);
var
  tablesList: TStringList;
  tableNames: Array of String;
  i: Integer;
begin
  tablesList := TStringList.Create;
  try
    dmGeneral.Connection.GetTableNames(tablesList, False);

    // Since RegisterDropComponent requires arrays, convert the list.
    SetLength(tableNames, tablesList.Count);
    for i := 0 to tablesList.Count - 1 do
      tableNames[i] := tablesList[i];

    RegisterDropComponent(AControl, AEvent, tableNames, [CF_JNCCDATA]);
  finally
    tablesList.Free;
  end;
end;  // RegisterCanDropAnything

{-------------------------------------------------------------------------------
}
procedure TfraMergeData.SetSourceItem(
    const Sender: TObject;
    const AFormat: Integer;
    const ASourceData: TKeyList;
    const ATextStrings: TStringList;
    var AHandled: Boolean);
begin
  try
    if (AFormat = CF_JNCCDATA) then
    begin
      if ASourceData.Header.ItemCount > 0 then
        if IsSystemSupplied(ASourceData.Header.TableName, ASourceData.Items[0].KeyField1) then
        begin
          MessageDlg(
              'The data item is system supplied and cannot be reassigned.',
              mtInformation,
              [mbOk],
              0);
          Exit; // no further action required
        end;

      SetTreeViewItem(tvSourceItem, ASourceData);
      // If new table, then clear dest box as well.
      if ASourceData.Header.TableName <> FTargetTable then
      begin
        tvTargetItem.Items.Clear;
        // We are not able to proceed yet.
        FTargetTable := '';
      end;
      // Store the table dropped so we can check when dropping a destination.
      FSourceTable := ASourceData.Header.TableName;
      FSourceKey := ASourceData.Items[0].KeyField1;
      AHandled := True;
      btnMerge.Enabled := (FSourceTable = FTargetTable);
      CheckForHierarchicalMerge;
    end;
  except
    // Item being dropped not in local db so ignore
    on ERecordMissingError do begin
      tvSourceItem.Items.Clear;
      raise ERecordMissingError.CreateNonCritical(EST_MERGEREC_MISSING);
    end;
  end;
end;  // SetSourceItem

{-------------------------------------------------------------------------------
}
procedure TfraMergeData.SetTargetItem(
    const Sender: TObject;
    const AFormat: Integer;
    const ASourceData: TKeyList;
    const ATextStrings: TStringList;
    var AHandled: Boolean);
begin
  try
    if (AFormat = CF_JNCCDATA) then
    begin
      // Must have a matching table.
      if (FSourceTable <> '') and (FSourceTable <> ASourceData.Header.TableName) then
      begin
        MessageDlg('You can only merge data items in a single table.', mtWarning, [mbOk], 0);
        Exit;
      end;
      SetTreeViewItem(tvTargetItem, ASourceData);
      FTargetTable := ASourceData.Header.TableName;
      // Ready to proceed.
      if FSourceTable = FTargetTable then
        btnMerge.Enabled := True;
      FTargetKey := ASourceData.Items[0].KeyField1;
      AHandled := True;
      CheckForHierarchicalMerge;
    end;
  except
    // Item being dropped not in local db so ignore.
    on ERecordMissingError do begin
      tvTargetItem.Items.Clear;
      raise ERecordMissingError.CreateNonCritical(EST_MERGEREC_MISSING);
    end;
  end;
end;  // SetTargetItem

{-------------------------------------------------------------------------------
}
function TfraMergeData.GetPrimaryKey(
    const ATableName: String;
    AWantMultiField: Boolean): String;
begin
  // Get key column name. Can't seem to be able to use GetRecordset with system procs!
  with dmGeneral.ExecuteSQL(Format('EXEC sp_pkeys @Table_Name = ''%s''', [ATableName]), True) do
    if not Eof then begin
      result := Fields['Column_Name'].Value;

      MoveNext;
      if not Eof and not AWantMultiField then
        raise EMultiFieldKey.Create(ResStr_MultipleFieldKey + ATableName)
      else
        while not Eof do begin
          result := result + ';' + Fields['Column_Name'].Value;
          MoveNext;
        end;
    end;
end;

{-------------------------------------------------------------------------------
}
function TfraMergeData.IsSystemSupplied(
    const ATableName: String;
    AKey: TKeyString): Boolean;
var
  keyName: String;
begin
  // Assume record is not system supplied, until proven otherwise.
  result := False;

  // Get key column name.
  keyName := GetPrimaryKey(ATableName, False);

  // Check table has a System_Supplied_Data column. Stop here if not found.
  // And again, can't seem to be able to use GetRecordset with system procs!
  with dmGeneral.ExecuteSQL(
      Format(
          'EXEC sp_columns @Table_Name = ''%s'', @Column_Name = ''System_Supplied_Data''',
          [ATableName]),
      True) do
    if Eof then Exit;

  // Return value of System_Supplied_Data for record.
  // Have to use ExecuteSQL because of variable table name.
  with dmGeneral.ExecuteSQL(
      Format(
          'SELECT System_Supplied_Data FROM "%s" WHERE "%s" = ''%s''',
          [ATableName, keyName, AKey]),
      True) do
    result := Fields['System_Supplied_Data'].Value = 1;
end;

{-------------------------------------------------------------------------------
}
procedure TfraMergeData.SetTreeViewItem(
    ATree: TTreeView;
    ASourceData: TKeyList);
var
  tableNode: TTreeNode;
begin
  // No data dropped
  if ASourceData.Header.ItemCount = 0 then
    Exit;

  with ATree.Items do begin
    Clear;
    // Add a tree node for the table.
    tableNode := Add(nil, ASourceData.Header.TableName + ' - ' + ASourceData.Items[0].KeyField1);
    tableNode.ImageIndex := 0; // table glyph
    tableNode.SelectedIndex := 0;
  end;
  // Read the fields and data into the tree view.
  GetRecordStruct(ASourceData, ATree, tableNode);
  // Expand the node one level.
  tableNode.Expand(True);
  // Select first node.
  ATree.Selected := ATree.Items.GetFirstNode;
end;

{-------------------------------------------------------------------------------
}
procedure TfraMergeData.CheckForHierarchicalMerge;
var
  locKey : string;
begin
  // Check for merging a "parent" with one of its "children". The reverse is ok though.
  if SameText(FSourceTable, 'Location') then begin
    locKey := FTargetKey;
    repeat
      // Although assumed, there should always be a valid key to start with.
      with dmGeneral.GetRecordset('usp_Location_Select', ['@Key', locKey]) do
        if Eof then // Just in case
          locKey := ''
        else
          locKey := VarToStr(Fields['Parent_Key'].Value);
    until (locKey = '') or (locKey = FSourceKey);

    // If the source is found in the chain of parents, then the merge is invalid
    if locKey = FSourceKey then begin
      MessageDlg('You cannot merge a site into one of its subsites.', mtInformation, [mbOk], 0);
      btnMerge.Enabled := False;
    end;
  end;
end;

{-------------------------------------------------------------------------------
}
procedure TfraMergeData.GetRecordStruct(
    AKeyList: TKeyList;
    ATree: TTreeView;
    ANode: TTreenode);
var
  idxItem, idxField: Integer;
  nodeField, nodeValue: TTreenode;
  data: TRecordDataNode;
  recordStrings: TStringList;
begin
  recordStrings := TStringList.Create;
  try
    for idxItem := 0 to AKeyList.Header.ItemCount - 1 do
    begin
      recordStrings.Clear;
      // Locate the record. No need to worry about multiple key fields.
      GetRecordStrings(
          recordStrings,
          AKeyList.Header.TableName,
          AKeylist.Items[idxItem].KeyField1);

      // Populate tree. One node per field name with one child node for the value.
      for idxField := 0 to recordStrings.Count - 1 do
      begin
        nodeField := ATree.Items.AddChild(ANode, recordStrings.Names[idxField]);
        nodeField.ImageIndex    := 1;
        nodeField.Selectedindex := 1;
        data := TRecordDataNode.Create;
        data.FDataValue := recordStrings.Values[recordStrings.Names[idxField]];
        data.FFieldName := recordStrings.Names[idxField];
        data.FTableName := AKeyList.Header.TableName;
        if data.FDataValue <> '' then
          nodeValue := ATree.Items.AddChildObject(nodeField, data.FDataValue, data)
        else
          nodeValue := ATree.Items.AddChildObject(nodeField, '[Blank]', data);

        nodeValue.ImageIndex    := 2;
        nodeValue.SelectedIndex := 2;
      end;
    end;
  finally
    recordStrings.Free;
  end;
end;

{-------------------------------------------------------------------------------
}
procedure TfraMergeData.PopulateRelationship(
    ARelIndex: Integer;
    const AMasterTableName: String;
    ANode: TTreeNode);
var
  keyList: TEditableKeyList;
  recordStrings: TStringList;
begin
  // Blank data
  if TRecordDataNode(ANode.Data).FDataValue = '' then
    Exit;

  keyList := TEditableKeylist.Create;
  try
    if ARelIndex = NAME_REL then
    begin
      recordStrings := TStringList.Create;
      GetRecordStrings(
          recordStrings,
          TN_NAME,
          TRecordDataNode(ANode.Data).FDataValue);
      if recordStrings.Values[TN_ORGANISATION] = 'True' then
        keyList.SetTable(TN_ORGANISATION)
      else
        keyList.SetTable(TN_INDIVIDUAL);
    end else
      keyList.SetTable(AMasterTableName);
    keyList.AddItem(TRecordDataNode(ANode.Data).FDataValue, '');
    GetRecordStruct(keyList, TTreeView(ANode.TreeView), ANode);
  finally
    keyList.Free;
  end;
end;

{-------------------------------------------------------------------------------
}
procedure TfraMergeData.GetRecordStrings(
    ARecordList: TStrings;
    const ATableName: String;
    const AKey: TKeyString;
    ADelimiter: String = '''');
var
  i: Integer;
  keyName: String;
begin
  if ARecordList = nil then
    raise EGeneralDataError.Create('Record list not created');

  // Handle special cases for key field names.
  keyName := GetPrimaryKey(ATableName, False);
  with dmGeneral.ExecuteSQL(
      Format(
          'SELECT * FROM "%s" WHERE "%s" = %s%s%s',
          [ATableName, keyName, ADelimiter, AKey, ADelimiter]),
      True) do
  begin
    if Eof then
      raise ERecordMissingError.Create(ResStr_RecordMissing + ATableName);

    // Construct name value pair.
    for i := 0 to Fields.Count - 1 do
      ARecordList.Add(Fields[i].Name + '=' + VarToStr(Fields[i].Value));
  end;
end;

{-------------------------------------------------------------------------------
}
procedure TfraMergeData.btnClearClick(Sender: TObject);
begin
  inherited;
  ClearTreeView(tvSourceItem);
  ClearTreeView(tvTargetItem);
  btnMerge.Enabled := False;
  FSourceTable := '';
  FTargetTable := '';
  FSourceKey := '';
  FTargetKey := '';
end;

{-------------------------------------------------------------------------------
}
procedure TfraMergeData.btnMergeClick(Sender: TObject);
var
  lCursor: TCursor;
begin
  inherited;
  // Check if this screen is the only one left open. If so, all ok, else abort for now-
  if Application.MainForm.MDIChildCount > 1 then
    MessageDlg('Some screens are still open. Please close these screens before '+
               'initiating the data merging process.', mtInformation, [mbOk], 0)
  else
  if FSourceKey = FTargetKey then
    MessageDlg(MST_ITEMS_SAME, mtInformation, [mbOk], 0)
  else
  // Get a confirmation, as this is a serious thing to do.
  if MessageDlg(
      Format(
          'Warning.  The data item %s in the %s table is about to be  deleted.  All data referring '
          + 'to this record will be reassigned to point to record %s.  Do you wish to proceed?',
          [FSourceKey, FSourceTable, FTargetKey]),
      mtConfirmation, [mbYes, mbNo], 0) = mrYes then
  begin
    lCursor := HourglassCursor;
    try
      btnMerge.Enabled := False;
      ProcessMerge(FSourceTable, FSourceKey, FTargetKey);
    finally
      DefaultCursor(lCursor);
    end;
    // Only gets here if no exceptions.
    MessageDlg(MST_MERGE_OK, mtInformation, [mbOk], 0);
    // Clear form, ready for next round, if any.
    btnClearClick(nil);
  end;
end;

{-------------------------------------------------------------------------------
}
procedure TfraMergeData.ProcessMerge(const ATable, ASourceKey, ATargetKey: String);
var
  lCursor: TCursor;
begin
  lCursor := HourglassCursor;
  SetStatus(Format('Merging items in %s table..', [ATable]));
  SetProgress(0, 120);
  // Start a transaction.
  dmGeneral.Connection.Execute('SET XACT_ABORT OFF');
  dmGeneral.Connection.Execute('SET ARITHABORT ON');
  dmGeneral.Connection.BeginTrans;
  try
    try
      PrepareMerge(ATable, ASourceKey, ATargetKey);
      // Ensure related one to one records processed for certain tables.
      if SameText(ATable, TN_NAME) or
         SameText(ATable, TN_INDIVIDUAL) or
         SameText(ATable, TN_ORGANISATION) then
        ProcessThreeTableMerge(TN_INDIVIDUAL, TN_ORGANISATION, TN_NAME, ASourceKey, ATargetKey)
      else
      if SameText(ATable, TN_SOURCE) or
         SameText(ATable, TN_SOURCE_FILE) or
         SameText(ATable, TN_REFERENCE) then
        ProcessThreeTableMerge(TN_SOURCE_FILE, TN_REFERENCE, TN_SOURCE, ASourceKey, ATargetKey)
      else
        ProcessSingleTableMerge(ATable, ASourceKey, ATargetKey, 0, 120);
      SetProgress(120, 120);
      // All work done OK, so commit.
      dmGeneral.Connection.CommitTrans;
    except
      on E:Exception do // any probs, then rollback
      begin
        dmGeneral.Connection.RollbackTrans;
        raise EMergeDataError.Create(EST_MERGE_ROLLBACK + #13#10#13#10'Error: ' + E.Message, E);
      end;
    end;
  finally
    SetProgress(0, 100);
    DefaultCursor(lCursor);
  end;
end;

{-------------------------------------------------------------------------------
}
procedure TfraMergeData.PrepareMerge(
    const ATable: String;
    const ASourceKey: String;
    const ATargetKey: String);
begin
  if SameText(ATable, TN_TAXON_LIST_ITEM) then begin
    dmGeneral.ExecuteSQL(
        Format(
            'UPDATE Index_Taxon_Name ' +
            'SET Recommended_Taxon_List_Item_Key=''%s'' ' +
            'WHERE Recommended_Taxon_List_Item_Key=''%s''',
            [ATargetKey, ASourceKey]),
        False);
    dmGeneral.ExecuteSQL(
        Format(
            'DELETE Index_Taxon_Name WHERE Taxon_List_Item_Key=''%s''',
            [ASourceKey]),
        False);
  end;
end;

{-------------------------------------------------------------------------------
}
procedure TfraMergeData.ProcessThreeTableMerge(
    const ATable1: String;
    const ATable2: String;
    const ATable3: String;
    const ASourceKey: TKeyString;
    const ATargetKey: TKeyString);
begin
  SetProgress(0, 120);
  ProcessSingleTableMerge(ATable1, ASourceKey, ATargetKey, 0, 40);
  SetProgress(40, 120);
  ProcessSingleTableMerge(ATable2, ASourceKey, ATargetKey, 40, 40);
  SetProgress(80, 120);
  ProcessSingleTableMerge(ATable3, ASourceKey, ATargetKey, 80, 40);
end;

{-------------------------------------------------------------------------------
  We need to check before merging detail tables, as the database being merged could be
  a partial db and tables may be missing (eg the import db) }
function TfraMergeData.TableExists(const ATableName: String): Boolean;
begin
  if Copy(ATableName, 1, 1) = '~' then
    Result := False // system table - ignore
  else
    with dmGeneral.ExecuteSQL(Format('EXEC sp_tables @Table_Name = ''%s''', [ATableName]), True) do
      Result := not Eof;
end;  // TableExists

{-------------------------------------------------------------------------------
}
procedure TfraMergeData.ProcessSingleTableMerge(
    const ATable: String;
    const ASourceKey: TKeyString;
    const ATargetKey: TKeyString;
    ATaskStart: Integer;
    ATaskMax: Integer);
var
  i: Integer;
  relationships: TJNCCRelationshipList;
begin
  // Raises error if table has composite key - this sort of table cannot be merged.
  try
    GetPrimaryKey(ATable, False);
  except on E:EMultiFieldKey do
    raise EMergeDataError.Create(EST_COMPOUNDKEY + ATable, E);
  end;

  if TableExists(ATable) then
  begin
    relationships := TJNCCRelationshipList.Create(dmGeneral.Connection);
    // Get a count of tables we must work on so we can set the progress bar.
    FWorkToDo := relationships.GetForeignTableCount(ATable);
    FWorkDone := 0;
    // Find the tables we need to fixup.
    for i := 0 to relationships.Count - 1 do
    begin
      with relationships.Relationship[i] do
      begin
        if SameText(MasterTable, ATable) and (not (OneToOneJoin(ATable, MasterTable))) then
          if not IsPrimaryKey(DetailTable, Fields[0].DetailName) then
          begin
            FixupJoin(DetailTable, Fields[0].DetailName, ASourceKey, ATargetKey);
            Inc(FWorkDone);
            if FWorkToDo <> 0 then
              SetProgress(ATaskStart + (FWorkDone * ATaskMax) div FWorkToDo, 120);
          end
          else //Delete the record in the other table
          begin
            if SameText(DetailTable, TN_REFERENCE) then //I have given up trying to be generic
            begin
              dmGeneral.ExecuteSQL(
                  Format(
                      'UPDATE Reference_Author SET Source_Key = ''%s'' WHERE Source_Key = ''%s''',
                      [ATargetKey, ASourceKey]));
              dmGeneral.ExecuteSQL(
                  Format(
                      'UPDATE Reference_Editor SET Source_Key = ''%s'' WHERE Source_Key = ''%s''',
                      [ATargetKey, ASourceKey]));
              dmGeneral.ExecuteSQL(
                  Format(
                      'UPDATE Reference_Number SET Source_Key = ''%s'' WHERE Source_Key = ''%s''',
                      [ATargetKey, ASourceKey]));

              // Renumber Sort_Order on Reference_Author. Move into stored proc?
              dmGeneral.ExecuteSQL(
                  Format(
                      'UPDATE Reference_Author SET Sort_Order = Ordered.Rank FROM ( '
                      + 'SELECT COUNT(*) AS Rank, R1.Author_Key AS AuthKey '
                      + 'FROM Reference_Author R1, Reference_Author R2 '
                      + 'WHERE R1.Source_Key = ''%s'' AND R2.Source_Key = ''%s'' '
                      + 'AND R1.Sort_Order >= R2.Sort_Order '
                      + 'GROUP BY R1.Sort_Order, R1.Author_Key ) AS Ordered '
                      + 'WHERE Source_Key = ''%s'' AND Ordered.AuthKey = Author_Key',
                      [ASourceKey, ASourceKey, ASourceKey]));
            end;
            dmGeneral.ExecuteSQL(
                Format(
                    'DELETE FROM "%s" WHERE "%s" = ''%s''',
                    [DetailTable, Fields[0].DetailName, ASourceKey]));
          end;
      end;
    end;
    // Handle non-physical metadata joins.
    if (ATable = TN_NAME) or (ATable = TN_INDIVIDUAL) then
      UpdateNameTable(ASourceKey, ATargetKey, ATaskStart, ATaskMax);
    if FDeleteSources then
      dmGeneral.ExecuteSQL(
          Format(
              'DELETE FROM "%s" WHERE "%s" = ''%s''',
              [ATable, GetPrimaryKey(ATable, False), ASourceKey]));
  end;
end;
{-------------------------------------------------------------------------------
  returns true if tables at either end of a join are part of a one-one join
  (eg name-individual) }
function TfraMergeData.OneToOneJoin(
    const ATable1: String;
    const ATable2: String): Boolean;
begin
  Result := False; // default
  if SameText(ATable1, TN_NAME) then
    Result := SameText(ATable2, TN_INDIVIDUAL) or
              SameText(ATable2, TN_ORGANISATION)
  else
  if SameText(ATable2, TN_NAME) then
    Result := SameText(ATable1, TN_INDIVIDUAL) or
              SameText(ATable1, TN_ORGANISATION)
  else
  if SameText(ATable1, TN_SOURCE) then
    Result := SameText(ATable2, TN_REFERENCE) or
              SameText(ATable2, TN_SOURCE_FILE)
  else
  if SameText(ATable2, TN_SOURCE) then
    Result := SameText(ATable1, TN_REFERENCE) or
              SameText(ATable1, TN_SOURCE_FILE);
end;  // OneToOneJoin

{-------------------------------------------------------------------------------
}
function TfraMergeData.IsPrimaryKey(
    const AReferencingTable: String;
    const AReferencingField: String): Boolean;
begin
  with dmGeneral.ExecuteSQL(
      Format('EXEC sp_HelpConstraint ''%s'', ''nomsg''', [AReferencingTable]),
      True) do
  begin
    Filter := Format(
        'constraint_keys = ''%s'' and constraint_type like ''PRIMARY KEY%%''',
        [AReferencingField]);
    Result := not (Eof and Bof);
  end;
end;

{-------------------------------------------------------------------------------
}
procedure TfraMergeData.FixupJoin(
    const ADetailTable: String;
    const ADetailName: String;
    const ASourceKey: TKeyString;
    const ATargetKey: TKeyString);
var
  prefField: Boolean;
  canUpdate: Boolean;
begin
  try
    // Ignore this system table
    if ADetailTable = 'Paste Errors' then Exit;

    if TableExists(ADetailTable) then
    begin
      // Have to checked if detail table contains Preferred field, as in Location_Name table
      // If there is such a field, set it to false during the update so the record/name
      // doesn't appear wrongly in a tree
      with dmGeneral.ExecuteSQL(
          Format(
              'SELECT 0 FROM SysColumns WHERE ID=Object_ID(''%s'') AND [Name]=''Preferred''',
              [ADetailTable]),
          True) do
        prefField :=
            not (Bof or Eof) and
            (FUnsetPreferred) and
            (FPreferredLinks.IndexOf(ADetailTable + '-' + ADetailName) <> -1);

      with dmGeneral.ExecuteSQL(
          Format(
              'SELECT Permissions(Object_Id(''%s''), ''%s'') As Perm ',
              [ADetailTable, ADetailName]),
          True) do
        canUpdate := ((Fields['Perm'].Value and 2) <> 0);

      if canUpdate then begin
        // Update Preferred flag BEFORE changing keys over.
        if prefField then
          dmGeneral.ExecuteSQL(
              Format(
                  'UPDATE "%s" SET Preferred = 0 WHERE "%s" = ''%s''',
                  [ADetailTable, ADetailName, ASourceKey]),
              False);
        // Now do the change over.
        try
          dmGeneral.ExecuteSQL(
              Format(
                  'UPDATE "%s" SET "%s" = ''%s'' WHERE "%s" = ''%s''',
                  [ADetailTable, ADetailName, ATargetKey, ADetailName, ASourceKey]),
              False);
        except
          on EOleException do // Caused duplicate primary key.
            dmGeneral.ExecuteSQL(
                Format(
                    'DELETE "%s" WHERE "%s" = ''%s''',
                    [ADetailTable, ADetailName, ASourceKey]),
                False);
        end;
      end;
    end;
  except
    on E:Exception do // Add error info.
      raise EMergeDataError.Create(EST_FIXUP_FAILURE + ADetailTable, E);
  end;
end;

{-------------------------------------------------------------------------------
  The name table has joins all over the place, which are not physically
  present.  They are either Entered_By, Changed_By or Checked_By fields,
  which also need fixing }
procedure TfraMergeData.UpdateNameTable(
    const ASourceKey: String;
    const ATargetKey: String;
    ATaskStart: Integer;
    ATaskMax: Integer);
var
  i: Integer;
  tablesList: TStringList;

  procedure FixupNames(const ANameField: String);
  begin
    with dmGeneral.ExecuteSQL(
        Format(
            'EXEC sp_columns @Table_Name=''%s'', @Column_Name=''%s''',
            [tablesList[i], ANameField]),
        True) do
      FixupJoin(tablesList[i], ANameField, ASourceKey, ATargetKey);
  end;

begin
  tablesList := TStringList.Create;
  try
    dmGeneral.Connection.GetTableNames(tablesList, False);

    for i := 0 to tablesList.Count - 1 do
    begin
      FixupNames('Entered_By');
      FixupNames('Changed_By');
      FixupNames('Checked_By');

      Inc(FWorkDone);
      SetProgress(ATaskStart + (FWorkDone * ATaskMax) div FWorkToDo, 120);
    end;
  finally
    tablesList.Free;
  end;
end;

end.
