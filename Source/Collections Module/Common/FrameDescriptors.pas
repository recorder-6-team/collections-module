{===============================================================================
  Unit:        FrameDescriptors.pas

  Defines:     TfraDescriptors

  Description:

  Model:       CollectionsCommonAndGeneral.mpb

  Created:     June 2003

  Last revision information:
    $Revision: 31 $
    $Date: 16/11/04 10:35 $
    $Author: Ericsalmon $

===============================================================================}

unit FrameDescriptors;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Grids, ExtCtrls, StdCtrls, ImageListButton, InterfaceDataModule,
  DssStringGrid, LuxembourgDataClasses, ComboListID, LuxIDComboBox, ConceptGroupComboBox,
  DataClasses, ADOInt, DataTypes, BaseDetailFrameUnit, BaseTabSheetFrameUnit,
  VagueDate, DateUtils, ExceptionForm;

type
  TfraDescriptors = class;

  TDescriptorItem = class(TLuxGridDataItem)
  private
    FAppliesTo: String;
    FParameterKey: TKeyString;
    FParameterName: String;
    FTimestamp: TSQLSvrTimestamp;
    FValue: String;
  protected
    procedure GetData(const Column: Integer; var AText: String; var AKey: TKeyString);
        override;
    procedure InitFromRecord(AFields: Fields); override;
    procedure SetData(const Column: Integer; const AText: String; const AKey: TKeyString);
        override;
    procedure ValidateData; override;
  public
    function ValidateParameter: Boolean;
    property AppliesTo: String read FAppliesTo;
    property ParameterKey: TKeyString read FParameterKey write FParameterKey;
    property ParameterName: String read FParameterName;
    property Timestamp: TSQLSvrTimestamp read FTimestamp write FTimestamp;
    property Value: String read FValue;
  end;
  
  TDescriptorList = class(TLuxGridDataList)
  private
    FDomainConceptGroupKey: TKeyString;
    FFrame: TfraDescriptors;
    FLocalDomainKey: TKeyString;
    FValidated: Boolean;
    function AddConcept(AItem: TLuxCachedDataItem): TKeyString;
    procedure AddConceptGroup(AItem: TLuxCachedDataItem);
    procedure ReuseNewKey(AKey: TKeyString; AText: String);
    procedure UpdateLast10Concepts(AParameterKey : TKeyString; AItem: TLuxCachedDataItem);
    procedure ValidateParameters;
  protected
    procedure DoAddition(AItem: TLuxCachedDataItem); override;
    procedure DoDeletion(AItem: TLuxCachedDataItem); override;
    procedure DoModification(AItem: TLuxCachedDataItem); override;
    function GetRecordset: _Recordset; override;
  public
    procedure ValidateContent; override;
    property DomainConceptGroupKey: TKeyString read FDomainConceptGroupKey write
        FDomainConceptGroupKey;
    property Frame: TfraDescriptors read FFrame write FFrame;
    property LocalDomainKey: TKeyString read FLocalDomainKey write FLocalDomainKey;
    property Validated: Boolean read FValidated write FValidated;
  end;
  
  {-----------------------------------------------------------------------------
    Descriptors tab page control.  This tab page is embedded onto the page control for the
    details of collection units (stores, specimens and collections) and occurrences.  The
    contents are stored in *_Data tables along with Measurements data.
  }
  TfraDescriptors = class(TBaseTabSheetFrame)
    btnAdd: TImageListButton;
    btnDel: TImageListButton;
    cmbParameter: TConceptGroupComboBox;
    sgDescriptors: TDSSStringGrid;
    shpList: TShape;
    procedure btnAddClick(Sender: TObject);
    procedure btnDelClick(Sender: TObject);
    procedure cmbParameterExit(Sender: TObject);
    procedure cmbParameterKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure cmbParameterPopulate(Sender: TObject);
    procedure sgDescriptorsKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    FDeleteAllStoredProcName: String;
    FDescriptorList: TDescriptorList;
    FDomainGroupStoredProcName: String;
    FLast10Concepts: TStringList;
    FParamKey: TKeyData;
    FParentKeyParamName: String;
    FTableName: String;
    FTableNameCompact: String;
    function CanAddRow: Boolean;
    procedure ClearLast10ConceptsList;
    function GetParams: TVariantArray;
    function GridIsEmpty: Boolean;
  protected
    procedure DeleteData; override;
    procedure EnableControls(AEnabled: Boolean); override;
    procedure LoadData; override;
    procedure RegisterControls; override;
    procedure SaveData; override;
    procedure SetKey(Value: TKeyString); override;
    procedure SetMasterFrameType(Value: TMasterFrameType); override;
    procedure ValidateData; override;
    property ParentKeyParamName: String read FParentKeyParamName;
    property TableName: String read FTableName;
    property TableNameCompact: String read FTableNameCompact;
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
    procedure UpdateComboBox;
    property Last10Concepts: TStringList read FLast10Concepts write FLast10Concepts;
    property ParamKey: TKeyData read FParamKey write FParamKey;
  end;
  
//==============================================================================
implementation

uses
  GeneralData, ApplicationSettings, ResourceStrings, LuxembourgConstants, Validation;

{$R *.dfm}

{-==============================================================================
    TDescriptorItem
===============================================================================}
{-------------------------------------------------------------------------------
}
procedure TDescriptorItem.GetData(const Column: Integer; var AText: String; var AKey:
    TKeyString);
begin
  AKey := '';
  AText := '';
  case Column of
    0: AText := AppliesTo;
    1: begin
         AText := ParameterName;
         AKey := ParameterKey;
       end;
    2: AText := Value;
  end;
end;  // TDescriptorItem.GetData 

{-------------------------------------------------------------------------------
}
procedure TDescriptorItem.InitFromRecord(AFields: Fields);
begin
  // Set DomainConceptGroupKey for list.
  // Set item's properties.
  SetItemKey(VarToStr(AFields['Item_Key'].Value));
  FParameterKey  := AFields['Parameter_Concept_Key'].Value;
  FParameterName := AFields['Parameter_Term'].Value;
  FValue         := AFields['Value'].Value;
  FAppliesTo     := AFields['Applies_To'].Value;
  FTimestamp     := AFields['Timestamp'].Value;
end;  // TDescriptorItem.InitFromRecord 

{-------------------------------------------------------------------------------
}
procedure TDescriptorItem.SetData(const Column: Integer; const AText: String; const AKey:
    TKeyString);
begin
  case Column of
    0: if FAppliesTo <> AText then begin
         FAppliesTo := AText;
         SetModified;
       end;
    1: if (FParameterName <> AText) then
       begin
         FParameterName := AText;
         FParameterKey := AKey;
         SetModified;
       end;
    2: if FValue <> AText then begin
         FValue := AText;
         SetModified;
       end;
  end;
end;  // TDescriptorItem.SetData

{-------------------------------------------------------------------------------
  Standard validation to check there is data in all the required columns for the relevant
      rows.
}
procedure TDescriptorItem.ValidateData;
begin
  ValidateValue(FAppliesTo <> '', Format(ResStr_MissingData, [ResStr_AppliesTo]));
  ValidateValue(FParameterName <> '', Format(ResStr_MissingData, [ResStr_Parameter]));
  ValidateValue(FValue <> '', Format(ResStr_MissingData, [ResStr_Value]));
end;  // TDescriptorItem.ValidateData 

{-------------------------------------------------------------------------------
  Validates the text in the parameter column to see if it is already in the database. This has
      been separated from the rest of the validation because the TDescriptorList object needs
      to store the parameters it has already asked about in a StringList. This is to stop
      asking the user if they want to add the same parameter twice.
}
function TDescriptorItem.ValidateParameter: Boolean;
begin
  Result := True;
  // If the user has typed in an entry into the combo box, we will not have a key.
  if FParameterKey = '' then
  begin
     // Check to see whether the text entered by the user into the combo box is
     // in the system supplied CG_DESCRIPTOR_PARAMETERS
     // or not.
    FParameterKey := VarToStr(dmGeneral.GetStoredProcOutputParam
        ('usp_ConceptKey_Get_ForConceptGroupAndItemName',
         ['@ConceptGroupKey', CG_DESCRIPTOR_PARAMETERS,
          '@Plaintext', FParameterName], '@ConceptKey'));
    // What? No Key???
    if FParameterKey = '' then
    begin
      // DomainCG exists, look for ParamKey.
      if TDescriptorList(Ownerlist).DomainConceptGroupKey <> '' then
        FParameterKey := VarToStr(dmGeneral.GetStoredProcOutputParam
            ('usp_ConceptKey_Get_ForConceptGroupAndItemName',
             ['@ConceptGroupKey', TDescriptorList(Ownerlist).DomainConceptGroupKey,
              '@Plaintext', FParameterName], '@ConceptKey'));
      // Still empty!!!
      if FParameterKey = '' then
        Result := False;
    end;
  end;
end;  // TDescriptorItem.ValidateParameter 

{-==============================================================================
    TDescriptorList
===============================================================================}
{-------------------------------------------------------------------------------
}
function TDescriptorList.AddConcept(AItem: TLuxCachedDataItem): TKeyString;
begin
  Result := VarToStr(dmGeneral.RunInsertStoredProc(TN_CONCEPT, 'usp_Concept_Insert',
               ['@ConceptGroupKey', DomainConceptGroupKey,
                '@TermName', TDescriptorItem(AItem).ParameterName,
                '@LanguageKey', AppSettings.ISOLanguage
               ], '@Key'));
  // See if this new Concept key can be used elsewhere.
  ReuseNewKey(Result, TDescriptorItem(AItem).ParameterName);
end;  // TDescriptorList.AddConcept 

{-------------------------------------------------------------------------------
}
procedure TDescriptorList.AddConceptGroup(AItem: TLuxCachedDataItem);
begin
  DomainConceptGroupKey := VarToStr(dmGeneral.RunInsertStoredProc
              (TN_CONCEPT_GROUP,'usp_ConceptGroup_Insert',
               ['@ConceptGroupName', ST_DESCRIPTOR_PARAMETERS,
                '@LocalDomainKey', LocalDomainKey,
                '@Version', '1'
               ], '@Key'));
end;  // TDescriptorList.AddConceptGroup 

{-------------------------------------------------------------------------------
  This method is called for each modified row in the String Grid 
}
procedure TDescriptorList.DoAddition(AItem: TLuxCachedDataItem);
begin
  if DomainConceptGroupKey = '' then
    AddConceptGroup(AItem);
  
  with TDescriptorItem(AItem) do begin
    if ParameterKey = '' then
      FParameterKey := AddConcept(AItem);
  
    UpdateLast10Concepts(ParameterKey, AItem);
  
    // Actually save the Data record itself.
    dmGeneral.RunInsertStoredProc(Frame.TableName,
                                  'usp_' + Frame.TableNameCompact + '_Insert',
                                  ['@IsDescriptor', 1,  // Descriptor data.
                                   Frame.ParentKeyParamName, MasterKey,
                                   '@ParameterConceptKey', ParameterKey,
                                   '@AppliesTo', AppliesTo,
                                   '@Value', Value
                                  ], '@Key');
  end;
end;  // TDescriptorList.DoAddition 

{-------------------------------------------------------------------------------
}
procedure TDescriptorList.DoDeletion(AItem: TLuxCachedDataItem);
begin
  with TDescriptorItem(AItem) do
    dmGeneral.RunDeleteStoredProc('usp_' + Frame.TableNameCompact + '_Delete',
                                  ['@Key', ItemKey, '@Timestamp', Timestamp]);
end;  // TDescriptorList.DoDeletion 

{-------------------------------------------------------------------------------
  This method is called for each modified row in the String Grid 
}
procedure TDescriptorList.DoModification(AItem: TLuxCachedDataItem);
begin
  if DomainConceptGroupKey = '' then
    AddConceptGroup(AItem);
  
  with TDescriptorItem(AItem) do begin
    if ParameterKey = '' then
      ParameterKey := AddConcept(AItem);
  
    UpdateLast10Concepts(ParameterKey, AItem);
  
    // Actually save the Data record itself.
    dmGeneral.RunUpdateStoredProc('usp_' + Frame.TableNameCompact + '_Update',
                                 ['@Key', ItemKey,
                                  '@IsDescriptor', 1, // 1 indicates it is Descriptor data.
                                  '@ParameterConceptKey', ParameterKey,
                                  '@AppliesTo', AppliesTo,
                                  '@Value', Value,
                                  '@Timestamp', Timestamp
                                 ]);
  end;
end;  // TDescriptorList.DoModification 

{-------------------------------------------------------------------------------
}
function TDescriptorList.GetRecordset: _Recordset;
begin
  Result := dmGeneral.GetRecordset('usp_' + Frame.TableNameCompact + '_Select',
                                   ['@Key', MasterKey, '@IsDescriptor', 1]);
end;  // TDescriptorList.GetRecordset 

{-------------------------------------------------------------------------------
  Once a new Concept has been added, go through the list checking the other items that don't
      have a key, and see if they have the same term as the concept we just added. If so, give
      these terms the newly generated Concept Key. This will stop it trying to add new
      Concepts with this term multiple times.
}
procedure TDescriptorList.ReuseNewKey(AKey: TKeyString; AText: String);
var
  lIdx: Integer;
begin
  for lIdx := 0 to Count-1 do
    with TDescriptorItem(Items[lIdx]) do
      if (ParameterKey = '') and (ParameterName = AText) then
        ParameterKey := AKey;
end;  // TDescriptorList.ReuseNewKey 

{-------------------------------------------------------------------------------
}
procedure TDescriptorList.UpdateLast10Concepts(AParameterKey : TKeyString; AItem:
    TLuxCachedDataItem);
var
  lIndex: Integer;
  i: Integer;
begin
  with TfraDescriptors(FFrame).Last10Concepts do
  begin
    lIndex := IndexOf(TDescriptorItem(AItem).FParameterName);
  
    if lIndex > -1 then // If the parameter name is in the last 10 item list.
    begin
      TKeyData(Objects[lIndex]).ItemKey := AParameterKey;
  
      for i := 0 to Count - 1 do
        AppSettings.AddItemToPersistentList(PL_DESCRIPTOR_PARAMETERS + DomainConceptGroupKey,
                                            AParameterKey, MAX_LAST_USED);
    end
  end;
end;  // TDescriptorList.UpdateLast10Concepts 

{-------------------------------------------------------------------------------
  When the user clicks save, the method will be called twice. Once before anything is saved to
      check everthing is OK, and then during the save. However, we only actually want it to
      validate once, so there is a Validated property that is set from the frame that ensures
      validation only happens once.
}
procedure TDescriptorList.ValidateContent;
begin
  if not FValidated then begin
    inherited;
    ValidateParameters;
  end;
end;  // TDescriptorList.ValidateContent 

{-------------------------------------------------------------------------------
  Validate the parameters. If the user is asked whether they want to add the parameter to the
      database, this method will store that parameter in a string list. This will stop the
      user being asked if they want to add the same parameter twice.
}
procedure TDescriptorList.ValidateParameters;
var
  i: Integer;
  lList: TStringList;
  lItem: TDescriptorItem;
begin
  lList := TStringList.Create;
  for i := 0 to Count - 1 do begin
    lItem := TDescriptorItem(Items[i]);
    // See if the parameter is in the database
    if not lItem.ValidateParameter then
      // It isn't in the database. See if the user has already been asked about it.
      // Also, it the parameter text is '' then don't bother checking it.
      if (lList.IndexOf(lItem.ParameterName) = -1) and not (lItem.ParameterName = '') then
      begin
        // The user hasn't been asked yet about this parameter. Ask them.
        if MessageDlg(Format(ResStr_AddMeasurementParameter, [lItem.ParameterName]),
                             mtInformation, [mbYes, mbNo], 0) <> mrYes then
          Abort
        else
          // The user wants to add it. Store it in the string list so that
          // they won't be asked again.
          lList.Add(lItem.ParameterName);
      end
  end;
  // Free the list because we are no longer interested in its contents.
  lList.Free;
end;  // TDescriptorList.ValidateParameters 

{-==============================================================================
    TfraDescriptors
===============================================================================}
{-------------------------------------------------------------------------------
  Constructor. Creates the list for the string grid and sets some properties of the
      components.
}
constructor TfraDescriptors.Create(aOwner: TComponent);
begin
  inherited Create(AOwner);
  
  FDescriptorList := TDescriptorList.Create(TDescriptorItem, sgDescriptors);
  FDescriptorList.Frame := Self;
  
  FLast10Concepts := TStringList.Create;
  
  sgDescriptors.Rows[0].CommaText := '"Applies To",Parameter,Value';
  // Setting the control sets the type to custom.
  sgDescriptors.ColumnsInfo[1].WinControl := cmbParameter;
  
  // Ensures the combo box can be typed into.
  cmbParameter.Style := csDropDown;
end;  // TfraDescriptors.Create 

{-------------------------------------------------------------------------------
  Destructor. Frees the stringlist containing the last 10 concepts. 
}
destructor TfraDescriptors.Destroy;
begin
  ClearLast10ConceptsList;
  FLast10Concepts.Free;
  
  inherited Destroy;
end;  // TfraDescriptors.Destroy 

{-------------------------------------------------------------------------------
  Handles a click on the 'add' button. 
}
procedure TfraDescriptors.btnAddClick(Sender: TObject);
begin
  inherited;
  with sgDescriptors do begin
    if (FDescriptorList.LocalDomainKey = '') then
      MessageDlg(ResStr_CannotAddDescriptors, mtInformation, [mbOK], 0)
    else
    if CanAddRow then begin
      FDescriptorList.AddNew(TDescriptorItem.CreateNew(FDescriptorList));
      Col := 0;
    end;
    Row := RowCount - 1;
    SetFocus;
  end;
end;  // TfraDescriptors.btnAddClick 

{-------------------------------------------------------------------------------
  Handles a click on the 'delete' button. 
}
procedure TfraDescriptors.btnDelClick(Sender: TObject);
begin
  inherited;
  if MessageDlg(Format(ResStr_ConfirmRowDelete, [ResStr_Row]),
                mtWarning, [mbYes, mbNo], 0) = mrYes then
  begin
    sgDescriptors.EditorMode := False;
    FDescriptorList.DeleteItem(sgDescriptors.Row);
  end;
end;  // TfraDescriptors.btnDelClick 

{-------------------------------------------------------------------------------
  Sees if the user should be allowed to add a new row to the grid. 
}
function TfraDescriptors.CanAddRow: Boolean;
begin
  with sgDescriptors do
    Result := (Cells[0, RowCount - 1] <> '') or (Cells[1, RowCount - 1] <> '') or
              (Cells[2, RowCount - 1] <> '');
end;  // TfraDescriptors.CanAddRow 

{-------------------------------------------------------------------------------
}
procedure TfraDescriptors.ClearLast10ConceptsList;
var
  i: Integer;
begin
  for i := 0 to FLast10Concepts.Count - 1 do
    FLast10Concepts.Objects[i].Free;
  
  FLast10Concepts.Clear;
end;  // TfraDescriptors.ClearLast10ConceptsList 

{-------------------------------------------------------------------------------
  When the parameter combo box is exitted, it checks to see whether the item is already in the list of the last 10 used items. If it isn't, then it is added to the head of the list. The item that has now been pushed out of the last 10 items is then removed.
  
  If the item is in the list and it already has a key, then this key is also stored in the
      list of 10 items.
}
procedure TfraDescriptors.cmbParameterExit(Sender: TObject);
begin
  inherited;
  
  if FLast10Concepts.IndexOf(cmbParameter.Text) = -1 then
    if cmbParameter.Text <> '' then
    begin
      FParamKey := TKeyData.Create;
      FParamKey.ItemKey := cmbParameter.CurrentStrID;
  
      FLast10Concepts.InsertObject(0, cmbParameter.Text, FParamKey);
  
      while FLast10Concepts.Count > MAX_LAST_USED do begin
        FLast10Concepts.Objects[MAX_LAST_USED].Free;
        FLast10Concepts.Delete(MAX_LAST_USED);
      end;
  
      UpdateComboBox;
    end;
end;  // TfraDescriptors.cmbParameterExit

{-------------------------------------------------------------------------------
}
procedure TfraDescriptors.cmbParameterKeyDown(Sender: TObject; var Key: Word; Shift:
    TShiftState);
begin
  inherited;
  
  if (Key = VK_RETURN)then
    if not CheckConceptGroupParameter(cmbParameter, cmbParameter.text,
                                      CG_DESCRIPTOR_PARAMETERS + ';' +
                                      FDescriptorList.DomainConceptGroupKey) then
      Key := 0
    else
      UpdateComboBox;
end;  // TfraDescriptors.cmbParameterKeyDown 

{-------------------------------------------------------------------------------
  Populate the combo box with the 10 last used concepts. 
}
procedure TfraDescriptors.cmbParameterPopulate(Sender: TObject);
var
  lIdx: Integer;
begin
  PopulateLast10ConceptsCombo(cmbParameter, FDescriptorList.DomainConceptGroupKey,
      PL_DESCRIPTOR_PARAMETERS + FDescriptorList.DomainConceptGroupKey,
      CG_DESCRIPTOR_PARAMETERS);
  // Now that the combobox is populated, populate the string list with the
  // same items that are in the combo box.
  for lIdx := 0 to cmbParameter.Count-1 do begin
    FParamKey := TKeyData.Create;
    FParamKey.ItemKey := cmbParameter.StrID[lIdx];
    FLast10Concepts.InsertObject(0, cmbParameter.Items[lIdx], FParamKey);
  end;
end;  // TfraDescriptors.cmbParameterPopulate 

{-------------------------------------------------------------------------------
}
procedure TfraDescriptors.DeleteData;
begin
  dmGeneral.RunDeleteStoredProc(FDeleteAllStoredProcName,
                                [FParentKeyParamName, Key, '@IsDescriptor', 1]);
end;  // TfraDescriptors.DeleteData 

{-------------------------------------------------------------------------------
  Toggle the read only property of the string grid. 
}
procedure TfraDescriptors.EnableControls(AEnabled: Boolean);
begin
  inherited;
  with sgDescriptors do begin
    // If there is no determination, the local domain and the domain concept
    // group key cannot be retrieved. This means parameters cannot be saved,
    // so don't give the option to add / remove them.
    ReadOnly := (not AEnabled) or (FDescriptorList.LocalDomainKey = '');
  end;
end;  // TfraDescriptors.EnableControls 

{-------------------------------------------------------------------------------
  Top level Specimens can either be a life science or earth science. Hence, when they are
      clicked on, this information needs to be passed into the usp_Specimen_Select procedure
      for each node. Hence, FLifeScience needs to be set for each node clicked on.
}
function TfraDescriptors.GetParams: TVariantArray;
begin
  Result := VarArrayOf(['@Key', Key,
                        '@DomainConceptGroupName', ST_DESCRIPTOR_PARAMETERS,
                        '@TopLevelNodeContext', NodeContext]);
end;  // TfraDescriptors.GetParams 

{-------------------------------------------------------------------------------
  See whether the grid is empty or not. Do this by checking the top row of the grid and the
      number of items in FDescriptorList.
}
function TfraDescriptors.GridIsEmpty: Boolean;
begin
  with sgDescriptors do
    Result := (Cells[0, 1] = '') and
              (Cells[1, 1] = '') and
              (Cells[2, 1] = '') and
              (FDescriptorList.Count <= 1);
end;  // TfraDescriptors.GridIsEmpty 

{-------------------------------------------------------------------------------
  Load the data from the recordset and initialize some variables. 
}
procedure TfraDescriptors.LoadData;
begin
  inherited;
  ClearLast10ConceptsList;
  cmbParameter.ItemIndex := -1;
  FDescriptorList.MasterKey := Key;
  if Assigned(RegisteredRecordsets[0]) then
    with RegisteredRecordsets[0] do
      if not Eof then begin
        FDescriptorList.LocalDomainKey := VarToStr(Fields['Local_Domain_Key'].Value);
        FDescriptorList.DomainConceptGroupKey :=
                                          VarToStr(Fields['Concept_Group_Key'].Value);
      end else begin
        FDescriptorList.LocalDomainKey := '';
        FDescriptorList.DomainConceptGroupKey := '';
        cmbParameter.Clear;
      end;
  sgDescriptors.EditorMode := False;
  FDescriptorList.Refresh;
  // Need to populate combo box when loading data. This is because if the user
  // enters a new parameter without having populated the combo box, when they
  // do populate it, the new item they entered will get cleared.
  // The combo box is populated if it hasn't yet been, or if the node context
  // is a specimen. This is because different specimen_units belong to different
  // domains, and each domain has a different list of most recently used parameters.
  // Therefore, it is best to repopulate the combo box each time for specimens.
  if (not cmbParameter.Populated) or (NodeContext = ncSpecimen) then
    cmbParameter.PopulateContent;
end;  // TfraDescriptors.LoadData 

{-------------------------------------------------------------------------------
  Register the recordset. 
}
procedure TfraDescriptors.RegisterControls;
begin
  inherited;
  // May not need to run the proc.
  if FDomainGroupStoredProcName <> '' then
    // Get the right Domain concept group, for when new descriptors are added to
    // the Thesaurus.
    RegisterRecordset(FDomainGroupStoredProcName, GetParams);
end;  // TfraDescriptors.RegisterControls 

{-------------------------------------------------------------------------------
  Save modified data to the database. 
}
procedure TfraDescriptors.SaveData;
begin
  if not GridIsEmpty then FDescriptorList.Update;
  sgDescriptors.EditorMode := False;
end;  // TfraDescriptors.SaveData 

{-------------------------------------------------------------------------------
}
procedure TfraDescriptors.SetKey(Value: TKeyString);
begin
  inherited SetKey(Value);
  
  FDescriptorList.MasterKey := Key;
end;  // TfraDescriptors.SetKey 

{-------------------------------------------------------------------------------
  Sets the table name and compact  
}
procedure TfraDescriptors.SetMasterFrameType(Value: TMasterFrameType);
begin
  inherited;
  case MasterFrameType of
    mftCollectionUnit:
      begin
        FTableName := TN_COLLECTION_UNIT_DATA;
        FTableNameCompact := StringReplace(TN_COLLECTION_UNIT_DATA, '_', '', [rfReplaceAll]);
        FParentKeyParamName := '@CollectionUnitKey';
        FDomainGroupStoredProcName := 'usp_DomainConceptGroup_Select_ForCollectionUnit';
        FDeleteAllStoredProcName := 'usp_CollectionUnitData_Delete_ForCollectionUnit';
      end;
    mftOccurrence:
      begin
        FTableName := TN_OCCURRENCE_DATA;
        FTableNameCompact := StringReplace(TN_OCCURRENCE_DATA, '_', '', [rfReplaceAll]);
        FParentKeyParamName := '@OccurrenceKey';
        FDomainGroupStoredProcName := 'usp_DomainConceptGroup_Select_ForOccurrence';
        FDeleteAllStoredProcName := 'usp_OccurrenceData_Delete_ForOccurrence';
      end;
    mftLocationFeature:
      begin
        { Tabs in Recorder are smaller than in Collections. So reduce last column to fit a
          vertical scrollbar. The 4 is for the grid border and ColCount for the column
          separators. User can still resize each column after that though.}
        with sgDescriptors do
          ColWidths[2] := Width - ColWidths[0] - ColWidths[1] -
                          4 - ColCount - GetSystemMetrics(SM_CXVSCROLL);

        FTableName := TN_LOCATION_FEATURE_DATA;
        FTableNameCompact := StringReplace(TN_LOCATION_FEATURE_DATA, '_', '', [rfReplaceAll]);
        FParentKeyParamName := '@LocationFeatureKey';
        FDomainGroupStoredProcName := '';  // Use global keys, no need for proc.
        FDeleteAllStoredProcName := '';  // Cascade delete on table, no need for proc.
  
        FDescriptorList.LocalDomainKey := LD_GLOBAL_SYSTEM_TERM_LIST;
        FDescriptorList.DomainConceptGroupKey := CG_DESCRIPTOR_PARAMETERS;
      end;
  end;
end;  // TfraDescriptors.SetMasterFrameType 

{-------------------------------------------------------------------------------
  Handles a keypress whilst the grid is focussed. 
}
procedure TfraDescriptors.sgDescriptorsKeyDown(Sender: TObject; var Key: Word; Shift:
    TShiftState);
begin
  inherited;
  with sgDescriptors do
    if (Key = VK_DOWN) then
      if (FDescriptorList.LocalDomainKey = '') then
        MessageDlg(ResStr_CannotAddDescriptors, mtInformation, [mbOK], 0)
      else if (Row = RowCount-1) then begin
        Key := 0;
        if CanAddRow then begin
          FDescriptorList.AddNew(TDescriptorItem.CreateNew(FDescriptorList));
          Row := RowCount - 1;
          Col := 0;
          SetFocus;
          EditorMode := True;
        end;
      end;
end;  // TfraDescriptors.sgDescriptorsKeyDown 

{-------------------------------------------------------------------------------
  This method updates the combo box history without actually saving anything to the registry -
      it is just called so that the user can type something into the cell, move to the next
      cell, and then be able to select what they just typed in.
}
procedure TfraDescriptors.UpdateComboBox;
var
  i: Integer;
begin
  for i := 0 to Last10Concepts.Count - 1 do
    if cmbParameter.IndexOf(Last10Concepts[i]) = -1 then
      cmbParameter.Add(Last10Concepts[i], TKeyData(Last10Concepts.Objects[i]).ItemKey);
end;  // TfraDescriptors.UpdateComboBox 

{-------------------------------------------------------------------------------
  All tab pages are validated before anything is actually saved. The list used by the string
      grid needs to be told to validate.
}
procedure TfraDescriptors.ValidateData;
begin
  inherited;
  // We want the string grid to be validated before any of the tab pages are
  // saved to the database. Normally, the string grid would be validated
  // again immediately prior to the tab page being saved. Hence, if the user
  // had entered a new parameter, they would be asked twice if they wanted to
  // add it to the database. The Validated property in TDescriptorList can be
  // set from here to ensure validation only occurs as we want.
  if not GridIsEmpty then
    with FDescriptorList do begin
      Validated := False;
      ValidateContent;
      Validated := True;
    end;
end;  // TfraDescriptors.ValidateData 

end.



