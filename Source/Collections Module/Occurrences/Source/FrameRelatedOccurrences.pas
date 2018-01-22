{===============================================================================
  Unit:        FrameRelatedOccurrences

  Defines:     TfraRelatedOccurrences

  Description:

  Model:       Occurrences.mpb

  Created:     October 2003

  Last revision information:
    $Revision: 4 $
    $Date: 19/12/06 14:36 $
    $Author: Ericsalmon $

===============================================================================}

unit FrameRelatedOccurrences;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, InterfaceDataModule, BaseTabSheetFrameUnit, StdCtrls, Grids, DataTypes,
  ImageListButton, ExtCtrls, ComboListID, LuxIDComboBox, DssStringGrid, ADOInt, DSSDataTypes,
  DropTarget, LuxembourgDataClasses, DataClasses, LinkedControls, BaseCompositeComponent;

type
  TRelatedOccItem = class (TLuxGridDataItem)
  private
    FComments: String;
    FRelatedOccKey: String;
    FRelatedOccName: String;
    FRelationTypeKey: String;
    FRelationTypeName: String;
    procedure SetComments(const Value: String);
    procedure SetRelatedOccKey(const Value: String);
    procedure SetRelatedOccName(const Value: String);
    procedure SetRelationTypeKey(const Value: String);
    procedure SetRelationTypeName(const Value: String);
  protected
    procedure GetData(const Column: Integer; var AText: String; var AKey: TKeyString); 
        override;
    procedure InitFromRecord(AFields: Fields); override;
  public
    property Comments: String read FComments write SetComments;
    property RelatedOccKey: String read FRelatedOccKey write SetRelatedOccKey;
    property RelatedOccName: String read FRelatedOccName write SetRelatedOccName;
    property RelationTypeKey: String read FRelationTypeKey write SetRelationTypeKey;
    property RelationTypeName: String read FRelationTypeName write SetRelationTypeName;
  end;
  
  TRelatedOccList = class (TLuxGridDataList)
  protected
    procedure DoAddition(AItem: TLuxCachedDataItem); override;
    procedure DoDeletion(AItem: TLuxCachedDataItem); override;
    procedure DoModification(AItem: TLuxCachedDataItem); override;
    function GetRecordset: _Recordset; override;
  end;
  
  TfraRelatedOccurrences = class (TBaseTabSheetFrame)
    btnAccept: TImageListButton;
    btnAdd: TImageListButton;
    btnDelete: TImageListButton;
    btnDiscard: TImageListButton;
    btnEdit: TImageListButton;
    cmbRelationshipTypes: TLuxIDComboBox;
    eRelatedOcc: TLinkedEdit;
    gbRelOccDetails: TGroupBox;
    Label4: TLabel;
    lblRelatedOcc: TLabel;
    lblType: TLabel;
    mmComments: TMemo;
    sgRelatedOccs: TDSSStringGrid;
    Shape5: TShape;
    procedure btnAcceptClick(Sender: TObject);
    procedure btnAddClick(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
    procedure btnDiscardClick(Sender: TObject);
    procedure btnEditClick(Sender: TObject);
    procedure cmbRelationshipTypesPopulate(Sender: TObject);
    procedure eRelatedOccFindData(Sender: TObject);
    procedure sgRelatedOccsClick(Sender: TObject);
    procedure sgRelatedOccsKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    FNewItem: Boolean;
    FRelatedOccs: TRelatedOccList;
    procedure ClearDetails;
    procedure DropRelatedOcc(const Sender: TObject; const AFormat: Integer; const ASourceData: 
        TKeyList; const ATextStrings: TstringList; var AHandled: Boolean);
    procedure DropRelatedOccOnEdit(const Sender: TObject; const AFormat: Integer; const 
        ASourceData: TKeyList; const ATextStrings: TstringList; var AHandled: Boolean);
    procedure DropRelatedOccOnGrid(const Sender: TObject; const AFormat: Integer; const 
        ASourceData: TKeyList; const ATextStrings: TstringList; var AHandled: Boolean);
    procedure EditDetails(ANewItem: Boolean);
    procedure RefreshDetails;
  protected
    procedure DeleteData; override;
    procedure EnableControls(AEnabled: Boolean); override;
    procedure LoadData; override;
    procedure RegisterDragDropComponents; override;
    procedure SaveData; override;
    procedure SetKey(Value: TKeyString); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;
  
//==============================================================================
implementation

{$R *.dfm}

uses
  GeneralData, ExceptionForm, ResourceStrings, Validation, SearchManager, LuxembourgConstants;

{-==============================================================================
    TfraRelatedOccurrences
===============================================================================}
{-------------------------------------------------------------------------------
}
constructor TfraRelatedOccurrences.Create(AOwner: TComponent);
begin
  inherited;
  
  FRelatedOccs := TRelatedOccList.Create(TRelatedOccItem, sgRelatedOccs);
  sgRelatedOccs.Options := sgRelatedOccs.Options + [goRowSelect];
end;  // TfraRelatedOccurrences.Create 

{-------------------------------------------------------------------------------
}
destructor TfraRelatedOccurrences.Destroy;
begin
  FRelatedOccs.Free;
  
  inherited;
end;  // TfraRelatedOccurrences.Destroy 

{-------------------------------------------------------------------------------
}
procedure TfraRelatedOccurrences.btnAcceptClick(Sender: TObject);
var
  lItem: TRelatedOccItem;
begin
  inherited;
  ValidateValue(eRelatedOcc.Text <> '',
                Format(ResStr_MissingData,
                       [Trim(StringReplace(lblRelatedOcc.Caption, ':', '', [rfReplaceAll]))]),
                eRelatedOcc);
  ValidateValue(DoCheck(eRelatedOcc, stOccurrence),
                Format(ResStr_InvalidData,
                       [Trim(StringReplace(lblRelatedOcc.Caption, ':', '', [rfReplaceAll]))]),
                eRelatedOcc);
  ValidateValue(eRelatedOcc.Key <> Key, ResStr_OccurrenceCircularRef, eRelatedOcc);
  ValidateValue(cmbRelationshipTypes.ItemIndex <> -1,
                Format(ResStr_MissingData,
                       [Trim(StringReplace(lblType.Caption, ':', '', [rfReplaceAll]))]),
                cmbRelationshipTypes);
  
  // If new item, create object
  if FNewItem then
    lItem := TRelatedOccItem.CreateNew(FRelatedOccs)
  else
    lItem := TRelatedOccItem(sgRelatedOccs.Objects[0, sgRelatedOccs.Row]);
  
  // Update properties.
  with lItem do begin
    RelatedOccKey    := eRelatedOcc.Key;
    RelatedOccName   := eRelatedOcc.Text;
    RelationTypeKey  := cmbRelationshipTypes.CurrentStrID;
    RelationTypeName := cmbRelationshipTypes.CurrentItem;
    Comments         := mmComments.Text;
  end;
  
  // Add new item to list
  if FNewItem then FRelatedOccs.AddNew(lItem);
  
  // And update controls state.
  EnableControls(True);
end;  // TfraRelatedOccurrences.btnAcceptClick 

{-------------------------------------------------------------------------------
}
procedure TfraRelatedOccurrences.btnAddClick(Sender: TObject);
begin
  inherited;
  EditDetails(True);
end;  // TfraRelatedOccurrences.btnAddClick 

{-------------------------------------------------------------------------------
}
procedure TfraRelatedOccurrences.btnDeleteClick(Sender: TObject);
begin
  inherited;
  if MessageDlg(ResStr_ConfirmRowDelete, mtInformation, [mbYes, mbNo], 0) = mrYes then begin
    FRelatedOccs.DeleteItem(sgRelatedOccs.Row);
    sgRelatedOccsClick(nil);
  end;
end;  // TfraRelatedOccurrences.btnDeleteClick 

{-------------------------------------------------------------------------------
}
procedure TfraRelatedOccurrences.btnDiscardClick(Sender: TObject);
begin
  inherited;
  EnableControls(True);
end;  // TfraRelatedOccurrences.btnDiscardClick 

{-------------------------------------------------------------------------------
}
procedure TfraRelatedOccurrences.btnEditClick(Sender: TObject);
begin
  inherited;
  EditDetails(False);
end;  // TfraRelatedOccurrences.btnEditClick 

{-------------------------------------------------------------------------------
}
procedure TfraRelatedOccurrences.ClearDetails;
begin
  eRelatedOcc.Text := '';
  eRelatedOcc.Key  := '';
  cmbRelationshipTypes.ItemIndex := -1;
  mmComments.Text  := '';
end;  // TfraRelatedOccurrences.ClearDetails 

{-------------------------------------------------------------------------------
}
procedure TfraRelatedOccurrences.cmbRelationshipTypesPopulate(Sender: TObject);
begin
  inherited;
  with dmGeneral.GetRecordset('usp_ThesaurusRelationForwardReverse_Select', [
      '@RelationUsage', 4]) do begin
    while not Eof do begin
      cmbRelationshipTypes.Add(VarToStr(Fields['Item_Name']),
                               VarToStr(Fields['Thesaurus_Relation_Type_Key']));
      MoveNext;
    end;
    Close;
  end;
end;  // TfraRelatedOccurrences.cmbRelationshipTypesPopulate 

{-------------------------------------------------------------------------------
}
procedure TfraRelatedOccurrences.DeleteData;
begin
  dmGeneral.RunDeleteStoredProc('usp_OccurrenceRelations_Delete_ForOccurrence',
                                ['@OccurrenceKey', Key]);
end;  // TfraRelatedOccurrences.DeleteData 

{-------------------------------------------------------------------------------
}
procedure TfraRelatedOccurrences.DropRelatedOcc(const Sender: TObject; const AFormat: Integer; 
    const ASourceData: TKeyList; const ATextStrings: TstringList; var AHandled: Boolean);
var
  lItemName: String;
begin
  if (AFormat = CF_JNCCDATA) and (ASourceData.Header.ItemCount > 0) and
     (CompareText(ASourceData.Header.TableName, TN_OCCURRENCE) = 0) then
    if ASourceData.Items[0].KeyField1 = Key then
      MessageDlg(ResStr_OccurrenceCircularRef, mtInformation, [mbOk], 0)
    else begin
      AHandled := True;
      lItemName := VarToStr(dmGeneral.GetStoredProcOutputParam(
                            'usp_PreferredDeterminationName_Get_ForOccurrence',
                            ['@OccurrenceKey', ASourceData.Items[0].KeyField1], '@ItemName'));
      if lItemName <> '' then begin
        // If dropped on grid, enabled bottom part to get a proper Relationship Type.
        if Sender = sgRelatedOccs then begin
          EditDetails(True);
          cmbRelationshipTypes.SetFocus;
        end;
        // Still set the value in edit box.
        eRelatedOcc.Key  := ASourceData.Items[0].KeyField1;
        eRelatedOcc.Text := lItemName;
      end else
        MessageDlg(ResStr_InvalidOccurrence, mtInformation, [mbOk], 0);
    end;
end;  // TfraRelatedOccurrences.DropRelatedOcc 

{-------------------------------------------------------------------------------
}
procedure TfraRelatedOccurrences.DropRelatedOccOnEdit(const Sender: TObject; const AFormat: 
    Integer; const ASourceData: TKeyList; const ATextStrings: TstringList; var AHandled: 
    Boolean);
begin
  if eRelatedOcc.EditMode = emBrowse then
    AHandled := True
  else
    DropRelatedOcc(eRelatedOcc, AFormat, ASourceData, ATextStrings, AHandled);
end;  // TfraRelatedOccurrences.DropRelatedOccOnEdit 

{-------------------------------------------------------------------------------
}
procedure TfraRelatedOccurrences.DropRelatedOccOnGrid(const Sender: TObject; const AFormat: 
    Integer; const ASourceData: TKeyList; const ATextStrings: TstringList; var AHandled: 
    Boolean);
begin
  if EditMode = emBrowse then
    AHandled := True
  else
    DropRelatedOcc(sgRelatedOccs, AFormat, ASourceData, ATextStrings, AHandled);
end;  // TfraRelatedOccurrences.DropRelatedOccOnGrid 

{-------------------------------------------------------------------------------
}
procedure TfraRelatedOccurrences.EditDetails(ANewItem: Boolean);
begin
  EnableControls(False);
  sgRelatedOccs.Enabled := False;  // Disable grid, so user can't mess things up.
  EnableContainedControls(gbRelOccDetails, True);
  SetRequiredFieldsColourState(True, [eRelatedOcc, cmbRelationshipTypes]);
  FNewItem := ANewItem;
  // Make sure all is clear for new item.
  if FNewItem then ClearDetails;
  eRelatedOcc.SetFocus;
end;  // TfraRelatedOccurrences.EditDetails 

{-------------------------------------------------------------------------------
}
procedure TfraRelatedOccurrences.EnableControls(AEnabled: Boolean);
begin
  inherited;
  // Object will be nil if grid empty.
  btnEdit.Enabled := AEnabled and Assigned(sgRelatedOccs.Objects[0, 1]);
  btnDelete.Enabled := btnEdit.Enabled;
  sgRelatedOccs.Enabled := True;
  EnableContainedControls(gbRelOccDetails, False);
  SetRequiredFieldsColourState(False, [eRelatedOcc, cmbRelationshipTypes]);
  RefreshDetails;
end;  // TfraRelatedOccurrences.EnableControls 

{-------------------------------------------------------------------------------
}
procedure TfraRelatedOccurrences.eRelatedOccFindData(Sender: TObject);
begin
  inherited;
  DoCheck(eRelatedOcc, stOccurrence);
end;  // TfraRelatedOccurrences.eRelatedOccFindData 

{-------------------------------------------------------------------------------
}
procedure TfraRelatedOccurrences.LoadData;
begin
  inherited;
  FRelatedOccs.MasterKey := Key;
  FRelatedOccs.Refresh;
  sgRelatedOccsClick(nil);
end;  // TfraRelatedOccurrences.LoadData 

{-------------------------------------------------------------------------------
}
procedure TfraRelatedOccurrences.RefreshDetails;
begin
  // If an item is selected, update the fields in details.
  if Assigned(sgRelatedOccs.Objects[0, sgRelatedOccs.Row]) then
    with TRelatedOccItem(sgRelatedOccs.Objects[0, sgRelatedOccs.Row]) do begin
      eRelatedOcc.Text := RelatedOccName;
      eRelatedOcc.Key  := RelatedOccKey;
      mmComments.Text  := Comments;
  
      if not cmbRelationshipTypes.Populated then begin
        // If not populated, always clear previous single item.
        cmbRelationshipTypes.Clear;
        // Then add selected item's values.
        cmbRelationshipTypes.Add(RelationTypeName, RelationTypeKey);
        cmbRelationshipTypes.ItemIndex := 0;
      end else
        cmbRelationshipTypes.ItemIndex := cmbRelationshipTypes.IDIndexOf(RelationTypeKey);
    end
  else
    ClearDetails;
end;  // TfraRelatedOccurrences.RefreshDetails 

{-------------------------------------------------------------------------------
}
procedure TfraRelatedOccurrences.RegisterDragDropComponents;
begin
  inherited;
  
  RegisterDropComponent(sgRelatedOccs, DropRelatedOccOnGrid,
                        [TN_OCCURRENCE], [CF_JNCCDATA]);
  RegisterDropComponent(eRelatedOcc.EditBox, DropRelatedOccOnEdit,
                        [TN_OCCURRENCE], [CF_JNCCDATA]);
end;  // TfraRelatedOccurrences.RegisterDragDropComponents 

{-------------------------------------------------------------------------------
}
procedure TfraRelatedOccurrences.SaveData;
begin
  inherited;
  
  FRelatedOccs.Update;
end;  // TfraRelatedOccurrences.SaveData 

{-------------------------------------------------------------------------------
}
procedure TfraRelatedOccurrences.SetKey(Value: TKeyString);
begin
  inherited;
  
  FRelatedOccs.MasterKey := Key;
end;  // TfraRelatedOccurrences.SetKey 

{-------------------------------------------------------------------------------
}
procedure TfraRelatedOccurrences.sgRelatedOccsClick(Sender: TObject);
begin
  inherited;
  RefreshDetails;
end;  // TfraRelatedOccurrences.sgRelatedOccsClick 

{-------------------------------------------------------------------------------
}
procedure TfraRelatedOccurrences.sgRelatedOccsKeyDown(Sender: TObject; var Key: Word; Shift: 
    TShiftState);
begin
  inherited;
  if Key = VK_INSERT then begin
    EditDetails(True);
    Key := 0;
  end;
end;  // TfraRelatedOccurrences.sgRelatedOccsKeyDown 

{-==============================================================================
    TRelatedOccItem
===============================================================================}
{-------------------------------------------------------------------------------
}
procedure TRelatedOccItem.GetData(const Column: Integer; var AText: String; var AKey: 
    TKeyString);
begin
  inherited;
  
  case Column of
    0: AText := RelatedOccName;
    1: AText := RelationTypeName;
  end;
end;  // TRelatedOccItem.GetData 

{-------------------------------------------------------------------------------
}
procedure TRelatedOccItem.InitFromRecord(AFields: Fields);
begin
  SetItemKey(AFields['Item_Key'].Value);
  FRelatedOccKey    := AFields['Related_Occurrence_Key'].Value;
  FRelatedOccName   := AFields['Related_Occurrence_Name'].Value;
  FRelationTypeKey  := AFields['Thesaurus_Relation_Type_Key'].Value;
  FRelationTypeName := AFields['Thesaurus_Relation_Type_Name'].Value;
  FComments         := VarToStr(AFields['Comment'].Value);
end;  // TRelatedOccItem.InitFromRecord 

{-------------------------------------------------------------------------------
}
procedure TRelatedOccItem.SetComments(const Value: String);
begin
  if Value <> FComments then begin
    FComments := Value;
    SetModified;
  end;
end;  // TRelatedOccItem.SetComments 

{-------------------------------------------------------------------------------
}
procedure TRelatedOccItem.SetRelatedOccKey(const Value: String);
begin
  if Value <> FRelatedOccKey then begin
    FRelatedOccKey := Value;
    SetModified;
  end;
end;  // TRelatedOccItem.SetRelatedOccKey 

{-------------------------------------------------------------------------------
}
procedure TRelatedOccItem.SetRelatedOccName(const Value: String);
begin
  if Value <> FRelatedOccName then begin
    FRelatedOccName := Value;
    SetModified;
  end;
end;  // TRelatedOccItem.SetRelatedOccName 

{-------------------------------------------------------------------------------
}
procedure TRelatedOccItem.SetRelationTypeKey(const Value: String);
begin
  if Value <> FRelationTypeKey then begin
    FRelationTypeKey := Value;
    SetModified;
  end;
end;  // TRelatedOccItem.SetRelationTypeKey 

{-------------------------------------------------------------------------------
}
procedure TRelatedOccItem.SetRelationTypeName(const Value: String);
begin
  if Value <> FRelationTypeName then begin
    FRelationTypeName := Value;
    SetModified;
  end;
end;  // TRelatedOccItem.SetRelationTypeName 

{-==============================================================================
    TRelatedOccList
===============================================================================}
{-------------------------------------------------------------------------------
}
procedure TRelatedOccList.DoAddition(AItem: TLuxCachedDataItem);
begin
  with TRelatedOccItem(AItem) do
    dmGeneral.RunInsertStoredProc(TN_OCCURRENCE_RELATION,
                                  'usp_OccurrenceRelation_Insert',
                                  ['@FromOccurrenceKey', MasterKey,
                                   '@ToOccurrenceKey', RelatedOccKey,
                                   '@RelationTypeKey', RelationTypeKey,
                                   '@Comment', Comments],
                                   '@Key');
end;  // TRelatedOccList.DoAddition 

{-------------------------------------------------------------------------------
}
procedure TRelatedOccList.DoDeletion(AItem: TLuxCachedDataItem);
begin
  with TRelatedOccItem(AItem) do
    dmGeneral.RunDeleteStoredProc('usp_OccurrenceRelation_Delete',
                                  ['@Key', ItemKey, '@Timestamp', Timestamp]);
end;  // TRelatedOccList.DoDeletion 

{-------------------------------------------------------------------------------
}
procedure TRelatedOccList.DoModification(AItem: TLuxCachedDataItem);
begin
  with TRelatedOccItem(AItem) do
    dmGeneral.RunUpdateStoredProc('usp_OccurrenceRelation_Update',
                                  ['@Key', ItemKey,
                                   '@ToOccurrenceKey', RelatedOccKey,
                                   '@RelationTypeKey', RelationTypeKey,
                                   '@Comment', Comments,
                                   '@Timestamp', Timestamp]);
end;  // TRelatedOccList.DoModification 

{-------------------------------------------------------------------------------
}
function TRelatedOccList.GetRecordset: _Recordset;
begin
  Result := dmGeneral.GetRecordset('usp_OccurrenceRelations_Select_ForOccurrence',
                                   ['@Key', MasterKey]);
end;  // TRelatedOccList.GetRecordset 

end.




