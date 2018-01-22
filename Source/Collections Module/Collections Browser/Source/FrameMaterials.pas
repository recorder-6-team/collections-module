{===============================================================================
  Unit:        FrameMaterials.pas

  Defines:     TfraMaterials

  Description:

  Model:       CollectionsCommonAndGeneral.mpb

  Created:     June 2003

  Last revision information:
    $Revision: 26 $
    $Date: 21/12/07 10:08 $
    $Author: Ericsalmon $

===============================================================================}
unit FrameMaterials;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, BaseTabSheetFrameUnit, ExtCtrls, StdCtrls, ImageListButton, Grids,
  InterfaceDataModule, BaseCompositeComponent, LinkedControls, DataTypes,
  DataClasses, LuxembourgDataClasses, ADODB, ADOInt, DropStruct, DropTarget, DSSDataTypes,
  ComboListID, LuxIDComboBox, ConceptGroupComboBox, DssStringGrid, BaseDragFrameUnit,
  Recorder2000_TLB;

type
  TfraMaterials = class;

  TMaterialItem = class (TLuxGridDataItem)
  private
    FMaterialIsNew: Boolean;
    FMaterialKey: TKeyString;
    FMaterialName: String;
    FQuantity: String;
    FUnitKey: TKeyString;
    FUnitName: String;
    FValidated: Boolean;
  protected
    procedure GetData(const Column: Integer; var AText: String; var AKey: TKeyString); 
        override;
    procedure InitFromRecord(AFields: Fields); override;
    procedure SetData(const Column: Integer; const AText: String; const AKey: TKeyString); 
        override;
    procedure SetModified; override;
    procedure ValidateData; override;
  public
    property MaterialIsNew: Boolean read FMaterialIsNew;
    property MaterialKey: TKeyString read FMaterialKey;
    property MaterialName: String read FMaterialName;
    property Quantity: String read FQuantity;
    property UnitKey: TKeyString read FUnitKey;
    property UnitName: String read FUnitName;
  end;
  
  TMaterialList = class (TLuxGridDataList)
  private
    FFrame: TfraMaterials;
  protected
    procedure DoAddition(AItem: TLuxCachedDataItem); override;
    procedure DoDeletion(AItem: TLuxCachedDataItem); override;
    procedure DoModification(AItem: TLuxCachedDataItem); override;
    function GetRecordset: _Recordset; override;
  public
    property Frame: TfraMaterials read FFrame write FFrame;
  end;
  
  {-----------------------------------------------------------------------------
    Tab page for the materials required for a conservation job.  Also forms the base form from 
    which the Materials tab for collection units is inherited from.
    Materials are stored as concepts in the thesaurus.  This tab page maintains a list of 
    materials used plus the amount and unit for each material.
  }
  TfraMaterials = class (TBaseTabSheetFrame)
    btnAdd: TImageListButton;
    btnRemove: TImageListButton;
    cmbUnit: TConceptGroupComboBox;
    eMaterial: TLinkedEdit;
    sgMaterials: TDSSStringGrid;
    shpList: TShape;
    procedure btnAddClick(Sender: TObject);
    procedure btnRemoveClick(Sender: TObject);
    procedure eMaterialFindData(Sender: TObject);
    procedure eMaterialGetData(Sender: TObject);
    procedure sgMaterialsKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure sgMaterialsGetEditText(Sender: TObject; ACol, ARow: Integer;
      var Value: String);
  private
    FDeleteAllStoredProcName: String;
    FMaterialList: TMaterialList;
    FParentKeyParamName: String;
    FTableName: String;
    FTableNameCompact: String;
    function CanAddRow: Boolean;
    function CheckConceptIsAMaterial(AConceptKey: TKeyString): Boolean;
    procedure DropMaterial(const Sender: TObject; const AFormat: Integer; const ASourceData:
        TKeyList; const ATextStrings: TStringList; var AHandled: Boolean);
    procedure eMaterialReturnKeyHandler(const AKeyList: IKeyList);
  protected
    procedure DeleteData; override;
    procedure EnableControls(AEnabled: Boolean); override;
    procedure LoadData; override;
    procedure RegisterControls; override;
    procedure RegisterDragDropComponents; override;
    procedure SaveData; override;
    procedure SetKey(Value: TKeyString); override;
    procedure SetMasterFrameType(Value: TMasterFrameType); override;
    procedure ValidateData; override;
    property ParentKeyParamName: String read FParentKeyParamName;
    property TableName: String read FTableName;
    property TableNameCompact: String read FTableNameCompact;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;
  
//==============================================================================
implementation

{$R *.dfm}

uses
  GeneralData, Validation, LuxembourgConstants, SearchManager, ResourceStrings,
  ExceptionForm, ThesaurusBrowser_TLB;

{-==============================================================================
    TMaterialItem
===============================================================================}
{-------------------------------------------------------------------------------
}
procedure TMaterialItem.GetData(const Column: Integer; var AText: String; var AKey: 
    TKeyString);
begin
  AKey := '';
  AText := '';
  case Column of
    0: begin
         AText := MaterialName;
         AKey := MaterialKey;
       end;
    1: AText := Quantity;
    2: begin
         AText := UnitName;
         AKey := UnitKey;
       end;
  end;
end;  // TMaterialItem.GetData 

{-------------------------------------------------------------------------------
}
procedure TMaterialItem.InitFromRecord(AFields: Fields);
begin
  SetItemKey(VarToStr(AFields['Item_Key'].Value));
  FMaterialKey := AFields['Material_Concept_Key'].Value;
  FMaterialName := AFields['Material_Item_Name'].Value;
  // These fields can be null
  FQuantity := VarToStr(AFields['Quantity'].Value);
  FUnitKey := VarToStr(AFields['Unit_Concept_Key'].Value);
  FUnitName := VarToStr(AFields['Unit_Item_Name'].Value);
end;  // TMaterialItem.InitFromRecord 

{-------------------------------------------------------------------------------
}
procedure TMaterialItem.SetData(const Column: Integer; const AText: String; const AKey: 
    TKeyString);
begin
  case Column of
    0: if (FMaterialKey <> AKey) or ((AKey = '') and (FMaterialName <> AText)) then
       begin
         FMaterialName := AText;
         FMaterialKey := AKey;
         SetModified;
       end;
    1: if FQuantity <> AText then begin
         FQuantity := AText;
         SetModified;
       end;
    2: if FUnitKey <> AKey then begin
         FUnitName := AText;
         FUnitKey := AKey;
         SetModified;
       end;
  end;
end;  // TMaterialItem.SetData 

{-------------------------------------------------------------------------------
  If the data is modified, FValidated needs to be set to False. 
}
procedure TMaterialItem.SetModified;
begin
  inherited;
  FValidated := False;
end;  // TMaterialItem.SetModified 

{-------------------------------------------------------------------------------
}
procedure TMaterialItem.ValidateData;
var
  lKey, lText: String;
begin
  // We don't want it to Validate twice or it will ask the user if they want to add a new
  // material twice. Hence, FValidated is a local variable that is set once validation
  // has successfully occurred.
  if not FValidated then begin
    // No material? Problem.
    ValidateValue(FMaterialName <> '', Format(ResStr_MissingData, [ResStr_Material]));

    // Material name but no key? Ask if it's a new one
    if (FMaterialKey = '') and (FMaterialName <> '') then begin
      lText := FMaterialName;
      if DoCheckUnique(lKey, lText, stTermInConceptGroup, CG_MATERIALS, ResStr_Material) then
      begin
        // Found a unique exact match. Update fields accordingly.
        FMaterialKey := lKey;
        FMaterialName := lText;
      end else begin
        FMaterialIsNew := False;
        case MessageDlg(Format(ResStr_AddNewMaterial, [FMaterialName]),
                        mtWarning, mbYesNoCancel, 0) of
          mrYes:
              // Set to true and create record when saving in list.
              // Handle in DoAddition and DoModification.
              FMaterialIsNew := True;
  
          mrNo:
              begin
                // Not a new one, so ask for existing to be selected.
                ValidateValue(DoCheck(lKey, lText, stTermInConceptGroup,
                                      CG_MATERIALS, ResStr_Material),
                              Format(ResStr_MissingData, [ResStr_Material]));
                // ValidateValue passed. User chose existing Material.
                FMaterialKey := lKey;
                FMaterialName := lText;
              end;
  
          mrCancel:
              // User chose to give up.
              Abort;
        end;
      end;
    end;
    FValidated := True;
  end;
end;  // TMaterialItem.ValidateData 

{-==============================================================================
    TMaterialList
===============================================================================}
{-------------------------------------------------------------------------------
}
procedure TMaterialList.DoAddition(AItem: TLuxCachedDataItem);
var
  lMaterialKey: TKeyString;
begin
  with TMaterialItem(AItem) do begin
    if MaterialIsNew then begin
      lMaterialKey := dmGeneral.RunInsertStoredProc(TN_CONCEPT, 'usp_Concept_Insert',
                                                    ['@ConceptGroupKey', CG_MATERIALS,
                                                     '@TermName', MaterialName],
                                                    '@Key');
    end else
      lMaterialKey := FMaterialKey;
  
    dmGeneral.RunInsertStoredProc(Frame.TableName,
                                  'usp_' + Frame.TableNameCompact + '_Insert',
                                  [Frame.ParentKeyParamName, MasterKey,
                                   '@MaterialKey', lMaterialKey,
                                   '@Quantity', Quantity,
                                   '@UnitKey', UnitKey], '@Key');
  end;
end;  // TMaterialList.DoAddition 

{-------------------------------------------------------------------------------
}
procedure TMaterialList.DoDeletion(AItem: TLuxCachedDataItem);
begin
  with TMaterialItem(AItem) do
    dmGeneral.RunDeleteStoredProc('usp_' + Frame.TableNameCompact + '_Delete',
                                  ['@Key', ItemKey, '@Timestamp', Timestamp]);
end;  // TMaterialList.DoDeletion 

{-------------------------------------------------------------------------------
}
procedure TMaterialList.DoModification(AItem: TLuxCachedDataItem);
var
  lMaterialKey: TKeyString;
begin
  with TMaterialItem(AItem) do begin
    if MaterialIsNew then begin
      lMaterialKey := dmGeneral.RunInsertStoredProc(TN_CONCEPT, 'usp_Concept_Insert',
                                                    ['@ConceptGroupKey', CG_MATERIALS,
                                                     '@TermName', MaterialName],
                                                    '@Key');
    end else
      lMaterialKey := FMaterialKey;
  
    dmGeneral.RunUpdateStoredProc('usp_' + Frame.TableNameCompact + '_Update',
                                  ['@Key', ItemKey,
                                   '@MaterialKey', lMaterialKey,
                                   '@Quantity', Quantity,
                                   '@UnitKey', UnitKey,
                                   '@Timestamp', Timestamp]);
  end;
end;  // TMaterialList.DoModification 

{-------------------------------------------------------------------------------
}
function TMaterialList.GetRecordset: _Recordset;
begin
  Result := dmGeneral.GetRecordset('usp_' + Frame.TableNameCompact + '_Select',
                                   ['@Key', MasterKey]);
end;  // TMaterialList.GetRecordset 

{-==============================================================================
    TfraMaterials
===============================================================================}
{-------------------------------------------------------------------------------
}
constructor TfraMaterials.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  
  FMaterialList := TMaterialList.Create(TMaterialItem, sgMaterials);
  FMaterialList.Frame := Self;

  sgMaterials.Rows[0].CommaText := ResStr_Material + ',' +
                                   ResStr_Quantity + ',' +
                                   ResStr_Unit;
  // Setting the control sets the type to custom.
  sgMaterials.ColumnsInfo[0].WinControl := eMaterial;
  sgMaterials.ColumnsInfo[2].WinControl := cmbUnit;
  cmbUnit.NoSelectionItemText := ResStr_NoSelection;
end;  // TfraMaterials.Create 

{-------------------------------------------------------------------------------
}
destructor TfraMaterials.Destroy;
begin
  FMaterialList.Free;
  
  inherited Destroy;
end;  // TfraMaterials.Destroy 

{-------------------------------------------------------------------------------
}
procedure TfraMaterials.btnAddClick(Sender: TObject);
begin
  inherited;
  with sgMaterials do begin
    if CanAddRow then begin
      FMaterialList.AddNew(TMaterialItem.CreateNew(FMaterialList));
      Col := 0;
    end;
    Row := RowCount - 1;
    SetFocus;
  end;
end;  // TfraMaterials.btnAddClick

{-------------------------------------------------------------------------------
}
procedure TfraMaterials.btnRemoveClick(Sender: TObject);
begin
  inherited;
  with sgMaterials do begin
    // Only ask if something in cells, or if it's an item coming from DB
    if (Cells[0, Row] <> '') or (Cells[1, Row] <> '') or (Cells[2, Row] <> '') or
       (Assigned(Objects[0, Row]) and (TMaterialItem(Objects[0, Row]).ItemKey <> '')) then
    begin
      if MessageDlg(Format(ResStr_ConfirmRowDelete, [ResStr_Row]),
                    mtWarning, [mbYes, mbNo], 0) = mrYes then
        FMaterialList.DeleteItem(Row);
    end else
      FMaterialList.DeleteItem(Row);
  end; //with
end;  // TfraMaterials.btnRemoveClick 

{-------------------------------------------------------------------------------
  Checks if the last row of the grid is empty, and allow for more rows to be added if not. 
}
function TfraMaterials.CanAddRow: Boolean;
begin
  with sgMaterials do
    Result := (Cells[0, RowCount-1] <> '') or (Cells[1, RowCount-1] <> '') or (Cells[2, RowCount-1] <> '');
end;  // TfraMaterials.CanAddRow

{-------------------------------------------------------------------------------
}
function TfraMaterials.CheckConceptIsAMaterial(AConceptKey: TKeyString): Boolean;
begin
  Result := dmGeneral.GetStoredProcOutputParam('usp_ConceptGroupKey_Get_ForConcept',
            ['@Key', AConceptKey], '@ConceptGroupKey') = CG_MATERIALS;
end;  // TfraMaterials.CheckConceptIsAMaterial 

{-------------------------------------------------------------------------------
}
procedure TfraMaterials.DeleteData;
begin
  dmGeneral.RunDeleteStoredProc(FDeleteAllStoredProcName, ['@Key', Key]);
end;  // TfraMaterials.DeleteData 

{-------------------------------------------------------------------------------
}
procedure TfraMaterials.DropMaterial(const Sender: TObject; const AFormat: Integer; const 
    ASourceData: TKeyList; const ATextStrings: TStringList; var AHandled: Boolean);
var
  lClickPoint: TPoint;
  lRow, lCol: Integer;
  lCounter: Integer;
  lMaterialItem: TMaterialItem;
  lMaterialKey: TKeyString;
  lMaterialName: String;
begin
  lClickPoint := sgMaterials.ScreenToClient(Mouse.CursorPos);
  sgMaterials.MouseToCell(lClickPoint.X, lClickPoint.Y, lCol, lRow);
  
  if EditMode = emBrowse then
    AHandled := True
  else
  if AFormat = CF_JNCCDATA then begin
    AHandled := True;
    for lCounter := 0 to (ASourceData.Header.ItemCount - 1) do begin
      lMaterialKey := ASourceData.Items[lCounter].KeyField1;
      lMaterialName := ConvertConceptKeyToCaption(ASourceData.Items[lCounter].KeyField1,
                                                  TN_CONCEPT);
      if CheckConceptIsAMaterial(lMaterialKey) then begin
        if lRow = -1 then begin
          lMaterialItem := TMaterialItem.CreateNew(FMaterialList);
          lMaterialItem.SetData(0, lMaterialName, lMaterialKey);
          FMaterialList.AddNew(lMaterialItem);
        end else begin
          eMaterial.Key := lMaterialKey;
          eMaterial.Text := lMaterialName;
        end;
      end;
    end;
  end;
end;  // TfraMaterials.DropMaterial 

{-------------------------------------------------------------------------------
}
procedure TfraMaterials.eMaterialFindData(Sender: TObject);
begin
  inherited;
  DoCheck(eMaterial, stTermInConceptGroup, CG_MATERIALS);
end;  // TfraMaterials.eMaterialFindData 

{-------------------------------------------------------------------------------
}
procedure TfraMaterials.eMaterialGetData(Sender: TObject);
var
  lIntf: IUnknown;
  lThesaurus: IfrmThesaurusBrowser;
begin
  inherited;
  lIntf := InitReturnData(
      eMaterialReturnKeyHandler, GUIDToString(CLASS_frmThesaurusBrowser));
  // Ask the thesaurus to display the materials concept group
  if Assigned(lIntf) then begin
    if Supports(lIntf, IID_IfrmThesaurusBrowser, lThesaurus) then
      lThesaurus.DisplayConceptGroup(CG_MATERIALS);
  end;
end;  // TfraMaterials.eMaterialGetData 

{-------------------------------------------------------------------------------
}
procedure TfraMaterials.eMaterialReturnKeyHandler(const AKeyList: IKeyList);
var
  lIsMAterial: Boolean;
  lTerm: String;
begin
  with AKeyList do
    if (CompareText(TableName, TN_CONCEPT) = 0) and (ItemCount > 0) then begin
      lIsMaterial := CheckConceptIsAMaterial(GetKeyItem(0).KeyField1);
      lTerm := ConvertConceptKeyToCaption(GetKeyItem(0).KeyField1, TN_CONCEPT);
  
      if lIsMaterial then
      begin
        eMaterial.Key := GetKeyItem(0).KeyField1;
        eMaterial.Text := lTerm;
      end else
        MessageDlg(Format(ResStr_InvalidMaterial, [lTerm]), mtInformation, [mbOk], 0);
    end;
end;  // TfraMaterials.eMaterialReturnKeyHandler 

{-------------------------------------------------------------------------------
}
procedure TfraMaterials.EnableControls(AEnabled: Boolean);
begin
  inherited;
  sgMaterials.ReadOnly := not AEnabled;
  // Need to add/remove the property each time we change edit mode, or it will
  // leave the contents of the most recently editted cell on the grid if
  // you click on a different node.
  if sgMaterials.ReadOnly then
    sgMaterials.Options := sgMaterials.Options - [goAlwaysShowEditor]
  else
    sgMaterials.Options := sgMaterials.Options + [goAlwaysShowEditor];
end;  // TfraMaterials.EnableControls

{-------------------------------------------------------------------------------
}
procedure TfraMaterials.LoadData;
begin
  inherited;
  eMaterial.Text := '';
  cmbUnit.ItemIndex := -1;
  FMaterialList.MasterKey := Key;
  FMaterialList.Refresh;
end;  // TfraMaterials.LoadData 

{-------------------------------------------------------------------------------
}
procedure TfraMaterials.RegisterControls;
begin
  inherited;
  RegisterConceptGroupComboBox(cmbUnit, CG_QUANTITY_UNITS);
end;  // TfraMaterials.RegisterControls 

{-------------------------------------------------------------------------------
}
procedure TfraMaterials.RegisterDragDropComponents;
begin
  RegisterDropComponent(sgMaterials, DropMaterial, [TN_CONCEPT], [CF_JNCCDATA]);
  RegisterDropComponent(eMaterial, DropMaterial, [TN_CONCEPT], [CF_JNCCDATA]);
end;  // TfraMaterials.RegisterDragDropComponents 

{-------------------------------------------------------------------------------
  Save modified data to the database. 
}
procedure TfraMaterials.SaveData;
begin
  FMaterialList.Update;
end;  // TfraMaterials.SaveData 

{-------------------------------------------------------------------------------
}
procedure TfraMaterials.SetKey(Value: TKeyString);
begin
  inherited SetKey(Value);
  
  FMaterialList.MasterKey := Key;
end;  // TfraMaterials.SetKey 

{-------------------------------------------------------------------------------
}
procedure TfraMaterials.SetMasterFrameType(Value: TMasterFrameType);
begin
  inherited;
  case MasterFrameType of
    mftCollectionUnit:
        begin
          FTableName := TN_COLLECTION_UNIT_MATERIAL;
          FTableNameCompact := StringReplace(TN_COLLECTION_UNIT_MATERIAL, '_', '',
                                             [rfReplaceAll]);
          FParentKeyParamName := '@CollectionUnitKey';
          FDeleteAllStoredProcName := 'usp_CollectionUnitMaterials_Delete';
        end;
    mftJob:
        begin
          FTableName := TN_CONSERVATION_JOB_MATERIAL;
          FTableNameCompact := StringReplace(TN_CONSERVATION_JOB_MATERIAL, '_', '',
                                             [rfReplaceAll]);
          FParentKeyParamName := '@ConservationJobKey';
          FDeleteAllStoredProcName := 'usp_ConservationJobMaterials_Delete';
        end;
  end;
end;  // TfraMaterials.SetMasterFrameType 

{-------------------------------------------------------------------------------
}
procedure TfraMaterials.sgMaterialsGetEditText(Sender: TObject; ACol, ARow: Integer;
  var Value: String);
begin
  inherited;
  // Enforce maximum length for Quantity in InplaceEditor.
  with TCustomGridAccessor(sgMaterials) do
    if Assigned(InplaceEditor) then
      if SendMessage(InplaceEditor.Handle, EM_GETLIMITTEXT, 0, 0) <> 20 then
        SendMessage(InplaceEditor.Handle, EM_LIMITTEXT, 20, 0);
end;

{-------------------------------------------------------------------------------
}
procedure TfraMaterials.sgMaterialsKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  inherited;
  with sgMaterials do
    if (Key = VK_DOWN) then
      if (Row = RowCount-1) then begin
        Key := 0;
        if CanAddRow then begin
          FMaterialList.AddNew(TMaterialItem.CreateNew(FMaterialList));
          Row := RowCount - 1;
          Col := 0;
          SetFocus;
        end;
      end;
end;  // TfraMaterials.sgMaterialsKeyDown

{-------------------------------------------------------------------------------
  All tab pages are validated before anything is actually saved. The list used by the string 
      grid needs to be told to validate. 
}
procedure TfraMaterials.ValidateData;
begin
  inherited;
  FMaterialList.ValidateContent;
end;  // TfraMaterials.ValidateData

end.
