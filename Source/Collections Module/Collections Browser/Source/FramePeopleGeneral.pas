{===============================================================================
  Unit:        FramePeopleGeneral.pas

  Defines:     TfraPeopleGeneral

  Description:

  Model:       CollectionsCommonAndGeneral.mpb

  Created:     May 2003

  Last revision information:
    $Revision: 16 $
    $Date: 16/10/12 11:27 $
    $Author: Alexanderpadley $

===============================================================================}

unit FramePeopleGeneral;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, BaseTabSheetFrameUnit, StdCtrls, ImageListButton, ExtCtrls,
  VagueDateEdit, ComboListID, LuxIDComboBox, ConceptGroupComboBox,
  BaseCompositeComponent, LinkedControls, InterfaceDataModule, DataTypes,
  Validation, LuxembourgConstants, GeneralData, DataClasses, DropTarget,
  ResourceStrings, BaseFullScreenFrameUnit, UserEdit;

type
  {-----------------------------------------------------------------------------
    Tab page control displaying details for a name related to a collection unit through the 
    Collection_Unit_Name table.
  }
  TfraPeopleGeneral = class (TBaseFullScreenFrame)
    cmbType: TConceptGroupComboBox;
    eDate: TVagueDateEdit;
    eName: TUserEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label8: TLabel;
    mmComments: TMemo;
  private
    FCollectionUnitNameKey: TKeyString;
    FIsRelatedNameLeafNode: Boolean;
    FTimestamp: TSqlSvrTimestamp;
    procedure DropName(const Sender: TObject; const AFormat: Integer; const ASourceData: 
        TKeyList; const ATextStrings: TStringList; var AHandled: Boolean);
    procedure GetCollectionUnitNameKey;
    function GetParams: TVariantArray;
  protected
    procedure DeleteData; override;
    function GetCaption: String; override;
    procedure LoadData; override;
    procedure RegisterControls; override;
    procedure RegisterDragDropComponents; override;
    procedure SaveData; override;
  end;
  
//==============================================================================
implementation

{$R *.dfm}

uses 
  ComCtrls, BaseDetailFrameUnit, BaseADODataModule;

{-==============================================================================
    TfraPeopleGeneral
===============================================================================}
{-------------------------------------------------------------------------------
  Run the delete stored proc. for this frame's record. 
}
procedure TfraPeopleGeneral.DeleteData;
begin
  dmGeneral.RunDeleteStoredProc('usp_CollectionUnitName_Delete',
                                ['@Key', FCollectionUnitNameKey, '@Timestamp', FTimestamp]);
end;  // TfraPeopleGeneral.DeleteData 

{-------------------------------------------------------------------------------
  Handle dropping of NAME data onto eName. 
}
procedure TfraPeopleGeneral.DropName(const Sender: TObject; const AFormat: Integer; const 
    ASourceData: TKeyList; const ATextStrings: TStringList; var AHandled: Boolean);
begin
  DropLinkedEditDataMultiple(AFormat, ASourceData, AHandled, eName,
      [TN_INDIVIDUAL, 'usp_FormattedNameForNameKey_Get', '@NameKey', '@FormattedName',
       TN_ORGANISATION, 'usp_FormattedNameForNameKey_Get', '@NameKey', '@FormattedName']);
end;  // TfraPeopleGeneral.DropName 

{-------------------------------------------------------------------------------
  Get the caption. 
}
function TfraPeopleGeneral.GetCaption: String;
begin
  if cmbType.Text = '' then
    Result := eName.Text
  else
    Result := eName.Text + ' - ' + cmbType.Text;
end;  // TfraPeopleGeneral.GetCaption 

{-------------------------------------------------------------------------------
  This frame can be accessed by clicking on two types of nodes - TPeopleOrganisationLeafNodes 
      and TRelatedNameLeafNodes. The first stores the Name_Key as its main key. The latter 
      stores Collection_Unit_Name_Key. As this frame displays Collection_Unit_Name records, 
      the latter has the correct key. The former therefore has to get its key from the 
      database. 
}
procedure TfraPeopleGeneral.GetCollectionUnitNameKey;
begin
  if FIsRelatedNameLeafNode then
    FCollectionUnitNameKey := Key
  else
    FCollectionUnitNameKey := dmGeneral.GetStoredProcOutputParam(
                                'usp_CollectionUnitName_Key_Get',
                                ['@CollectionUnitKey', ParentKey, '@NameKey', Key],
                                '@CollectionUnitNameKey');
end;  // TfraPeopleGeneral.GetCollectionUnitNameKey 

{-------------------------------------------------------------------------------
  Because we cannot use Key to get the record from the database (because if the 
      TPeopleOrganisationLeafNode was the leaf node, it will have given the frame the wrong 
      key), we need to ensure the correct key is used. 
}
function TfraPeopleGeneral.GetParams: TVariantArray;
begin
  GetCollectionUnitNameKey;
  Result := VarArrayOf(['@Key', FCollectionUnitNameKey]);
end;  // TfraPeopleGeneral.GetParams 

{-------------------------------------------------------------------------------
  Load the timestamp. 
}
procedure TfraPeopleGeneral.LoadData;
begin
  inherited;
  if not RegisteredRecordsets[0].Eof then
    FTimestamp := RegisteredRecordsets[0].Fields['Timestamp'].Value;
end;  // TfraPeopleGeneral.LoadData 

{-------------------------------------------------------------------------------
  Register the recordset and the controls. 
}
procedure TfraPeopleGeneral.RegisterControls;
begin
  inherited;
  // Need to find this out before LoadData is called, so do it here.
  FIsRelatedNameLeafNode := AdditionalProperties.GetProperty(PROP_IS_RELATED_NAME_LEAF_NODE);
  // Register recordsets used
  RegisterRecordset('usp_CollectionUnitName_Select', GetParams);
  
  // Register controls getting their value straight from the registered recordsets.
  RegisterControl(eName, 'RelatedName', 'Name_Key', True, ResStr_Name,
                  CheckLinkedName, 'Name', ConvertNameKeyToCaption);
  RegisterControl(cmbType, 'TypeTerm', 'Relation_Type_Concept_Key', True, ResStr_Type);
  RegisterConceptGroupComboBox(cmbType, CG_COLLECTION_UNIT_TO_NAME_RELATIONSHIPS);
  RegisterControl(eDate, '');
  RegisterControl(mmComments, 'Comment');
end;  // TfraPeopleGeneral.RegisterControls 

{-------------------------------------------------------------------------------
  Register the drag and drop components. 
}
procedure TfraPeopleGeneral.RegisterDragDropComponents;
begin
  RegisterDropComponent(eName, DropName, [TN_NAME, TN_INDIVIDUAL, TN_ORGANISATION],
                        [CF_JNCCDATA]);
end;  // TfraPeopleGeneral.RegisterDragDropComponents 

{-------------------------------------------------------------------------------
  Save the data to the database. 
}
procedure TfraPeopleGeneral.SaveData;
var
  lParams: Array of Variant;
begin
  inherited;
  lParams := VarArrayOf(['@Key', FCollectionUnitNameKey,
                         '@CollectionUnitKey', ParentKey,
                         '@NameKey', eName.Key,
                         '@RelationTypeConceptKey', cmbType.CurrentStrID,
                         '@VagueDateStart', eDate.VagueDate.StartDate,
                         '@VagueDateEnd', eDate.VagueDate.EndDate,
                         '@VagueDateType', eDate.VagueDate.DateTypeString,
                         '@Comment', mmComments.Text,
                         '@Timestamp', FTimestamp
                         ]);
  // Empty key means new record.
  if FCollectionUnitNameKey = '' then
    Key := VarToStr(dmGeneral.RunInsertStoredProc(TN_COLLECTION_UNIT_NAME,
                                                  'usp_CollectionUnitName_Insert', lParams,
                                                  '@Key'))
  else
    dmGeneral.RunUpdateStoredProc('usp_CollectionUnitName_Update', lParams);
end;  // TfraPeopleGeneral.SaveData 

end.
