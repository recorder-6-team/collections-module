{===============================================================================
  Unit:        ConceptGroupGeneral

  Defines:     TfraConceptGroupGeneral

  Description:

  Created:

  Last revision information:
    $Revision: 16 $
    $Date: 30/08/11 15:16 $
    $Author: Jamesbichard $

===============================================================================}

unit ConceptGroupGeneral;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, BaseTabSheetFrameUnit, ComboListID,
  LuxIDComboBox, DataTypes, GeneralData, DataClasses, EasyShell, ADOInt,
  GeneralFunctions, ResourceStrings, ConceptGroupQualityCheckHistory,
  ConfirmHistoryUpdate, ApplicationSettings, PublishedTermRuleSelector,
  TermGenerationFunctions, LuxembourgConstants;

type
  {-----------------------------------------------------------------------------
    General tab page allowing the details of a concept group to be viewed and
    edited.
  }
  TfraConceptGroupGeneral = class(TBaseTabSheetFrame)
    btnUrlGo: TButton;
    cmbParentChildRelationship: TLuxIDComboBox;
    eAuthority: TEdit;
    eName: TEdit;
    eUrl: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    lblLastChecked: TLabel;
    btnUpdate: TButton;
    btnHistory: TButton;
    lblLastCheckedBy: TLabel;
    fraPublishedTermRuleSelector: TfraPublishedTermRuleSelector;
    procedure btnUrlGoClick(Sender: TObject);
    procedure cmbParentChildRelationshipPopulate(Sender: TObject);
    procedure eUrlChange(Sender: TObject);
    procedure btnHistoryClick(Sender: TObject);
    procedure btnUpdateClick(Sender: TObject);
  private
    FChildKey: TKeyString;
    FParentChildRelationshipKey: TKeyString;
    FTimestamp: TSQLSvrTimestamp;
    FHasHistory: Boolean;
    FTermGeneratorKey: string;
    FName: string;
    FAuthority: string;
    procedure EnableGoButton;
  protected
    procedure DeleteData; override;
    procedure EnableControls(AEnabled: Boolean); overload; override;
    function GetCaption: string; override;
    procedure LoadData; override;
    procedure RegisterControls; override;
    procedure SaveData; override;
    procedure ValidateData; override;
  public
    procedure ChooseParentChildRelationship;
    property ChildKey: TKeyString read FChildKey write FChildKey;
  end;
  
//==============================================================================
implementation

uses
  ThesaurusApplicationSettings, BaseDetailFrameUnit;

{$R *.dfm}

//==============================================================================
{-==============================================================================
    TfraConceptGroupGeneral
===============================================================================}
{-------------------------------------------------------------------------------
  When clicked, shells to the user's default internet browser and displays the
          Url specified in eUrl.
}
procedure TfraConceptGroupGeneral.btnUrlGoClick(Sender: TObject);
begin
  inherited;
  ShellFile(eUrl.Text);
end;  // TfraConceptGroupGeneral.btnUrlGoClick 

{-------------------------------------------------------------------------------
}
procedure TfraConceptGroupGeneral.ChooseParentChildRelationship;
var
  RelationTypeKey: Variant;
  Index: Integer;
begin
  FParentChildRelationshipKey := '';
  
  RelationTypeKey := dmGeneral.GetStoredProcOutputParam(
                          'usp_LocalDomain_GetDefaultHierarchyRelationType',
                          ['@local_domain_key', ParentKey],
                          '@hierarchy_relation_type_key');
  
  if VarIsNull(RelationTypeKey) then
    cmbParentChildRelationship.ItemIndex := 0
  else
  begin
    if not cmbParentChildRelationship.Populated then
        cmbParentChildRelationshipPopulate(Self);
  
    Index := cmbParentChildRelationship.IDIndexOf(VarToStr(RelationTypeKey));
    if Index = -1 then Index := 0;
    cmbParentChildRelationship.ItemIndex := Index;
  end;
end;  // TfraConceptGroupGeneral.ChooseParentChildRelationship 

{-------------------------------------------------------------------------------
  Populates the cmbParentChildRelationship combo box with records from the
          Thesaurus_Relation_Type table.
  
  Lists all thesaurus relations where
          Semantic_Relationship.Forward_Equivalence_Definite is 1 and
          Semantic_Relationship.Reverse_Equivalence_Possible is 1, but
          Semantic_Relationship.Reverse_Equivalence_Definite is 0.  These are
          relationships where the right side of the relationship is a
          specialisation of the left side.
}
procedure TfraConceptGroupGeneral.cmbParentChildRelationshipPopulate(Sender:
        TObject);
begin
  inherited;
  
  with dmGeneral.GetRecordset('usp_ThesaurusRelationTypes_Select_WithEquivalenceBits',
                                  ['@ForwardEquivalencePossible', 1,
                                   '@ForwardEquivalenceDefinite', 1,
                                   '@ReverseEquivalencePossible', 1,
                                   '@ReverseEquivalenceDefinite', 0
                                  ]) do
    while not EOF do begin
      cmbParentChildRelationship.Add(
          VarToStr(Fields['Item_Name'].Value),
          VarToStr(Fields['Thesaurus_Relation_Type_Key'].Value));
      MoveNext;
    end;
end;  // TfraConceptGroupGeneral.cmbParentChildRelationshipPopulate 

{-------------------------------------------------------------------------------
}
procedure TfraConceptGroupGeneral.DeleteData;
begin
  dmGeneral.RunDeleteStoredProc('usp_ConceptGroup_Delete',
           ['@Key', Key,
            '@Timestamp', FTimestamp,
            '@SyncTaxonDict', ThesApplicationSettings.SyncTaxonDictDeletions]);
  with ThesApplicationSettings do
    if LogDeletions then LogDeletion('usp_ConceptGroup_Delete',
           ['@Key', Key,
            '@Timestamp', FTimestamp,
            '@SyncTaxonDict', ThesApplicationSettings.SyncTaxonDictDeletions]);
end;  // TfraConceptGroupGeneral.DeleteData 

{-------------------------------------------------------------------------------
  Calls a method to decide whether btnUrlGo should be enabled or not. 
}
procedure TfraConceptGroupGeneral.EnableControls(AEnabled: Boolean);
begin
  inherited;
  EnableGoButton;
  btnUpdate.Enabled := (Length(Key) > 0);
  btnHistory.Enabled := (Length(Key) > 0) and FHasHistory;
  fraPublishedTermRuleSelector.cmbPublishedTermRule.ReadOnly := not AEnabled;
end;  // TfraConceptGroupGeneral.EnableControls

{-------------------------------------------------------------------------------
  Decide whether btnUrlGo should be enabled or not. It is used by
          EnableControls and eUrlChange.
}
procedure TfraConceptGroupGeneral.EnableGoButton;
begin
  if eUrl.Text = '' then btnUrlGo.Enabled := False
                    else btnUrlGo.Enabled := True;

end;  // TfraConceptGroupGeneral.EnableGoButton 

{-------------------------------------------------------------------------------
  Handler for eUrl changing. Calls a method to decide whether btnUrlGo should
          be enabled or not.
}
procedure TfraConceptGroupGeneral.eUrlChange(Sender: TObject);
begin
  inherited;
  EnableGoButton;
end;  // TfraConceptGroupGeneral.eUrlChange 

{-------------------------------------------------------------------------------
  Returns the caption for the tree view. 
}
function TfraConceptGroupGeneral.GetCaption: string;
begin
  Result := eName.Text;
end;  // TfraConceptGroupGeneral.GetCaption 

{-------------------------------------------------------------------------------
  Store the timestamp and the ParentChildRelationshipKey. 
}
procedure TfraConceptGroupGeneral.LoadData;
begin
  inherited;
  if not RegisteredRecordsets[0].Eof then begin
    FTimestamp := RegisteredRecordsets[0].Fields['Timestamp'].Value;
    FParentChildRelationshipKey := VarToStr(RegisteredRecordsets[0].Fields
                                      ['Hierarchy_Relation_Type_Key'].Value);
    FHasHistory := RegisteredRecordsets[0].Fields['HasHistory'].Value;
    FTermGeneratorKey := VarToStr(RegisteredRecordsets[0].Fields['Term_Generator_Key'].Value);
    FName := VarToStr(RegisteredRecordsets[0].Fields['Item_Name'].Value);
    FAuthority := VarToStr(RegisteredRecordsets[0].Fields['Authority'].Value);
  end
  else
    ChooseParentChildRelationship;

end;  // TfraConceptGroupGeneral.LoadData

{-------------------------------------------------------------------------------
  Register the recordset and the controls.
}
procedure TfraConceptGroupGeneral.RegisterControls;
begin
  inherited;
  // Register recordsets used
  RegisterRecordset('usp_ConceptGroup_Select');
  // Register controls getting their value straight from the registered recordsets.
  RegisterControl(eName, 'Item_Name', True, ResStr_Name);
  RegisterControl(eAuthority, 'Authority');
  RegisterControl(eUrl, 'URL');
  RegisterControl(cmbParentChildRelationship, 'Hierarchy_Relation_Type_Name',
                                              'Hierarchy_Relation_Type_Key');
  RegisterAsyncControl(
      lblLastCheckedBy,
      'usp_ConceptGroup_Select_RecentHistory',
      ['@Key', 'Concept_Group_Key'],
      '@LastCheckedDetails');
  RegisterControl(fraPublishedTermRuleSelector.cmbPublishedTermRule,
      'Term_Generator_Name',
      'Term_Generator_Key');
  fraPublishedTermRuleSelector.SetDefaultItem;
end;  // TfraConceptGroupGeneral.RegisterControls

{-------------------------------------------------------------------------------
  Save the data.
}
procedure TfraConceptGroupGeneral.SaveData;
var
  lParams: Array of Variant;
  lUpdateDescendents: Boolean;
begin
  lUpdateDescendents := false;
  if (eName.Text <> FName) or
      (eAuthority.Text <> FAuthority) or
      (fraPublishedTermRuleSelector.Key <> FTermGeneratorKey) then
  begin
    if MessageDlg(
        Format(ResStr_UpdateDescendentConcepts, ['concept group']),
        mtConfirmation,
        [mbYes, mbNo], 0) = mrYes then
     lUpdateDescendents := true;
  end;
  lParams := VarArrayOf(['@Key', Key,
                          '@Url', eUrl.Text,
                          '@Authority', eAuthority.Text,
                          '@ConceptGroupName', eName.Text,
                          '@HierarchyRelationTypeKey', cmbParentChildRelationship.CurrentStrID,
                          '@TermGeneratorKey', fraPublishedTermRuleSelector.Key,
                          '@LocalDomainKey', ParentKey,
                          '@UpdateDescendents', lUpdateDescendents,
                          '@Timestamp', FTimestamp,
                          '@SystemSuppliedData', 0
                        ]);
  // Empty key means new record.
  if Key = '' then
  begin
    Key := VarToStr(dmGeneral.RunInsertStoredProc('Concept_Group',
                                                  'usp_ConceptGroup_Insert',
                                                  lParams,
                                                  '@Key'));
  end
  else
    dmGeneral.RunUpdateStoredProc('usp_ConceptGroup_Update', lParams);

  //N.B. Published and search terms for concepts within this group are
  //updated through use of a trigger.

end;  // TfraConceptGroupGeneral.SaveData 

{-------------------------------------------------------------------------------
  Validates that the user is allowing the concept hierarchy to be rebuilt if
          the hierarchy type has changed.
}
procedure TfraConceptGroupGeneral.ValidateData;
var
  lRecordset: _Recordset;
begin
  inherited ValidateData;
  
  if (FParentChildRelationshipKey <> cmbParentChildRelationship.CurrentStrID) and
     (FParentChildRelationshipKey <> '') then
  begin
    lRecordset := dmGeneral.GetRecordset('usp_Concept_Select_ForConceptGroup',
                                            ['@ConceptGroupKey', Key]);
    if lRecordset.RecordCount <> 0 then
      if ConfirmYesNo(ResStr_ConfirmConceptGroupHierarchyRebuild) = mrYes then
        // Empty Concept_Lineage table and rebuild for all concepts in the concept group.
        dmGeneral.RunStoredProc('usp_ConceptLineage_EmptyAndRebuild', ['@Key',Key])
      else
        Abort;
    lRecordset.Close;
  end;
end;  // TfraConceptGroupGeneral.ValidateData


{-------------------------------------------------------------------------------
}
procedure TfraConceptGroupGeneral.btnHistoryClick(Sender: TObject);
begin
  with TdlgConceptGroupQualityCheckHistory.Create(nil) do
    try
      LoadHistory(Key);
      ShowModal;
    finally
      Free;
    end;
end;

{-------------------------------------------------------------------------------
}
procedure TfraConceptGroupGeneral.btnUpdateClick(Sender: TObject);
begin
  inherited;
  with TdlgConfirmHistoryUpdate.Create(nil) do
    try
      SetKeys(Key, AppSettings.UserID);
      SetMessage(ResStr_MarkAsChecked);
      if ShowModal = mrYes then FHasHistory := true;
    finally
      Free;
    end;
  UpdateAsyncControls;
  if FHasHistory then btnHistory.Enabled := true;
end;

end.



