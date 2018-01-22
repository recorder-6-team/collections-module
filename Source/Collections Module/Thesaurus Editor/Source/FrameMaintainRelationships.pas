{===============================================================================
  Unit:        FrameMaintainRelationships.pas

  Defines:     TfraMaintainRelationships

  Description: Frame displaying Maintain Relationships screen. Allows saving of
               data.

  Model:       ThesaurusEditor.mpb

  Created:     December 2003

  Last revision information:
    $Revision: 6 $
    $Date: 15/12/06 10:59 $
    $Author: Ericsalmon $

===============================================================================}

unit FrameMaintainRelationships;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, FrameBaseMaintainRelationships, StdCtrls, CheckLst, ComboListID,
  LuxIDComboBox, DataTypes, ResourceStrings, LuxembourgConstants, ExceptionForm,
  DSSDataTypes;

type
  {-----------------------------------------------------------------------------
    Frame allowing a Thesaurus relation type to be edited.  Each relationship 
    type describes the relationship between any number of concepts, meanings 
    and term versions within the thesaurus.  Relationship types also describe 
    the relationships between occurrences and collection units.
    The dialog operates in 2 modes - browse (default) and edit.
  }
  TfraMaintainRelationships = class (TfraBaseMaintainRelationships)
    chklbAvailability: TCheckListBox;
    cmbSemanticRelationship: TLuxIDComboBox;
    eForwardTerm: TEdit;
    eReverseTerm: TEdit;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    procedure cmbSemanticRelationshipChange(Sender: TObject);
    procedure cmbSemanticRelationshipPopulate(Sender: TObject);
  private
    FTimestamp: TSQLSvrTimestamp;
    FUnidirectionalList: TStringList;
    procedure PopulateCheckBoxes;
  protected
    procedure EnableControls(AEnabled: Boolean); override;
    function GetCaption: string; override;
    function GetDeleteStoredProcName: string; override;
    procedure LoadData; override;
    procedure RegisterControls; override;
    procedure SaveData; override;
    procedure ValidateData; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;
  

implementation

uses GeneralData;

{$R *.dfm}

{-==============================================================================
    TfraMaintainRelationships
===============================================================================}
{-------------------------------------------------------------------------------
  Create the stringlist. 
}
constructor TfraMaintainRelationships.Create(AOwner: TComponent);
begin
  inherited;
  FUnidirectionalList := TStringList.Create;
end;  // TfraMaintainRelationships.Create 

{-------------------------------------------------------------------------------
  Destroy the stringlist. 
}
destructor TfraMaintainRelationships.Destroy;
begin
  FUnidirectionalList.Free;
  inherited;
end;  // TfraMaintainRelationships.Destroy 

{-------------------------------------------------------------------------------
  Unidirectional relationships don't need a reverse term, whereas bidirectional 
          ones do. This method makes the ReverseTerm field disabled/enabled. 
}
procedure TfraMaintainRelationships.cmbSemanticRelationshipChange(Sender: 
        TObject);
begin
  inherited;
  if FUnidirectionalList.Values[cmbSemanticRelationship.CurrentStrID] = 'True' then begin
    eReverseTerm.Enabled := False;
    eReverseTerm.Text := '';
    eReverseTerm.Color := clWindow;
  end else begin
    eReverseTerm.Enabled := True;
    if EditMode = emEdit then
      eReverseTerm.Color := clYellow;
  end;
end;  // TfraMaintainRelationships.cmbSemanticRelationshipChange 

{-------------------------------------------------------------------------------
  Populate the combo box with semantic relations. 
}
procedure TfraMaintainRelationships.cmbSemanticRelationshipPopulate(Sender: 
        TObject);
begin
  inherited;
  
  with dmGeneral.GetRecordset('usp_SemanticRelations_Select',[]) do begin
    while not EOF do begin
      cmbSemanticRelationship.Add(VarToStr(Fields['Item_Name'].Value),
                  VarToStr(Fields['Key'].Value));
      FUnidirectionalList.Add(VarToStr(Fields['Key'].Value) +
                           '=' + VarToStr(Fields['Unidirectional'].Value));
      MoveNext;
    end; // while
    Close;
  end; // with
end;  // TfraMaintainRelationships.cmbSemanticRelationshipPopulate 

{-------------------------------------------------------------------------------
  Enable the controls. 
}
procedure TfraMaintainRelationships.EnableControls(AEnabled: Boolean);
begin
  inherited;
  // The checklist box isn't registered so needs to be enabled manually here.
  chklbAvailability.Enabled := AEnabled;
  SetRequiredFieldsColourState(False, [eReverseTerm]);
  cmbSemanticRelationshipChange(Self);
end;  // TfraMaintainRelationships.EnableControls 

{-------------------------------------------------------------------------------
  Return the caption. 
}
function TfraMaintainRelationships.GetCaption: string;
begin
  Result := eRelationshipName.Text;
end;  // TfraMaintainRelationships.GetCaption 

{-------------------------------------------------------------------------------
  Retrieves the name of the delete stored proc. 
}
function TfraMaintainRelationships.GetDeleteStoredProcName: string;
begin
  Result := 'usp_ThesaurusRelationType_Delete';
end;  // TfraMaintainRelationships.GetDeleteStoredProcName 

{-------------------------------------------------------------------------------
  Load the timestamp, populate the checkboxes and populate the combo box. 
}
procedure TfraMaintainRelationships.LoadData;
begin
  inherited;
  if not RegisteredRecordsets[0].EOF then begin
    FTimestamp := RegisteredRecordsets[0].Fields['Timestamp'].Value;
    PopulateCheckBoxes;
    // Need to populate the combo box at this point because we need to know if the
    // relationship is uni/bidirectional.
    cmbSemanticRelationship.PopulateContent;
  end;
end;  // TfraMaintainRelationships.LoadData 

{-------------------------------------------------------------------------------
  Populates the check boxes with the information from the database. 
}
procedure TfraMaintainRelationships.PopulateCheckBoxes;
begin
  with RegisteredRecordsets[0] do begin
    if Fields['Concept'].Value <> NULL then
      chklbAvailability.Checked[0] := true
    else
      chklbAvailability.Checked[0] := false;
  
    if Fields['Meaning'].Value <> NULL then
      chklbAvailability.Checked[1] := true
    else
      chklbAvailability.Checked[1] := false;
  
    if Fields['Term_Version'].Value <> NULL then
      chklbAvailability.Checked[2] := true
    else
      chklbAvailability.Checked[2] := false;
  
    if Fields['Occurrence'].Value <> NULL then
      chklbAvailability.Checked[3] := true
    else
      chklbAvailability.Checked[3] := false;
  
    if Fields['Collection_Unit'].Value <> NULL then
      chklbAvailability.Checked[4] := true
    else
      chklbAvailability.Checked[4] := false;
  end;
end;  // TfraMaintainRelationships.PopulateCheckBoxes 

{-------------------------------------------------------------------------------
  Register the controls. 
}
procedure TfraMaintainRelationships.RegisterControls;
begin
  inherited;
  // Register recordsets used
  RegisterRecordset('usp_ThesaurusRelationTypes_Select_ForEditor');
  
  // Register controls getting their value straight from the registered recordsets.
  RegisterControl(eRelationshipName, 'Item_Name', True, ResStr_RelationshipName);
  RegisterControl(eForwardTerm, 'Forward_Term', True, ResStr_ForwardTerm);
  RegisterControl(eReverseTerm, 'Reverse_Term');
  RegisterControl(cmbSemanticRelationship, 'Semantic_Relation_Name',
              'Semantic_Relation_Key', True, ResStr_SemanticRelationship);
  
end;  // TfraMaintainRelationships.RegisterControls 

{-------------------------------------------------------------------------------
  Save the data to the database. 
}
procedure TfraMaintainRelationships.SaveData;
var
  lParams: Array of Variant;
begin
  lParams := VarArrayOf(['@Key', Key,
                          '@SemanticRelationKey', cmbSemanticRelationship.CurrentStrID,
                          '@ItemName', eRelationshipName.Text,
                          '@ForwardTerm', eForwardTerm.Text,
                          '@ReverseTerm', eReverseTerm.Text,
                          '@Timestamp', FTimestamp,
                          '@Concept', chklbAvailability.Checked[0],
                          '@Meaning', chklbAvailability.Checked[1],
                          '@TermVersion', chklbAvailability.Checked[2],
                          '@Occurrence', chklbAvailability.Checked[3],
                          '@CollectionUnit', chklbAvailability.Checked[4]
                        ]);
  // Empty key means new record.
  if Key = '' then
    Key := VarToStr(dmGeneral.RunInsertStoredProc(TN_THESAURUS_RELATION_TYPE,
                                                  'usp_ThesaurusRelationType_Insert',
                                                  lParams,
                                                  '@Key'))
  else
    dmGeneral.RunUpdateStoredProc('usp_ThesaurusRelationType_Update', lParams);
end;  // TfraMaintainRelationships.SaveData 

{-------------------------------------------------------------------------------
  If we are dealing with a bidirectional relationship, the Reverse Term control 
          needs to be validated. 
}
procedure TfraMaintainRelationships.ValidateData;
begin
  inherited;
  if FUnidirectionalList.Values[cmbSemanticRelationship.CurrentStrID] <> 'True' then
    ValidateValue(eReverseTerm.Text <> '', Format(ResStr_MissingData, [ResStr_ReverseTerm]));
end;  // TfraMaintainRelationships.ValidateData 


end.



