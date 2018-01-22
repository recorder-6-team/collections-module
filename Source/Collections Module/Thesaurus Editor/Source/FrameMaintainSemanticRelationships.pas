{===============================================================================
  Unit:        FrameMaintainSemanticRelationships.pas

  Defines:     TfraMaintainSemanticRelationships

  Description: Frame displaying Maintain Semantic Relationships screen. Allows
               saving of data.

  Model:       ThesaurusEditor.mpb

  Created:     December 2003

  Last revision information:
    $Revision: 4 $
    $Date: 10/02/04 15:40 $
    $Author: Anthonysimpson $

===============================================================================}

unit FrameMaintainSemanticRelationships;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, FrameBaseMaintainRelationships, ImgList, StdCtrls, ComCtrls,
  DataTypes, ExceptionForm, ResourceStrings, LuxembourgConstants;

type
  {-----------------------------------------------------------------------------
    Frame allowing a semantic relation's details to be viewed and edited.  Each 
    semantic relation describes the underlying logic for any number of 
    Thesaurus_Relation_Type records.
    The frame operates in 2 modes - browse (default) and edit.
  }
  TfraMaintainSemanticRelationships = class (TfraBaseMaintainRelationships)
    chkUnidirectional: TCheckBox;
    cmbChronology: TComboBox;
    cmbEquivalence: TComboBoxEx;
    ilRelations: TImageList;
    Label1: TLabel;
    Label3: TLabel;
    Label5: TLabel;
    mmDescription: TMemo;
  private
    FTimestamp: TSQLSvrTimestamp;
    function ComboFlagsToID(AFEP, AFED, AREP, ARED, AAdjacent: Boolean): 
            Integer;
    procedure ComboIDToFlags(AComboBoxID: Integer; var AFEP, AFED, AREP, ARED, 
            AAdjacent: Boolean);
  protected
    procedure EnableControls(AEnabled: Boolean); override;
    function GetCaption: string; override;
    function GetDeleteStoredProcName: string; override;
    procedure LoadData; override;
    procedure RegisterControls; override;
    procedure SaveData; override;
    procedure ValidateData; override;
  end;
  

implementation

{$R *.dfm}

uses GeneralData;

{-==============================================================================
    TfraMaintainSemanticRelationships
===============================================================================}
{-------------------------------------------------------------------------------
  Function to convert flags to a number for the combo box to use.
  FEP means Forward_Equivalence_Possible,
  FED means Forward_Equivalence_Definite,
  REP means Reverse_Equivalence_Possible,
  RED means Reverse_Equivalence_Definite
  
  The items in the combo box are numbered as follows:
  0) Items are not related
  1) Items are adjacent 
  2) Items are overlapping 
  3) First item contains second item 
  4) Second item contains first item 
  5) Items are equal  
}
function TfraMaintainSemanticRelationships.ComboFlagsToID(AFEP, AFED, AREP, 
        ARED, AAdjacent: Boolean): Integer;
begin
  if (AFEP = True) and (AREP = True) then begin // If FEP, REP true, could be 2,3,4,5
    if ARED = True then begin // Could be 4 or 5
      if AFED = True then
        Result := 5
      else
        Result := 4
    end
    else begin // Could be 2 or 3
      if AFED = True then
        Result := 3
      else
        Result := 2
    end
  end
  else if AAdjacent = True // If FEP, FED, REP and RED false, could be 0 or 1
    then Result := 1
  else Result := 0; // Must be 0
end;  // TfraMaintainSemanticRelationships.ComboFlagsToID 

{-------------------------------------------------------------------------------
  Function that converts a combo box ID into flags that can be passed into the 
          Semantic_Relation table. 
}
procedure TfraMaintainSemanticRelationships.ComboIDToFlags(AComboBoxID: Integer;
        var AFEP, AFED, AREP, ARED, AAdjacent: Boolean);
begin
  case AComboBoxID of
   1 : AAdjacent := True;
   2 : begin
        AFEP := True;
        AREP := True;
        end;
   3 : begin
        AFED := True;
        AFEP := True;
        AREP := True;
        end;
   4 : begin
        AFEP := True;
        ARED := True;
        AREP := True;
        end;
   5 : begin
        AFED := True;
        AFEP := True;
        AREP := True;
        ARED := True;
        end;
  end;
end;  // TfraMaintainSemanticRelationships.ComboIDToFlags 

{-------------------------------------------------------------------------------
}
procedure TfraMaintainSemanticRelationships.EnableControls(AEnabled: Boolean);
begin
  inherited;
  cmbEquivalence.Enabled := AEnabled;
  SetRequiredFieldsColourState(AEnabled, [cmbEquivalence]);
end;  // TfraMaintainSemanticRelationships.EnableControls

{-------------------------------------------------------------------------------
}
function TfraMaintainSemanticRelationships.GetCaption: string;
begin
  Result := eRelationshipName.Text;
end;  // TfraMaintainSemanticRelationships.GetCaption

{-------------------------------------------------------------------------------
  Gets the name of the stored proc to delete Semantic_Relation records from the 
          database.
}
function TfraMaintainSemanticRelationships.GetDeleteStoredProcName: string;
begin
  Result := 'usp_SemanticRelation_Delete';
end;  // TfraMaintainSemanticRelationships.GetDeleteStoredProcName 

{-------------------------------------------------------------------------------
  Loads the timestamp and the correct item for the combo box.  
}
procedure TfraMaintainSemanticRelationships.LoadData;
begin
  inherited;
  
  if not RegisteredRecordsets[0].Eof then begin
    with RegisteredRecordsets[0] do begin
      FTimestamp := Fields['Timestamp'].Value;
      cmbEquivalence.ItemIndex :=
          ComboFlagsToID(Fields['Forward_Equivalence_Possible'].Value,
                          Fields['Forward_Equivalence_Definite'].Value,
                          Fields['Reverse_Equivalence_Possible'].Value,
                          Fields['Reverse_Equivalence_Definite'].Value,
                          Fields['Adjacent'].Value);
      if Fields['Chronological_Overlap'].Value <> Null then
        cmbChronology.ItemIndex := Fields['Chronological_Overlap'].Value
      else
        cmbChronology.ItemIndex := 0;
    end;
  end else
    cmbChronology.ItemIndex := 0;
end;  // TfraMaintainSemanticRelationships.LoadData 

{-------------------------------------------------------------------------------
}
procedure TfraMaintainSemanticRelationships.RegisterControls;
begin
  // Register recordsets used
  RegisterRecordset('usp_SemanticRelation_Select');
  
  // Register controls getting their value straight from the registered recordsets.
  RegisterControl(eRelationshipName, 'Item_Name', True, ResStr_RelationshipName);
  RegisterControl(chkUnidirectional, 'Unidirectional');
  RegisterControl(mmDescription, 'Description');
end;  // TfraMaintainSemanticRelationships.RegisterControls 

{-------------------------------------------------------------------------------
  Saves the data to the database. 
}
procedure TfraMaintainSemanticRelationships.SaveData;
var
  lParams: Array of Variant;
  lFEP, lFED, lREP, lRED, Adjacent: Boolean;
begin
  ComboIDToFlags(cmbEquivalence.ItemIndex, lFEP, lFED, lREP, lRED, Adjacent);
  
  lParams := VarArrayOf(['@Key', Key,
                          '@ItemName', eRelationshipName.Text,
                          '@Unidirectional', chkUnidirectional.Checked,
                          '@ForwardEquivalencePossible', lFEP,
                          '@ForwardEquivalenceDefinite', lFED,
                          '@ReverseEquivalencePossible', lREP,
                          '@ReverseEquivalenceDefinite', lRED,
                          '@Adjacent', Adjacent,
                          '@ChronologicalOverlap', cmbChronology.ItemIndex,
                          '@Description', mmDescription.Text,
                          '@Timestamp', FTimestamp
                        ]);
  
  // Empty key means new record.
  if Key = '' then
    Key := VarToStr(dmGeneral.RunInsertStoredProc(TN_SEMANTIC_RELATION,
                                                  'usp_SemanticRelation_Insert',
                                                  lParams,
                                                  '@Key'))
  else
    dmGeneral.RunUpdateStoredProc('usp_SemanticRelation_Update', lParams);
end;  // TfraMaintainSemanticRelationships.SaveData 

{-------------------------------------------------------------------------------
}
procedure TfraMaintainSemanticRelationships.ValidateData;
begin
  inherited;
  ValidateValue(cmbEquivalence.ItemIndex >= 0, Format(ResStr_MissingData,
      [ResStr_Equivalence]), cmbEquivalence);
end;  // TfraMaintainSemanticRelationships.ValidateData 

end.




