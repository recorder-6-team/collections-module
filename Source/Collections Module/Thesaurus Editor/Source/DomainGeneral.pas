{===============================================================================
  Unit:         DomainGeneral

  Defines:      TfraDomainGeneral

  Description:  Details frame for a Domain

  Model:        ThesaurusEditor

  Last revision information:
    $Revision: 26 $
    $Date: 30/08/11 15:16 $
    $Author: Jamesbichard $

===============================================================================}

unit DomainGeneral;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, BaseTabSheetFrameUnit, RestrictedEdits,
  ComboListID, LuxIDComboBox, DataTypes, DSSDataTypes, GeneralData, ResourceStrings, Math,
  ADOInt, ExceptionForm, LuxembourgConstants, PublishedTermRuleSelector,
  TermGenerationFunctions;

type
  {-----------------------------------------------------------------------------
    General tab page allowing the details of a domain to be viewed and edited.
  }
  TfraDomainGeneral = class(TBaseTabSheetFrame)
    btnFetchSecurityBit: TButton;
    cbOccurrencesAllowed: TCheckBox;
    cmbRelationType: TLuxIDComboBox;
    eItemName: TEdit;
    eSecurityBit: TNumberEdit;
    Label1: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    fraPublishedTermRuleSelector: TfraPublishedTermRuleSelector;
    procedure btnFetchSecurityBitClick(Sender: TObject);
    procedure cbOccurrencesAllowedClick(Sender: TObject);
    procedure cmbRelationTypePopulate(Sender: TObject);
    procedure eSecurityBitKeyDown(Sender: TObject; var Key: Word; Shift:
            TShiftState);
  private
    FDomainMask: Variant;
    FSecurityBitLoaded: string;
    FSecurityBitReadOnly: Boolean;
    FTimestamp: TSQLSvrTimestamp;
    FTermGeneratorKey: string;
    FDomainName: string;
    procedure CalculateDomainMask;
    procedure SetSecurityBitReadOnlyCallback(ATarget: TObject; AValue: Variant);
  protected
    procedure DeleteData; override;
    procedure EnableControls(AEnabled: Boolean); override;
    function GetCaption: string; override;
    procedure LoadData; override;
    procedure RegisterControls; override;
    procedure SaveData; override;
    procedure ValidateData; override;
  end;
  
//==============================================================================
implementation

uses
  GeneralFunctions, BaseADODataModule, ThesaurusApplicationSettings;

{$R *.dfm}

//==============================================================================
{-==============================================================================
    TfraDomainGeneral
===============================================================================}
{-------------------------------------------------------------------------------
  Handler that retrieves the next available Security Bit value and puts it into
          the eSecurityBit field.
}
procedure TfraDomainGeneral.btnFetchSecurityBitClick(Sender: TObject);
var
  lSecurityBit: Integer;
begin
  inherited;
  lSecurityBit := dmGeneral.GetStoredProcOutputParam(
                                            'usp_DomainMask_NextAvailable_Get',
                                            [],
                                            '@SecurityBit');
  // If the proc returns a value of 33, we know that it was unable to find
  // a free Domain Mask.
  if lSecurityBit = 33 then
    ShowInformation(ResStr_AllSecurityBitsInUse)
  else
    eSecurityBit.Text := IntToStr(lSecurityBit);
end;  // TfraDomainGeneral.btnFetchSecurityBitClick 

{-------------------------------------------------------------------------------
  Calculates the domain mask from the value entered by the user into the
          security bit field.
}
procedure TfraDomainGeneral.CalculateDomainMask;
begin
  if eSecurityBit.Text <> '' then
    // The formula for this is:   x = 2^(n-1)
    FDomainMask := Longint(Round(IntPower(2, StrToInt(eSecurityBit.Text)-1)))
  else
    FDomainMask := NULL;
end;  // TfraDomainGeneral.CalculateDomainMask 

{-------------------------------------------------------------------------------
  Depending on the checked state of cbOccurrenceAllowed, the SecurityBit is
          mandatory. This method changes the colour of it appropriately. If it
          mandatory, validation will occur on it.
}
procedure TfraDomainGeneral.cbOccurrencesAllowedClick(Sender: TObject);
begin
  inherited;
  SetRequiredFieldsColourState(
                      (EditMode = emEdit) and (cbOccurrencesAllowed.Checked),
                      [eSecurityBit]);
end;  // TfraDomainGeneral.cbOccurrencesAllowedClick 

{-------------------------------------------------------------------------------
  Populates cmbRelationType combo box with records from the
          Thesaurus_Relation_Type table.
}
procedure TfraDomainGeneral.cmbRelationTypePopulate(Sender: TObject);
begin
  inherited;
  with dmGeneral.GetRecordset('usp_ThesaurusRelationTypes_Select_WithEquivalenceBits',
                                  ['@ForwardEquivalencePossible', 1,
                                   '@ForwardEquivalenceDefinite', 1,
                                   '@ReverseEquivalencePossible', 1,
                                   '@ReverseEquivalenceDefinite', 0
                                  ]) do
    while not EOF do begin
      cmbRelationType.Add(
          VarToStr(Fields['Item_Name'].Value),
          VarToStr(Fields['Thesaurus_Relation_Type_Key'].Value));
      MoveNext;
    end;
end;  // TfraDomainGeneral.cmbRelationTypePopulate 

{-------------------------------------------------------------------------------
}
procedure TfraDomainGeneral.DeleteData;
begin
  dmGeneral.RunDeleteStoredProc('usp_Domain_Delete',
                               ['@Key', Key, '@Timestamp', FTimestamp]);
  with ThesApplicationSettings do
    if LogDeletions then LogDeletion('usp_Domain_Delete',
                               ['@Key', Key, '@Timestamp', FTimestamp]);
end;  // TfraDomainGeneral.DeleteData 

{-------------------------------------------------------------------------------
  Toggle the colour of the required fields. 
}
procedure TfraDomainGeneral.EnableControls(AEnabled: Boolean);
begin
  inherited;
  SetRequiredFieldsColourState(
                      (EditMode = emEdit) and (cbOccurrencesAllowed.Checked),
                      [eSecurityBit]);
  // Refresh the check for existing concepts, in case it has changed since loading
  if AEnabled then
    FSecurityBitReadOnly := dmGeneral.GetStoredProcOutputParam('usp_ConceptCount_Get_ForDomain',
            ['@Domain', Key], '@ConceptCount')<>0;
  eSecurityBit.ReadOnly := FSecurityBitReadOnly and (not AEnabled);
  btnFetchSecurityBit.Enabled := (not FSecurityBitReadOnly) and AEnabled;
  fraPublishedTermRuleSelector.cmbPublishedTermRule.ReadOnly := not AEnabled;
end;  // TfraDomainGeneral.EnableControls 

{-------------------------------------------------------------------------------
  If the SecurityBit field is set to read only and the frame is in edit mode,
          then a message is displayed informing the user that they cannot edit
          this field.
}
procedure TfraDomainGeneral.eSecurityBitKeyDown(Sender: TObject; var Key: Word;
        Shift: TShiftState);
begin
  inherited;
  if FSecurityBitReadOnly and (EditMode = emEdit) then
    raise EBrowserFrameError.CreateNonCritical(ResStr_SecurityBitCannotBeEditted);
end;  // TfraDomainGeneral.eSecurityBitKeyDown 

{-------------------------------------------------------------------------------
  Returns the caption for the tree view. 
}
function TfraDomainGeneral.GetCaption: string;
begin
  Result := eItemName.Text;
end;  // TfraDomainGeneral.GetCaption 

{-------------------------------------------------------------------------------
  Save the Timestamp, and convert the domain mask back into a value for the
          'security bit'.
}
procedure TfraDomainGeneral.LoadData;
begin
  inherited;
  
  dmGeneral.CancelAsyncCommands(SetSecurityBitReadOnlyCallback);
  if not RegisteredRecordsets[0].EOF then begin
    FTimestamp := RegisteredRecordsets[0].Fields['Timestamp'].Value;
    FTermGeneratorKey := VarToStr(RegisteredRecordsets[0].Fields['Term_Generator_Key'].Value);
    FDomainName := VarToStr(RegisteredRecordsets[0].Fields['Item_Name'].Value);
    if (RegisteredRecordsets[0].Fields['Domain_Mask'].Value <> 0) and
       (RegisteredRecordsets[0].Fields['Domain_Mask'].Value <> Null) then
      // The formula for this is:   n = (log x / log 2) + 1
      eSecurityBit.Text := IntToStr(Round(
          Log10(Longword(RegisteredRecordsets[0].Fields['Domain_Mask'].Value))
          / Log10(2.0) + 1));
      FSecurityBitLoaded := eSecurityBit.Text
  end;

  // The value in this control is read only if there are any concepts in the domain.
  dmGeneral.GetAsyncData('usp_ConceptCount_Get_ForDomain',
            ['@Domain', Key], '@ConceptCount', nil, SetSecurityBitReadOnlyCallback);
end;  // TfraDomainGeneral.LoadData 

{-------------------------------------------------------------------------------
  Register the recordset and the controls. 
}
procedure TfraDomainGeneral.RegisterControls;
begin
  inherited;
  // Register recordsets used
  RegisterRecordset('usp_Domain_Select');
  
  // Register controls getting their value straight from the registered recordsets.
  RegisterControl(eItemName, 'Item_Name', True, ResStr_DomainName);
  RegisterControl(cmbRelationType, 'Default_Hierarchy_Relation_Type_Name',
                                   'Default_Hierarchy_Relation_Type_Key');
  RegisterControl(
      fraPublishedTermRuleSelector.cmbPublishedTermRule,
      'Term_Generator_Name',
      'Term_Generator_Key');
  RegisterControl(cbOccurrencesAllowed, 'Has_Occurrences');
  fraPublishedTermRuleSelector.SetDefaultItem('<default>');
end;  // TfraDomainGeneral.RegisterControls

{-------------------------------------------------------------------------------
  Convert the 'security bit' and convert it into the domain mask. Then saves
          the inserted or updated data.
}
procedure TfraDomainGeneral.SaveData;
var
  lParams: Array of Variant;
  lUpdateDescendents: Boolean;
begin
  lUpdateDescendents := false;
  if (eItemName.Text <> FDomainName) or
      (fraPublishedTermRuleSelector.Key <> FTermGeneratorKey) then
  begin
    if MessageDlg(
        Format(ResStr_UpdateDescendentConcepts, ['domain']),
        mtConfirmation,
        [mbYes, mbNo], 0) = mrYes then
     lUpdateDescendents := true;
  end;

  lParams := VarArrayOf(['@Key', Key,
                          '@ItemName', eItemName.Text,
                          '@SubjectAreaKey', ParentKey,
                          '@HasOccurrences', cbOccurrencesAllowed.Checked,
                          '@DefaultHierarchyRelationTypeKey', cmbRelationType.CurrentStrID,
                          '@DomainMask', FDomainMask,
                          '@TermGeneratorKey', fraPublishedTermRuleSelector.Key,
                          '@UpdateDescendents', lUpdateDescendents,
                          '@SystemSuppliedData', 0,
                          '@Timestamp', FTimestamp
                        ]);

  // Empty key means new record.
  if Key = '' then
    Key := VarToStr(dmGeneral.RunInsertStoredProc(TN_DOMAIN,
                                                    'usp_Domain_Insert',
                                                    lParams,
                                                    '@Key'))
  else
    dmGeneral.RunUpdateStoredProc('usp_Domain_Update', lParams);

  //N.B. Published and search terms for concepts within this domain are
  //updated through use of a trigger.

  
end;  // TfraDomainGeneral.SaveData

{-------------------------------------------------------------------------------
}
procedure TfraDomainGeneral.SetSecurityBitReadOnlyCallback(ATarget: TObject;
        AValue: Variant);
begin
  if AValue <> 0 then
    FSecurityBitReadOnly := True
  else
    FSecurityBitReadOnly := False;
end;  // TfraDomainGeneral.SetSecurityBitReadOnlyCallback

{-------------------------------------------------------------------------------
  Checks the value of the 'security bit' field is between 1 and 32. 
}
procedure TfraDomainGeneral.ValidateData;
var
  lDomainMaskExists: Boolean;
begin
  inherited;
  if cbOccurrencesAllowed.Checked then
    ValidateValue(eSecurityBit.Text <> '', Format(ResStr_MissingData, [ResStr_SecurityBit]));
  if eSecurityBit.Text <> '' then begin
    ValidateValue(IsInt(eSecurityBit.Text), Format(ResStr_MustBeInteger,
                                                        [ResStr_SecurityBit]));
    ValidateValue((StrToInt(eSecurityBit.Text) > 0) and (StrToInt(eSecurityBit.Text) <= 32),
                                Format(ResStr_AskForPositiveInteger, [1, 32]));
    CalculateDomainMask;
    lDomainMaskExists := (dmGeneral.GetStoredProcOutputParam(
                                                  'usp_DomainMask_Exists',
                                                  ['@DomainMask', FDomainMask],
                                                  '@MatchExists') = True);
    if lDomainMaskExists and (FSecurityBitLoaded <> eSecurityBit.Text) then
      MessageDlg(ResStr_SecurityBitInUse, mtInformation, [mbOK], 0);
  end else
    FDomainMask := NULL;
end;  // TfraDomainGeneral.ValidateData

end.



