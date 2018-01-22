{===============================================================================
  Unit:        Validation

  Defines:     <nothing>

  Description: Functions to check the validity of data.

  Model:       <none>

  Created:     August 2003

  Last revision information:
    $Revision: 31 $
    $Date: 9/10/07 10:09 $
    $Author: Davidkelly $

===============================================================================}

unit Validation;

interface

uses
  SysUtils, LinkedControls, SearchManager, ConceptGroupComboBox, Variants,
  GeneralData, LuxembourgConstants, Dialogs, ResourceStrings;

type
  TCheckLinkedDataFunction = function(AControl: TLinkedEdit;
                                      const AAdditionalCaption: String= ''): Boolean;

  TConvertKeyToCaptionFunction = function(const AKey: String; const ATableName: String): String;

//------------------------------------------------------------------------------
function CheckLinkedIndividual(AControl: TLinkedEdit; const AAdditionalCaption: String= ''): Boolean;
function CheckLinkedOrganisation(AControl: TLinkedEdit; const AAdditionalCaption: String= ''): Boolean;
function CheckLinkedName(AControl: TLinkedEdit; const AAdditionalCaption: String= ''): Boolean;
function CheckLinkedDeptOrgName(AControl: TLinkedEdit; const AAdditionalCaption: String= ''): Boolean;
function CheckLinkedStore(AControl: TLinkedEdit; const AAdditionalCaption: String= ''): Boolean;
function CheckLinkedSurvey(AControl: TLinkedEdit; const AAdditionalCaption: String= ''): Boolean;
function CheckLinkedCollection(AControl: TLinkedEdit; const AAdditionalCaption: String= ''): Boolean;
function CheckLinkedReference(AControl: TLinkedEdit; const AAdditionalCaption: String= ''): Boolean;
function CheckLinkedDeterminationLifeScience(AControl: TLinkedEdit; const AAdditionalCaption: String= ''): Boolean;
function CheckLinkedDeterminationEarthScience(AControl: TLinkedEdit; const AAdditionalCaption: String= ''): Boolean;
function CheckLinkedLocation(AControl: TLinkedEdit; const AAdditionalCaption: String= ''): Boolean;
function CheckLinkedConcept(AControl: TLinkedEdit; const AAdditionalCaption: String= ''): Boolean;
function CheckLinkedTerm(AControl: TLinkedEdit; const AAdditionalCaption: String= ''): Boolean;
function CheckLinkedTermInConceptGroup(AControl: TLinkedEdit; AConceptGroupKey: String; const AAdditionalCaption: String= ''): Boolean;
function CheckConceptGroupParameter(AControl: TConceptGroupComboBox; const AAdditionalCaption:  String= ''; AConceptGroupKeys: String=''): Boolean;
function CheckConceptGroupProcess(AControl: TConceptGroupComboBox; const AAdditionalCaption:  String= ''; AConceptGroupKeys: String=''): Boolean;

// If there is no specific function, use these
function DoCheck(AControl: TLinkedEdit; ASearchType: TSearchType;
  const ASearchKey: String = ''; const AAdditionalCaption: String= ''): Boolean; overload;

function DoCheck(AControl: TConceptGroupComboBox; ASearchType: TSearchType;
  const ASearchKey: String = ''; const AAdditionalCaption: String= ''): Boolean; overload;

function DoCheck(var AKey, AText: String; ASearchType: TSearchType;
  const ASearchKey: String = ''; const AAdditionalCaption: String= ''): Boolean; overload;

function DoCheckUnique(var AKey, AText: String; ASearchType: TSearchType;
  const ASearchKey: String = ''; const AAdditionalCaption: String= ''): Boolean;

//------------------------------------------------------------------------------
function ConvertIndividualKeyToCaption(const AKey: String; const ATableName: String): String;
function ConvertOrganisationKeyToCaption(const AKey: String; const ATableName: String): String;
function ConvertNameKeyToCaption(const AKey: String; const ATableName: String): String;
function ConvertStoreKeyToCaption(const AKey: String; const ATableName: String): String;
function ConvertSurveyKeyToCaption(const AKey: String; const ATableName: String): String;
function ConvertCollectionKeyToCaption(const AKey: String; const ATableName: String): String;
function ConvertDeterminationLifeScienceKeyToCaption(const AKey: String; const ATableName: String): String;
function ConvertDeterminationEarthScienceKeyToCaption(const AKey: String; const ATableName: String): String;
function ConvertLocationKeyToCaption(const AKey: String; const ATableName: String): String;
function ConvertReferenceKeyToCaption(const AKey: String; const ATableName: String): String;
function ConvertSpecimenKeyToCaption(const AKey: String; const ATableName: String): String;
function ConvertConceptKeyToCaption(const AKey: String; const ATableName: String): String;
function ConvertTermKeyToCaption(const AKey: String; const ATableName: String): String;

function CheckIsIndividual(const AKey: String): Boolean;
function UpdateIndividualNameControl(AControl: TLinkedEdit; const AKey, AMsg: String): Boolean;

function ConceptHasMappedTaxon(const AConceptKey: String): Boolean;

//==============================================================================
implementation

//==============================================================================
{-------------------------------------------------------------------------------
  Call overloaded DoCheck to get Key/Text values if failed first checks.
}
function DoCheck(AControl: TConceptGroupComboBox; ASearchType: TSearchType;
  const ASearchKey: String = ''; const AAdditionalCaption: String= ''): Boolean;
var lText, lKey: String;
begin
  // Ok if Key already there. We can assume value is correct.
  if AControl.CurrentStrID <> '' then
    Result := True
  else
  // If no key, and no text, control is clear, so assume it's correct too.
  if AControl.Text = '' then
    Result := True
  else begin
    lText := AControl.Text;
    Result := DoCheck(lKey, lText, ASearchType, ASearchKey, AAdditionalCaption);
   // AControl.CurrentStrID := lKey;
    if Result then AControl.Text := lText;
  end;
end;

{-------------------------------------------------------------------------------
  Call overloaded DoCheck to get Key/Text values if failed first checks.
}
function DoCheck(AControl: TLinkedEdit; ASearchType: TSearchType;
  const ASearchKey: String = ''; const AAdditionalCaption: String= ''): Boolean;
var lText, lKey: String;
begin
  // Ok if Key already there. We can assume value is correct.
  if AControl.Key <> '' then
    Result := True
  else
  // If no key, and no text, control is clear, so assume it's correct too.
  if AControl.Text = '' then
    Result := True
  else begin
    lText := AControl.Text;
    Result := DoCheck(lKey, lText, ASearchType, ASearchKey, AAdditionalCaption);
    AControl.Key := lKey;
    if Result then AControl.Text := lText;
  end;
end;

{-------------------------------------------------------------------------------
  Overloaded to search and return Key/Text values independently from any kind
  of controls.
}
function DoCheck(var AKey, AText: String; ASearchType: TSearchType;
  const ASearchKey: String = ''; const AAdditionalCaption: String= ''): Boolean;
begin
  with TSearchManager.Create do
    try
      SearchType := ASearchType;
      SearchKey  := ASearchKey;
      AdditionalCaption := AAdditionalCaption;
      // Get the key, if there is one coming back...
      AKey := RunSearch(AText);
      // Success?
      Result := AKey <> '';
      // If so, update Text too.
      if Result then AText := ResultText;
    finally
      Free;
    end;
end;

{-------------------------------------------------------------------------------
  Search for a unique item and return a Key/Text values independently from any kind
  of controls.
}
function DoCheckUnique(var AKey, AText: String; ASearchType: TSearchType;
  const ASearchKey: String = ''; const AAdditionalCaption: String= ''): Boolean;
begin
  with TSearchManager.Create do
    try
      SearchType := ASearchType;
      SearchKey  := ASearchKey;
      AdditionalCaption := AAdditionalCaption;
      // Get the key, if there is one coming back...
      AKey := FindUnique(AText);
      // Success?
      Result := AKey <> '';
      // If so, update Text too.
      if Result then AText := ResultText;
    finally
      Free;                     
    end;
end;

{-------------------------------------------------------------------------------
}
function CheckLinkedDeterminationEarthScience(AControl: TLinkedEdit; const AAdditionalCaption: String= ''): Boolean;
begin
  Result := DoCheck(AControl, stDeterminationEarthScience, '', AAdditionalCaption);
end;

{-------------------------------------------------------------------------------
}
function CheckLinkedLocation(AControl: TLinkedEdit; const AAdditionalCaption: String= ''): Boolean;
begin
  Result := DoCheck(AControl, stLocation, '', AAdditionalCaption);
end;

{-------------------------------------------------------------------------------
}
function CheckLinkedDeterminationLifeScience(AControl: TLinkedEdit; const AAdditionalCaption: String= ''): Boolean;
begin
  Result := DoCheck(AControl, stDeterminationLifeScience, '', AAdditionalCaption);
end;

{-------------------------------------------------------------------------------
}
function CheckLinkedIndividual(AControl: TLinkedEdit; const AAdditionalCaption: String= ''): Boolean;
begin
  Result := DoCheck(AControl, stIndividual, '', AAdditionalCaption);
end;

{-------------------------------------------------------------------------------
}
function CheckLinkedOrganisation(AControl: TLinkedEdit; const AAdditionalCaption: String= ''): Boolean;
begin
  Result := DoCheck(AControl, stOrganisation, '', AAdditionalCaption);
end;

{-------------------------------------------------------------------------------
}
function CheckLinkedDeptOrgName(AControl: TLinkedEdit; const AAdditionalCaption: String= ''): Boolean;
begin
  Result := DoCheck(AControl, stPeopleOrganisationDepartment, '', AAdditionalCaption);
end;

{-------------------------------------------------------------------------------
}
function CheckLinkedName(AControl: TLinkedEdit; const AAdditionalCaption: String= ''): Boolean;
begin
  Result := DoCheck(AControl, stPeopleOrganisation, '', AAdditionalCaption);
end;

{-------------------------------------------------------------------------------
}
function CheckLinkedSurvey(AControl: TLinkedEdit; const AAdditionalCaption: String= ''): Boolean;
begin
  Result := DoCheck(AControl, stSurvey, '', AAdditionalCaption);
end;

{-------------------------------------------------------------------------------
}
function CheckLinkedCollection(AControl: TLinkedEdit; const AAdditionalCaption: String= ''): Boolean;
begin
  Result := DoCheck(AControl, stCollection, '', AAdditionalCaption);
end;

{-------------------------------------------------------------------------------
}
function CheckLinkedReference(AControl: TLinkedEdit; const AAdditionalCaption: String= ''): Boolean;
begin
  Result := DoCheck(AControl, stInternalReferences, '', AAdditionalCaption);
end;

{-------------------------------------------------------------------------------
}
function CheckLinkedStore(AControl: TLinkedEdit; const AAdditionalCaption: String= ''): Boolean;
begin
  Result := DoCheck(AControl, stStoreName, '', AAdditionalCaption);
end;

{-------------------------------------------------------------------------------
}
function CheckLinkedConcept(AControl: TLinkedEdit; const AAdditionalCaption: String= ''): Boolean;
begin
  Result := DoCheck(AControl, stConcept, '', AAdditionalCaption);
end;

{-------------------------------------------------------------------------------
}
function CheckLinkedTerm(AControl: TLinkedEdit; const AAdditionalCaption: String= ''): Boolean;
begin
  Result := DoCheck(AControl, stTerm, '', AAdditionalCaption);
end;

{-------------------------------------------------------------------------------
}
function CheckLinkedTermInConceptGroup(AControl: TLinkedEdit; AConceptGroupKey: String;
    const AAdditionalCaption: String= ''): Boolean;
begin
  Result := DoCheck(AControl, stTermInConceptGroup, AConceptGroupKey, AAdditionalCaption);
end;

{-------------------------------------------------------------------------------
}
function CheckConceptGroupParameter(AControl: TConceptGroupComboBox; const AAdditionalCaption:  String= ''; AConceptGroupKeys: String=''): Boolean;
begin
  Result := DoCheck(AControl, stParameter, AConceptGroupKeys, AAdditionalCaption);
end;

{-------------------------------------------------------------------------------
}
function CheckConceptGroupProcess(AControl: TConceptGroupComboBox; const AAdditionalCaption:  String= ''; AConceptGroupKeys: String=''): Boolean;
begin
  Result := DoCheck(AControl, stProcess, AConceptGroupKeys, AAdditionalCaption);
end;

{-------------------------------------------------------------------------------
}
function ConvertIndividualKeyToCaption(const AKey: String; const ATableName: String): String;
begin
  Result := '';
  if CompareText(ATableName, TN_INDIVIDUAL) = 0 then
    Result := VarToStr(dmGeneral.GetStoredProcOutputParam('usp_Name_Get',
                       ['@Key', AKey], '@Caption'));
end;

{-------------------------------------------------------------------------------
}
function ConvertReferenceKeyToCaption(const AKey: String; const ATableName: String): String;
begin
  Result := VarToStr(dmGeneral.GetStoredProcOutputParam('usp_Reference_Get',
                     ['@Key', AKey], '@Caption'));
end;

{-------------------------------------------------------------------------------
}
function ConvertOrganisationKeyToCaption(const AKey: String; const ATableName: String): String;
begin
  Result := '';
  if CompareText(ATableName, TN_ORGANISATION) = 0 then
    Result := VarToStr(dmGeneral.GetStoredProcOutputParam('usp_Name_Get',
                       ['@Key', AKey], '@Caption'));
end;

{-------------------------------------------------------------------------------
}
function ConvertNameKeyToCaption(const AKey: String; const ATableName: String): String;
begin
  Result := VarToStr(dmGeneral.GetStoredProcOutputParam('usp_Name_Get',
                     ['@Key', AKey], '@Caption'));
end;

{-------------------------------------------------------------------------------
}
function ConvertStoreKeyToCaption(const AKey: String; const ATableName: String): String;
begin
  Result := VarToStr(dmGeneral.GetStoredProcOutputParam('usp_StoreName_Get',
                     ['@StoreKey', AKey], '@StoreName'));
end;

{-------------------------------------------------------------------------------
}
function ConvertConceptKeyToCaption(const AKey: String; const ATableName: String): String;
begin
  Result := VarToStr(dmGeneral.GetStoredProcOutputParam('usp_ConceptName_Get',
                     ['@Key', AKey], '@Caption'));
end;

{-------------------------------------------------------------------------------
}
function ConvertTermKeyToCaption(const AKey: String; const ATableName: String): String;
begin
  Result := VarToStr(dmGeneral.GetStoredProcOutputParam('usp_TermName_Get',
                     ['@Key', AKey], '@Caption'));
end;

{-------------------------------------------------------------------------------
}
function ConvertSurveyKeyToCaption(const AKey: String; const ATableName: String): String;
begin
  Result := VarToStr(dmGeneral.GetStoredProcOutputParam('usp_SurveyName_Get',
                     ['@Key', AKey], '@Caption'));
end;

{-------------------------------------------------------------------------------
}
function ConvertCollectionKeyToCaption(const AKey: String; const ATableName: String): String;
begin
  Result := VarToStr(dmGeneral.GetStoredProcOutputParam('usp_CollectionName_Get',
                     ['@Key', AKey], '@Caption'));
end;

{-------------------------------------------------------------------------------
}
function ConvertDeterminationLifeScienceKeyToCaption(const AKey: String; const ATableName: String): String;
begin
  Result := VarToStr(dmGeneral.GetStoredProcOutputParam('usp_DeterminationName_Get',
                     ['@Key', AKey, '@IsLifeScience', 1], '@Caption'));
end;

{-------------------------------------------------------------------------------
}
function ConvertDeterminationEarthScienceKeyToCaption(const AKey: String; const ATableName: String): String;
begin
  Result := VarToStr(dmGeneral.GetStoredProcOutputParam('usp_DeterminationName_Get',
                     ['@Key', AKey, '@IsLifeScience', 0], '@Caption'));
end;

{-------------------------------------------------------------------------------
}
function ConvertLocationKeyToCaption(const AKey: String; const ATableName: String): String;
begin
  Result := VarToStr(dmGeneral.GetStoredProcOutputParam('usp_LocationName_Get',
                     ['@Key', AKey], '@Caption'));
end;

{-------------------------------------------------------------------------------
}
function ConvertSpecimenKeyToCaption(const AKey: String; const ATableName: String): String;
begin
  Result := VarToStr(dmGeneral.GetStoredProcOutputParam('usp_SpecimenNameAndRegistration_Get',
                     ['@Key', AKey], '@Caption'));
end;

{-------------------------------------------------------------------------------
}
function CheckIsIndividual(const AKey: String): Boolean;
begin
  Result := dmGeneral.GetStoredProcOutputParam('usp_Name_IsIndividual_Get',
                                               ['@NameKey', AKey], '@IsIndividual') = True;
end;  // CheckIsIndividual

{-------------------------------------------------------------------------------
}
function UpdateIndividualNameControl(AControl: TLinkedEdit; const AKey, AMsg: String): Boolean;
begin
  if not CheckIsIndividual(AKey) then begin
    MessageDlg(AMsg, mtInformation, [mbOK], 0);
    if AControl.CanFocus then AControl.SetFocus;
    Result := False;
  end else begin
    AControl.Key := AKey;
    AControl.Text := ConvertIndividualKeyToCaption(AKey, TN_INDIVIDUAL);
    Result := True;
  end;
end;  // UpdateIndividualNameControl

{-------------------------------------------------------------------------------
}
function ConceptHasMappedTaxon(const AConceptKey: String): Boolean;
begin
  Result := not VarIsNull(dmGeneral.GetStoredProcOutputParam('usp_Concept_GetTaxonListItem',
                                                             ['@ConceptKey', AConceptKey],
                                                             '@TaxonListItemKey'));
  if Result then
    MessageDlg(ResStr_TaxonConceptMappingExists, mtInformation, [mbOk], 0);
end;  // ConceptHasMappedTaxon

end.
