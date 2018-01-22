{===============================================================================
  Unit:        SearchManager

  Defines:     TSearchManager

  Description: Class for managing searches in the Collections Module

  Model:       Find.mpb

  Created:     August 2003

  Last revision information:
    $Revision: 42 $
    $Date: 7/07/09 9:29 $
    $Author: Ericsalmon $

===============================================================================}
unit SearchManager;

interface

uses
  SysUtils, Windows, Messages, Classes, Graphics, Controls, Forms, Dialogs,
  ResourceStrings, Find, ADODb, GeneralFunctions, GeneralData, Variants;

type
  TSearchType = (stCollection, stIndividual, stOrganisation,
      stPeopleOrganisation, stPeopleOrganisationDepartment,stStoreName,
      stTermInConceptGroupVersion, stConditionCheck, stTasksIdentified,
      stEnquiry, stJob, stMovement, stSpecimen, stSurvey, stAccession, stLoan,
      stDeterminationLifeScience, stDeterminationEarthScience,
      stLocation, stParameter, stOccurrence, stInternalReferences, stProcess,
      stConcept, stTerm, stNone, stTermInConceptGroup, stTaxonOccurrence,
      stConceptRank, stTermInSubjectArea);

  TSearchManager = class(TObject)
  private
    FAdditionalCaption: String;
    FResultText: String;
    FSearchKey: String;
    FSearchType: TSearchType;
    function GetFindDialogTitle: String;
    function GetRecordset(const ASearchTerm: String): _Recordset;
    function GetResolveDuplicateStoredProc: String;
    function GetStoredProc: String;
    function HaveOneExactMatch(ARecordset: _Recordset; const ASearchTerm: String; WantExact:
        Boolean): Boolean;
    procedure SetAdditionalCaption(const Value: String);
    procedure SetSearchKey(const Value: String);
    procedure SetSearchType(Value: TSearchType);
    function ShowFindDialog(const ASearchTerm: String; ARecordset: _Recordset): String;
  public
    function FindUnique(const ASearchTerm: String): String;
    function RunSearch: String; overload;
    function RunSearch(const ASearchTerm: String; ANoFindDialog: Boolean = False): String;
        overload;
    property AdditionalCaption: String read FAdditionalCaption write SetAdditionalCaption;
    property ResultText: String read FResultText;
    property SearchKey: String read FSearchKey write SetSearchKey;
    property SearchType: TSearchType read FSearchType write SetSearchType;
  end;
  
//==============================================================================
implementation

uses
  ApplicationSettings;

{-==============================================================================
    TSearchManager
===============================================================================}
{-------------------------------------------------------------------------------
  Returns a key for the item searched if an exact single match is found. 
}
function TSearchManager.FindUnique(const ASearchTerm: String): String;
var
  lRecordset: _Recordset;
begin
  lRecordset := GetRecordset(ASearchTerm);
  
  if HaveOneExactMatch(lRecordset, ASearchTerm, True) then
  begin
    Result      := lRecordset.Fields['Item_Key'].Value;
    FResultText := lRecordset.Fields['SearchTerm'].Value;
  end else
    Result := '';
end;  // TSearchManager.FindUnique 

{-------------------------------------------------------------------------------
  Returns part of the find dialog caption appropriate to the search type.  The returned text
      has 'Search for ' prefixed to it to form the dialog caption.
}
function TSearchManager.GetFindDialogTitle: String;
begin
  case FSearchType of
    stAccession :                   Result := ResStr_Accessions;
    stCollection:                   Result := ResStr_Collections;
    stConcept:                      Result := ResStr_Concepts;
    stConceptRank:                  Result := ResStr_ConceptRanks;
    stConditionCheck:               Result := ResStr_ConditionChecks;
    stDeterminationLifeScience:     Result := ResStr_DeterminationsLifeSciences;
    stDeterminationEarthScience:    Result := ResStr_DeterminationsEarthSciences;
    stEnquiry:                      Result := ResStr_Enquiries;
    stIndividual:                   Result := ResStr_Individuals;
    stInternalReferences:           Result := ResStr_InternalReferences;
    stJob:                          Result := ResStr_Jobs;
    stLoan:                         Result := ResStr_Loans;
    stLocation:                     Result := ResStr_Locations;
    stMovement:                     Result := ResStr_Movements;
    stOccurrence:                   Result := ResStr_Occurrences;
    stOrganisation:                 Result := ResStr_Organisations;
    stParameter:                    Result := ResStr_Parameters;
    stPeopleOrganisation:           Result := ResStr_PeopleOrganisations;
    stPeopleOrganisationDepartment: Result := ResStr_PeopleOrganisationsDepartments;
    stProcess:                      Result := ResStr_Processes;
    stSpecimen:                     Result := ResStr_Specimens;
    stStoreName:                    Result := ResStr_Stores;
    stSurvey:                       Result := ResStr_Surveys;
    stTasksIdentified:              Result := ResStr_TasksIdentified;
    stTaxonOccurrence:              Result := ResStr_TaxonOccurrences;
    stTerm,
    stTermInConceptGroup,
    stTermInConceptGroupVersion,
    stTermInSubjectArea:            Result := ResStr_Terms;
  end;
  if FAdditionalCaption <> '' then
    result := result + ' (' + FAdditionalCaption + ')';
end;  // TSearchManager.GetFindDialogTitle 

{-------------------------------------------------------------------------------
}
function TSearchManager.GetRecordset(const ASearchTerm: String): _Recordset;
var
  lCursor: TCursor;
begin
  lCursor := HourglassCursor;
  try
    Result := dmGeneral.GetRecordset(
        GetStoredProc, [
        '@SearchKey', SearchKey,
        '@SearchText', StringReplace(ASearchTerm, '*', '%', [rfReplaceAll]),
        '@PreferredSynonymsOnly', AppSettings.PreferredSynonymsOnly
        ]);
  finally
    DefaultCursor(lCursor);
  end;
end;  // TSearchManager.GetRecordset 

{-------------------------------------------------------------------------------
}
function TSearchManager.GetResolveDuplicateStoredProc: String;
begin
  case FSearchType of
    stTermInConceptGroup,
    stTermInConceptGroupVersion,
    stTermInSubjectArea:         Result := 'usp_ConceptFullySpecified_Select';
    stStoreName:                 Result := 'usp_StoreFullySpecified_Select';
    stLocation:                  Result := 'usp_LocationFullySpecified_Select';
  else
    Result := '';
  end;
end;  // TSearchManager.GetResolveDuplicateStoredProc

{-------------------------------------------------------------------------------
  Uses the search type to identify the stored procedure to run.
}
function TSearchManager.GetStoredProc: String;
begin
  case FSearchType of
    stAccession:                    Result := 'usp_Accessions_Select_ForSearch';
    stCollection:                   Result := 'usp_Collection_Select_ForSearch';
    stConcept:                      Result := 'usp_Concepts_Select_ForSearch';
    stConceptRank:                  Result := 'usp_ConceptRanks_Select_ForSearch';
    stConditionCheck:               Result := 'usp_ConditionChecks_Select_ForSearch';
    stDeterminationLifeScience:     Result := 'usp_DeterminationsLifeSciences_Select_ForSearch';
    stDeterminationEarthScience:    Result := 'usp_DeterminationsEarthSciences_Select_ForSearch';
    stEnquiry:                      Result := 'usp_Enquiries_Select_ForSearch';
    stIndividual:                   Result := 'usp_Individual_Select_ForNameSearch';
    stInternalReferences:           Result := 'usp_InternalReferences_Select_ForSearch';
    stJob:                          Result := 'usp_Jobs_Select_ForSearch';
    stLoan:                         Result := 'usp_Loans_Select_ForSearch';
    stLocation:                     Result := 'usp_Locations_Select_ForSearch';
    stMovement:                     Result := 'usp_Movements_Select_ForSearch';
    stOccurrence:                   Result := 'usp_Occurrences_Select_ForSearch';
    stOrganisation:                 Result := 'usp_Organisation_Select_ForNameSearch';
    stParameter:                    Result := 'usp_Parameters_Select_ForSearch';
    stPeopleOrganisation:           Result := 'usp_Name_Select_ForNameSearch';
    stPeopleOrganisationDepartment: Result := 'usp_DeptOrgPeople_Select_ForNameSearch';
    stProcess:                      Result := 'usp_Processes_Select_ForSearch';
    stSpecimen:                     Result := 'usp_Specimens_Select_ForSearch';
    stStoreName:                    Result := 'usp_Store_Select_ForNameSearch';
    stSurvey:                       Result := 'usp_Survey_Select_ForSearch';
    stTasksIdentified:              Result := 'usp_Tasks_Select_ForSearch';
    stTaxonOccurrence:              Result := 'usp_TaxonOccurrences_Select_ForSearch';
    stTerm:                         Result := 'usp_Terms_Select_ForSearch';
    stTermInConceptGroup:           Result := 'usp_Concept_Select_ForConceptGroupSearch';
    stTermInConceptGroupVersion:    Result := 'usp_Concept_Select_ForConceptGroupVersionSearch';
    stTermInSubjectArea:            Result := 'usp_Concept_Select_ForSubjectAreaSearch';
  end;
end;  // TSearchManager.GetStoredProc 

{-------------------------------------------------------------------------------
  Returns trus if there is one, and only one exact match for the search term in the recordset. 
}
function TSearchManager.HaveOneExactMatch(ARecordset: _Recordset; const ASearchTerm: String;
    WantExact: Boolean): Boolean;
begin
  with ARecordset do
  begin
    // If only one record, then must be one match.
    Result := RecordCount = 1;

    // If a single exact match is required, check for it. This used to also require
    // that the record count was 1. However, it sometimes happened that more than
    // one record was returned, (i.e. 'dorset' and 'dorset software' when searching
    // for 'dorset'), yet there was is obviously an exact match here.
    if WantExact then
      Result :=
          (RecordCount > 0) and
          (CompareText(VarToStr(Fields['SearchTerm'].Value), ASearchTerm) = 0)
    else
    if RecordCount > 1 then
    begin
      // If more than one record, check if first item matches but second one doesn't.
      if (not Result) and (RecordCount > 1) then
      begin
        MoveFirst;
        if CompareText(VarToStr(Fields['SearchTerm'].Value), ASearchTerm) = 0 then
        begin
          // first term does match
          MoveNext;
          // check second one doesn't
          Result := CompareText(VarToStr(Fields['SearchTerm'].Value), ASearchTerm) <> 0;
        end; // if
        MoveFirst;
      end; // if
    end; // if
  end;
end;  // TSearchManager.HaveOneExactMatch 

{-------------------------------------------------------------------------------
  Runs a search.  In this case, no initial search text is provided and the Find dialog is
      always shown.
}
function TSearchManager.RunSearch: String;
begin
  Result := ShowFindDialog('', nil);
end;  // TSearchManager.RunSearch 

{-------------------------------------------------------------------------------
  Runs a search.  If a unique match is found, or the Find dialog is shown and the user selects
      a match, then the resulting item key is returned.  Otherwise the result is an empty
      string.
}
function TSearchManager.RunSearch(const ASearchTerm: String; ANoFindDialog: Boolean = False):
    String;
var
  lRecordset: _Recordset;
  lSearchTerm: String;
begin
  // setup wildcards
  lSearchTerm := StringReplace(ASearchTerm, '%', '*', [rfReplaceAll]);
  lRecordset  := GetRecordset(lSearchTerm);
  if HaveOneExactMatch(lRecordset, lSearchTerm, False) then
  begin
    Result      := lRecordset.Fields['Item_Key'].Value;
    FResultText := lRecordset.Fields['SearchTerm'].Value;
  end else
  if not ANoFindDialog then
    Result := ShowFindDialog(ASearchTerm, lRecordset);
end;  // TSearchManager.RunSearch 

{-------------------------------------------------------------------------------
  Accessor method. 
}
procedure TSearchManager.SetAdditionalCaption(const Value: String);
begin
  FAdditionalCaption := Value;
end;  // TSearchManager.SetAdditionalCaption 

{-------------------------------------------------------------------------------
  Accessor method. 
}
procedure TSearchManager.SetSearchKey(const Value: String);
begin
  FSearchKey := Value;
end;  // TSearchManager.SetSearchKey

{-------------------------------------------------------------------------------
  Accessor method. 
}
procedure TSearchManager.SetSearchType(Value: TSearchType);
begin
  FSearchType := Value;
end;  // TSearchManager.SetSearchType 

{-------------------------------------------------------------------------------
  Displays the find dialog as no unique match was found. 
}
function TSearchManager.ShowFindDialog(const ASearchTerm: String; ARecordset: _Recordset): String;
begin
  with TdlgFind.Create(nil) do
    try
      Title                          := GetFindDialogTitle;
      StoredProcName                 := GetStoredProc;
      ResolveDuplicateStoredProcName := GetResolveDuplicateStoredProc;
      SetStoredProcParameters([
          '@PreferredSynonymsOnly', AppSettings.PreferredSynonymsOnly,
          '@SearchKey', SearchKey
          ]);

      if Assigned(ARecordset) then
        PopulateFromExistingRecordset(ARecordset, ASearchTerm);

      StartSearch(ASearchTerm);
      if ShowModal = mrOk then
      begin
        Result      := ResultKey;
        FResultText := ResultText;
      end else
        Result := '';
    finally
      Free;
    end;
end;  // TSearchManager.ShowFindDialog

end.
