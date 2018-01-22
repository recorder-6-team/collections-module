{===============================================================================
  Unit:        TermGenerationFunctions.pas

  Description: Helper functions for term generation manipulations

  Created:     August 2011

  Last revision information:
    $Revision: 3 $
    $Date: 15/09/11 11:03 $
    $Author: Jamesbichard $

===============================================================================}

unit TermGenerationFunctions;

interface

uses Variants, SysUtils, ADODB, GeneralData, LuxembourgConstants;

function GetPublishedTerm(
  const AKey: string;
  AParams: Array of Variant): string;
function GetSearchTerms(
  const AKey: string;
  AParams: Array of Variant): _Recordset;
function GetTermGeneratorKey(
  const AKey: string;
  AIsConceptGroupKey: Boolean;
  AGetFromParent: Boolean = false): string;

implementation

uses BaseADODataModule;

{-------------------------------------------------------------------------------
  Returns published terms. The key is a term generator key.
}
function GetPublishedTerm(
  const AKey: string;
  AParams: Array of Variant): string;
var
  lUpdateFunction: string;
begin
  Result := '';
  with dmGeneral.GetRecordset('usp_TermGenerator_Select',
        ['@Key', AKey]) do
  begin
    if not EOF then
      lUpdateFunction := VarToStr(Fields['Published_Term_Function'].Value);
  end;
  Result := dmGeneral.RunUserFunction(lUpdateFunction, AParams);
end;

{-------------------------------------------------------------------------------
  Returns search terms. The key is a term generator key
}
function GetSearchTerms(
  const AKey: string;
  AParams: Array of Variant): _Recordset;
var
  lSearchTermsProc: string;
begin
  with dmGeneral.GetRecordset('usp_TermGenerator_Select',
        ['@Key', AKey]) do
  begin
    if not EOF then
      lSearchTermsProc := VarToStr(Fields['Search_Term_Procedure'].Value);
  end;
  Result := dmGeneral.GetRecordset(lSearchTermsProc, AParams);
end;

{-------------------------------------------------------------------------------
  Returns the term generator key for a given concept, to be used for updating
  published and search terms. The boolean allows for specifying a concept group
  key, in case we are trying to get the term generator for a new concept with
  no ancestors.
}
function GetTermGeneratorKey(
  const AKey: string;
  AIsConceptGroupKey: Boolean;
  AGetFromParent: Boolean): string;
begin
  Result := dmGeneral.RunUserFunction('dbo.ufn_GetTermGenerator',
        [AKey, AIsConceptGroupKey, AGetFromParent]);
end;


end.
