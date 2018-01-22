{===============================================================================
  Unit:        ImportFromDict

  Defines:     TdlgImportFromDict

  Description: Checklist import dialog

  Created:     Dec 2003

  Last revision information:
    $Revision: 8 $
    $Date: 26/05/11 15:50 $
    $Author: Andrewkemp $

===============================================================================}
unit ImportFromDict;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ChecklistTransferForm, ADODB, DomainConceptGroupSelector,
  StdCtrls, ComboListID, ImageListButton, DataTypes, ExtCtrls,
  ConceptGroupQualityChecker;

type
  TdlgImportFromDict = class(TdlgChecklistTransferForm)
    procedure cmbChecklistClick(Sender: TObject);
  protected
    FTimestamp: TSQLSvrTimestamp;
    procedure RunTransfer; override;
    procedure Validate; override;
    function TransferProcedure: string; override;
    procedure Notify; override;
    procedure ResolvePotentialSynonyms; virtual;
  end;

var
  dlgImportFromDict: TdlgImportFromDict;

implementation

{$R *.dfm}

uses
  GeneralData, ResourceStrings;

procedure TdlgImportFromDict.RunTransfer;
begin
{-------------------------------------------------------------------------------
  Perform the import.
}
  FTimestamp := dmGeneral.GetStoredProcOutputParam(
                        'usp_ConceptGroup_GetLatestTimestamp',
                        ['@concept_group_key', ConceptGroupKey],
                        '@timestamp');
  inherited;
end;

procedure TdlgImportFromDict.Validate;
var
  CurrentListKey: string;
begin
{-------------------------------------------------------------------------------
  Update Valid according to whether or not specified import is possible.
}
  CurrentListKey := VarToStr(dmGeneral.GetStoredProcOutputParam(
                          'usp_ConceptGroup_GetTaxonList',
                          ['@concept_group_key', ConceptGroupKey],
                          '@taxon_list_key'));
  Valid := (CurrentListKey = '') or (CurrentListKey = ChecklistKey);
  if not Valid then
  begin
    ShowMessage(ResStr_ConceptGroupAlreadyHasChecklist);
  end;
end;

function TdlgImportFromDict.TransferProcedure: string;
begin
{ ------------------------------------------------------------------------------
  Name of stored procedure called to perform import.
}
  Result := 'usp_ConceptGroup_ImportTaxonList';
end;

procedure TdlgImportFromDict.Notify;
begin
{-------------------------------------------------------------------------------
  Notify the user of the completed import, and prompt for resolution of
  potential synonyms if necessary.
}
  ShowMessage(Format(ResStr_ImportChecklistComplete, [cmbChecklist.Text]));
  ResolvePotentialSynonyms;
end;

procedure TdlgImportFromDict.ResolvePotentialSynonyms;
var
  lNumberOfPotSynonyms: Integer;
begin
{-------------------------------------------------------------------------------
  Prompt the user to deal with potential synonyms in imported data (concepts
  with timestamp greater then the initial value).
}
  lNumberOfPotSynonyms := dmGeneral.GetStoredProcOutputParam(
      'usp_PotentialSynonyms_Count',
      ['@ConceptGroupKey', ConceptGroupKey],
      '@RowCount');

  if lNumberOfPotSynonyms > 0 then
    TfrmConceptGroupQualityChecker.Create(
      Owner,
      true,
      ConceptGroupKey,
      '',
      '',
      0,  //ensures ROWCOUNT = 0, so there is no upper limit on results returned
      true,
      FTimestamp);   
end;

{-------------------------------------------------------------------------------
}
procedure TdlgImportFromDict.cmbChecklistClick(Sender: TObject);
var
  ConceptGroupKey: string;
  DomainKey: string;
begin
  ConceptGroupKey := VarToStr(dmGeneral.GetStoredProcOutputParam(
                            'usp_TaxonList_GetConceptGroup',
                            ['@taxon_list_key', ChecklistKey],
                            '@concept_group_key'));

  fraDomainConceptGroupsSelector.cmbDomains.Enabled := (ConceptGroupKey = '');
  fraDomainConceptGroupsSelector.cmbConceptGroups.Enabled :=
          (ConceptGroupKey = '');

  if ConceptGroupKey <> '' then
  begin
    DomainKey := dmGeneral.GetStoredProcOutputParam(
                        'usp_ConceptGroup_GetDomain',
                        ['@Key', ConceptGroupKey],
                        '@domain_key');
    fraDomainConceptGroupsSelector.SelectDomain(DomainKey);
    fraDomainConceptGroupsSelector.SelectConceptGroup(ConceptGroupKey);
  end;
  inherited;
end;

end.
