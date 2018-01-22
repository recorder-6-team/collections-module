{===============================================================================
  Unit:        ExportToDict

  Defines:     TdlgExportToDict

  Description: Concept group to taxon dictionary export dialog

  Created:     Dec 2003

  Last revision information:
    $Revision: 10 $
    $Date: 23/06/11 16:56 $
    $Author: Jamesbichard $

===============================================================================}
unit ExportToDict;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ChecklistTransferForm, DomainConceptGroupSelector, StdCtrls,
  ComboListID, ImageListButton, ADODB, ExtCtrls, SelectTaxonList;

type
  {-----------------------------------------------------------------------------
    Dialog allowing the user to select a a concept group in the Thesaurus to
    export to a checklist in the Taxon Dictionary.
  }
  TdlgExportToDict = class(TdlgChecklistTransferForm)
    chkAllListVersions: TCheckBox;
    gbPreferredLists: TGroupBox;
    btnMoveUp: TImageListButton;
    btnMoveDown: TImageListButton;
    btnAdd: TImageListButton;
    btnDelete: TImageListButton;
    lbPreferredLists: TIDListBox;
    procedure btnAddClick(Sender: TObject);
    procedure lbPreferredListsClick(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
    procedure btnMoveUpClick(Sender: TObject);
    procedure btnMoveDownClick(Sender: TObject);
    procedure fraDomainConceptGroupsSelectorcmbDomainsSelect(
      Sender: TObject);
    procedure fraDomainConceptGroupsSelectorcmbConceptGroupsClick(
      Sender: TObject);
  private
    procedure SelectChecklist;
    procedure GetPreferredLists;
    procedure EnablePreferredListsControls;
  protected
    procedure CloseTransfer; override;
    procedure Notify; override;
    function TransferProcedure: string; override;
    procedure Validate; override;
    procedure StartTransfer; override;
  public
    procedure SetComboboxes(ADomainKey: String; AConceptGroupKey: String);
  end;

implementation

{$R *.dfm}

uses
  GeneralData, DataClasses, ResourceStrings;

{-------------------------------------------------------------------------------
}
procedure TdlgExportToDict.CloseTransfer;
begin
  inherited;

  if chkAllListVersions.Checked then
    dmGeneral.RunStoredProc('usp_TaxonListItem_EnableAllListVersions',
                            ['@taxon_list_key', ChecklistKey]);

  // Always rebuild Index_Taxon_Name afterward.
  dmGeneral.RunStoredProc('usp_IndexTaxonName_Rebuild_ForSingleList',
                          ['@taxon_list_key', ChecklistKey]);
end;

{-------------------------------------------------------------------------------
  Name of stored procedure called to perform export.
}
function TdlgExportToDict.TransferProcedure: string;
begin
  Result := 'usp_TaxonList_ImportConceptGroup';
end;

{-------------------------------------------------------------------------------
  Update Valid according to whether or not specified export is possible.
}
procedure TdlgExportToDict.Validate;
var
  currentGroupKey: TKeyString;
begin
  currentGroupKey := VarToStr(dmGeneral.GetStoredProcOutputParam(
                            'usp_TaxonList_GetConceptGroup',
                            ['@taxon_list_key', ChecklistKey],
                            '@concept_group_key'));
  Valid := (currentGroupKey = '') or (currentGroupKey = ConceptGroupKey);
  if not Valid then
  begin
    ShowMessage(ResStr_ChecklistAlreadyHasConceptGroup);
  end;
end;

{-------------------------------------------------------------------------------
  Notify the user of the completed export.
}
procedure TdlgExportToDict.Notify;
begin
  ShowMessage(Format(
          ResStr_ExportConceptGroupComplete,
          [fraDomainConceptGroupsSelector.cmbConceptGroups.Text]));
end;

{-------------------------------------------------------------------------------
}
procedure TdlgExportToDict.SelectChecklist;
var
  taxonListKey: TKeyString;
begin
  taxonListKey := VarToStr(dmGeneral.GetStoredProcOutputParam(
                            'usp_ConceptGroup_GetTaxonList',
                            ['@concept_group_key', ConceptGroupKey],
                            '@taxon_list_key'));

  cmbChecklist.Enabled := (taxonListKey = '');

  if not cmbChecklist.Populated then
  begin
    cmbChecklist.PopulateContent;
  end;

  if taxonListKey <> '' then
  begin
    cmbChecklist.ItemIndex := cmbChecklist.IDIndexOf(taxonListKey);
  end
  else
  begin
    if cmbChecklist.IndexOf('<Create new checklist>') = -1 then
      cmbChecklist.Insert(0, '<Create new checklist>', '');
    cmbChecklist.ItemIndex := 0;
  end;
end;

{-------------------------------------------------------------------------------
  Sets the values in fraDomainConceptGroupsSelector using the specified keys
}
procedure TdlgExportToDict.SetComboboxes(
  ADomainKey: String;
  AConceptGroupKey: String);
begin
  if Length(ADomainKey) > 0 then
  begin
    fraDomainConceptGroupsSelector.SelectDomain(ADomainKey);
    if Length(AConceptGroupKey) > 0 then
    begin
      fraDomainConceptGroupsSelector.SelectConceptGroup(AConceptGroupKey);
      SelectChecklist;
      GetPreferredLists;
    end;
  end;
  UpdateButtons;
  EnablePreferredListsControls;
end;

{-------------------------------------------------------------------------------
  Opens a SelectTaxonList dialog with the option to add unselected taxon lists
  to the list box.
}
procedure TdlgExportToDict.btnAddClick(Sender: TObject);
var
  lRecordset: _Recordset;
  i: Integer;
begin
  lRecordset := dmGeneral.GetRecordset(
    'usp_TaxonList_Select',
    ['@ConceptGroupKey', fraDomainConceptGroupsSelector.ConceptGroupKey]);

  with TdlgSelectTaxonList.Create(nil) do
    try
      for i := 0 to lbPreferredLists.Count - 1 do
      begin
        AddPreferredList(lbPreferredLists.StrID[i]);
      end;
      GetTaxonLists;
      if ShowModal = mrOk then
      begin
        lbPreferredLists.Add(GetTaxonListName, GetTaxonListKey);
        lbPreferredLists.Selected[lbPreferredLists.Count - 1] := True;
        EnablePreferredListsControls;
      end;
    finally
      Free;
    end;
end;

{-------------------------------------------------------------------------------
  Populates list box with preferred lists for selected concept group.
}
procedure TdlgExportToDict.GetPreferredLists;
var
  lRecordset: _Recordset;
begin
  lbPreferredLists.Clear;
  lRecordset := dmGeneral.GetRecordset(
                  'usp_ConceptGroupPreferredTaxonList_Select',
                  ['@ConceptGroupKey', fraDomainConceptGroupsSelector.ConceptGroupKey]);

  while not lRecordset.Eof do
  begin
    lbPreferredLists.Add(
      VarToStr(lRecordset.Fields['Item_Name'].Value),
      VarToStr(lRecordset.Fields['Item_Key'].Value));
    lRecordset.MoveNext;
  end;
end;

{-------------------------------------------------------------------------------
}
procedure TdlgExportToDict.lbPreferredListsClick(Sender: TObject);
begin
  inherited;
  EnablePreferredListsControls;
end;

{-------------------------------------------------------------------------------
}
procedure TdlgExportToDict.btnDeleteClick(Sender: TObject);
var
  lIndex: Integer;
begin
  if lbPreferredLists.CurrentStrID <> '' then
  begin
    if (lbPreferredLists.ItemIndex = lbPreferredLists.Count - 1) then
      lIndex :=lbPreferredLists.ItemIndex - 1
    else if (lbPreferredLists.Count = 1) then
      lIndex := -1
    else
      lIndex := lbPreferredLists.ItemIndex;
    lbPreferredLists.Delete(lbPreferredLists.ItemIndex);
    if lIndex <> -1 then lbPreferredLists.Selected[lIndex] := True;
    EnablePreferredListsControls;
  end;
end;

{-------------------------------------------------------------------------------
}
procedure TdlgExportToDict.btnMoveUpClick(Sender: TObject);
begin
  if lbPreferredLists.CurrentStrID <> '' then
  begin
    lbPreferredLists.SwapItems(
      lbPreferredLists.ItemIndex,
      lbPreferredLists.ItemIndex - 1);
    //SwapListItems(lbPreferredLists.ItemIndex, lbPreferredLists.ItemIndex - 1);
    lbPreferredLists.Selected[lbPreferredLists.ItemIndex - 1] := True;
    EnablePreferredListsControls;
  end;
end;

{-------------------------------------------------------------------------------
}
procedure TdlgExportToDict.btnMoveDownClick(Sender: TObject);
begin
  if lbPreferredLists.CurrentStrID <> '' then
  begin
    lbPreferredLists.SwapItems(
      lbPreferredLists.ItemIndex,
      lbPreferredLists.ItemIndex + 1);
    //SwapListItems(lbPreferredLists.ItemIndex, lbPreferredLists.ItemIndex + 1);
    lbPreferredLists.Selected[lbPreferredLists.ItemIndex + 1] := True;
    EnablePreferredListsControls;
  end;
end;

{-------------------------------------------------------------------------------

}
procedure TdlgExportToDict.EnablePreferredListsControls;
var
  lListSelected: Boolean;
begin
  lListSelected := lbPreferredLists.ItemIndex <> -1;
  btnAdd.Enabled := fraDomainConceptGroupsSelector.cmbConceptGroups.ItemIndex <> -1;
  btnDelete.Enabled := lListSelected;
  btnMoveUp.Enabled := lListSelected and (lbPreferredLists.ItemIndex > 0);
  btnMoveDown.Enabled := lListSelected and
      (lbPreferredLists.ItemIndex < lbPreferredLists.Count - 1);
end;

{-------------------------------------------------------------------------------
}
procedure TdlgExportToDict.fraDomainConceptGroupsSelectorcmbDomainsSelect(
  Sender: TObject);
begin
  inherited;
  cmbChecklist.Clear;
  lbPreferredLists.Clear;
  EnablePreferredListsControls;
end;

{-------------------------------------------------------------------------------
  Clears any existing preferred lists for the selected concept group, and
  inserts records for each of the lists in the listbox before beginning the
  export procedure.
}
procedure TdlgExportToDict.StartTransfer;
var
  i: Integer;
  lPreferredListsUpdated: Boolean;
begin
  lPreferredListsUpdated := False;
  dmGeneral.Connection.BeginTrans;
  try
    dmGeneral.RunStoredProc(
        'usp_ConceptGroupPreferredTaxonList_Delete',
        ['@ConceptGroupKey', fraDomainConceptGroupsSelector.ConceptGroupKey]);

    for i := 0 to lbPreferredLists.Count - 1 do
    begin
      dmGeneral.RunStoredProc(
        'usp_ConceptGroupPreferredTaxonList_Insert',
        ['@ConceptGroupKey', fraDomainConceptGroupsSelector.ConceptGroupKey,
         '@TaxonListKey', lbPreferredLists.StrId[i]]);
    end;
    dmGeneral.Connection.CommitTrans;
    lPreferredListsUpdated := True;
  except
    on E: Exception do dmGeneral.Connection.RollbackTrans;
  end;  
  if lPreferredListsUpdated then inherited;
end;

{-------------------------------------------------------------------------------
}
procedure TdlgExportToDict.fraDomainConceptGroupsSelectorcmbConceptGroupsClick(
  Sender: TObject);
begin
  SelectChecklist;
  if fraDomainConceptGroupsSelector.cmbConceptGroups.ItemIndex <> -1 then
  begin
    btnAdd.Enabled := True;
    GetPreferredLists;
  end;
  UpdateButtons;
end;

end.
