{===============================================================================
  Unit:        LocalDomainGeneral

  Defines:

  Description:

  Created:

  Last revision information:
    $Revision: 12 $
    $Date: 30/08/11 15:16 $
    $Author: Jamesbichard $

===============================================================================}

unit LocalDomainGeneral;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, BaseTabSheetFrameUnit, ExtCtrls, BaseDetailFrameUnit,
  ComboListID, LuxIDComboBox, DataTypes, GeneralData,
  LuxembourgConstants, PublishedTermRuleSelector, TermGenerationFunctions;

type
  {-----------------------------------------------------------------------------
    Details frame allowing the user to view and edit a domain version.
    This frame is embedded onto TfrmDomainsAndConceptGroups when the user is
    editing a domain version.
  }
  TfraLocalDomainGeneral = class(TBaseTabSheetFrame)
    cmbLanguage: TLuxIDComboBox;
    eConceptGroupLabel: TEdit;
    eLocalDomainName: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    lblLanguage: TLabel;
    fraPublishedTermRuleSelector: TfraPublishedTermRuleSelector;
    procedure cmbLanguagePopulate(Sender: TObject);
  private
    FTimestamp: TSQLSvrTimestamp;
    FTermGeneratorKey: string;
    FLocalDomainName: string;
  protected
    procedure DeleteData; override;
    procedure EnableControls(AEnabled: Boolean); override;
    function GetCaption: string; override;
    procedure LoadData; override;
    procedure RegisterControls; override;
    procedure SaveData; override;
  end;
  

implementation

{$R *.dfm}

uses
  ResourceStrings, ThesaurusApplicationSettings;

{-==============================================================================
    TfraLocalDomainGeneral
===============================================================================}
{-------------------------------------------------------------------------------
  Populate the cmbLanguage combo box. 
}
procedure TfraLocalDomainGeneral.cmbLanguagePopulate(Sender: TObject);
begin
  inherited;
  with dmGeneral.GetRecordset('usp_Languages_Select', []) do
    while not EOF do begin
      cmbLanguage.Add(
          VarToStr(Fields['Language_Key'].Value) + ' - ' + VarToStr(
              Fields['Item_Name'].Value),
          VarToStr(Fields['Language_Key'].Value));
      MoveNext;
    end;
end;  // TfraLocalDomainGeneral.cmbLanguagePopulate 

{-------------------------------------------------------------------------------
}
procedure TfraLocalDomainGeneral.DeleteData;
begin
  dmGeneral.RunDeleteStoredProc('usp_LocalDomain_Delete',
                               ['@Key', Key, '@Timestamp', FTimestamp]);
  with ThesApplicationSettings do
    if LogDeletions then LogDeletion('usp_LocalDomain_Delete',
                               ['@Key', Key, '@Timestamp', FTimestamp]);
end;  // TfraLocalDomainGeneral.DeleteData 

{-------------------------------------------------------------------------------
  Toggle the colour of the required controls. 
}
procedure TfraLocalDomainGeneral.EnableControls(AEnabled: Boolean);
begin
  inherited;
  SetRequiredFieldsColourState(AEnabled, [eLocalDomainName, cmbLanguage,
                                          eConceptGroupLabel]);
  fraPublishedTermRuleSelector.cmbPublishedTermRule.ReadOnly := not AEnabled;
end;  // TfraLocalDomainGeneral.EnableControls 

{-------------------------------------------------------------------------------
  Return the caption for the treeview. 
}
function TfraLocalDomainGeneral.GetCaption: string;
begin
  Result := eLocalDomainName.Text + ' (' + cmbLanguage.Text + ')';
end;  // TfraLocalDomainGeneral.GetCaption 

{-------------------------------------------------------------------------------
  Load the timestamp. 
}
procedure TfraLocalDomainGeneral.LoadData;
begin
  inherited;
  if not RegisteredRecordsets[0].Eof then begin
    FTimestamp := RegisteredRecordsets[0].Fields['Timestamp'].Value;
    FLocalDomainName := VarToStr(RegisteredRecordsets[0].Fields['Item_Name'].Value);
    FTermGeneratorKey := VarToStr(RegisteredRecordsets[0].Fields['Term_Generator_Key'].Value);
  end else
    eConceptGroupLabel.Text := ResStr_List;
end;  // TfraLocalDomainGeneral.LoadData 

{-------------------------------------------------------------------------------
  Register the recordset and the controls. 
}
procedure TfraLocalDomainGeneral.RegisterControls;
begin
  inherited;
  // Register recordsets used
  RegisterRecordset('usp_LocalDomain_Select');
  
  // Register controls getting their value straight from the registered recordsets.
  RegisterControl(eLocalDomainName, 'Item_Name', True, ResStr_LocalDomainName);
  RegisterControl(
          cmbLanguage,
          'Language_Name',
          'Language_Key',
          True,
          ResStr_PrimaryLanguage);
  RegisterControl(
          eConceptGroupLabel,
          'Concept_Group_Label',
          True,
          ResStr_ConceptGroupLabel);
  RegisterControl(
          fraPublishedTermRuleSelector.cmbPublishedTermRule,
          'Term_Generator_Name',
          'Term_Generator_Key');
  fraPublishedTermRuleSelector.SetDefaultItem;
end;  // TfraLocalDomainGeneral.RegisterControls 

{-------------------------------------------------------------------------------
  Save the data. 
}
procedure TfraLocalDomainGeneral.SaveData;
var
  lParams: Array of Variant;
  lUpdateDescendents: Boolean;
begin
  lUpdateDescendents := false;
  if (eLocalDomainName.Text <> FLocalDomainName) or
      (fraPublishedTermRuleSelector.Key <> FTermGeneratorKey) then
  begin
    if MessageDlg(
        Format(ResStr_UpdateDescendentConcepts, ['local domain']),
        mtConfirmation,
        [mbYes, mbNo], 0) = mrYes then
     lUpdateDescendents := true;
  end;

  lParams := VarArrayOf(['@Key', Key,
                          '@ItemName', eLocalDomainName.Text,
                          '@LanguageKey', cmbLanguage.CurrentStrID,
                          '@DomainKey', ParentKey,
                          '@ConceptGroupLabel', eConceptGroupLabel.Text,
                          '@TermGeneratorKey', fraPublishedTermRuleSelector.Key, 
                          '@UpdateDescendents', lUpdateDescendents,
                          '@Timestamp', FTimestamp,
                          '@SystemSuppliedData', 0
                        ]);
  // Empty key means new record.
  if Key = '' then
    Key := VarToStr(dmGeneral.RunInsertStoredProc(TN_LOCAL_DOMAIN,
                                                    'usp_LocalDomain_Insert',
                                                    lParams,
                                                    '@Key'))
  else
    dmGeneral.RunUpdateStoredProc('usp_LocalDomain_Update', lParams);

  //N.B. Published and search terms for concepts within this local domain are
  //updated through use of a trigger.

end;  // TfraLocalDomainGeneral.SaveData

end.
