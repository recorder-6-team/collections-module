{===============================================================================
  Unit:        DesignationGeneral

  Defines:     TfraDesignationGeneral

  Description: Frame for the details of a concept's designation

  Created:     Nov 2003

  Last revision information:
    $Revision: 7 $
    $Date: 10/09/08 14:35 $
    $Author: Ericsalmon $

===============================================================================}
unit DesignationGeneral;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, BaseTabSheetFrameUnit, ExtCtrls, VagueDateEdit,
  ComboListID, LuxIDComboBox, ApplicationSettings, ConceptGroupComboBox,
  LuxembourgConstants, DataTypes, ResourceStrings;

type
  {-----------------------------------------------------------------------------
    General tab page allowing the details of a concept's designation to be 
    viewed and edited.  Note that many details of a designation are entered 
    using the Metadata tab.
  }
  TfraDesignationGeneral = class (TBaseTabSheetFrame)
    cmbStatus: TConceptGroupComboBox;
    eDateFrom: TVagueDateEdit;
    eDateTo: TVagueDateEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
  private
    FTimestamp: TSQLSvrTimestamp;
  protected
    procedure DeleteData; override;
    procedure EnableControls(AEnabled: Boolean); override;
    function GetCaption: string; override;
    procedure LoadData; override;
    procedure RegisterControls; override;
    procedure SaveData; override;
  end;
  
implementation

uses GeneralData;

{-==============================================================================
    TfraDesignationGeneral
===============================================================================}
{-------------------------------------------------------------------------------
  Run the delete stored proc. 
}
procedure TfraDesignationGeneral.DeleteData;
begin
  dmGeneral.RunDeleteStoredProc('usp_ConceptDesignation_Delete',
                               ['@Key', Key, '@Timestamp', FTimestamp]);
end;  // TfraDesignationGeneral.DeleteData 

{-------------------------------------------------------------------------------
  Enable the controls. 
}
procedure TfraDesignationGeneral.EnableControls(AEnabled: Boolean);
begin
  inherited;
  SetRequiredFieldsColourState(AEnabled, [cmbStatus, eDateFrom]);
end;  // TfraDesignationGeneral.EnableControls 

{-------------------------------------------------------------------------------
  Get the caption. 
}
function TfraDesignationGeneral.GetCaption: string;
begin
  Result := cmbStatus.CurrentItem;
end;  // TfraDesignationGeneral.GetCaption

{-------------------------------------------------------------------------------
  Store the timestamp. 
}
procedure TfraDesignationGeneral.LoadData;
begin
  inherited;
  
  with RegisteredRecordsets[0] do
    if not EOF then
      FTimestamp := Fields['Timestamp'].Value;
end;  // TfraDesignationGeneral.LoadData 

{-------------------------------------------------------------------------------
  Register the concept group and the controls.
}
procedure TfraDesignationGeneral.RegisterControls;
begin
  inherited;
  // Register recordsets used
  RegisterRecordset('usp_ConceptDesignation_Select');

  // Register controls getting their value straight from the registered recordsets.
  RegisterControl(cmbStatus, 'Designation_Type_Concept_Name',
            'Designation_Type_Concept_Key', True, ResStr_Status);

  // The correct CG Key depends on the current domain.
  RegisterConceptGroupComboBox(
      cmbStatus,
      VarToStr(dmGeneral.GetStoredProcOutputParam(
          'usp_ConceptDesignationsAvailable_Get',
          ['@Key', AdditionalProperties.GetProperty(PROP_CONCEPT_GROUP_KEY)],
          '@DesignationTypesGroupKey')));

  RegisterControl(eDateFrom, 'From', True, ResStr_DateFrom);
  RegisterControl(eDateTo, 'To');
end;  // TfraDesignationGeneral.RegisterControls

{-------------------------------------------------------------------------------
  Save the data to the database. 
}
procedure TfraDesignationGeneral.SaveData;
var
  lParams: Array of Variant;
begin
  lParams := VarArrayOf(['@Key', Key,
                          '@ConceptKey', ParentKey,
                          '@DesignationTypeConceptKey', cmbStatus.CurrentStrID,
                          '@FromVagueDateStart', eDateFrom.VagueDate.StartDate,
                          '@FromVagueDateEnd', eDateFrom.VagueDate.EndDate,
                          '@FromVagueDateType', eDateFrom.VagueDate.DateTypeString,
                          '@ToVagueDateStart', eDateTo.VagueDate.StartDate,
                          '@ToVagueDateEnd', eDateTo.VagueDate.EndDate,
                          '@ToVagueDateType', eDateTo.VagueDate.DateTypeString,
                          '@Timestamp', FTimestamp
                        ]);
  // Empty key means new record.
  if Key = '' then
    Key := VarToStr(dmGeneral.RunInsertStoredProc(TN_CONCEPT_DESIGNATION,
                                        'usp_ConceptDesignation_Insert',
                                        lParams,
                                        '@Key'
                                        ))
  else
    dmGeneral.RunUpdateStoredProc('usp_ConceptDesignation_Update', lParams);
end;  // TfraDesignationGeneral.SaveData 

{$R *.dfm}

end.



