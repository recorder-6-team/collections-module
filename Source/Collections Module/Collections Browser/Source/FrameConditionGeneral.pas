{===============================================================================
  Unit:        FrameConditionGeneral.pas

  Defines:     TfraConditionGeneral

  Description:

  Created:     June 2003

  Last revision information:
    $Revision: 14 $
    $Date: 16/10/12 11:27 $
    $Author: Alexanderpadley $

===============================================================================}
unit FrameConditionGeneral;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, BaseTabSheetFrameUnit, ExtCtrls, StdCtrls, ImageListButton,
  BaseCompositeComponent, LinkedControls, ComboListID, ConceptGroupComboBox,
  LuxembourgConstants, VagueDateEdit, DataClasses, DropTarget, DataTypes,
  ResourceStrings, InterfaceDataModule, LuxIDComboBox, ExceptionForm, Recorder2000_TLB,
  UserEdit;

type
  {-----------------------------------------------------------------------------
    General details tab page for the main aspects of a condition check.  This is embedded 
    onto TfraCondition when required.
  }
  TfraConditionGeneral = class (TBaseTabSheetFrame)
    chkAppliesToAllSpecimens: TCheckBox;
    cmbCondition: TConceptGroupComboBox;
    cmbType: TConceptGroupComboBox;
    eCheckedBy: TUserEdit;
    eDate: TVagueDateEdit;
    eRefNumber: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    lblCheckedBy: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    mmDetails: TMemo;
    mmDomains: TMemo;
    procedure eCheckedByGetData(Sender: TObject);
  private
    FTimestamp: TSQLSvrTimestamp;
    procedure DropCheckedBy(const Sender: TObject; const AFormat: Integer; const
        ASourceData: TKeyList; const ATextStrings: TStringList; var AHandled: Boolean);
    procedure UpdateCheckedBy(const AKeyList: IKeyList);
  protected
    procedure DeleteData; override;
    function GetCaption: String; override;
    procedure LoadData; override;
    procedure RegisterControls; override;
    procedure RegisterDragDropComponents; override;
    procedure SaveData; override;
    procedure ValidateData; override;
  end;

//==============================================================================
implementation

{$R *.dfm}

uses
  Validation, GeneralData, GeneralFunctions;

{-==============================================================================
    TfraConditionGeneral
===============================================================================}
{-------------------------------------------------------------------------------
  Run the delete stored proc to remove the record for this frame. 
}
procedure TfraConditionGeneral.DeleteData;
begin
  dmGeneral.RunDeleteStoredProc('usp_ConditionCheck_Delete',
                                ['@Key', Key, '@Timestamp', FTimestamp]);
end;  // TfraConditionGeneral.DeleteData 

{-------------------------------------------------------------------------------
  Handle drag and drop onto eCheckedBy 
}
procedure TfraConditionGeneral.DropCheckedBy(const Sender: TObject; const AFormat: 
    Integer; const ASourceData: TKeyList; const ATextStrings: TStringList; var AHandled: 
    Boolean);
begin
  DropLinkedEditData(AFormat, ASourceData, AHandled, eCheckedBy, TN_INDIVIDUAL,
                     'usp_FormattedNameForNameKey_Get', '@NameKey', '@FormattedName');
end;  // TfraConditionGeneral.DropCheckedBy 

{-------------------------------------------------------------------------------
}
procedure TfraConditionGeneral.eCheckedByGetData(Sender: TObject);
begin
  inherited;
  InitReturnData(UpdateCheckedBy, TN_NAME);
end;  // TfraConditionGeneral.eNameGetData

{-------------------------------------------------------------------------------
  Gets the caption for the node names. 
}
function TfraConditionGeneral.GetCaption: String;
begin
  Result := eDate.Text + ' - ' + cmbType.Text + ' - ' + eRefNumber.Text;
end;  // TfraConditionGeneral.GetCaption 

{-------------------------------------------------------------------------------
  Load the timestamp into a local variable. Also, decide whether chkAppliesToAllSpecimens 
      should be visible. 
}
procedure TfraConditionGeneral.LoadData;
var
  lHasCollections, lHasStores: Boolean;
begin
  inherited LoadData;
  with RegisteredRecordsets[0] do begin
    if not Eof then begin
      FTimestamp      := Fields['Timestamp'].Value;
      lHasCollections := Fields['Has_Collections'].Value;
      lHasStores      := Fields['Has_Stores'].Value;
    end else begin
      lHasCollections := False;
      lHasStores      := False;
    end;
  end;
  if not (lHasCollections or lHasStores) then
    chkAppliesToAllSpecimens.Visible := False
  else
    chkAppliesToAllSpecimens.Visible := True;
end;  // TfraConditionGeneral.LoadData 

{-------------------------------------------------------------------------------
  Register the controls and datasets.  
}
procedure TfraConditionGeneral.RegisterControls;
begin
  inherited;
  // Register recordsets used
  RegisterRecordset('usp_ConditionCheck_Select');
  
  // Register controls getting their value straight from the registered recordsets.
  RegisterControl(cmbType, 'Check_Type_Name', 'Type_Concept_Key', True, ResStr_CheckType);
  RegisterConceptGroupComboBox(cmbType, CG_CONDITION_CHECK_TYPES);
  RegisterControl(eRefNumber, 'Ref_Number', True, ResStr_ReferenceNumber);
  RegisterControl(eDate, '', True, ResStr_Date);
  RegisterControl(eCheckedBy, 'Checked_By_Name', 'Checked_By_Name_Key', True,
                  ResStr_CheckedBy, CheckLinkedIndividual, 'Name',
                  ConvertIndividualKeyToCaption);
  eCheckedBy.OnGetData := eCheckedByGetData;

  RegisterControl(cmbCondition, 'Condition_Name', 'Condition_Concept_Key', True,
                  ResStr_Condition);
  RegisterConceptGroupComboBox(cmbCondition, CG_CONDITION_CHECK_CONDITIONS);
  RegisterControl(mmDetails, 'Details');
    // Register asynchronously loaded controls
  RegisterAsyncControl(mmDomains, 'usp_DomainsForMask_Get',
                       ['@Mask', 'Domain_Mask'], '@Domains');
end;  // TfraConditionGeneral.RegisterControls 

{-------------------------------------------------------------------------------
  Register controls for drag and drop 
}
procedure TfraConditionGeneral.RegisterDragDropComponents;
begin
  RegisterDropComponent(eCheckedBy, DropCheckedBy, [TN_NAME, TN_INDIVIDUAL], [CF_JNCCDATA]);
end;  // TfraConditionGeneral.RegisterDragDropComponents 

{-------------------------------------------------------------------------------
  Save the record to the database. 
}
procedure TfraConditionGeneral.SaveData;
var
  lParams: Array Of Variant;
begin
  lParams := VarArrayOf(['@Key', Key,
                         '@TypeConceptKey', cmbType.CurrentStrID,
                         '@RefNumber', eRefNumber.Text,
                         '@VagueDateStart', eDate.VagueDate.StartDate,
                         '@VagueDateEnd', eDate.VagueDate.EndDate,
                         '@VagueDateType', eDate.VagueDate.DateTypeString,
                         '@CheckedByNameKey', eCheckedBy.Key,
                         '@ConditionConceptKey', cmbCondition.CurrentStrID,
                         '@AppliesToContainedSpecimens', chkAppliesToAllSpecimens.Checked,
                         '@Details', mmDetails.Text,
                         '@Timestamp', FTimestamp]);
  // Empty key means new record.
  if Key = '' then
    Key := VarToStr(dmGeneral.RunInsertStoredProc(TN_CONSERVATION_CHECK,
                                  'usp_ConditionCheck_Insert', lParams, '@Key'))
  else
    dmGeneral.RunUpdateStoredProc('usp_ConditionCheck_Update', lParams);
end;  // TfraConditionGeneral.SaveData 

{-------------------------------------------------------------------------------
}
procedure TfraConditionGeneral.UpdateCheckedBy(const AKeyList: IKeyList);
begin
  UpdateIndividualNameControl(eCheckedBy, AKeyList.GetKeyItem(0).KeyField1,
      Format(ResStr_MustBeIndividual, [LopColon(lblCheckedBy.Caption)]));
end;  // TfraConditionGeneral.UpdateCheckedBy

{-------------------------------------------------------------------------------
  Additional validation for the contents of the frame. 
}
procedure TfraConditionGeneral.ValidateData;
begin
  inherited;
  if eCheckedBy.Key <> '' then
    ValidateValue(CheckIsIndividual(eCheckedBy.Key),
                  Format(ResStr_MustBeIndividual, [LopColon(lblCheckedBy.Caption)]));
end;  // TfraConditionGeneral.ValidateData 

end.




