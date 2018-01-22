{===============================================================================
  Unit:        FrameTaskGeneral.pas

  Defines:     TfraTaskGeneral

  Description:

  Created:     June 2003

  Last revision information:
    $Revision: 25 $
    $Date: 16/10/12 11:27 $
    $Author: Alexanderpadley $

===============================================================================}
unit FrameTaskGeneral;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, BaseTabSheetFrameUnit, StdCtrls, ImageListButton, ExtCtrls,
  ComCtrls, DataTypes, DropTarget, DataClasses, BaseCompositeComponent,
  LinkedControls, VagueDateEdit, LuxIDComboBox, ConceptGroupComboBox, ExceptionForm,
  LuxembourgConstants, ResourceStrings, InterfaceDataModule, GeneralFunctions,
  ComboListID, DSSDataTypes, Recorder2000_TLB, UserEdit;

type
  {-----------------------------------------------------------------------------
    General details tab page for the main aspects of tasks.  This is embedded onto 
    TfraTask when required.
  }
  TfraTaskGeneral = class (TBaseTabSheetFrame)
    cmbDuration: TConceptGroupComboBox;
    cmbPriority: TLuxIDComboBox;
    cmbStatus: TLuxIDComboBox;
    cmbType: TConceptGroupComboBox;
    eDateSet: TVagueDateEdit;
    eDuration: TEdit;
    eIdentifiedBy: TUserEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    lblIdentifiedBy: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    mmAction: TMemo;
    mmComments: TMemo;
    udDuration: TUpDown;
    procedure cmbPriorityPopulate(Sender: TObject);
    procedure cmbStatusPopulate(Sender: TObject);
    procedure eIdentifiedByGetData(Sender: TObject);
    procedure udDurationClick(Sender: TObject; Button: TUDBtnType);
  private
    FConservationCheckKey: Variant;
    FConservationJobKey: Variant;
    FTimestamp: TSQLSvrTimestamp;
    procedure DropIdentifiedBy(const Sender: TObject; const AFormat: Integer; const
        ASourceData: TKeyList; const ATextStrings: TStringList; var AHandled: Boolean);
    procedure UpdateIdentifiedBy(const AKeyList: IKeyList);
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

uses
  Validation, GeneralData;

{$R *.dfm}

{-==============================================================================
    TfraTaskGeneral
===============================================================================}
{-------------------------------------------------------------------------------
  Populate the combo box. 
}
procedure TfraTaskGeneral.cmbPriorityPopulate(Sender: TObject);
begin
  cmbPriority.Add(ResStr_Low, 0);
  cmbPriority.Add(ResStr_Medium, 1);
  cmbPriority.Add(ResStr_High, 2);
  cmbPriority.Add(ResStr_Urgent, 3);
end;  // TfraTaskGeneral.cmbPriorityPopulate 

{-------------------------------------------------------------------------------
  Populate the Status combo box. 
}
procedure TfraTaskGeneral.cmbStatusPopulate(Sender: TObject);
begin
  cmbStatus.Add(ResStr_Pending, 0);
  cmbStatus.Add(ResStr_Open, 1);
  cmbStatus.Add(ResStr_Closed, 2);
  cmbStatus.Add(ResStr_Postponed, 3);
  cmbStatus.Add(ResStr_Abandoned, 4);
end;  // TfraTaskGeneral.cmbStatusPopulate 

{-------------------------------------------------------------------------------
  Run the delete stored proc. for this frame. 
}
procedure TfraTaskGeneral.DeleteData;
begin
  dmGeneral.RunDeleteStoredProc('usp_Task_Delete',
                               ['@Key', Key, '@Timestamp', FTimestamp]);
end;  // TfraTaskGeneral.DeleteData 

{-------------------------------------------------------------------------------
  Handle the drag drop onto the eIdentifiedBy linked edit. 
}
procedure TfraTaskGeneral.DropIdentifiedBy(const Sender: TObject; const AFormat: Integer; 
    const ASourceData: TKeyList; const ATextStrings: TStringList; var AHandled: Boolean);
begin
  DropLinkedEditData(AFormat, ASourceData, AHandled, eIdentifiedBy, TN_INDIVIDUAL,
                     'usp_FormattedNameForNameKey_Get', '@NameKey', '@FormattedName');
end;  // TfraTaskGeneral.DropIdentifiedBy 

{-------------------------------------------------------------------------------
}
procedure TfraTaskGeneral.eIdentifiedByGetData(Sender: TObject);
begin
  inherited;
  InitReturnData(UpdateIdentifiedBy, TN_NAME);
end;  // TfraTaskGeneral.eIdentifiedByGetData

{-------------------------------------------------------------------------------
  Gets the caption for the node names.
}
function TfraTaskGeneral.GetCaption: String;
begin
  Result := eDateSet.Text + ' - ' + cmbStatus.Text;
end;  // TfraTaskGeneral.GetCaption 

{-------------------------------------------------------------------------------
  Load data into variables. 
}
procedure TfraTaskGeneral.LoadData;
begin
  inherited LoadData;
  
  if not RegisteredRecordsets[0].Eof then begin
     FTimestamp :=
       RegisteredRecordsets[0].Fields['Timestamp'].Value;
     FConservationCheckKey :=
       RegisteredRecordsets[0].Fields['Conservation_Check_Key'].Value;
     FConservationJobKey :=
       RegisteredRecordsets[0].Fields['Conservation_Job_Key'].Value;
  end else
    // Defaults to the person who performed the condition check.
    with dmGeneral.GetRecordset('usp_ConditionCheck_Select', ['@Key', ParentKey]) do begin
      eIdentifiedBy.Key := VarToStr(Fields['Checked_By_Name_Key'].Value);
      eIdentifiedBy.Text := VarToStr(Fields['Checked_By_Name'].Value);
    end;
end;  // TfraTaskGeneral.LoadData 

{-------------------------------------------------------------------------------
  Register the controls and datasets.   
}
procedure TfraTaskGeneral.RegisterControls;
begin
  inherited;
   // Register recordsets used
  RegisterRecordset('usp_Task_Select');
   // Register controls getting their value straight from the registered recordsets.
  RegisterControl(eDateSet, 'Set', True, ResStr_DateSet);
  RegisterControl(cmbStatus, '', 'Status', True, ResStr_Status);
  RegisterControl(cmbType, 'Type_Item_Name', 'Type_Concept_Key', True, ResStr_Type);
  RegisterConceptGroupComboBox(cmbType, CG_CONSERVATION_TASK_TYPES);
  RegisterControl(cmbPriority, '', 'Priority', True, ResStr_Priority);
  RegisterControl(eDuration, ResStr_Duration);
  RegisterControl(cmbDuration, 'Duration_Item_Name', 'Duration_Unit_Concept_Key');
  RegisterConceptGroupComboBox(cmbDuration, CG_TIME_UNITS);
  RegisterControl(eIdentifiedBy, 'Identifier_Name', 'Identifier_Name_Key',
                  True, ResStr_IdentifiedBy, CheckLinkedIndividual, 'Name',
                  ConvertIndividualKeyToCaption);
  eIdentifiedBy.OnGetData := eIdentifiedByGetData;

  RegisterControl(mmAction, 'Task_Action');
  RegisterControl(mmComments, 'Comment', 'Comments');
end;  // TfraTaskGeneral.RegisterControls

{-------------------------------------------------------------------------------
  Register controls for drag and drop.
}
procedure TfraTaskGeneral.RegisterDragDropComponents;
begin
  RegisterDropComponent(eIdentifiedBy, DropIdentifiedBy, [TN_NAME, TN_INDIVIDUAL],
                        [CF_JNCCDATA]);
end;  // TfraTaskGeneral.RegisterDragDropComponents 

{-------------------------------------------------------------------------------
  Save the data to the database. 
}
procedure TfraTaskGeneral.SaveData;
var
  lParams: Array Of Variant;
begin
  if Key = '' then
    FConservationCheckKey := ParentKey;
  
  lParams := VarArrayOf(['@Key', Key,
                         '@ConservationCheckKey', FConservationCheckKey,
                         '@ConservationJobKey', FConservationJobKey,
                         '@SetVagueDateStart', eDateSet.VagueDate.StartDate,
                         '@SetVagueDateEnd', eDateSet.VagueDate.EndDate,
                         '@SetVagueDateType', eDateSet.VagueDate.DateTypeString,
                         '@Status', cmbStatus.CurrentIntID,
                         '@TypeConceptKey', cmbType.CurrentStrID,
                         '@Priority', cmbPriority.CurrentIntID,
                         '@Duration', eDuration.Text,
                         '@DurationUnitConceptKey', cmbDuration.CurrentStrID,
                         '@IdentifierNameKey', eIdentifiedBy.Key,
                         '@TaskAction', mmAction.Text,
                         '@Comment', mmComments.Text,
                         '@Timestamp', FTimestamp
                        ]);
  // Empty key means new record.
  if Key = '' then
    Key := VarToStr(dmGeneral.RunInsertStoredProc(TN_CONSERVATION_TASK,
                                                  'usp_Task_Insert', lParams, '@Key'))
  else
    dmGeneral.RunUpdateStoredProc('usp_Task_Update', lParams);
end;  // TfraTaskGeneral.SaveData 

{-------------------------------------------------------------------------------
  Can't use the 'Associate' property of udDuration because then it can't cope with 
      floating point numbers. Hence, the OnClick event handler does the 
      addition/subtraction manually here. 
}
procedure TfraTaskGeneral.udDurationClick(Sender: TObject; Button: TUDBtnType);
begin
  inherited;
  if EditMode = emEdit then begin
    if eDuration.Text = '' then eDuration.Text := '0'
    else if IsFloat(eDuration.Text) then begin
      if StrToFloat(eDuration.Text) < 0 then eDuration.Text := '0'
      else begin
        if Button = btNext then
          eDuration.Text := FloatToStr(StrToFloat(eDuration.Text) + 1)
        else if StrToFloat(eDuration.Text) - 1 >= 0 then
          eDuration.Text := FloatToStr(StrToFloat(eDuration.Text) - 1);
      end;
    end;
  end;
end;  // TfraTaskGeneral.udDurationClick

{-------------------------------------------------------------------------------
}
procedure TfraTaskGeneral.UpdateIdentifiedBy(const AKeyList: IKeyList);
begin
  UpdateIndividualNameControl(eIdentifiedBy, AKeyList.GetKeyItem(0).KeyField1,
      Format(ResStr_MustBeIndividual, [LopColon(lblIdentifiedBy.Caption)]));
end;  // TfraTaskGeneral.UpdateIdentifiedBy

{-------------------------------------------------------------------------------
  Additional validation for fields on the frame. 
}
procedure TfraTaskGeneral.ValidateData;
begin
  inherited;
  if eDuration.Text <> '' then begin
    ValidateValue(IsFloat(eDuration.Text), ResStr_DurationMustBeNumber, eDuration);
    ValidateValue(StrToFloat(eDuration.Text) >= 0,
                        Format(ResStr_MustBeNonNegative, [ResStr_Duration]),
                        eDuration);
    ValidateValue(cmbDuration.ItemIndex <> -1,
                        Format(ResStr_ValueRequiresUnit, [ResStr_Duration]),
                        eDuration);
  end else
    cmbDuration.ItemIndex := -1;
end;  // TfraTaskGeneral.ValidateData 

end.
