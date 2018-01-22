{===============================================================================
  Unit:        FrameMaterialMovement.pas

  Defines:     TfraMaterialMovement

  Description:

  Model:       CollectionsMovements

  Created:     October 2003

  Last revision information:
    $Revision: 29 $
    $Date: 3/08/04 14:59 $
    $Author: Ericsalmon $

===============================================================================}

unit FrameMaterialMovement;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, BaseMaterialMovementDetailsFrameUnit, StdCtrls, InterfaceDataModule,
  ImageListButton, ExtCtrls, ComboListID, LuxIDComboBox, ConceptGroupComboBox,
  VagueDateEdit, ExceptionForm, BaseCompositeComponent, Validation,
  LinkedControls, DataTypes, DataClasses, DropTarget, BaseTabSheetFrameUnit,
  Recorder2000_TLB;

type
  {-----------------------------------------------------------------------------
    Tab page control for the details of a movement of materials (acquisition or disposal) 
    where the destination of the material is known.
    This tab control is inherited from TBaseMaterialMovementScreen and includes the components 
    documented on that base class.  The documentation here details the additional 
    functionality in this tab page only.
  }
  TfraMaterialMovementDetails = class (TBaseMaterialMovementFrame)
    cmbAcquisitionMethod: TConceptGroupComboBox;
    cmbCurrency: TConceptGroupComboBox;
    eAmount: TEdit;
    eDepartment: TLinkedEdit;
    Label7: TLabel;
    Label9: TLabel;
    lblAcquisitionMethod: TLabel;
    lblDepartment: TLabel;
  private
    FMovementOutbound: Boolean;
    FMovementType: Integer;
    procedure DropDepartment(const Sender: TObject; const AFormat: Integer; const ASourceData:
        TKeyList; const ATextStrings: TStringList; var AHandled: Boolean);
    function GetHoldingOrgName: String;
    function IsMemberOfHoldingOrg(AKey: TKeyString): Boolean;
  protected
    procedure DeleteData; override;
    procedure RegisterControls; override;
    procedure ValidateData; override;
  public
    procedure DoRegisterControls; override;
    function GetCaption: String; override;
    procedure LoadData; override;
    procedure RegisterDragDropComponents; override;
    procedure SaveData; override;
  end;
  
//==============================================================================
implementation

uses
  LuxembourgConstants, GeneralData, ResourceStrings, BaseDetailFrameUnit,
  GeneralFunctions;

{$R *.dfm}

{-==============================================================================
    TfraMaterialMovementDetails
===============================================================================}
{-------------------------------------------------------------------------------
  Run the delete proc. for this frame.
}
procedure TfraMaterialMovementDetails.DeleteData;
begin
  dmGeneral.RunDeleteStoredProc('usp_MovementMaterialDetail_Delete',
                               ['@Key', Key, '@Timestamp', FTimestamp]);
end;  // TfraMaterialMovementDetails.DeleteData

{-------------------------------------------------------------------------------
  Register the correct recordset.
}
procedure TfraMaterialMovementDetails.DoRegisterControls;
begin
  // Register recordsets used
  RegisterRecordset('usp_MovementMaterialDetail_Select');
end;  // TfraMaterialMovementDetails.DoRegisterControls

{-------------------------------------------------------------------------------
  Handle the dropping of a department on the eDepartment LinkedEdit.
}
procedure TfraMaterialMovementDetails.DropDepartment(const Sender: TObject; const AFormat:
    Integer; const ASourceData: TKeyList; const ATextStrings: TStringList; var AHandled:
    Boolean);
begin
  DropLinkedEditDataMultiple(AFormat, ASourceData, AHandled, eDepartment,
      [TN_INDIVIDUAL, 'usp_FormattedNameForNameKey_Get', '@NameKey', '@FormattedName',
       TN_ORGANISATION, 'usp_FormattedNameForNameKey_Get', '@NameKey', '@FormattedName',
       TN_ORGANISATION_DEPARTMENT, 'usp_DepartmentName_Get', '@NameKey', '@FormattedName']);
end;  // TfraMaterialMovementDetails.DropDepartment

{-------------------------------------------------------------------------------
  Get the caption.
}
function TfraMaterialMovementDetails.GetCaption: String;
begin
  if FMovementType = 2 then
    if FMovementOutbound then Result := ResStr_MaterialReturned
                         else Result := ResStr_MaterialLoaned
  else if FMovementType = 3 then
    if FMovementOutbound then Result := ResStr_MaterialLoaned
                         else Result := ResStr_MaterialReturned
  else if FMovementOutbound then Result := ResStr_DisposalDetails
                            else Result := ResStr_AcquisitionDetails;
  Result := Result + ' - ' + eDate.text;
end;  // TfraMaterialMovementDetails.GetCaption 

{-------------------------------------------------------------------------------
  Get the name of the holding organisation from the database for the validation message. 
}
function TfraMaterialMovementDetails.GetHoldingOrgName: String;
begin
  Result := dmGeneral.GetStoredProcOutputParam('usp_HoldingOrgName_Get',
                                                [],
                                                '@HoldingOrg');
end;  // TfraMaterialMovementDetails.GetHoldingOrgName 

{-------------------------------------------------------------------------------
  Takes a name key and returns true or false depending on whether they are a member of the 
      holding organisation or not. 
}
function TfraMaterialMovementDetails.IsMemberOfHoldingOrg(AKey: TKeyString): Boolean;
begin
  Result := (dmGeneral.GetStoredProcOutputParam('usp_MemberOfHoldingOrg_Get',
                                                ['@Key', AKey],
                                                '@IsMember') <> 0);
end;  // TfraMaterialMovementDetails.IsMemberOfHoldingOrg 

{-------------------------------------------------------------------------------
  For movements in an outward direction, the associated label's caption is changed to 
      'Sent To:'. There are two overlapping labels on the frame to do this. However, only one 
      is visible at a time. The reason for doing it this way, rather than with two resource
      strings is that 'Received By:' goes over two lines, whereas 'Sent To:' only takes one
      line - and hence it looks incorrectly spaced if placed in the same place as the former.
}
procedure TfraMaterialMovementDetails.LoadData;
begin
  inherited LoadData;
  FMovementType := AdditionalProperties.GetProperty(PROP_MOVEMENT_TYPE);
  FMovementOutbound := StrToBool(AdditionalProperties.GetProperty(PROP_MOVEMENT_OUTBOUND));
  if FMovementOutbound then lblDepartment.Caption := ResStr_SentTo + ':'
                       else lblDepartment.Caption := ResStr_ReceivedBy + ':';
  cmbAcquisitionMethod.Visible := not ((FMovementType in [2, 3]) or FMovementOutbound);
  lblAcquisitionMethod.Visible := cmbAcquisitionMethod.Visible;
end;  // TfraMaterialMovementDetails.LoadData 

{-------------------------------------------------------------------------------
  Register the controls. 
}
procedure TfraMaterialMovementDetails.RegisterControls;
begin
  inherited RegisterControls;
  
  RegisterControl(eDepartment, 'Receiver_Display', 'Receiver_Key', True,
                  ResStr_ReceivedBy, CheckLinkedDeptOrgName,
                  'Name', ConvertNameKeyToCaption);
  RegisterControl(eAmount, 'Value_Amount', ResStr_Amount);
  RegisterControl(cmbCurrency, 'Currency_Name', 'Currency_Concept_Key');
  RegisterConceptGroupComboBox(cmbCurrency, CG_CURRENCIES);
  RegisterControl(cmbAcquisitionMethod, 'Acquisition_Method_Name',
                                            'Acquisition_Method_Concept_Key');
  RegisterConceptGroupComboBox(cmbAcquisitionMethod, CG_ACQUISITION_METHODS);
end;  // TfraMaterialMovementDetails.RegisterControls 

{-------------------------------------------------------------------------------
  Register the drag / drop controls. 
}
procedure TfraMaterialMovementDetails.RegisterDragDropComponents;
begin
  inherited RegisterDragDropComponents;

  RegisterDropComponent(eDepartment, DropDepartment,
                        [TN_NAME, TN_INDIVIDUAL, TN_ORGANISATION, TN_ORGANISATION_DEPARTMENT],
                        [CF_JNCCDATA]);
end;  // TfraMaterialMovementDetails.RegisterDragDropComponents 

{-------------------------------------------------------------------------------
  Save the frame's data to the database. 
}
procedure TfraMaterialMovementDetails.SaveData;
var
  lParams: Array Of Variant;
begin
  inherited;    // For validation to be called.
  lParams := VarArrayOf(['@Key', Key,
                         '@ParentKey', ParentKey,
                         '@ContactNameKey', eContact.Key,
                         '@VagueDateStart', eDate.VagueDate.StartDate,
                         '@VagueDateEnd', eDate.VagueDate.EndDate,
                         '@VagueDateType', eDate.VagueDate.DateTypeString,
                         '@Completed',chkCompleted.checked,
                         '@ReceiverNameKey', eDepartment.Key,
                         '@ValueAmount', eAmount.Text,
                         '@CurrencyConceptKey', cmbCurrency.CurrentStrID,
                         '@AcquisitionMethodConceptKey', cmbAcquisitionMethod.CurrentStrID,
                         '@Notes', mmNotes.Text,
                         '@Outbound', FOutbound,
                         '@Timestamp', FTimestamp
                         ]);
   // Empty key means new record.
  if Key = '' then
    Key := VarToStr(dmGeneral.RunInsertStoredProc(TN_MOVEMENT_OF_MATERIAL,
                                                  'usp_MovementMaterialDetail_Insert',
                                                  lParams, '@Key'))
  else
    dmGeneral.RunUpdateStoredProc('usp_MovementMaterialDetail_Update', lParams);
end;  // TfraMaterialMovementDetails.SaveData 

{-------------------------------------------------------------------------------
  If the movement is inbound, the name entered in ther received by field must either be the 
      holding organisation or a member of the holding organisation.
  
  If the movement is an Internal Transfer, the name in the contact field must be a member of 
      the holding organisation. 
}
procedure TfraMaterialMovementDetails.ValidateData;
begin
  inherited;
  if not FMovementOutbound then
    ValidateValue(IsMemberOfHoldingOrg(eDepartment.Key),
                      Format(ResStr_MustBeMemberOfOrg,
                          [lblDepartment.Caption, GetHoldingOrgName]),
                      eDepartment)
  else
    ValidateValue(not IsMemberOfHoldingOrg(eDepartment.Key),
                      Format(ResStr_MustNotBeMemberOfOrg,
                          [lblDepartment.Caption, GetHoldingOrgName]),
                      eDepartment);
  
  if eContact.Key <> '' then begin
    ValidateValue(CheckIsIndividual(eContact.Key),
                            Format(ResStr_MustBeIndividual, [ResStr_Contact]),
                            eContact);
    if FMovementType = 6 then begin
      ValidateValue(IsMemberOfHoldingOrg(eContact.Key),
                      Format(ResStr_MustBeMemberOfOrg,
                          [ResStr_Contact, GetHoldingOrgName]),
                      eContact);
    end;
  end;
  if eAmount.Text <> '' then
    ValidateValue(IsFloat(eAmount.Text), Format(ResStr_MustBeANumber, [ResStr_Amount]),
                                   eAmount);
end;  // TfraMaterialMovementDetails.ValidateData 

end.

