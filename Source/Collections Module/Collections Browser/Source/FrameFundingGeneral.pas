{===============================================================================
  Unit:        FrameJobFunding.pas

  Defines:     TfraJobFunding

  Description:

  Created:     May 2003

  Last revision information:
    $Revision: 10 $
    $Date: 16/10/12 11:27 $
    $Author: Alexanderpadley $

===============================================================================}

unit FrameFundingGeneral;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ImageListButton, Grids, DataClasses, DropTarget,
  ExtCtrls, BaseTabSheetFrameUnit, BaseDetailFrameUnit, ComboListID,
  LuxIDComboBox, ConceptGroupComboBox, BaseCompositeComponent,
  LinkedControls, VagueDateEdit, DataTypes, ResourceStrings, Validation,
  RestrictedEdits, UserEdit;

type
  {-----------------------------------------------------------------------------
    General details tab page for the main aspects of job funding.  This is embedded onto 
    TfraFunding when required.
  }
  TfraFundingGeneral = class (TBaseTabSheetFrame)
    cmbCurrency: TConceptGroupComboBox;
    eDate: TVagueDateEdit;
    eName: TUserEdit;
    Label10: TLabel;
    Label3: TLabel;
    Label5: TLabel;
    Label7: TLabel;
    Label9: TLabel;
    mmDetails: TMemo;
    eAmount: TNumberEdit;
  private
    FIsMovement: Boolean;
    FTimestamp: TSQLSvrTimestamp;
    procedure DropName(const Sender: TObject; const AFormat: Integer; const ASourceData: 
        TKeyList; const ATextStrings: TStringList; var AHandled: Boolean);
    function GetIsMovementParam: TVariantArray;
  protected
    procedure DeleteData; override;
    function GetCaption: String; override;
    procedure LoadData; override;
    procedure RegisterControls; override;
    procedure RegisterDragDropComponents; override;
    procedure SaveData; override;
  public
    property IsMovement: Boolean read FIsMovement write FIsMovement;
  end;
  
//==============================================================================
implementation

{$R *.dfm}

uses InterfaceDataModule, LuxembourgConstants, GeneralData;

{-==============================================================================
    TfraFundingGeneral
===============================================================================}
{-------------------------------------------------------------------------------
  Run the delete stored proc. for this frame. 
}
procedure TfraFundingGeneral.DeleteData;
begin
  dmGeneral.RunDeleteStoredProc('usp_Funding_Delete',
                               ['@Key', Key,
                                '@IsMovement', FIsMovement,
                                '@Timestamp', FTimestamp]);
end;  // TfraFundingGeneral.DeleteData 

{-------------------------------------------------------------------------------
  Handle dropping of NAME data onto eName. 
}
procedure TfraFundingGeneral.DropName(const Sender: TObject; const AFormat: Integer; 
    const ASourceData: TKeyList; const ATextStrings: TStringList; var AHandled: Boolean);
begin
  DropLinkedEditData(AFormat, ASourceData, AHandled, eName, TN_INDIVIDUAL,
                 'usp_FormattedNameForNameKey_Get', '@NameKey', '@FormattedName');
end;  // TfraFundingGeneral.DropName 

{-------------------------------------------------------------------------------
  Gets the caption of the frame. 
}
function TfraFundingGeneral.GetCaption: String;
begin
  Result := eName.Text;
end;  // TfraFundingGeneral.GetCaption 

{-------------------------------------------------------------------------------
}
function TfraFundingGeneral.GetIsMovementParam: TVariantArray;
begin
  Result := VarArrayOf(['@Key', Key,
                        '@IsMovement', FIsMovement]);
end;  // TfraFundingGeneral.GetIsMovementParam 

{-------------------------------------------------------------------------------
  Loads any extra required data into the frame. 
}
procedure TfraFundingGeneral.LoadData;
begin
  inherited LoadData;
  
  if not RegisteredRecordsets[0].Eof then
    FTimestamp := RegisteredRecordsets[0].Fields['Timestamp'].Value;
end;  // TfraFundingGeneral.LoadData 

{-------------------------------------------------------------------------------
  Registers the controls used in the frame. 
}
procedure TfraFundingGeneral.RegisterControls;
begin
  inherited RegisterControls;
  
  RegisterRecordSet('usp_Funding_Select',GetIsMovementParam);
  RegisterControl(eName,'Funded_By','Funded_By_Name_Key', True, ResStr_FundedBy,
        CheckLinkedIndividual, 'Name', ConvertIndividualKeyToCaption);
  RegisterControl(eDate,'',True,ResStr_Date);
  RegisterControl(eAmount,'Amount',True,ResStr_Amount);
  RegisterControl(cmbCurrency,'Currency','Currency_Concept_Key', True, ResStr_Currency);
  RegisterConceptGroupComboBox(cmbCurrency,CG_CURRENCIES);
  RegisterControl(mmDetails,'Details');
end;  // TfraFundingGeneral.RegisterControls 

{-------------------------------------------------------------------------------
  Register the drag drop components. 
}
procedure TfraFundingGeneral.RegisterDragDropComponents;
begin
  RegisterDropComponent(eName, DropName, [TN_NAME, TN_INDIVIDUAL, TN_ORGANISATION],
      [CF_JNCCDATA]);
end;  // TfraFundingGeneral.RegisterDragDropComponents 

{-------------------------------------------------------------------------------
  Saves\Updates data in the tables using the frame data. 
}
procedure TfraFundingGeneral.SaveData;
var
  lParams: Array of Variant;
begin
  lParams := VarArrayOf(['@Key', Key,
                           '@ParentKey', ParentKey,
                           '@FundedByNameKey',eName.Key,
                           '@VagueDateStart',eDate.VagueDate.StartDate,
                           '@VagueDateEnd',eDate.VagueDate.EndDate,
                           '@VagueDateType',eDate.VagueDate.DateTypeString,
                           '@Amount',eAmount.Text,
                           '@CurrencyConceptKey',cmbCurrency.CurrentStrID,
                           '@Details',mmDetails.Text,
                           '@Timestamp',FTimestamp,
                           '@IsMovement',FIsMovement]);
  // If inserting a record
  if Key = '' then
    begin
      if FIsMovement = True then
       Key := VarToStr(dmGeneral.RunInsertStoredProc(TN_MOVEMENT_FUNDING,
                                                      'usp_Funding_Insert',
                                                      lParams,
                                                      '@Key'))
      else
       Key := VarToStr(dmGeneral.RunInsertStoredProc(TN_CONSERVATION_JOB_FUNDING,
                                                      'usp_Funding_Insert',
                                                      lParams,
                                                      '@Key'));
    end
  // Else if updating a record
  else
    dmGeneral.RunUpdateStoredProc('usp_Funding_Update', lParams);
end;  // TfraFundingGeneral.SaveData 

end.











