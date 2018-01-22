{===============================================================================
  Unit:        FrameValuationGeneral.pas

  Defines:     TfraValuationGeneral

  Description:

  Model:       CollectionsCommonAndGeneral.mpb

  Created:     May 2003

  Last revision information:
    $Revision: 13 $
    $Date: 16/10/12 11:27 $
    $Author: Alexanderpadley $

===============================================================================}

unit FrameValuationGeneral;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, BaseTabSheetFrameUnit, StdCtrls, ImageListButton, ExtCtrls,
  DataTypes, DataClasses, VagueDateEdit, InterfaceDataModule, VagueDate,
  BaseCompositeComponent, LinkedControls, ComboListID, LuxIDComboBox,
  ConceptGroupComboBox, GeneralData, ResourceStrings, RestrictedEdits,
  ExceptionForm, UserEdit;

type
  {-----------------------------------------------------------------------------
    General tab page control for details of a selected valuation.
  }
  TfraValuationGeneral = class (TBaseTabSheetFrame)
    cmbCurrency: TConceptGroupComboBox;
    cmbType: TConceptGroupComboBox;
    eDate: TVagueDateEdit;
    eDateFrom: TVagueDateEdit;
    eDateTo: TVagueDateEdit;
    eName: TUserEdit;
    eRefNumber: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    mmDescription: TMemo;
    eValue: TNumberEdit;
  private
    FTimestamp: TSQLSvrTimestamp;
    procedure DropName(const Sender: TObject; const AFormat: Integer; const ASourceData: 
        TKeyList; const ATextStrings: TStringList; var AHandled: Boolean);
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
  LuxembourgConstants, Validation, DropTarget;

{-==============================================================================
    TfraValuationGeneral
===============================================================================}
{-------------------------------------------------------------------------------
  Run the delete stored proc. for this frame. 
}
procedure TfraValuationGeneral.DeleteData;
begin
  dmGeneral.RunDeleteStoredProc('usp_Valuation_Delete',
                                ['@Key', Key, '@Timestamp', FTimestamp]);
end;  // TfraValuationGeneral.DeleteData 

{-------------------------------------------------------------------------------
  Handle dropping of NAME data onto eName. 
}
procedure TfraValuationGeneral.DropName(const Sender: TObject; const AFormat: Integer; const 
    ASourceData: TKeyList; const ATextStrings: TStringList; var AHandled: Boolean);
begin
  DropLinkedEditDataMultiple(AFormat, ASourceData, AHandled, eName,
      [TN_INDIVIDUAL, 'usp_FormattedNameForNameKey_Get', '@NameKey', '@FormattedName',
       TN_ORGANISATION, 'usp_FormattedNameForNameKey_Get', '@NameKey', '@FormattedName']);
end;  // TfraValuationGeneral.DropName

{-------------------------------------------------------------------------------
  Return date + ' - ' + type as the caption. 
}
function TfraValuationGeneral.GetCaption: String;
begin
  Result := eDate.Text;
  if cmbType.Text<>'' then Result := Result + ' - ' + cmbType.Text;
end;  // TfraValuationGeneral.GetCaption 

{-------------------------------------------------------------------------------
  Store the timestamp required for updates. 
}
procedure TfraValuationGeneral.LoadData;
begin
  inherited;
  if not RegisteredRecordsets[0].Eof then
    FTimestamp := RegisteredRecordsets[0].Fields['Timestamp'].Value;
end;  // TfraValuationGeneral.LoadData

{-------------------------------------------------------------------------------
  Register the controls against the recordsets.
}
procedure TfraValuationGeneral.RegisterControls;
begin
  inherited;
  // Register recordsets used
  RegisterRecordset('usp_Valuation_Select');

  // Register controls getting their value straight from the registered recordsets.
  RegisterControl(eDate, '', True, ResStr_Date);
  RegisterControl(eRefNumber, 'Ref_Number');
  RegisterControl(cmbType, 'TypeTerm', 'Type_Concept_Key');
  RegisterConceptGroupComboBox(cmbType, CG_VALUATION_TYPES);
  RegisterControl(eName, 'ValuedBy', 'Valued_By_Name_Key', True, ResStr_ValuedBy,
                  CheckLinkedName, 'Name', ConvertNameKeyToCaption);
  RegisterControl(eValue, 'Value_Amount', True, ResStr_Value);
  RegisterControl(cmbCurrency, 'CurrencyTerm', 'Currency_Concept_Key', True,
                  ResStr_Currency);
  RegisterConceptGroupComboBox(cmbCurrency, CG_CURRENCIES);
  RegisterControl(eDateFrom, 'Valid_From');
  RegisterControl(eDateTo, 'Valid_To');
  RegisterControl(mmDescription, 'Description');
end;  // TfraValuationGeneral.RegisterControls

{-------------------------------------------------------------------------------
  Register eName to receive dropped Individual or Organisation data.
}
procedure TfraValuationGeneral.RegisterDragDropComponents;
begin
  RegisterDropComponent(eName, DropName, [TN_NAME, TN_INDIVIDUAL, TN_ORGANISATION],
                        [CF_JNCCDATA]);
end;  // TfraValuationGeneral.RegisterDragDropComponents 

{-------------------------------------------------------------------------------
  Save the frame to the database. 
}
procedure TfraValuationGeneral.SaveData;
var
  lParams: Array of Variant;
begin
  lParams := VarArrayOf(['@Key', Key,
                         '@ParentKey', ParentKey,
                         '@VagueDateStart', eDate.VagueDate.StartDate,
                         '@VagueDateEnd', eDate.VagueDate.EndDate,
                         '@VagueDateType', eDate.VagueDate.DateTypeString,
                         '@RefNumber', eRefNumber.Text,
                         '@TypeConceptKey', cmbType.CurrentStrID,
                         '@ValuedByNameKey', eName.Key,
                         '@ValueAmount', eValue.Text,
                         '@CurrencyConceptKey', cmbCurrency.CurrentStrID,
                         '@ValidFromVagueDateStart', eDateFrom.VagueDate.StartDate,
                         '@ValidFromVagueDateEnd', eDateFrom.VagueDate.EndDate,
                         '@ValidFromVagueDateType', eDateFrom.VagueDate.DateTypeString,
                         '@ValidToVagueDateStart', eDateTo.VagueDate.StartDate,
                         '@ValidToVagueDateEnd', eDateTo.VagueDate.EndDate,
                         '@ValidToVagueDateType', eDateTo.VagueDate.DateTypeString,
                         '@Description', mmDescription.Text,
                         '@Timestamp', FTimestamp
                         ]);
  
  // Empty key means new record.
  if Key = '' then
    Key := VarToStr(dmGeneral.RunInsertStoredProc(TN_VALUATION, 'usp_Valuation_Insert',
                                                  lParams, '@Key'))
  else
    dmGeneral.RunUpdateStoredProc('usp_Valuation_Update', lParams);
end;  // TfraValuationGeneral.SaveData

{-------------------------------------------------------------------------------
}
procedure TfraValuationGeneral.ValidateData;
var
  lCurrentDate: TVagueDate;
begin
  inherited;
  lCurrentDate := StringToVagueDate(DateToStr(Date));

  ValidateValue(CompareVagueDateToVagueDate(lCurrentDate, eDate.VagueDate) <> -1,
                Format(ResStr_DateCannotBeInFuture, [ResStr_EnquiredDate]), eDate);

  ValidateValue(eValue.Text <> '.', ResStr_EnterAValidValuation, eValue);
end;  // TfraEnquiryGeneral.ValidateData

end.

