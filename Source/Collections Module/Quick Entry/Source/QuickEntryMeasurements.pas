{===============================================================================
  Unit:        QuickEntryMeasurements.pas

  Defines:     TfrmQuickEntryMeasurements

  Description: Form holder for the FrameMeasurementsGeneral frame.
               Returns details of measurements to the Quick Entry Manager
                   screen.

  Model:       QuickEntry.mpb

  Created:     August 2003

  Last revision information:
    $Revision: 7 $
    $Date: 18/11/04 14:35 $
    $Author: Ericsalmon $

===============================================================================}
unit QuickEntryMeasurements;

interface

uses Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, ExtCtrls,
  StdCtrls, Buttons, InterfaceDataModule, ImageListButton, ComboListID, BaseDragFrameUnit,
  BaseDetailFrameUnit, BaseTabSheetFrameUnit, LuxIDComboBox, ConceptGroupComboBox;

type
  TfrmQuickEntryMeasurements = class(TForm)
    btnCancel: TImageListButton;
    btnOK: TImageListButton;
    lblInstruction: TLabel;
    rgMeasurementRelatesTo: TRadioGroup;
    pnlCollectionData: TPanel;
    lblAppliesTo: TLabel;
    eAppliesTo: TEdit;
    lblAccuracy: TLabel;
    eAccuracy: TEdit;
    lblParameter: TLabel;
    cmbParameter: TConceptGroupComboBox;
    lblUnit: TLabel;
    cmbUnit: TConceptGroupComboBox;
    pnlTaxonData: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    eTxAccuracy: TEdit;
    cmbTxType: TIDComboBox;
    cmbTxQualifier: TIDComboBox;
    cmbTxUnit: TIDComboBox;
    cmbMethod: TConceptGroupComboBox;
    Label5: TLabel;
    lblDuration: TLabel;
    eDuration: TEdit;
    procedure btnOKClick(Sender: TObject);
    procedure rgMeasurementRelatesToClick(Sender: TObject);
    procedure cmbParameterPopulate(Sender: TObject);
    procedure cmbMethodPopulate(Sender: TObject);
    procedure cmbUnitPopulate(Sender: TObject);
    procedure cmbTxTypePopulate(Sender: TObject);
    procedure cmbTxQualifierPopulate(Sender: TObject);
    procedure cmbTxUnitPopulate(Sender: TObject);
    procedure cmbTxTypeChange(Sender: TObject);
  private
    FParameterKey: String;
    FTaxonData: Boolean;
    procedure ValidateData;
    procedure PopulateCombo(ACombo: TIDComboBox; const AStoredProc,
      AKeyName, ADisplayName: String; AParams: array of Variant);
    procedure SetTaxonData(const Value: Boolean);
    function GetAccuracy: String;
    function GetAppliesTo: String;
    function GetDuration: String;
    function GetMethodKey: String;
    function GetParameterKey: String;
    function GetParameterValue: String;
    function GetQualifierKey: String;
    function GetTaxonOnly: Boolean;
    function GetTaxonUnitKey: String;
    function GetThesaurusUnitKey: String;
  public
    constructor Create(AOwner: TComponent); override;
    procedure HideRadioGroup;
    property AccuracyValue: String read GetAccuracy;
    property AppliesToValue: String read GetAppliesTo;
    property DurationValue: String read GetDuration;
    property MethodKey: String read GetMethodKey;
    property ParameterValue: String read GetParameterValue;
    property ParameterKey: String read GetParameterKey;
    property QualifierKey: String read GetQualifierKey;
    property TaxonData: Boolean read FTaxonData write SetTaxonData;
    property TaxonOnly: Boolean read GetTaxonOnly;
    property TaxonUnitKey: String read GetTaxonUnitKey;
    property ThesaurusUnitKey: String read GetThesaurusUnitKey;
  end;

//==============================================================================

implementation

{$R *.DFM}

uses
  ApplicationSettings, DataTypes, LuxembourgConstants, ExceptionForm,
  ResourceStrings, GeneralData, Variants, GeneralFunctions;

{-==============================================================================
    TfrmQuickEntryMeasurements
===============================================================================}
{-------------------------------------------------------------------------------
}
constructor TfrmQuickEntryMeasurements.Create(AOwner: TComponent);
var
  lHasTypes: Boolean;
begin
  inherited;
  eAppliesTo.Color     := AppSettings.MandatoryColour;
  cmbParameter.Color   := AppSettings.MandatoryColour;
  cmbTxType.Color      := AppSettings.MandatoryColour;
  cmbTxQualifier.Color := AppSettings.MandatoryColour;
  cmbTxUnit.Color      := AppSettings.MandatoryColour;

  // Force Type to be populated.
  cmbTxType.PopulateContent;
  lHasTypes := cmbTxType.Count > 0;
  cmbTxQualifier.Enabled := lHasTypes;
  cmbTxUnit.Enabled      := lHasTypes;
  if lHasTypes then begin
    cmbTxType.ItemIndex := 0;
    cmbTxTypeChange(nil);
  end;
end;  // TfrmQuickEntryMeasurements.Create

{-------------------------------------------------------------------------------
}
procedure TfrmQuickEntryMeasurements.btnOKClick(Sender: TObject);
begin
  ValidateData;
  ModalResult := mrOk;
end;  // TfrmQuickEntryMeasurements.btnOKClick

{-------------------------------------------------------------------------------
}
procedure TfrmQuickEntryMeasurements.cmbMethodPopulate(Sender: TObject);
begin
  PopulateCombo(cmbMethod, 'usp_Concept_Select_ForConceptGroup',
                'Concept_Key', 'Item_Name', ['@ConceptGroupKey', CG_MEASUREMENT_METHODS]);
end;  // TfrmQuickEntryMeasurements.cmbMethodPopulate

{-------------------------------------------------------------------------------
  Populates the Parameter combo with a list of any parameter concept.
}
procedure TfrmQuickEntryMeasurements.cmbParameterPopulate(Sender: TObject);
begin
  cmbParameter.Clear;
  PopulateCombo(cmbParameter, 'usp_AnyMeasurementParameter_Select',
                'Concept_Key', 'Item_Name', []);
end;  // TfrmQuickEntryMeasurements.cmbParameterPopulate

{-------------------------------------------------------------------------------
}
procedure TfrmQuickEntryMeasurements.cmbUnitPopulate(Sender: TObject);
begin
  PopulateCombo(cmbUnit, 'usp_Concept_Select_ForConceptGroup',
                'Concept_Key', 'Item_Name', ['@ConceptGroupKey', CG_MEASUREMENT_UNIT]);
end;  // TfrmQuickEntryMeasurements.cmbUnitPopulate

{-------------------------------------------------------------------------------
}
procedure TfrmQuickEntryMeasurements.cmbTxQualifierPopulate(Sender: TObject);
begin
  PopulateCombo(cmbTxQualifier, 'usp_MeasurementQualifier_Select_ForType',
                'Item_Key', 'Short_Name', ['@Type', cmbTxType.CurrentStrID]);
end;  // TfrmQuickEntryMeasurements.cmbTxQualifierPopulate

{-------------------------------------------------------------------------------
}
procedure TfrmQuickEntryMeasurements.cmbTxTypeChange(Sender: TObject);
begin
  // Qualifier and Unit depend on the Type, so clear them when Type changes.
  cmbTxQualifier.Clear;
  cmbTxUnit.Clear;
end;  // TfrmQuickEntryMeasurements.cmbTxTypeChange

{-------------------------------------------------------------------------------
}
procedure TfrmQuickEntryMeasurements.cmbTxTypePopulate(Sender: TObject);
begin
  PopulateCombo(cmbTxType, 'usp_MeasurementType_Select',
                'Item_Key', 'Short_Name', ['@DataTable', 'Taxon_Occurrence_Data']);
end;  // TfrmQuickEntryMeasurements.cmbTxTypePopulate

{-------------------------------------------------------------------------------
}
procedure TfrmQuickEntryMeasurements.cmbTxUnitPopulate(Sender: TObject);
begin
  PopulateCombo(cmbTxUnit, 'usp_MeasurementUnit_Select_ForType',
                'Item_Key', 'Short_Name', ['@Type', cmbTxType.CurrentStrID]);
end;  // TfrmQuickEntryMeasurements.cmbTxUnitPopulate

{-------------------------------------------------------------------------------
}
function TfrmQuickEntryMeasurements.GetAccuracy: String;
begin
  if pnlTaxonData.Visible then
    Result := eTxAccuracy.Text
  else
    Result := eAccuracy.Text;
end;  // TfrmQuickEntryMeasurements.GetAccuracy

{-------------------------------------------------------------------------------
}
function TfrmQuickEntryMeasurements.GetAppliesTo: String;
begin
  if pnlTaxonData.Visible then
    Result := ''
  else
    Result := eAppliesTo.Text;
end;  // TfrmQuickEntryMeasurements.GetAppliesTo

{-------------------------------------------------------------------------------
}
function TfrmQuickEntryMeasurements.GetDuration: String;
begin
  if pnlTaxonData.Visible then
    Result := ''
  else
    Result := eDuration.Text;
end;  // TfrmQuickEntryMeasurements.GetDuration

{-------------------------------------------------------------------------------
}
function TfrmQuickEntryMeasurements.GetMethodKey: String;
begin
  if pnlTaxonData.Visible then
    Result := ''
  else
    Result := cmbMethod.CurrentStrID;
end;  // TfrmQuickEntryMeasurements.GetMethodKey

{-------------------------------------------------------------------------------
}
function TfrmQuickEntryMeasurements.GetParameterKey: String;
begin
  if pnlTaxonData.Visible then
    Result := ''
  else
    Result := cmbParameter.CurrentStrID;
end;  // TfrmQuickEntryMeasurements.GetParameterKey

{-------------------------------------------------------------------------------
}
function TfrmQuickEntryMeasurements.GetParameterValue: String;
begin
  if pnlTaxonData.Visible then
    Result := Format('%s of %s (%s)', [cmbTxType.Text, cmbTxQualifier.Text, cmbTxUnit.Text])
  else
    Result := cmbParameter.Text;
end;  // TfrmQuickEntryMeasurements.GetParameterValue

{-------------------------------------------------------------------------------
}
function TfrmQuickEntryMeasurements.GetQualifierKey: String;
begin
  if pnlTaxonData.Visible then
    Result := cmbTxQualifier.CurrentStrID
  else
    Result := '';
end;  // TfrmQuickEntryMeasurements.GetQualifierKey

{-------------------------------------------------------------------------------
}
function TfrmQuickEntryMeasurements.GetTaxonOnly: Boolean;
begin
  // The panel is shown when measurement applies to Taxon Occurrence only.
  Result := pnlTaxonData.Visible;
end;  // TfrmQuickEntryMeasurements.GetTaxonOnly

{-------------------------------------------------------------------------------
}
function TfrmQuickEntryMeasurements.GetTaxonUnitKey: String;
begin
  if pnlTaxonData.Visible then
    Result := cmbTxUnit.CurrentStrID
  else
    Result := '';
end;  // TfrmQuickEntryMeasurements.GetTaxonUnitKey

{-------------------------------------------------------------------------------
}
function TfrmQuickEntryMeasurements.GetThesaurusUnitKey: String;
begin
  if pnlTaxonData.Visible then
    Result := ''
  else
    Result := cmbUnit.CurrentStrID;
end;  // TfrmQuickEntryMeasurements.GetThesaurusUnitKey

{-------------------------------------------------------------------------------
}
procedure TfrmQuickEntryMeasurements.HideRadioGroup;
begin
  // Hide the radio group and shrink the dialog to avoid big blank space.
  rgMeasurementRelatesTo.Visible := False;
  btnOk.Top     := rgMeasurementRelatesTo.Top;
  btnCancel.Top := rgMeasurementRelatesTo.Top;
  ClientHeight  := btnOk.Top + btnOk.Height + 8;
end;  // TfrmQuickEntryMeasurements.HideRadioGroup

{-------------------------------------------------------------------------------
}
procedure TfrmQuickEntryMeasurements.PopulateCombo(ACombo: TIDComboBox; const
  AStoredProc, AKeyName, ADisplayName: String; AParams: Array of Variant);
var
  lCursor: TCursor;
begin
  lCursor := HourglassCursor;
  try
    with dmGeneral.GetRecordset(AStoredProc, AParams) do begin
      while not Eof do begin
        ACombo.Add(VarToStr(Fields[ADisplayName].Value),
                   VarToStr(Fields[AKeyName].Value));
        MoveNext;
      end;
      Close;
    end;
  finally
    DefaultCursor(lCursor);
  end;
end;  // TfrmQuickEntryMeasurements.PopulateCombo

{-------------------------------------------------------------------------------
}
procedure TfrmQuickEntryMeasurements.rgMeasurementRelatesToClick(Sender: TObject);
begin
  pnlCollectionData.Visible := (rgMeasurementRelatesTo.ItemIndex = 0) or not TaxonData;
  pnlTaxonData.Visible      := (rgMeasurementRelatesTo.ItemIndex = 1) and FTaxonData;
end;  // TfrmQuickEntryMeasurements.rgMeasurementRelatesToClick

{-------------------------------------------------------------------------------
}
procedure TfrmQuickEntryMeasurements.SetTaxonData(const Value: Boolean);
begin
  FTaxonData := Value;
  rgMeasurementRelatesToClick(nil);
end;  // TfrmQuickEntryMeasurements.SetTaxonData

{-------------------------------------------------------------------------------
  Additional validation.
}
procedure TfrmQuickEntryMeasurements.ValidateData;
var
  i: Integer;
begin
  inherited;
  if (rgMeasurementRelatesTo.ItemIndex = 1) and FTaxonData then begin
    ValidateValue(cmbTxQualifier.ItemIndex <> -1, Format(ResStr_MissingData, [ResStr_Qualifier]));
    ValidateValue(cmbTxUnit.ItemIndex <> -1, Format(ResStr_MissingData, [ResStr_Unit]));
  end else begin
    ValidateValue(Trim(eAppliesTo.Text) <> '', Format(ResStr_MissingData, [ResStr_AppliesTo]));
    ValidateValue(Trim(eAppliesTo.Text) <> '', Format(ResStr_MissingData, [ResStr_AppliesTo]));
    ValidateValue(Trim(cmbParameter.Text) <> '', Format(ResStr_MissingData, [ResStr_Parameter]));

    FParameterKey := '';
    if Trim(cmbParameter.Text) <> '' then begin
      i := cmbParameter.IndexOf(cmbParameter.Text);
      if i > -1 then begin
        // Parameter is already listed, so select it.
        cmbParameter.ItemIndex := i;
        FParameterKey := cmbParameter.CurrentStrID;
      end else begin
        // Not in list yet, but could already be in DB.
        FParameterKey := VarToStr(dmGeneral.GetStoredProcOutputParam
                           ('usp_ConceptKey_Get_ForConceptGroupAndItemName',
                            ['@ConceptGroupKey', CG_MEASUREMENT_PARAMETERS,
                             '@Plaintext', cmbParameter.Text],
                             '@ConceptKey'));
        if FParameterKey = '' then
          if MessageDlg(Format(ResStr_AddMeasurementParameter, [cmbParameter.Text]),
                        mtInformation, [mbYes, mbNo], 0) <> mrYes then
          begin
            // User says don't add, so focus on combobox and abort save process.
            cmbParameter.SetFocus;
            Abort;  // Stop save process
          end;
      end;
    end;
  end;
end;  // TfrmQuickEntryMeasurements.ValidateData

end.
