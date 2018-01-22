{===============================================================================
  Unit:        FrameMeasurementsGeneral.pas

  Defines:     FrameMeasurementsGeneral

  Description:

  Created:     June 2003

  Model:       CollectionsCommonAndGeneral.mpb

  Last revision information:
    $Revision: 25 $
    $Date: 1/09/04 17:01 $
    $Author: Ericsalmon $

===============================================================================}

unit FrameMeasurementsGeneral;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, BaseDragFrameUnit, ComboListID, LuxIDComboBox,
  BaseDetailFrameUnit, BaseTabSheetFrameUnit, ConceptGroupComboBox, DataTypes,
  GeneralData, ExceptionForm, RestrictedEdits, GeneralFunctions;

type
  {-----------------------------------------------------------------------------
    Tab page control containing data for a selected measurement.  
    This control can be bound to a record in any table with the appropriate structure (e.g.
    Occurrence_Data or Collection_Unit_Data).
    This screen operates in 2 modes, Detailed and Simple.  When in Detailed mode, all
    controls are visible.  When in Simple mode, only eAppliesTo, cmbParameter and eValue
    are displayed and the controls are layed out to maintain an organised appearance.
  }
  TfraMeasurementsGeneral = class(TBaseTabSheetFrame)
    btnMore: TButton;
    cmbMethod: TConceptGroupComboBox;
    cmbParameter: TConceptGroupComboBox;
    cmbUnit: TConceptGroupComboBox;
    eAccuracy: TEdit;
    eAppliesTo: TEdit;
    eDuration: TEdit;
    eValue: TEdit;
    lblAccuracy: TLabel;
    lblAppliesTo: TLabel;
    lblDuration: TLabel;
    lblMethod: TLabel;
    lblParameter: TLabel;
    lblUnit: TLabel;
    lblValue: TLabel;
    constructor Create(AOwner: TComponent); override;
    procedure btnMoreClick(Sender: TObject);
    procedure cmbParameterPopulate(Sender: TObject);
    procedure eValueExit(Sender: TObject);
  private
    FDateTimeStart: String;
    FDomainConceptGroupKey: String;
    FDomainGroupStoredProcName: String;
    FLocalDomainKey: String;
    FLowerValue: String;
    FParameterKey: String;
    FParentKeyParamName: String;
    FTableName: String;
    FTableNameCompact: String;
    FTimestamp: TSQLSvrTimestamp;
    FTopNodeContext: TNodeContext;
    FUpperValue: String;
    function GetParamsForDomainGroup: TVariantArray;
    function GetParamsForRecord: TVariantArray;
    procedure ParseValue(const AText: String; var ALowerValue, AUpperValue: String);
    procedure SetControlDetail(AShowMoreDetail: Boolean);
    procedure SetControlPosition(AShowMoreDetail: Boolean);
    procedure SetLocalDomainKey(const Value: String);
  protected
    procedure DeleteData; override;
    procedure EnableControls(AEnabled: Boolean); override;
    function GetCaption: String; override;
    procedure LoadData; override;
    procedure RegisterControls; override;
    procedure SaveData; override;
    procedure SetMasterFrameType(Value: TMasterFrameType); override;
    procedure ValidateData; override;
    property LocalDomainKey: String read FLocalDomainKey write SetLocalDomainKey;
  end;
  
//==============================================================================
implementation

uses
  ADODB, ApplicationSettings, LuxembourgConstants, VagueDate, DateUtils,
  ResourceStrings, StrUtils;

{$R *.dfm}

const
  I_VSPACER = 32;

{-==============================================================================
    TfraMeasurementsGeneral
===============================================================================}
{-------------------------------------------------------------------------------
  Constructor. Sets up the combo box to the correct type at runtime and starts the frame to
      show less controls.
}
constructor TfraMeasurementsGeneral.Create(AOwner: TComponent);
begin
  inherited;
  cmbParameter.Style := csDropDown;
  btnMoreClick(nil);
  // Allow 50 for lower and higher values, 2 for spaces around the separator and as
  // many characters as it takes for the separator, which can presumably be localised.
  eValue.MaxLength := 102 + Length(ResStr_ToLowerCase);
end;  // TfraMeasurementsGeneral.Create 

{-------------------------------------------------------------------------------
  Toggles the amount of components visible on the frame. 
}
procedure TfraMeasurementsGeneral.btnMoreClick(Sender: TObject);
begin
  inherited;
  SetControlPosition(btnMore.Caption = ResStr_DetailsMore);
  SetControlDetail(btnMore.Caption = ResStr_DetailsMore);
end;  // TfraMeasurementsGeneral.btnMoreClick 

{-------------------------------------------------------------------------------
  Populates the combo box with the last 10 conceps used. 
}
procedure TfraMeasurementsGeneral.cmbParameterPopulate(Sender: TObject);
begin
  inherited;
  PopulateLast10ConceptsCombo(cmbParameter, FDomainConceptGroupKey,
                              PL_MEASUREMENT_PARAMETERS + FDomainConceptGroupKey,
                              CG_MEASUREMENT_PARAMETERS);
end;  // TfraMeasurementsGeneral.cmbParameterPopulate 

{-------------------------------------------------------------------------------
  Run the delete stored proc. for this frame. 
}
procedure TfraMeasurementsGeneral.DeleteData;
begin
  dmGeneral.RunDeleteStoredProc('usp_' + FTableNameCompact + '_Delete',
                                ['@Key', Key, '@Timestamp', FTimestamp]);
end;  // TfraMeasurementsGeneral.DeleteData 

{-------------------------------------------------------------------------------
  Enable btnMore. 
}
procedure TfraMeasurementsGeneral.EnableControls(AEnabled: Boolean);
begin
  inherited;
  SetRequiredFieldsColourState(AEnabled, [eValue]);
  btnMore.Enabled := True;
end;  // TfraMeasurementsGeneral.EnableControls 

{-------------------------------------------------------------------------------
}
procedure TfraMeasurementsGeneral.eValueExit(Sender: TObject);
begin
  inherited;
  // Values are stored in FLowerValue and FUpperValue so that they can be
  // validated in Validation and so they don't need to be recalculated on the
  // save.
  ParseValue(eValue.Text, FLowerValue, FUpperValue);
end;  // TfraMeasurementsGeneral.eValueExit 

{-------------------------------------------------------------------------------
  Return the caption. When the browser node asks for the caption, and you are adding a measurement, the DateTimeStart hasn't always been loaded by the LoadData. Hence, use the current Date because that will always be correct for an insert.
}
function TfraMeasurementsGeneral.GetCaption: String;
begin
  if FDateTimeStart = '' then
    Result := eAppliesTo.Text + ' - ' + DateToStr(Date)
  else
    Result := eAppliesTo.Text + ' - ' + FDateTimeStart;
end;  // TfraMeasurementsGeneral.GetCaption 

{-------------------------------------------------------------------------------
  Get the paramters for the domain group. 
}
function TfraMeasurementsGeneral.GetParamsForDomainGroup: TVariantArray;
begin
  Result := VarArrayOf(['@Key', ParentKey,
                        '@DomainConceptGroupName', ST_MEASUREMENT_PARAMETERS,
                        '@TopLevelNodeContext', FTopNodeContext]);
end;  // TfraMeasurementsGeneral.GetParamsForDomainGroup 

{-------------------------------------------------------------------------------
  Get the parameters for the record. 
}
function TfraMeasurementsGeneral.GetParamsForRecord: TVariantArray;
begin
  Result := VarArrayOf(['@Key', Key, '@IsDescriptor', 0]);
end;  // TfraMeasurementsGeneral.GetParamsForRecord 

{-------------------------------------------------------------------------------
  Load the time stamp, FDateTimeStart, Local Domain Key and Domain Concept Group Key. 
}
procedure TfraMeasurementsGeneral.LoadData;
begin
  inherited;
  with RegisteredRecordsets[0] do
    if not Eof then begin
      FTimestamp := Fields['Timestamp'].Value;
      FDateTimeStart := DateToStr(Fields['Date_Time_Start'].Value);
      FLowerValue := VarToStr(Fields['Lower_Value'].Value);
      FUpperValue := VarToStr(Fields['Upper_Value'].Value);
      if FLowerValue = ' ' then   // Cannot be Null, so ' ' might be there instead.
        eValue.Text := ResStr_ToLowerCase + ' ' + FUpperValue
      else begin
        if FUpperValue = '' then
          eValue.Text := FLowerValue
        else
          eValue.Text := FLowerValue + ' ' + ResStr_ToLowerCase + ' ' + FUpperValue;
      end
    end;
  if Assigned(RegisteredRecordsets[1]) then
    // The concept group key of the concept group called 'Measurement Parameters'
    // in the domain associated with the data item the data is attached to
    // is needed.
    with RegisteredRecordsets[1] do
      if not Eof then begin
        FLocalDomainKey := VarToStr(Fields['Local_Domain_Key'].Value);
        FDomainConceptGroupKey := VarToStr(Fields['Concept_Group_Key'].Value);
      end;
end;  // TfraMeasurementsGeneral.LoadData 

{-------------------------------------------------------------------------------
  Parse the contents of the eValue field as specified in 4.2.3.14. Also, some additional
      clarifications have been made, i.e. '-' must be surrounded by spaces. 'To' replaces '-'
      on parsing.
}
procedure TfraMeasurementsGeneral.ParseValue(const AText: String; var ALowerValue,
    AUpperValue: String);
var
  lTempText, lSeparator: String;
  lPos: Integer;
begin
  // Make lSeparator out of the ResourceString for 'to' surrounded with spaces.
  lSeparator := ' ' + ResStr_ToLowerCase + ' ';
  // As we are searching for '- ', make sure any '-' at the end
  // have spaces after.
  lTempText := AText + ' ';
  // Replace '- ' with ' to ' for easier parsing of negative numbers. Put
  // spaces back around it because StringReplace seems to trim lTempText
  lTempText := ' ' + StringReplace(lTempText, '- ', lSeparator, []) + ' ';
  // Get the position of the first ' to '
  lPos := Pos(lSeparator, lTempText);
  // If no '- ' or ' to ', just put the value in the Lower Value field.
  if (lPos = 0) then begin
    ALowerValue := Trim(lTempText);
    AUpperValue := '';
  end
  // Otherwise, separate.
  else begin
    ALowerValue := Trim(LeftStr(lTempText, lPos));
    AUpperValue := Trim(RightStr(lTempText,
                                 Length(lTempText) - lPos - Length(lSeparator) + 1));
    // Put a space on if empty, so just that space can be saved,
    // so '5 to' is displayed rather than '5'
    if AUpperValue = '' then AUpperValue := ' ';
    eValue.Text := ALowerValue + lSeparator + AUpperValue;
  end;
end;  // TfraMeasurementsGeneral.ParseValue 

{-------------------------------------------------------------------------------
  Register the recordset and the controls. 
}
procedure TfraMeasurementsGeneral.RegisterControls;
begin
  inherited;
  FTopNodeContext := AdditionalProperties.GetProperty(PROP_TOP_NODE_CONTEXT);
  
  // Register recordsets used
  RegisterRecordset('usp_' + FTableNameCompact + '_Select', GetParamsForRecord);
  // May not need to run the proc.
  if FDomainGroupStoredProcName <> '' then
  // Get the right Domain concept group, for when new descriptors are added to
  // the Thesaurus.
    RegisterRecordset(FDomainGroupStoredProcName, GetParamsForDomainGroup);
  
  // Register controls getting their value straight from the registered recordsets.
  RegisterControl(eAppliesTo, 'Applies_To', True, ResStr_AppliesTo);
  RegisterControl(cmbMethod, 'Method_Term', 'Method_Concept_Key');
  RegisterConceptGroupComboBox(cmbMethod, CG_MEASUREMENT_METHODS);
  RegisterControl(cmbParameter, 'Parameter_Term', 'Parameter_Concept_Key',
                  True, ResStr_Parameter);
  RegisterControl(eDuration, 'Duration');
  RegisterControl(eAccuracy, 'Accuracy');
  RegisterControl(cmbUnit, 'Unit_Term', 'Unit_Concept_Key');
  RegisterConceptGroupComboBox(cmbUnit, CG_MEASUREMENT_UNIT);
end;  // TfraMeasurementsGeneral.RegisterControls 

{-------------------------------------------------------------------------------
  Save the frame's data to the database. 
}
procedure TfraMeasurementsGeneral.SaveData;
var
  lParams: Array of Variant;
  lCollectionUnitKey: Variant;
  lVagueDate: TVagueDate;
begin
  lCollectionUnitKey := Null;
  if MasterFrameType = mftCollectionUnit then lCollectionUnitKey := ParentKey;
  
  // If Domain Concept Group doesn't exist yet, create it, so it can be used to
  // save list in registry.
  if FDomainConceptGroupKey = '' then
    FDomainConceptGroupKey := VarToStr(dmGeneral.RunInsertStoredProc
                (TN_CONCEPT_GROUP,'usp_ConceptGroup_Insert',
                 ['@ConceptGroupName', ST_MEASUREMENT_PARAMETERS,
                  '@LocalDomainKey', FLocalDomainKey,
                  '@Version', '1'],
                 '@Key'));
  // If new parameter, save it to Domain Concept Group
  if (FParameterKey = '') and (Trim(cmbParameter.Text) <> '') then begin
    lVagueDate := StringToVagueDate(FormatDateTime('dd/mm/yyyy', Today));
    FParameterKey := VarToStr(dmGeneral.RunInsertStoredProc(TN_CONCEPT,
                                   'usp_Concept_Insert',
                                   ['@ConceptGroupKey', FDomainConceptGroupKey,
                                    '@TermName', Trim(cmbParameter.Text),
                                    '@LanguageKey', AppSettings.ISOLanguage,
                                    '@VagueDateFrom', lVagueDate.StartDate,
                                    '@VagueDateTo', lVagueDate.EndDate,
                                    '@VagueDateType', lVagueDate.DateTypeString],
                                   '@Key'));
    // Display the Domain and Concept Group the parameter has been added to.
    MessageDlg(Format(ResStr_ParamAddedToDomainConceptGroup,
                  [cmbParameter.Text, dmGeneral.GetStoredProcOutputParam
                        ('usp_DomainConceptGroup_Name_Get',
                        ['@Key', FDomainConceptGroupKey],
                        '@Name')]),
            mtInformation, [mbOK], 0);
    // Add to combo and re-select.
    cmbParameter.Add(cmbParameter.Text, FParameterKey);
    cmbParameter.ItemIndex := cmbParameter.Count - 1;
  end;
  // Add Parameter to persistent list.
  AppSettings.AddItemToPersistentList(PL_MEASUREMENT_PARAMETERS + FDomainConceptGroupKey,
                                      FParameterKey, MAX_LAST_USED);
  
  lParams := VarArrayOf(['@Key', Key,
                         FParentKeyParamName, ParentKey,
                         '@AppliesTo', eAppliesTo.Text,
                         '@MethodConceptKey', cmbMethod.CurrentStrID,
                         '@Duration', eDuration.Text,
                         '@Accuracy', eAccuracy.Text,
                         '@ParameterConceptKey', FParameterKey,
                         '@UnitConceptKey', cmbUnit.CurrentStrID,
                         '@Value', FLowerValue,
                         '@UpperValue', FUpperValue,
                         '@IsDescriptor', 0, // 0 indicates it is a Data Measurement
                         '@Timestamp', FTimestamp]);
  
  // Empty key means new record.
  if Key = '' then
    Key := VarToStr(dmGeneral.RunInsertStoredProc(FTableName,
                                                  'usp_' + FTableNameCompact + '_Insert',
                                                  lParams, '@Key'))
  else
    dmGeneral.RunUpdateStoredProc('usp_' + FTableNameCompact + '_Update', lParams);
end;  // TfraMeasurementsGeneral.SaveData 

{-------------------------------------------------------------------------------
  Actually makes the controls (in)visible. 
}
procedure TfraMeasurementsGeneral.SetControlDetail(AShowMoreDetail: Boolean);
begin
  lblMethod.Visible   := AShowMoreDetail;
  cmbMethod.Visible   := AShowMoreDetail;
  lblDuration.Visible := AShowMoreDetail;
  eDuration.Visible   := AShowMoreDetail;
  lblAccuracy.Visible := AShowMoreDetail;
  eAccuracy.Visible   := AShowMoreDetail;
  lblUnit.Visible     := AShowMoreDetail;
  cmbUnit.Visible     := AShowMoreDetail;
  // Switch button caption.
  if AShowMoreDetail then
    btnMore.Caption := ResStr_DetailsLess
  else
    btnMore.Caption := ResStr_DetailsMore;
end;  // TfraMeasurementsGeneral.SetControlDetail 

{-------------------------------------------------------------------------------
  Sets the position of the controls when they are toggled. 
}
procedure TfraMeasurementsGeneral.SetControlPosition(AShowMoreDetail: Boolean);
  
  procedure AlignControls(AControls: Array of TControl);
  var
    i: Integer;
  begin
    for i := 0 to High(AControls) div 2 do begin
      AControls[i * 2    ].Top := lblAppliesTo.Top + (i + 1) * I_VSPACER;
      AControls[i * 2 + 1].Top := eAppliesTo.Top + (i + 1) * I_VSPACER;
    end;
  end;
  
begin
  if AShowMoreDetail then
    AlignControls([lblMethod, cmbMethod, lblDuration, eDuration,
                   lblAccuracy, eAccuracy, lblParameter, cmbParameter,
                   lblUnit, cmbUnit, lblValue, eValue])
  else
    AlignControls([lblParameter, cmbParameter, lblValue, eValue]);
end;  // TfraMeasurementsGeneral.SetControlPosition 

{-------------------------------------------------------------------------------
}
procedure TfraMeasurementsGeneral.SetLocalDomainKey(const Value: String);
begin
  FLocalDomainKey := Value;
end;  // TfraMeasurementsGeneral.SetLocalDomainKey 

{-------------------------------------------------------------------------------
  Set the master frame type so that it knows whether to get data for collection units or
      occurrences.
}
procedure TfraMeasurementsGeneral.SetMasterFrameType(Value: TMasterFrameType);
begin
  inherited;
  case MasterFrameType of
    mftCollectionUnit:
      begin
        FTableName := TN_COLLECTION_UNIT_DATA;
        FTableNameCompact := StringReplace(TN_COLLECTION_UNIT_DATA, '_', '', [rfReplaceAll]);
        FParentKeyParamName := '@CollectionUnitKey';
        FDomainGroupStoredProcName := 'usp_DomainConceptGroup_Select_ForCollectionUnit';
      end;
  
    mftOccurrence:
      begin
        FTableName := TN_OCCURRENCE_DATA;
        FTableNameCompact := StringReplace(TN_OCCURRENCE_DATA, '_', '', [rfReplaceAll]);
        FParentKeyParamName := '@OccurrenceKey';
        FDomainGroupStoredProcName := 'usp_DomainConceptGroup_Select_ForOccurrence';
      end;
  
    mftLocationFeature:
      begin
        FTableName := TN_LOCATION_FEATURE_DATA;
        FTableNameCompact := StringReplace(TN_LOCATION_FEATURE_DATA, '_', '', [rfReplaceAll]);
        FParentKeyParamName := '@LocationFeatureKey';
        FDomainGroupStoredProcName := '';  // Use global keys, no need for proc.
  
        FLocalDomainKey := LD_GLOBAL_SYSTEM_TERM_LIST;
        FDomainConceptGroupKey := CG_MEASUREMENT_PARAMETERS;
      end;
  end;
end;  // TfraMeasurementsGeneral.SetMasterFrameType 

{-------------------------------------------------------------------------------
  Additional validation. 
}
procedure TfraMeasurementsGeneral.ValidateData;
var
  lIdx: Integer;
begin
  inherited;
  ValidateValue(FLocalDomainKey <> '', ResStr_CannotAddMeasurement);
  ValidateValue(eValue.Text <> '', Format(ResStr_MissingData, [ResStr_Value]));
  ValidateValue(Length(FLowerValue) <= 50,
                Format(ResStr_CannotBeMoreThanXCharacters, [ResStr_LowerValue, '50']));
  ValidateValue(Length(FUpperValue) <= 50,
                Format(ResStr_CannotBeMoreThanXCharacters, [ResStr_UpperValue, '50']));
  
  FParameterKey := '';
  if Trim(cmbParameter.Text) <> '' then begin
    lIdx := cmbParameter.IndexOf(cmbParameter.Text);
    if lIdx > -1 then begin
      // Parameter is already listed, so select it.
      cmbParameter.ItemIndex := lIdx;
      FParameterKey := cmbParameter.CurrentStrID;
    end else begin
      // Not in list yet, but could already be in DB.
      FParameterKey := VarToStr(dmGeneral.GetStoredProcOutputParam
                         ('usp_ConceptKey_Get_ForConceptGroupAndItemName',
                          ['@ConceptGroupKey', CG_MEASUREMENT_PARAMETERS,
                           '@Plaintext', cmbParameter.Text],
                           '@ConceptKey'));
      // What? No Key??
      if FParameterKey = '' then begin
        // No Parameter. Check in DomainCG, if DomainCG exists.
        if FDomainConceptGroupKey <> '' then
          FParameterKey := VarToStr(dmGeneral.GetStoredProcOutputParam
                                    ('usp_ConceptKey_Get_ForConceptGroupAndItemName',
                                     ['@ConceptGroupKey', FDomainConceptGroupKey,
                                      '@Plaintext', cmbParameter.Text],
                                      '@ConceptKey'));
  
        // Still no key!!
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
end;  // TfraMeasurementsGeneral.ValidateData 

end.
