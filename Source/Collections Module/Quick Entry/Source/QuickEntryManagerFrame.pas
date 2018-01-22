 {===============================================================================
  Unit:        QuickEntryManagerFrame.pas

  Defines:     TfraQuickEntryManager

  Description: Implements the Quick Entry Manager screen.

  Model:       QuickEntry.mpb

  Created:     August 2003

  Last revision information:
    $Revision: 52 $
    $Date: 8/08/08 16:09 $
    $Author: Ericsalmon $

===============================================================================}
unit QuickEntryManagerFrame;

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  ActiveX, AxCtrls, QuickEntry_TLB, StdVcl, StdCtrls, VagueDateEdit,
  ComboListID, ImageListButton, Grids, ExtCtrls, ADODB, DataStringGrid,
  Recorder2000_TLB, DataTypes, InterfaceDataModule, Contnrs, 
  QuickEntryData, Variants, ADOInt, QuickEntryMeasurements, QEAddNumber, QEAddMetadata,
  ExceptionForm, ApplicationSettings, VagueDate, StrUtils, BaseDragFrameUnit,
  BaseCompositeComponent, QuickEntryDataControls, LuxIDComboBox,
  BaseDetailFrameUnit, QuickEntryInsertTables, Buttons;

resourcestring
  ResStr_TemplateNameRequired = 'A template name is required.';
  ResStr_FollowingFieldsAdded =
      'Before saving this template, the following fields must be checked to ensure ' +
      'that the template is valid.'#13#10'%s'#13#10'Do you wish to proceed?';
  ResStr_SelectedSubjectAreaNotAvailable =
      'The subject area you have selected for this template does not contain ' +
      'any domains that are visible to you and therefore cannot be selected ' +
      'for use by you.  Are you sure you wish to proceed?';
  ResStr_TaxonDetOrDetChecked = 'Either Taxon Determination or Thesaurus ' +
      'Determination must to be checked.';
  ResStr_InvalidDefaultValues = 'The following fields have invalid default values:'+
      #13#10'%s'#13#10'Do you wish to remove these values and proceed?';


type
  TEditMode = (emEdit, emBrowse);

  TTemplateType = (ttSpecimens, ttOccurrences);

  {-----------------------------------------------------------------------------
    Exception class for QuickEntryManagerFrame unit.
  }
  EQuickEntryManager = class(TExceptionPath)
  end;

  {-----------------------------------------------------------------------------
    Wrapper class for the summary details of a Quick Entry template.
  }
  TQuickEntryTemplate = class(TObject)
  private
    FCanDelete: Boolean;
    FKey: string;
    FSubjectAreaKey: string;
    FTemplateType: TTemplateType;
    FTimestamp: TSQLSvrTimestamp;
    procedure SetCanDelete(Value: Boolean);
    procedure SetKey(const Value: string);
    procedure SetSubjectAreaKey(const Value: string);
    procedure SetTemplateType(Value: TTemplateType);
    procedure SetTimestamp(Value: TSQLSvrTimestamp);
  public
    constructor Create(AKey, AName, ASubjectAreaKey : string; ATemplateType:
        TTemplateType; ATimestamp: TSQLSvrTimestamp; ACanDelete: Boolean);
    property CanDelete: Boolean read FCanDelete write SetCanDelete;
    property Key: string read FKey write SetKey;
    property SubjectAreaKey: string read FSubjectAreaKey write
        SetSubjectAreaKey;
    property TemplateType: TTemplateType read FTemplateType write
        SetTemplateType;
    property Timestamp: TSQLSvrTimestamp read FTimestamp write SetTimestamp;
  end;
  
  TfraQuickEntryManager = class(TBaseDetailFrame)
    btnAdd: TImageListButton;
    btnAddMeasurement: TImageListButton;
    btnCancel: TImageListButton;
    btnDelete: TImageListButton;
    btnEdit: TImageListButton;
    btnSave: TImageListButton;
    cmbDataType: TComboBox;
    cmbSubjectArea: TLuxIDComboBox;
    eName: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    lbTemplates: TListBox;
    pnlDetails: TPanel;
    pnlSelector: TPanel;
    sgFields: TStringGrid;
    Splitter1: TSplitter;
    btnMoveUp: TImageListButton;
    btnMoveDown: TImageListButton;
    btnAddNumber: TBitBtn;
    btnAddMetadata: TBitBtn;
    procedure btnAddClick(Sender: TObject);
    procedure btnAddMeasurementClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
    procedure btnEditClick(Sender: TObject);
    procedure btnMoveUpClick(Sender: TObject);
    procedure btnMoveDownClick(Sender: TObject);
    procedure sgFieldsSelectCell(Sender: TObject; ACol, ARow: Integer;
      var CanSelect: Boolean);
    procedure btnSaveClick(Sender: TObject);
    procedure cmbDataTypeChange(Sender: TObject);
    procedure InitialiseDataEntryControls;
    procedure lbTemplatesClick(Sender: TObject);
    procedure btnAddNumberClick(Sender: TObject);
    procedure btnAddMetadataClick(Sender: TObject);
    procedure sgFieldsRowMoved(Sender: TObject; FromIndex,
      ToIndex: Integer);
  private
    FCurrentGridControl: TWinControl;
    FDataStringGrid: TDataStringGrid;
    FEditMode: TEditMode;
    FLastTemplateIndex: Integer;
    FSpecimenDataEntryControls: array [0..21] of TSpecimenDataEntryControl;
    FtfRowChanging: Boolean;
    FDiscardedMeasurements: TObjectList;
    FRegistrationMacro: String;
    FAccessionMacro: String;
    procedure AddMeasurementRow(ADialog: TfrmQuickEntryMeasurements);
    procedure AddMetadataRow(ADialog: TfrmQuickEntryMetadata);
    procedure AddNumberRow(ADialog: TfrmQuickEntryNumber);
    procedure AddRow(AIndex: Integer);
    procedure AddSpecimenEventHandler(ASpecimenDataEntryControl:
        TSpecimenDataEntryControl);
    procedure ApplyDummyValueToFieldAtRow(ARow: integer; AWrapper:
        TInsertWrapper);
    procedure CheckboxStateChanged(ACol, ARow: integer);
    procedure CheckHiddenAndLocked(ACol, ARow: Integer);
    procedure CheckCell(ACol, ARow: integer);
    function CheckDefaultValues(ATestOnly: Boolean): String;
    function CheckDeterminationTicked(AFieldsToCheck: String): Boolean;
    function Checked(ACol, ARow: integer): Boolean;
    function CheckNecessaryExtraFields(ATestOnly: boolean): string;
    function CheckSubjectAreaAvailable: Boolean;
    procedure ClearTemplates;
    function FindRowByQEFieldKey(lQEFieldKey: string): Integer;
    function GetHiddenValue(AName: string; ARow: Integer): string;
    procedure GridCanEditCell(var ioAccept: Boolean; ACol, ARow: Integer);
    procedure GridControlKeyDown(Sender: TObject; Var Key: Word;Shift:
        TShiftState);
    procedure GridCustomEditCell(Panel: TPanel; ACol, ARow: Integer);
    procedure InsertField(ARow: Integer; ASequence: Integer; const
        ATemplateKey: String);
    function IsCustomRow(ARow: integer): Integer;
    function GetMacro(ANumberType: String): String;
    procedure PopulateTemplateFields;
    procedure PopulateTemplates;
    procedure RefreshMoveButtons(ARow: Integer);
    procedure RefreshTemplateDetails(ATemplate: TQuickEntryTemplate; AIndex:
        integer);
    function RowIsChecked(lRow: integer): Boolean;
    procedure SaveTemplate;
    procedure SetEditMode(const Value: TEditMode);
    procedure SetHiddenValue(AName: string; ARow: Integer; const Value: string);
    procedure SetMeasurementState;
    procedure SpecimenEventHandlerExit(Sender: TObject);
    property HiddenValue[AName: string; ARow: Integer]: string read
        GetHiddenValue write SetHiddenValue;
    function CheckMeasurementRows(ACheckTaxonDataRows: Boolean): Boolean;
    function GetCellColour(ACol,ARow: integer; var AHandled: boolean): TColor;
  protected
    procedure CMChildKey(Var msg: TCMChildKey); message CM_CHILDKEY;
    procedure RegisterDragDropComponents; override;
  public
    destructor Destroy; override;
    procedure Initialise;
    property EditMode: TEditMode read FEditMode write SetEditMode;
  end;

//==============================================================================
implementation

{$R *.DFM}

uses
  ComObj, ComServ, GeneralData, ResourceStrings, Dialogs, LinkedControls,
  FrameMeasurementsGeneral, ConceptGroupComboBox, Math, LuxembourgConstants,
  GeneralFunctions, BaseADODataModule;

const
  IS_CUSTOM = 'Is_Custom';
  DATA_TYPE      = 'Data_Type';
  DEFAULT_VALUE  = 'Default_Value';

type
  TQEFieldInfo = class
  private
    FTemplateFieldKey: String;
    FTimestamp: TSQLSvrTimestamp;
  public
    property TemplateFieldKey: String read FTemplateFieldKey write FTemplateFieldKey;
    property Timestamp: TSQLSvrTimestamp read FTimestamp write FTimestamp;

end;

{-==============================================================================
    TQuickEntryTemplate
===============================================================================}
{-------------------------------------------------------------------------------
  Instance initialisation.
}
constructor TQuickEntryTemplate.Create(AKey, AName, ASubjectAreaKey : string;
    ATemplateType: TTemplateType; ATimestamp: TSQLSvrTimestamp; ACanDelete: Boolean);
begin
  FKey := AKey;
  FSubjectAreaKey := ASubjectAreaKey;
  FTemplateType := ATemplateType;
  FTimestamp := ATimestamp;
  FCanDelete := ACanDelete;
end;  // TQuickEntryTemplate.Create 

{-------------------------------------------------------------------------------
}
procedure TQuickEntryTemplate.SetCanDelete(Value: Boolean);
begin
  FCanDelete := Value;
end;  // TQuickEntryTemplate.SetCanDelete 

{-------------------------------------------------------------------------------
  Accessor method. 
}
procedure TQuickEntryTemplate.SetKey(const Value: string);
begin
  FKey := Value;
end;  // TQuickEntryTemplate.SetKey 

{-------------------------------------------------------------------------------
  Accessor method. 
}
procedure TQuickEntryTemplate.SetSubjectAreaKey(const Value: string);
begin
  FSubjectAreaKey := Value;
end;  // TQuickEntryTemplate.SetSubjectAreaKey 

{-------------------------------------------------------------------------------
  Accessor method. 
}
procedure TQuickEntryTemplate.SetTemplateType(Value: TTemplateType);
begin
  FTemplateType := Value;
end;  // TQuickEntryTemplate.SetTemplateType 

{-------------------------------------------------------------------------------
  Accessor method. 
}
procedure TQuickEntryTemplate.SetTimestamp(Value: TSQLSvrTimestamp);
begin
  FTimestamp := Value;
end;  // TQuickEntryTemplate.SetTimestamp 

{-==============================================================================
    TfraQuickEntryManager
===============================================================================}
{-------------------------------------------------------------------------------
  Objecte destruction.  Cleanup windows controls first, otherwise windwos
      handle problems may occur.
}
destructor TfraQuickEntryManager.Destroy;
var
  lIdx: Integer;
begin
  ClearTemplates;
  FDiscardedMeasurements.Free;
  for lIdx := ComponentCount - 1 downto 0 do
    Components[lIdx].Free;
  TdmGeneral.Discard;
  DiscardAppSettings;
  inherited;
end;  // TfraQuickEntryManager.Destroy

{-------------------------------------------------------------------------------
  Add a new row to the grid using the Add measurements form.
}
procedure TfraQuickEntryManager.AddMeasurementRow(ADialog: TfrmQuickEntryMeasurements);
var
  lIdx: Integer;
begin
  with ADialog do begin
    sgFields.RowCount := sgFields.RowCount + 1;
    lIdx := sgFields.RowCount - 1;
    //populate a row in the grid with details of this measurement
    sgFields.Cells[0, lIdx] := ParameterValue;
    sgFields.Cells[3, lIdx] := '';
    sgFields.Cells[4, lIdx] := '';
    //by default put it on the specimen tab in case the user forgets.
    sgFields.Cells[2, lIdx] := STR_TRUE;

    AddRow(lIdx);

    HiddenValue['Measurement_Applies_To', lIdx]            := AppliesToValue;
    HiddenValue['Measurement_Parameter_Concept_Key', lIdx] := ParameterKey;
    HiddenValue['Measurement_Unit_Concept_Key', lIdx]      := ThesaurusUnitKey;
    HiddenValue['Measurement_Method_Concept_Key', lIdx]    := MethodKey;
    HiddenValue['Measurement_Duration', lIdx]              := DurationValue;
    HiddenValue['Measurement_Accuracy', lIdx]              := AccuracyValue;
    HiddenValue[DATA_TYPE, lIdx]                           := '0'; //plain text
    HiddenValue['Is_Custom', lIdx]                         := '1';
    HiddenValue['Measurement_Is_Specimen', lIdx] :=
        Sysutils.BoolToStr(rgMeasurementRelatesTo.ItemIndex = 0);
    HiddenValue['Taxon_Measurement_Qualifier_Key', lIdx]   := QualifierKey;
    HiddenValue['Taxon_Measurement_Unit_Key', lIdx]        := TaxonUnitKey;
    HiddenValue['Measurement_Is_TaxonData', lIdx]          := SysUtils.BoolToStr(TaxonOnly);
  end; // with ADialog
end;  // TfraQuickEntryManager.AddMeasurementRow
 
{-------------------------------------------------------------------------------
  Add a new row to the grid using the Add metadata dialog
}
procedure TfraQuickEntryManager.AddMetadataRow(ADialog: TfrmQuickEntryMetadata);
var
  lIdx: Integer;
begin
  with ADialog do begin
    sgFields.RowCount := sgFields.RowCount + 1;
    lIdx := sgFields.RowCount - 1;
    //populate a row in the grid with details of this measurement
    sgFields.Cells[0, lIdx] := TableName + ' ' + ItemName;
    sgFields.Cells[3, lIdx] := '';
    sgFields.Cells[4, lIdx] := '';
    //by default put it on the specimen tab in case the user forgets.
    sgFields.Cells[2, lIdx] := STR_TRUE;

    AddRow(lIdx);

    HiddenValue['Metadata_Type_Key', lIdx]        := MetadataTypeKey;
    HiddenValue[DATA_TYPE, lIdx]                  := '0'; //plain text
    HiddenValue['Is_Custom', lIdx]                := '3';
  end;
end;

{-------------------------------------------------------------------------------
  Add a new row to the grid using the Add number dialog
}
procedure TfraQuickEntryManager.AddNumberRow(ADialog: TfrmQuickEntryNumber);
var
  lIdx: Integer;
begin
  with ADialog do begin
    sgFields.RowCount := sgFields.RowCount + 1;
    lIdx := sgFields.RowCount - 1;
    //populate a row in the grid with details of this measurement
    sgFields.Cells[0, lIdx] := NumberType;
    sgFields.Cells[3, lIdx] := '';
    sgFields.Cells[4, lIdx] := '';
    //by default put it on the specimen tab in case the user forgets.
    sgFields.Cells[2, lIdx] := STR_TRUE;

    AddRow(lIdx);

    HiddenValue['Number_Type_Concept_Key', lIdx]  := NumberTypeKey;
    HiddenValue['Number_Preferred', lIdx]         := SysUtils.BoolToStr(IsPreferred);
    if NumberTypeKey = REGISTRATION_NUMBER then
      HiddenValue[DATA_TYPE, lIdx]                := IntToStr(CT_REGISTRATION)
    else
      HiddenValue[DATA_TYPE, lIdx]                := '0';
    HiddenValue['Is_Custom', lIdx]                := '2';
  end;
end;

{-------------------------------------------------------------------------------
  Must explicitly set all the HiddenValues of a new row to allow
  the move buttons to work.
}
procedure TfraQuickEntryManager.AddRow(AIndex: Integer);
begin
  HiddenValue['Field_Lookup_Key', AIndex] := '';
  HiddenValue[DATA_TYPE, AIndex] := '';
  HiddenValue['QE_Field_Key', AIndex] := '';
  HiddenValue[DEFAULT_VALUE, AIndex] := '';
  HiddenValue['Is_Custom', AIndex] := '';
  HiddenValue['Measurement_Applies_To', AIndex] := '';
  HiddenValue['Measurement_Accuracy', AIndex] := '';
  HiddenValue['Measurement_Duration', AIndex] := '';
  HiddenValue['Measurement_Parameter_Concept_Key', AIndex] := '';
  HiddenValue['Measurement_Unit_Concept_Key', AIndex] := '';
  HiddenValue['Measurement_Method_Concept_Key', AIndex] := '';
  HiddenValue['QE_Template_Field_Key', AIndex] := '';
  HiddenValue['Measurement_Is_Specimen', AIndex] := '';
  HiddenValue['Measurement_Is_TaxonData', AIndex] := '';
  HiddenValue['Taxon_Measurement_Qualifier_Key', AIndex] := '';
  HiddenValue['Taxon_Measurement_Unit_Key', AIndex] := '';
  HiddenValue['Table_Name', AIndex] := '';
  HiddenValue['Field_Name', AIndex] := '';
  HiddenValue['Number_Type_Concept_Key', AIndex] := '';
  HiddenValue['Number_Preferred', AIndex] := '';
  HiddenValue['Metadata_Type_Key', AIndex] := '';
  FDataStringGrid.TimestampCol['Timestamp', AIndex] := null;
end;

{-------------------------------------------------------------------------------
}
procedure TfraQuickEntryManager.AddSpecimenEventHandler(
    ASpecimenDataEntryControl: TSpecimenDataEntryControl);
begin
  // Use accessor class to assign OnExit for any control
  TWinControlAccessor(ASpecimenDataEntryControl.Control).OnExit := SpecimenEventHandlerExit
end;  // TfraQuickEntryManager.AddSpecimenEventHandler 

{-------------------------------------------------------------------------------
  Puts a dummy value into the insert wrapper for the field specified by a row
      in the grid.  This is used when using the insert wrapper to determine
      other fields that must be present on the template.
}
procedure TfraQuickEntryManager.ApplyDummyValueToFieldAtRow(ARow: integer;
    AWrapper: TInsertWrapper);
var
  lDummyValue: string;
begin
  if IsCustomRow(ARow) = 0 then begin
    if FSpecimenDataEntryControls[StrToInt(HiddenValue[DATA_TYPE , ARow])].DataType = 14 then
      lDummyValue := ResStr_No
    else
      lDummyValue := 'Unknown';
    AWrapper.Field[TTableName(StrToInt(HiddenValue['Table_Name', ARow])),
                   TFieldName(StrToInt(HiddenValue['Field_Name', ARow])), 0] := lDummyValue;
  end else if IsCustomRow(ARow) = 1 then
    AWrapper.AddMeasurement('','','','','','55','', '',
        (HiddenValue['Measurement_Is_Specimen', ARow] = STR_TRUE),
        (HiddenValue['Measurement_Is_TaxonData', ARow] = STR_TRUE), '', '');
end;  // TfraQuickEntryManager.ApplyDummyValueToFieldAtRow

{-------------------------------------------------------------------------------
  Adds a new empty template and sets in edit mode
}
procedure TfraQuickEntryManager.btnAddClick(Sender: TObject);
var
  lNewTemplate: TQuickEntryTemplate;
begin
  FLastTemplateIndex := lbTemplates.ItemIndex;
  //Adds a new empty template and sets in edit mode
  lNewTemplate := TQuickEntryTemplate.Create('','<' + ResStr_NewTemplate + '>',
                                             '', ttOccurrences,null, true);
  lbTemplates.AddItem('<' + ResStr_NewTemplate + '>', lNewTemplate);
  lbTemplates.ItemIndex := lbTemplates.Items.Count -1;
  lbTemplatesClick(lbTemplates);
  
  PopulateTemplateFields;
  btnEditClick(btnEdit);
end;  // TfraQuickEntryManager.btnAddClick 

{-------------------------------------------------------------------------------
  Display add measurement dialog allowing a measurement field to be added to
      those available.
}
procedure TfraQuickEntryManager.btnAddMeasurementClick(Sender: TObject);
var
  lDialog: TfrmQuickEntryMeasurements;
begin
  lDialog := TfrmQuickEntryMeasurements.Create(nil);
  with lDialog do
    try
      if cmbDataType.Text = ResStr_Occurrences then begin
        rgMeasurementRelatesTo.ItemIndex := 1;
        HideRadioGroup;
      end;
      lDialog.TaxonData := RowIsChecked(FindRowByQEFieldKey(QE_FIELD_TAXON_DETERMINATION));
      if ShowModal = mrOK then
        AddMeasurementRow(lDialog);
    finally
      Free;
    end;
end;  // TfraQuickEntryManager.btnAddMeasurementClick  

{-------------------------------------------------------------------------------
}
procedure TfraQuickEntryManager.btnAddMetadataClick(Sender: TObject);
var
  lDialog: TfrmQuickEntryMetadata;
  i: Integer;
begin
  lDialog := TfrmQuickEntryMetadata.Create(nil);
  with lDialog do
    try  
      for i := 1 to sgFields.RowCount - 1 do
        if HiddenValue['Metadata_Type_Key', i] <> '' then
          RemoveFromCombo(HiddenValue['Metadata_Type_Key', i]);
      if ShowModal = mrOK then
        AddMetadataRow(lDialog);
    finally
      Free;
    end;
end;  // TfraQuickEntryManager.btnaddMetadataClick

{-------------------------------------------------------------------------------
}
procedure TfraQuickEntryManager.btnAddNumberClick(Sender: TObject);
var
  lDialog: TfrmQuickEntryNumber;
  i: Integer;
begin
  lDialog := TfrmQuickEntryNumber.Create(nil);
  with lDialog do
    try
      for i := 1 to sgFields.RowCount - 1 do
        if HiddenValue['Number_Type_Concept_Key', i] <> '' then
          RemoveFromCombo(HiddenValue['Number_Type_Concept_Key', i]);
        // Reg number is a standard field, so we don't want to see it in the dialog.
        RemoveFromCombo(REGISTRATION_NUMBER);
      if ShowModal = mrOK then
        AddNumberRow(lDialog);
    finally
      Free;
    end;
end;  // TfraQuickEntryManager.btnAddNumberClick

{-------------------------------------------------------------------------------
}
procedure TfraQuickEntryManager.btnCancelClick(Sender: TObject);
begin
  EditMode := emBrowse;
  with lbTemplates do
    if TQuickEntryTemplate(Items.Objects[ItemIndex]).Key = '' then
    begin
      TQuickEntryTemplate(Items.Objects[ItemIndex]).Free;
      Items.Delete(ItemIndex);
      //reselect the previously chosen template
      ItemIndex := FLastTemplateIndex;
    end;
  lbTemplatesClick(lbTemplates);
end;  // TfraQuickEntryManager.btnCancelClick

{-------------------------------------------------------------------------------
}
procedure TfraQuickEntryManager.btnDeleteClick(Sender: TObject);
var
  lTemplate: TQuickEntryTemplate;
  lIndex: Integer;
begin
  lIndex := lbTemplates.ItemIndex;
  lTemplate := TQuickEntryTemplate(lbTemplates.Items.Objects[lbTemplates.ItemIndex]);
  if MessageDlg(Format(ResStr_TemplateDelete, [lbTemplates.Items[
      lbTemplates.ItemIndex]]),mtWarning, [mbYes,mbNo],0) = mrYes then
    try
      dmGeneral.RunDeleteStoredProc('usp_QETemplate_Delete',
                                    ['@QETemplateKey', ltemplate.Key,
                                     '@Timestamp', lTemplate.Timestamp]);
    finally
      //Select another template
      PopulateTemplates;
      if lbTemplates.Items.Count >0 then
      begin
        if lIndex >0 then
          lbTemplates.ItemIndex := lIndex -1
        else
          lbTemplates.ItemIndex := 0;
        lbTemplatesClick(lbTemplates);
      end;
    end;
end;  // TfraQuickEntryManager.btnDeleteClick 

{-------------------------------------------------------------------------------
}
procedure TfraQuickEntryManager.btnEditClick(Sender: TObject);
begin
  EditMode := emEdit;
end;  // TfraQuickEntryManager.btnEditClick

{-------------------------------------------------------------------------------
  Moves the selected row up one place in the grid.
}
procedure TfraQuickEntryManager.btnMoveUpClick(Sender: TObject);
begin
  inherited;
  if sgFields.Row > 1 then
    FDataStringGrid.MoveRow(sgFields.Row, sgFields.Row - 1);
end;

{-------------------------------------------------------------------------------
  Moves the selected row down one place in the grid.
}
procedure TfraQuickEntryManager.btnMoveDownClick(Sender: TObject);
begin
  inherited;
  if sgFields.Row < sgFields.RowCount - 1 then
    FDataStringGrid.MoveRow(sgFields.Row, sgFields.Row + 1);
end;

procedure TfraQuickEntryManager.sgFieldsRowMoved(Sender: TObject; FromIndex, ToIndex: Integer);
begin
  inherited;
  RefreshMoveButtons(sgFields.Row);
end;

{-------------------------------------------------------------------------------
  Disable the move up or down button if we're on the top or bottom row, and
  enable it if we move away from that row.
}
procedure TfraQuickEntryManager.sgFieldsSelectCell(Sender: TObject; ACol,
  ARow: Integer; var CanSelect: Boolean);
begin
  inherited;
  RefreshMoveButtons(ARow);
  sgFields.Invalidate;
end;

{-------------------------------------------------------------------------------
  Saves the changes to the template.
}
procedure TfraQuickEntryManager.btnSaveClick(Sender: TObject);
var
  lFieldsToCheck: string;
begin
  inherited;
  ValidateValue(eName.Text <> '', ResStr_TemplateNameRequired, eName);

  lFieldsToCheck := CheckDefaultValues(True);
  if lFieldsToCheck <> '' then
    if ConfirmYesNo(Format(ResStr_InvalidDefaultValues, [lFieldsToCheck])) = mrNo then
      Exit;
  CheckDefaultValues(False);

  if not CheckSubjectAreaAvailable then
    Exit;
  lFieldsToCheck := CheckNecessaryExtraFields(True);
  if lFieldsToCheck <> '' then begin
    if not CheckDeterminationTicked(lFieldsToCheck) then Exit;
    if ConfirmYesNo(Format(ResStr_FollowingFieldsAdded, [lFieldsToCheck])) = mrNo then
      Exit;
  end;
  CheckNecessaryExtraFields(False);
  try
    SaveTemplate;
  finally
    EditMode := emBrowse;
  end;
end;  // TfraQuickEntryManager.btnSaveClick 

{-------------------------------------------------------------------------------
  Changes the underlying field state and handles other events that occur when a
      checkbox is toggled.
}
procedure TfraQuickEntryManager.CheckboxStateChanged(ACol, ARow: integer);
var
  lstFieldKey: string;
  
  // Ensure that a given field is either checked or unchecked
  procedure EnsureChecked(AFieldKey: string; AState: boolean);
  var
    lRow: integer;
  begin
    lRow := FindRowByQEFieldKey(AFieldKey);
    if AState <> RowIsChecked(lRow) then begin
      if AState then
        // Enable on General tab
        sgFields.Cells[1, lRow] := STR_TRUE
      else begin
        // Disable both tabs
        sgFields.Cells[1, lRow] := STR_FALSE;
        sgFields.Cells[2, lRow] := STR_FALSE;
      end;
      sgFields.Invalidate;
    end;
  end;

begin
  if not FtfRowChanging then
  lstFieldKey := HiddenValue['QE_Field_Key', ARow];
  begin
    // Check each special field case
    if (lstFieldKey = QE_FIELD_TAXON_DETERMINATION) then begin
      if CheckMeasurementRows(False) then
        EnsureChecked(QE_FIELD_DETERMINATION, not RowIsChecked(FindRowByQEFieldKey(lstFieldKey)))
      else
        EnsureChecked(QE_FIELD_TAXON_DETERMINATION, False);
    end else
    if (lstFieldKey = QE_FIELD_DETERMINATION) then begin
      if CheckMeasurementRows(True) then
        EnsureChecked(QE_FIELD_TAXON_DETERMINATION,
                      not RowIsChecked(FindRowByQEFieldKey(lstFieldKey)))
      else
        EnsureChecked(QE_FIELD_DETERMINATION, False);
    end else
    if (lstFieldKey = QE_FIELD_ACQUISITION_REFERENCE_NUMBER) and (Checked(ACol, ARow)) then
    begin
      //find out whether Department is checked
      if RowIsChecked(FindRowByQEFieldKey(QE_FIELD_DEPARTMENT)) then
        //Department is checked, which means one of Accession Number or
        //Acquisition Reference Number must also be checked.
        if not RowIsChecked(FindRowByQEFieldKey(QE_FIELD_ACCESSION_NUMBER)) then
          CheckCell(ACol, ARow);
    end else
    if (lstFieldKey = QE_FIELD_ACCESSION_NUMBER) and (not Checked(ACol, ARow)) then
      //find out whether Department is checked
      if RowIsChecked(FindRowByQEFieldKey(QE_FIELD_DEPARTMENT)) then
        //Department is checked, which means one of Accession Number or
        //Acquisition Reference Number must also be checked.
        if not RowIsChecked(FindRowByQEFieldKey(QE_FIELD_ACQUISITION_REFERENCE_NUMBER)) then
          CheckCell(ACol, ARow);

    SetMeasurementState;
  end;// if not FtfRowChanging
  CheckHiddenAndLocked(ACol, ARow);
end;  // TfraQuickEntryManager.CheckboxStateChanged

{-------------------------------------------------------------------------------
  This procedure checks if the hidden or locked fields are checked, and enables
  or disables the other checkboxes accordingly.
}
procedure TfraQuickEntryManager.CheckHiddenAndLocked(ACol, ARow: Integer);
begin
  // Can only hide or lock if there is a default value or a number generation macro.
  if ((ACol = 5) or (ACol = 6)) and (HiddenValue[DEFAULT_VALUE, ARow] = '') then begin
    if (((HiddenValue[DATA_TYPE, ARow] <> IntToStr(CT_REGISTRATION))
        or (FRegistrationMacro = ''))
        and ((HiddenValue[DATA_TYPE, ARow] <> IntToStr(CT_ACCESSION)) or (FAccessionMacro = '')))
        then begin
      sgFields.Cells[ACol, ARow] := STR_FALSE;
      sgFields.Invalidate;
    end;
  end;
  // Can't lock the field if it doesn't appear on either of the tabs.
  if (Checked(6, ARow)) then begin
    if (not Checked(2, ARow)) and (not Checked(1, ARow)) then
      sgFields.Cells[6, ARow] := STR_FALSE;
    sgFields.Invalidate;
  end;
  // If hide is checked, uncheck the others.
  if (Checked(5, ARow)) then begin
    sgFields.Cells[2, ARow] := STR_FALSE;
    sgFields.Cells[1, ARow] := STR_FALSE;
    sgFields.Cells[6, ARow] := STR_FALSE;
    sgFields.Invalidate;
  end;
end;

{-------------------------------------------------------------------------------
  Checks a cell in the grid.
}
procedure TfraQuickEntryManager.CheckCell(ACol, ARow: integer);
begin
  sgFields.Cells[ACol, ARow] := STR_TRUE;
  sgFields.Invalidate;
end;  // TfraQuickEntryManager.CheckCell

{-------------------------------------------------------------------------------
  Checks that any entered default values are valid data, e.g. locations exist etc.
  If ATestOnly is true, constructs a string containing the fields with bad data.
  If AtestOnly is false, it resets the fields with bad default values to not having
  any default values.
}
function TfraQuickEntryManager.CheckDefaultValues(ATestOnly: Boolean): String;
var
  i: Integer;
begin
  Result := '';
  for i := 1 to sgFields.RowCount - 1 do
    if (HiddenValue[DEFAULT_VALUE, i] = '') and (sgFields.Cells[4, i] <> '') then begin
      if ATestOnly then begin
        try
          HiddenValue[DEFAULT_VALUE, i] := FSpecimenDataEntryControls[StrToInt(HiddenValue[DATA_TYPE , i])].ForceValue(0);
          sgFields.Cells[4, i] := FSpecimenDataEntryControls[StrToInt(HiddenValue[DATA_TYPE , i])].Display[0];
        except
          AddToCommaSeparatedList(Result, sgFields.Cells[0, i]);
        end;
      end else begin
        sgFields.Cells[4, i] := '';
      end;
    end;
end;

{-------------------------------------------------------------------------------
  This method checks that either Determination or Taxon Determination is checked. If they aren't, a warning message is displayed and the save is stopped.
}
function TfraQuickEntryManager.CheckDeterminationTicked(AFieldsToCheck:
    String): Boolean;
var
  lTable: TTableName;
  lRowIdx: Integer;
  lHiddenValue: string;
begin
  Result := True;
  
  // If either Determination or Taxon_Determination is found to be checked
  // it will leave the method immediately and its return value will be True.
  for lTable := Low(TTableName) to High(TTableName) do begin
    for lRowIdx := 1 to sgFields.RowCount - 1 do begin
      // Newly added measurements will have '' as their hidden value. Trying
      // to do StrToInt on '' will cause it to fall over, so check it isn't ''
      lHiddenValue := HiddenValue['Table_Name', lRowIdx];
      if (lHiddenValue <> '') and (TTableName(StrToInt(lHiddenValue)) = lTable) then
        if (HiddenValue['QE_Field_Key', lRowIdx] = QE_FIELD_TAXON_DETERMINATION)
           and RowIsChecked(lRowIdx) then
          Exit
        else
        if (HiddenValue['QE_Field_Key', lRowIdx] = QE_FIELD_DETERMINATION)
           and RowIsChecked(lRowIdx) then
          Exit;
    end;
  end;  // for lTable
  // It will only get this far if neither the Taxon_Determination or
  // Determination check boxes have been checked. In that event show a warning
  // message.
  MessageDlg(ResStr_TaxonDetOrDetChecked, mtWarning, [mbOK], 0);
  Result := False;
end;  // TfraQuickEntryManager.CheckDeterminationTicked 

{-------------------------------------------------------------------------------
  Returns true if the checkbox at the given grid cell is checked. 
}
function TfraQuickEntryManager.Checked(ACol, ARow: integer): Boolean;
begin
  Result := (sgFields.Cells[ACol, ARow] = STR_TRUE);
end;  // TfraQuickEntryManager.Checked 

{-------------------------------------------------------------------------------
  Checks the fields that are necessarily present when the already existing
      fields are present.
  If ATestOnly is True, then just constructs a string describing the fields
      that will be checked.
}
function TfraQuickEntryManager.CheckNecessaryExtraFields(ATestOnly: boolean):
    string;
var
  lTable: TTableName;
  lRowIdx: Integer;
  lInsertWrapper: TInsertWrapper;
  lInvalidFields: TInvalidFields;
begin
  lInsertWrapper := TInsertWrapper.Create;
  lInsertWrapper.Occurrences := cmbDataType.Text = ResStr_Occurrences;
  Result := '';
  try
    // First, find the fields that are selected and set a default value
    // so they pass validation
    for lRowIdx := 1 to sgFields.RowCount - 1 do
      if RowIsChecked(lRowIdx) then
        ApplyDummyValueToFieldAtRow(lRowIdx, lInsertWrapper);
    // Now ask the wrapper to validate the fields
    lInvalidFields := lInsertWrapper.Validate;
    // Now, set these fields on the General tab
    for lTable := Low(TTableName) to High(TTableName) do begin
      if lInvalidFields[lTable]<>[] then
        for lRowIdx := 1 to sgFields.RowCount-1 do
          // Check not a measurement row
          if (HiddenValue['Table_Name', lRowIdx] <> '') and
             (HiddenValue['Field_Name', lRowIdx] <> '') then begin
            if (TTableName(StrToInt(HiddenValue['Table_Name', lRowIdx])) = lTable) and
               (TFieldName(StrToInt(HiddenValue['Field_Name', lRowIdx]))
                                                    in lInvalidFields[lTable])
            then begin
              AddToCommaSeparatedList(Result, sgFields.Cells[0, lRowIdx]);
              // As we are adding a new template, we want both the Determination
              // and Taxon_Determination check boxes not to be checked.
              // If the user tries to save with both unchecked, then validation
              // will catch it.
              if not ((HiddenValue['QE_Field_Key', lRowIdx] = QE_FIELD_TAXON_DETERMINATION) or
                      (HiddenValue['QE_Field_Key', lRowIdx] = QE_FIELD_DETERMINATION)) and
                 not ATestOnly then
                CheckCell(1, lRowIdx);
            end;
          end;
    end;
  finally
    lInsertWrapper.Free;
  end;
end;  // TfraQuickEntryManager.CheckNecessaryExtraFields 

{-------------------------------------------------------------------------------
  Checks that the subject area selected for a template is available for the
      user.  If not, then displays a warning allowing the user to cancel the
      save operation.  returns True if save operation can proceed.
}
function TfraQuickEntryManager.CheckSubjectAreaAvailable: Boolean;
var
  lCanSeeSubjectArea: Boolean;
begin
  Result := True; // default
  // Is a subject area selected?
  if cmbSubjectArea.ItemIndex>-1 then begin
    // If so, can the user see it
    lCanSeeSubjectArea := dmGeneral.GetStoredProcOutputParam(
        'usp_SubjectAreaAvailable_Get',
        ['@Key', cmbSubjectArea.CurrentStrID],
        '@IsAvailable');
    if not lCanSeeSubjectArea then
      Result := ConfirmYesNo(ResStr_SelectedSubjectAreaNotAvailable)=mrYes
  end;
end;  // TfraQuickEntryManager.CheckSubjectAreaAvailable 

{-------------------------------------------------------------------------------
  Clears the list of templates and associated classes. 
}
procedure TfraQuickEntryManager.ClearTemplates;
var
  lIdx: Integer;
begin
  for lIdx := 1 to lbTemplates.Items.Count - 1 do
    TQuickEntryTemplate(lbTemplates.Items.Objects[lIdx]).Free;
  lbTemplates.Clear;
  eName.Text := '';
  cmbDataType.ItemIndex := -1;
  cmbSubjectArea.ItemIndex := -1;
end;  // TfraQuickEntryManager.ClearTemplates

{-------------------------------------------------------------------------------
}
procedure TfraQuickEntryManager.cmbDataTypeChange(Sender: TObject);
begin
  if cmbDataType.Text = ResStr_Specimens then
  begin
    FDataStringGrid.DisplayName[2] := ResStr_VisibleOnSpecimenTab;
    btnAddMeasurement.Enabled := True;
    btnAddNumber.Enabled := True;
    btnAddMetadata.Enabled := True;
    PopulateTemplateFields;
  end else
  if cmbDataType.Text = ResStr_Occurrences then
  begin
    FDataStringGrid.DisplayName[2] := ResStr_VisibleOnOccurrenceTab;
    btnAddMeasurement.Enabled := True;
    btnAddNumber.Enabled := False;
    btnaddMetadata.Enabled := False;
    PopulateTemplateFields;
  end;
end;  // TfraQuickEntryManager.cmbDataTypeChange 

{-------------------------------------------------------------------------------
}
procedure TfraQuickEntryManager.CMChildKey(Var msg: TCMChildKey);
begin
  if Assigned(FCurrentGridControl) then
    if (msg.Sender = FCurrentGridControl) or
        (FCurrentGridControl.ContainsControl(msg.Sender)) then
    begin
      Case msg.CharCode of
        VK_LEFT, VK_RIGHT, VK_UP, VK_Down:
        begin
          GridControlKeyDown(FCurrentGridControl, msg.Charcode, []);
          If msg.CharCode = 0 then
            msg.Result := 1;
        end;
      end;
    end;
end;  // TfraQuickEntryManager.CMChildKey

{-------------------------------------------------------------------------------
  Returns the index of the row with the given key. 
}
function TfraQuickEntryManager.FindRowByQEFieldKey(lQEFieldKey: string):
    Integer;
var
  i: Integer;
begin
  result := -1;
  for i := 1 to sgFields.RowCount - 1 do
    if HiddenValue['QE_Field_Key', i] = lQEFieldKey then
    begin
      result := i;
      break;
    end;
end;  // TfraQuickEntryManager.FindRowByQEFieldKey

{-------------------------------------------------------------------------------
}
function TfraQuickEntryManager.GetHiddenValue(AName: string; ARow: Integer):
    string;
begin
  Result := FDataStringGrid.InvisibleColValue[AName, ARow];
end;  // TfraQuickEntryManager.GetHiddenValue 

{-------------------------------------------------------------------------------
}
procedure TfraQuickEntryManager.GridCanEditCell(var ioAccept: Boolean; ACol, ARow: Integer);
begin
  ioAccept := (ACol >= sgFields.FixedCols) and (ARow >= sgFields.FixedRows) and
              ((HiddenValue['QE_Field_Key', ARow] <> QE_FIELD_FIELD_RECORD_TYPE) or
               (sgFields.Cells[ACol, 0] <> ResStr_VisibleOnGeneralTab));

  // Hidden and Locked checkboxes are only enabled if there is a default value
  // or a macro for the number
  if ((ACol = 5) or (ACol = 6)) and (HiddenValue[DEFAULT_VALUE, ARow] = '') then
    if (((HiddenValue[DATA_TYPE, ARow] <> IntToStr(CT_REGISTRATION))
        or (FRegistrationMacro = '')) and
        ((HiddenValue[DATA_TYPE, ARow] <> IntToStr(CT_ACCESSION)) or (FAccessionMacro = '')))
        then
      ioAccept := False;
  // Additionally, locked is only enabled if one of the "On this tab" checkboxes is checked
  if ((ACol = 6) and (not Checked(1, ARow)) and (not Checked(2, ARow))) then
    ioAccept := False;
  // If hidden is checked, none of the others are allowed to be.
  if (((ACol = 1) or (ACol = 2) or (ACol = 6)) and (Checked(5, ARow))) then
    ioAccept := False;
end;  // TfraQuickEntryManager.GridCanEditCell

{-------------------------------------------------------------------------------
  Implements the key down for the grid 
}
procedure TfraQuickEntryManager.GridControlKeyDown(Sender: TObject; Var Key:
    Word;Shift: TShiftState);

    //This function works out whether left and right arrows should
  //cause the cell to change.
  function ArrowsShouldWork(lKey: Word): boolean;
  begin
    result := False;
    if Sender is TCustomEdit then
      with TCustomEdit(Sender) do
      begin
        if (SelLength = Length(Text)) or
            (((lKey = VK_Right) and (SelStart = Length(Text))) or
            ((lKey = VK_Left) and (SelStart = 0) and (selLength = 0))) then
          result := true;
      end
    else if Sender is TLinkedEdit then
      with TLinkedEdit(sender) do
      begin
        if (SelLength = Length(Text)) or
            (((lKey = VK_Right) and (SelStart = Length(Text))) or
            ((lKey = VK_Left) and (SelStart = 0) and (selLength = 0))) then
          result := true;
      end
    else if Sender is TLuxIDComboBox then
      with TLuxIDComboBox(Sender) do
      begin
        if not DroppedDown then
          result := true;
      end
    else if Sender is TCheckBox then
      result := true;
  end;

begin
  case Key of
  Vk_Right:
    if ArrowsShouldWork(Key) then
      if sgFields.Col < sgFields.ColCount - 1 then
      begin
        SpecimenEventHandlerExit(Sender);
        sgFields.Col := sgFields.Col + 1;
        Key := 0;
      end
      else if sgFields.Row < sgFields.RowCount - 1 then
      begin
        SpecimenEventHandlerExit(Sender);
        sgFields.Col := 1;
        sgFields.Row := sgFields.Row + 1;
        Key := 0;
      end;
  VK_Left:
    if ArrowsShouldWork(Key) then
      if sgFields.Col > 1 then
      begin
        SpecimenEventHandlerExit(Sender);
        sgFields.Col := sgFields.Col - 1;
        Key := 0;
      end
      else if sgFields.Row > 1 then
      begin
        SpecimenEventHandlerExit(Sender);
        sgFields.Col := sgFields.ColCount - 1;
        sgFields.Row := sgFields.Row - 1;
        Key := 0;
      end;
  VK_UP:
    if not (Sender is TLuxIDComboBox) then
      if sgFields.Row > 1 then
      begin
        SpecimenEventHandlerExit(Sender);
        sgFields.Row := sgFields.Row - 1;
        Key := 0;
      end;
  VK_Down:
    if not (Sender is TLuxIDComboBox) then
    begin
      if sgFields.Row < sgFields.RowCount - 1 then
      begin
        SpecimenEventHandlerExit(Sender);
        sgFields.Row := sgFields.Row + 1;
        Key := 0;
      end;
    end;
  end;
  if sgFields.Col = 3 then //the column without custom edit
    if sgFields.CanFocus then
      sgFields.SetFocus;
end;  // TfraQuickEntryManager.GridControlKeyDown 

{-------------------------------------------------------------------------------
  Puts controls on the grid so that cells appear to be edited using them.
      Checkboxes are placed on the grid for the two check box columns, and for
      the default value column, the control placed on the form depends on the
      type of field.
}
procedure TfraQuickEntryManager.GridCustomEditCell(Panel: TPanel; ACol, ARow:
    Integer);
begin
  if not FDataStringGrid.Readonly then begin
    if IsCustomRow(ARow) = 1 then
      if RowIsChecked(FindRowByQEFieldKey(QE_FIELD_TAXON_DETERMINATION)) then
        if HiddenValue['Measurement_Is_Specimen', ARow] <> STR_TRUE then
        begin
          Panel.Visible := false;
          Exit;
        end;

    if (FCurrentGridControl <> nil) then
      FCurrentGridControl.Visible := False;

    if ACol = 4 then //default value column
    begin
      Panel.BorderStyle := bsNone;
      with FSpecimenDataEntryControls[StrToInt(HiddenValue[DATA_TYPE , ARow])] do
      begin
        FCurrentGridControl := Control;
        if DataType = 5 then//concept combo box
          if FieldLookupKey <> HiddenValue['Field_Lookup_Key', ARow] then
          begin //repopulate if lookup key has changed.
            FieldLookupKey := HiddenValue['Field_Lookup_Key', ARow];
            RegisterConceptGroupComboBox(TConceptGroupComboBox(Control), FieldLookupKey);
          end;
        Value[0] := HiddenValue[DEFAULT_VALUE, ARow];
        Display[0] := sgFields.Cells[ACol,ARow];
      end;

      with FCurrentGridControl do
      begin
        Top := 0;
        Width := Panel.ClientWidth;
        Left := 0;
        Height := Panel.ClientHeight;
        Visible := true;
        if CanFocus then SetFocus;
      end;
    end;//if ACol = 4
  end;
end;  // TfraQuickEntryManager.GridCustomEditCell 

{-------------------------------------------------------------------------------
  Loads data from database. This code appears in this public method rather than
      the Create or Initialise procedures because it should not be run until
      the frame is visible.
}
procedure TfraQuickEntryManager.Initialise;
begin
  FtfRowChanging := False;
  cmbSubjectArea.NoSelectionItemText := ResStr_NoSelection;
  cmbSubjectArea.OnPopulate := dmQuickEntry.FillSubjectAreaCombo;
  
  cmbDataType.AddItem(ResStr_Specimens, nil);
  cmbDataType.AddItem(ResStr_Occurrences, nil);

  //set up grid
  FDataStringGrid := TDataStringGrid.Create(sgFields);
  with FDataStringGrid do
  begin
    AddColumn(ResStr_Field, 'Field', 100, adVarChar, False, true);
    AddColumn(ResStr_VisibleOnGeneralTab, 'General_Tab', 115, adBoolean, true, true);
    AddColumn(ResStr_VisibleOnSpecimenTab, 'Specimen_Tab', 125, adBoolean, true, true);
    AddColumn(ResStr_DisplayAltName, 'Alternative_Name', 140, adVarChar, true, true);
    AddColumn(Resstr_DefaultValue, 'Default_Display', 140, adVarchar, true, false);
    AddColumn('Hide', 'Hidden', 120, adBoolean, true, true);
    AddColumn('Locked', 'Locked', 120, adBoolean, true, true);
    CustomEdit[4] := true;
    AddInvisibleCol('Field_Lookup_Key');
    AddInvisibleCol(DATA_TYPE);
    AddInvisibleCol('QE_Field_Key');
    AddInvisibleCol(DEFAULT_VALUE);
    AddInvisibleCol('Is_Custom');
    AddInvisibleCol('Measurement_Applies_To');
    AddInvisibleCol('Measurement_Accuracy');
    AddInvisibleCol('Measurement_Duration');
    AddInvisibleCol('Measurement_Parameter_Concept_Key');
    AddInvisibleCol('Measurement_Unit_Concept_Key');
    AddInvisibleCol('Measurement_Method_Concept_Key');
    AddInvisibleCol('QE_Template_Field_Key');
    AddInvisibleCol('Measurement_Is_Specimen');
    AddInvisibleCol('Measurement_Is_TaxonData');
    AddInvisibleCol('Taxon_Measurement_Qualifier_Key');
    AddInvisibleCol('Taxon_Measurement_Unit_Key');
    AddTimestampCol('Timestamp');
    AddInvisibleCol('Table_Name');
    AddInvisibleCol('Field_Name');
    AddInvisibleCol('Number_Type_Concept_Key');
    AddInvisibleCol('Number_Preferred');
    AddInvisibleCol('Metadata_Type_Key');
    OnCustomEditCell := GridCustomEditCell;
    OnCheckBoxClick  := CheckboxStateChanged;
    OnCanEditCell    := GridCanEditCell;
    OnCellBGColour  := GetCellColour;
  end; //with FDataStringGrid do

  // The first AddColumn resets ColCount to 1, which resets FixedCols to 0!!!
  sgFields.FixedCols := 1;

  FEditMode := emBrowse;
  PopulateTemplates;
  InitialiseDataEntryControls;
  
  cmbSubjectArea.Text := '<' + ResStr_Unrestricted + '>';
  if lbTemplates.Items.Count > 0 then
  begin
    lbTemplates.ItemIndex := 0;
    lbTemplatesClick(lbTemplates);
  end;

  FDiscardedMeasurements := TObjectList.Create;
  FRegistrationMacro := GetMacro('Registration');
  FAccessionMacro := GetMacro('Accession');
end;  // TfraQuickEntryManager.Initialise

{-------------------------------------------------------------------------------
}
procedure TfraQuickEntryManager.InitialiseDataEntryControls;
var
  i: Integer;
begin
  for i := 0 to High(FSpecimenDataEntryControls) do
  begin
    FSpecimenDataEntryControls[i] := TSpecimenDataEntryControl.Create(Self);
    with FSpecimenDataEntryControls[i] do begin
      Visible := False;
      Parent := Self;
      DataType := i;
      SetupDataEntryControl(False);
      Control.Parent := FDataStringGrid.CustomEditPanel;
      if Control is TLinkedEdit then
        TLinkedEdit(Control).ShowDragDropBorder := False;
    end;
    AddSpecimenEventHandler(FSpecimenDataEntryControls[i]);
  end;
end;  // TfraQuickEntryManager.InitialiseDataEntryControls 

{-------------------------------------------------------------------------------
}
procedure TfraQuickEntryManager.InsertField(ARow: Integer; ASequence: Integer;
    const ATemplateKey: String);
begin
  with sgFields do
    if (Cells[0, ARow] <> '') then
      dmGeneral.RunInsertStoredProc('QE_Template_Field',
        'usp_QETemplateField_Insert',
        ['@Key', null,
         '@GeneralTab', (CompareText(Cells[1, ARow], STR_TRUE) = 0),
         '@SpecimenTab', (CompareText(Cells[2, ARow], STR_TRUE) = 0),
         '@ItemName', Cells[3, ARow],
         '@DefaultValue', HiddenValue[DEFAULT_VALUE, ARow],
         '@DefaultDisplay', Cells[4, ARow],
         '@QEFieldKey', HiddenValue['QE_Field_Key', ARow],
         '@QETemplateKey', ATemplateKey,
         '@IsCustom', IsCustomRow(ARow),
         '@Sequence', ASequence,
         '@MeasurementAppliesTo', HiddenValue['Measurement_Applies_To', ARow],
         '@MeasurementDuration', HiddenValue['Measurement_Duration', ARow],
         '@MeasurementAccuracy', HiddenValue['Measurement_Accuracy', ARow],
         '@MeasurementParameterConceptKey',
             HiddenValue['Measurement_Parameter_Concept_Key', ARow],
         '@MeasurementMethodConceptKey',
             HiddenValue['Measurement_Method_Concept_Key', ARow],
         '@MeasurementUnitConceptKey',
             HiddenValue['Measurement_Unit_Concept_Key', ARow],
         '@MeasurementIsSpecimen',
             HiddenValue['Measurement_Is_Specimen', ARow] = STR_TRUE,
         '@TaxonMeasurementQualifierKey',
             HiddenValue['Taxon_Measurement_Qualifier_Key', ARow],
         '@TaxonMeasurementUnitKey',
             HiddenValue['Taxon_Measurement_Unit_Key', ARow],
         '@MeasurementIsTaxonData',
             HiddenValue['Measurement_Is_TaxonData', ARow] = STR_TRUE,
         '@NumberTypeConceptKey',
             HiddenValue['Number_Type_Concept_Key', ARow],
         '@NumberPreferred',
             HiddenValue['Number_Preferred', ARow] = STR_TRUE,
         '@Hidden', (CompareText(sgFields.Cells[5, ARow], STR_TRUE) = 0),
         '@Locked', (CompareText(sgFields.Cells[6, ARow], STR_TRUE) = 0),
         '@MetadataTypeKey',
             HiddenValue['Metadata_Type_Key', ARow]
        ],
        '@Key');
end;  // TfraQuickEntryManager.InsertField

{-------------------------------------------------------------------------------
  Returns true if the row refers to a measurement field.
}
function TfraQuickEntryManager.IsCustomRow(ARow: integer): Integer;
begin
  if HiddenValue[IS_CUSTOM, ARow] = '' then
    Result := 0
  else
    Result := StrToInt(HiddenValue[IS_CUSTOM, ARow]);
end;  // TfraQuickEntryManager.IsCustomRow

{-------------------------------------------------------------------------------
  Returns true if a macro is asigned to auto-generate regiatration numbers.
}
function TfraQuickEntryManager.GetMacro(ANumberType: String): String;
var
  lOutput: String;
begin
  lOutput := dmGeneral.GetStoredProcOutputParam('usp_Macro_Get',
        ['@NumberType', ANumberType,
        '@Dept', '#' + ResStr_Unknown + '#'],
        '@Macro');
  Result := lOutput;
end;

{-------------------------------------------------------------------------------
  When selecting a template, display the details.
}
procedure TfraQuickEntryManager.lbTemplatesClick(Sender: TObject);
var
  lTemplate: TQuickEntryTemplate;
begin
  if lbTemplates.ItemIndex >=0 then
  begin
    // Ensure details are up to date
    lTemplate := TQuickEntryTemplate(lbTemplates.Items.Objects[lbTemplates.ItemIndex]);
    RefreshTemplateDetails(lTemplate, lbTemplates.ItemIndex);
    eName.Text := lbTemplates.Items[lbTemplates.ItemIndex];
    with lTemplate do
    begin
      case templatetype of
        ttSpecimens:
          begin
            cmbDataType.ItemIndex:=0;
            FDataStringGrid.DisplayName[2] := ResStr_VisibleOnSpecimenTab;
          end;
        ttOccurrences:
          begin
            cmbDataType.ItemIndex := 1;
            FDataStringGrid.DisplayName[2] := ResStr_VisibleOnOccurrenceTab;
          end;
      end;
      if not cmbSubjectArea.Populated then
        cmbSubjectArea.PopulateContent;
      cmbSubjectArea.ItemIndex := cmbSubjectArea.IDIndexOf(SubjectAreaKey);
      btnEdit.Enabled := true;
      btnDelete.Enabled := CanDelete;
    end;
    PopulateTemplateFields;
  end;
end;  // TfraQuickEntryManager.lbTemplatesClick

{-------------------------------------------------------------------------------
  Populates the grid with template fields. 
}
procedure TfraQuickEntryManager.PopulateTemplateFields;
var
  i: Integer;
  lrs: _Recordset; 
  lTaxonRow, lThesaurusRow: Integer;
begin
  lrs := dmGeneral.GetRecordset('usp_QETemplateField_Select_ForTemplate',
                  ['@QETemplateKey',
                   TQuickEntryTemplate(lbTemplates.Items.Objects[lbTemplates.ItemIndex]).Key,
                   '@TemplateType',
                   IfThen(cmbDataType.Text = ResStr_Occurrences, 6, 1)]);
  try
    FDataStringGrid.PopulateGrid(lrs);
    //change boolean default values into yes or no strings
    for i := 1 to sgFields.RowCount - 1 do
    begin
      if (StrToInt(HiddenValue[DATA_TYPE, i]) = 14) then begin
        sgFields.Cells[4,i] := IfThen(STR_TRUE = HiddenValue[DEFAULT_VALUE, i],
                                      ResStr_Yes, ResStr_No);
        HiddenValue[DEFAULT_VALUE, i] := IfThen(STR_TRUE = HiddenValue[DEFAULT_VALUE, i],
                                      STR_TRUE, STR_FALSE);
      end;
      //replace strings with codes for table & field names for later use in validation
      if HiddenValue['Table_Name', i]<> '' then
        HiddenValue['Table_Name', i] :=
            IntToStr(Integer(StringToTableName(HiddenValue['Table_Name', i])));
      if HiddenValue['Field_Name', i] <> '' then
        HiddenValue['Field_Name', i] :=
            IntToStr(Integer(StringToFieldName(HiddenValue['Field_Name',  i])));
    end;
    sgFields.FixedCols := 1; 
    lTaxonRow := FindRowByQEFieldKey(QE_FIELD_TAXON_DETERMINATION);
    lThesaurusRow := FindRowByQEFieldKey(QE_FIELD_DETERMINATION);
    if (not RowIsChecked(lTaxonRow)) and (not RowIsChecked(lThesaurusRow)) then
      CheckCell(1, lThesaurusRow);
  finally
    lrs.Close;
  end;
end;  // TfraQuickEntryManager.PopulateTemplateFields 

{-------------------------------------------------------------------------------
  Populates the template list box with templates, and creates objects
      containing details of the templates to attach to the list box items.
}
procedure TfraQuickEntryManager.PopulateTemplates;
var
  lTemplateType: TTemplateType;
  ltfOldNullStrictConvert: Boolean;
begin
  // convert subject area key to empty string if null
  ClearTemplates;
  ltfOldNullStrictConvert := NullStrictConvert;
  NullStrictConvert := false;
  try
    with dmGeneral.GetRecordset('usp_QETemplates_Select', []) do
      try
        while not Eof do begin
          //Fill the list box with the names of templates and fill the
          //objects with TQuickEntryTemplate objects filled with the other
          // properties of the templates.
          if Fields['Template_Type'].Value = 1 then
            lTemplateType := ttSpecimens
          else
            lTemplateType := ttOccurrences;
          lbTemplates.AddItem(Fields['Item_Name'].Value,
                TQuickEntryTemplate.Create(Fields['QE_Template_Key'].Value,
                                           Fields['Item_Name'].Value,
                                           Fields['Subject_Area_Key'].Value,
                                           lTemplateType,
                                           TSQLSvrTimestamp(Fields['timestamp'].Value),
                                           Fields['CanDelete'].Value));
          MoveNext;
        end;
      finally
        Close;
      end;
  finally
    NullStrictConvert := ltfOldNullStrictConvert;
  end;
  sgFields.RowCount := 2;
  sgFields.Rows[1].Clear;
  //refresh state of buttons Add edit & delete buttons
  EditMode := EditMode;
end;  // TfraQuickEntryManager.PopulateTemplates

{-------------------------------------------------------------------------------
  Checks if either the move up or move down button should be disabled.
}
procedure TfraQuickEntryManager.RefreshMoveButtons(ARow: Integer);
begin  
  btnMoveUp.Enabled   := (ARow > 1) and (EditMode = emEdit);
  btnMoveDown.Enabled := (ARow < sgFields.RowCount - 1) and (EditMode = emEdit);
end;

{-------------------------------------------------------------------------------
  Refreshes the basic details of a template. 
}
procedure TfraQuickEntryManager.RefreshTemplateDetails(ATemplate:
    TQuickEntryTemplate; AIndex: integer);
begin
  if ATemplate.Key <> '' then
    with dmGeneral.GetRecordset('usp_QETemplate_Select', ['@Key', ATemplate.Key]) do
    begin
      // Update caption in list box
      lbTemplates.Items[AIndex] := Fields['Item_Name'].Value;
      // Update template instance details
      ATemplate.SubjectAreaKey := VarToStr(Fields['Subject_Area_Key'].Value);
      ATemplate.Timestamp := TSQLSvrTimestamp(Fields['timestamp'].Value);
      if Fields['Template_Type'].Value = 1 then
        ATemplate.TemplateType := ttSpecimens
      else
        ATemplate.TemplateType := ttOccurrences;
      ATemplate.CanDelete := Fields['CanDelete'].Value;
    end;
end;  // TfraQuickEntryManager.RefreshTemplateDetails 

{-------------------------------------------------------------------------------
}
procedure TfraQuickEntryManager.RegisterDragDropComponents;
var
  i: Integer;
begin
  inherited;
  for i := 0 to High(FSpecimenDataEntryControls) do
    FSpecimenDataEntryControls[i].RegisterLinkedEditControl(Self);
  {TODO: Sort out dropping for the grid itself}
end;  // TfraQuickEntryManager.RegisterDragDropComponents 

{-------------------------------------------------------------------------------
  Returns if either the general or specimen or hidden cell is checked for a given row.
}
function TfraQuickEntryManager.RowIsChecked(lRow: integer): Boolean;
begin
  Result := (sgFields.Cells[1, lRow] = STR_TRUE) or
            (sgFields.Cells[2, lRow] = STR_TRUE) or
            (sgFields.Cells[5, lRow] = STR_TRUE);
end;  // TfraQuickEntryManager.RowIsChecked 

{-------------------------------------------------------------------------------
  Saves the template 
}
procedure TfraQuickEntryManager.SaveTemplate;
var
  lTemplate: TQuickEntryTemplate;
  lIndex: Integer;
  i: Integer;
  lSequence: Integer;
  lstDeletedList, lstUpdatedList: string;
begin
  lTemplate := TQuickEntryTemplate(lbTemplates.Items.Objects[lbTemplates.ItemIndex]);
  try
    if lTemplate.Key = '' then //add
      lTemplate.Key := dmGeneral.RunInsertStoredProc('QE_Template',
          'usp_QETemplate_Insert',
          ['@Key', null,
           '@ItemName', eName.Text,
           '@TemplateType', ifthen(cmbDataType.Text = resStr_Occurrences, 0,1),
           '@SubjectAreaKey', cmbSubjectArea.CurrentStrID],
           '@Key')
    else //edit
      dmGeneral.RunUpdateStoredProc('usp_QETemplate_Update',
          ['@Key', lTemplate.Key,
          '@Timestamp', lTemplate.Timestamp,
          '@ItemName', eName.Text,
          '@TemplateType', ifthen(cmbDataType.Text = resStr_Occurrences, 0,1),
          '@SubjectAreaKey', cmbSubjectArea.CurrentStrID]);
    lSequence := 0;
    lbTemplates.Items[lbTemplates.ItemIndex] := eName.Text;
    for i := 1 to sgFields.RowCount - 1 do
    begin
      if HiddenValue['QE_Template_Field_Key', i]<> '' then
      begin
        // existing row
        if not RowIsChecked(i) then begin
          {Delete fields which have both General_Tab and Specimen_Tab
          unchecked}
          if HiddenValue['QE_Template_Field_Key', i] <> '' then
            dmGeneral.RunDeleteStoredProc('usp_QETemplateField_Delete',
                ['@Timestamp', FDataStringGrid.TimestampCol['Timestamp', i],
                 '@QETemplateFieldKey', HiddenValue['QE_Template_Field_Key', i]]);
        end
        else begin
          try
            //update
            dmGeneral.RunUpdateStoredProc('usp_QETemplateField_Update',
                ['@GeneralTab', (CompareText(sgFields.Cells[1,i], STR_TRUE) = 0),
                '@SpecimenTab', (CompareText(sgFields.Cells[2,i], STR_TRUE) = 0),
                '@ItemName', sgFields.Cells[3, i],
                '@DefaultValue', HiddenValue[DEFAULT_VALUE,i],
                '@DefaultDisplay', sgFields.Cells[4,i],
                '@Timestamp', FDataStringGrid.TimestampCol['Timestamp', i],
                '@Key', HiddenValue['QE_Template_Field_Key', i],
                '@Sequence', lSequence,
                '@NumberTypeConceptKey', HiddenValue['Number_Type_Concept_Key', i],
                '@NumberPreferred', HiddenValue['Number_Preferred', i],
                '@Hidden', (CompareText(sgFields.Cells[5,i], STR_TRUE) = 0),
                '@Locked', (CompareText(sgFields.Cells[6,i], STR_TRUE) = 0),
                '@MetadataTypeKey', HiddenValue['Metadata_Type_Key', i]]);
          except
            on EUpdatedException do
              lstUpdatedList := lstUpdatedList  + ', ' + sgFields.Cells[3,i];
            on EDeletedException do
              lstUpdatedList := lstDeletedList + ', ' + sgFields.Cells[3,i];
          end
        end;
      end
      else //insert
        if RowIsChecked(i) then
          InsertField(i, lSequence, lTemplate.Key);
      if RowIsChecked(i) then Inc(lSequence);
    end;
    // Remove the discarded measurement fields too.
    for i := 0 to FDiscardedMeasurements.Count - 1 do
      with TQEFieldInfo(FDiscardedMeasurements[i]) do
            dmGeneral.RunDeleteStoredProc('usp_QETemplateField_Delete',
                ['@Timestamp', Timestamp, '@QETemplateFieldKey', TemplateFieldKey]);
    FDiscardedMeasurements.Clear;

    RefreshTemplateDetails(lTemplate, lbTemplates.ItemIndex);
    PopulateTemplateFields;
    if lstUpdatedList<> '' then
      raise EUpdatedException.CreateNonCritical(Format(ResStr_RecordsUpdated,
            [RightStr(lstUpdatedList, StrLen(PAnsiChar(lstUpdatedList)))]));
    if lstDeletedList<> '' then
      raise EUpdatedException.CreateNonCritical(Format(ResStr_RecordsDeleted,
            [RightStr(lstUpdatedList, StrLen(PAnsiChar(lstDeletedList)))]));
  except
    on EUpdatedException do begin
      // Record updated by other user, so refresh on screen details.
      lbTemplatesClick(nil);
      raise;
    end;
    on EDeletedException do begin
      // Record delete by other user, so remove details
      lIndex := lbTemplates.ItemIndex-1;
      PopulateTemplates;
      // Select template adjacent to deleted one
      lbTemplates.ItemIndex := Max(Min(lIndex, lbTemplates.Items.Count-1), 0);
      lbTemplatesClick(nil);
      lbTemplates.Repaint;
      raise;
    end;
  end;
end;  // TfraQuickEntryManager.SaveTemplate 

{-------------------------------------------------------------------------------
  Enables or disables controls depending on whether a template is being edited. 
}
procedure TfraQuickEntryManager.SetEditMode(const Value: TEditMode);
begin
  FEditMode := Value;
  lbTemplates.Enabled       := FEditMode = emBrowse;
  btnAdd.Enabled            := FEditMode = emBrowse;
  btnEdit.Enabled           := (FEditMode = emBrowse) and (lbTemplates.ItemIndex >= 0);
  btnDelete.Enabled         := btnEdit.Enabled;
  eName.Enabled             := FEditMode = emEdit;
  cmbDataType.Enabled       := FEditMode = emEdit;
  cmbSubjectArea.Enabled    := FEditMode = emEdit;
  FDataStringGrid.Readonly  := FEditMode = emBrowse;
  btnCancel.Enabled         := FEditMode = emEdit;
  btnSave.Enabled           := FEditMode = emEdit;
  btnAddMeasurement.Enabled := FEditMode = emEdit;
  btnAddNumber.Enabled      := (FEditMode = emEdit) and (cmbDataType.Text = ResStr_Specimens);
  btnAddMetadata.Enabled      := (FEditMode = emEdit) and (cmbDataType.Text = ResStr_Specimens);
  RefreshMoveButtons(sgFields.Row);
  // Ensure focused row is displayed properly when going into Edit Mode
  sgFields.Invalidate;
  if FEditMode = emEdit then
    sgFields.Options := sgFields.Options + [goRowMoving]
  else
    sgFields.Options := sgFields.Options - [goRowMoving];
  
  if FEditMode = emBrowse then
    FDataStringGrid.HideCustomEdit
  else
    eName.SetFocus;
end;  // TfraQuickEntryManager.SetEditMode 

{-------------------------------------------------------------------------------
}
procedure TfraQuickEntryManager.SetHiddenValue(AName: string; ARow: Integer;
    const Value: string);
begin
  FDataStringGrid.InvisibleColValue[AName, ARow] := Value;
end;  // TfraQuickEntryManager.SetHiddenValue 

{-------------------------------------------------------------------------------
  Enables or disables the Add measurement button, and any selected measurements,
      according to whether measurements are currently allowed.
}
procedure TfraQuickEntryManager.SetMeasurementState;
begin
  btnAddMeasurement.Enabled :=
    RowIsChecked(FindRowByQEFieldKey(QE_FIELD_TAXON_DETERMINATION)) or
    RowIsChecked(FindRowByQEFieldKey(QE_FIELD_DETERMINATION));
end;  // TfraQuickEntryManager.SetMeasurementState

{-------------------------------------------------------------------------------
}
procedure TfraQuickEntryManager.SpecimenEventHandlerExit(Sender: TObject);
begin
  with(TSpecimenDataEntryControl(TWinControl(Sender).Owner)) do
  begin
    if ((Checked(5, sgFields.Row) or Checked(6, sgFields.Row)) and (Display[0] = '')) then
    begin
      Value[0] := HiddenValue[DEFAULT_VALUE, sgFields.Row];
      Display[0] := sgFields.Cells[4, sgFields.Row];
    end else begin
      HiddenValue[DEFAULT_VALUE, sgFields.Row] := Value[0];
      sgFields.Cells[4, sgFields.Row] := Display[0];
    end;
  end;
end;  // TfraQuickEntryManager.SpecimenEventHandlerExit

{-------------------------------------------------------------------------------
}
function TfraQuickEntryManager.CheckMeasurementRows(ACheckTaxonDataRows: Boolean): Boolean;
var
  i: Integer;
  lNeedConfirm: Boolean;
  lStrValue: String;
  lQEFieldInfo: TQEFieldInfo;
begin
  Result       := True;
  lNeedConfirm := False;
  lStrValue    := SysUtils.BoolToStr(ACheckTaxonDataRows);

  // Check for existing measurement rows. These will be removed if user confirms change
  // from Taxon Determination to Thesaurus Determination, and vice-versa, but only if
  // measurements set for occurrences.
  for i := 1 to sgFields.RowCount - 1 do
    if (IsCustomRow(i) = 1) and
       (HiddenValue['Measurement_Is_Specimen', i] = STR_FALSE) and
       (HiddenValue['Measurement_Is_TaxonData', i] = lStrValue) then
    begin
      // Found one, that's all we want to know at this point.
      lNeedConfirm := True;
      Break;
    end;

  if lNeedConfirm then begin
    Result := MessageDlg(ResStr_ConfirmQEManagerDeterminationChange,
                         mtConfirmation, [mbYes, mbNo], 0) = mrYes;
    // User say do it, so let's do it.
    if Result then
      // Go in reverse as rows are being removed as we go along.
      for i := sgFields.RowCount - 1 downto 1 do
        if (IsCustomRow(i) = 1) and
           (HiddenValue['Measurement_Is_Specimen', i] = STR_FALSE) and
           (HiddenValue['Measurement_Is_TaxonData', i] = lStrValue) then
        begin
          // Already in DB, need to remove it from there too, but only on Save!
          if HiddenValue['QE_Template_Field_Key', i] <> '' then begin
            lQEFieldInfo := TQEFieldInfo.Create;
            lQEFieldInfo.TemplateFieldKey := HiddenValue['QE_Template_Field_Key', i];
            lQEFieldInfo.Timestamp        := FDataStringGrid.TimestampCol['Timestamp', i];
            FDiscardedMeasurements.Add(lQEFieldInfo);
          end;
          // Now remove the row from grid, and all associated objects
          FDataStringGrid.DeleteRow(i);
        end;
  end;
end;  // TfraQuickEntryManager.CheckMeasurementRows

{-------------------------------------------------------------------------------
}
function TfraQuickEntryManager.GetCellColour(ACol, ARow: integer;
  var AHandled: boolean): TColor;
begin
  Result := MergeColours(clWindow, clBtnFace, 70);
  AHandled := (ACol=0) and (ARow=sgFields.Row) and (EditMode=emEdit);
end;  // TfraQuickEntryManager.GetCellColour

end.
