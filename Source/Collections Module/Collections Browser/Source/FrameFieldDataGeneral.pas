{===============================================================================
  Unit:        FrameFieldDataGeneral.pas

  Defines:     TfraFieldDataGeneral

  Description:

  Model:       CollectionsSpecimens.mpb

  Created:     June 2003
===============================================================================}

unit FrameFieldDataGeneral;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, BaseFullScreenFrameUnit, BaseDetailFrameUnit, ExtCtrls, StdCtrls,
  ImageListButton, Grids, GeneralFunctions, CompositeComponent, SpatialRef,
  VagueDateEdit, ComboListID, LuxIDComboBox, BaseCompositeComponent, LinkedControls,
  InterfaceDataModule, DssStringGrid, LuxembourgDataClasses, ADOInt, DataClasses,
  DataTypes, VagueDate, Recorder2000_TLB, BaseTabSheetFrameUnit,
  ExceptionForm, DropTarget, Menus, Math, DSSDataTypes, ValidationData,
  UserEdit;

type
  EQuickEntryException = class(TExceptionPath)
  end;

type
  TFieldName = (fnCollectionUnitKey, fnUsualLocationCode, fnCurrentLocationCode,
      fnUsualContainerCollectionUnitKey, fnCurrentContainerCollectionUnitKey,
      fnParentCollectionCollectionUnitKey, fnSpecimenTypeConceptKey,
      fnConfidential, fnDangerous, fnLifeSciences, fnDeterminationKey,
      fnConceptKey, fnOccurrenceKey, fnDeterminerNameKey, fnVagueDate,
      fnMovementCollectionUnitKey, fnMovementDirectionKey, fnUsedSpecimen,
      fnTaxonListItemKey, fnComment, fnSampleKey, fnSampleTypeKey, fnSpatialRef,
      fnReceiverOrganisationNameKey, fnNumber, fnInscription, fnNameKey, fnRecordTypeKey,
      fnLocationName, fnLocationKey, fnSurveyKey, fnLabel, fnValue, fnSurveyEventKey);

  TFieldNames = set of TFieldName;

  TDecodedSpatialRef = record
    SpatialRef: Variant;
    SpatialRefSystem: Variant;
    Lat: Variant;
    Long: Variant;
    SpatialRefQualifier: Variant;
  end;
  
  TSurveyEventTable = class(TObject)
  private
    FSpatialRef: TSpatialRef;
    FSurveyEventKey: String;
    FSurveyKey: String;
  public
    function CheckForExisting: TKeyString;
    procedure Clear;
    procedure Execute;
    property SpatialRef: TSpatialRef read FSpatialRef write FSpatialRef;
  end;
  
  TSampleTable = class(TObject)
  private
    FLocationKey: String;
    FLocationName: String;
    FSampleKey: String;
    FSampleTypeKey: String;
    FSpatialRef: TSpatialRef;
    FSurveyKey: String;
    FVagueDate: TVagueDate;
  public
    function CheckForExisting: TKeyString;
    procedure Clear;
    procedure Execute;
    property SpatialRef: TSpatialRef read FSpatialRef write FSpatialRef;
  end;
  
  TFieldCollectorItem = class(TLuxGridDataItem)
  private
    FName: String;
    FNameKey: TKeyString;
    FSampleKey: TKeyString;
    FSERecorderKey: TKeyString;
  protected
    procedure GetData(const Column: Integer; var AText: String; var ANameKey: TKeyString);
        override;
    procedure InitFromRecord(AFields: Fields); override;
    procedure SetData(const Column: Integer; const AText: String; const AKey: TKeyString);
        override;
    procedure ValidateData; override;
  public
    property Name: String read FName;
    property NameKey: TKeyString read FNameKey;
    property SampleKey: TKeyString read FSampleKey;
    property SERecorderKey: TKeyString read FSERecorderKey write FSERecorderKey;
  end;
  
  TFieldCollectorList = class(TLuxGridDataList)
  private
    FSampleKey: TKeyString;
    FSurveyEventKey: TKeyString;
  protected
    procedure DoAddition(AItem: TLuxCachedDataItem); override;
    procedure DoDeletion(AItem: TLuxCachedDataItem); override;
    procedure DoModification(AItem: TLuxCachedDataItem); override;
    function GetRecordset: _Recordset; override;
  public
    property SampleKey: TKeyString read FSampleKey write FSampleKey;
    property SurveyEventKey: TKeyString read FSurveyEventKey write FSurveyEventKey;
  end;
  
  {-----------------------------------------------------------------------------
    General details tab displaying and allowing editing of the main details of a summary of
    the field data associated with a specimen.  Each observation linked to a specimen has a
    single item of field data.  Zero or one of these items is flagged as the Gathering
    Event, which indicates that the specimen was collected during that event.
    When saving a new record, the system follows the steps below:
    1) Checks for an existing sample inside the survey which matches the spatial reference,
    sample type, date and the exact list of sample recorders, and the sample or survey
    event points to the entered location.
    2) If a match is found, then the user is prompted 'This will attach the data to an
    existing sample.  Click Yes to accept, No to create a new sample or Cancel to abort the
    save operation.'  with Yes, No and Cancel option buttons.  If the user clicks Yes,
    proceeds to step 7.  If the user clicks Cancel, aborts the save operation.
    3) Checks for a matching survey event inside the survey (location and date must match
    and the list of sample recorders must be a subset of the list of survey event recorders)
    .
    4) If a match is found, then the user is prompted 'This will attach the data to an
    existing survey event.  Click Yes to accept, No to create a new sample or Cancel to
    abort the save operation.'  with Yes, No and Cancel option buttons.  If the user clicks
    Yes, proceeds to step 6.  If the user clicks Cancel, aborts the save operation.
    5) Creates a new survey event record inside the identified survey.  The list of survey
    event recorders is assumed to be the same as the list of sample recorders.
    6) Creates a new sample record inside the identified survey event.
    7) If Specimen_Unit.Life_Sciences=1 then creates a new Taxon_Occurrence record, else
    creates a new Occurrence record.  Record Type is set to Field Observation and the
    occurrence record key is entered into Specimen_Field_Data.Taxon_Occurrence_Key or
    Occurrence_Key appropriately.
    When saving an existing record, the system updates the sample to reflect the changes
    made only if the occurrence is the only one in the sample.  Likewise, the survey event
    is only updated if there is only one sample and occurrence inside the survey.
    Otherwise a new sample or survey event key is created.
  }

  TfraFieldDataGeneral = class(TBaseTabSheetFrame, IRequestor, ILatLong)
    btnAdd: TImageListButton;
    btnDateInferred: TImageListButton;
    btnFieldCollectorsInferred: TImageListButton;
    btnLocationInferred: TImageListButton;
    btnRemove: TImageListButton;
    btnSampleTypeInferred: TImageListButton;
    btnSpatialRefInferred: TImageListButton;
    btnSurveyInferred: TImageListButton;
    chkGatheringEvent: TCheckBox;
    cmbGatheringMethod: TLuxIDComboBox;
    eDate: TVagueDateEdit;
    eLocation: TLinkedEdit;
    eName: TUserEdit;
    eSpatialRef: TSpatialRef;
    eSurvey: TLinkedEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label8: TLabel;
    lblDate: TLabel;
    pmMapWindow: TPopupMenu;
    sgCollectors: TDSSStringGrid;
    shpList: TShape;
    eVagueLocation: TEdit;
    Label5: TLabel;
    procedure IRequestor.Update = UpdateRequestedData;
    procedure btnAddClick(Sender: TObject);
    procedure btnRemoveClick(Sender: TObject);
    procedure cmbGatheringMethodPopulate(Sender: TObject);
    procedure eLocationChange(Sender: TObject);
    procedure eLocationExit(Sender: TObject);
    procedure eLocationFindData(Sender: TObject);
    procedure eNameExit(Sender: TObject);
    procedure eNameFindData(Sender: TObject);
    procedure eNameGetData(Sender: TObject);
    procedure eNameKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure eSpatialRefGetFromMap(Sender: TObject);
    procedure InferenceClick(Sender: TObject);
    procedure sgCollectorsKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    FCollectionUnitKey: TKeyString;
    FFieldCollectorList: TFieldCollectorList;
    FLatitude: Double;
    FLifeScience: Boolean;
    FLoading: Boolean;
    FLongitude: Double;
    FOccurrenceKey: TKeyString;
    FOriginalGatheringMethod: TKeyString;
    FSampleKey: TKeyString;
    FSpatialRef: String;
    FSpatialRefQualifier: String;
    FSurveyEventKey: TKeyString;
    FTaxonOccurrenceKey: TKeyString;
    FTimestamp: TSQLSvrTimestamp;
    procedure AskAttachToExistingEvent;
    procedure AskAttachToExistingSample;
    function CanAddRow: Boolean;
    procedure CanFindEvent;
    procedure CanFindSample;
    function CheckForSample: TKeyString;
    function CheckForSurveyEvent: TKeyString;
    function CheckNoDuplication(var AName: String): Boolean;
    procedure ClearInferredButtons;
    procedure ClearSpatialRef;
    procedure CreateNewSample;
    procedure CreateNewSurveyEvent;
    procedure DropLocation(const Sender: TObject; const AFormat: Integer; const ASourceData:
        TKeyList; const ATextStrings: TStringList; var AHandled: Boolean);
    procedure DropSurvey(const Sender: TObject; const AFormat: Integer; const ASourceData:
        TKeyList; const ATextStrings: TStringList; var AHandled: Boolean);
    procedure FillSampleTableValues;
    procedure FillSurveyEventTableValues;
    procedure GetLocationSpatialRef;
    function GetParams: TVariantArray;
    function Get_ErrorMsg: WideString; safecall;
    function Get_Latitude: Double; safecall;
    function Get_Longitude: Double; safecall;
    function InferredCaptionToValue(ACaption: String): ShortInt;
    procedure InferredValueToButtonImage(AButton: TImageListButton; AValue: Integer);
    procedure InsertUpdateOccurrenceRecord;
    procedure IsNewFieldData1;
    procedure IsNewFieldData2;
    procedure IsNewFieldData3;
    procedure IsOneOccurrenceInEvent;
    procedure IsOneOccurrenceInSample;
    procedure MapMenuItemClick(Sender: TObject);
    procedure UpdateName(const AKeyList: IKeyList);
    procedure UpdateRequestedData(const KeyList: IKeyList); safecall;
    procedure UpdateSample;
    procedure UpdateSurveyEvent;
    procedure ValidateStringGrid;
  protected
    procedure DeleteData; override;
    procedure EnableControls(AEnabled: Boolean); override;
    function GetCaption: String; override;
    procedure LoadData; override;
    procedure RegisterControls; override;
    procedure RegisterDragDropComponents; override;
    procedure SaveData; override;
    procedure SetKey(Value: TKeyString); override;
    procedure ValidateData; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure UpdateMapWindowSelector; override;
  end;
  
var
  FSampleTable: TSampleTable;
  FSurveyEventTable: TSurveyEventTable;

//==============================================================================
implementation

uses
  ApplicationSettings, BaseADODataModule, ResourceStrings, LuxembourgConstants,
  Validation, SearchManager, GeneralData;

{$R *.dfm}

{-==============================================================================
    TFieldCollectorItem
===============================================================================}
{-------------------------------------------------------------------------------
}
procedure TFieldCollectorItem.GetData(const Column: Integer; var AText: String; var ANameKey:
    TKeyString);
begin
  AText := Name;
  ANameKey := NameKey;
end;  // TFieldCollectorItem.GetData 

{-------------------------------------------------------------------------------
}
procedure TFieldCollectorItem.InitFromRecord(AFields: Fields);
begin
  SetItemKey(VarToStr(AFields['Sample_Key'].Value));
  FNameKey := AFields['Name_Key'].Value;
  FName := AFields['Name'].Value;
  FSampleKey := AFields['Sample_Key'].Value;
  FSERecorderKey := AFields['SE_Recorder_Key'].Value;
end;  // TFieldCollectorItem.InitFromRecord 

{-------------------------------------------------------------------------------
}
procedure TFieldCollectorItem.SetData(const Column: Integer; const AText: String; const AKey:
    TKeyString);
begin
  FName := AText;
  FNameKey := AKey;
  SetModified;
end;  // TFieldCollectorItem.SetData 

{-------------------------------------------------------------------------------
}
procedure TFieldCollectorItem.ValidateData;
var
  lText, lKey: String;
begin
  if (FNameKey = '') and (FName <> '') then begin
    lText := FName;
    if DoCheckUnique(lKey, lText, stIndividual, '', ResStr_Individual) then begin
      // Found a unique exact match. Update fields accordingly.
      FNameKey := lKey;
      FName := lText;
    end;
  end;
  ValidateValue(FNameKey <> '', Format(ResStr_InvalidNameInGrid, [FName, ResStr_Individual]));
end;  // TFieldCollectorItem.ValidateData 

{-==============================================================================
    TFieldCollectorList
===============================================================================}
{-------------------------------------------------------------------------------
}
procedure TFieldCollectorList.DoAddition(AItem: TLuxCachedDataItem);
begin
  with TFieldCollectorItem(AItem) do begin
    SERecorderKey := VarToStr(dmGeneral.RunInsertStoredProc(TN_SURVEY_EVENT_RECORDER,
                  'usp_SurveyEventRecorder_Insert',
                 ['@Key', SERecorderKey,
                  '@NameKey', NameKey,
                  '@SurveyEventKey', Self.FSurveyEventKey,
                  // Hardcoded surveyor for time being.
                  '@RecorderRoleKey', SURVEYOR_RECORDER_ROLE,
                  '@EnteredBy', dmGeneral.Recorder.CurrentSettings.UserIDKey
                 ], '@Key'));
    dmGeneral.RunUpdateStoredProc('usp_SampleRecorder_Insert',
                   ['@SampleKey', Self.FSampleKey,
                    '@SERecorderKey', SERecorderKey,
                    '@EnteredBy', dmGeneral.Recorder.CurrentSettings.UserIDKey
                   ]);
  end;
end;  // TFieldCollectorList.DoAddition 

{-------------------------------------------------------------------------------
}
procedure TFieldCollectorList.DoDeletion(AItem: TLuxCachedDataItem);
begin
  with TFieldCollectorItem(AItem) do
    dmGeneral.RunDeleteStoredProc('usp_SampleRecorder_Delete',
                                    ['@SampleKey', SampleKey,
                                    '@SurveyEventRecorderKey', SERecorderKey]);
end;  // TFieldCollectorList.DoDeletion 

{-------------------------------------------------------------------------------
  We always want to run the Insert method, but for modifications, we had better run the Delete
      method as well beforehand.
}
procedure TFieldCollectorList.DoModification(AItem: TLuxCachedDataItem);
begin
  DoDeletion(AItem);
  DoAddition(AItem);
end;  // TFieldCollectorList.DoModification 

{-------------------------------------------------------------------------------
}
function TFieldCollectorList.GetRecordset: _Recordset;
begin
  Result := dmGeneral.GetRecordset('usp_SampleRecorders_Select',
                                     ['@Key', MasterKey]);
end;  // TFieldCollectorList.GetRecordset 

{-==============================================================================
    TfraFieldDataGeneral
===============================================================================}
{-------------------------------------------------------------------------------
  Constructor. Creates the list for the string grid and the insert wrapper. 
}
constructor TfraFieldDataGeneral.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  
  FFieldCollectorList := TFieldCollectorList.Create(TFieldCollectorItem, sgCollectors);
  sgCollectors.Rows[0].CommaText := ResStr_Name;
  // Setting the control sets the type to custom.
  sgCollectors.ColumnsInfo[0].WinControl := eName;
  FSampleTable := TSampleTable.Create;
  FSampleTable.SpatialRef := eSpatialRef;
  FSurveyEventTable := TSurveyEventTable.Create;
  FSurveyEventTable.SpatialRef := eSpatialRef;

  UpdateMapWindowSelector;
  FLoading := False;
end;  // TfraFieldDataGeneral.Create 

{-------------------------------------------------------------------------------
  Destructor frees the list of the string grid. 
}
destructor TfraFieldDataGeneral.Destroy;
begin
  FFieldCollectorList.Free;
  FSampleTable.Free;
  FSurveyEventTable.Free;
  
  inherited Destroy;
end;  // TfraFieldDataGeneral.Destroy 

{-------------------------------------------------------------------------------
  Ask the user if they want to attach the data to the existing Survey Event. 
}
procedure TfraFieldDataGeneral.AskAttachToExistingEvent;
begin
  case ConfirmYesNoCancel(ResStr_AttachDataToExistingSurveyEventConfirm) of
    mrYes:    IsNewFieldData2;
    mrNo:     begin
                FSurveyEventTable.FSurveyEventKey := '';
                CreateNewSurveyEvent;
              end;
    mrCancel: Abort;
  end;
end;  // TfraFieldDataGeneral.AskAttachToExistingEvent 

{-------------------------------------------------------------------------------
  Ask the user if they want to attach the data to the existing sample. 
}
procedure TfraFieldDataGeneral.AskAttachToExistingSample;
begin
  case ConfirmYesNoCancel(ResStr_AttachDataToExistingSampleConfirm) of
    mrYes:    IsNewFieldData1;
    mrNo:     begin
                // Clear sample key so that a new sample will be created.
                FSampleTable.FSampleKey := '';
                CanFindEvent;
              end;
    mrCancel: Abort;
  end;
end;  // TfraFieldDataGeneral.AskAttachToExistingSample 

{-------------------------------------------------------------------------------
  Handle a click on the add button. 
}
procedure TfraFieldDataGeneral.btnAddClick(Sender: TObject);
begin
  inherited;
  if CanAddRow then begin
    FFieldCollectorList.AddNew(TFieldCollectorItem.CreateNew(FFieldCollectorList));
    sgCollectors.Row := FFieldCollectorList.ItemCount;
  end else
    sgCollectors.Row := sgCollectors.RowCount - 1;
  eName.SetFocus;
end;  // TfraFieldDataGeneral.btnAddClick 

{-------------------------------------------------------------------------------
  Handle a click on the remove button. 
}
procedure TfraFieldDataGeneral.btnRemoveClick(Sender: TObject);
begin
  inherited;
  with sgCollectors do
    if (Cells[Col, Row] <> '') or (eName.Text <> '') then begin
      if MessageDlg(Format(ResStr_ConfirmRowDelete, [ResStr_Row]),
                     mtWarning, [mbYes, mbNo], 0) = mrYes then
        FFieldCollectorList.DeleteItem(Row);
    end else
      FFieldCollectorList.DeleteItem(Row);
  EnableControls(True);
end;  // TfraFieldDataGeneral.btnRemoveClick 

{-------------------------------------------------------------------------------
}
function TfraFieldDataGeneral.CanAddRow: Boolean;
begin
  with sgCollectors do
    Result := ((Row = RowCount - 1) and (eName.Text <> '')) or
              (Cells[0, RowCount - 1] <> '');
end;  // TfraFieldDataGeneral.CanAddRow 

{-------------------------------------------------------------------------------
  See if a matching event can be found.
}
procedure TfraFieldDataGeneral.CanFindEvent;
var
  lSurveyEventKey: TKeyString;
begin
  lSurveyEventKey := CheckForSurveyEvent;
  FSurveyEventTable.FSurveyEventKey := lSurveyEventKey;

  if lSurveyEventKey <> '' then begin
    if lSurveyEventKey = FSurveyEventKey  then IsNewFieldData2
                                          else AskAttachToExistingEvent;
  end else
    IsNewFieldData3;
end;  // TfraFieldDataGeneral.CanFindEvent

{-------------------------------------------------------------------------------
  See if a matching sample can be found. 
}
procedure TfraFieldDataGeneral.CanFindSample;
var
  lSampleKey: TKeyString;
begin
  lSampleKey := CheckForSample;
  FSampleTable.FSampleKey := lSampleKey;

  if lSampleKey <> '' then begin
    // If the Sample is found, we need to retrieve the Survey_Event_Key
    // so that it can be used for the Occurrence and the Field Collectors.
    FSurveyEventTable.FSurveyEventKey := dmGeneral.GetStoredProcOutputParam(
                                            'usp_SurveyEventKey_ForSample_Get',
                                            ['@Key', lSampleKey], '@SurveyEventKey');
    if lSampleKey = FSampleKey then IsNewFieldData1
                               else AskAttachToExistingSample;
  end else
    CanFindEvent;
end;  // TfraFieldDataGeneral.CanFindSample

{-------------------------------------------------------------------------------
  Check for an existing sample.
}
function TfraFieldDataGeneral.CheckForSample: TKeyString;
var
  lFieldCollectorKey: TKeyString;
  lIdx: Integer;
begin
  // Prepare a temp table used to select the list of specimens to show.
  dmGeneral.ExecuteSQL(SQL_CREATE_TEMP_FIELD_COLLECTORS);

  try
    // Populate the temp table.
    for lIdx := 0 to FFieldCollectorList.ItemCount - 1 do begin
      lFieldCollectorKey := TFieldCollectorItem(FFieldCollectorList.Items[lIdx]).NameKey;
      if lFieldCollectorKey <> '' then
        dmGeneral.ExecuteSQL(Format(SQL_INSERT_TEMP_FIELD_COLLECTORS, [lFieldCollectorKey]));
    end;

    FillSampleTableValues;

    Result := FSampleTable.CheckForExisting;
  finally
    dmGeneral.ExecuteSQL(SQL_DROP_TEMP_FIELD_COLLECTORS);
  end;
end;  // TfraFieldDataGeneral.CheckForSample

{-------------------------------------------------------------------------------
  Check for an existing survey event.
}
function TfraFieldDataGeneral.CheckForSurveyEvent: TKeyString;
var
  lFieldCollectorKey: TKeyString;
  lIdx: Integer;
begin
  // Prepare a temp table used to select the list of specimens to show.
  dmGeneral.ExecuteSQL(SQL_CREATE_TEMP_FIELD_COLLECTORS);

  try
    // Populate the temp table.
    for lIdx := 0 to FFieldCollectorList.ItemCount - 1 do begin
      lFieldCollectorKey := TFieldCollectorItem(FFieldCollectorList.Items[lIdx]).NameKey;
      if lFieldCollectorKey <> '' then
        dmGeneral.ExecuteSQL(Format(SQL_INSERT_TEMP_FIELD_COLLECTORS, [lFieldCollectorKey]));
    end;

    FillSurveyEventTableValues;
    Result := FSurveyEventTable.CheckForExisting;
  finally
    dmGeneral.ExecuteSQL(SQL_DROP_TEMP_FIELD_COLLECTORS);
  end;
end;  // TfraFieldDataGeneral.CheckForSurveyEvent

{-------------------------------------------------------------------------------
  Method that checks there is no duplication of names in the string grid. 
}
function TfraFieldDataGeneral.CheckNoDuplication(var AName: String): Boolean;
var
  x, y: Integer;
begin
  Result := True;
  for x := 0 to FFieldCollectorList.ItemCount - 1 do
    for y := 0 to FFieldCollectorList.ItemCount - 1 do
      if (TFieldCollectorItem(FFieldCollectorList.Items[x]).NameKey =
                TFieldCollectorItem(FFieldCollectorList.Items[y]).NameKey) and (x <> y) then
                    begin
        Result := False;
        AName := TFieldCollectorItem(FFieldCollectorList.Items[x]).Name;
        Break;
      end;
end;  // TfraFieldDataGeneral.CheckNoDuplication 

{-------------------------------------------------------------------------------
  Reset the inferred data buttons so that they show all ticks. 
}
procedure TfraFieldDataGeneral.ClearInferredButtons;
begin
  InferredValueToButtonImage(btnSurveyInferred, 0);
  InferredValueToButtonImage(btnLocationInferred, 0);
  InferredValueToButtonImage(btnSpatialRefInferred, 0);
  InferredValueToButtonImage(btnSampleTypeInferred, 0);
  InferredValueToButtonImage(btnDateInferred, 0);
  InferredValueToButtonImage(btnFieldCollectorsInferred, 0);
end;  // TfraFieldDataGeneral.ClearInferredButtons 

{-------------------------------------------------------------------------------
  Empty the Spatial Ref. component of data. 
}
procedure TfraFieldDataGeneral.ClearSpatialRef;
begin
  eSpatialRef.SpatialRef := '';
  eSpatialRef.Qualifier  := '';
end;  // TfraFieldDataGeneral.ClearSpatialRef 

{-------------------------------------------------------------------------------
  Populate the Gathering Method combo box. 
}
procedure TfraFieldDataGeneral.cmbGatheringMethodPopulate(Sender: TObject);
begin
  with dmGeneral.GetRecordset('usp_GatheringMethod_Select', []) do begin
  
    while not Eof do begin
      cmbGatheringMethod.Add(VarToStr(Fields['Short_Name'].Value),
                             VarToStr(Fields['Sample_Type_Key'].Value));
      MoveNext;
    end; // while
  end; // with
end;  // TfraFieldDataGeneral.cmbGatheringMethodPopulate 

{-------------------------------------------------------------------------------
  Handles the creation of a sample. 
}
procedure TfraFieldDataGeneral.CreateNewSample;
begin
  FSampleTable.Execute;
  IsNewFieldData1;
end;  // TfraFieldDataGeneral.CreateNewSample 

{-------------------------------------------------------------------------------
  Handles the creating of a Survey Event. 
}
procedure TfraFieldDataGeneral.CreateNewSurveyEvent;
begin
  FSurveyEventTable.Execute;
  IsNewFieldData2;
end;  // TfraFieldDataGeneral.CreateNewSurveyEvent 

{-------------------------------------------------------------------------------
  Deletes the record associated with this frame. 
}
procedure TfraFieldDataGeneral.DeleteData;
begin
  dmGeneral.RunDeleteStoredProc('usp_SpecimenFieldData_Delete',
                               ['@Key', Key, '@Timestamp', FTimestamp]);
end;  // TfraFieldDataGeneral.DeleteData 

{-------------------------------------------------------------------------------
}
procedure TfraFieldDataGeneral.DropLocation(const Sender: TObject; const AFormat: Integer; const
    ASourceData: TKeyList; const ATextStrings: TStringList; var AHandled: Boolean);
begin
  DropLinkedEditData(AFormat, ASourceData, AHandled, eLocation, TN_LOCATION,
                     'usp_LocationName_Get', '@Key', '@Caption');
  //Update Spatial Ref field and Qualifier
  eLocationChange(self);
end;  // TfraFieldDataGeneral.DropLocation 

{-------------------------------------------------------------------------------
}
procedure TfraFieldDataGeneral.DropSurvey(const Sender: TObject; const AFormat: Integer; const
    ASourceData: TKeyList; const ATextStrings: TStringList; var AHandled: Boolean);
begin
  DropLinkedEditData(AFormat, ASourceData, AHandled, eSurvey, TN_SURVEY,
                     'usp_SurveyName_Get', '@Key', '@Caption');
end;  // TfraFieldDataGeneral.DropSurvey 

{-------------------------------------------------------------------------------
}
procedure TfraFieldDataGeneral.eLocationChange(Sender: TObject);
begin
  inherited;
  // If location key specified, and spatial ref either blank or has not been
  // changed then update the spatial ref to that of the new location.
  if (eLocation.Key <> '') and
     (((CompareText(eSpatialRef.SpatialRef, FSpatialRef) = 0) and
     (eSpatialRef.Qualifier = FSpatialRefQualifier)) or
     (eSpatialRef.SpatialRef = '')) then begin
    if not FLoading then begin
      GetLocationSpatialRef;
      eSpatialRef.SpatialRef := FSpatialRef;
      eSpatialRef.Qualifier  := FSpatialRefQualifier;
    end;
  end;
end;  // TfraFieldDataGeneral.eLocationChange

{-------------------------------------------------------------------------------
}
procedure TfraFieldDataGeneral.eLocationExit(Sender: TObject);
begin
  inherited;
  eLocationFindData(Self);
end;  // TfraFieldDataGeneral.eLocationExit

{-------------------------------------------------------------------------------
}
procedure TfraFieldDataGeneral.eLocationFindData(Sender: TObject);
begin
  inherited;
  CheckLinkedLocation(eLocation, eLocation.Text);
  eLocationChange(self);
end;  // TfraFieldDataGeneral.eLocationFindData 

{-------------------------------------------------------------------------------
  Set the colours of the controls in edit mode. 
}
procedure TfraFieldDataGeneral.EnableControls(AEnabled: Boolean);
begin
  inherited;
  SetRequiredFieldsColourState(AEnabled, [cmbGatheringMethod, eSurvey, sgCollectors]);

  sgCollectors.ReadOnly := not AEnabled;
  eSpatialRef.ReadOnly := not AEnabled;

  btnRemove.Enabled := (EditMode = emEdit) and (sgCollectors.Cells[0, 1] <> '');

  if AEnabled then
    eSpatialRef.Color := MergeColours(AppSettings.MandatoryColour, clWindow, 25)
  else
    eSpatialRef.Color := clWindow;
  eLocation.Color := eSpatialRef.Color;
  eVagueLocation.Color := eSpatialRef.Color;
end;  // TfraFieldDataGeneral.EnableControls

{-------------------------------------------------------------------------------
  Make sure the contents of the Field Collector linked edit is parsed when the user exits it.
}
procedure TfraFieldDataGeneral.eNameExit(Sender: TObject);
begin
  inherited;
  btnRemove.Enabled := (EditMode = emEdit) and
                       ((sgCollectors.Cells[0, 1] <> '') or (eName.Text <> ''));
end;  // TfraFieldDataGeneral.eNameExit

{-------------------------------------------------------------------------------
  Handler for OnFindData for the floating linked edit control on the string grid.
}
procedure TfraFieldDataGeneral.eNameFindData(Sender: TObject);
begin
  inherited;
  DoCheck(eName, stIndividual);
end;  // TfraFieldDataGeneral.eNameFindData

{-------------------------------------------------------------------------------
  Get data from the Names and Addresses module.
}
procedure TfraFieldDataGeneral.eNameGetData(Sender: TObject);
begin
  inherited;
  InitReturnData(UpdateName, TN_NAME);
end;  // TfraFieldDataGeneral.eNameGetData

{-------------------------------------------------------------------------------
}
procedure TfraFieldDataGeneral.eNameKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  inherited;
  if (Key in [VK_DELETE, VK_BACK]) and (eName.Text = '') then begin
    // Need to get rid of some messages in queue first.
    Application.ProcessMessages;
    // Now remove.
    FFieldCollectorList.DeleteItem(sgCollectors.Row);
    Key := 0;
  end
  else
    // This is unfortunately necessary since the eName control overrides
    // the OnKeyPress of the TUserEdit.
    eName.GetCurrentUserF11(Sender, Key, Shift);
end;  // TfraFieldDataGeneral.eNameKeyDown

{-------------------------------------------------------------------------------
}
procedure TfraFieldDataGeneral.eSpatialRefGetFromMap(Sender: TObject);
begin
  inherited;
  dmGeneral.Recorder.RequestData(Self, 'MapPoint');
  eLocationChange(self);
end;  // TfraFieldDataGeneral.eSpatialRefGetFromMap

{-------------------------------------------------------------------------------
  Fill the Sample table object with values.
}
procedure TfraFieldDataGeneral.FillSampleTableValues;
var
  lLatLong: ILatLong;
begin
  lLatLong := dmGeneral.Recorder.RecorderFunctions.DecodeSpatialRef(eSpatialRef.EnteredRef);
  with FSampleTable do begin
    FSampleTypeKey := cmbGatheringMethod.CurrentStrID;
    FVagueDate     := eDate.VagueDate;
    FLocationKey   := eLocation.Key;
    FLocationName  := eVagueLocation.Text;
    FSurveyKey     := eSurvey.Key;
  end;
  // Have this line outside of the 'with' because TSampleTable and
  // TfraFieldDataGeneral have variables called FSampleKey and it passes in the
  // wrong one.
  FSampleTable.FSampleKey := FSampleKey;
end;  // TfraFieldDataGeneral.FillSampleTableValues

{-------------------------------------------------------------------------------
  Fill the Survey Event table object with values.
}
procedure TfraFieldDataGeneral.FillSurveyEventTableValues;
begin
  FSurveyEventTable.FSurveyKey := eSurvey.Key;
  FSurveyEventTable.FSurveyEventKey := FSurveyEventKey;
end;  // TfraFieldDataGeneral.FillSurveyEventTableValues

{-------------------------------------------------------------------------------
  Get the caption. 
}
function TfraFieldDataGeneral.GetCaption: String;
begin
  Result := VagueDateToString(eDate.VagueDate);
  if Result = '' then Result := 'Unknown';
  if eVagueLocation.Text <> '' then
    Result := Result + ' - ' + eVagueLocation.Text
  else
  if eLocation.Text <> '' then
    Result := Result + ' - ' + eLocation.Text;
end;  // TfraFieldDataGeneral.GetCaption 

{-------------------------------------------------------------------------------
}
procedure TfraFieldDataGeneral.GetLocationSpatialRef;
var
  lAppSpatialRefSystem, lLocationSpatialRefSystem: String;
begin
  lAppSpatialRefSystem := dmGeneral.Recorder.RecorderFunctions.SpatialRefSystem;
  with dmGeneral.GetRecordset('usp_SpatialRef_Select_ForLocation',
                              ['@Key', eLocation.Key]) do
    if not EOF then begin
      FLatitude                 := Fields['Lat'].Value;
      FLongitude                := Fields['Long'].Value;
      FSpatialRefQualifier      := VarToStr(Fields['Qualifier'].Value);
      lLocationSpatialRefSystem := VarToStr(Fields['System'].Value);
  
      // See if the SpatialRef needs to be converted or just loaded from DB.
      if lAppSpatialRefSystem = lLocationSpatialRefSystem then begin
        eSpatialRef.EnteredSystem := lLocationSpatialRefSystem;
        FSpatialRef := VarToStr(Fields['SpatialRef'].Value);
      end else
        FSpatialRef := dmGeneral.Recorder.RecorderFunctions.EncodeSpatialRef(Self as ILatLong);
    end;
end;  // TfraFieldDataGeneral.GetLocationSpatialRef 

{-------------------------------------------------------------------------------
  Get the parameters required for the save data. 
}
function TfraFieldDataGeneral.GetParams: TVariantArray;
var
  lLatLong: ILatLong;
begin
  lLatLong := dmGeneral.Recorder.RecorderFunctions.DecodeSpatialRef(eSpatialRef.EnteredRef);

  Result := VarArrayOf(
      ['@Key', Key,
       '@CollectionUnitKey', ParentKey,
       '@OccurrenceKey', FOccurrenceKey,
       '@TaxonOccurrenceKey', FTaxonOccurrenceKey,
       '@GatheringEvent', chkGatheringEvent.Checked,
       '@InferredSurvey', InferredCaptionToValue(btnSurveyInferred.Caption),
       '@InferredLocation', InferredCaptionToValue(btnLocationInferred.Caption),
       '@InferredSpatialRef', InferredCaptionToValue(btnSpatialRefInferred.Caption),
       '@InferredSampleType', InferredCaptionToValue(btnSampleTypeInferred.Caption),
       '@InferredDate', InferredCaptionToValue(btnDateInferred.Caption),
       '@InferredCollectors', InferredCaptionToValue(btnFieldCollectorsInferred.Caption),
       '@SurveyEventKey', FSurveyEventKey,
       '@SampleKey', FSampleKey,
       '@SurveyKey', eSurvey.Key,
       '@LocationKey', eLocation.Key,
       '@LocationName', eVagueLocation.Text,
       '@SpatialRefQualifier', Iif(eSpatialRef.EnteredRef='', null, eSpatialRef.Qualifier),
       '@SpatialRef', Iif(eSpatialRef.EnteredRef='', null, eSpatialRef.EnteredRef),
       '@Lat', Iif(eSpatialRef.EnteredRef='', null, lLatLong.Latitude),
       '@Long', Iif(eSpatialRef.EnteredRef='', null, lLatLong.Longitude),
       '@GatheringMethod', cmbGatheringMethod.CurrentStrID,
       '@VagueDateStart', eDate.VagueDate.StartDate,
       '@VagueDateEnd', eDate.VagueDate.EndDate,
       '@VagueDateType', eDate.VagueDate.DateTypeString,
       '@Timestamp', FTimestamp
      ]);
end;  // TfraFieldDataGeneral.GetParams 

{-------------------------------------------------------------------------------
}
function TfraFieldDataGeneral.Get_ErrorMsg: WideString;
begin
  Result := 'Error here';
end;  // TfraFieldDataGeneral.Get_ErrorMsg 

{-------------------------------------------------------------------------------
}
function TfraFieldDataGeneral.Get_Latitude: Double;
begin
  Result := FLatitude;
end;  // TfraFieldDataGeneral.Get_Latitude 

{-------------------------------------------------------------------------------
}
function TfraFieldDataGeneral.Get_Longitude: Double;
begin
  Result := FLongitude;
end;  // TfraFieldDataGeneral.Get_Longitude 

{-------------------------------------------------------------------------------
  Handle a click on one of the inferred buttons. 
}
procedure TfraFieldDataGeneral.InferenceClick(Sender: TObject);
begin
  with TImageListButton(Sender) do begin
    if Caption='' then
      Caption:='!'
    else if Caption='!' then
      Caption:='?'
    else if Caption='?' then
      Caption:='!?'
    else if Caption='!?' then
      Caption:='';
    if Caption='' then
      ImageIndex := 6    // enable the tick
    else
      ImageIndex := -1;  // disable the tick to show the caption
  end;
end;  // TfraFieldDataGeneral.InferenceClick 

{-------------------------------------------------------------------------------
  Convert the caption of a button to a number that can be saved in the database. 
}
function TfraFieldDataGeneral.InferredCaptionToValue(ACaption: String): ShortInt;
begin
  if      ACaption = ''   then Result := 0
  else if ACaption = '!'  then Result := 1
  else if ACaption = '?'  then Result := 2
  else if ACaption = '!?' then Result := 3
  else
    raise EBrowserFrameError.Create(ResStr_InferredButtonCaptionIncorrect);
end;  // TfraFieldDataGeneral.InferredCaptionToValue 

{-------------------------------------------------------------------------------
  Give the inferred buttons their images and captions according to the values from the
      database.
}
procedure TfraFieldDataGeneral.InferredValueToButtonImage(AButton: TImageListButton; AValue: Integer);
begin
  with AButton do begin
    case AValue of
      0 : begin
            Caption := '';
            ImageIndex := 6;
          end;
      1 : begin
            Caption := '!';
            ImageIndex := -1;
          end;
      2 : begin
            Caption := '?';
            ImageIndex := -1;
          end;
      3 : begin
            Caption := '!?';
            ImageIndex  := -1;
          end;
    else
      raise EBrowserFrameError.Create(ResStr_InferredButtonCaptionIncorrect);
    end;
  end;
end;  // TfraFieldDataGeneral.InferredValueToButtonImage 

{-------------------------------------------------------------------------------
  Either insert or update the Occurrence record. 
}
procedure TfraFieldDataGeneral.InsertUpdateOccurrenceRecord;
begin
  FSampleKey      := FSampleTable.FSampleKey;
  FSurveyEventKey := FSurveyEventTable.FSurveyEventKey;
  
  // Doing an update.
  if Key <> '' then begin
    if FLifeScience then
      dmGeneral.RunUpdateStoredProc('usp_TaxonOccurrence_Update_ForSample',
                                    ['@Key', FTaxonOccurrenceKey, '@SampleKey', FSampleKey])
    else
      dmGeneral.RunUpdateStoredProc('usp_Occurrence_Update_ForSample',
                                    ['@Key', FOccurrenceKey, '@SampleKey', FSampleKey]);
  end else begin
    // If there is an existing sample inside the survey event that matches spatial
    // ref, sample type, date and field collectors, and the user chooses to attach
    // data to an existing sample, the Insert Wrapper won't yet have the correct
    // Survey Event key, so get it here.
    if (FSurveyEventKey = '') and (FSampleKey <> '') then
      FSurveyEventKey := dmGeneral.GetStoredProcOutputParam
                                                ('usp_SurveyEventKey_ForSample_Get',
                                                ['@Key', FSampleKey],
                                                '@SurveyEventKey');
  
    with dmGeneral do begin
      if FLifeScience then begin
        FTaxonOccurrenceKey := RunInsertStoredProc(TN_TAXON_OCCURRENCE,
                                'usp_TaxonOccurrence_Insert',
                                ['@Key', FTaxonOccurrenceKey,
                                 '@SampleKey', FSampleKey,
                                 '@EnteredBy', Recorder.CurrentSettings.UserIDKey,
                                 '@Checked', 1,
                                 '@CheckedBy', Recorder.CurrentSettings.UserIDKey,
                                 '@RecordTypeKey', FIELD_COLLECTOR_RECORD_TYPE],
                                '@Key');
        RunStoredProc('usp_TaxonOccurrence_TaxonDetermination_Update',
                      ['@TaxonOccurrenceKey', FTaxonOccurrenceKey,
                       '@SpecimenUnitKey', FCollectionUnitKey]);
      end else begin
        FOccurrenceKey := RunInsertStoredProc(TN_OCCURRENCE,
                                'usp_Occurrence_Insert',
                                ['@Key', FOccurrenceKey,
                                 '@SampleKey', FSampleKey,
                                 '@Checked', 1,
                                 '@CheckedBy', Recorder.CurrentSettings.UserIDKey,
                                 '@SpecimenKey', FCollectionUnitKey,
                                 '@RecordTypeKey', FIELD_COLLECTOR_RECORD_TYPE_CONCEPT],
                                '@Key');
        RunStoredProc('usp_Occurrence_Determination_Update',
                      ['@OccurrenceKey', FOccurrenceKey,
                       '@SpecimenUnitKey', FCollectionUnitKey]);
      end;
    end;
  end;
end;  // TfraFieldDataGeneral.InsertUpdateOccurrenceRecord 

{-------------------------------------------------------------------------------
  Comes here after updating or creating a new sample or if the user decides to attach the data
      to the existing sample.
}
procedure TfraFieldDataGeneral.IsNewFieldData1;
begin
  InsertUpdateOccurrenceRecord;
end;  // TfraFieldDataGeneral.IsNewFieldData1 

{-------------------------------------------------------------------------------
  Comes here after having created or updated an event, or the user decides to attach the data
      to the existing event.
}
procedure TfraFieldDataGeneral.IsNewFieldData2;
begin
  if Key = '' then CreateNewSample
              else IsOneOccurrenceInSample;
end;  // TfraFieldDataGeneral.IsNewFieldData2

{-------------------------------------------------------------------------------
  Comes here if a matching event cannot be found.
}
procedure TfraFieldDataGeneral.IsNewFieldData3;
begin
  if Key = '' then CreateNewSurveyEvent
              else IsOneOccurrenceInEvent;
end;  // TfraFieldDataGeneral.IsNewFieldData3 

{-------------------------------------------------------------------------------
  See if there is only one occurrence in the event. 
}
procedure TfraFieldDataGeneral.IsOneOccurrenceInEvent;
begin
  if (dmGeneral.GetStoredProcOutputParam('usp_OccurrencesInEvent_Count_Get',
                                         ['@Key', FSurveyEventKey], '@Count')) = 1 then
    UpdateSurveyEvent
  else
    CreateNewSurveyEvent;
end;  // TfraFieldDataGeneral.IsOneOccurrenceInEvent 

{-------------------------------------------------------------------------------
  See if there is only one occurrence in the sample. 
}
procedure TfraFieldDataGeneral.IsOneOccurrenceInSample;
begin
  if (dmGeneral.GetStoredProcOutputParam('usp_OccurrencesInSample_Count_Get',
                                         ['@Key', FSampleKey], '@Count')) = 1 then
    UpdateSample
  else
    CreateNewSample;
end;  // TfraFieldDataGeneral.IsOneOccurrenceInSample

{-------------------------------------------------------------------------------
  Load the data from the recordset. 
}
procedure TfraFieldDataGeneral.LoadData;
begin
  FLoading := True;
  try
    inherited;
    if not RegisteredRecordsets[0].Eof then begin
      with RegisteredRecordsets[0] do begin
        // FSpatialRef and FSpatialRefQualifier to remember what field values
        // were when they were loaded, so we can see if they have changed.
        FSpatialRef               := VarToStr(Fields['Spatial_Ref'].Value);
        FSpatialRefQualifier      := VarToStr(Fields['Spatial_Ref_Qualifier'].Value);
        eSpatialRef.EnteredSystem := VarToStr(Fields['Spatial_Ref_System'].Value);
        FTimestamp                := Fields['Timestamp'].Value;
        FSampleKey                := VarToStr(Fields['Sample_Key'].Value);
        FSurveyEventKey           := VarToStr(Fields['Survey_Event_Key'].Value);
        FTaxonOccurrenceKey       := VarToStr(Fields['Taxon_Occurrence_Key'].Value);
        FOccurrenceKey            := VarToStr(Fields['Occurrence_Key'].Value);
        FOriginalGatheringMethod  := VarToStr(Fields['Sample_Type_Key'].Value);
        InferredValueToButtonImage(btnSurveyInferred, Fields['Inferred_Survey'].Value);
        InferredValueToButtonImage(btnLocationInferred, Fields['Inferred_Location'].Value);
        InferredValueToButtonImage(btnSpatialRefInferred,
                                   Fields['Inferred_Spatial_Ref'].Value);
        InferredValueToButtonImage(btnSampleTypeInferred,
                                   Fields['Inferred_Sample_Type'].Value);
        InferredValueToButtonImage(btnDateInferred, Fields['Inferred_Date'].Value);
        InferredValueToButtonImage(btnFieldCollectorsInferred,
                                   Fields['Inferred_Collectors'].Value);
      end;
      eSpatialRef.EnteredRef := FSpatialRef;
      eSpatialRef.Qualifier  := FSpatialRefQualifier;
    end else begin
      FSurveyEventKey := '';
      ClearInferredButtons;
      ClearSpatialRef;
      eDate.Text := DateToStr(Date);
    end;
    FCollectionUnitKey := AdditionalProperties.GetProperty(PROP_PARENT_KEY);
    FFieldCollectorList.MasterKey := Key;
    FFieldCollectorList.Refresh;
    FLifeScience := AdditionalProperties.GetProperty(PROP_SPECIMEN_IS_LIFESCIENCES);

  finally
    FLoading := False;
  end;
end;  // TfraFieldDataGeneral.LoadData

{-------------------------------------------------------------------------------
}
procedure TfraFieldDataGeneral.MapMenuItemClick(Sender: TObject);
begin
  // The index of the map to use is stored in the Tag of the MenuItem.
  if (Sender is TMenuItem) then
    dmGeneral.Recorder.RequestData(Self, 'MapPoint' + IntToStr(TMenuItem(Sender).Tag));
end;  // TfraFieldDataGeneral.MapMenuItemClick

{-------------------------------------------------------------------------------
  Register the controls.
}
procedure TfraFieldDataGeneral.RegisterControls;
begin
  inherited;
  // Register recordsets used
  RegisterRecordset('usp_FieldData_Select');

  // Register controls getting their value straight from the registered recordsets.
  RegisterControl(eSurvey, 'Survey_Name', 'Survey_Key', True, ResStr_Survey,
                  CheckLinkedSurvey, 'Survey', ConvertSurveyKeyToCaption);
  RegisterControl(eLocation, 'Location', 'Location_Key', CheckLinkedLocation,
                  'Location', ConvertLocationKeyToCaption);
  eLocation.OnFindData := eLocationFindData;
  
  RegisterControl(cmbGatheringMethod,'Sample_Type','Sample_Type_Key', True,
                  ResStr_GatheringMethod);
  RegisterControl(eDate,'');
  RegisterControl(chkGatheringEvent, 'Gathering_Event');
  RegisterControl(eVagueLocation, 'Location_Name');
end;  // TfraFieldDataGeneral.RegisterControls 

{-------------------------------------------------------------------------------
  Register the drag and drop components. 
}
procedure TfraFieldDataGeneral.RegisterDragDropComponents;
begin
  RegisterDropComponent(eSurvey, DropSurvey, [TN_SURVEY], [CF_JNCCDATA]);
  RegisterDropComponent(eLocation, DropLocation, [TN_LOCATION], [CF_JNCCDATA]);
end;  // TfraFieldDataGeneral.RegisterDragDropComponents 

{-------------------------------------------------------------------------------
  Saves the data to the database. 
}
procedure TfraFieldDataGeneral.SaveData;
begin
  inherited;  // For validation to be called on a TBaseFullScreenFrame frame
  CanFindSample;
  
  FSampleKey      := FSampleTable.FSampleKey;
  FSurveyEventKey := FSurveyEventTable.FSurveyEventKey;

  FFieldCollectorList.SampleKey      := FSampleKey;
  FFieldCollectorList.SurveyEventKey := FSurveyEventKey;
  FFieldCollectorList.Update;
  
  // Empty key means new record.
  if Key = '' then
    Key := VarToStr(dmGeneral.RunInsertStoredProc(TN_SPECIMEN_FIELD_DATA,
                                                  'usp_FieldData_Insert', GetParams, '@Key'))
  else
    dmGeneral.RunUpdateStoredProc('usp_FieldData_Update', GetParams);
end;  // TfraFieldDataGeneral.SaveData 

{-------------------------------------------------------------------------------
  Sets the MasterKey in the FFieldCollectorList.
}
procedure TfraFieldDataGeneral.SetKey(Value: TKeyString);
begin
  inherited SetKey(Value);
  FFieldCollectorList.MasterKey := Key;
end;  // TfraFieldDataGeneral.SetKey

{-------------------------------------------------------------------------------
}
procedure TfraFieldDataGeneral.sgCollectorsKeyDown(Sender: TObject; var Key: Word; Shift:
    TShiftState);
begin
  inherited;
  with sgCollectors do
    if (Row = RowCount - 1) and (Key = VK_DOWN) and not CanAddRow then Key := 0;
end;  // TfraFieldDataGeneral.sgCollectorsKeyDown 

{-------------------------------------------------------------------------------
}
procedure TfraFieldDataGeneral.UpdateMapWindowSelector;
begin
  inherited;
  // Have to specify different MenuItem Click handler, as we need the map window
  // displayed AFTER the request for data gets to Recorder.
  dmGeneral.UpdateMapMenu(Self, pmMapWindow.Items, True, MapMenuItemClick);
  eSpatialRef.UpdateMapWindowSelector;
end;  // TfraFieldDataGeneral.UpdateMapWindowSelector 

{-------------------------------------------------------------------------------
}
procedure TfraFieldDataGeneral.UpdateName(const AKeyList: IKeyList);
var
  lKey: TKeyString;
begin
  lKey := AKeyList.GetKeyItem(0).KeyField1;
  
  if not CheckIsIndividual(lKey) then begin
    MessageDlg(ResStr_StaffResponsibleMustBeAnIndividual, mtInformation, [mbOK], 0);
    if eName.CanFocus then eName.SetFocus;
  end else begin
    eName.Key := lKey;
    eName.Text := ConvertIndividualKeyToCaption(lKey, TN_INDIVIDUAL);
  end;
end;  // TfraFieldDataGeneral.UpdateName 

{-------------------------------------------------------------------------------
}
procedure TfraFieldDataGeneral.UpdateRequestedData(const KeyList: IKeyList);
begin
  if CompareText(KeyList.TableName, TN_SPATIAL_REF) = 0 then begin
    if KeyList.ItemCount>0 then begin
      eSpatialRef.SpatialRef := KeyList.GetKeyItem(0).KeyField2;
      eSpatialRef.Qualifier := 'Internal Map';
    end;
  end else
    inherited;
end;  // TfraFieldDataGeneral.UpdateRequestedData 

{-------------------------------------------------------------------------------
  Update the existing sample. 
}
procedure TfraFieldDataGeneral.UpdateSample;
begin
  FillSampleTableValues;
  FSampleTable.Execute;
  IsNewFieldData1;
end;  // TfraFieldDataGeneral.UpdateSample 

{-------------------------------------------------------------------------------
  Update the existing survey event. 
}
procedure TfraFieldDataGeneral.UpdateSurveyEvent;
begin
  FillSurveyEventTableValues;
  FSurveyEventTable.Execute;
  IsNewFieldData2;
end;  // TfraFieldDataGeneral.UpdateSurveyEvent 

{-------------------------------------------------------------------------------
  Validate the data. The user must enter either a location or a spatial reference. 
}
procedure TfraFieldDataGeneral.ValidateData;
var
  lText: String;
  lNoDuplication: Boolean;
  lCurrentDate: TVagueDate;
//  lLatLong: ILatLong;
  lValidResult: TValidationResult;
begin
  inherited;
  ValidateValue(
      (eLocation.Text <> '') or (eSpatialRef.SpatialRef <> '') or (eVagueLocation.Text <> ''),
      Format(
          ResStr_MissingDataForControls,
          [ResStr_SpatialRef + ', ' + ResStr_Location + ' or ' + ResStr_LocationName]),
      eLocation);
  ValidateValue(
      (eSpatialRef.SpatialRef = '') or (eSpatialRef.Qualifier <> ''),
      Format(ResStr_MissingData, [ResStr_SpatialRefQualifier]),
      eSpatialRef);

  // Use ValidationData to validate bounding box and all that.
  if ((eLocation.Text <> '') or (eSpatialRef.SpatialRef <> '') or (eVagueLocation.Text = '')) and
     (eSurvey.Key <> '') then
  begin
    with TdmValidation.Create(nil) do
      try
        SetDatabase(dmGeneral.Connection);
        lValidResult := CheckEventInSurvey(
            eSurvey.Key,
            eSpatialRef.EnteredRef,
            eSpatialRef.EnteredSystem,
            eLocation.Key);
      finally
        Free;
      end;

    with lValidResult do
      if Copy(Message, Length(Message), 1) = '.' then Delete(Message, Length(Message), 1);

    ValidateValue(lValidResult.Success, lValidResult.Message, eLocation);
  end;

  if (eDate.Text <> '') and (eDate.DateTypeString <> 'U') then begin
    lCurrentDate := StringToVagueDate(DateToStr(Date));
    ValidateValue(CompareVagueDateToVagueDate(lCurrentDate, eDate.VagueDate) <> -1,
                  Format(ResStr_DateCannotBeInFuture, [ResStr_Date]), eDate);
  end;

  ValidateStringGrid;
  
  ValidateValue(sgCollectors.Cells[0, 1] <> '', ResStr_MustBeFieldCollector);
  lNoDuplication := CheckNoDuplication(lText);
  ValidateValue(lNoDuplication,
                Format(ResStr_DuplicationInGrid, [lText, ResStr_FieldCollectors]));
end;  // TfraFieldDataGeneral.ValidateData

{-------------------------------------------------------------------------------
  Before saving the data, giving the user the option to change any incorrect names in the
      string grid by using the Find Dialog. If they press cancel, validation furthur down the
      line will reveal the invalid name to the user.
}
procedure TfraFieldDataGeneral.ValidateStringGrid;
var
  lIdx: Integer;
begin
  for lIdx := 1 to sgCollectors.RowCount - 1 do begin
    sgCollectors.Row := lIdx;
    eNameFindData(Self);
  end;
  FFieldCollectorList.ValidateContent;
end;  // TfraFieldDataGeneral.ValidateStringGrid

{-==============================================================================
    TSurveyEventTable
===============================================================================}
{-------------------------------------------------------------------------------
}
function TSurveyEventTable.CheckForExisting: TKeyString;
var
  lLatLong: ILatLong;
begin
  lLatLong := dmGeneral.Recorder.RecorderFunctions.DecodeSpatialRef(FSpatialRef.EnteredRef);
  
  FSurveyEventKey := VarToStr(
      dmGeneral.GetStoredProcOutputParam('usp_SurveyEventKey_Get',
          ['@Key', Null,
           '@SurveyKey', FSurveyKey,
           '@VagueDateStart', FSampleTable.FVagueDate.StartDate,
           '@VagueDateEnd', FSampleTable.FVagueDate.EndDate,
           '@VagueDateType', FSampleTable.FVagueDate.DateTypeString,
           '@SpatialRef', FSpatialRef.EnteredRef,
           '@SpatialRefSystem', FSpatialRef.EnteredSystem,
           '@Lat', lLatLong.Latitude,
           '@Long', lLatLong.Longitude,
           '@SpatialRefQualifier', FSpatialRef.Qualifier,
           '@LocationKey',FSampleTable.FLocationKey,
           '@LocationName', FSampleTable.FLocationName],
          '@Key'));
  Result := FSurveyEventKey;
end;  // TSurveyEventTable.CheckForExisting 

{-------------------------------------------------------------------------------
}
procedure TSurveyEventTable.Clear;
begin
  inherited;
  FSurveyKey := '';
  FSurveyEventKey := '';
end;  // TSurveyEventTable.Clear

{-------------------------------------------------------------------------------
}
procedure TSurveyEventTable.Execute;
var
  ltfOldNullStrictConvert: Boolean;
  lLatLong: ILatLong;
  lLocationName: string;
begin
  inherited;
  lLatLong := dmGeneral.Recorder.RecorderFunctions.DecodeSpatialRef(FSpatialRef.EnteredRef);

  ltfOldNullStrictConvert := NullStrictConvert;
  NullStrictConvert := False;
  // We only store the location name if there is no other locality info, otherwise
  // its possible that each sample in an event could have a different location
  // name so it can't be stored here.
  if (FSpatialRef.EnteredRef='') and (FSampleTable.FLocationKey='') then
    lLocationName := FSampleTable.FLocationName
  else
    lLocationName := '';
  try
    if FSurveyEventKey = '' then
      FSurveyEventKey := VarToStr(
          dmGeneral.RunInsertStoredProc('Survey_Event', 'usp_SurveyEvent_Insert',
              ['@Key', Null,
               '@SurveyKey', FSurveyKey,
               '@EnteredBy', dmGeneral.Recorder.CurrentSettings.UserIDKey,
               '@VagueDateStart', FSampleTable.FVagueDate.StartDate,
               '@VagueDateEnd', FSampleTable.FVagueDate.EndDate,
               '@VagueDateType', FSampleTable.FVagueDate.DateTypeString,
               '@SpatialRef', Iif(FSpatialRef.EnteredRef='', null, FSpatialRef.EnteredRef),
               '@SpatialRefSystem', Iif(FSpatialRef.EnteredRef='', null, FSpatialRef.EnteredSystem),
               '@Lat', Iif(FSpatialRef.EnteredRef='', null, lLatLong.Latitude),
               '@Long', Iif(FSpatialRef.EnteredRef='', null, lLatLong.Longitude),
               '@SpatialRefQualifier', Iif(FSpatialRef.EnteredRef='', null, FSpatialRef.Qualifier),
               '@LocationKey', FSampleTable.FLocationKey,
               '@LocationName', FSampleTable.FLocationName,
               '@Comment',''],
              '@Key'))
    else
      dmGeneral.RunUpdateStoredProc('usp_SurveyEvent_Update',
              ['@Key', FSurveyEventKey,
               '@SurveyKey', FSurveyKey,
               '@ChangedBy', dmGeneral.Recorder.CurrentSettings.UserIDKey,
               '@VagueDateStart', FSampleTable.FVagueDate.StartDate,
               '@VagueDateEnd', FSampleTable.FVagueDate.EndDate,
               '@VagueDateType', FSampleTable.FVagueDate.DateTypeString,
               '@SpatialRef', Iif(FSpatialRef.EnteredRef='', null, FSpatialRef.EnteredRef),
               '@SpatialRefSystem', Iif(FSpatialRef.EnteredRef='', null, FSpatialRef.EnteredSystem),
               '@Lat', Iif(FSpatialRef.EnteredRef='', null, lLatLong.Latitude),
               '@Long', Iif(FSpatialRef.EnteredRef='', null, lLatLong.Longitude),
               '@SpatialRefQualifier', Iif(FSpatialRef.EnteredRef='', null, FSpatialRef.Qualifier),
               '@LocationKey', FSampleTable.FLocationKey,
               '@LocationName', FSampleTable.FLocationName,
               '@Comment',''
              ]);
  finally
    NullStrictConvert := ltfOldNullStrictConvert;
  end;
end;  // TSurveyEventTable.Execute 

{-==============================================================================
    TSampleTable
===============================================================================}
{-------------------------------------------------------------------------------
}
function TSampleTable.CheckForExisting: TKeyString;
var
  lLatLong: ILatLong;
begin
  lLatLong := dmGeneral.Recorder.RecorderFunctions.DecodeSpatialRef(FSpatialRef.EnteredRef);
  
  //first try to find an existing sample
  FSampleKey := VarToStr(dmGeneral.GetStoredProcOutputParam('usp_SampleKey_Get',
        ['@Key', Null,
         '@VagueDateStart', Trunc(FVagueDate.StartDate),
         '@VagueDateEnd', Trunc(FVagueDate.EndDate),
         '@VagueDateType', FVagueDate.DateTypeString,
           '@SpatialRef', FSpatialRef.EnteredRef,
           '@SpatialRefSystem', FSpatialRef.EnteredSystem,
           '@Lat', lLatLong.Latitude,
           '@Long', lLatLong.Longitude,
           '@SpatialRefQualifier', FSpatialRef.Qualifier,
         '@SampleTypeKey', FSampleTypeKey,
         '@SurveyEventKey', FSurveyEventTable.FSurveyEventKey,
         '@LocationName', FLocationName,
         '@LocationKey', FLocationKey,
         '@SurveyKey', FSurveyKey],
        '@Key'));
  Result := FSampleKey;
end;  // TSampleTable.CheckForExisting 

{-------------------------------------------------------------------------------
}
procedure TSampleTable.Clear;
begin
  inherited;
  FLocationKey := '';
  FLocationName := '';
  FSampleKey := '';
  FSampleTypeKey := '';
  FVagueDate.DateTypeString := '';
end;  // TSampleTable.Clear 

{-------------------------------------------------------------------------------
}
procedure TSampleTable.Execute;
var
  ltfOldNullStrictConvert: Boolean;
  lLatLong: ILatLong;
begin
  inherited;
  lLatLong := dmGeneral.Recorder.RecorderFunctions.DecodeSpatialRef(FSpatialRef.EnteredRef);
  
  ltfOldNullStrictConvert := NullStrictConvert;
  NullStrictConvert := False;
  try
    if FSampleKey = '' then //insert a sample
      FSampleKey := VarToStr(
        dmGeneral.RunInsertStoredProc(TN_SAMPLE, 'usp_Sample_Insert',
            ['@Key', Null,
             '@VagueDateStart', Trunc(FVagueDate.StartDate),
             '@VagueDateEnd', Trunc(FVagueDate.EndDate),
             '@VagueDateType', FVagueDate.DateTypeString,
             '@SpatialRef', Iif(FSpatialRef.EnteredRef='', null, FSpatialRef.EnteredRef),
             '@SpatialRefSystem', Iif(FSpatialRef.EnteredRef='', null, FSpatialRef.EnteredSystem),
             '@Lat', Iif(FSpatialRef.EnteredRef='', null, lLatLong.Latitude),
             '@Long', Iif(FSpatialRef.EnteredRef='', null, lLatLong.Longitude),
             '@SpatialRefQualifier', Iif(FSpatialRef.EnteredRef='', null, FSpatialRef.Qualifier),
             '@SampleTypeKey', FSampleTypeKey,
             '@EnteredBy', dmGeneral.Recorder.CurrentSettings.UserIDKey,
             '@SurveyEventKey', FSurveyEventTable.FSurveyEventKey,
             '@LocationName', FLocationName,
             '@LocationKey', FLocationKey,
             '@Comment', ''],
            '@Key'))
    else
      dmGeneral.RunUpdateStoredProc('usp_Sample_Update',
            ['@Key', FSampleKey,
             '@VagueDateStart', Trunc(FVagueDate.StartDate),
             '@VagueDateEnd', Trunc(FVagueDate.EndDate),
             '@VagueDateType', FVagueDate.DateTypeString,
             '@SpatialRef', Iif(FSpatialRef.EnteredRef='', null, FSpatialRef.EnteredRef),
             '@SpatialRefSystem', Iif(FSpatialRef.EnteredRef='', null, FSpatialRef.EnteredSystem),
             '@Lat', Iif(FSpatialRef.EnteredRef='', null, lLatLong.Latitude),
             '@Long', Iif(FSpatialRef.EnteredRef='', null, lLatLong.Longitude),
             '@SpatialRefQualifier', Iif(FSpatialRef.EnteredRef='', null, FSpatialRef.Qualifier),
             '@SampleTypeKey', FSampleTypeKey,
             '@ChangedBy', dmGeneral.Recorder.CurrentSettings.UserIDKey,
             '@SurveyEventKey', FSurveyEventTable.FSurveyEventKey,
             '@LocationName', FLocationName,
             '@LocationKey', FLocationKey,
             '@Comment', '']);
  finally
    NullStrictConvert := ltfOldNullStrictConvert;
  end;
end;  // TSampleTable.Execute

end.

