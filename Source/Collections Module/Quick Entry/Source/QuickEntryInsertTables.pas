{===============================================================================
  Unit:        QuickEntryInsertTables.pas

  Defines:     TInsertWrapper (The only class here defined designed to be
                  used outside this unit)

  Description: Defines classes used in inserting records using the
               QuickEntryScreen. Also has Validation function which determines
               if enough fields have been entered. This is used on the Quick
               Entry screen and also the QuickEntryManager screen.

  Model:       QuickEntry.mpb

  Created:     September 2003

===============================================================================}

unit QuickEntryInsertTables;

interface

uses
  SysUtils, Windows, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, VagueDate, ExceptionForm, DataClasses, ADODB;

resourcestring
  ResStr_Sample = 'Sample';
  ResStr_DeterminationDateMustBeAfterSampleDate =
      'The Determination Date cannot occur before the Sample Date.';

type

  {-----------------------------------------------------------------------------
    Exception class for QuickEntryFrame unit.
  }
  EQuickEntryException = class(TExceptionPath)
  end;

  TTableName = (tnDetermination, tnOccurrence, tnSample, tnSurveyEvent,
      tnSpecimenUnit, tnSpecimenLabel, tnMovementOfMaterial, tnCollectionUnitNumber,
      tnSurveyEventRecorder, tnMovementOfOwnership, tnMovement, tnGeoArea);

  TFieldName = (fnCollectionUnitKey, fnUsualLocationCode, fnCurrentLocationCode,
      fnUsualContainerCollectionUnitKey, fnCurrentContainerCollectionUnitKey,
      fnParentCollectionCollectionUnitKey, fnSpecimenTypeConceptKey,
      fnConfidential, fnDangerous, fnLifeSciences, fnDeterminationKey,
      fnConceptKey, fnOccurrenceKey, fnDeterminerNameKey, fnVagueDate,
      fnMovementCollectionUnitKey, fnMovementDirectionKey, fnUsedSpecimen,
      fnTaxonListItemKey, fnComment, fnSampleKey, fnSampleTypeKey, fnSpatialRef,
      fnReceiverOrganisationNameKey, fnNumber, fnInscription, fnNameKey, fnRecordTypeKey,
      fnLocationName, fnLocationKey, fnSurveyKey, fnLabel, fnValue, fnSurveyEventKey,
      fnDeterminerRoleKey, fnTaxonQualifierKey, fnTaxonUnitKey, fnStaffResponsibleNameKey,
      fnAcquisitionNumber, fnAccessionNumber, fnInferredCollectors, fnInferredDeterminers,
      fnGeoAreaKey, fnInternalUse);

  TFieldNames = set of TFieldName;

  TInvalidFields = array[TTableName] of TFieldNames;

  TDecodedSpatialRef = record
    SpatialRef: Variant;
    SpatialRefSystem: Variant;
    Lat: Variant;
    Long: Variant;
    SpatialRefQualifier: Variant;
  end;

  EInsertTablesException = class(EQuickEntryException)
  end;
  
  TInsertWrapper = class;

  {-----------------------------------------------------------------------------
    Base class for classes that wrap the fields for a single table.  These
    classes are used to populate the fields in the table when Quick Entry
    data is processed.
  }
  TInsertTable = class(TObject)
  private
    FWillInsert: Boolean;
    FWrapper: TInsertWrapper;
    function GetFields(FieldName: TFieldName; AIndex: Integer): string; virtual; abstract;
  protected
    FChildTables: TList;
    FParentTables: TList;
    procedure Clear; virtual;
    function DecodeStructuredSpatialRef(const AStructuredRef: string):
        TDecodedSpatialRef;
    procedure SetFields(FieldName: TFieldName; AIndex: Integer; const Value: string); virtual;
  public
    constructor Create(AWrapper: TInsertWrapper); virtual;
    destructor Destroy; override;
    procedure AddParentTable(lInsertTable: TInsertTable);
    function CheckForExisting: TKeyString; virtual;
    procedure CheckLinks; virtual; abstract;
    function ClearIfDescendantWillInsert: Boolean; virtual;
    function ClearIfParentWillInsert: Boolean; virtual;
    procedure Execute(APrepared : Boolean = False); virtual;
    function Validate: TFieldNames; virtual; abstract;
    property Fields[FieldName: TFieldName; AIndex: Integer]: string read GetFields write
        SetFields;
    property WillInsert: Boolean read FWillInsert;
    property Wrapper: TInsertWrapper read FWrapper;
  end;
  
  {-----------------------------------------------------------------------------
    Class that wraps fields in the Survey_Event table.
    Survey events need a child sample.
  }
  TSurveyEventTable = class(TInsertTable)
  private
    FSurveyEventKey: string;
    FSurveyKey: string;
    function GetFields(FieldName: TFieldName; AIndex: Integer): string; override;
  protected
    procedure SetFields(FieldName: TFieldName; AIndex: Integer; const Value: string); override;
  public
    function CheckForExisting: TKeyString; override;
    procedure CheckLinks; override;
    procedure Clear; override;
    procedure Execute(APrepared : Boolean = False); override;
    function Validate: TFieldNames; override;
  end;
  
  {-----------------------------------------------------------------------------
    Class that wraps fields in the Specimen_Unit table.
    Specimen units need a parent determination.
  }
  TSpecimenUnitTable = class(TInsertTable)
  private
    FCollectionUnitKey: string;
    FCurrentContainerCollectionUnitKey: string;
    FCurrentLocationCode: string;
    FDangerous: string;
    FLifeSciences: string;
    FParentCollectionCollectionUnitKey: string;
    FSpecimenTypeConceptKey: string;
    FUsualContainerCollectionUnitKey: string;
    FUsualLocationCode: string;
    FInternalUse: string;
    function GetFields(FieldName: TFieldName; AIndex: Integer): string; override;
  protected
    procedure SetFields(FieldName: TFieldName; AIndex: Integer; const Value: string); override;
  public
    procedure CheckLinks; override;
    procedure Clear; override;
    procedure Execute(APrepared : Boolean = False); override;
    function Validate: TFieldNames; override;
  end;
  
  {-----------------------------------------------------------------------------
    Class that wraps fields in the Occurrence_Data table.
  }
  TSpecimenLabelTable = class(TInsertTable)
  private
    FInscription: string;
    FLabel: string;
    FSpecimenLabelKey: string;
    function GetFields(FieldName: TFieldName; AIndex: Integer): string; override;
  protected
    procedure SetFields(FieldName: TFieldName; AIndex: Integer; const Value: string); override;
  public
    procedure CheckLinks; override;
    procedure Clear; override;
    procedure Execute(APrepared : Boolean = False); override;
    function Validate: TFieldNames; override;
  end;
  
  {-----------------------------------------------------------------------------
    Class that wraps fields in the Sample table.
  }
  TSampleTable = class(TInsertTable)
  private
    FLocationKey: string;
    FLocationName: string;
    FSampleKey: string;
    FSampleTypeKey: string;
    FStructuredSpatialRef: string;
    FSurveyKey: string;
    FVagueDate: TVagueDate;
    function GetFields(FieldName: TFieldName; AIndex: Integer): string; override;
    procedure ValidateSpatialRef;
  protected
    procedure SetFields(FieldName: TFieldName; AIndex: Integer; const Value: string); override;
  public
    function CheckForExisting: TKeyString; override;
    procedure CheckLinks; override;
    procedure Clear; override;
    procedure Execute(APrepared : Boolean = False); override;
    function Validate: TFieldNames; override;
  end;

  {-----------------------------------------------------------------------------
    Class that wraps fields in the Occurrence or Taxon_Occurrence table.
  }
  TOccurrenceTable = class(TInsertTable)
  private
    FComment: string;
    FHasInserted: Boolean;
    FOccurrenceKey: string;
    FRecordTypeKey: string;
    FInferredCollectors: string;
    FInferredDeterminers: string;
    function GetFields(FieldName: TFieldName; AIndex: Integer): string; override;
  protected
    procedure SetFields(FieldName: TFieldName; AIndex: Integer; const Value: string); override;
  public
    constructor Create(AInsertWrapper: TInsertWrapper); override;
    procedure CheckLinks; override;
    procedure Clear; override;
    procedure Execute(APrepared : Boolean = False); override;
    function Validate: TFieldNames; override;
    property HasInserted: Boolean read FHasInserted;
  end;

  {-----------------------------------------------------------------------------
    Class that wraps fields in the Movement_Of_Material table.
  }
  TMovementOfMaterialTable = class(TInsertTable)
  private
    FReceiverOrganisationNameKey: string;
    function GetFields(FieldName: TFieldName; AIndex: Integer): string; override;
  protected
    procedure SetFields(FieldName: TFieldName; AIndex: Integer; const Value: string); override;
  public
    procedure CheckLinks; override;
    procedure Clear; override;
    procedure Execute(APrepared : Boolean = False); override;
    function Validate: TFieldNames; override;
  end;

  {-----------------------------------------------------------------------------
    Class that wraps fields in the Movement_Of_Ownership table.
  }
  TMovementOfOwnershipTable = class(TInsertTable)
  private
    function GetFields(FieldName: TFieldName; AIndex: Integer): string; override;
  protected
    procedure SetFields(FieldName: TFieldName; AIndex: Integer; const Value: string); override;
  public
    procedure CheckLinks; override;
    procedure Clear; override;
    procedure Execute(APrepared : Boolean = False); override;
    function Validate: TFieldNames; override;
  end;

  {-----------------------------------------------------------------------------
    Class that wraps fields in the Movement table.
  }
  TMovementTable = class(TInsertTable)
  private
    FMovementKey: string;
    FMovementDirectionKey: string;
    FMovementNumber: string;
    FStaffResponsibleNameKey: string;
    FMovementCollectionUnitKey: string;
    function GetFields(FieldName: TFieldName; AIndex: Integer): string; override;
    procedure GetMovementDirectionKey;
  protected
    procedure SetFields(FieldName: TFieldName; AIndex: Integer; const Value: string); override;
  public
    constructor Create(AWrapper: TInsertWrapper); override;
    procedure CheckLinks; override;
    procedure Clear; override;
    procedure Execute(APrepared : Boolean = False); override;
    function Validate: TFieldNames; override;
  end;

  {-----------------------------------------------------------------------------
    Class that wraps fields in the Metadata table.
  }
  TMetadataTable = class(TInsertTable)
  private
    FMetadataText: string;
    FMetadataKey: TKeyString;
    FMetadataTypeKey: TKeyString;
    function GetFields(FieldName: TFieldName; AIndex: Integer): string; override;
  protected
    procedure SetFields(FieldName: TFieldName; AIndex: Integer; const Value: string); override;
  public
    constructor Create(AWrapper: TInsertWrapper); override;
    procedure CheckLinks; override;
    procedure Clear; override;
    procedure Execute(APrepared : Boolean = False); override;
    function Validate: TFieldNames; override;
  end;

  {-----------------------------------------------------------------------------
    Class that wraps fields in the Determination table.
  }
  TDeterminationTable = class(TInsertTable)
  private
    FConceptKey: string;
    FDeterminationKey: string;
    FDeterminerNameKeys: TStringList;
    FDeterminerRoleKey: String;
    FIsForSpecimen: Boolean;
    FTaxonListItemKey: string;
    FVagueDate: TVagueDate;
    function GetFields(FieldName: TFieldName; AIndex: Integer): string; override;
    procedure SetIsForSpecimen(Value: Boolean);
    procedure ReadDefaultDeterminerName;
  protected
    procedure Clear; override;
    procedure SetFields(FieldName: TFieldName; AIndex: Integer; const Value: string); override;
  public
    constructor Create(AWrapper: TInsertWrapper); override;
    destructor Destroy; override;
    procedure CheckLinks; override;
    procedure Execute(APrepared : Boolean = False); override;
    function Validate: TFieldNames; override;
    property IsForSpecimen: Boolean read FIsForSpecimen write SetIsForSpecimen;
    property DeterminationKey: string read FDeterminationKey write
        FDeterminationKey;
  end;

  {-----------------------------------------------------------------------------
    Class that wraps fields in the Collection_Unit_Number table.
  }
  TCollectionUnitNumberTable = class(TInsertTable)
  private
    FNumber: string;
    FNumberTypeKey: string;
    FNumberPreferred: Boolean;
    function GetFields(FieldName: TFieldName; AIndex: Integer): string; override;
  protected
    procedure SetFields(FieldName: TFieldName; AIndex: Integer; const Value: string); override;
  public
    constructor Create(AWrapper: TInsertWrapper); override;
    procedure CheckLinks; override;
    procedure Clear; override;
    procedure Execute(APrepared : Boolean = False); override;
    function Validate: TFieldNames; override;
  end;

  {-----------------------------------------------------------------------------
    Class that wraps fields in the Survey_Event_Recorder table.
  }
  TSurveyEventRecorderTable = class(TInsertTable)
  private
    FNameKeys: TStringList;
    FSERecorderKeys: TStringList;
    function GetFields(FieldName: TFieldName; AIndex: Integer): string; override;
  protected
    procedure SetFields(FieldName: TFieldName; AIndex: Integer; const Value: string); override;
  public
    constructor Create(AWrapper: TInsertWrapper); override;
    destructor Destroy; override;
    procedure CheckLinks; override;
    procedure Clear; override;
    procedure Execute(APrepared : Boolean = False); override;
    function Validate: TFieldNames; override;
  end;

  {-----------------------------------------------------------------------------
    Class that wraps fields in the Survey_Event_Geo_Area table.
  }
  TGeoAreaTable = class(TInsertTable)
  private
    FGeoAreaConceptKeys: TStringList;
    FSEGeoAreaKeys: TStringList;
    function GetFields(FieldName: TFieldName; AIndex: Integer): string; override;
  protected
    procedure SetFields(FieldName: TFieldName; AIndex: Integer; const Value: string); override;
  public
    constructor Create(AWrapper: TInsertWrapper); override;
    destructor Destroy; override;
    procedure CheckLinks; override;
    procedure Clear; override;
    procedure Execute(APrepared : Boolean = False); override;
    function Validate: TFieldNames; override;
  end;

  TInsertTableClass = class of TInsertTable;
  
  {-----------------------------------------------------------------------------
    Class that wraps the tables and fields available as destinations for
    data input using Quick Entry.
  }
  TInsertWrapper = class(TObject)
  private
    FCollectionUnitNumberTable: TCollectionUnitNumberTable;
    FConfidential: Boolean;
    FCustomNumberTables: TStringList;
    FDeterminationTables: TStringList;
    FDisplayPrompts: Boolean;
    FMetadataTables: TStringList;
    FMovementOfMaterialTable: TMovementOfMaterialTable;
    FMovementOfOwnershipTable: TMovementOfOwnershipTable;
    FMovementTable: TMovementTable;
    FOccurrenceDataTables: TStringList;
    FOccurrences: Boolean;
    FOccurrenceTable: TOccurrenceTable;
    FRequiresRecorders: Boolean;
    FSampleTable: TSampleTable;
    FSpecimenDataTables: TStringList;
    FSpecimenLabelTable: TSpecimenLabelTable;
    FSpecimenUnitTable: TSpecimenUnitTable;
    FSurveyEventRecorderTable: TSurveyEventRecorderTable;
    FSurveyEventTable: TSurveyEventTable;
    FGeoAreaTable: TGeoAreaTable;
    FtfMovementOfMaterialWillInsert: Boolean;
    FWantOccurrences: Boolean;
    FCreatedFromOccurrence: Boolean;
    FTaxonOccurrenceKey: string;
    FOccurrenceKey: string;
    function GetField(TableName: TTableName; FieldName: TFieldName; AIndex: Integer): string;
    procedure SetField(TableName: TTableName; FieldName: TFieldName; AIndex: Integer; const
        Value: string);
    procedure SetOccurrences(Value: Boolean);
    procedure SetRequiresRecorders(Value: Boolean);
    procedure SetWantOccurrences(Value: Boolean);
    function IsLifeSciences: Boolean;
    procedure AddDetermination(AFieldName: TFieldName; const AIndex: Integer;
        const Value: string);
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddMeasurement(const AAppliesTo, AMethodConceptKey, ADuration,
        AUnitConceptKey, AParameterConceptKey, AValue, AAccuracy,
        ATemplateFieldKey: String; AIsSpecimen: Boolean; AIsTaxonData: Boolean;
        const ATaxonQualifierKey, ATaxonUnitKey: String);
    procedure AddMetadata(AMetadataTypeKey, AMetadataText: String);
    procedure AddNumber(ANumberTypeKey: String; ANumberPreferred: Boolean; ANumber: String);
    procedure AddDeterminations(AFieldName: TFieldName; const AIndex: Integer;
        const Values: TStringList);
    function CheckForExisting(ATableName: TTableName): TKeyString;
    procedure Clear;
    procedure CreateTable(var AVar: TInsertTable; AClass: TInsertTableClass);
    procedure Execute(APrepared : Boolean = False);
    function HasAttachedData: Boolean;
    procedure PretendToExecute;
    function Validate: TInvalidFields;
    property DisplayPrompts: Boolean read FDisplayPrompts write FDisplayPrompts;
    property Field[TableName: TTableName; FieldName: TFieldName; AIndex: Integer]: string
        read GetField write SetField;
    property Occurrences: Boolean read FOccurrences write SetOccurrences;
    property RequiresRecorders: Boolean read FRequiresRecorders write
        SetRequiresRecorders;
    property WantOccurrences: Boolean read FWantOccurrences write
        SetWantOccurrences;
    property CreatedFromOccurrence: Boolean read FCreatedFromOccurrence
        write FCreatedFromOccurrence;
    property TaxonOccurrenceKey: string read FTaxonOccurrenceKey write
        FTaxonOccurrenceKey;
    property OccurrenceKey: string read FOccurrenceKey write
        FOccurrenceKey;
  end;
  
  {-----------------------------------------------------------------------------
    Base class for classes that wraps fields in any Data (Measurement) table.
  }
  TDataTable = class(TInsertTable)
  private
    function GetFields(FieldName: TFieldName; AIndex: Integer): string; override;
  protected
    FAccuracy: string;
    FAppliesTo: string;
    FDuration: string;
    FMethodConceptKey: string;
    FParameterConceptKey: string;
    FUnitConceptKey: string;
    FValue: string;
    FIsTaxonData: Boolean;
    FTaxonQualifierKey: String;
    FTaxonUnitKey: String;
    procedure SetFields(FieldName: TFieldName; AIndex: Integer; const Value: string); override;
  public
    function Validate: TFieldNames; override;
  end;

  {-----------------------------------------------------------------------------
    Class that wraps fields in the Occurrence_Data table.
  }
  TOccurrenceDataTable = class(TDataTable)
  public
    procedure CheckLinks; override;
    procedure Execute(APrepared : Boolean = False); override;
  end;

  {-----------------------------------------------------------------------------
    Class that wraps fields in the Collection_Unit_Data table.
  }
  TSpecimenDataTable = class(TDataTable)
  public
    procedure CheckLinks; override;
    procedure Execute(APrepared : Boolean = False); override;
  end;

function StringToFieldName(st: String): TFieldName;
function StringToTableName(st: String): TTableName;
function StrToVarStr(st:String): Variant;

//==============================================================================
implementation

uses
  GeneralData, Variants, StrUtils, Math, LuxembourgFunctions,
  LuxembourgConstants, ResourceStrings, ApplicationSettings,
  BaseADODataModule;

{-------------------------------------------------------------------------------
}
function StrToVarStr(st:String): Variant;
begin
  if st= '' then
    Result := Null
  else
    Result := st;
end;

{-------------------------------------------------------------------------------
}
function StringToFieldName(st: String): TFieldName;
begin
  if SameText(st, 'Collection_Unit_Key') or
      SameText(st, 'Specimen_Unit_Key') then
    Result := fnCollectionUnitKey
  else if SameText(st, 'Usual_Location_Code') then
    Result := fnUsualLocationCode
  else if SameText(st, 'Current_Location_Code') then
    Result := fnCurrentLocationCode
  else if SameText(st, 'Usual_Container_Collection_Unit_Key') then
    Result := fnUsualContainerCollectionUnitKey
  else if SameText(st, 'Current_Container_Collection_Unit_Key') then
    Result := fnCurrentContainerCollectionUnitKey
  else if SameText(st, 'Parent_collection_Collection_Unit_Key') then
    Result := fnParentCollectionCollectionUnitKey
  else if SameText(st, 'Specimen_Type_Concept_Key') then
    Result := fnSpecimenTypeConceptKey
  else if SameText(st, 'Confidential') then
    Result := fnConfidential
  else if SameText(st, 'Dangerous') then
    Result := fnDangerous
  else if SameText(st, 'Life_Sciences') then
    Result := fnLifeSciences
  else if SameText(st, 'Determination_Key') then
    Result := fnDeterminationKey
  else if SameText(st, 'Concept_Key') then
    Result := fnConceptKey
  else if SameText(st, 'Occurrence_Key') then
    Result :=  fnOccurrenceKey
  else if SameText(st, 'Determiner_Name_Key') then
    Result := fnDeterminerNameKey
  else if SameText(st, 'Vague_Date') then
    Result := fnVagueDate
  else if SameText(st, 'Movement_Collection_Unit_Key') then
    Result := fnMovementCollectionUnitKey
  else if SameText(st, 'Movement_Direction_Key') then
    Result := fnMovementDirectionKey
  else if SameText(st, 'Spatial_Ref') then
    Result := fnSpatialRef
  else if SameText(st, 'Receiver_Organisation_Name_Key') then
    Result := fnReceiverOrganisationNameKey
  else if SameText(st, 'Number') then
    Result := fnNumber
  else if SameText(st, 'Sample_Type_Key') then
    Result := fnSampleTypeKey
  else if SameText(st, 'Sample_Key') then
    Result := fnSampleKey
  else if SameText(st, 'Inscription') then
    Result := fnInscription
  else if SameText(st, 'Label') then
    Result := fnLabel
  else if SameText(st, 'Taxon_List_Item_Key') then
    Result := fnTaxonListItemKey
  else if SameText(st, 'Name_Key') then
    Result := fnNameKey
  else if SameText(st, 'Record_Type_Key') then
    Result := fnRecordTypeKey
  else if SameText(st, 'Location_Name') then
    Result := fnLocationName
  else if SameText(st, 'Comment') then
    Result := fnComment
  else if SameText(st, 'Location_Key') then
    Result := fnLocationKey
  else if SameText(st, 'Survey_Key') then
    Result := fnSurveyKey
  else if SameText(st, 'Determiner_Role_Key') then
    Result := fnDeterminerRoleKey
  else if SameText(st, 'Staff_Responsible_Name_Key') then
    Result := fnStaffResponsibleNameKey
  else if SameText(st, 'Accession_Number') then
    Result := fnAccessionNumber
  else if SameText(st, 'Acquisition_Number') then
    Result := fnAcquisitionNumber    
  else if SameText(st, 'Inferred_Collectors') then
    Result := fnInferredCollectors
  else if SameText(st, 'Inferred_Determiners') then
    Result := fnInferredDeterminers
  else if SameText(st, 'Survey_Event_Geo_Area_Key') then
    Result := fnGeoAreaKey
  else if SameText(st, 'Internal_Use') then
    Result := fnInternalUse
  else
    raise EInsertTablesException.Create('Invalid Field Name: ' + st);
end;

{-------------------------------------------------------------------------------
}
function StringToTableName(st: String): TTableName;
begin
   if (CompareText(st, 'Determination') =0) or
      (CompareText(st, 'Taxon_Determination') =0)then
     Result := tnDetermination
   else if (CompareText(st, 'Occurrence') =0) or
      (CompareText(st, 'Taxon_Occurrence') =0) then
     Result := tnOccurrence
   else if CompareText(st, 'Sample') =0 then
     Result := tnSample
   else if CompareText(st, 'Survey_Event') =0 then
     Result := tnSurveyEvent
   else if CompareText(st, 'Specimen_Unit') =0 then
     Result := tnSpecimenUnit
   else if CompareText(st, 'Specimen_Label') =0 then
     Result := tnSpecimenLabel
   else if CompareText(st, 'Movement_Of_Material') = 0 then
     Result := tnMovementOfMaterial
   else if CompareText(st, 'Movement_Of_Ownership') = 0 then
     Result := tnMovementOfOwnership
   else if CompareText(st, 'Collection_Unit') =0 then
     Result := tnSpecimenUnit
   else if CompareText(st, 'Collection_Unit_Number') = 0 then
     Result := tnCollectionUnitNumber
   else if CompareText(st, 'Survey_Event_Recorder') = 0 then
     Result := tnSurveyEventRecorder
   else if CompareText(st, 'Movement') = 0 then
     Result := tnMovement
   else if CompareText(st, 'Survey_Event_Geo_Area') = 0 then
    Result := tnGeoArea
   else
     raise EInsertTablesException.Create('Invalid Table Name: ' + st);
end;

{-------------------------------------------------------------------------------
}
function StrToVarBool(st: String) :Variant;
begin
  if st = Sysutils.BoolToStr(True) then
    Result := True
  else if st = Sysutils.BoolToStr(False) then
    Result := False
  else
    Result := Null;
end;

{-==============================================================================
    TInsertWrapper
===============================================================================}
{-------------------------------------------------------------------------------
}
constructor TInsertWrapper.Create;
begin
  FOccurrenceDataTables := TStringList.Create;
  FSpecimenDataTables := TStringList.Create;
  FMetadataTables := TStringList.Create;
  FCustomNumberTables := TStringList.Create;
  FDeterminationTables := TStringList.Create;
  FWantOccurrences := True;
  RequiresRecorders := True;
end;  // TInsertWrapper.Create 

{-------------------------------------------------------------------------------
}
destructor TInsertWrapper.Destroy;
var
  i: Integer;
begin
  FCollectionUnitNumberTable.Free;
  FMovementOfMaterialTable.Free;
  FMovementOfOwnershipTable.Free;
  FOccurrenceTable.Free;
  FSampleTable.Free;
  FSpecimenLabelTable.Free;
  FSpecimenUnitTable.Free;
  FGeoAreaTable.Free;
  FSurveyEventRecorderTable.Free;
  FSurveyEventTable.Free;
  for i := 0 to FSpecimenDataTables.Count - 1 do
    FSpecimenDataTables.Objects[i].Free;
  for i := 0 to FOccurrenceDataTables.Count - 1 do
    FOccurrenceDataTables.Objects[i].Free;
  for i := 0 to FMetadataTables.Count - 1 do
    FMetadataTables.Objects[i].Free;
  for i := 0 to FCustomNumberTables.Count - 1 do
    FCustomNumberTables.Objects[i].Free;
  for i := 0 to FDeterminationTables.Count - 1 do
    FDeterminationTables.Objects[i].Free;
  FSpecimenDataTables.Clear;
  FOccurrenceDataTables.Clear;
  FMetadataTables.Clear;
  FCustomNumberTables.Clear;
  FDeterminationTables.Clear;
  FSpecimenDataTables.Free;
  FOccurrenceDataTables.Free;
  FMetadataTables.Free;
  FCustomNumberTables.Free;
  FDeterminationTables.Free;
  inherited;
end;  // TInsertWrapper.Destroy 

{-------------------------------------------------------------------------------
}
procedure TInsertWrapper.AddMeasurement(const AAppliesTo, AMethodConceptKey,
    ADuration, AUnitConceptKey, AParameterConceptKey, AValue, AAccuracy,
    ATemplateFieldKey: String; AIsSpecimen: Boolean; AIsTaxonData: Boolean;
    const ATaxonQualifierKey, ATaxonUnitKey: String);
var
  lStringList: TStringList;
  lDataTable: TDataTable;
  i: Integer;
begin
  if AIsSpecimen then
    lStringList := FSpecimenDataTables
  else
    lStringList := FOccurrenceDataTables;
  i := lStringList.IndexOf(ATemplateFieldKey);
  if i = -1 then
  begin
    if AIsSpecimen then
      lDataTable := TSpecimenDataTable.Create(Self)
    else
      lDataTable := TOccurrenceDataTable.Create(Self);
    lDataTable.FAccuracy := AAccuracy;
    lDataTable.FAppliesTo := AAppliesTo;
    lDataTable.FDuration := ADuration;
    lDataTable.FMethodConceptKey := AMethodConceptKey;
    lDataTable.FParameterConceptKey := AParameterConceptKey;
    lDataTable.FUnitConceptKey := AUnitConceptKey;
    lDataTable.FIsTaxonData := AIsTaxonData;
    lDataTable.FTaxonQualifierKey := ATaxonQualifierKey;
    lDataTable.FTaxonUnitKey := ATaxonUnitKey;
    lDataTable.CheckLinks;
    lStringList.AddObject(ATemplateFieldKey, lDataTable);
  end else
    lDataTable := TDataTable(lStringList.Objects[i]);
  lDataTable.Fields[fnValue, 0] := AValue;
end;  // TInsertWrapper.AddMeasurement

{-------------------------------------------------------------------------------
}
procedure TInsertWrapper.AddMetadata(AMetadataTypeKey, AMetadataText: String);
var
  lMetadataTable: TMetadataTable;
  i: Integer;
begin
  i := FMetadataTables.IndexOf(AMetadataTypeKey);
  if i = -1 then begin
    lMetadataTable := TMetadataTable.Create(Self);
    lMetadataTable.FMetadataTypeKey := AMetadataTypeKey;
    lMetadataTable.FMetadataText := AMetadataText;
    FMetadataTables.AddObject(AMetadataTypeKey, lMetadataTable);
  end else
    lMetadataTable := TMetadataTable(FMetadataTables.Objects[i]);
  lMetadataTable.Fields[fnValue, 0] := AMetadataText;
end;  // TInsertWrapper.AddMetadata

{-------------------------------------------------------------------------------
}
procedure TInsertWrapper.AddNumber(ANumberTypeKey: String; ANumberPreferred: Boolean;
    ANumber: String);
var
  lCustomNumberTable: TCollectionUnitNumberTable;
  i: Integer;
begin
  i := FCustomNumberTables.IndexOf(ANumberTypeKey);
  if i = -1 then begin
    lCustomNumberTable := TCollectionUnitNumberTable.Create(Self);
    lCustomNumberTable.FNumberTypeKey := ANumberTypeKey;
    lCustomNumberTable.FNumberPreferred := ANumberPreferred;
    lCustomNumberTable.FNumber := ANumber;
    FCustomNumberTables.AddObject(ANumberTypeKey, lCustomNumberTable);
  end else
    lCustomNumberTable := TCollectionUnitNumberTable(FCustomNumberTables.Objects[i]);
  lCustomNumberTable.Fields[fnNumber, 0] := ANumber;
end;  // TInsertWrapper.AddNumber

{-------------------------------------------------------------------------------
  Adds a determination table and sets the value of the specified field.
  If this field is not the ConceptKey, the value is set for all the
  Determination tables, since there is no way of identifying which one
  it relates to, and they should all have identical data anyway.
  The corresponding Determiner Name is stored in the
  TDeterminationTable.FDeterminerNameKeys field, using the same index as the
  DeterminationTable itself is stored at in FDeterminationTables.
}
procedure TInsertWrapper.AddDetermination(AFieldName: TFieldName;
  const AIndex: Integer; const Value: string);
var
  lDeterminationTable: TDeterminationTable;
  i: integer;
begin
  if AFieldName = fnConceptKey then begin
    // FDeterminationTables uses the ConceptKey as the string to store determination tables
    i := FDeterminationTables.IndexOf(Value);
    if (i < 0) then begin
      if (FDeterminationTables.Count > 0) and (FDeterminationTables[0] = '') then begin
        // Unlabelled determination table found - use ConceptKey to label it
        FDeterminationTables[0] := Value;
        lDeterminationTable := TDeterminationTable(FDeterminationTables.Objects[0]);
      end else begin
        // No matching determination table - create a new one
        lDeterminationTable := TDeterminationTable.Create(Self);
        lDeterminationTable.CheckLinks;
        FDeterminationTables.AddObject(Value, lDeterminationTable);
      end;    // if (FDeterminationTables.Count > 0) and (FDeterminationTables[0] = '')
    end else begin
      // Use the matching determination table
      lDeterminationTable := TDeterminationTable(FDeterminationTables.Objects[i]);
    end;    // if (FDeterminationTables.IndexOf(Value) < 0)
    lDeterminationTable.Fields[AFieldName, AIndex] := Value;
  end else begin
    // We don't know the concept key, so can't locate a specific determination table
    if FDeterminationTables.Count = 0 then begin
      // No determination table - create a new unlabelled one
      lDeterminationTable := TDeterminationTable.Create(Self);
      lDeterminationTable.CheckLinks;
      FDeterminationTables.AddObject('', lDeterminationTable);
    end;    // if FDeterminationTables.Count = 0
    // Set the field value for all determination tables
    for i := 0 to Pred(FDeterminationTables.Count) do
      TDeterminationTable(FDeterminationTables.Objects[i]).Fields[AFieldName, AIndex] := Value;
  end;    // if AFieldName = fnConceptKey
end;    // TInsertWrapper.AddDetermination(

{-------------------------------------------------------------------------------
}
procedure TInsertWrapper.AddDeterminations(AFieldName: TFieldName;
  const AIndex: Integer; const Values: TStringList);
var
  i: integer;
begin
  for i := 0 to Pred(Values.Count) do
    AddDetermination(AFieldName, AIndex, Values.Names[i]);
end;    // TInsertWrapper.AddDeterminations

{-------------------------------------------------------------------------------
}
function TInsertWrapper.CheckForExisting(ATableName: TTableName): TKeyString;
  
  function CheckForExisting(iInsertTable: TInsertTable): TKeyString;
  begin
    if Assigned(iInsertTable) then
      Result := iInsertTable.CheckForExisting;
  end;
  
begin
  case ATableName of
    tnSample :      Result := CheckForExisting(FSampleTable);
    tnSurveyEvent : Result := CheckForExisting(FSurveyEventTable);
  end;
end;  // TInsertWrapper.CheckForExisting 

{-------------------------------------------------------------------------------
}
procedure TInsertWrapper.Clear;
var
  i: Integer;
  
  procedure ClearTable(iInsertTable: TInsertTable);
  begin
    if Assigned(iInsertTable) then
      iInsertTable.Clear;
  end;
  
begin
  ClearTable(FCollectionUnitNumberTable);
  ClearTable(FMovementOfMaterialTable);
  ClearTable(FMovementOfOwnershipTable);
  ClearTable(FOccurrenceTable);
  ClearTable(FSampleTable);
  ClearTable(FSpecimenLabelTable);
  ClearTable(FSpecimenUnitTable);
  ClearTable(FGeoAreaTable);
  ClearTable(FSurveyEventRecorderTable);
  ClearTable(FSurveyEventTable);
  for i := 0 to FSpecimenDataTables.Count - 1 do
    ClearTable(TSpecimenDataTable(FSpecimenDataTables.Objects[i]));
  for i := 0 to FOccurrenceDataTables.Count - 1 do
    ClearTable(TOccurrenceDataTable(FOccurrenceDataTables.Objects[i]));
  for i := 0 to FMetadataTables.Count - 1 do
    ClearTable(TMetadataTable(FMetadataTables.Objects[i]));
  for i := 0 to FCustomNumberTables.Count - 1 do
    ClearTable(TCollectionUnitNumberTable(FCustomNumberTables.Objects[i]));
  for i := 0 to FDeterminationTables.Count - 1 do
    ClearTable(TDeterminationTable(FDeterminationTables.Objects[i]));
end;  // TInsertWrapper.Clear

{-------------------------------------------------------------------------------
  Creates an insert table and ensures its links are set up.  If it already
      exists, then ignored.
}
procedure TInsertWrapper.CreateTable(var AVar: TInsertTable; AClass:
    TInsertTableClass);
begin
  if not Assigned(AVar) then begin
    AVar := AClass.Create(Self);
    AVar.CheckLinks;
  end;
end;  // TInsertWrapper.CreateTable

{-------------------------------------------------------------------------------
  Trigger the insert procedures required for each table that has been linked to
      the quick entry form.  The order of tables executed is selected to ensure
      no referential integrity problems.
}
procedure TInsertWrapper.Execute(APrepared : Boolean = False);
var
  i: Integer;  
  procedure ExecuteTable(iInsertTable: TInsertTable; APrepared: Boolean =
      False);
  begin
    if Assigned(iInsertTable) then
      if iInsertTable.WillInsert then
         iInsertTable.Execute(APrepared);
  end;

begin
  //save whether FMovementOfMaterial is to be inserted
  if Assigned(FMovementOFMaterialTable) then
    FtfMovementOfMaterialWillInsert := FMovementOfMaterialTable.WillInsert
  else
    FtfMovementOfMaterialWillInsert := False;
  ExecuteTable(FSpecimenUnitTable);

  for i := 0 to FDeterminationTables.Count - 1 do
    ExecuteTable(TDeterminationTable(FDeterminationTables.Objects[i]));

  ExecuteTable(FCollectionUnitNumberTable);
  ExecuteTable(FMovementOfMaterialTable);
  ExecuteTable(FMovementOfOwnershipTable);
  ExecuteTable(FSpecimenLabelTable);
  if (not CreatedFromOccurrence) then
  begin
    ExecuteTable(FSurveyEventTable, APrepared);
    ExecuteTable(FSampleTable, APrepared);
    ExecuteTable(FSurveyEventRecorderTable);
    ExecuteTable(FGeoAreaTable);
  end;

  //If we arrive from an existing occurrence, we must always call
  //FOccurrenceTables's execute to insert specimen field data  
  if ((not Assigned(FOccurrenceTable)) and CreatedFromOccurrence) then
    CreateTable(TInsertTable(FOccurrenceTable), TOccurrenceTable);
  ExecuteTable(FOccurrenceTable);
  if (not CreatedFromOccurrence) then
  begin
    for i := 0 to FOccurrenceDataTables.Count - 1 do
      ExecuteTable(TOccurrenceDataTable(FOccurrenceDataTables.Objects[i]));
  end;
  for i := 0 to FMetadataTables.Count - 1 do
    ExecuteTable(TMetadataTable(FMetadataTables.Objects[i]));
  for i := 0 to FCustomNumberTables.Count - 1 do
    ExecuteTable(TCollectionUnitNumberTable(FCustomNumberTables.Objects[i]));
  for i := 0 to FSpecimenDataTables.Count - 1 do
    ExecuteTable(TSpecimenDataTable(FSpecimenDataTables.Objects[i]));
end;  // TInsertWrapper.Execute

{-------------------------------------------------------------------------------
}
function TInsertWrapper.GetField(TableName: TTableName; FieldName: TFieldName;
    AIndex: Integer): string;
var
  lFromTable: TInsertTable;
begin
  if FieldName = fnConfidential then begin
    if FConfidential then
      Result := ResStr_Yes
    else
      Result := ResStr_No;
  end else begin
    case TableName of
      tnDetermination:
        if FDeterminationTables.Count = 0 then lFromTable := nil
        else lFromTable := TInsertTable(FDeterminationTables.Objects[0]);
      tnOccurrence:           lFromTable := FOccurrenceTable;
      tnSample:               lFromTable := FSampleTable;
      tnSurveyEvent:          lFromTable := FSurveyEventTable;
      tnSpecimenUnit:         lFromTable := FSpecimenUnitTable;
      tnSpecimenLabel:        lFromTable := FSpecimenLabelTable;
      tnMovementOfMaterial:   lFromTable := FMovementOfMaterialTable;
      tnMovementOfOwnership:  lFromTable := FMovementOfOwnershipTable;
      tnCollectionUnitNumber: lFromTable := FCollectionUnitNumberTable;
      tnSurveyEventRecorder:  lFromTable := FSurveyEventRecorderTable;
      tnMovement:             lFromTable := FMovementTable;
      tnGeoArea:              lFromTable := FGeoAreaTable;
    else
      lFromTable := nil;
    end; // case
    if Assigned(lFromTable) then
      Result := lFromTable.Fields[FieldName, AIndex];
  end;
end;  // TInsertWrapper.GetField 

{-------------------------------------------------------------------------------
}
function TInsertWrapper.HasAttachedData: Boolean;
begin
  // Return true if any of the tables are assigned, which means some data exists
  Result := Assigned(FCollectionUnitNumberTable)
      or (FDeterminationTables.Count > 0)
      or Assigned(FMovementOfMaterialTable)
      or Assigned(FMovementOfOwnershipTable)
      or Assigned(FOccurrenceTable)
      or Assigned(FSampleTable)
      or Assigned(FSpecimenLabelTable)
      or Assigned(FSpecimenUnitTable)
      or Assigned(FGeoAreaTable)
      or Assigned(FSurveyEventRecorderTable)
      or Assigned(FSurveyEventTable);
end;  // TInsertWrapper.HasAttachedData 

{-------------------------------------------------------------------------------
  Returns True unless there is a Determination table with a ConceptKey value
}
function TInsertWrapper.IsLifeSciences: Boolean;
var
  i: integer;
begin
  Result := True;
  for i := 0 to Pred(FDeterminationTables.Count) do
    if TDeterminationTable(FDeterminationTables.Objects[i]).FConceptKey <> '' then begin
      Result := False;
      Break;
    end;
end;    // TInsertWrapper.IsLifeSciences

{-------------------------------------------------------------------------------
}
procedure TInsertWrapper.PretendToExecute;
var
  i: Integer;
  
  procedure PretendToExecuteTable(iInsertTable: TInsertTable);
  begin
    if Assigned(iInsertTable) then
      if iInsertTable.WillInsert then
         iInsertTable.FWillInsert := False;
  end;
  
begin
  { TODO : What is the point of this?  All it does is set WillInsert to false
      on all the tables. }
  PretendToExecuteTable(FCollectionUnitNumberTable);
  for i := 0 to FDeterminationTables.Count - 1 do
    PretendToExecuteTable(TDeterminationTable(FDeterminationTables.Objects[i]));
  PretendToExecuteTable(FMovementOfMaterialTable);
  PretendToExecuteTable(FMovementOfOwnershipTable);
  PretendToExecuteTable(FOccurrenceTable);
  PretendToExecuteTable(FSampleTable);
  PretendToExecuteTable(FSpecimenLabelTable);
  PretendToExecuteTable(FSpecimenUnitTable);
  PretendToExecuteTable(FGeoAreaTable);
  PretendToExecuteTable(FSurveyEventRecorderTable);
  PretendToExecuteTable(FSurveyEventTable);
  for i := 0 to FSpecimenDataTables.Count - 1 do
    PretendToExecuteTable(TSpecimenDataTable(FSpecimenDataTables.Objects[i]));
  for i := 0 to FOccurrenceDataTables.Count - 1 do
    PretendToExecuteTable(TOccurrenceDataTable(
        FOccurrenceDataTables.Objects[i]));
  for i := 0 to FMetadataTables.Count - 1 do
    PretendToExecuteTable(TMetaDataTable(FMetadataTables.Objects[i]));
  for i := 0 to FCustomNumberTables.Count - 1 do
    PretendToExecuteTable(TCollectionUnitNumberTable(FCustomNumberTables.Objects[i]));
end;  // TInsertWrapper.PretendToExecute

{-------------------------------------------------------------------------------
}
procedure TInsertWrapper.SetField(TableName: TTableName; FieldName: TFieldName;
    AIndex: Integer; const Value: string);
var
  i: integer;
begin
  if FieldName = fnConfidential then
    // field stored centrally
    FConfidential := Value=Sysutils.BoolToStr(true)
  else
    case TableName of
    tnDetermination:
    begin
      if FDeterminationTables.Count = 0 then
        AddDetermination(FieldName, AIndex, Value)
      else for i := 0 to Pred(FDeterminationTables.Count) do
        TDeterminationTable(FDeterminationTables.Objects[i]).Fields[FieldName, AIndex] := Value;
    end;
    tnOccurrence:
    begin
      CreateTable(TInsertTable(FOccurrenceTable), TOccurrenceTable);
      FOccurrenceTable.Fields[FieldName, AIndex] := Value;
    end;
    tnSample:
    begin
      CreateTable(TInsertTable(FSampleTable), TSampleTable);
      FSampleTable.Fields[FieldName, AIndex] := Value;
    end;
    tnSurveyEvent:
    begin
      CreateTable(TInsertTable(FSurveyEventTable), TSurveyEventTable);
      FSurveyEventTable.Fields[FieldName, AIndex] := Value;
    end;
    tnSpecimenUnit:
    begin
      CreateTable(TInsertTable(FSpecimenUnitTable), TSpecimenUnitTable);
      FSpecimenUnitTable.Fields[FieldName, AIndex] := Value;
    end;
    tnSpecimenLabel:
    begin
      CreateTable(TInsertTable(FSpecimenLabelTable), TSpecimenLabelTable);
      FSpecimenLabelTable.Fields[FieldName, AIndex] := Value;
    end;
    tnMovementOfMaterial:
    begin
      CreateTable(TInsertTable(FMovementOfMaterialTable),
          TMovementOfMaterialTable);
      FMovementOfMaterialTable.Fields[FieldName, AIndex] := Value;
    end;
    tnMovementOfOwnership:
    begin
      CreateTable(TInsertTable(FMovementOfOwnershipTable),
          TMovementOfOwnershipTable);
      FMovementOfOwnershipTable.Fields[FieldName, AIndex] := Value;
    end;
    tnMovement:
    begin
      CreateTable(TInsertTable(FMovementTable), TMovementTable);
      FMovementTable.Fields[FieldName, AIndex] := Value;
    end;
    tnCollectionUnitNumber:
    begin
      CreateTable(TInsertTable(FCollectionUnitNumberTable),
          TCollectionUnitNumberTable);
      FCollectionUnitNumberTable.Fields[FieldName, AIndex] := Value;
    end;
    tnSurveyEventRecorder:
    begin
      CreateTable(TInsertTable(FSurveyEventRecorderTable),
          TSurveyEventRecorderTable);
      FSurveyEventRecorderTable.Fields[FieldName, AIndex] := Value;
    end;
    tnGeoArea:
    begin
      CreateTable(TInsertTable(FGeoAreaTable),
          TGeoAreaTable);
      FGeoAreaTable.Fields[FieldName, AIndex] := Value;
    end;
  end;
end;  // TInsertWrapper.SetField

{-------------------------------------------------------------------------------
  Accessor, ensure that the Occurrences table is set or Specimens table, ready
      for validation.
}
procedure TInsertWrapper.SetOccurrences(Value: Boolean);
begin
  FOccurrences := Value;
  
  if Value and (not Assigned(FOccurrenceTable)) then begin
    FOccurrenceTable := TOccurrenceTable.Create(self);
    FOccurrenceTable.CheckLinks;
  end
  else if (not Value) and (not Assigned(FSpecimenUnitTable)) then begin
    FSpecimenUnitTable := TSpecimenUnitTable.Create(self);
    FSpecimenUnitTable.CheckLinks;
  end;
end;  // TInsertWrapper.SetOccurrences 

{-------------------------------------------------------------------------------
}
procedure TInsertWrapper.SetRequiresRecorders(Value: Boolean);
begin
  FRequiresRecorders := Value;
end;  // TInsertWrapper.SetRequiresRecorders 

{-------------------------------------------------------------------------------
  Accessor method 
}
procedure TInsertWrapper.SetWantOccurrences(Value: Boolean);
begin
  FWantOccurrences := Value;
end;  // TInsertWrapper.SetWantOccurrences


{-------------------------------------------------------------------------------
}
function TInsertWrapper.Validate: TInvalidFields;

  procedure InitializeResult;
  var
    I: TTableName;
  begin
    for I := Low(TTableName) to High(TTableName) do Result[I] := [];
  end;

  function ValidateTable(iInsertTable: TInsertTable): TFieldNames;
  begin
    if Assigned(iInsertTable) then
      if iInsertTable.WillInsert then
        Result := iInsertTable.Validate
      else
        Result := []
    else
      Result := [];
  end;

  function ValidateTables(iInsertTables: TStringList): TFieldNames;
  var
    i: integer;
  begin
    Result := [];
    for i := 0 to Pred(iInsertTables.Count) do
      Result := Result + TInsertTable(iInsertTables.Objects[i]).Validate;
  end;

begin
  InitializeResult;
  Result[tnCollectionUnitNumber] := ValidateTable(FCollectionUnitNumberTable);
  Result[tnDetermination] := ValidateTables(FDeterminationTables);
  Result[tnMovementOfMaterial] := ValidateTable(FMovementOfMaterialTable);
  Result[tnMovementOfOwnership] := ValidateTable(FMovementOfOwnershipTable);
  Result[tnMovement] := ValidateTable(FMovementTable);
  if not CreatedFromOccurrence then
  begin
    Result[tnOccurrence] := ValidateTable(FOccurrenceTable);
    Result[tnSample] := ValidateTable(FSampleTable);
  end;

  Result[tnSpecimenLabel] := ValidateTable(FSpecimenLabelTable);
  Result[tnSpecimenUnit] := ValidateTable(FSpecimenUnitTable);
  if not CreatedFromOccurrence then
  begin
    Result[tnSurveyEventRecorder] := ValidateTable(FSurveyEventRecorderTable);
    Result[tnSurveyEvent] := ValidateTable(FSurveyEventTable);
    Result[tnGeoArea] := ValidateTable(FGeoAreaTable);
  end;
end;  // TInsertWrapper.Validate

{-==============================================================================
    TDataTable
===============================================================================}
{-------------------------------------------------------------------------------
}
function TDataTable.GetFields(FieldName: TFieldName; AIndex: Integer): string;
begin
  if FieldName = fnValue then
    Result := FValue;
end;  // TDataTable.GetFields 

{-------------------------------------------------------------------------------
}
procedure TDataTable.SetFields(FieldName: TFieldName; AIndex: Integer; const Value: string);
begin
  inherited SetFields(FieldName, AIndex, Value);
  
  if FieldName = fnValue then
    FValue := Value;
end;  // TDataTable.SetFields 

{-------------------------------------------------------------------------------
}
function TDataTable.Validate: TFieldNames;
begin
  Result := [];
end;  // TDataTable.Validate 

{-==============================================================================
    TSpecimenDataTable
===============================================================================}

{-------------------------------------------------------------------------------
  Ensure that other required tables exist and are linked.
}
procedure TSpecimenDataTable.CheckLinks;
begin
  with FWrapper do begin
    CreateTable(TInsertTable(FSpecimenUnitTable), TSpecimenUnitTable);
    AddParentTable(FSpecimenUnitTable);
  end;
end;  // TSpecimenDataTable.CheckLinks 

{-------------------------------------------------------------------------------
}
procedure TSpecimenDataTable.Execute(APrepared : Boolean = False);
begin
  inherited;
  dmGeneral.RunInsertStoredProc('Collection_Unit_Data', 'usp_CollectionUnitData_Insert',
      ['@CollectionUnitKey', FWrapper.FSpecimenUnitTable.FCollectionUnitKey,
       '@AppliesTo', FAppliesTo,
       '@MethodConceptKey', FMethodConceptKey,
       '@Duration', FDuration,
       '@Accuracy', FAccuracy,
       '@ParameterConceptKey', FParameterConceptKey,
       '@UnitConceptKey', FUnitConceptKey,
       '@Value', FValue,
       '@IsDescriptor', False,
       '@Key', Null],
      '@Key');
end;  // TSpecimenDataTable.Execute 

{-==============================================================================
    TSurveyEventRecorderTable
===============================================================================}
{-------------------------------------------------------------------------------
}
constructor TSurveyEventRecorderTable.Create(AWrapper: TInsertWrapper);
begin
  inherited Create(AWrapper);

  FNameKeys := TStringList.Create;
  FSERecorderKeys := TStringList.Create;
end;  // TSurveyEventRecorderTable.Create

{-------------------------------------------------------------------------------
}
destructor TSurveyEventRecorderTable.Destroy;
begin
  FNameKeys.Free;
  FSERecorderKeys.Free;

  inherited Destroy;
end;    // TSurveyEventRecorderTable.Destroy

{-------------------------------------------------------------------------------
  Ensure that other required tables exist and are linked.
}
procedure TSurveyEventRecorderTable.CheckLinks;
begin
  with FWrapper do begin
    CreateTable(TInsertTable(FSampleTable), TSampleTable);
    AddParentTable(FSampleTable);
  end;
end;  // TSurveyEventRecorderTable.CheckLinks

{-------------------------------------------------------------------------------
}
procedure TSurveyEventRecorderTable.Clear;
begin
  inherited;
  FNameKeys.Clear;
  FSERecorderKeys.Clear;
end;  // TSurveyEventRecorderTable.Clear

{-------------------------------------------------------------------------------
}
procedure TSurveyEventRecorderTable.Execute(APrepared : Boolean = False);
var
  ltfOldNullStrictConvert: Boolean;
  i: Integer;
begin
  inherited;

  ltfOldNullStrictConvert := NullStrictConvert;
  NullStrictConvert := False;
  try
    for i := 0 to FNameKeys.Count - 1 do begin
      if i < FSERecorderKeys.Count then
        FSERecorderKeys[i] := VarToStr(
            dmGeneral.RunInsertStoredProc('Survey_Event_Recorder',
                'usp_SurveyEventRecorder_Insert',
                ['@Key', Null,
                 '@NameKey', FNameKeys[i],
                 '@SurveyEventKey', FWrapper.FSurveyEventTable.FSurveyEventKey,
                 '@RecorderRoleKey', SURVEYOR_RECORDER_ROLE,
                 '@EnteredBy', dmGeneral.Recorder.CurrentSettings.UserIDKey],
                '@Key'))
      else
        FSERecorderKeys.Add(VarToStr(dmGeneral.RunInsertStoredProc('Survey_Event_Recorder',
                            'usp_SurveyEventRecorder_Insert',
                            ['@Key', Null,
                             '@NameKey', FNameKeys[i],
                             '@SurveyEventKey', FWrapper.FSurveyEventTable.FSurveyEventKey,
                             '@RecorderRoleKey', SURVEYOR_RECORDER_ROLE,
                             '@EnteredBy', dmGeneral.Recorder.CurrentSettings.UserIDKey],
                             '@Key')));
      //insert the sample recorder as well
      dmGeneral.GetRecordset('usp_SampleRecorder_Insert',
                             ['@SampleKey', FWrapper.FSampleTable.FSampleKey,
                              '@SERecorderKey', FSERecorderKeys[i],
                              '@EnteredBy', dmGeneral.Recorder.CurrentSettings.UserIDKey]);
    end;
  finally
    NullStrictConvert := ltfOldNullStrictConvert;
  end;
end;  // TSurveyEventRecorderTable.Execute

{-------------------------------------------------------------------------------
}
function TSurveyEventRecorderTable.GetFields(FieldName: TFieldName; AIndex: Integer): string;
begin
  if FieldName = fnNameKey then begin
    if AIndex < FNameKeys.Count then
      Result := FNameKeys[AIndex]
    else
      Result := '';
  end;
end;  // TSurveyEventRecorderTable.GetFields

{-------------------------------------------------------------------------------
}
procedure TSurveyEventRecorderTable.SetFields(FieldName: TFieldName; AIndex: Integer; const
    Value: string);
var
  i: Integer;
begin
  inherited SetFields(FieldName, AIndex, Value);

  if FieldName = fnNameKey then begin
    if AIndex < FNameKeys.Count then
      FNameKeys[AIndex] := Value
    else begin
      for i := FNameKeys.Count to AIndex - 1 do
        FNameKeys.Add('');
      FNameKeys.Add(Value);
    end;
  end;
end;  // TSurveyEventRecorderTable.SetFields

{-------------------------------------------------------------------------------
  Validate to ensure mandatory fields present.
}
function TSurveyEventRecorderTable.Validate: TFieldNames;
var
  i: Integer;
begin
  Result := [];
  // if any recorders are blank
  for i := 0 to FNameKeys.Count - 1 do
    if FNameKeys[i] = '' then
      Result := Result + [fnNameKey];
  // or there are none whatsoever
  if (FNameKeys.Count=0) and (Result=[]) then
    Result := Result + [fnNameKey];
end;  // TSurveyEventRecorderTable.Validate

{-==============================================================================
    TCollectionUnitNumberTable
===============================================================================}
{-------------------------------------------------------------------------------
  Set default values as the values for the non-custom field registration number.
  If a number is custom, the values will be overwritten later.
  Other non-custom numbers do not use this class.
}
constructor TCollectionUnitNumberTable.Create(AWrapper: TInsertWrapper);
begin
  inherited Create(AWrapper);
  FNumberTypeKey := REGISTRATION_NUMBER;
  FNumberPreferred := True;
end;

{-------------------------------------------------------------------------------
  Ensure that other required tables exist and are linked.
}
procedure TCollectionUnitNumberTable.CheckLinks;
begin
  with FWrapper do begin
    CreateTable(TInsertTable(FSpecimenUnitTable), TSpecimenUnitTable);
    AddParentTable(FSpecimenUnitTable);
  end;
end;  // TCollectionUnitNumberTable.CheckLinks 

{-------------------------------------------------------------------------------
}
procedure TCollectionUnitNumberTable.Clear;
begin
  inherited Clear;
  
  FNumber := '';
end;  // TCollectionUnitNumberTable.Clear 

{-------------------------------------------------------------------------------
}
procedure TCollectionUnitNumberTable.Execute(APrepared : Boolean = False);
var
  ltfOldNullStrictConvert: Boolean;
begin
  inherited;
  ltfOldNullStrictConvert := NullStrictConvert;
  NullStrictConvert := False;
  try
    FNumber := VarToStr(dmGeneral.RunInsertStoredProc('Collection_Unit_Number',
        'usp_CollectionUnitNumber_Insert',
        ['@Key', Null,
         '@CollectionUnitKey', FWrapper.FSpecimenUnitTable.FCollectionUnitKey,
         '@Number', FNumber,
         '@Preferred', IfThen(FNumberPreferred, 1, 0),
         '@TypeConceptKey', FNumberTypeKey,
         '@Notes', Null],
        '@Key'));
  finally
    NullStrictConvert := ltfOldNullStrictConvert;
  end;
end;  // TCollectionUnitNumberTable.Execute

{-------------------------------------------------------------------------------
}
function TCollectionUnitNumberTable.GetFields(FieldName: TFieldName; AIndex: Integer): string;
begin
  case FieldName of
    fnNumber:
      Result := FNumber;
  end;
end;  // TCollectionUnitNumberTable.GetFields 

{-------------------------------------------------------------------------------
}
procedure TCollectionUnitNumberTable.SetFields(FieldName: TFieldName; AIndex: Integer; const
    Value: string);
begin
  inherited SetFields(FieldName, AIndex, Value);
  
  case FieldName of
    fnNumber:
      FNumber := Value;
  end;
end;  // TCollectionUnitNumberTable.SetFields 

{-------------------------------------------------------------------------------
  Validate to ensure mandatory fields present. 
}
function TCollectionUnitNumberTable.Validate: TFieldNames;
begin
  Result := [];
  if FNumber = '' then Result := [fnNumber];
end;  // TCollectionUnitNumberTable.Validate 

{-==============================================================================
    TInsertTable
===============================================================================}
{-------------------------------------------------------------------------------
}
constructor TInsertTable.Create(AWrapper: TInsertWrapper);
begin
  inherited Create;
  
  FWillInsert := True;
  FWrapper := AWrapper;
  FChildTables := TList.Create;
  FParentTables := TList.Create;
end;  // TInsertTable.Create 

{-------------------------------------------------------------------------------
}
destructor TInsertTable.destroy;
begin
  FChildTables.Free;
  FParentTables.Free;
  inherited;
end;  // TInsertTable.destroy 

{-------------------------------------------------------------------------------
}
procedure TInsertTable.AddParentTable(lInsertTable: TInsertTable);
begin
  if FParentTables.IndexOf(lInsertTable)= -1 then
    FParentTables.Add(lInsertTable);
  if lInsertTable.FChildTables.IndexOf(Self) = -1 then
    lInsertTable.FChildTables.Add(Self);
end;  // TInsertTable.AddParentTable 

{-------------------------------------------------------------------------------
}
function TInsertTable.CheckForExisting: TKeyString;
begin
  // TODO -cMM: TInsertTable.CheckForExisting default body inserted
  //Result := ;
end;  // TInsertTable.CheckForExisting 

{-------------------------------------------------------------------------------
}
procedure TInsertTable.Clear;
var
  i: Integer;
begin
  FWillInsert := true;
  for i := 0 to FParentTables.Count - 1 do
    TInsertTable(FParentTables[i]).ClearIfParentWillInsert;
  for i := 0 to FChildTables.Count - 1 do
    TInsertTable(FChildTables[i]).ClearIfDescendantWillInsert;
end;  // TInsertTable.Clear 

{-------------------------------------------------------------------------------
}
function TInsertTable.ClearIfDescendantWillInsert: Boolean;
var
  i: Integer;
begin
  if not FWillInsert then
  begin
    Result := false;
    for i := 0 to FChildTables.Count - 1 do
      if TInsertTable(FChildTables[i]).ClearIfDescendantWillInsert then
        if not Result then
        begin
          Result := True;
          Clear;
        end;
  end
  else
    Result := true;

end;  // TInsertTable.ClearIfDescendantWillInsert 

{-------------------------------------------------------------------------------
  Clears this table if the table has an ancestor which will be inserted. This
      function will be called by a child. For example, if a sample and a taxon
      determination are to be inserted, then the taxon determination calls this
      function on the taxon occurrence table.
}
function TInsertTable.ClearIfParentWillInsert: Boolean;
var
  i: Integer;
begin
  if not FWillInsert then
  begin
    Result := false;
    for i := 0 to FParentTables.Count -1 do
      if TInsertTable(FParentTables[i]).ClearIfParentWillInsert then
        if not Result then
        begin
          Result := True;
          Clear;
        end;
  end
  else
    Result := true;
end;  // TInsertTable.ClearIfParentWillInsert 

{-------------------------------------------------------------------------------
  Converts a spatial ref in a structured string to a structure. 
}
function TInsertTable.DecodeStructuredSpatialRef(const AStructuredRef: string):
    TDecodedSpatialRef;
var
  lStringList: TStringList;
begin
  with Result do begin
    SpatialRef := Null;
    SpatialRefSystem := Null;
    Lat := Null;
    Long := Null;
    SpatialRefQualifier := Null;
    if AStructuredRef <> '' then begin
      lStringList := TStringList.Create;
      lStringList.CommaText := AStructuredRef;
      if lStringList.Count = 5 then try
        SpatialRef := lStringList[0];
        SpatialRefSystem := lStringList[1];
        Lat := StrToFloat(lStringList[2]);
        Long := StrToFloat(lStringList[3]);
        SpatialRefQualifier := lStringList[4];
      finally
        lStringList.Free;
      end;
    end;
  end;
end;  // TInsertTable.DecodeStructuredSpatialRef

{-------------------------------------------------------------------------------
  Executes the SQLStatement. 
}
procedure TInsertTable.Execute(APrepared : Boolean = False);
var
  i: Integer;
begin
  FWillInsert := False; //Ensure that this won't be called again
  try //Execute all the parent tables
    for i := 0 to FParentTables.Count -1 do
      if TInsertTable(FParentTables[i]).WillInsert then
        TInsertTable(FParentTables[i]).Execute;
  except
    on Exception do
    begin
      FWillInsert := True;
      raise;
    end;
  end;
end;  // TInsertTable.Execute 

{-------------------------------------------------------------------------------
}
procedure TInsertTable.SetFields(FieldName: TFieldName; AIndex: Integer; const Value: string);
begin
  if not FWillInsert and (Value<> GetFields(FieldName, AIndex)) then
    Clear; //clear all fields
end;  // TInsertTable.SetFields 

{-==============================================================================
    TSurveyEventTable
===============================================================================}
{-------------------------------------------------------------------------------
}
function TSurveyEventTable.CheckForExisting: TKeyString;
var
  lRef: TDecodedSpatialRef;
  i: Integer;
begin
  dmGeneral.ExecuteSQL(SQL_CREATE_TEMP_FIELD_COLLECTORS);

  try
    // Populate the temp table.
    for i := 0 to FWrapper.FSurveyEventRecorderTable.FNameKeys.Count - 1 do
      dmGeneral.ExecuteSQL(Format(SQL_INSERT_TEMP_FIELD_COLLECTORS,
                                  [FWrapper.FSurveyEventRecorderTable.FNameKeys[i]]));

    lRef := DecodeStructuredSpatialRef(FWrapper.FSampleTable.FStructuredSpatialRef);

    FSurveyEventKey := VarToStr(
        dmGeneral.GetStoredProcOutputParam('usp_SurveyEventKey_Get',
            ['@Key', Null,
             '@SurveyKey', FSurveyKey,
             '@VagueDateStart', FWrapper.FSampleTable.FVagueDate.StartDate,
             '@VagueDateEnd', FWrapper.FSampleTable.FVagueDate.EndDate,
             '@VagueDateType', FWrapper.FSampleTable.FVagueDate.DateTypeString,
             '@SpatialRef', lRef.SpatialRef,
             '@SpatialRefSystem', lRef.SpatialRefSystem,
             '@Lat', lRef.Lat,
             '@Long', lRef.Long,
             '@SpatialRefQualifier', lRef.SpatialRefQualifier,
             '@LocationKey',FWrapper.FSampleTable.FLocationKey,
             '@LocationName', FWrapper.FSampleTable.FLocationName],
            '@Key'));
    Result := FSurveyEventKey;
  finally
    dmGeneral.ExecuteSQL(SQL_DROP_TEMP_FIELD_COLLECTORS);
  end;
end;  // TSurveyEventTable.CheckForExisting

{-------------------------------------------------------------------------------
  Ensure that other required tables exist and are linked. 
}
procedure TSurveyEventTable.CheckLinks;
begin
  with FWrapper do
    CreateTable(TInsertTable(FSampleTable), TSampleTable);
    // Sample table will add this as a parent automatically
end;  // TSurveyEventTable.CheckLinks 

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
procedure TSurveyEventTable.Execute(APrepared : Boolean = False);
var
  ltfOldNullStrictConvert: Boolean;
  lRef: TDecodedSpatialRef;
begin
  inherited;
    lRef := DecodeStructuredSpatialRef(FWrapper.FSampleTable.FStructuredSpatialRef);
    ltfOldNullStrictConvert := NullStrictConvert;
    NullStrictConvert := False;
    try
      if not APrepared then FSurveyEventKey := CheckForExisting;

      if ((FSurveyEventKey = '') and (not FWrapper.CreatedFromOccurrence)) then
      //insert a survey event provided we have not come from an existing occurrence
        FSurveyEventKey := VarToStr(
            dmGeneral.RunInsertStoredProc('Survey_Event', 'usp_SurveyEvent_Insert',
                ['@Key', Null,
                 '@SurveyKey', FSurveyKey,
                 '@EnteredBy', dmGeneral.Recorder.CurrentSettings.UserIDKey,
                 '@VagueDateStart', FWrapper.FSampleTable.FVagueDate.StartDate,
                 '@VagueDateEnd', FWrapper.FSampleTable.FVagueDate.EndDate,
                 '@VagueDateType', FWrapper.FSampleTable.FVagueDate.DateTypeString,
                 '@SpatialRef', lRef.SpatialRef,
                 '@SpatialRefSystem', lRef.SpatialRefSystem,
                 '@Lat', lRef.Lat,
                 '@Long', lRef.Long,
                 '@SpatialRefQualifier', lRef.SpatialRefQualifier,
                 '@LocationKey',FWrapper.FSampleTable.FLocationKey,
                 '@LocationName', FWrapper.FSampleTable.FLocationName,
                 '@Comment',''],
                '@Key'));
    finally
      NullStrictConvert := ltfOldNullStrictConvert;
    end;

end;  // TSurveyEventTable.Execute

{-------------------------------------------------------------------------------
}
function TSurveyEventTable.GetFields(FieldName: TFieldName; AIndex: Integer): string;
begin
  case FieldName of
    fnSurveyKey:
      Result := FSurveyKey;
    fnSurveyEventKey:
      Result := FSurveyEventKey;
  end;
end;  // TSurveyEventTable.GetFields 

{-------------------------------------------------------------------------------
}
procedure TSurveyEventTable.SetFields(FieldName: TFieldName; AIndex: Integer; const Value:
    string);
begin
  inherited SetFields(FieldName, AIndex, Value);

  case FieldName of
    fnSurveyKey:
      FSurveyKey := Value;
    fnSurveyEventKey:
      FSurveyEventKey := Value;
  end;
end;  // TSurveyEventTable.SetFields 

{-------------------------------------------------------------------------------
  Validate to ensure mandatory fields present. 
}
function TSurveyEventTable.Validate: TFieldNames;
begin
  Result := [];
  if FSurveyKey = '' then
    Result := Result + [fnSurveyKey];
end;  // TSurveyEventTable.Validate 

{-==============================================================================
    TSpecimenUnitTable
===============================================================================}
{-------------------------------------------------------------------------------
  Ensure that other required tables exist and are linked. 
}
procedure TSpecimenUnitTable.CheckLinks;
var
  i: integer;
begin
  with FWrapper do begin
    if FDeterminationTables.Count = 0 then
      FDeterminationTables.AddObject('', TDeterminationTable.Create(FWrapper));
    for i := 0 to Pred(FDeterminationTables.Count) do begin
      TDeterminationTable(FDeterminationTables.Objects[i]).CheckLinks;
      TDeterminationTable(FDeterminationTables.Objects[i]).IsForSpecimen := True;
    end;
  end;
end;  // TSpecimenUnitTable.CheckLinks 

{-------------------------------------------------------------------------------
}
procedure TSpecimenUnitTable.Clear;
begin
  inherited;
  FCollectionUnitKey := '';
  FDangerous := '';
  FLifeSciences := '';
  FParentCollectionCollectionUnitKey := '';
  FSpecimenTypeConceptKey := '';
  FCurrentContainerCollectionUnitKey  := '';
  FCurrentLocationCode := '';
  FUsualContainerCollectionUnitKey := '';
  FUsualLocationCode := '';
end;  // TSpecimenUnitTable.Clear 

{-------------------------------------------------------------------------------
}
procedure TSpecimenUnitTable.Execute(APrepared : Boolean = False);
var
  ltfOldNullStrictConvert: Boolean;
  internalUse: Variant;
begin
  inherited;
  FLifeSciences := Sysutils.BoolToStr(FWrapper.IsLifeSciences);
  ltfOldNullStrictConvert := NullStrictConvert;
  NullStrictConvert := False;
  internalUse := StrToVarBool(FInternalUse);
  if internalUse = Null then
    internalUse := False;
  try
    FCollectionUnitKey := VarToStr(
        dmGeneral.RunInsertStoredProc('Collection_Unit', 'usp_Specimen_Insert',
            ['@Key', Null,
             '@ParentCollectionCollectionUnitKey', FParentCollectionCollectionUnitKey,
             '@SpecimenTypeConceptKey', FSpecimenTypeConceptKey,
             '@Confidential', FWrapper.FConfidential,
             '@Dangerous', StrToVarBool(FDangerous),
             '@LifeSciences', StrToVarBool(FLifeSciences),
             '@Checked', '0',
             '@CurrentContainerCollectionUnitKey', FCurrentContainerCollectionUnitKey,
             '@CurrentLocationCode', FCurrentLocationCode,
             '@UsualContainerCollectionUnitKey', FUsualContainerCollectionUnitKey,
             '@UsualLocationCode' , FUsualLocationCode,
             '@InternalUse', internalUse],
            '@Key'));
  finally
    NullStrictConvert := ltfOldNullStrictConvert;
  end;
end;  // TSpecimenUnitTable.Execute

{-------------------------------------------------------------------------------
}
function TSpecimenUnitTable.GetFields(FieldName: TFieldName; AIndex: Integer): string;
begin
  case FieldName of
    fnCollectionUnitKey:
      Result := FCollectionUnitKey;
    fnParentCollectionCollectionUnitKey:
      Result := FParentCollectionCollectionUnitKey;
    fnSpecimenTypeConceptKey:
      Result := FSpecimenTypeConceptKey;
    fnConfidential:
      Result := Sysutils.BoolToStr(FWrapper.FConfidential);
    fnDangerous:
      Result := FDangerous;
    fnLifeSciences:
      Result := FLifeSciences;
    fnCurrentContainerCollectionUnitKey:
      Result := FCurrentContainerCollectionUnitKey;
    fnCurrentLocationCode:
      Result := FCurrentLocationCode;
    fnUsualContainerCollectionUnitKey:
      Result := FUsualContainerCollectionUnitKey;
    fnUsualLocationCode:
      Result := FUsualLocationCode;
  end;
end;  // TSpecimenUnitTable.GetFields 

{-------------------------------------------------------------------------------
}
procedure TSpecimenUnitTable.SetFields(FieldName: TFieldName; AIndex: Integer; const Value:
    string);
begin
  inherited SetFields(FieldName, AIndex, Value);
  
  if FieldName = fnCollectionUnitKey then
    FCollectionUnitKey := Value
  else if FieldName = fnParentCollectionCollectionUnitKey then
    FParentCollectionCollectionUnitKey := Value
  else if FieldName = fnSpecimenTypeConceptKey then
    FSpecimenTypeConceptKey := Value
  else if FieldName = fnConfidential then
    FWrapper.FConfidential := StrToVarBool(Value)
  else if FieldName = fnDangerous then
    FDangerous := Value
  else if FieldName = fnLifeSciences then
    FLifeSciences := Value
  else if FieldName = fnCurrentContainerCollectionUnitKey then
    FCurrentContainerCollectionUnitKey := Value
  else if FieldName = fnCurrentLocationCode then
    FCurrentLocationCode := value
  else if FieldName = fnUsualContainerCollectionUnitKey then
    FUsualContainerCollectionUnitKey := Value
  else if FieldName = fnUsualLocationCode then
    FUsualLocationCode := Value
  else if FieldName = fnInternalUse then
    FInternalUse := Value;
end;  // TSpecimenUnitTable.SetFields

{-------------------------------------------------------------------------------
  Validate to ensure mandatory fields present. 
}
function TSpecimenUnitTable.Validate: TFieldNames;
begin
  Result := [];
  if FParentCollectionCollectionUnitKey = '' then
    Result := Result + [fnParentCollectionCollectionUnitKey];
  if FSpecimenTypeConceptKey = '' then
    Result := Result + [fnSpecimenTypeConceptKey];
  if FUsualContainerCollectionUnitKey = '' then
    Result := Result + [fnUsualContainerCollectionUnitKey];
  {if FUsualLocationCode = '' then
    Result := Result + [fnUsualLocationCode];}
end;  // TSpecimenUnitTable.Validate 

{-==============================================================================
    TSpecimenLabelTable
===============================================================================}
{-------------------------------------------------------------------------------
  Ensure that other required tables exist and are linked. 
}
procedure TSpecimenLabelTable.CheckLinks;
begin
  with FWrapper do begin
    CreateTable(TInsertTable(FSpecimenUnitTable), TSpecimenUnitTable);
    AddParentTable(FSpecimenUnitTable);
  end;
end;  // TSpecimenLabelTable.CheckLinks 

{-------------------------------------------------------------------------------
}
procedure TSpecimenLabelTable.Clear;
begin
  inherited;
  FInscription := '';
  FLabel := '';
  FSpecimenLabelKey := '';
end;  // TSpecimenLabelTable.Clear 

{-------------------------------------------------------------------------------
}
procedure TSpecimenLabelTable.Execute(APrepared : Boolean = False);
var
  ltfOldNullStrictConvert: Boolean;
begin
  inherited;
  ltfOldNullStrictConvert := NullStrictConvert;
  NullStrictConvert := False;
  try
    if FInscription <> '' then
      FSpecimenLabelKey := VarToStr(
          dmGeneral.RunInsertStoredProc('Specimen_Label', 'usp_SpecimenLabel_Insert',
              ['@Key', Null,
               '@CollectionUnitKey', FWrapper.FSpecimenUnitTable.FCollectionUnitKey,
               '@IsInscription', True,
               '@Inscription', FInscription,
               '@Translated', Null,
               '@LanguageConceptKey', Null,
               '@InferredAuthor', Null,
               '@Comments', Null,
               '@ConfidenceConceptKey', Null,
               '@IsCurrent', 0],
              '@Key'));
    if FLabel<> '' then
      FSpecimenLabelKey := VarToStr(
          dmGeneral.RunInsertStoredProc('Specimen_Label', 'usp_SpecimenLabel_Insert',
              ['@Key', Null,
               '@CollectionUnitKey', FWrapper.FSpecimenUnitTable.FCollectionUnitKey,
               '@IsInscription', False,
               '@Inscription', FLabel,
               '@Translated', Null,
               '@LanguageConceptKey', Null,
               '@InferredAuthor', Null,
               '@Comments', Null,
               '@ConfidenceConceptKey', Null,
               '@IsCurrent', 0],
              '@Key'));
  finally
    NullStrictConvert := ltfOldNullStrictConvert;
  end;
end;  // TSpecimenLabelTable.Execute 

{-------------------------------------------------------------------------------
}
function TSpecimenLabelTable.GetFields(FieldName: TFieldName; AIndex: Integer): string;
begin
  case FieldName of
    fnInscription:
      Result := FInscription;
    fnLabel:
      Result := FLabel;
  end;
end;  // TSpecimenLabelTable.GetFields 

{-------------------------------------------------------------------------------
}
procedure TSpecimenLabelTable.SetFields(FieldName: TFieldName; AIndex: Integer; const Value:
    string);
begin
  inherited SetFields(FieldName, AIndex, Value);
  
  case FieldName of
    fnInscription:
      FInscription := Value;
    fnLabel:
      FLabel := Value;
  end;
end;  // TSpecimenLabelTable.SetFields 

{-------------------------------------------------------------------------------
  Validate to ensure mandatory fields present.
}
function TSpecimenLabelTable.Validate: TFieldNames;
begin
  Result := [];
  if (FInscription ='') and (FLabel ='') then
    Result := Result + [fnInscription, fnLabel];
end;  // TSpecimenLabelTable.Validate 

{-==============================================================================
    TSampleTable
===============================================================================}
{-------------------------------------------------------------------------------
}
function TSampleTable.CheckForExisting: TKeyString;
var
  lRef: TDecodedSpatialRef;
  i: Integer;
begin
  dmGeneral.ExecuteSQL(SQL_CREATE_TEMP_FIELD_COLLECTORS);

  try
    // Populate the temp table.
    for i := 0 to FWrapper.FSurveyEventRecorderTable.FNameKeys.Count - 1 do
      dmGeneral.ExecuteSQL(Format(SQL_INSERT_TEMP_FIELD_COLLECTORS,
                                  [FWrapper.FSurveyEventRecorderTable.FNameKeys[i]]));

    lRef := DecodeStructuredSpatialRef(FStructuredSpatialRef);

    //first try to find an existing sample
    FSampleKey := VarToStr(dmGeneral.GetStoredProcOutputParam('usp_SampleKey_Get',
          ['@Key', Null,
           '@VagueDateStart', Trunc(FVagueDate.StartDate),
           '@VagueDateEnd', Trunc(FVagueDate.EndDate),
           '@VagueDateType', FVagueDate.DateTypeString,
           '@SpatialRef', lRef.SpatialRef,
           '@SpatialRefSystem', lRef.SpatialRefSystem,
           '@Lat', lRef.Lat,
           '@Long', lRef.Long,
           '@SpatialRefQualifier', lRef.SpatialRefQualifier,
           '@SampleTypeKey', FSampleTypeKey,
           '@LocationName', FLocationName,
           '@LocationKey', FLocationKey,
           '@SurveyKey', FWrapper.FSurveyEventTable.FSurveyKey],
          '@Key'));
    Result := FSampleKey;
  finally
    dmGeneral.ExecuteSQL(SQL_DROP_TEMP_FIELD_COLLECTORS);
  end;
end;  // TSampleTable.CheckForExisting

{-------------------------------------------------------------------------------
  Ensure that other required tables exist and are linked. 
}
procedure TSampleTable.CheckLinks;
begin
  with FWrapper do begin
    CreateTable(TInsertTable(FSurveyEventTable), TSurveyEventTable);
    AddParentTable(FSurveyEventTable);
    if FWrapper.RequiresRecorders then begin
      CreateTable(TInsertTable(FSurveyEventRecorderTable),
          TSurveyEventRecorderTable);
      FSurveyEventRecorderTable.CheckLinks;
    end;
    // Only create occurrences if the wrapper is set up for this.  Otherwise,
    // only sample, survey event stuff is done.
    if FWrapper.WantOccurrences then
      CreateTable(TInsertTable(FOccurrenceTable), TOccurrenceTable);
    // FOccurrenceTable will automatically set up its parents
  end;
end;  // TSampleTable.CheckLinks 

{-------------------------------------------------------------------------------
}
procedure TSampleTable.Clear;
begin
  inherited;
  FLocationKey := '';
  FLocationName := '';
  FSampleKey := '';
  FSampleTypeKey := '';
  FStructuredSpatialRef := '';
  FVagueDate.DateTypeString := '';
end;  // TSampleTable.Clear 

{-------------------------------------------------------------------------------
}
procedure TSampleTable.Execute(APrepared : Boolean = False);
var
  ltfOldNullStrictConvert: Boolean;
  lRef: TDecodedSpatialRef;
begin
  inherited;
  lRef := DecodeStructuredSpatialRef(FStructuredSpatialRef);

  ltfOldNullStrictConvert := NullStrictConvert;
  NullStrictConvert := False;
  try
    if not APrepared then FSampleKey := CheckForExisting;
  
    if ((FSampleKey = '') and (not FWrapper.CreatedFromOccurrence)) then
    //insert a sample provided we have not come from an existing occurrence
      FSampleKey := VarToStr(
        dmGeneral.RunInsertStoredProc('Sample', 'usp_Sample_Insert',
            ['@Key', Null,
             '@VagueDateStart', Trunc(FVagueDate.StartDate),
             '@VagueDateEnd', Trunc(FVagueDate.EndDate),
             '@VagueDateType', FVagueDate.DateTypeString,
             '@SpatialRef', lRef.SpatialRef,
             '@SpatialRefSystem', lRef.SpatialRefSystem,
             '@Lat', lRef.Lat,
             '@Long', lRef.Long,
             '@SpatialRefQualifier', lRef.SpatialRefQualifier,
             '@SampleTypeKey', FSampleTypeKey,
             '@EnteredBy', dmGeneral.Recorder.CurrentSettings.UserIDKey,
             '@SurveyEventKey', FWrapper.FSurveyEventTable.FSurveyEventKey,
             '@LocationName', StrToVarStr(FLocationName),
             '@LocationKey', StrToVarStr(FLocationKey),
             '@Comment', ''],
            '@Key'));
  finally
    NullStrictConvert := ltfOldNullStrictConvert;
  end;
end;  // TSampleTable.Execute

{-------------------------------------------------------------------------------
}
function TSampleTable.GetFields(FieldName: TFieldName; AIndex: Integer): string;
begin
  case FieldName of
    fnSampleKey:
      Result := FSampleKey;
    fnSampleTypeKey:
      Result := FSampleTypeKey;
    fnSpatialRef:
      Result := FStructuredSpatialRef;
    fnVagueDate:
      Result := VagueDateToString(FVagueDate);
    fnLocationKey:
      Result := FLocationKey;
    fnLocationName:
      Result := FLocationName;
    fnSurveyKey:
      Result := FSurveyKey;
  end;
end;  // TSampleTable.GetFields 

{-------------------------------------------------------------------------------
}
procedure TSampleTable.SetFields(FieldName: TFieldName; AIndex: Integer; const Value: string);
begin
  inherited SetFields(FieldName, AIndex, Value);
  
  case FieldName of
    fnSampleKey:
      FSampleKey := Value;
    fnSampleTypeKey:
      FSampleTypeKey := Value;
    fnSpatialRef:
      FStructuredSpatialRef := Value;
    fnVagueDate:
      FVagueDate := StringToVagueDate(Value);
    fnLocationName:
      FLocationName := Value;
    fnLocationKey:
      FLocationKey := Value;
    fnSurveyKey:
      FSurveyKey := Value;
  end;
end;  // TSampleTable.SetFields 

{-------------------------------------------------------------------------------
  Validate to ensure mandatory fields present and date is not in future. 
}
function TSampleTable.Validate: TFieldNames;
var
  lCurrentDate: TVagueDate;
begin
  Result := [];
  if (FLocationName= '') and (FStructuredSpatialRef = '') then
    Result := Result + [fnLocationName, fnSpatialRef];
  if FSampleTypeKey = '' then
    Result := Result + [fnSampleTypeKey];
  if FVagueDate.DateTypeString = '' then
    Result := Result + [fnVagueDate];
  // Check date is not in future
  lCurrentDate := StringToVagueDate(DateToStr(Date));
  if CompareVagueDateToVagueDate(lCurrentDate, FVagueDate)=-1 then
    raise EQuickEntryException.Create(Format(
        ResStr_DateCannotBeInFuture, [ResStr_Sample]));
  ValidateSpatialRef;
end;  // TSampleTable.Validate

{-------------------------------------------------------------------------------
  Checks that the spatial reference is in the survey bounding box 
}
procedure TSampleTable.ValidateSpatialRef;
var
  lRef: TDecodedSpatialRef;
begin
  // Do we need to validate?
  if ((FStructuredSpatialRef <> '') or (FLocationKey <> '')) and
     (FWrapper.FSurveyEventTable.FSurveyKey <> 'Unknown') and
     (FWrapper.FSurveyEventTable.FSurveyKey <> '') then
  begin
    if FStructuredSpatialRef = '' then begin
      // Retrieve the location's spatial ref
      with dmGeneral.GetRecordset('usp_Location_Select', ['@Key', FLocationKey]) do begin
        lRef.SpatialRef := Fields['Spatial_Ref'].Value;
        lRef.SpatialRefSystem := Fields['Spatial_Ref_System'].Value;
        lRef.SpatialRefQualifier := Fields['Spatial_Ref_Qualifier'].Value;
        lRef.Lat := Fields['Lat'].Value;
        lRef.Long := Fields['Long'].Value;

        // No spatial ref specified by user, but one found from specified location
        with TStringList.Create do
          try
            Add(lRef.SpatialRef);
            Add(lRef.SpatialRefSystem);
            Add(FloatToStr(lRef.Lat));
            Add(FloatToStr(lRef.Long));
            Add(lRef.SpatialRefQualifier);
            FStructuredSpatialRef := CommaText;
          finally
            Free;
          end;
      end;
    end else
      // use the one the user typed in
      lRef := DecodeStructuredSpatialRef(FStructuredSpatialRef);

    with dmGeneral.GetRecordset('usp_Survey_Select',
                                ['@Key', FWrapper.FSurveyEventTable.FSurveyKey]) do
    begin
      if not VarIsNull(Fields['SW_Lat'].Value) then
        if (lRef.Lat > Fields['NE_Lat'].Value) or (lRef.Lat < Fields['SW_Lat'].Value) or
           (lRef.Long > Fields['NE_Long'].Value) or (lRef.Long < Fields['SW_Long'].Value) then
          raise EQuickEntryException.Create(ResStr_NotInBoundingBox);
    end;
  end;
end;  // TSampleTable.ValidateSpatialRef 

{-==============================================================================
    TOccurrenceTable
===============================================================================}
{-------------------------------------------------------------------------------
}
constructor TOccurrenceTable.Create(AInsertWrapper: TInsertWrapper);
begin
  inherited Create(AInsertWrapper);

  // Set default values for inferred collectors and determiners.
  FInferredCollectors := '0';
  FInferredDeterminers := '0';
end;

{-------------------------------------------------------------------------------
  Ensure that other required tables exist and are linked.
}
procedure TOccurrenceTable.CheckLinks;
var
  i: integer;
begin
  with FWrapper do begin
    CreateTable(TInsertTable(FSampleTable), TSampleTable);
    AddParentTable(FSampleTable);
    if FDeterminationTables.Count = 0 then
      FDeterminationTables.AddObject('', TDeterminationTable.Create(FWrapper));
    for i := 0 to Pred(FDeterminationTables.Count) do
      TDeterminationTable(FDeterminationTables.Objects[i]).CheckLinks;
  end;
end;  // TOccurrenceTable.CheckLinks 

{-------------------------------------------------------------------------------
}
procedure TOccurrenceTable.Clear;
begin
  inherited;
  FComment := '';
  FOccurrenceKey := '';
  FRecordTypeKey := '';
  FInferredCollectors := '';
  FInferredDeterminers := '';
end;  // TOccurrenceTable.Clear 

{-------------------------------------------------------------------------------
  Insert the fields into a new record in either the Occurrence or
      Taxon_Occurrence table.
}
procedure TOccurrenceTable.Execute(APrepared : Boolean = False);
var
  ltfOldNullStrictConvert: Boolean;
  lTaxonOccKey, lOccKey: String;
begin
  inherited;
  
  if not (FWrapper.CreatedFromOccurrence) then
  begin
  ltfOldNullStrictConvert := NullStrictConvert;
  NullStrictConvert := False;
  try
    //determine whether this is a taxon occurrence table or an occurrence table
    if not FWrapper.IsLifeSciences then //occurrence
      FOccurrenceKey := VarToStr(
        dmGeneral.RunInsertStoredProc('Occurrence', 'usp_Occurrence_Insert',
            ['@SampleKey',FWrapper.FSampleTable.FSampleKey,
             '@Confidential', FWrapper.FConfidential,
             '@Comment', FComment,
             '@SurveyorsRef', Null,
             '@Confidential', FWrapper.FConfidential,
             '@RecordTypeKey', IfThen(FRecordTypeKey='', Null, FRecordTypeKey),
             '@Checked', 1,
             '@CheckedBy', Null,
             '@CheckedDate', Null],
            '@Key'))
    else
      FOccurrenceKey := VarToStr(
          dmGeneral.RunInsertStoredProc('Taxon_Occurrence', 'usp_TaxonOccurrence_Insert',
            ['@SampleKey',FWrapper.FSampleTable.FSampleKey,
             '@Confidential', FWrapper.FConfidential,
             '@Comment', FComment,
             '@Checked', 1,
             '@EnteredBy', dmGeneral.Recorder.CurrentSettings.UserIDKey,
             '@RecordTypeKey', IfThen(FRecordTypeKey='', 'NBNSYS0000000026', FRecordTypeKey)],
            '@Key'));
    FHasInserted := true;
  finally
    NullStrictConvert := ltfOldNullStrictConvert;
  end;
  end
  else
  begin
    if FWrapper.TaxonOccurrenceKey = '' then
    begin
      FOccurrenceKey := FWrapper.OccurrenceKey;
    end
    else
      FOccurrenceKey := FWrapper.TaxonOccurrenceKey;
  end;

  // Need to add join in Specimen_Field_Data if inserting in Specimen_Unit AND Tx/Occurrence
  if Assigned(FWrapper.FSpecimenUnitTable) then begin
    lTaxonOccKey := '';
    lOccKey := '';
    if FWrapper.IsLifeSciences then
      lTaxonOccKey := FWrapper.FOccurrenceTable.FOccurrenceKey
    else
      lOccKey := FWrapper.FOccurrenceTable.FOccurrenceKey;

    dmGeneral.RunInsertStoredProc('Specimen_Field_Data', 'usp_SpecimenFieldData_Insert',
        ['@CollectionUnitKey', FWrapper.FSpecimenUnitTable.FCollectionUnitKey,
         '@OccurrenceKey', lOccKey,
         '@TaxonOccurrenceKey', lTaxonOccKey,
         '@InferredSurvey', 0,
         '@InferredLocation', 0,
         '@InferredSpatialRef', 0,
         '@InferredSampleType', 0,
         '@InferredDate', 0,
         '@InferredCollectors', FInferredCollectors,
         '@InferredDeterminers', FInferredDeterminers,
         '@GatheringEvent', 1],
         '@Key');
  end;
end;  // TOccurrenceTable.Execute

{-------------------------------------------------------------------------------
  Accessor for Occurrence Key, Comment and Confidential fields. 
}
function TOccurrenceTable.GetFields(FieldName: TFieldName; AIndex: Integer): string;
begin
  case FieldName of
    fnOccurrenceKey:
      Result := FOccurrenceKey;
    fnComment:
      Result := FComment;
    fnConfidential:
      Result := Sysutils.BoolToStr(FWrapper.FConfidential);
    fnRecordTypeKey:
      Result := FRecordTypeKey;
    fnInferredCollectors:
      Result := FInferredCollectors;
    fnInferredDeterminers:
      Result := FInferredDeterminers;
  end;
end;  // TOccurrenceTable.GetFields

{-------------------------------------------------------------------------------
}
procedure TOccurrenceTable.SetFields(FieldName: TFieldName; AIndex: Integer; const Value:
    string);
begin
  inherited SetFields(FieldName, AIndex, Value);
  
  case FieldName of
    fnOccurrenceKey:
      FOccurrenceKey := Value;
    fnComment:
      FComment := Value;
    fnConfidential:
      FWrapper.FConfidential := StrToVarBool(Value);
    fnRecordTypeKey:
      FRecordTypeKey := Value;
    fnInferredCollectors:
      FInferredCollectors := Value;
    fnInferredDeterminers:
      FInferredDeterminers := Value;
  end;
end;  // TOccurrenceTable.SetFields 

{-------------------------------------------------------------------------------
  Validate to ensure mandatory fields present. 
}
function TOccurrenceTable.Validate: TFieldNames;
begin
  Result := [];
  FHasInserted := False;
end;  // TOccurrenceTable.Validate 

{-==============================================================================
    TOccurrenceDataTable
===============================================================================}
{-------------------------------------------------------------------------------
  Ensure that other required tables exist and are linked. 
}
procedure TOccurrenceDataTable.CheckLinks;
begin
  with FWrapper do begin
    CreateTable(TInsertTable(FOccurrenceTable), TOccurrenceTable);
    AddParentTable(FOccurrenceTable);
  end;
end;  // TOccurrenceDataTable.CheckLinks 

{-------------------------------------------------------------------------------
}
procedure TOccurrenceDataTable.Execute(APrepared: Boolean = False);
begin
  inherited;
    //determine whether this is a taxon occurrence table or an occurrence table
    if (not FWrapper.IsLifeSciences) or
       not FIsTaxonData then //occurrence
      dmGeneral.RunInsertStoredProc('Occurrence_Data', 'usp_OccurrenceData_Insert',
          ['@OccurrenceKey', FWrapper.FOccurrenceTable.FOccurrenceKey,
           '@AppliesTo', FAppliesTo,
           '@MethodConceptKey', FMethodConceptKey,
           '@Duration', FDuration,
           '@Accuracy', FAccuracy,
           '@ParameterConceptKey', FParameterConceptKey,
           '@UnitConceptKey', FUnitConceptKey,
           '@Value', FValue,
           '@IsDescriptor', 0,
           '@Key', Null,
           '@Checked', 1],
          '@Key')
    else
      dmGeneral.RunInsertStoredProc('Taxon_Occurrence_Data', 'usp_TaxonOccurrenceData_Insert',
          ['@TaxonOccurrenceKey', FWrapper.FOccurrenceTable.FOccurrenceKey,
           '@QualifierKey', FTaxonQualifierKey,
           '@Accuracy', Copy(FAccuracy, 1, 10),
           '@UnitKey', FTaxonUnitKey,
           '@Data', Copy(FValue, 1, 20),
           '@EnteredBy', dmGeneral.Recorder.CurrentSettings.UserIDKey],
          '@Key');
end;  // TOccurrenceDataTable.Execute

{-==============================================================================
    TMovementOfMaterialTable
===============================================================================}
{-------------------------------------------------------------------------------
  Ensure that other required tables exist and are linked. 
}
procedure TMovementOfMaterialTable.CheckLinks;
begin
  with FWrapper do begin
    CreateTable(TInsertTable(FSpecimenUnitTable), TSpecimenUnitTable);
    AddParentTable(FSpecimenUnitTable);
    CreateTable(TInsertTable(FMovementTable), TMovementTable);
    AddParentTable(FMovementTable);    
  end;
end;  // TMovementOfMaterialTable.CheckLinks 

{-------------------------------------------------------------------------------
}
procedure TMovementOfMaterialTable.Clear;
begin
  inherited;
  FReceiverOrganisationNameKey:= '';
end;  // TMovementOfMaterialTable.Clear

{-------------------------------------------------------------------------------
}
procedure TMovementOfMaterialTable.Execute(APrepared : Boolean = False);
begin
  inherited;
  (*dmGeneral.RunInsertStoredProc('Movement_Of_Material',
      'usp_MovementOfMaterial_Insert',
      ['@Key', Null,
       '@MovementDirectionKey', FWrapper.FMovementTable.FMovementDirectionKey,
       '@VagueDateStart', Null,
       '@VagueDateEnd', Null,
       '@VagueDateType', 'U',
       '@ReceiverOrganisationDepartmentKey', FReceiverOrganisationNameKey],
      '@Key');*)
end;  // TMovementOfMaterialTable.Execute

{-------------------------------------------------------------------------------
}
function TMovementOfMaterialTable.GetFields(FieldName: TFieldName; AIndex: Integer): string;
begin
  case FieldName of
    fnReceiverOrganisationNameKey:
      Result := FReceiverOrganisationNameKey;
  end;
end;  // TMovementOfMaterialTable.GetFields

{-------------------------------------------------------------------------------
}
procedure TMovementOfMaterialTable.SetFields(FieldName: TFieldName; AIndex: Integer; const
    Value: string);
begin
  inherited SetFields(FieldName, AIndex, Value);

  case FieldName of
    fnReceiverOrganisationNameKey:
      FReceiverOrganisationNameKey := Value;
  end;
end;  // TMovementOfMaterialTable.SetFields

{-------------------------------------------------------------------------------
  Validate to ensure mandatory fields present. 
}
function TMovementOfMaterialTable.Validate: TFieldNames;
begin
  Result := [];
  if FReceiverOrganisationNameKey = '' then
    Result := Result + [fnReceiverOrganisationNameKey];
end;  // TMovementOfMaterialTable.Validate 

{-==============================================================================
    TDeterminationTable
===============================================================================}
{-------------------------------------------------------------------------------
  Object initialisation
}
constructor TDeterminationTable.Create(AWrapper: TInsertWrapper);
begin
  inherited Create(AWrapper);
  
  FIsForSpecimen := False;
  FDeterminerNameKeys := TStringList.Create;
end;  // TDeterminationTable.Create

{-------------------------------------------------------------------------------
  Destructor
}
destructor TDeterminationTable.Destroy;
begin
  FDeterminerNameKeys.Free;

  inherited Destroy;
end;

{-------------------------------------------------------------------------------
  Ensure that other required tables exist and are linked. 
}
procedure TDeterminationTable.CheckLinks;
begin
  // Setup the parents - this is flexible
  if Assigned(FWrapper.FOccurrenceTable) then
    AddParentTable(FWrapper.FOccurrenceTable);
  if Assigned(FWrapper.FSpecimenUnitTable) then
    AddParentTable(FWrapper.FSpecimenUnitTable);
end;  // TDeterminationTable.CheckLinks

{-------------------------------------------------------------------------------
}
procedure TDeterminationTable.Clear;
begin
  inherited;
  FDeterminationKey := '';
  FDeterminerNameKeys.Clear;
  FDeterminerRoleKey := '';
  FVagueDate.DateTypeString := '';
  FConceptKey := '';
  FTaxonListItemKey := '';
end;  // TDeterminationTable.Clear 

{-------------------------------------------------------------------------------
  Insert the data into a new record in either the Determination or
      Taxon_Determination tables.
}
procedure TDeterminationTable.Execute(APrepared : Boolean = False);
var
  ltfOldNullStrictConvert: Boolean;
  ltfPreferred: Boolean;
  ltfIncludeInLabel: Boolean;
  lOccurrenceKey: TKeyString;
  i, lDeterminationTableIndex, lCountNameKeys, lLastDeterminationIndex: Integer;

begin
  inherited;
  if Assigned(FWrapper.FOccurrenceTable) then
    ltfPreferred := FWrapper.FOccurrenceTable.HasInserted or
                    FWrapper.FOccurrenceTable.WillInsert
  else
    ltfPreferred := True;

  if FDeterminerRoleKey = '' then
    FDeterminerRoleKey := DEFAULT_DETERMINER_ROLE;
  if FVagueDate.DateTypeString = '' then
    FVagueDate.DateTypeString := 'U';
  lDeterminationTableIndex := FWrapper.FDeterminationTables.IndexOfObject(Self);
  // Only the first determination on a quick entry line can be preferred
  if lDeterminationTableIndex > 0 then
    ltfPreferred := False;
  if FDeterminerNameKeys.Count = 0 then
    ReadDefaultDeterminerName;
  lCountNameKeys := FDeterminerNameKeys.Count;
  // Extend FDeterminerNameKeys so that it has at least lDeterminationTableIndex items
  for i := lCountNameKeys to lDeterminationTableIndex do
    FDeterminerNameKeys.Add(FDeterminerNameKeys[Pred(lCountNameKeys)]);
  if lDeterminationTableIndex < Pred(FWrapper.FDeterminationTables.Count) then
    lLastDeterminationIndex := lDeterminationTableIndex
  else
    lLastDeterminationIndex := Pred(FDeterminerNameKeys.Count);

  ltfOldNullStrictConvert := NullStrictConvert;
  NullStrictConvert := False;
  try
    for i := lDeterminationTableIndex to lLastDeterminationIndex do begin
      If FConceptKey<>'' then
      begin
        // 'Include in label' is not available for entry here,
        // instead it is defaulted to the value of 'preferred' (unless the
        // domain is 'Stratigraphy').
        ltfIncludeInLabel :=
            ConceptHasDomain(FConceptKey, DOM_STRATIGRAPHY) or ltfPreferred;
        lOccurrenceKey := IfThen(
            FWrapper.CreatedFromOccurrence,
            '',
            Fields[fnOccurrenceKey, i]);
      //this is a Determination
        FDeterminationKey := VarToStr(
            dmGeneral.RunInsertStoredProc('Determination', 'usp_Determination_Insert',
              ['@Key', Null,
               '@DeterminedItemKey', FConceptKey,
               '@OccurrenceKey', lOccurrenceKey,
               '@SpecimenCollectionUnitKey', Fields[fnCollectionUnitKey, i],
               '@DeterminationTypeKey', DEFAULT_DETERMINATION_TYPE,
               '@NomenclaturalStatusConceptKey', Null,
               '@Confidence', 0,
               '@DeterminerNameKey', FDeterminerNameKeys[i],
               '@DeterminerRoleKey', FDeterminerRoleKey,
               '@InferredDeterminer', 0,
               '@VagueDateStart', Trunc(FVagueDate.StartDate),
               '@VagueDateEnd', Trunc(FVagueDate.EndDate),
               '@VagueDateType',FVagueDate.DateTypeString,
               '@UsedSpecimen', StrToVarBool(Fields[fnUsedSpecimen, i]),
               '@Preferred', ltfPreferred,
               '@Method', Null,
               '@Notes', Null,
               '@IsForSpecimen', IsForSpecimen,
               '@IncludeInLabel', ltfIncludeInLabel],
               '@Key'))
      end
      else
      begin //assume Taxon_Determination
        if Assigned(FWrapper.FOccurrenceTable)
            and not FWrapper.CreatedFromOccurrence
        then
          lOccurrenceKey := FWrapper.FOccurrenceTable.FOccurrenceKey
        else
        begin
          lOccurrenceKey := '';
        end;
        FDeterminationKey := VarToStr(dmGeneral.RunInsertStoredProc(
            'Taxon_Determination',
            'usp_TaxonDetermination_Insert',
            ['@Key', Null,
             '@DeterminedItemKey', FTaxonListItemKey,
             '@OccurrenceKey', lOccurrenceKey,
             '@SpecimenCollectionUnitKey', Fields[fnCollectionUnitKey, i],
             '@DeterminationTypeKey', DEFAULT_DETERMINATION_TYPE,
             '@NomenclaturalStatusConceptKey', Null,
             '@Confidence', 0,
             '@DeterminerNameKey', FDeterminerNameKeys[i],
             '@DeterminerRoleKey', FDeterminerRoleKey,
             '@InferredDeterminer', 0,
             '@VagueDateStart', Trunc(FVagueDate.StartDate),
             '@VagueDateEnd', Trunc(FVagueDate.EndDate),
             '@VagueDateType',FVagueDate.DateTypeString,
             '@UsedSpecimen', StrToVarBool(Fields[fnUsedSpecimen, i]),
             '@Preferred', ltfPreferred,
             '@Method', Null,
             '@Notes', Null,
             '@IsForSpecimen', IsForSpecimen,
              // 'Include in label' is not available for entry here,
              // instead it is defaulted to the value of 'preferred'.
             '@IncludeInLabel', ltfPreferred],
             '@Key'));
      end;
      // Only the first determination can go in as preferred
      ltfPreferred := False;

    end;
  finally
    NullStrictConvert := ltfOldNullStrictConvert;
  end;
end;  // TDeterminationTable.Execute

{-------------------------------------------------------------------------------
}
function TDeterminationTable.GetFields(FieldName: TFieldName; AIndex: Integer): string;
begin
  case FieldName of
    fnCollectionUnitKey:
      if Assigned(FWrapper.FSpecimenUnitTable) then
        Result := FWrapper.FSpecimenUnitTable.FCollectionUnitKey
      else
        Result := Null;
    fnDeterminationKey:
      Result := FDeterminationKey;
    fnConceptKey:
      Result := FConceptKey;
    fnTaxonListItemKey:
      Result := FTaxonListItemKey;
    fnOccurrenceKey:
      if Assigned(FWrapper.FOccurrenceTable) then
        Result := FWrapper.FOccurrenceTable.FOccurrenceKey
      else
        Result := Null;
    fnDeterminerNameKey:
      if AIndex < FDeterminerNameKeys.Count then
        Result := FDeterminerNameKeys[AIndex]
      else
        Result := '';
    fnDeterminerRoleKey:
      Result := FDeterminerRoleKey;
    fnVagueDate:
      Result := VagueDateToString(FVagueDate);
    fnUsedSpecimen:
      Result := Sysutils.BoolToStr(Assigned(FWrapper.FSpecimenUnitTable));
  end;
end;  // TDeterminationTable.GetFields

{-------------------------------------------------------------------------------
  Get the default determiner - first field recorder,
  or if there are no field recorders, the current user
}
procedure TDeterminationTable.ReadDefaultDeterminerName;
begin
  if Assigned(FWrapper.FSurveyEventRecorderTable)
      and Assigned(FWrapper.FSurveyEventRecorderTable.FNameKeys)
      and (FWrapper.FSurveyEventRecorderTable.FNameKeys.Count > 0) then
    FDeterminerNameKeys.Add(FWrapper.FSurveyEventRecorderTable.FNameKeys[0])
  else
    FDeterminerNameKeys.Add(AppSettings.UserID);
end;

{-------------------------------------------------------------------------------
}
procedure TDeterminationTable.SetFields(FieldName: TFieldName; AIndex: Integer; const Value:
    string);
var
  i: Integer;
begin
  inherited SetFields(FieldName, AIndex, Value);

  case FieldName of
    fnCollectionUnitKey:
      begin
        FWrapper.CreateTable(TInsertTable(FWrapper.FSpecimenUnitTable), TSpecimenUnitTable);
        AddParentTable(FWrapper.FSpecimenUnitTable);
        FWrapper.FSpecimenUnitTable.FCollectionUnitKey := Value;
      end;
    fnDeterminationKey:
      FDeterminationKey := Value;
    fnConceptKey:
      FConceptKey := Value;
    fnOccurrenceKey:
      begin
        FWrapper.CreateTable(TInsertTable(FWrapper.FOccurrenceTable), TOccurrenceTable);
        AddParentTable(FWrapper.FOccurrenceTable);
        FWrapper.FOccurrenceTable.FOccurrenceKey := Value;
      end;
    fnDeterminerNameKey:
      begin
        if AIndex < FDeterminerNameKeys.Count then
          FDeterminerNameKeys[AIndex] := Value
        else begin
          for i := FDeterminerNameKeys.Count to AIndex - 1 do
            FDeterminerNameKeys.Add('');
          FDeterminerNameKeys.Add(Value);
        end;
      end;
    fnDeterminerRoleKey:
      FDeterminerRoleKey := Value;
    fnVagueDate:
      FVagueDate := StringToVagueDate(Value);
    fnTaxonListItemKey:
      FTaxonListItemKey := Value;
  end;
end;  // TDeterminationTable.SetFields

{-------------------------------------------------------------------------------
}
procedure TDeterminationTable.SetIsForSpecimen(Value: Boolean);
begin
  FIsForSpecimen := Value;
end;  // TDeterminationTable.SetIsForSpecimen 

{-------------------------------------------------------------------------------
  Validate to ensure mandatory fields present and date is not in future. 
}
function TDeterminationTable.Validate: TFieldNames;
var
  lCurrentDate: TVagueDate;
begin
  Result := [];
  if (FTaxonListItemKey = '') and (FConceptKey = '') then
    Result := Result + [fnTaxonListItemKey, fnConceptKey]
  else
  if FVagueDate.DateTypeString <> '' then begin
    // Check date is not in future
    lCurrentDate := StringToVagueDate(DateToStr(Date));
    if CompareVagueDateToVagueDate(lCurrentDate, FVagueDate) = -1 then
      raise EQuickEntryException.CreateNonCritical(Format(
        ResStr_DateCannotBeInFuture, [ResStr_Determination]));

    // check the beginning determination vague date is = or > the beginning gathering vague date value
    if Assigned(FWrapper.FSampleTable) then
    begin
      if (FVagueDate.DateTypeString[1] <> 'U') and
         (FWrapper.FSampleTable.FVagueDate.DateTypeString[1] <> 'U') then
        if (FVagueDate.StartDate < FWrapper.FSampleTable.FVagueDate.StartDate) then
          raise EQuickEntryException.CreateNonCritical(ResStr_DeterminationDateMustBeAfterSampleDate);
    end;
  end;
end;  // TDeterminationTable.Validate

{-==============================================================================
    TMovementOfOwnershipTable
===============================================================================}
{-------------------------------------------------------------------------------
  Ensure that other required tables exist and are linked.
}
procedure TMovementOfOwnershipTable.CheckLinks;
begin
  with FWrapper do begin
    CreateTable(TInsertTable(FSpecimenUnitTable), TSpecimenUnitTable);
    AddParentTable(FSpecimenUnitTable);
    CreateTable(TInsertTable(FMovementTable), TMovementTable);
    AddParentTable(FMovementTable);
  end;
end;  // TMovementOfOwnershipTable.CheckLinks 

{-------------------------------------------------------------------------------
}
procedure TMovementOfOwnershipTable.Clear;
begin
  inherited;
end;  // TMovementOfOwnershipTable.Clear

{-------------------------------------------------------------------------------
}
procedure TMovementOfOwnershipTable.Execute(APrepared : Boolean = False);
begin
  // Ensure that the movement is inserted.
  if not FWrapper.FtfMovementOfMaterialWillInsert then
    FWrapper.FMovementTable.FWillInsert := True;
  inherited;
  // nothing to insert as record is created automatically when movement inserted
end;  // TMovementOfOwnershipTable.Execute

{-------------------------------------------------------------------------------
}
function TMovementOfOwnershipTable.GetFields(FieldName: TFieldName; AIndex: Integer): string;
begin

end;  // TMovementOfOwnershipTable.GetFields

{-------------------------------------------------------------------------------
}
procedure TMovementOfOwnershipTable.SetFields(FieldName: TFieldName; AIndex: Integer; const
    Value: string);
begin
  inherited SetFields(FieldName, AIndex, Value);
end;  // TMovementOfOwnershipTable.SetFields

{-------------------------------------------------------------------------------
  Validate to ensure mandatory fields present.
}
function TMovementOfOwnershipTable.Validate: TFieldNames;
begin
  Result := [];
end;  // TMovementOfOwnershipTable.Validate

{ TMovementTable }

{-------------------------------------------------------------------------------
}
procedure TMovementTable.CheckLinks;
begin
  inherited;
end;

{-------------------------------------------------------------------------------
}
procedure TMovementTable.Clear;
begin
  inherited;
  FMovementKey := '';
  FMovementNumber := '';
  FMovementDirectionKey := '';
  FMovementCollectionUnitKey := '';
end;

{-------------------------------------------------------------------------------
}
constructor TMovementTable.Create(AWrapper: TInsertWrapper);
begin
  inherited Create(AWrapper);
  // default staff responsible = 'unknown'
  FStaffResponsibleNameKey := 'NBNSYS0000000004';
end;

{-------------------------------------------------------------------------------
}
procedure TMovementTable.Execute(APrepared: Boolean);
var
  ltfOldNullStrictConvert: Boolean;
  lDepartmentKey: OleVariant;
begin
  inherited;
  ltfOldNullStrictConvert := NullStrictConvert;
  NullStrictConvert := False;
  try
    GetMovementDirectionKey;
    if FMovementDirectionKey='' then begin
      // Create a movement
      if Assigned(FWrapper.FMovementOfMaterialTable) then
        lDepartmentKey := FWrapper.FMovementOfMaterialTable.FReceiverOrganisationNameKey
      else
        lDepartmentKey := null;
      FMovementKey := VarToStr(
          dmGeneral.RunInsertStoredProc('Movement', 'usp_Movement_Insert',
              ['@Key', Null,
               '@MovementType', 0,
               '@WithAcquisition', Assigned(Wrapper.FMovementOfMaterialTable),
               '@OtherPartyNameKey', 'NBNSYS0000000003', // unknown
               '@StaffResponsibleNameKey', FStaffResponsibleNameKey, // unknown
               '@DepartmentKey', lDepartmentKey,
               '@ContactNameKey', Null,
               '@VagueDateStart', Null,
               '@VagueDateEnd', Null,
               '@VagueDateType', 'U',
               '@Number', FMovementNumber,
               '@Notes', Null],
              '@Key'));
      GetMovementDirectionKey;
    end;
    // attach the specimen record to the inbound movement
    FMovementCollectionUnitKey := VarToStr(
        dmGeneral.RunInsertStoredProc('Movement_Collection_Unit',
            'usp_MovementCollectionUnit_Insert',
            ['@Key', Null,
             '@CollectionUnitKey', FWrapper.FSpecimenUnitTable.FCollectionUnitKey,
             '@MovementDirectionKey', FMovementDirectionKey],
            '@Key'));
  finally
    NullStrictConvert := ltfOldNullStrictConvert;
  end;
end;

{-------------------------------------------------------------------------------
}
function TMovementTable.GetFields(FieldName: TFieldName; AIndex: Integer): string;
begin
  case FieldName of
    fnAccessionNumber, fnAcquisitionNumber:
      Result := FMovementNumber;
    fnStaffResponsibleNameKey:
      Result := FStaffResponsibleNameKey;
  end;
end;

{-------------------------------------------------------------------------------
}
procedure TMovementTable.SetFields(FieldName: TFieldName; AIndex: Integer;
  const Value: string);
begin
  inherited SetFields(FieldName, AIndex, Value);
  case FieldName of
    fnAccessionNumber: begin
      FMovementNumber := Value;
      // As we have an accession number, ensure there is a movement of ownership
      with FWrapper do
        CreateTable(TInsertTable(FMovementofOwnershipTable), TMovementOfOwnershipTable);
    end;
    fnAcquisitionNumber: begin
      FMovementNumber := Value;
      // As we have an acquisition number, ensure there is a movement of material and ownership
      with FWrapper do begin
        CreateTable(TInsertTable(FMovementofOwnershipTable), TMovementOfOwnershipTable);
        CreateTable(TInsertTable(FMovementofMaterialTable), TMovementOfMaterialTable);
      end;
    end;
    fnStaffResponsibleNameKey:
      FStaffResponsibleNameKey := Value;
  end;
end;

{-------------------------------------------------------------------------------
  Validate to ensure mandatory fields present.  Also ensure staff responsible
    is part of the holding organisation.
}
function TMovementTable.Validate: TFieldNames;
begin
  Result := [];
  if FMovementNumber = '' then
    Result := Result + [fnAcquisitionNumber];
  if (FStaffResponsibleNameKey<>'') and (FStaffResponsibleNameKey<>'Unknown') and
      (FStaffResponsibleNameKey<>'NBNSYS0000000004'{Unknown}) then
    if not dmGeneral.GetStoredProcOutputParam('usp_MemberOfHoldingOrg_Get',
          ['@Key', FStaffResponsibleNameKey], '@IsMember') then begin
      Result := Result + [fnStaffResponsibleNameKey];
      raise EQuickEntryException.Create(Format(ResStr_MustBeMemberOfOrg,
          [ResStr_StaffResponsible,
          VarToStr(dmGeneral.GetStoredProcOutputParam('usp_HoldingOrgName_Get', [],
                                                        '@HoldingOrg'))]));
    end;
end;

{-------------------------------------------------------------------------------
  Retrieve the movement direction key based on the movement number
}
procedure TMovementTable.GetMovementDirectionKey;
begin
  // Try to find a pre-existing movement by looking for a suitable movement direction record
  if not Assigned(FWrapper.FMovementOfOwnershipTable) then
    // for acquisition numbers, we aren't fussy about the movement type
    FMovementDirectionKey :=  dmGeneral.GetStoredProcOutputParam(
        'usp_MovementDirectionKey_Get_ForNumber',
        ['@Key', Null, '@MovementNumber', FMovementNumber], '@Key')
  else
    // accession number, so inbound movement must be an accession
    FMovementDirectionKey :=  dmGeneral.GetStoredProcOutputParam(
        'usp_MovementDirectionAccessionOrExchangeKey_Get',
        ['@Key', Null, '@MovementNumber', FMovementNumber], '@Key');
end;  

{ TMetadataTable }

{-------------------------------------------------------------------------------
}
procedure TMetadataTable.CheckLinks;
begin
  with FWrapper do begin
    CreateTable(TInsertTable(FSpecimenUnitTable), TSpecimenUnitTable);
    AddParentTable(FSpecimenUnitTable);
  end;
end;

{-------------------------------------------------------------------------------
}
procedure TMetadataTable.Clear;
begin
  inherited;
  FMetadataKey := '';
  FMetadataText := '';
  FMetadataTypeKey := '';
end;

{-------------------------------------------------------------------------------
}
constructor TMetadataTable.Create(AWrapper: TInsertWrapper);
begin
  inherited Create(AWrapper);
end;  

{-------------------------------------------------------------------------------
}
procedure TMetadataTable.Execute(APrepared: Boolean);
var
  ltfOldNullStrictConvert: Boolean;
begin
  inherited;
  ltfOldNullStrictConvert := NullStrictConvert;
  NullStrictConvert := False;
  try
    FMetadataKey := VarToStr(dmGeneral.RunInsertStoredProc('Metadata', 'usp_Metadata_Insert',
                    ['@MetadataKey', Null,
                    '@RecordKey', FWrapper.FSpecimenUnitTable.FCollectionUnitKey,
                    '@Text', FMetadataText,
                    '@MetadataTypeKey', FMetadataTypeKey,
                    '@SessionID', dmGeneral.Recorder.CurrentSettings.SessionID],
                    '@MetadataKey'));
  finally
    NullStrictConvert := ltfOldNullStrictConvert;
  end;
end;  

{-------------------------------------------------------------------------------
}
function TMetadataTable.GetFields(FieldName: TFieldName; AIndex: Integer): string;
begin
  if FieldName = fnValue then
    Result := FMetadataText;
end;

{-------------------------------------------------------------------------------
}
procedure TMetadataTable.SetFields(FieldName: TFieldName; AIndex: Integer;
  const Value: string);
begin
  inherited SetFields(FieldName, AIndex, Value);
  
  if FieldName = fnValue then
   FMetadataText := Value;
end;

{-------------------------------------------------------------------------------
  Validate to ensure mandatory fields present.
}
function TMetadataTable.Validate: TFieldNames;
begin 
  Result := [];
end;

{-==============================================================================
    TGeoAreaTable
===============================================================================}
{-------------------------------------------------------------------------------
}
constructor TGeoAreaTable.Create(AWrapper: TInsertWrapper);
begin
  inherited Create(AWrapper);

  FGeoAreaConceptKeys := TStringList.Create;
  FSEGeoAreaKeys := TStringList.Create;
end;  // TGeoAreaTable.Create

{-------------------------------------------------------------------------------
}
destructor TGeoAreaTable.Destroy;
begin
  FGeoAreaConceptKeys.Free;
  FSEGeoAreaKeys.Free;

  inherited Destroy;
end;  // TGeoAreaTable.Destroy

{-------------------------------------------------------------------------------
  Ensure that other required tables exist and are linked.
}
procedure TGeoAreaTable.CheckLinks;
begin
  with FWrapper do begin
    CreateTable(TInsertTable(FSampleTable), TSampleTable);
    AddParentTable(FSampleTable);
  end;
end;  // TGeoAreaTable.CheckLinks

{-------------------------------------------------------------------------------
}
procedure TGeoAreaTable.Clear;
begin
  inherited;
  FGeoAreaConceptKeys.Clear;
  FSEGeoAreaKeys.Clear;
end;  // TGeoAreaTable.Clear

{-------------------------------------------------------------------------------
}
procedure TGeoAreaTable.Execute(APrepared : Boolean = False);
var
  ltfOldNullStrictConvert: Boolean;
  i: Integer;
begin
  inherited;

  ltfOldNullStrictConvert := NullStrictConvert;
  NullStrictConvert := False;
  try
    for i := 0 to FGeoAreaConceptKeys.Count - 1 do begin
      if i < FSEGeoAreaKeys.Count then
        FSEGeoAreaKeys[i] := VarToStr(
            dmGeneral.RunInsertStoredProc('Survey_Event_Geo_Area',
                'usp_SurveyEventGeoArea_Insert',
                ['@Key', Null,     
                 '@SurveyEventKey', FWrapper.FSurveyEventTable.FSurveyEventKey,
                 '@ConceptKey', FGeoAreaConceptKeys[i],
                 '@EnteredSessionId', dmGeneral.Recorder.CurrentSettings.SessionID],
                '@Key'))
      else
        FSEGeoAreaKeys.Add(VarToStr(dmGeneral.RunInsertStoredProc('Survey_Event_Geo_Area',
                            'usp_SurveyEventGeoArea_Insert',
                            ['@Key', Null,
                             '@SurveyEventKey', FWrapper.FSurveyEventTable.FSurveyEventKey,
                             '@ConceptKey', FGeoAreaConceptKeys[i],
                             '@EnteredSessionId', dmGeneral.Recorder.CurrentSettings.SessionID],
                             '@Key')));
    end;
  finally
    NullStrictConvert := ltfOldNullStrictConvert;
  end;
end;  // TGeoAreaTable.Execute

{-------------------------------------------------------------------------------
}
function TGeoAreaTable.GetFields(FieldName: TFieldName; AIndex: Integer): string;
begin
  if FieldName = fnGeoAreaKey then begin
    if AIndex < FGeoAreaConceptKeys.Count then
      Result := FGeoAreaConceptKeys[AIndex]
    else
      Result := '';
  end;
end;  // TGeoAreaTable.GetFields

{-------------------------------------------------------------------------------
}
procedure TGeoAreaTable.SetFields(FieldName: TFieldName; AIndex: Integer; const
    Value: string);
var
  i: Integer;
begin
  inherited SetFields(FieldName, AIndex, Value);

  if FieldName = fnGeoAreaKey then begin
    if AIndex < FGeoAreaConceptKeys.Count then
      FGeoAreaConceptKeys[AIndex] := Value
    else begin
      for i := FGeoAreaConceptKeys.Count to AIndex - 1 do
        FGeoAreaConceptKeys.Add('');
      FGeoAreaConceptKeys.Add(Value);
    end;
  end;
end;  // TGeoAreaTable.SetFields

{-------------------------------------------------------------------------------
  Validate to ensure mandatory fields present.
}
function TGeoAreaTable.Validate: TFieldNames;
var
  i: Integer;
begin
  Result := [];
  for i := 0 to FGeoAreaConceptKeys.Count - 1 do
    if FGeoAreaConceptKeys[i] = '' then
      Result := Result + [fnGeoAreaKey];
end;  // TGeoAreaTable.Validate

end.
