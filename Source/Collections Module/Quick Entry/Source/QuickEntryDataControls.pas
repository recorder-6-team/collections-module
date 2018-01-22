{===============================================================================
  Unit:        QuickEntryDataControls.pas

  Defines:     TDataEntryControl, TGeneralDataEntryControl,
      TSpecimenDataEntryControl

  Description: Implements controls used on the QuickEntryFrame and
               QuickEntryManagerFrame to allow the user to enter data.

  Model:       QuickEntryManager.mpb

  Created:     September 2003

  Last revision information:
    $Revision: 60 $
    $Date: 8/12/11 17:45 $
    $Author: Jamesbichard $

===============================================================================}

unit QuickEntryDataControls;

interface

uses
  SysUtils, Windows, Messages, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls,
  ExtCtrls, Recorder2000_TLB, ImgList, DataTypes, DataClasses, BaseDragFrameUnit,
  QuickEntryInsertTables, ExceptionForm, LinkedControls, Menus, XPMenu, DSSDataTypes,
  ImageListButton, QEMultiValuePopup, Contnrs;

resourcestring
  ResStr_ItemNotInTemplateSubjectArea =
      'The item is not in the template''s subject area and cannot be selected.';
  ResStr_Add   = 'Add %s';
  ResStr_Items = '%d items';
  ResStr_ConceptNotInGeoArea = 'The selected concept is not from a valid geographic area.';

type
  TGetMacroNumberEvent = procedure(Sender: TObject; var ANumber: string) of object;

  {-----------------------------------------------------------------------------
    Linked edit control that has additional drop down menu for map list.
  }
  TSpatialRefLinkedEdit = class(TLinkedEdit)
  private
    FDropDownButton: TButton;
    FDropDownMenu: TPopupMenu;
    procedure DropDownClick(Sender: TObject);
    procedure MenuItemClick(Sender: TObject);
  protected
    procedure DoResize; override;
    procedure SetEditMode(const Value: TEditMode); override;
    procedure SetEnabled(Value: Boolean); override;
    procedure SetShowButton(Value: Boolean); override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  {-----------------------------------------------------------------------------
    Linked edit control that has additional button for selecting multiple values.
  }
  TMultiValueLinkedEdit = class(TLinkedEdit)
  private
    FAddButton: TImageListButton;
    FItemKeys: TStringList;
    FDataRowKey: Integer;
    FDataType: Integer;
    FFieldName: String;
    FTemplateFieldKey: TKeyString;
    FPopupOpened: Boolean;
    FDataItemKeys: TStringList;
    FOnModify: TNotifyEvent;
    procedure AddClick(Sender: TObject);
    procedure EditChange(Sender: TObject);
    procedure EditClick(Sender: TObject);
    procedure EditEnter(Sender: TObject);
    procedure EditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure EditKeyPressed(Sender: TObject; var Key: Char);
  protected
    procedure DoResize; override;
    procedure SetEditMode(const Value: TEditMode); override;
    procedure SetEnabled(Value: Boolean); override;
    procedure SetShowButton(Value: Boolean); override;
    procedure SetText(const Value: String); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ClearItemList;
    procedure DeleteSavedValues;
    procedure SetDisplayAndValue(AIndex: Integer; const ADisplay: String; const AValue: TKeyString);
    property DataRowKey: Integer read FDataRowKey write FDataRowKey;
    property DataType: Integer read FDataType write FDataType;
    property FieldName: String read FFieldname write FFieldName;
    property ItemKeys: TStringList read FItemKeys;
    property PopupOpened: Boolean read FPopupOpened write FPopupOpened;
    property TemplateFieldKey: TKeyString read FTemplateFieldKey write FTemplateFieldKey;
    property DataItemKeys: TStringList read FDataItemKeys;
    property OnModify: TNotifyEvent read FOnModify write FOnModify;
  end;

  {-----------------------------------------------------------------------------
    Exception class for QuickEntryDataControls unit.
  }
  EQuickEntryControls = class(EQuickEntryException)
  end;

  {-----------------------------------------------------------------------------
  }
  TDataEntryControlItem = class(TObject)
  private
    FDataItemKey: Integer;
    FDefaultDisplay: string;
    FDefaultValue: string;
    FDisplay: string;
    FOldDisplay: string;
    FOldValue: string;
    FTimestamp: TSQLSvrTimestamp;
    FValue: string;
  public
    constructor Create;
    destructor Destroy; override;
    property DataItemKey: Integer read FDataItemKey write FDataItemKey;
    property DefaultDisplay: string read FDefaultDisplay write FDefaultDisplay;
    property DefaultValue: string read FDefaultValue write FDefaultValue;
    property Display: string read FDisplay write FDisplay;
    property OldDisplay: string read FOldDisplay write FOldDisplay;
    property OldValue: string read FOldValue write FOldValue;
    property Timestamp: TSQLSvrTimestamp read FTimestamp write FTimestamp;
    property Value: string read FValue write FValue;
  end;

  {-----------------------------------------------------------------------------
  }
  TDataEntryControlItemList = class(TObject)
  private
    FItems: TObjectList;
    function GetCount: Integer;
    function GetItem(iIndex: Integer): TDataEntryControlItem;
    procedure Clear;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(AItem: TDataEntryControlItem);
    property Items[iIndex:Integer]: TDataEntryControlItem read GetItem;
    property Count: Integer read GetCount;
  end;

  {-----------------------------------------------------------------------------
    Class used for putting quick entry fields on TfraQuickEntry. The control
    is comprised of a panel containing a label and another arbitrary control.
    The control is created in the SetupDataEntryControl procedure and its
    type is depended on the DataType property, which correlates with the
    Data_Type field in QE_Field.
  }
  TDataEntryControl = class(TPanel, IRequestor)
    procedure IRequestor.Update = RequestorUpdate;
  private
    FColumnCaption: string;
    FControl: TWinControl;
    FDataType: Integer;
    FDefaultWidth: Integer;
    FFieldLookupKey: string;
    FFieldName: TFieldName;
    FFormatted: string;
    FIsCustom: Integer;
    FMeasurementAccuracy: string;
    FMeasurementAppliesTo: string;
    FMeasurementDuration: string;
    FMeasurementIsSpecimen: Boolean;
    FMeasurementMethodConceptKey: string;
    FMeasurementParameterConceptKey: string;
    FMeasurementUnitConceptKey: string;
    FOnSetParent: TNotifyEvent;
    FOriginalPlainText: string;
    FSubjectAreaKey: string;
    FTable: string;
    FTableName: TTableName;
    FTemplateFieldKey: string;
    FXPMenu: TXPMenu;
    FMeasurementIsTaxonData: Boolean;
    FMeasurementTaxonUnitKey: String;
    FMeasurementTaxonQualifierKey: String;
    FOnGetMacroNumber: TGetMacroNumberEvent;
    FMetadataTypeKey: string;
    FNumberTypeKey: string;
    FNumberPreferred: Boolean;
    FDataEntryControlItemList: TDataEntryControlItemList;
    FOnUpdateMultivalue: TNotifyEvent;
    function ConceptInGeographySubjectArea(const conceptKey: String): Boolean;
    procedure ConceptDropped(const AKey: string);
    function GetDataItemKey(Index: Integer): Integer;
    function GetItemCount: Integer;
    function GetTimestamp(Index: Integer): TSQLSvrTimestamp;
    procedure ItemDropped(const AKey1, ATableName: string);
    procedure LinkedEditFindData(Sender: TObject);
    procedure LinkedEditGetData(Sender: TObject);
    procedure LinkedEditGetThesaurusData(AConceptGroup: string);
    procedure MacroKeyDown(Sender: TObject; var Key: Word;  Shift: TShiftState);
    function Plaintext(const AText: string): string;
    procedure PositionControl;
    procedure SetColumnCaption(const NewValue: string);
    procedure SetDataItemKey(Index: Integer; Value: Integer);
    procedure SetDataType(const Value: Integer);
    procedure SetDefaultWidth(Value: Integer);
    procedure SetFieldLookupKey(const Value: string);
    procedure SetFieldName(const Value: TFieldName);
    procedure SetSubjectAreaKey(const NewValue: string);
    procedure SetTableName(const Value: TTableName);
    procedure SetTimestamp(Index: Integer; const Value: TSQLSvrTimestamp);
    procedure SetValueForSpatialRef(ALatLong: ILatLong; const ASystem,
        AQualifier: string);
    procedure TaxonDropped(const AKey: string);
    procedure UpdateConcept(const AKey: string);
    procedure UpdateConceptText(const AValue: string);
    procedure SetOnGetMacroNumber(Value: TGetMacroNumberEvent);
    procedure UpdateTaxon(const AKey: string);
    procedure MultivalueControlUpdated(sender: TObject);
  protected
    function GetDefaultDisplay(Index: Integer): string; virtual;
    function GetDefaultValue(Index: Integer): string; virtual;
    function GetDisplay(Index: Integer): string; virtual;
    function GetOldDisplay(Index: Integer): string; virtual;
    function GetOldValue(Index: Integer): string; virtual;
    function GetValue(Index: Integer): string; virtual;
    procedure SetDefaultDisplay(Index: Integer; const Value: string); virtual;
    procedure SetDefaultValue(Index: Integer; const Value: string); virtual;
    procedure SetDisplay(Index: Integer; const Value: string); virtual;
    procedure SetOldDisplay(Index: Integer; const Value: string); virtual;
    procedure SetOldValue(Index: Integer; const Value: string); virtual;
    procedure SetParent(AParentWindow: TWinControl); override;
    procedure SetValue(Index: Integer; const Value: string); virtual;
  public
    constructor Create(AOwner:TComponent); override;
    destructor Destroy; override;
    function ForceValue(AItem: Integer): string;
    procedure LinkedEditDataDragOverCheck(APoint: TPoint; const ATable, AFieldKey: String;
        var Accept: Boolean);
    procedure LinkedEditDataDropped(const Sender: TObject; const iFormat :
        integer; const iSourceData: TKeyList; const iTextStrings : TstringList;
        var ioHandled : boolean);
    procedure LocationNameDropped(const Sender: TObject; const iFormat :
        integer; const iSourceData: TKeyList; const iTextStrings : TstringList;
        var ioHandled : boolean);
    procedure RegisterLinkedEditControl(ABaseDragFrame: TBaseDragFrame);
        virtual;
    procedure RequestorUpdate(const KeyList: IKeyList); overload; safecall;
    procedure Reset;
    procedure SetupDataEntryControl(AllowMultiValues: Boolean);
    procedure UpdateDataItem(const ADataRowKey: Integer; AIndex: Integer);
    procedure ValidateSpatialRef;
    property ColumnCaption: string read FColumnCaption write SetColumnCaption;
    property Control: TWinControl read FControl write FControl;
    property DataItemKey[Index: Integer]: Integer read GetDataItemKey write SetDataItemKey;
    property DataType: Integer read FDataType write SetDataType;
    property DefaultDisplay[Index: Integer]: string read GetDefaultDisplay write SetDefaultDisplay;
    property DefaultValue[Index: Integer]: string read GetDefaultValue write SetDefaultValue;
    property DefaultWidth: Integer read FDefaultWidth write SetDefaultWidth;
    property Display[Index: Integer]: string read GetDisplay write SetDisplay;
    property FieldLookupKey: string read FFieldLookupKey write
        SetFieldLookupKey;
    property FieldName: TFieldName read FFieldName write SetFieldName;
    property IsCustom: Integer read FIsCustom write FIsCustom;
    property ItemCount: Integer read GetItemCount;
    property MeasurementAccuracy: string read FMeasurementAccuracy write
        FMeasurementAccuracy;
    property MeasurementAppliesTo: string read FMeasurementAppliesTo write
        FMeasurementAppliesTo;
    property MeasurementDuration: string read FMeasurementDuration write
        FMeasurementDuration;
    property MeasurementIsSpecimen: Boolean read FMeasurementIsSpecimen write
        FMeasurementIsSpecimen;
    property MeasurementIsTaxonData: Boolean read FMeasurementIsTaxonData write
        FMeasurementIsTaxonData;
    property MeasurementMethodConceptKey: string read
        FMeasurementMethodConceptKey write FMeasurementMethodConceptKey;
    property MeasurementParameterConceptKey: string read
        FMeasurementParameterConceptKey write FMeasurementParameterConceptKey;
    property MeasurementTaxonQualifierKey: String read FMeasurementTaxonQualifierKey write
        FMeasurementTaxonQualifierKey;
    property MeasurementTaxonUnitKey: String read FMeasurementTaxonUnitKey write
        FMeasurementTaxonUnitKey;
    property MeasurementUnitConceptKey: string read FMeasurementUnitConceptKey
        write FMeasurementUnitConceptKey;
    property OldDisplay[Index: Integer]: string read GetOldDisplay write SetOldDisplay;
    property OldValue[Index: Integer]: string read GetOldValue write SetOldValue;
    property SubjectAreaKey: string read FSubjectAreaKey write
        SetSubjectAreaKey;
    property Table: string read FTable;
    property TableName: TTableName read FTableName write SetTableName;
    property TemplateFieldKey: string read FTemplateFieldKey write
        FTemplateFieldKey;
    property Timestamp[Index: Integer]: TSQLSvrTimestamp read GetTimestamp write SetTimestamp;
    property Value[Index: Integer]: string read GetValue write SetValue;
    property XPMenu: TXPMenu read FXPMenu write FXPMenu;
    property OnGetMacroNumber: TGetMacroNumberEvent read FOnGetMacroNumber write
        SetOnGetMacroNumber;
    property MetadataTypeKey: string read FMetadataTypeKey write FMetadataTypeKey;
    property NumberTypeKey: string read FNumberTypeKey write FNumberTypeKey;
    property NumberPreferred: Boolean read FNumberPreferred write FNumberPreferred;
    property OnUpdateMultivalue: TNotifyEvent read FOnUpdateMultivalue write FOnUpdateMultivalue;
  end;

  {-----------------------------------------------------------------------------
    Instances of this class are intended to go on the General Tab of
    TfraQuickEntry. The class has nothing beyond its parent at present but
    it may be useful to specialise later.
  }
  TGeneralDataEntryControl = class(TDataEntryControl)
  end;

  {-----------------------------------------------------------------------------
    TDataEntryControl which expects that the control will be lifted of it
    and placed on another parent. Also has GridColumn property which allows
    it to be associated with a column in a grid.
  }
  TSpecimenDataEntryControl = class(TDataEntryControl)
  private
    FEnabled: Boolean;
    FGridColumn: Integer;
    FLocked: Boolean;
    FOnControlReplaced: TNotifyEvent;
  protected
    function GetDisplay(Index: Integer): string; override;
    function GetValue(Index: Integer): string; override;
    procedure SetDisplay(Index: Integer; const Value: string); override;
    procedure SetEnabled(Value: Boolean); override;
    procedure SetLocked(Value: Boolean);
    procedure SetValue(Index: Integer; const Value: string); override;
  public
    procedure RegisterLinkedEditControl(ABaseDragFrame: TBaseDragFrame);
        override;
    procedure ReplaceEditControl;
    property Enabled: Boolean read FEnabled write SetEnabled;
    property GridColumn: Integer read FGridColumn write FGridColumn;
    property Locked: Boolean read FLocked write SetLocked;
  published
    property OnControlReplaced: TNotifyEvent read FOnControlReplaced write
        FOnControlReplaced;
  end;

//==============================================================================
implementation

uses
  ComboListID, GeneralData, VagueDateEdit, QuickEntryData,
  InterfaceDataModule, Validation, Variants, VagueDate, ConceptGroupComboBox,
  LuxIDComboBox,ResourceStrings, Math, ActiveX, ThesaurusBrowser_TLB,
  LuxembourgConstants, GeneralFunctions, ApplicationSettings, QuickEntryFrame,
  LuxembourgDataClasses, ComObj, SearchManager, ADODB, ADOInt;

{-==============================================================================
    TDataEntryControl
===============================================================================}
{-------------------------------------------------------------------------------
  Sets some intial properties
}
constructor TDataEntryControl.Create(AOwner:TComponent);
begin
  inherited Create(AOwner);
  BevelInner := bvNone;
  BevelOuter := bvNone;
  FDataEntryControlItemList := TDataEntryControlItemList.Create();
end;  // TDataEntryControl.Create

{-------------------------------------------------------------------------------
  Object finalisation
}
destructor TDataEntryControl.Destroy;
begin
  FDataEntryControlItemList.Free;
  inherited Destroy;
end;  // TDataEntryControl.Destroy

{-------------------------------------------------------------------------------
}
function TDataEntryControl.ConceptInGeographySubjectArea(const conceptKey: String): Boolean;
begin
  Result := VarToStr(dmGeneral.GetStoredProcOutputParam(
      'usp_SubjectArea_Select_ForConcept_Get',
      ['@Key', conceptKey],
      '@Subject_Area_Key')) = CG_GEO_SUBJECT;
end;

{-------------------------------------------------------------------------------
  Handle the update of the data control when a concept is dropped.  Includes
      validation of subject area.
}
procedure TDataEntryControl.ConceptDropped(const AKey: string);
begin
  if SubjectAreaKey <> '' then
    if VarToStr(dmGeneral.GetStoredProcOutputParam(
        'usp_SubjectArea_Select_ForConcept_Get',
        ['@Key', AKey], '@Subject_Area_Key'))<>SubjectAreaKey then begin
    ShowInformation(ResStr_ItemNotInTemplateSubjectArea);
    Exit;
  end;
  UpdateConcept(AKey);
end;  // TDataEntryControl.ConceptDropped

{-------------------------------------------------------------------------------
  Does the utmost possible to return a value from the user, in some cases
      asking for user input through the find dialog. Raises an error if the
      attempt to return a value is unsuccessful.
}
function TDataEntryControl.ForceValue(AItem: Integer): string;
var
  lCurrentDate: TVagueDate;
begin
  // If no linked value already available,
  if Value[AItem] = '' then
  begin
    if Display[AItem]<>'' then
      case DataType of
        CT_SPATIAL_REF: //spatial ref
          ValidateSpatialRef;
        CT_TAXON, CT_CONCEPT, CT_INDIVIDUAL, CT_ORGANISATION, CT_STORE,
        CT_STAFF_RESPONSIBLE, CT_GEO_AREA, CT_LOCATION:
            //Find dialog
          LinkedEditFindData(nil);
      end; // case
  end;

  Result := Value[AItem];
  if Result = '' then
    raise EQuickEntryControls.CreateNonCritical(Format(ResStr_PleaseEnterAValueForThe,
                                                       [ColumnCaption]))
  else
  if DataType = CT_VAGUE_DATE then begin
    // Ensure vague dates are not in future
    lCurrentDate := StringToVagueDate(DateToStr(Date));
    if CompareVagueDateToVagueDate(lCurrentDate, TVagueDateEdit(FControl).VagueDate) = -1 then
      raise EQuickEntryControls.CreateNonCritical(Format(ResStr_DateCannotBeInFuture,
                                                         [ColumnCaption]));
  end;
end;  // TDataEntryControl.ForceValue

{-------------------------------------------------------------------------------
}
function TDataEntryControl.GetDataItemKey(Index: Integer): Integer;
begin
  if Index < FDataEntryControlItemList.Count then
    Result := FDataEntryControlItemList.Items[Index].DataItemKey
  else
    Result := -1;
end;  // TDataEntryControl.GetDataItemKey

{-------------------------------------------------------------------------------
}
function TDataEntryControl.GetDefaultDisplay(Index: Integer): string;
begin
  if Index < FDataEntryControlItemList.Count then
    Result := FDataEntryControlItemList.Items[Index].DefaultDisplay
  else
    Result := '';
end;  // TDataEntryControl.GetDefaultDisplay

{-------------------------------------------------------------------------------
}
function TDataEntryControl.GetDefaultValue(Index: Integer): string;
begin
  if Index < FDataEntryControlItemList.Count then
    Result := FDataEntryControlItemList.Items[Index].DefaultValue
  else
    Result := '';
end;  // TDataEntryControl.GetDefaultValue

{-------------------------------------------------------------------------------
  Returns a string representing the value which will eventually be entered in
      the database. This is a display value meaningful to humans.
}
function TDataEntryControl.GetDisplay(Index: Integer): string;
begin
  Case DataType of
    CT_FREETEXT, CT_REGISTRATION, CT_ACCESSION:
        Result := TCustomEdit(Control).Text;

    CT_VAGUE_DATE:
        with TVagueDateEdit(Control) do
          if IsVagueDate(Text) then
            Result := VagueDateToString(StringToVagueDate(Text))
          else
            Result := '';

    CT_TAXON:
        Result := TLinkedEdit(Control).Text;

    CT_SPATIAL_REF, CT_LOCATION,
    CT_ORGANISATION, CT_STORE, CT_STAFF_RESPONSIBLE:
        Result := TLinkedEdit(Control).Text;

    CT_CONCEPT:
      if Control is TMultiValueLinkedEdit then begin
        with TMultiValueLinkedEdit(Control) do
          if (Index = 0) and (ItemKeys.Count = 0) then
            Result := Text
          else
            Result := ItemKeys.ValueFromIndex[Index];
      end else begin
        // Return formatted value, if still valid
        if FOriginalPlainText = TLinkedEdit(Control).Text then
          Result := FFormatted
        else
          Result := TLinkedEdit(Control).Text;
      end;

    CT_CONCEPT_IN_GROUP, CT_COLLECTION, CT_DEPARTMENT, CT_SURVEY,
    CT_SAMPLE_TYPE, CT_YES_NO, CT_RECORD_TYPE, CT_DETERMINER_ROLE, CT_INFERRED:
        with TLuxIDComboBox(Control) do
          if CurrentStrID <> '' then
            Result := Text
          else
            Result := '';

    CT_INDIVIDUAL, CT_GEO_AREA:
      if Control is TMultiValueLinkedEdit then begin
        with TMultiValueLinkedEdit(Control) do
          if (Index = 0) and (ItemKeys.Count = 0) then
            Result := Text
          else
            Result := ItemKeys.ValueFromIndex[Index];
      end else
        Result := TLinkedEdit(Control).Text;
  end;
end;  // TDataEntryControl.GetDisplay

{-------------------------------------------------------------------------------
}
function TDataEntryControl.GetItemCount: Integer;
begin
  if (DataType in [CT_CONCEPT, CT_INDIVIDUAL, CT_GEO_AREA]) and (Control is TMultiValueLinkedEdit) then
    Result := Max(1, TMultiValueLinkedEdit(Control).ItemKeys.Count)
  else
    Result := 1;
end;  // TDataEntryControl.GetItemCount

{-------------------------------------------------------------------------------
}
function TDataEntryControl.GetOldDisplay(Index: Integer): string;
begin
  if Index < FDataEntryControlItemList.Count then
    Result := FDataEntryControlItemList.Items[Index].OldDisplay
  else
    Result := '';
end;  // TDataEntryControl.GetOldDisplay

{-------------------------------------------------------------------------------
}
function TDataEntryControl.GetOldValue(Index: Integer): string;
begin
  if Index < FDataEntryControlItemList.Count then
    Result := FDataEntryControlItemList.Items[Index].OldValue
  else
    Result := '';
end;  // TDataEntryControl.GetOldValue

{-------------------------------------------------------------------------------
}
function TDataEntryControl.GetTimestamp(Index: Integer): TSQLSvrTimestamp;
begin
  if Index < FDataEntryControlItemList.Count then
    Result := FDataEntryControlItemList.Items[Index].Timestamp
  else
    Result := Null;
end;

{-------------------------------------------------------------------------------
  Returns the value which will eventually be entered into the database.
}
function TDataEntryControl.GetValue(Index: Integer): string;
begin
  case DataType of
    CT_FREETEXT, CT_REGISTRATION, CT_ACCESSION:
        Result := TCustomEdit(Control).Text;

    CT_VAGUE_DATE:
      with TVagueDateEdit(Control) do
        if IsVagueDate(Text) then
          Result := VagueDateToString(StringToVagueDate(Text))
        else
          Result := '';

    CT_SPATIAL_REF, CT_TAXON, CT_LOCATION,
    CT_ORGANISATION, CT_STORE, CT_STAFF_RESPONSIBLE:
        Result := TLinkedEdit(Control).Key;

    CT_CONCEPT_IN_GROUP, CT_COLLECTION, CT_DEPARTMENT, CT_SURVEY,
    CT_SAMPLE_TYPE, CT_YES_NO, CT_RECORD_TYPE, CT_DETERMINER_ROLE, CT_INFERRED:
        Result := TLuxIDComboBox(Control).CurrentStrId;

    CT_CONCEPT, CT_INDIVIDUAL, CT_GEO_AREA:
      if Control is TMultiValueLinkedEdit then begin
        with TMultiValueLinkedEdit(Control) do
          if (Index = 0) and (ItemKeys.Count = 0) then
            Result := TLinkedEdit(Control).Key
          else
            Result := ItemKeys.Names[Index];
      end else
        Result := TLinkedEdit(Control).Key;
  end;
end;  // TDataEntryControl.GetValue

{-------------------------------------------------------------------------------
  Updates the control for a single item, dropped as part of a ItemKeys or
      reqturn data procedure.
}
procedure TDataEntryControl.ItemDropped(const AKey1, ATableName: string);
var
  lLatLong: ILatLong;
  lNewValue, lNewDisplay: string;
  lNewTimestamp: TSQLSvrTimestamp;
begin
  Value[0] := AKey1;
  case DataType of
    CT_SPATIAL_REF:
      begin
        TLinkedEdit(Control).Text := ATableName;
        Display[0] := TLinkedEdit(Control).Text;
        //save as lat long
        lLatLong := dmGeneral.Recorder.RecorderFunctions.DecodeSpatialRef(Display[0]);
        SetValueForSpatialRef(lLatLong, dmGeneral.Recorder.SpatialRefSystem, 'Internal Map');
      end;
    CT_TAXON:
      TaxonDropped(Value[0]);

    CT_CONCEPT:// determination concept
      if not ConceptHasMappedTaxon(Value[0]) then
        ConceptDropped(Value[0]);

    CT_LOCATION:
      Display[0] := dmGeneral.GetStoredProcOutputParam('usp_LocationName_Get',
          ['@Key', Value[0]], '@Caption');

    CT_INDIVIDUAL:
      if CompareText(ATableName, TN_INDIVIDUAL) = 0 then begin
        if Control is TMultiValueLinkedEdit then
          TMultiValueLinkedEdit(Control).SetDisplayAndValue(0,
              dmGeneral.GetStoredProcOutputParam('usp_Name_Get', ['@Key', AKey1],
              '@Caption'), AKey1)
        else
          Display[0] := dmGeneral.GetStoredProcOutputParam('usp_Name_Get',
              ['@Key', AKey1], '@Caption');
      end;
    CT_STAFF_RESPONSIBLE:
      if CompareText(ATableName, TN_INDIVIDUAL) = 0 then
        Display[0] := dmGeneral.GetStoredProcOutputParam('usp_Name_Get',
            ['@Key', Value[0]], '@Caption');

    CT_ORGANISATION:
      if CompareText(ATableName, TN_ORGANISATION) = 0 then
        Display[0] := dmGeneral.GetStoredProcOutputParam('usp_Name_Get',
            ['@Key', Value[0]], '@Caption');

    CT_STORE:
      if CompareText(ATableName, TN_STORE) = 0 then
        Display[0] := dmGeneral.GetStoredProcOutputParam('usp_Store_Get',
            ['@Key', Value[0]], '@Caption');

    CT_GEO_AREA:
      if Control is TMultiValueLinkedEdit then
        TMultiValueLinkedEdit(Control).SetDisplayAndValue(0,
            ConvertConceptKeyToCaption(AKey1, ATableName), AKey1);
      else
        Display[0] := ConvertConceptKeyToCaption(AKey1, ATableName);
  end;

  if Control is TMultiValueLinkedEdit then
  begin
    with TMultiValueLinkedEdit(Control) do
    begin
      DeleteSavedValues;

      DataItemKey[0] := dmGeneral.RunInsertStoredProc(
          'QE_Data_Item',
          'usp_QEDataItem_Insert_ForMultiValues',
          ['@DataRowKey', DataRowKey,
           '@TemplateFieldKey', TemplateFieldKey,
           '@DataValue', AKey1,
           '@DataDisplay', Text,
           '@Position', 0], '@DataItemKey');
      dmQuickEntry.RefreshDataItem(DataItemKey[0], lNewValue, lNewDisplay, lNewTimestamp);
      Value[0] := lNewValue;
      Display[0] := lNewDisplay;
      Timestamp[0] := lNewTimestamp;
    end;
  end;
end;  // TDataEntryControl.ItemDropped

{-------------------------------------------------------------------------------
  Event handler for when data is dragged over a linked edit control.
}
procedure TDataEntryControl.LinkedEditDataDragOverCheck(APoint: TPoint; const ATable,
    AFieldKey: String; var Accept: Boolean);
begin
  Accept := (DataType <> CT_GEO_AREA) or ConceptInGeographySubjectArea(AFieldKey);
end;  // TDataEntryControl.LinkedEditDataDragOverCheck

{-------------------------------------------------------------------------------
  Event handler for when data is dropped on a linked edit control
}
procedure TDataEntryControl.LinkedEditDataDropped(const Sender: TObject; const
    iFormat : integer; const iSourceData: TKeyList; const iTextStrings :
    TStringList; var ioHandled : boolean);
var
  lTableName: string;
begin
  if CompareText(iSourceData.Header.TableName, MIXED_DATA) = 0 then
    lTableName := iSourceData.Items[0].KeyField2
  else
    lTableName := iSourceData.Header.TableName;

  if iFormat = dmGeneral.Recorder.JNCCFormat then
    ItemDropped(iSourceData.Items[0].KeyField1, lTableName);

  if DataType in [CT_TAXON, CT_CONCEPT, CT_LOCATION,
      CT_INDIVIDUAL, CT_ORGANISATION, CT_STAFF_RESPONSIBLE, CT_GEO_AREA] then
    ioHandled := True;
end;  // TDataEntryControl.LinkedEditDataDropped

{-------------------------------------------------------------------------------
  Event handler for the return key press. Displays the appropriate Find dialog.
}
procedure TDataEntryControl.LinkedEditFindData(Sender: TObject);
begin
  if Control is TLinkedEdit then
    case DataType of
      CT_TAXON:
        begin
          if Control is TMultiValueLinkedEdit then
            TMultiValueLinkedEdit(Control).ClearItemList;
          if CheckLinkedDeterminationLifeScience(TLinkedEdit(Control), '') then
            //if not ConceptHasMappedTaxon(TLinkedEdit(Control).Key) then
              if Value[0] <> '' then UpdateTaxon(Value[0]);
        end;

      CT_CONCEPT:
        begin
          if Control is TMultiValueLinkedEdit then
            TMultiValueLinkedEdit(Control).ClearItemList;
          if CheckLinkedDeterminationEarthScience(TLinkedEdit(Control), '') then
            if not ConceptHasMappedTaxon(TLinkedEdit(Control).Key) then
              if Value[0] <> '' then UpdateConcept(Value[0]);
        end;

      CT_LOCATION:
        CheckLinkedLocation(TLinkedEdit(Control), '');

      CT_INDIVIDUAL:
        begin
          if Control is TMultiValueLinkedEdit then
            TMultiValueLinkedEdit(Control).ClearItemList;
          CheckLinkedIndividual(TLinkedEdit(Control), '');
        end;

      CT_STAFF_RESPONSIBLE:
        CheckLinkedIndividual(TLinkedEdit(Control), '');

      CT_ORGANISATION:
        CheckLinkedOrganisation(TLinkedEdit(Control), '');

      CT_STORE:
        CheckLinkedStore(TLinkedEdit(Control), '');

      CT_GEO_AREA:
        begin
          if Control is TMultiValueLinkedEdit then
            TMultiValueLinkedEdit(Control).ClearItemList;
          DoCheck(TLinkedEdit(Control), stTermInSubjectArea, CG_GEO_SUBJECT);
        end;
    end;
end;  // TDataEntryControl.LinkedEditFindData

{-------------------------------------------------------------------------------
  Event handler for the button of the linked edit controls. Sends a search
      request to Recorder 2000.
}
procedure TDataEntryControl.LinkedEditGetData(Sender: TObject);
var
  conceptGroupKey: String;
begin
  case DataType of
    CT_SPATIAL_REF:
      dmGeneral.Recorder.RequestData(Self, 'MAPPOINT');

    CT_TAXON:
      dmGeneral.Recorder.RequestData(Self, 'TAXON');

    CT_CONCEPT://concept - retrieve from ThesaurusBrowser addin
      begin
        if Control is TMultiValueLinkedEdit then
          TMultiValueLinkedEdit(Control).ClearItemList;
        if SubjectAreaKey = '' then
          dmGeneral.Recorder.RequestCOMData(Self, CLASS_frmThesaurusBrowser)
        else
          LinkedEditGetThesaurusData(dmGeneral.GetStoredProcOutputParam(
              'usp_FindFirstConceptGroupForSubject',
              ['@SubjectAreaKey', SubjectAreaKey],
              '@ConceptGroupKey'));
      end;

    CT_LOCATION:
      dmGeneral.Recorder.RequestData(Self, 'LOCATION');

    CT_INDIVIDUAL:
      begin
        if Control is TMultiValueLinkedEdit then
          TMultiValueLinkedEdit(Control).ClearItemList;
        dmGeneral.Recorder.RequestData(Self, 'NAME');
      end;

    CT_STAFF_RESPONSIBLE:
      dmGeneral.Recorder.RequestData(Self, 'NAME');

    CT_ORGANISATION:
      dmGeneral.Recorder.RequestData(Self, 'NAME');

    CT_GEO_AREA:
      begin
        if Control is TMultiValueLinkedEdit then
          TMultiValueLinkedEdit(Control).ClearItemList;

        conceptGroupKey := dmGeneral.GetStoredProcOutputParam(
            'usp_FindFirstConceptGroupForSubject',
            ['@SubjectAreaKey', CG_GEO_SUBJECT],
            '@ConceptGroupKey');
        LinkedEditGetThesaurusData(conceptGroupKey);
      end;
  end;
end;  // TDataEntryControl.LinkedEditGetData

{-------------------------------------------------------------------------------
  If the linked edit get data event calls the thesaurus browser, this method
  handles it.
}
procedure TDataEntryControl.LinkedEditGetThesaurusData(AConceptGroup: string);
var
  lIntf: IUnknown;
  lThesaurus: IfrmThesaurusBrowser;
begin
  try
    with CreateCOMObject(CLASS_AutoApplicationSettings) as IRecorder2000 do
      lIntf := RequestCOMData(Self as IRequestor, CLASS_frmThesaurusBrowser);
  except
    on EOleSysError do begin
      with CreateCOMObject(CLASS_AutoApplicationSettings) as IRecorder2000 do
        RequestData(Self as IRequestor, GUIDToString(CLASS_frmThesaurusBrowser));
      lIntf := nil;
    end;
  end;
  // Ask the thesaurus to display the specified concept group
  if Assigned(lIntf) then begin
    if Supports(lIntf, IID_IfrmThesaurusBrowser, lThesaurus) then
      lThesaurus.DisplayConceptGroup(AConceptGroup);
  end;
end;  // TDataEntryControl.LinkedEditGetThesaurusData

{-------------------------------------------------------------------------------
  Handle when a location is dropped as a term from the Thesaurus browser.
}
procedure TDataEntryControl.LocationNameDropped(const Sender: TObject; const
    iFormat : integer; const iSourceData: TKeyList; const iTextStrings :
    TstringList; var ioHandled : boolean);
var
  lDroppedTerm: string;
begin
  if (FControl is TCustomEdit) and (iSourceData.Header.ItemCount > 0) then begin
    lDroppedTerm := dmGeneral.GetStoredProcOutputParam(
        'usp_ConceptItemName_Get',
        ['@Key', iSourceData.Items[0].KeyField1,
        '@IncludeCommonName', 0,
        '@IncludeAuthor', 0,
        '@Formatted', 1],
        '@ItemName');
    // Insert concept key and name into the text
    TCustomEdit(FControl).Text := iSourceData.Items[0].KeyField1 + ' ' + lDroppedTerm;
  end;
end;  // TDataEntryControl.LocationNameDropped 

{-------------------------------------------------------------------------------
  OnKeyDown handler for edit controls linked to the macro generation system.
      Pressing the enter key generates a new macro number.
}
procedure TDataEntryControl.MacroKeyDown(Sender: TObject; var Key: Word;
    Shift: TShiftState);
var
  lNumber: string;
begin
  if (Key=VK_RETURN) and Assigned(FOnGetMacroNumber) then begin
    FOnGetMacroNumber(self, lNumber);
    Display[0] := lNumber;
    Key := 0;
  end;
end;  // TDataEntryControl.MacroKeyDown

{-------------------------------------------------------------------------------
  Return a formatted term stripped of its italic markers 
}
function TDataEntryControl.Plaintext(const AText: string): string;
begin
  Result := StringReplace(
      StringReplace(AText, '<i>', '', [rfReplaceAll]),
      '</i>', '', [rfReplaceAll]
      );
end;  // TDataEntryControl.Plaintext 

{-------------------------------------------------------------------------------
  Positions the control in the correct place. 
}
procedure TDataEntryControl.PositionControl;
begin
  Control.Parent := Self;
  Control.Align := alTop;
end;  // TDataEntryControl.PositionControl 

{-------------------------------------------------------------------------------
  Registers the control as a drop target if it is a Linked Edit control. 
}
procedure TDataEntryControl.RegisterLinkedEditControl(ABaseDragFrame:
    TBaseDragFrame);
begin
  if FControl is TLinkedEdit then
    with TLinkedEdit(FControl) do
    begin
      if FTable<> '' then
      begin
        ABaseDragFrame.RegisterDropComponent(
            TWinControl(Control),
            LinkedEditDataDropped,
            [FTable],
            [dmGeneral.Recorder.JNCCFormat, CF_Text],
            LinkedEditDataDragOverCheck);
      end;
    end
  else if FieldName = fnLocationName then
    ABaseDragFrame.RegisterDropComponent(
        TWinControl(Control),
        LocationNameDropped,
        [TN_CONCEPT],
        [dmGeneral.Recorder.JNCCFormat]);
end;  // TDataEntryControl.RegisterLinkedEditControl

{-------------------------------------------------------------------------------
  Used to comply with the implementation of IRequestor interface.
  Updates the control with information from the returned data.
}
procedure TDataEntryControl.RequestorUpdate(const KeyList: IKeyList);
begin
  if KeyList.ItemCount > 0 then
    if (DataType <> CT_GEO_AREA) or
       ConceptInGeographySubjectArea(KeyList.GetKeyItem(0).KeyField1) then
      ItemDropped(KeyList.GetKeyItem(0).KeyField1, KeyList.GetKeyItem(0).KeyField2)
    else
      ShowInformation(ResStr_ConceptNotInGeoArea);
end;  // TDataEntryControl.RequestorUpdate

{-------------------------------------------------------------------------------
  Reset the data entry control's internal lists (used when loading new content
     on switching row)
}
procedure TDataEntryControl.Reset;
begin
  FDataEntryControlItemList.Clear;
end;

{-------------------------------------------------------------------------------
  Accessor method
}
procedure TDataEntryControl.SetColumnCaption(const NewValue: string);
begin
  FColumnCaption := NewValue;
end;  // TDataEntryControl.SetColumnCaption 

{-------------------------------------------------------------------------------
  Accessor method
}
procedure TDataEntryControl.SetDataItemKey(Index: Integer; Value: Integer);
var
  i: Integer;
begin
  if Index < FDataEntryControlItemList.Count then
    FDataEntryControlItemList.Items[Index].DataItemKey := Value
  else begin
    for i := FDataEntryControlItemList.Count to Index do
      FDataEntryControlItemList.Add(TDataEntryControlItem.Create());
    FDataEntryControlItemList.Items[Index].DataItemKey := Value;
  end;
end;  // TDataEntryControl.SetDataItemKey
   
{-------------------------------------------------------------------------------
  Accessor method
}
procedure TDataEntryControl.SetDefaultDisplay(Index: Integer; const Value: string);
var
  i: Integer;
begin
  if Index < FDataEntryControlItemList.Count then
    FDataEntryControlItemList.Items[Index].DefaultDisplay := Value
  else begin
    for i := FDataEntryControlItemList.Count to Index do
      FDataEntryControlItemList.Add(TDataEntryControlItem.Create());
    FDataEntryControlItemList.Items[Index].DefaultDisplay := Value;
  end;
end;  // TDataEntryControl.SetDefaultDisplay

{-------------------------------------------------------------------------------
  Accessor method
}
procedure TDataEntryControl.SetDefaultValue(Index: Integer; const Value: string);
var
  i: Integer;
begin
  if Index < FDataEntryControlItemList.Count then
    FDataEntryControlItemList.Items[Index].DefaultValue := Value
  else begin
    for i := FDataEntryControlItemList.Count to Index do
      FDataEntryControlItemList.Add(TDataEntryControlItem.Create());
    FDataEntryControlItemList.Items[Index].DefaultValue := Value;
  end;
end;  // TDataEntryControl.SetDefaultValue

{-------------------------------------------------------------------------------
  Sets the data type indicator of the control, influencing its behaviour.
      SetupDataEntryControl must be called after this property has been set
}
procedure TDataEntryControl.SetDataType(const Value: Integer);
begin
  FDataType := Value;
end;  // TDataEntryControl.SetDataType 

{-------------------------------------------------------------------------------
  Accessor method 
}
procedure TDataEntryControl.SetDefaultWidth(Value: Integer);
begin
  FDefaultWidth := Value;
end;  // TDataEntryControl.SetDefaultWidth

{-------------------------------------------------------------------------------
  Sets the human-readable display on the control. 
}
procedure TDataEntryControl.SetDisplay(Index: Integer; const Value: string);
var
  i: Integer;
begin
  case DataType of
    CT_FREETEXT, CT_VAGUE_DATE, CT_REGISTRATION, CT_ACCESSION:
      TCustomEdit(Control).Text := Value;

    CT_CONCEPT:
      UpdateConceptText(Value);

    // Will only be called for Individuals and Geo Areas if the control
    // is a linked edit rather than a multi-value linked edit.
    CT_SPATIAL_REF, CT_TAXON, CT_LOCATION, CT_ORGANISATION, CT_STORE,
        CT_STAFF_RESPONSIBLE, CT_INDIVIDUAL, CT_GEO_AREA:
      TLinkedEdit(Control).Text := Value;

    CT_CONCEPT_IN_GROUP, CT_COLLECTION, CT_DEPARTMENT, CT_SURVEY,
    CT_SAMPLE_TYPE, CT_YES_NO, CT_RECORD_TYPE, CT_DETERMINER_ROLE, CT_INFERRED:
      with TLuxIDComboBox(Control) do begin
        i := IndexOf(Value);
        if i < 0 then begin
          if Assigned(OnPopulate) then OnPopulate(Control);
          i := IndexOf(Value);
        end;
        ItemIndex := i;
      end;
  end;
end;  // TDataEntryControl.SetDisplay

{-------------------------------------------------------------------------------
  Sets the field lookup key for use with Concept combo boxes.
}
procedure TDataEntryControl.SetFieldLookupKey(const Value: string);
begin
  FFieldLookupKey := Value;
end;  // TDataEntryControl.SetFieldLookupKey

{-------------------------------------------------------------------------------
  Sets a property of the control indicating which database field the control
      maps to.
}
procedure TDataEntryControl.SetFieldName(const Value: TFieldName);
begin
  FFieldName := Value;
end;  // TDataEntryControl.SetFieldName 

{-------------------------------------------------------------------------------
  Sets the human-readable display on the control.
}
procedure TDataEntryControl.SetOldDisplay(Index: Integer; const Value: string);
var
  i: Integer;
begin
  if Index < FDataEntryControlItemList.Count then
    FDataEntryControlItemList.Items[Index].OldDisplay := Value
  else begin
    for i := FDataEntryControlItemList.Count to Index do
      FDataEntryControlItemList.Add(TDataEntryControlItem.Create());
    FDataEntryControlItemList.Items[Index].OldDisplay := Value;
  end;
end;  // TDataEntryControl.SetOldDisplay

{-------------------------------------------------------------------------------
}
procedure TDataEntryControl.SetOldValue(Index: Integer; const Value: string);
var
  i: Integer;
begin
  if Index < FDataEntryControlItemList.Count then
    FDataEntryControlItemList.Items[Index].OldValue := Value
  else begin
    for i := FDataEntryControlItemList.Count to Index do
      FDataEntryControlItemList.Add(TDataEntryControlItem.Create());
    FDataEntryControlItemList.Items[Index].OldValue := Value;
  end;
end;  // TDataEntryControl.SetOldValue

{-------------------------------------------------------------------------------
  Initialises some properties which shouldn't be initialised until the control
      has got a parent.
}
procedure TDataEntryControl.SetParent(AParentWindow: TWinControl);
begin
  inherited;
  if Assigned(AParentWindow) then
  begin
    if Assigned(FOnSetParent) then
      FOnSetParent(Self);
  end;
end;  // TDataEntryControl.SetParent 

{-------------------------------------------------------------------------------
  Accessor method 
}
procedure TDataEntryControl.SetSubjectAreaKey(const NewValue: string);
begin
  FSubjectAreaKey := NewValue;
end;  // TDataEntryControl.SetSubjectAreaKey 

{-------------------------------------------------------------------------------
  Sets a property of the control indicating which database table the control
      maps to.
}
procedure TDataEntryControl.SetTableName(const Value: TTableName);
begin
  FTableName := Value;
end;  // TDataEntryControl.SetTableName

{-------------------------------------------------------------------------------
  Sets up the Control property of DataEntry controls depending on the data type
      and FieldLookup key, if appropriate.
  Multiple values are only allowed on the Quick Entry screen, not the Quick Entry
      Manager screen, hence the new parameter.
}
procedure TDataEntryControl.SetupDataEntryControl(AllowMultiValues: Boolean);
begin
  case DataType of
    CT_FREETEXT, CT_REGISTRATION, CT_ACCESSION : //free text
      begin
        if not Assigned(Control) then
          Control := TEdit.Create(Self);
        if DataType in [CT_REGISTRATION, CT_ACCESSION] then
          with TEdit(Control) do begin
            OnKeyDown := MacroKeyDown;
            Hint := ResStr_MacroHint;
            ShowHint := True;
          end;
      end;

    CT_VAGUE_DATE:
      if not Assigned(Control) then
        Control := TVagueDateEdit.Create(Self);

    CT_SPATIAL_REF:
      begin
        if not Assigned(Control) then begin
          Control := TSpatialRefLinkedEdit.Create(Self);
          // Spatial ref is always uppercase
          TSpatialRefLinkedEdit(Control).EditBox.CharCase := ecUpperCase;
        end;
        FTable := '';
      end;

    CT_TAXON:
      begin
        if not Assigned(Control) then
          Control := TLinkedEdit.Create(Self);
        FTable := TN_TAXON_LIST_ITEM;
      end;

    CT_CONCEPT:
      begin
        if AllowMultiValues then begin
          if not Assigned(Control) then
            Control := TMultiValueLinkedEdit.Create(Self);
          TMultiValueLinkedEdit(Control).DataType := CT_CONCEPT;
          TMultiValueLinkedEdit(Control).OnModify := MultivalueControlUpdated;
        end else begin
          if not Assigned(Control) then
            Control := TLinkedEdit.Create(Self);
        end;    // if AllowMultiValues
        FTable := TN_CONCEPT;
      end;

    CT_CONCEPT_IN_GROUP:  //concept combobox
      if not Assigned(Control) then
        Control := TConceptGroupComboBox.Create(Self);
      //Remember to register this concept combo box

    CT_COLLECTION:
      begin
        if not Assigned(Control) then
          Control := TLuxIDComboBox.Create(Self);
        TLuxIDComboBox(Control).OnPopulate := dmQuickEntry.FillCollectionCombo;
      end;

    CT_DEPARTMENT:
      begin
        if not Assigned(Control) then
          Control := TLuxIDComboBox.Create(Self);
        TLuxIDComboBox(Control).OnPopulate := dmQuickEntry.FillDepartmentCombo;
      end;

    CT_LOCATION:
      begin
        if not Assigned(Control) then
          Control := TLinkedEdit.Create(Self);
        FTable := TN_LOCATION;
      end;

    CT_SURVEY:
      begin
        if not Assigned(Control) then
          Control := TLuxIDComboBox.Create(Self);
        TLuxIDComboBox(Control).OnPopulate := dmQuickEntry.FillSurveyCombo;
      end;

    CT_INDIVIDUAL:
      begin
        if AllowMultiValues then begin
          if not Assigned(Control) then
            Control := TMultiValueLinkedEdit.Create(Self);
          TMultiValueLinkedEdit(Control).DataType := CT_INDIVIDUAL;
          TMultiValueLinkedEdit(Control).OnModify := MultivalueControlUpdated;
        end else begin
          if not Assigned(Control) then
            Control := TLinkedEdit.Create(Self);
        end;
        FTable := TN_INDIVIDUAL;
      end;

    CT_STAFF_RESPONSIBLE:
      begin
        if not Assigned(Control) then
          Control := TLinkedEdit.Create(Self);
        FTable := TN_INDIVIDUAL;
      end;

    CT_ORGANISATION:
      begin
        if not Assigned(Control) then
          Control := TLinkedEdit.Create(Self);
        FTable := TN_ORGANISATION;
      end;

    CT_SAMPLE_TYPE:
    begin
      if not Assigned(Control) then
        Control := TLuxIDComboBox.Create(Self);
      TLuxIDComboBox(Control).OnPopulate := dmQuickEntry.FillSampleTypeCombo;
    end;

    CT_STORE:
      begin
        if not Assigned(Control) then
          Control := TLinkedEdit.Create(Self);
        TLinkedEdit(Control).ShowButton := False;
        FTable := TN_STORE;
      end;

    CT_YES_NO:
      begin
        if not Assigned(Control) then
          Control := TLuxIDComboBox.Create(Self);
        TLuxIDComboBox(Control).OnPopulate :=  dmQuickEntry.FillYesNoCombo;
      end;

    CT_RECORD_TYPE:
      begin
        if not Assigned(Control) then
          Control := TLuxIDComboBox.Create(Self);
        // Control is filled dynamically depending on the determination type and domain
      end;

    CT_DETERMINER_ROLE:
      begin
        if not Assigned(Control) then
          Control := TLuxIDComboBox.Create(Self);
        TLuxIDComboBox(Control).OnPopulate := dmQuickEntry.FillDeterminerRoleCombo;
      end;

    CT_INFERRED:
      begin
        if not Assigned(Control) then
          Control := TLuxIDComboBox.Create(Self);
        TLuxIDComboBox(Control).OnPopulate := dmQuickEntry.FillInferredCombo;
      end;

    CT_GEO_AREA:
      begin
        if AllowMultiValues then begin
          if not Assigned(Control) then
            Control := TMultiValueLinkedEdit.Create(Self);
          TMultiValueLinkedEdit(Control).DataType := CT_GEO_AREA;
          TMultiValueLinkedEdit(Control).OnModify := MultivalueControlUpdated;
        end else begin
          if not Assigned(Control) then
            Control := TLinkedEdit.Create(Self);
        end;
        FTable := TN_CONCEPT;
      end;
  else
    if not Assigned(Control) then
      Control := TEdit.Create(Self);
  end;

  if FControl is TLinkedEdit then
    with TLinkedEdit(FControl) do
    begin
      // Return data doesn't work for stores
      if FDataType <> CT_STORE then
        OnGetData := LinkedEditGetData;
      // Find dialog doesn't work for spatial references
      if FDataType<>CT_SPATIAL_REF then
        OnFindData := LinkedEditFindData;
      ImageList := dmInterface.ilButtons;
      ImageIndex := 12;
    end
  else
  if FControl is TLuxIDComboBox then
    with TLuxIdComboBox(FControl) do
    begin
      HasNoSelectionItem := true;
      NoSelectionItemText := ResStr_NoSelection;
    end;
  PositionControl;
  Control.Visible := True;
end;  // TDataEntryControl.SetupDataEntryControl

{-------------------------------------------------------------------------------
  Sets the value property of the control with data useful to the database. 
}
procedure TDataEntryControl.SetValue(Index: Integer; const Value: string);
var
  liIndex: Integer;
begin
  Case DataType of
    CT_FREETEXT, CT_VAGUE_DATE, CT_REGISTRATION, CT_ACCESSION:
      TCustomEdit(Control).Text := Value;

    // Will only be called for Individuals and Geo Areas if the control
    // is a linked edit rather than a multi-value linked edit.
    CT_SPATIAL_REF, CT_TAXON, CT_CONCEPT, CT_LOCATION, CT_ORGANISATION,
    CT_STORE, CT_STAFF_RESPONSIBLE, CT_INDIVIDUAL, CT_GEO_AREA:
      TLinkedEdit(Control).Key := Value;

    CT_CONCEPT_IN_GROUP, CT_COLLECTION, CT_DEPARTMENT, CT_SURVEY,
    CT_SAMPLE_TYPE, CT_YES_NO, CT_RECORD_TYPE, CT_DETERMINER_ROLE, CT_INFERRED:
      with TLuxIDComboBox(Control) do
      begin
        liIndex := IDIndexOf(Value);
        if liIndex < 0 then
        begin
          If Assigned(OnPopulate) then OnPopulate(Control);
          liIndex := IDIndexOf(Value);
        end;
        ItemIndex := liIndex;
      end;
  end;
end;  // TDataEntryControl.SetValue

{-------------------------------------------------------------------------------
  Accessor
}
procedure TDataEntryControl.SetTimestamp(Index: Integer; const Value: TSQLSvrTimestamp);
var
  i: Integer;
begin
  if Index < FDataEntryControlItemList.Count then
    FDataEntryControlItemList.Items[Index].Timestamp := Value
  else begin
    for i := FDataEntryControlItemList.Count to Index do
      FDataEntryControlItemList.Add(TDataEntryControlItem.Create());
    FDataEntryControlItemList.Items[Index].Timestamp := Value;
  end;
end;

{-------------------------------------------------------------------------------
  Stores the value for a spatial reference- this is a structured string which
      described each required component of the spatial reference.
}
procedure TDataEntryControl.SetValueForSpatialRef(ALatLong: ILatLong; const
    ASystem, AQualifier: string);
begin
  with TStringList.Create do
    try
      Add(Display[0]);
      Add(ASystem);
      Add(FloatToStr(ALatLong.Latitude));
      Add(FloatToStr(ALatLong.Longitude));
      Add(AQualifier);
      Value[0] := CommaText;
    finally
      Free;
    end;
end;  // TDataEntryControl.SetValueForSpatialRef




{-------------------------------------------------------------------------------
  Handle the update of the data control when a taxon is dropped.  Includes
      validation of subject area.
}
procedure TDataEntryControl.TaxonDropped(const AKey: string);
begin
  if SubjectAreaKey <> '' then
    if VarToStr(dmGeneral.GetStoredProcOutputParam(
        'usp_SubjectArea_Select_ForTaxon_Get',
        ['@Key', AKey], '@Subject_Area_Key')) <> SubjectAreaKey then
    begin
      ShowInformation(ResStr_ItemNotInTemplateSubjectArea);
      Exit;
    end;
    UpdateTaxon(AKey);
end;  // TDataEntryControl.TaxonDropped

{-------------------------------------------------------------------------------
  Updates a concept control with text corresponding to the specified concept.
}
procedure TDataEntryControl.UpdateConcept(const AKey: string);
var
  lRecordset: _Recordset;
  lText: string;
begin
  lRecordset := dmGeneral.GetRecordset(
      'usp_Concept_GetItemNameAndGroup',
      ['@Key', AKey]);

  if not lRecordset.EOF then
  begin
    lText := lRecordset.Fields['Concept_Name'].Value;
    if AppSettings.ShowGroupInQuickEntry then
    begin
      lText := lText + ' - ' + lRecordset.Fields['Group_Name'].Value;
    end;

    FOriginalPlaintext := Plaintext(lText);
    FFormatted := lText;
    TMultiValueLinkedEdit(Control).SetDisplayAndValue(0, lText, AKey);
  end;
end;  // TDataEntryControl.UpdateConcept

{-------------------------------------------------------------------------------
  Updates a taxon control with text corresponding to the specified key.
}
procedure TDataEntryControl.UpdateTaxon(const AKey: string);
var
  lText: string;
begin
  lText := dmGeneral.GetStoredProcOutputParam(
      'usp_TaxonName_Get',
      ['@TaxonListItemKey', AKey],
      '@TaxonName');

  if AppSettings.ShowGroupInQuickEntry then
  begin
    lText := lText + ' - '
        + dmGeneral.GetStoredProcOutputParam(
            'usp_TaxonListItem_GetListName',
            ['@TaxonListItemKey', AKey],
            '@TaxonListName');
  end;

  TLinkedEdit(Control).Text := lText;
end;

{-------------------------------------------------------------------------------
  Updates a concept control with a newly retrieved formatted term.  Stores the
      plaintext.
}
procedure TDataEntryControl.UpdateConceptText(const AValue: string);
begin
  FOriginalPlaintext := Plaintext(AValue);
  FFormatted := AValue;
  TLinkedEdit(Control).Text := FOriginalPlaintext;
end;

{-------------------------------------------------------------------------------
  Writes the value entered in the control back to the database (in the
      QE_Data_Item table.)
}
procedure TDataEntryControl.UpdateDataItem(const ADataRowKey: Integer; AIndex: Integer);
var
  lstNewValue: string;
  lstNewDisplay: string;
  lTimestamp: OLEVariant;
  lDataItemKey: Integer;
  lRunMethod: Boolean;
begin
  lRunMethod := True;
  if (DataType in [CT_CONCEPT, CT_INDIVIDUAL, CT_GEO_AREA]) and
     (Control is TMultiValueLinkedEdit) then
  begin
    if (TMultiValueLinkedEdit(Control).PopupOpened) or (ItemCount > 1) then
      // If a MultiValueLinkedEdit has been through the multi value popup screen,
      // all the items have already been updated
      lRunMethod := False;
    if (ItemCount = 1) then
      // If there is only one item, it can be changed again without using the popup,
      // and if it is we want this method to be run.
      TMultiValueLinkedEdit(Control).PopupOpened := False;
  end;
  if lRunMethod then begin
    try
      // Ensure the record only exists if there is data to store
      if DataItemKey[AIndex] <> -1 then begin
        if (Value[AIndex] <> '') or (Display[AIndex] <> '') then
          dmQuickEntry.UpdateDataItem(
              DataItemKey[AIndex], Value[AIndex], Display[AIndex], Timestamp[AIndex])
        else begin
          dmQuickEntry.DeleteDataItem(DataItemKey[AIndex], Timestamp[AIndex]);
          DataItemKey[AIndex] := -1;
        end
      end else
      if (Value[AIndex] <> '') or (Display[AIndex] <> '') then begin
        dmQuickEntry.InsertDataItem(
            ADataRowKey, Value[AIndex], Display[AIndex], TemplateFieldKey, lDataItemKey, lTimestamp);
        DataItemKey[AIndex] := lDataItemKey;
      end;
    finally //update controls whether or not record has been updated
      if DataItemKey[AIndex] <> -1 then begin
        dmQuickEntry.RefreshDataItem(DataItemKey[AIndex], lstNewValue, lstNewDisplay, lTimestamp);
        if (DataType in [CT_CONCEPT, CT_INDIVIDUAL, CT_GEO_AREA]) and
           (Control is TMultiValueLinkedEdit) then
        begin
          TMultiValueLinkedEdit(Control).SetDisplayAndValue(AIndex, lstNewDisplay, lstNewValue);
          TMultiValueLinkedEdit(Control).TemplateFieldKey := TemplateFieldKey;
        end else begin
          Display[AIndex] := lstNewDisplay;
          Value[AIndex]   := lstNewValue;
        end;
        Timestamp[AIndex] := lTimestamp;
      end; // if
      OldDisplay[AIndex] := Display[AIndex];
      OldValue[AIndex]   := Value[AIndex];
    end;
  end;
end;  // TDataEntryControl.UpdateDataItem

{-------------------------------------------------------------------------------
  If a spatial reference has been entered by hand, validates that it's a real
      spatial reference.
}
procedure TDataEntryControl.ValidateSpatialRef;
var
  lLatLong: ILatLong;
begin
  if (Value[0] = '') and (Display[0]<> '')then begin
    //this means that the user has typed into the box
    //save as lat long
    lLatLong := dmGeneral.Recorder.RecorderFunctions.DecodeSpatialRef(Display[0]);
    if lLatLong.ErrorMsg <> '' then //invalid spatial ref
      raise EQuickEntryControls.Create(Resstr_InvalidSpatialRef);
    SetValueForSpatialRef(
        lLatLong,
        dmGeneral.Recorder.RecorderFunctions.IdentifySpatialRefSystem(Display[0]),
        'Original');
  end;
end;  // TDataEntryControl.ValidateSpatialRef

{-------------------------------------------------------------------------------
  Accessor for event handler when a macro number is required
}
procedure TDataEntryControl.SetOnGetMacroNumber(Value: TGetMacroNumberEvent);
begin
  FOnGetMacroNumber := Value;
end;

{-------------------------------------------------------------------------------

}
procedure TDataEntryControl.MultivalueControlUpdated(sender: TObject);
var
  i: Integer;
  lDataValue, lDataDisplay: String;
  lTimestamp: OleVariant;
begin
  if sender is TMultiValueLinkedEdit then
  begin
    with TMultiValueLinkedEdit(sender) do
    begin
      for i := 0 to DataItemKeys.Count - 1 do
      begin
        if (DataItemKeys[i] <> '') then
        begin
          dmQuickEntry.RefreshDataItem(
            StrToInt(DataItemKeys[i]),
            lDataValue,
            lDataDisplay,
            lTimestamp);
          SetDataItemKey(i, StrToInt(DataItemKeys[i]));
          SetTimestamp(i, lTimestamp);
        end;
      end;
    end;
    if Assigned(FOnUpdateMultivalue) then FOnUpdateMultivalue(Self);
  end;
end;

{-==============================================================================
    TSpecimenDataEntryControl
===============================================================================}
{-------------------------------------------------------------------------------
}
function TSpecimenDataEntryControl.GetDisplay(Index: Integer): string;
begin
  if Control.Parent = nil then
    ReplaceEditControl; //combo box stuff won't work unless it has parent
  Result := inherited GetDisplay(Index);
end;  // TSpecimenDataEntryControl.GetDisplay 

{-------------------------------------------------------------------------------
}
function TSpecimenDataEntryControl.GetValue(Index: Integer): string;
begin
  if Control.Parent = nil then
    ReplaceEditControl; //combo box stuff won't work unless it has parent
  Result := inherited GetValue(Index);
end;  // TSpecimenDataEntryControl.GetValue

{-------------------------------------------------------------------------------
}
procedure TSpecimenDataEntryControl.RegisterLinkedEditControl(ABaseDragFrame: TBaseDragFrame);
begin
  if Control.Parent = nil then
    ReplaceEditControl; //can't register if control has no parent
  inherited;
end;  // TSpecimenDataEntryControl.RegisterLinkedEditControl

{-------------------------------------------------------------------------------
  Sets the control back on the panel if it isn't on it.
}
procedure TSpecimenDataEntryControl.ReplaceEditControl;
begin
  Control.Parent  := Self;
  Control.Align   := alTop;
  Control.Visible := True;
  // Set standard control border as not on grid
  if Control is TCustomEdit then
    TCustomEditAccessor(Control).BorderStyle := bsSingle
  else
  if Control is TLinkedEdit then
    TLinkedEdit(Control).BorderStyle := bsSingle;
  if Assigned(FOnControlReplaced) then
    FOnControlReplaced(Self);
end;  // TSpecimenDataEntryControl.ReplaceEditControl

{-------------------------------------------------------------------------------
}
procedure TSpecimenDataEntryControl.SetDisplay(Index: Integer; const Value: string);
begin
  if Control.Parent = nil then
    ReplaceEditControl; //combo box stuff won't work unless it has parent
  inherited;
end;  // TSpecimenDataEntryControl.SetDisplay

{-------------------------------------------------------------------------------
}
procedure TSpecimenDataEntryControl.SetEnabled(Value: Boolean);
begin
  inherited;
  if Assigned(Control) then
    Control.Enabled := Value;
  FEnabled := Value;
end;  // TSpecimenDataEntryControl.SetEnabled

{-------------------------------------------------------------------------------
}
procedure TSpecimenDataEntryControl.SetLocked(Value: Boolean);
begin
  FLocked := Value;
  Enabled := not Value;
end;  // TSpecimenDataEntryControl.SetLocked

{-------------------------------------------------------------------------------
}
procedure TSpecimenDataEntryControl.SetValue(Index: Integer; const Value: string);
begin
  if Control.Parent = nil then
    ReplaceEditControl; //combo box stuff won't work unless it has parent
  inherited;
end;  // TSpecimenDataEntryControl.SetValue

{-==============================================================================
    TSpatialRefLinkedEdit
===============================================================================}
{-------------------------------------------------------------------------------
  Object initialisation.
}
constructor TSpatialRefLinkedEdit.Create(AOwner: TComponent);
begin
  inherited;
  FDropDownButton := TButton.Create(Self);
  with FDropDownButton do begin
    Parent     := Self;
    Height     := Self.Height;
    Width      := 16;
    ParentFont := False;
    Font.Name  := 'Marlett';  // MS font.
    Font.Style := [];
    Caption    := '6';  // Dropdown arrow
    Visible    := True;
    TabOrder   := 2;
    OnClick    := DropDownClick;
  end;
  FDropDownMenu := TPopupMenu.Create(Self);
  FDropDownMenu.Images := dmInterface.ilButtons;
end;  // TSpatialRefLinkedEdit.Create

{-------------------------------------------------------------------------------
  Resize ensures buttons stay in correct place.
}
procedure TSpatialRefLinkedEdit.DoResize;
var
  lBtnWidth: Integer;
begin
  // If Button not shown, shape and edit will be the length of control.
  if FButton.Visible then lBtnWidth := FButton.Width + FDropDownButton.Width
                     else lBtnWidth := 0;

  if FShape.Visible then begin
    Self.Height := FEdit.Height + 2;
    FShape.SetBounds(0, 0, Self.Width - lBtnWidth, Self.Height);
    FEdit.SetBounds(1, 1, FShape.Width - 2, FEdit.Height);
    FButton.Height := FShape.Height;
  end else begin
    Self.Height := FEdit.Height;
    FEdit.SetBounds(0, 0, Self.Width - lBtnWidth, FEdit.Height);
    FButton.Height := FEdit.Height;
  end;
  FDropDownButton.Height := FButton.Height;

  FButton.Top  := 0;
  FButton.Left := Self.Width - lBtnWidth;
  FDropDownButton.Left := Self.Width - FDropDownButton.Width;

  // Hide the button at design-time.
  if not FButton.Visible then begin
    FButton.Height := FButton.Height - 2;
    FButton.Top := 1;
    FButton.Left := FButton.Left - 1;
  end;

  Invalidate;
end;  // TSpatialRefLinkedEdit.DoResize

{-------------------------------------------------------------------------------
  Click handler for map menu item, sets up a return data link to the selected
      map.
}
procedure TSpatialRefLinkedEdit.DropDownClick(Sender: TObject);
var
  lPos: TPoint;
begin
  if Assigned(FDropDownMenu) then begin
    dmGeneral.UpdateMapMenu(Self, FDropDownMenu.Items, True, MenuItemClick);
    lPos := FButton.ClientToScreen(Point(0, FButton.Height));

    if Assigned(TDataEntryControl(Owner).XPMenu) then
      TDataEntryControl(Owner).XPMenu.InitComponent(FDropDownMenu);

    FDropDownMenu.Popup(lPos.X, lPos.Y);
  end;
end;  // TSpatialRefLinkedEdit.DropDownClick

{-------------------------------------------------------------------------------
  Click handler to set up a return data link to a specified map.
}
procedure TSpatialRefLinkedEdit.MenuItemClick(Sender: TObject);
var
  lIntf: IRequestor;
begin
  // The index of the map to use is stored in the Tag of the MenuItem.
  if (Sender is TMenuItem) and Assigned(Parent) then
    if Supports(Parent, IID_IRequestor, lIntf) then
      dmGeneral.Recorder.RequestData(lIntf, 'MAPPOINT' + IntToStr(TMenuItem(Sender).Tag));
end;  // TSpatialRefLinkedEdit.MenuItemClick

{-------------------------------------------------------------------------------
}
procedure TSpatialRefLinkedEdit.SetEditMode(const Value: TEditMode);
begin
  inherited;
  FDropDownButton.Enabled := Value = emEdit;
end;  // TSpatialRefLinkedEdit.SetEditMode

{-------------------------------------------------------------------------------
}
procedure TSpatialRefLinkedEdit.SetEnabled(Value: Boolean);
begin
  inherited;
  FDropDownButton.Enabled := Value;
end;  // TSpatialRefLinkedEdit.SetEnabled

{-------------------------------------------------------------------------------
  Drop down button hidden when main button hidden.
}
procedure TSpatialRefLinkedEdit.SetShowButton(Value: Boolean);
begin
  inherited;
  FDropDownButton.Visible := Value;
end;  // TSpatialRefLinkedEdit.SetShowButton

{-==============================================================================
    TMultiValueLinkedEdit
===============================================================================}
{-------------------------------------------------------------------------------
  Object initialisation.
}
constructor TMultiValueLinkedEdit.Create(AOwner: TComponent);
begin
  inherited;
  FAddButton := TImageListButton.Create(Self);
  with FAddButton do begin
    Parent     := Self;
    Height     := Self.Height;
    Width      := 23;
    ParentFont := False;
    Font.Name  := 'Marlett';  // MS font.
    Font.Style := [];
    Visible    := True;
    TabOrder   := 2;
    OnClick    := AddClick;
    ImageList  := dmInterface.ilButtons;
    ImageIndex := 0;
  end;
  FItemKeys := TStringList.Create;
  FDataItemKeys := TStringList.Create;
  FEdit.OnChange := EditChange;
  FEdit.OnClick := EditClick;
  FEdit.OnEnter := EditEnter;
  FEdit.OnKeydown := EditKeyDown;
  FEdit.OnKeyPress := EditKeyPressed;
  FPopupOpened := False;
end;  // TMultiValueLinkedEdit.Create

{-------------------------------------------------------------------------------
  Destructor
}
destructor TMultiValueLinkedEdit.Destroy;
begin
  FItemKeys.Free;
  FDataItemKeys.Free;
  inherited;
end;

{-------------------------------------------------------------------------------
  Resize ensures buttons stay in correct place.
}
procedure TMultiValueLinkedEdit.DoResize;
var
  lBtnWidth: Integer;
begin
  // If Button not shown, shape and edit will be the length of control.
  if FButton.Visible then lBtnWidth := FButton.Width + FAddButton.Width
                     else lBtnWidth := 0;

  if FShape.Visible then begin
    Self.Height := FEdit.Height + 2;
    FShape.SetBounds(0, 0, Self.Width - lBtnWidth, Self.Height);
    FEdit.SetBounds(1, 1, FShape.Width - 2, FEdit.Height);
    FButton.Height := FShape.Height;
  end else begin
    Self.Height := FEdit.Height;
    FEdit.SetBounds(0, 0, Self.Width - lBtnWidth, FEdit.Height);
    FButton.Height := FEdit.Height;
  end;
  FAddButton.Height := FButton.Height;

  FButton.Top  := 0;
  FButton.Left := Self.Width - lBtnWidth;
  FAddButton.Left := Self.Width - FAddButton.Width;

  // Hide the button at design-time.
  if not FButton.Visible then begin
    FButton.Height := FButton.Height - 2;
    FButton.Top := 1;
    FButton.Left := FButton.Left - 1;
  end;

  Invalidate;
end;  // TMultiValueLinkedEdit.DoResize 

{-------------------------------------------------------------------------------
  Event handler for the add button click event. Calls the QEMultiValuePopup
  dialog.
}
procedure TMultiValueLinkedEdit.AddClick(Sender: TObject);
var
  lDialog: TfrmQEMultiValuePopup;
  i: Integer;
  item: TIndividualItem;
begin
  lDialog := TfrmQEMultiValuePopup.Create(nil);
  with lDialog do
    try
      Caption := Format(ResStr_Add, [FieldName]);
      sgValues.Cells[0, 0] := FieldName;
      LoadData(TemplateFieldKey, DataRowKey, DataType);
      // If we have changed the data directly on the screen,
      // we must ensure the popup knows this.
      if FItemKeys.Count <= 1 then
        LoadData(FEdit.Text, Key);

      if ShowModal = mrOK then begin
        ClearItemList;
        FDataItemKeys.Clear;
        FPopupOpened := True;
        for i := 0 to IndividualList.Count - 1 do begin
          item := TIndividualItem(IndividualList.Items[i]);
          if not (item.Deleted or (item.DataValue = '')) then
            FItemKeys.Add(item.DataValue + '=' + item.DataDisplay);
            FDataItemKeys.Add(item.ItemKey);
        end;

        if FItemKeys.Count > 1 then begin
          Text := '<i>' + Format(ResStr_Items, [FItemKeys.Count]) + '</i>';
          FEdit.Font.Style := [fsItalic];
          FButton.Enabled := False;
        end else begin
          Text := ItemKeys.ValueFromIndex[0];
          FEdit.Font.Style := [];
          FButton.Enabled := Self.Enabled;
        end;

        if Assigned(FOnModify) then FOnModify(Self);
      end;
    finally
      Free;
    end;
end;

{-------------------------------------------------------------------------------
}
procedure TMultiValueLinkedEdit.ClearItemList;
begin
  FItemKeys.Clear;
end;

{-------------------------------------------------------------------------------
}
procedure TMultiValueLinkedEdit.DeleteSavedValues;
begin
  dmGeneral.RunDeleteStoredProc(
      'usp_QEDataItem_DeleteForMultiValues',
      ['@DataRowKey', DataRowKey,
       '@TemplateFieldKey', TemplateFieldKey]);
end;

{-------------------------------------------------------------------------------
}
procedure TMultiValueLinkedEdit.SetEditMode(const Value: TEditMode);
begin
  inherited;
  FAddButton.Enabled := Value = emEdit;
end;  // TMultiValueLinkedEdit.SetEditMode

{-------------------------------------------------------------------------------
}
procedure TMultiValueLinkedEdit.SetEnabled(Value: Boolean);
begin
  inherited;
  FAddButton.Enabled := Value;
end;  // TMultiValueLinkedEdit.SetEnabled

{-------------------------------------------------------------------------------
  Add button hidden when main button hidden.
}
procedure TMultiValueLinkedEdit.SetShowButton(Value: Boolean);
begin
  inherited;
  FAddButton.Visible := Value;
end;  // TMultiValueLinkedEdit.SetShowButton

{-------------------------------------------------------------------------------
}
procedure TMultivalueLinkedEdit.SetText(const Value: String);
begin
  inherited SetText(RemoveSubStrings(Value, ['<i>', '</i>']));
end;

{-------------------------------------------------------------------------------
}
procedure TMultiValueLinkedEdit.EditChange(Sender: TObject);
begin
  if FEdit.Modified then
  begin
    if ItemKeys.Count > 1 then if ItemKeys.Names[0] <> '' then
    begin
      ClearItemList;
      DeleteSavedValues;
    end;
    SetDisplayAndValue(0, FEdit.Text, '');
    PopupOpened := false;
  end;
  if Assigned(OnChange) then OnChange(Self);
end;  // TMultiValueLinkedEdit.EditChange

{-------------------------------------------------------------------------------
  If there is more than one value, clicking the textbox opens the multivalue popup
}
procedure TMultiValueLinkedEdit.EditClick(Sender: TObject);
begin
  if FItemKeys.Count > 1 then
    AddClick(Sender);
end;  // TMultiValueLinkedEdit.EditClick

{-------------------------------------------------------------------------------
  If there is more than one value, clicking the textbox opens the multivalue popup
}
procedure TMultiValueLinkedEdit.EditEnter(Sender: TObject);
begin
  if FItemKeys.Count > 1 then
    FButton.Enabled := False;
end;

{-------------------------------------------------------------------------------
  On pressing Insert, open the multivalue popup. If there is more than one value,
  we don't want any of the other keys to do anything. (We can't do this by disabling
  the control, as then the EditClick event wouldn't fire.)
}
procedure TMultiValueLinkedEdit.EditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if FItemKeys.Count <= 1 then begin
    if Assigned(OnKeyDown) then OnKeyDown(Sender, Key, Shift);
    if (Key=VK_F2) and Assigned(OnGetData) then
      OnGetData(Self);
  end;
  if Key = VK_INSERT then
    AddClick(Sender)
  else if FItemKeys.Count > 1 then
    Key := 0;
end;  // TMultiValueLinkedEdit.EditKeyDown

{-------------------------------------------------------------------------------
  As above, only does things if there are less than two values.
}
procedure TMultiValueLinkedEdit.EditKeyPressed(Sender: TObject; var Key: Char);
begin
  if FItemKeys.Count <= 1 then begin
    if Key in [#13, #27] then begin
      if (Key = #13) and Assigned(OnFindData) then OnFindData(Self);
      // Remember to clear it, or we get an annoying beep.
      Key := #0;
    end;
  end else begin
    Key := #0;
  end;
end;  // TLinkedEdit.EditKeyPressed

{-------------------------------------------------------------------------------
  Sets the display and values of this control. This is called in place of
  TDataEntryControl methods if the control is a MultiValueLinkedEdit.
}
procedure TMultiValueLinkedEdit.SetDisplayAndValue(AIndex: Integer; const ADisplay: String;
      const AValue: TKeyString);
var
  i: Integer;
begin
  if AIndex < FItemKeys.Count then
    FItemKeys[AIndex] := AValue + '=' + ADisplay
  else begin
    for i := FItemKeys.Count to AIndex - 1 do
      FItemKeys.Add('');
    FItemKeys.Add(AValue + '=' + ADisplay);
  end;

  if FItemKeys.Count <= 1 then begin
    Text := ADisplay;
    Key := AValue;
    FEdit.Font.Style := [];
    FButton.Enabled := Self.Enabled;
  end else begin
    Text := '<i>' + Format(ResStr_Items, [FItemKeys.Count]) + '</i>';
    FEdit.Font.Style := [fsItalic];
    FButton.Enabled := False;
  end;
end;  // TMultiValueLinkedEdit.SetDisplayAndValue

{-==============================================================================
    TDataEntryControlItem
===============================================================================}
{-------------------------------------------------------------------------------
  Constructor
}
constructor TDataEntryControlItem.Create;
begin
  FTimestamp := TVariantObject.Create('').OLEVariant;
  FDataItemKey := -1;
end;  // TDataEntryControlItem.Create;

{-------------------------------------------------------------------------------
  Destructor
}
destructor TDataEntryControlItem.Destroy;
begin
  inherited;
end;  // TDataEntryControlItem.Destroy

{-==============================================================================
    TDataEntryControlItem
===============================================================================}
{-------------------------------------------------------------------------------
  Constructor
}
constructor TDataEntryControlItemList.Create;
begin
  FItems := TObjectList.Create;
end;  // TDataEntryControlItemList.Create

{------------------------------------------------------------------------------
  Destructor
}
destructor TDataEntryControlItemList.Destroy;
begin
  FItems.Free;
  inherited;
end;  // TDataEntryControlItemList.Destroy

{-------------------------------------------------------------------------------
}
function TDataEntryControlItemList.GetCount: Integer;
begin
  Result := FItems.Count;
end;  // TDataEntryControlItemList.GetCount

{-------------------------------------------------------------------------------
}
function TDataEntryControlItemList.GetItem(iIndex: Integer): TDataEntryControlItem;
begin
  Result := FItems[iIndex] as TDataEntryControlItem;
end;  // TDataEntryControlItemList.GetItem

{-------------------------------------------------------------------------------
  Add an item to the list
}
procedure TDataEntryControlItemList.Add(AItem: TDataEntryControlItem);
begin
  FItems.Add(AItem);
end;  // TDataEntryControlItemList.Add
 
{-------------------------------------------------------------------------------
  Clear all items from the list
}
procedure TDataEntryControlItemList.Clear;
begin
  FItems.Clear;
end;  // TDataEntryControlItemList.Clear

end.
