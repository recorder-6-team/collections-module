{===============================================================================
  Unit:        QuickEntryFrame.pas

  Defines:     TfraQuickEntry

  Description: Implements the Quick Entry screen.

  Model:       QuickEntry.mpb

  Created:     August 2003
===============================================================================}

unit QuickEntryFrame;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ActiveX, AxCtrls, QuickEntry_TLB, StdVcl, Menus, ImgList, StdCtrls,
  ExtCtrls, Grids, ImageListButton, ComCtrls, VagueDateEdit, 
  Recorder2000_TLB, DataStringGrid, DataTypes, BaseDetailFrameUnit, RapTree,
  BaseDragFrameUnit, Dataclasses, QuickEntryDataControls, DSSDataTypes,
  ExceptionForm, QuickEntryInsertTables, ADODB, UserMessages, XPMenu, ComObj;

type
  EQuickEntryFrame = class(TExceptionPath)
  end;

  {-----------------------------------------------------------------------------
    Frame that is embedded onto the TQuickEntryForm class, allowing drag and
    drop behaviour to be inherited from TBaseDragDropFrame.
  }
  TfraQuickEntry = class(TBaseDetailFrame)
    btnAdd: TImageListButton;
    btnDiscardSession: TImageListButton;
    btnProcessSelected: TImageListButton;
    btnProcessSession: TImageListButton;
    btnRecAdd: TImageListButton;
    btnRecDel: TImageListButton;
    btnRecFirst: TImageListButton;
    btnRecLast: TImageListButton;
    btnRecNext: TImageListButton;
    btnRecPrev: TImageListButton;
    btnValidateAll: TImageListButton;
    chkAsForm: TCheckBox;
    chkValidated: TCheckBox;
    eSessionName: TEdit;
    ilRecords: TImageList;
    Label1: TLabel;
    lblRecordNumber: TLabel;
    lblValidated: TLabel;
    Panel1: TPanel;
    pcMain: TPageControl;
    pmGrid: TPopupMenu;
    pmGridFillDown: TMenuItem;
    pmGridFillUp: TMenuItem;
    pnlAdd: TPanel;
    pnlButtons: TPanel;
    pnlRecButtons: TPanel;
    pnlSpecimenControls: TPanel;
    sbGeneralControls: TScrollBox;
    sbSpecimenControls: TScrollBox;
    sgQuickEntry: TStringGrid;
    tsGeneral: TTabSheet;
    tsSpecimen: TTabSheet;
    pmAdd: TPopupMenu;
    pmAddMultiple: TMenuItem;
    pmAddFromFile: TMenuItem;
    sbHiddenControls: TScrollBox;
    lblLastUser: TLabel;
    eLastUser: TLabel;
    procedure btnAddClick(Sender: TObject);
    procedure btnDiscardSessionClick(Sender: TObject);
    procedure btnProcessSelectedClick(Sender: TObject);
    procedure btnProcessSessionClick(Sender:TObject);
    procedure btnRecAddClick(Sender: TObject);
    procedure btnRecDelClick(Sender: TObject);
    procedure btnRecFirstClick(Sender: TObject);
    procedure btnRecLastClick(Sender: TObject);
    procedure btnRecNextClick(Sender: TObject);
    procedure btnRecPrevClick(Sender: TObject);
    procedure btnValidateAllClick(Sender: TObject);
    procedure chkAsFormClick(Sender: TObject);
    procedure chkValidatedClick(Sender: TObject);
    procedure eSessionNameExit(Sender: TObject);
    procedure FrameResize(Sender: TObject);
    procedure pmAddFromFileClick(Sender: TObject);
    procedure pmAddMultipleClick(Sender: TObject);
    procedure pmGridFillDownClick(Sender: TObject);
    procedure pmGridFillUpClick(Sender: TObject);
    procedure sgQuickEntryExit(Sender: TObject);
  private
    FCurrentGridControl: TWinControl;
    FDataStringGrid: TMultiValueDataStringGrid;
    FDiscarded: Boolean;
    FGeneralEntryControls: TStringList;
    FGeneralRowKey: Integer;
    FHiddenEntryControls: TStringList;
    FInitialised: Boolean;
    FIsLifeSciences: Boolean;
    FStoreInitiallySupplied: Boolean;
    FLastUser : String;
    FNumberOfRecords: Integer;
    FRecordNumber: Integer;
    FRecordTypePopulatedDomainKey: string;
    FSessionKey: LongInt;
    FSessionName: string;
    FSessionTimestamp: TSQLSvrTimestamp;
    FSpecimenEntryControls: TStringList;
    FSubjectAreaKey: string;
    FTemplateKey: string;
    FTemplateName: string;
    FTemplateType: Byte;
    FtfchkValidatedDislocated: Boolean;
    FtfRowChanging: Boolean;
    FXPMenu: TXPMenu;
    FExitHandlerDisabled: Boolean;
    FLoading: Boolean;
    FHasRegistrationMacro: Boolean;
    FHasAccessionMacro: Boolean;
    FOccurrenceKey: string;
    FTaxonOccurrenceKey: string;
    // FGridValues is a copy  of the specimen string grid, but stores all the items
    // in a multi-valued cell while the grid displays 'n items'
    FGridValues: array of array of array of string;
    FStoreAlteredManually: TStringList;
    procedure AddFields;
    procedure AddGeneralEventHandler(AGeneralDataEntryControl: TGeneralDataEntryControl);
    procedure AddNewEntryControl(AParent: TWinControl; ABoundsRect: TRect;
        ACaption: String; ADataType: Integer; AFieldLookupKey: String;
        ATemplateFieldKey: String; ADefaultWidth: integer; ADataEntryControl:
        TDataEntryControl);
    procedure AddRow;
    procedure AddRows(ACount: integer);
    procedure AddSpecimenEventHandler(ASpecimenDataEntryControl:
        TSpecimenDataEntryControl);
    procedure CheckBoxClick(ACol, ARow : integer);
    procedure DiscardSession(Sender: TObject);
    procedure DisplayValidatedCheckbox(ARow: integer);
    procedure FillRegion(ATopRow, ABottomRow, ACol: integer);
    procedure FocusFirstItem;
    procedure GeneralEventHandlerChange(Sender: TObject);
    procedure GeneralEventHandlerExit(Sender: TObject);
    procedure GeneralStoreEventHandlerExit(Sender: TObject);
    function GetHiddenValue(AName: string; ARow: Integer; AItem: Integer): string;
    function GetTimestampValue(AName: string; ARow: Integer; AItem: Integer): TSQLSvrTimestamp;
    function GetRowProcessed(Index: Integer): Boolean;
    function GetRowValidated(ARow: Integer): Boolean;
    procedure GetSessionDetails;
    procedure GridDragOver(APoint: TPoint; const ATable, AFieldKey: string; var Accept: Boolean);
    procedure GridDrop(const Sender: TObject; const iFormat : integer;
        const iSourceData: TKeyList; const iTextStrings : TStringList;
        const iIsPasteOperation: boolean; var ioHandled : boolean);
    procedure InitDataEntryControl(ADataEntryControl: TDataEntryControl;
        ARecordset: _Recordset; AScrollbox: TScrollbox; var ARect: TRect);
    procedure InitMeasurementDataEntryControl(ADataEntryControl: TDataEntryControl;
        ARecordset: _Recordset);
    procedure InitMetadataDataEntryControl(ADataEntryControl: TDataEntryControl;
        ARecordset: _Recordset);
    procedure InitNumberDataEntryControl(ADataEntryControl: TDataEntryControl;
        ARecordset: _Recordset);
    procedure InitStandardDataEntryControl(ADataEntryControl: TDataEntryControl;
        ARecordset: _Recordset);
    procedure LoadGeneralDefaults(AControlList: TStringList);
    procedure LoadRow(ARow: integer);
    //procedure LocationEventHandlerChange(Sender: TObject);
    procedure LocationChanged(Sender: TObject; ASpecimenTab: Boolean);
    procedure GeneralLocationEventHandlerChange(Sender: TObject);
    procedure SpecimenLocationEventHandlerChange(Sender: TObject);
    procedure SpecimenStoreEventHandlerChange(Sender: TObject);
    procedure SpecimenStoreEventManualHandlerChange(Sender: TObject);
    procedure GeneralStoreEventHandlerChange(Sender: TObject);
    procedure GeneralStoreEventManualHandlerChange(Sender: TObject);
    procedure PopulateFields;
    procedure PopulateSessionItems;
    procedure PopulateWrapperWithGeneralOrHiddenFields(iWrapper: TInsertWrapper;
        AControlList: TStringList);
    procedure PopulateWrapperWithSpecimenFields(iWrapper: TInsertWrapper);
    procedure ProcessRegion(ATopRow, ABottomRow: integer; ASelectedOnly: Boolean);
    procedure ProcessRow(ARow: integer);
    procedure ReplaceChkValidated;
    procedure SaveRow(ARow: integer);
    procedure SelectControl(AControl: TWinControl);
    procedure SetHiddenValue(AName: string; ARow: Integer; AItem: Integer; const Value: string);
    procedure SetTimestampValue(AName: string; ARow: Integer; AItem: Integer; const Value: TSQLSvrTimestamp);
    procedure SetNavigatorButtonStates(ARow: integer);
    procedure SetNumberOfRecords(const Value: Integer);
    procedure SetRecordNumber(const Value: Integer);
    procedure SetRowProcessed(Index: Integer; Value: Boolean);
    procedure SetRowValidated(ARow: Integer; AValue: Boolean);
    procedure SetSessionKey(const Value: LongInt);
    procedure ShowAsForm;
    procedure ShowAsGrid;
    procedure SpecimenControlKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure SpecimenControlReplaced(Sender:TObject);
    procedure SpecimenEventHandlerChange(Sender: TObject);
    procedure SpecimenEventHandlerExit(Sender: TObject);
    procedure SpecimenStoreEventHandlerExit(Sender: TObject);
    procedure SpecimenMouseWheelDown(Sender: TObject; Shift: TShiftState;
        MousePos: TPoint; var Handled: Boolean);
    procedure SpecimenMouseWheelUp(Sender: TObject; Shift: TShiftState;
        MousePos: TPoint; var Handled: Boolean);
    procedure StringGridCanEditCell(var ioAccept: Boolean; ACol, ARow: Integer);
    function StringGridCellBGColour(ACol, ARow: integer; var AHandled: Boolean): TColor;
    function StringGridCellTextColour(ACol, ARow: integer; var AHandled: Boolean): TColor;
    procedure StringGridCustomEdit(Panel: TPanel; ACol, ARow: integer);
    procedure StringGridRepositionCustomEdit(Panel: TPanel; ACol, ARow: Integer);
    procedure StringGridRowChanged(Sender: TObject; ARow: integer);
    procedure TryToValidateRow(ARow: integer; AValidated: boolean);
    procedure UpdateDataEntryControlsState;
    procedure UpdateGeneralDataEntryControl(AControl: TGeneralDataEntryControl);
    function ValidData(ACol, ARow, AItem: Integer): Boolean;
    procedure ValidateRow(ARow: integer; iWrapper : TInsertWrapper;
        AFocusInvalidControl: boolean);
    property HiddenValue[AName: string; ARow: Integer; AItem:Integer]: string read
        GetHiddenValue write SetHiddenValue;
    property TimestampValue[AName: string; ARow: Integer; AItem: Integer]: TSQLSvrTimestamp
        read GetTimestampValue write SetTimestampValue;
    property NumberOfRecords: Integer read FNumberOfRecords write
        SetNumberOfRecords;
    property RecordNumber: Integer read FRecordNumber write SetRecordNumber;
    procedure UpdateDataItemAndSynch(AControl: TDataEntryControl; ARow: integer; AIndex: Integer);
    procedure GetMacroNumber(Sender: TObject; var ANumber: string);
    procedure GenerateMacroNumbersForHiddenFields(const ARowKey: String);
    function GetDepartmentName(AForSpecimenTab: boolean): string;
    procedure UpdateMacroNumber(AUpdatedControl: TDataEntryControl);
    function FindControl(ADataType: integer; AForSpecimenTab: boolean): TDataEntryControl;
    procedure RefreshAfterMultivalueEdit(sender: TObject);
  protected
    procedure CMChildKey(Var msg: TCMChildKey); message CM_CHILDKEY;
    procedure RegisterDragDropComponents; override;
    procedure WMValidateRow(var msg: TMessage); message WM_VALIDATEROW;
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
    procedure AddImportedRow(Values: array of Variant); virtual;
    procedure Initialise;
    procedure PopulateRecordTypeCombo(AControl: TDataEntryControl);
    procedure SetColWidth(ADataEntryControl: TDataEntryControl);
    procedure SetControlWidth(ADataEntryControl: TDataEntryControl);
    property LastUser : String read FLastUser;
    property RowProcessed[Index: Integer]: Boolean read GetRowProcessed write SetRowProcessed;
    property RowValidated[ARow: Integer]: Boolean read GetRowValidated write SetRowValidated;
    property SessionKey: LongInt read FSessionKey write SetSessionKey;
    property SessionName: string read FSessionName;
    property TemplateName: string read FTemplateName;
    property XPMenu: TXPMenu read FXPMenu write FXPMenu;
  end;

//==============================================================================
implementation

{$R *.dfm}

uses
  ComServ, QuickEntryData, GeneralData, Variants, StrUtils,
  AdoInt, ResourceStrings, ApplicationSettings, LinkedControls,
  InterfaceDataModule, Validation, LuxIDComboBox, Math, StdConvs,
  ConceptGroupComboBox, GeneralFunctions, Types, LuxembourgConstants, QEImport,
  VagueDate, BaseADODataModule, SpatialRefFuncs;

const
  CHAR_WIDTH_ALLOWANCE =   4;
  MAX_FIELD_WIDTH      = 350;

  CONTROL_MARGIN       =   8;
  CONTROL_SPACING      =   4;
  CONTROL_HEIGHT       =  23;
  VALIDATED_COLUMN     =   1;

  // Invisible Column Names
  DATA_ITEM_KEY = 'DataItemKey';
  DATA_ROW_KEY  = 'QE_Data_Row_Key';  // should match field name
  PROCESSED     = 'Processed';

  STANDARD_FIELD     = 0;
  CUSTOM_MEASUREMENT = 1;
  CUSTOM_NUMBER      = 2;
  CUSTOM_METADATA    = 3;
  
type
  {-----------------------------------------------------------------------------
    Accessor class for TStringGrid's protected members.
  }
  TStringGridAccessorQuickEntryFrame = class(TStringGrid)
  end;

{-==============================================================================
    TfraQuickEntry
===============================================================================}
{-------------------------------------------------------------------------------
  Object initialisation 
}
constructor TfraQuickEntry.Create(AOwner : TComponent);
begin
  inherited;
  btnProcessSelected.OnClick := btnProcessSelectedClick;
  btnProcessSession.OnClick  := btnProcessSessionClick;
  FSpecimenEntryControls     := TStringList.Create;
  FGeneralEntryControls      := TStringList.Create;
  FHiddenEntryControls       := TStringList.Create;
  FDiscarded                 := False;
  FIsLifeSciences            := False; // default unless we find taxon determination field
  pcMain.ActivePage          := tsGeneral;
  // ensure right anchored controls are correct
  pnlRecButtons.Left         := pnlSpecimenControls.Width - pnlRecButtons.Width;
  FExitHandlerDisabled       := False;
  FHasRegistrationMacro      := False;
  FHasAccessionMacro         := False;
  FStoreInitiallySupplied    := False;
end;  // TfraQuickEntry.Create

{-------------------------------------------------------------------------------
}
destructor TfraQuickEntry.Destroy;
var
  lIdx: Integer;
  lCtrl: TWinControl;

  procedure ClearControlList(AControlList: TStringList);
  begin
    if Assigned(AControlList) then
    begin
      //objects are owned by form so no need to free them
      AControlList.Clear;
      AControlList.Free;
    end;
  end;

begin
  // Ensure selected control posts its changes, but only if session is NOT discarded.
  if not FDiscarded and (Parent is TCustomForm) then
    if Assigned(TCustomForm(Parent).ActiveControl) then begin
      lCtrl := TCustomForm(Parent).ActiveControl;
      repeat
        if lCtrl is TGeneralDataEntryControl then
          UpdateGeneralDataEntryControl(TGeneralDataEntryControl(lCtrl));
        lCtrl := lCtrl.Parent;
      until not Assigned(lCtrl)
    end;
  UnregisterDragDropComponents;
  
  try
    if FInitialised and (not FDiscarded) then
      SaveRow(sgQuickEntry.Row);
  except on E:EUpdatedException do
    // Catch any errors to do with the save (e.g. Timestamp problems). Show
    // the error with a ShowInformation rather than reraising it with Raise
    // because we are in the destructor and it would go horribly wrong.
    ShowInformation(E.Message);
  end;

  ClearControlList(FSpecimenEntryControls);
  ClearControlList(FGeneralEntryControls);
  ClearControlList(FHiddenEntryControls);
  FDataStringGrid.Free;
  TdmGeneral.Discard;
  DiscardAppSettings;
  // Free objects before window handle lost
  for lIdx := ComponentCount-1 downto 0 do
    Components[lIdx].Free;
  inherited;
end;  // TfraQuickEntry.Destroy 

{-------------------------------------------------------------------------------
  Queries the database for the fields that are present in this template and
      puts a control on the form for each one.
}
procedure TfraQuickEntry.AddFields;
var
  lRS: _Recordset;
  lGeneralRect: TRect;
  lSpecimenRect: TRect;
  lHiddenRect: TRect;
  ltfOldNullStrictConvert: Boolean;
  lDataEntryControl: TDataEntryControl;
  lKey: String;


  procedure AddGeneralOrHiddenControl(AScrollBox: TScrollBox; var ARectangle: TRect;
    AControlList:TStringList; ARecordset: _Recordset);
  var
    lEnableControl: Boolean;
  begin 
    // Create new data entry control
    lDataEntryControl := TGeneralDataEntryControl.Create(Self);
    InitDataEntryControl(lDataEntryControl, ARecordset, AScrollBox, ARectangle);
    if lDataEntryControl.DataType in [CT_CONCEPT, CT_INDIVIDUAL, CT_GEO_AREA] then
    begin
      // For multi-valued controls, make it known that this is a default value,
      // and if there are values stored in the database this value should be replaced
      with TMultiValueLinkedEdit(lDataEntryControl.Control) do
      begin
        FieldName := lDataEntryControl.ColumnCaption;
        SetDisplayAndValue(
            0,
            VarToStr(ARecordset.Fields['Default_Display'].Value),
            VarToStr(ARecordset.Fields['Default_Value'].Value));
        TemplateFieldKey := lDataEntryControl.TemplateFieldKey;
      end;
    end else begin
      lDataEntryControl.Display[0] := VarToStr(ARecordset.Fields['Default_Display'].Value);
      lDataEntryControl.Value[0]   := VarToStr(ARecordset.Fields['Default_Value'].Value);
    end;

    lDataEntryControl.Visible := not ARecordset.Fields['Hidden'].Value;

    AddGeneralEventHandler(TGeneralDataEntryControl(lDataEntryControl));
    AControlList.AddObject(lKey, lDataEntryControl);

    lEnableControl :=  ARecordset.Fields['Locked'].Value
                or
                (
                  (
                    (not
                      (VarIsNull(ARecordset.Fields['Taxon_Occurrence_Key'].Value)
                      )
                    )
                    or
                    (not
                      (VarIsNull(ARecordset.Fields['Occurrence_Key'].Value)
                      )
                    )
                  )
                  and (ARecordset.Fields['Initialized_From_Occurrence'].Value)
                );
    lDataEntryControl.Control.Enabled := not lEnableControl;
  end;

begin
  lGeneralRect.Left    := CONTROL_MARGIN;
  lGeneralRect.Right   := sbGeneralControls.Width - CONTROL_MARGIN * 2;
  lGeneralRect.Top     := CONTROL_MARGIN;
  lGeneralRect.Bottom  := CONTROL_MARGIN + CONTROL_HEIGHT;
  lSpecimenRect        := lGeneralRect;
  lSpecimenRect.Top    := lSpecimenRect.Top + CONTROL_HEIGHT + CONTROL_SPACING;
  lSpecimenRect.Bottom := lSpecimenRect.Top + CONTROL_HEIGHT + CONTROL_SPACING;
  lHiddenRect          := lGeneralRect;


  with FDataStringGrid do
  begin
    AddColumn('Validated', ResStr_Validated, 30, adboolean, true, true);
    OnCheckBoxClick              := CheckBoxClick;
    CustomEdit[VALIDATED_COLUMN] := true;
    AddInvisibleCol(DATA_ROW_KEY);
    AddTimestampCol('RowTimestamp');
    AddInvisibleCol(PROCESSED);
  end;

  ltfOldNullStrictConvert := NullStrictConvert;
  NullStrictConvert       := false;

  lRS := dmGeneral.GetRecordset('usp_QETemplateField_Select_ForTemplate',
                                ['@QETemplateKey', FTemplateKey, '@TemplateType',
                                IfThen(FTemplateType = 0, 6, 1),
                                '@QESessionKey', FSessionKey]);
  FTaxonOccurrenceKey := lRS.Fields['Taxon_Occurrence_Key'].Value;
  FOccurrenceKey := lRS.Fields['Occurrence_Key'].Value;
  try
    with lRS do
      try
        while not Eof do begin
          // skip null fields that are not on the template
          if not VarIsNull(Fields['QE_Template_Field_Key'].Value) then begin
            lKey := Fields['QE_Template_Field_Key'].Value;

            if (Fields['Is_Custom'].Value = 0) then
              if Fields['Data_Type'].Value = CT_TAXON then
                FIsLifeSciences := True;

            if Fields['Specimen_Tab'].Value then
            begin
              // Add controls to the specimen tab
              lDataEntryControl := TSpecimenDataEntryControl.Create(Self);

              InitDataEntryControl(lDataEntryControl, lRS, sbSpecimenControls, lSpecimenRect);
              if lDataEntryControl.Control is TCustomEdit then
                TEdit(lDataEntryControl.Control).BorderStyle := bsNone
              else
              if lDataEntryControl.Control is TLinkedEdit then
                TLinkedEdit(lDataEntryControl.Control).BorderStyle := bsNone;

              with TSpecimenDataEntryControl(lDataEntryControl) do
                OnControlReplaced := SpecimenControlReplaced;

              // Also add columns to the specimen grid.
              with FDataStringGrid do
              begin
                AddColumn(StrUtils.Ifthen(VarToStr(Fields['Alternative_Name'].Value) = '',
                          Fields['Field'].Value, Fields['Alternative_Name'].Value),
                          lKey + 'Display', 100, advarchar, True, false);
                CustomEdit[sgQuickEntry.ColCount-1] := true;
                AddTimestampCol(lKey + 'Timestamp');
                AddInvisibleCol(lKey + 'Value');
                AddInvisibleCol(lKey + DATA_ITEM_KEY);
              end;

              if lDataEntryControl.DataType in [CT_CONCEPT, CT_INDIVIDUAL, CT_GEO_AREA] then begin
                // For multi-valued controls, make it known that this is a default value,
                // and if there are values stored in the database this value should be replaced
                with TMultiValueLinkedEdit(lDataEntryControl.Control) do begin
                  FieldName := lDataEntryControl.ColumnCaption;
                  SetDisplayAndValue(
                      0,
                      VarToStr(Fields['Default_Display'].Value),
                      VarToStr(Fields['Default_Value'].Value));
                  TemplateFieldKey := lDataEntryControl.TemplateFieldKey;
                end;
                lDataEntryControl.OnUpdateMultivalue := RefreshAfterMultivalueEdit;
              end else begin
                lDataEntryControl.Display[0] := VarToStr(Fields['Default_Display'].Value);
                lDataEntryControl.Value[0]   := VarToStr(Fields['Default_Value'].Value);
              end;

              TSpecimenDataEntryControl(lDataEntryControl).Locked := (Fields['Locked'].Value
                or
                (
                  (
                    (not
                      (VarIsNull(Fields['Taxon_Occurrence_Key'].Value)
                     )
                    )
                    or
                    (not
                     (VarIsNull(Fields['Occurrence_Key'].Value)
                      )
                    )
                  )
                  and (Fields['Initialized_From_Occurrence'].Value)
                )
              );
              TSpecimenDataEntryControl(lDataEntryControl).GridColumn := sgQuickEntry.ColCount - 1;
              //Add properties & events to the control so it can communicate with the grid.
              AddSpecimenEventHandler(TSpecimenDataEntryControl(lDataEntryControl));
              SetColWidth(lDataEntryControl);
              FSpecimenEntryControls.AddObject(lKey, lDataEntryControl);
            end;

            if Fields['General_Tab'].Value then
              AddGeneralOrHiddenControl(
                  sbGeneralControls,
                  lGeneralRect,
                  FGeneralEntryControls,
                  lRS);

            if Fields['Hidden'].Value then
              AddGeneralOrHiddenControl(
                  sbHiddenControls,
                  lHiddenRect,
                  FHiddenEntryControls,
                  lRS);
          end;
          MoveNext;
        end;
      finally
        Close;
      end;
  finally
    NullStrictCOnvert := ltfOldNullStrictConvert;
  end;
end;  // TfraQuickEntry.AddFields

{-------------------------------------------------------------------------------
  Adds an OnExit event handler to the controls appearing on the General tab, so
      that the values the user enter in them can be saved.
}
procedure TfraQuickEntry.AddGeneralEventHandler(AGeneralDataEntryControl: TGeneralDataEntryControl);
begin
  TWinControlAccessor(AGeneralDataEntryControl.Control).OnExit := GeneralEventHandlerExit;
  if AGeneralDataEntryControl.Control is TLinkedEdit then
  begin
    if AGeneralDataEntryControl.DataType = CT_LOCATION then
      TLinkedEdit(AGeneralDataEntryControl.Control).OnChange := GeneralLocationEventHandlerChange
    else if (AGeneralDataEntryControl.DataType = CT_STORE) then
      if (AGeneralDataEntryControl.FieldName = fnUsualContainerCollectionUnitKey) then
        TLinkedEdit(AGeneralDataEntryControl.Control).OnChange := GeneralStoreEventHandlerChange
      else if (AGeneralDataEntryControl.FieldName = fnCurrentContainerCollectionUnitKey) then
        TLinkedEdit(AGeneralDataEntryControl.Control).OnChange := GeneralStoreEventManualHandlerChange
      else
        TLinkedEdit(AGeneralDataEntryControl.Control).OnChange := GeneralEventHandlerChange
    else
      TLinkedEdit(AGeneralDataEntryControl.Control).OnChange := GeneralEventHandlerChange;
  end else
  if AGeneralDataEntryControl.Control is TLuxIDComboBox then
    TLuxIdComboBox(AGeneralDataEntryControl.Control).OnChange := GeneralEventHandlerChange
  else
  if AGeneralDataEntryControl.Control is TEdit then
    TEdit(AGeneralDataEntryControl.Control).OnChange := GeneralEventHandlerChange
  else
  if AGeneralDataEntryControl.Control is TVagueDateEdit then
    TVagueDateEdit(AGeneralDataEntryControl.Control).OnChange := GeneralEventHandlerChange;
end;  // TfraQuickEntry.AddGeneralEventHandler

{+------------------------------------------------------------------------------
  Add a row to the grid, and corresponding records to the database.
  
  The elements of Values occur in pairs. The first in each pair is an index
  into the list of specimen/occurrence fields in the template.  The second
  is the displayed text to be entered for that field.
}
procedure TfraQuickEntry.AddImportedRow(Values: array of Variant);
var
  I: Integer;
  J: Integer;
  lRow: Integer;
  lRowKey: String;
  lCtrl: TSpecimenDataEntryControl;
  lDisplay: String;
  lValue: Variant;

  function ConvertImportedValue(Imported: Variant; DataType: Integer): String;
  begin
    case DataType of
      CT_VAGUE_DATE:
        try
          // convert to canonical form
          Result := VagueDateToString(StringToVagueDate(Imported));
        except
          on EVagueDateError do Result := Imported;
        end;
    else
      Result := Imported;
    end;
  end;

begin
  FDataStringGrid.HideCustomEdit;
  // If first (and only) row is empty, re-use it first before adding more.
  lRow := sgQuickEntry.RowCount - 1;
  lRowKey := HiddenValue[DATA_ROW_KEY, lRow, 0];
  // Check if a new row is actually needed.
  if (sgQuickEntry.RowCount > 2) or FDataStringGrid.RowContainsData(1) then
  begin
    sgQuickEntry.RowCount := sgQuickEntry.RowCount + 1;
    lRow := sgQuickEntry.RowCount - 1;
    for i := 0 to Length(FGridValues) - 1 do
      if Length(FGridValues[i]) < sgQuickEntry.RowCount then
        SetLength(FGridValues[i], sgQuickEntry.RowCount);

    // add master record
    with dmGeneral.GetRecordset('usp_QEDataRow_Insert', ['@QESessionKey', SessionKey]) do
      if not Eof then
      begin
        lRowKey := Fields[DATA_ROW_KEY].Value;
        HiddenValue[DATA_ROW_KEY, lRow, 0] := lRowKey;
        TimestampValue['RowTimestamp', lRow, 0] := Fields['Timestamp'].Value;
        GenerateMacroNumbersForHiddenFields(lRowKey);
      end;
  end;
  // Ensure no selection on row.
  sgQuickEntry.Cells[0, lRow] := '0';

  // populate with imported data
  J := 0;
  for I := 0 to FSpecimenEntryControls.Count - 1 do
  begin
    lCtrl    := FSpecimenEntryControls.Objects[I] as TSpecimenDataEntryControl;
    lDisplay := lCtrl.DefaultDisplay[0];
    lValue   := lCtrl.DefaultValue[0];

    if J < High(Values) then
      if lCtrl.GridColumn - 2 = Values[J] then
      begin
        lDisplay             := ConvertImportedValue(Values[J + 1], lCtrl.DataType);
        lCtrl.DataItemKey[0] := -1;

        if lCtrl.DataType in [CT_CONCEPT, CT_INDIVIDUAL, CT_GEO_AREA] then
          TMultiValueLinkedEdit(lCtrl.Control).SetDisplayAndValue(0, lDisplay, lValue)
        else
          lCtrl.Display[0] := lDisplay;

        lCtrl.UpdateDataItem(StrToInt(lRowKey), 0);
        lValue := lCtrl.Value[0];
        HiddenValue[lCtrl.TemplateFieldKey + DATA_ITEM_KEY, lRow, 0] :=
            IfThen(lCtrl.DataItemKey[0] = -1, '', IntToStr(lCtrl.DataItemKey[0]));
        J := J + 2;
      end;

    sgQuickEntry.Cells[lCtrl.GridColumn, lRow] := lDisplay;
    if Length(FGridValues[lCtrl.GridColumn, lRow]) = 0 then
      SetLength(FGridValues[lCtrl.GridColumn, lRow], 1);

    FGridValues[lCtrl.GridColumn, lRow, 0] := lDisplay;
    HiddenValue[lCtrl.TemplateFieldKey + 'Value', lRow, 0] := lValue;
  end;

  NumberOfRecords := sgQuickEntry.RowCount - 1;
  LoadRow(sgQuickEntry.Row);
  SetNavigatorButtonStates(sgQuickEntry.Row);
end;  // TfraQuickEntry.AddImportedRow 

{-------------------------------------------------------------------------------
  Sets up some basic properties of a TDataEntryControl to be placed on the
      form.
}
procedure TfraQuickEntry.AddNewEntryControl(AParent: TWinControl; ABoundsRect: TRect;
    ACaption: String; ADataType: Integer; AFieldLookupKey: String;
    ATemplateFieldKey: String; ADefaultWidth: integer; ADataEntryControl: TDataEntryControl);
begin
  with ADataEntryControl do
  begin
    BevelKind  := bkNone;
    BevelInner := bvNone;
    BevelOuter := bvNone;
    // Setup the control's label
    with TLabel.Create(Self) do begin
      Caption := ACaption;
      Left    := lblValidated.Left;
      Top     := ABoundsrect.Top + 3;
      Parent  := AParent;
    end;
    ColumnCaption := ACaption;
    Left          := chkValidated.Left;
    Top           := ABoundsRect.Top;
    DataType      := ADataType;
    DefaultWidth  := ADefaultWidth;
    Parent        := AParent;
    SetControlWidth(ADataEntryControl);
    TemplateFieldKey := ATemplateFieldKey;
    // Propagate the XPMenu reference to the control so SpatialRefLinkedEdit can use it.
    XPMenu := Self.XPMenu;
    SetupDataEntryControl(True);
  end;//with ADataEntryControl
end;  // TfraQuickEntry.AddNewEntryControl

{-------------------------------------------------------------------------------
  Adds a new row to the grid
}
procedure TfraQuickEntry.AddRow;
var
  i, newIdx: Integer;
  rs: _Recordset;
  number: String;
  lRowKey: String;
begin
  //first, add new row to the grid
  sgQuickEntry.RowCount := sgQuickEntry.RowCount + 1;
  for i := 0 to Length(FGridValues) - 1 do
    if Length(FGridValues[i]) < sgQuickEntry.RowCount then
      SetLength(FGridValues[i], sgQuickEntry.RowCount);

  newIdx :=  sgQuickEntry.RowCount - 1;
  for i := 0 to FSpecimenEntryControls.Count - 1 do
    with TSpecimenDataEntryControl(FSpecimenEntryControls.Objects[i]) do
    begin
      if (DataType in [CT_REGISTRATION, CT_ACCESSION])
         and Locked
         and ((DefaultValue[0] = '')
           or ((DataType = CT_REGISTRATION) and FHasRegistrationMacro)
           or ((DataType = CT_ACCESSION) and FHasAccessionMacro)) then
      begin
        GetMacroNumber(TSpecimenDataEntryControl(FSpecimenEntryControls.Objects[i]), number);
        DefaultDisplay[0] := number;
        DefaultValue[0]   := number;
        if DataType = CT_REGISTRATION then
          FHasRegistrationMacro := True
        else
        if DataType = CT_ACCESSION then
          FHasAccessionMacro := True;
      end;
      sgQuickEntry.Cells[GridColumn, newIdx] := DefaultDisplay[0];
      if Length(FGridValues[GridColumn, newIdx]) = 0 then
        SetLength(FGridValues[GridColumn, newIdx], 1);
      FGridValues[GridColumn, newIdx, 0] := DefaultDisplay[0];
      HiddenValue[TemplateFieldKey + 'Value', newIdx, 0] := DefaultValue[0];
    end;

  //add a new row record to the database
  rs := dmGeneral.GetRecordset('usp_QEDataRow_Insert', ['@QESessionKey', SessionKey]);
  if not (rs.EOF and rs.BOF) then
  begin
    lRowKey := rs.Fields[DATA_ROW_KEY].Value;
    HiddenValue[DATA_ROW_KEY, newIdx, 0] := lRowKey;
    TimestampValue['RowTimestamp', newIdx, 0] := rs.Fields['Timestamp'].Value;
    GenerateMacroNumbersForHiddenFields(lRowKey);
  end;

  sgQuickEntry.Row := newIdx;
  NumberOfRecords  := sgQuickEntry.RowCount - 1;
end;  // TfraQuickEntry.AddRow

{-------------------------------------------------------------------------------
  Add a fixed number of empty rows to the grid. 
}
procedure TfraQuickEntry.AddRows(ACount: integer);
var
  lCursor: TCursor;
  lProgress: Integer;
  lIdx: Integer;
  lRecorder: IRecorderMainForm;
begin
  lRecorder := dmGeneral.Recorder.RecorderMainForm;
  lRecorder.StartProgressBar;
  lRecorder.StatusText := ResStr_AddingRows;
  lCursor := HourglassCursor;
  try
    // Get instance of Recorder main form for progress info
    lRecorder := dmGeneral.Recorder.RecorderMainForm;
    for lIdx := 0 to ACount -1 do
    begin
      lProgress := lIdx * 100 div ACount;
      // Update Recorder progress only when required
      if lRecorder.Progress <> lProgress then
        lRecorder.Progress := lProgress;
      AddRow;
    end;
  finally
    lRecorder.Progress := 0;
    lRecorder.StopProgressBar;
    lRecorder.StatusText := '';
    DefaultCursor(lCursor);
    SetNavigatorButtonStates(sgQuickEntry.Row);
    UpdateDataEntryControlsState;
  end;
end;  // TfraQuickEntry.AddRows 

{-------------------------------------------------------------------------------
  Adds an OnExit event handler to the a data entry control. 
}
procedure TfraQuickEntry.AddSpecimenEventHandler(ASpecimenDataEntryControl:
    TSpecimenDataEntryControl);
begin
  TWinControlAccessor(ASpecimenDataEntryControl.Control).OnExit := SpecimenEventHandlerExit;
  if ASpecimenDataEntryControl.Control is TLinkedEdit then
  begin
    if ASpecimenDataEntryControl.DataType = CT_LOCATION then
      TLinkedEdit(ASpecimenDataEntryControl.Control).OnChange := SpecimenLocationEventHandlerChange
    else if (ASpecimenDataEntryControl.DataType = CT_STORE) then
      if (ASpecimenDataEntryControl.FieldName = fnUsualContainerCollectionUnitKey) then
      begin
        TLinkedEdit(ASpecimenDataEntryControl.Control).OnChange := SpecimenStoreEventHandlerChange;
        TWinControlAccessor(ASpecimenDataEntryControl.Control).OnExit := SpecimenStoreEventHandlerExit;
      end
      else if (ASpecimenDataEntryControl.FieldName = fnCurrentContainerCollectionUnitKey) then
        TLinkedEdit(ASpecimenDataEntryControl.Control).OnChange := SpecimenStoreEventManualHandlerChange
      else
        TLinkedEdit(ASpecimenDataEntryControl.Control).OnChange := SpecimenEventHandlerChange
    else
      TLinkedEdit(ASpecimenDataEntryControl.Control).OnChange := SpecimenEventHandlerChange
  end else
  if ASpecimenDataEntryControl.Control is TLuxIDComboBox then
    TLuxIdComboBox(ASpecimenDataEntryControl.Control).OnChange := SpecimenEventHandlerChange
  else
  if ASpecimenDataEntryControl.Control is TEdit then
    TEdit(ASpecimenDataEntryControl.Control).OnChange := SpecimenEventHandlerChange
  else
  if ASpecimenDataEntryControl.Control is TVagueDateEdit then
    TVagueDateEdit(ASpecimenDataEntryControl.Control).OnChange := SpecimenEventHandlerChange;
end;  // TfraQuickEntry.AddSpecimenEventHandler

{-------------------------------------------------------------------------------
}
procedure TfraQuickEntry.btnAddClick(Sender: TObject);
var
  lPosPopup: TPoint;
begin
  lPosPopup := ClientToScreen(Point(
      pnlButtons.Left + pnlAdd.Left + btnAdd.Left,
      pnlButtons.Top + pnlAdd.Top + btnAdd.Top + btnAdd.Height));
  pmAdd.Popup(lPosPopup.X, lPosPopup.Y);
end;  // TfraQuickEntry.btnAddMultipleClick 

{-------------------------------------------------------------------------------
  Allow the user to discard data entered in the data entry session. 
}
procedure TfraQuickEntry.btnDiscardSessionClick(Sender: TObject);
begin
  if dmQuickEntry.SessionHasUnprocessedRecords(FSessionKey) then
    if MessageDlg(ResStr_ConfirmSessionDiscard, mtWarning, [mbYes, mbNo], 0) = mrNo then
      Abort;
  DiscardSession(Sender);
end;  // TfraQuickEntry.btnDiscardSessionClick

procedure TfraQuickEntry.DiscardSession(Sender: TObject);
var lCursor: TCursor;
begin
  lCursor := HourglassCursor;
  dmGeneral.Recorder.RecorderMainForm.StatusText := ResStr_DiscardingSession;
  try
    FDiscarded := True;
    dmQuickEntry.DeleteSession(FSessionKey, FSessionTimestamp);
    // Close the form as it is no longer valid.
    dmGeneral.Recorder.MenuOptionClick(ResStr_MnuFile + ';' + ResStr_MnuClose);
  finally
    dmGeneral.Recorder.RecorderMainForm.StatusText := '';
    DefaultCursor(lCursor);
  end;
end; // TfraQuickEntry.DiscardSession

{-------------------------------------------------------------------------------
  Move data in the selected rows into the main database. 
}
procedure TfraQuickEntry.btnProcessSelectedClick(Sender: TObject);
begin
  ProcessRegion(1, sgQuickEntry.RowCount - 1, True);
end;  // TfraQuickEntry.btnProcessSelectedClick 

{-------------------------------------------------------------------------------
  Moves all unprocessed data in the entry session into the main database. 
}
procedure TfraQuickEntry.btnProcessSessionClick(Sender:TObject);
begin
  ProcessRegion(1, sgQuickEntry.RowCount - 1, False);
end;  // TfraQuickEntry.btnProcessSessionClick

{-------------------------------------------------------------------------------
  Add a new record to the data entry session. 
}
procedure TfraQuickEntry.btnRecAddClick(Sender: TObject);
begin
  AddRow;
  sgQuickEntry.Row := sgQuickEntry.RowCount - 1;
  sgQuickEntry.Col := Min(2, sgQuickEntry.ColCount-1);
  // first field after validate checkbox.
  FocusFirstItem;
  UpdateDataEntryControlsState;
end;  // TfraQuickEntry.btnRecAddClick 

{-------------------------------------------------------------------------------
  Allow the user to delete the current row from the session. 
}
procedure TfraQuickEntry.btnRecDelClick(Sender: TObject);
var
  ltfWillBeDeleted: Boolean;
  lIdx: Integer;
  lDeleteTerm: string;
  lSelCount: Integer;
  lInitialSelectedRow: Integer;
  lCursor: TCursor;
  lRowsToDelete: TList;
  
  procedure DeleteGridRow(ARow: integer);
  begin
    dmGeneral.RunDeleteStoredProc(
        'usp_QEDataRow_Delete',
        ['@QEDataRowKey', HiddenValue[DATA_ROW_KEY, ARow, 0],
         '@Timestamp', TimestampValue['RowTimestamp', ARow, 0]]);
    // If deleting the last row, then add a row first to pick up defaults
    // as we never leave a grid with no rows
    if (ARow = sgQuickEntry.FixedRows) and
       (sgQuickEntry.RowCount = sgQuickEntry.FixedRows + 1) then
      AddRow;
    FDataStringGrid.DeleteRow(ARow);
  end;

begin
  // Count the selected items
  lSelCount := 0;
  if not chkAsForm.Checked then
    for lIdx := 0 to sgQuickEntry.RowCount-1 do
      if sgQuickEntry.Cells[0, lIdx]='1' then Inc(lSelCount);

  if FDataStringGrid.RowContainsData(sgQuickEntry.Row) or (lSelCount>1) then begin
    // Display warning if more than one row, or current row contains data
    // Message needs correct plural
    if lSelCount = 1 then
      lDeleteTerm := ResStr_Record
    else
      lDeleteTerm := ResStr_Records;

    ltfWillBeDeleted := MessageDlg(Format(ResStr_ConfirmRowDelete, [lDeleteTerm]),
                                   mtWarning, [mbYes,  mbNo], 0)= mrYes
  end else
    ltfWillBeDeleted := True;
  
  if ltfWillBeDeleted then
  begin
    lCursor := HourglassCursor;
    try
      if chkAsForm.Checked then begin
        DeleteGridRow(FRecordNumber);
        LoadRow(FRecordNumber);
      end else begin
        lInitialSelectedRow := sgQuickEntry.Row;
        if lSelCount = 0 then
          DeleteGridRow(lInitialSelectedRow)
        else begin
          lRowsToDelete := TList.Create;
          try
            // Delete selected rows, first identify the rows to delete as
            // deleting a row unsets the selection
            for lIdx := sgQuickEntry.RowCount - 1 downto 1 do
              if sgQuickEntry.Cells[0, lIdx] = '1' then
                lRowsToDelete.Add(Pointer(lIdx));
            // now do the deletion, we are already in reverse order
            for lIdx := 0 to lRowsToDelete.Count - 1 do
              DeleteGridRow(Integer(lRowsToDelete[lIdx]));
          finally
            lRowsToDelete.Free;
          end;
        end;
        sgQuickEntry.Invalidate;
      end;
      LoadRow(sgQuickEntry.Row);
      // Refresh the label and ensure current cell displays correctly
      FDataStringGrid.HideCustomEdit;
      sgQuickEntry.Col := 1;
      NumberOfRecords  := sgQuickEntry.RowCount - 1;
      StringGridRowChanged(nil, sgQuickEntry.Row);
    finally
      DefaultCursor(lCursor);
    end;
  end;
  UpdateDataEntryControlsState;
end;  // TfraQuickEntry.btnRecDelClick 

{-------------------------------------------------------------------------------
}
procedure TfraQuickEntry.btnRecFirstClick(Sender: TObject);
begin
  sgQuickEntry.Row := 1;
  UpdateDataEntryControlsState;
end;  // TfraQuickEntry.btnRecFirstClick

{-------------------------------------------------------------------------------
}
procedure TfraQuickEntry.btnRecLastClick(Sender: TObject);
begin
  sgQuickEntry.Row := sgQuickEntry.RowCount - 1;
  UpdateDataEntryControlsState;
end;  // TfraQuickEntry.btnRecLastClick

{-------------------------------------------------------------------------------
}
procedure TfraQuickEntry.btnRecNextClick(Sender: TObject);
begin
  sgQuickEntry.Row := sgQuickEntry.Row + 1;
  UpdateDataEntryControlsState;
end;  // TfraQuickEntry.btnRecNextClick

{-------------------------------------------------------------------------------
}
procedure TfraQuickEntry.btnRecPrevClick(Sender: TObject);
begin
  sgQuickEntry.Row := sgQuickEntry.Row - 1;
  UpdateDataEntryControlsState;
end;  // TfraQuickEntry.btnRecPrevClick 

{-------------------------------------------------------------------------------
}
procedure TfraQuickEntry.btnValidateAllClick(Sender: TObject);
var
  i, liUpdatedRecords, liDeletedRecords, liSuccessful, liDataRowKey: Integer;
  ltfValidated, ltfProcessed: Boolean;
  lTimestamp: TSQLSvrTimestamp;
  lstMessageString: string;
  lCursor: TCursor;
  lWrapper: TInsertWrapper;
  lFailedRows: TIntegerDynArray;
  lRecorder: IRecorderMainForm;
  lProgress: Integer;
begin
  lRecorder            := dmGeneral.Recorder.RecorderMainForm;
  lCursor              := HourglassCursor;
  FtfRowChanging       := True;
  lRecorder.StartProgressBar;
  lRecorder.StatusText := ResStr_ValidatingRows;
  lWrapper             := nil;
  try
    FDataStringGrid.HideCustomEdit;
    liUpdatedRecords := 0;
    liDeletedRecords := 0;
    liSuccessful     := 0;
    SetLength(lFailedRows, 0);
    lWrapper := TInsertWrapper.Create;
    i := 1;
    while i <= sgQuickEntry.RowCount -1 do
    begin
      //check if Validated & processed
      if not (RowValidated[i] or RowProcessed[i]) then
        with FDataStringGrid do
        begin
          try
            try
              ValidateRow(i, lWrapper, False);
              liDataRowKey := StrToInt(Trim(HiddenValue[DATA_ROW_KEY, i, 0]));
              try
                dmQuickEntry.UpdateDataRow(liDataRowKey, True, False,
                                           TimestampCol['RowTimestamp', i, 0]);
                liSuccessful := liSuccessful + 1;
              except
                on EUpdatedException do
                  Inc(liUpdatedRecords);
              end;
              dmQuickEntry.RefreshDataRow(liDataRowKey, ltfValidated,
                                          ltfProcessed, lTimestamp);
              RowProcessed[i] := ltfProcessed;
              RowValidated[i] := ltfValidated;
              TimestampCol['RowTimestamp', i, 0] := lTimestamp;
            except
              on EQuickEntryException do begin
                // Store the row to select later
                SetLength(lFailedRows, Length(lFailedRows) + 1);
                lFailedRows[High(lFailedRows)] := i;
              end;
            end; // try
            i := i + 1;
          except
            on EDeletedException do
            begin
              Inc(liDeletedRecords);
              DeleteRow(i);
            end;
          end;//try..except
        end // with FDataStringGrid
      else
        i := i + 1;
      lProgress := i * 100 div sgQuickEntry.RowCount-1;
      // Update Recorder progress only when required
      if lRecorder.Progress<>lProgress then
        lRecorder.Progress:=lProgress;
    end; // while

    if liSuccessful = 1 then
      lstMessageString := ResStr_ReportSuccessfulSingleValidated
    else
      lstMessageString := Format(ResStr_ReportSuccessfulNumberValidated, [liSuccessful]);

    if liUpdatedRecords = 1 then
      lstMessageString := lstMessageString + #10#13 + ResStr_ReportUpdatedSingleValidated
    else
    if liUpdatedRecords > 1 then
      lstMessageString := lstMessageString + #10#13 +
                          Format(ResStr_ReportUpdatedNumberValidated, [liUpdatedRecords]);

    if liDeletedRecords = 1 then
      lstMessageString := lstMessageString + #10#13 + ResStr_ReportDeletedSingleValidated
    else
    if liDeletedRecords > 1 then
      lstMessageString := lstMessageString + #10#13 +
                          Format(ResStr_ReportDeletedNumberValidated, [liDeletedRecords]);
  finally
    DefaultCursor(lCursor);
    FtfRowChanging := False;
    lRecorder.Progress := 0;
    lRecorder.StopProgressBar;
    lWrapper.Free;
    sgQuickEntry.Invalidate;
  end; // try

  if Length(lFailedRows) > 0 then
  begin
    // Select the bad rows and show a messsage
    for i := 0 to High(lFailedRows) do
      sgQuickEntry.Cells[0, lFailedRows[i]] := '1';
    ShowInformation(ResStr_SelectedRowsInvalid);
  end;
  MessageDlg(lstMessageString, mtInformation, [mbOK], 0);
  FDataStringGrid.HideCustomEdit;
end;  // TfraQuickEntry.btnValidateAllClick

{-------------------------------------------------------------------------------
  Toggle the display between grid mode and form mode.
}
procedure TfraQuickEntry.chkAsFormClick(Sender: TObject);
begin
  if chkAsForm.State = cbChecked then
    ShowAsForm
  else
    ShowAsGrid;

  AppSettings.QuickEntryShowAsForm := chkAsForm.Checked;
  {TODO: Controls should also accept drop when embedded on the grid}
  if FInitialised then begin
    UnRegisterDragDropComponents;
    RegisterDragDropComponents;
  end;
end;  // TfraQuickEntry.chkAsFormClick 

{-------------------------------------------------------------------------------
}
procedure TfraQuickEntry.chkValidatedClick(Sender: TObject);
begin
  if FtfRowChanging then Exit;
  try
    TryToValidateRow(sgQuickEntry.Row, chkValidated.Checked);
  finally
    chkValidated.Checked := RowValidated[sgQuickEntry.Row];
  end;
end;  // TfraQuickEntry.chkValidatedClick

{-------------------------------------------------------------------------------
  Where a key stroke originated from FCurrentGridControl and is a key with a
      special meaning, calls SpecimenControlKeyDown.
}
procedure TfraQuickEntry.CMChildKey(Var msg: TCMChildKey);
begin
  if Assigned(FCurrentGridControl) then
    if (msg.Sender = FCurrentGridControl) or
       (FCurrentGridControl.ContainsControl(msg.Sender)) then
    begin
      case msg.CharCode of
        VK_LEFT, VK_RIGHT, VK_UP, VK_Down:
          begin
            SpecimenControlKeyDown(FCurrentGridControl, msg.Charcode, []);
            If msg.CharCode = 0 then
              msg.Result := 1;
          end;
        192: //apostrophe key
          begin
            if GetAsyncKeyState(VK_Control)<> 0 then
            begin
              SpecimenControlKeyDown(FCurrentGridControl, msg.Charcode, [ssCtrl]);
              If msg.CharCode = 0 then
                msg.Result := 1;
            end;
          end;
      end;
    end;
end;  // TfraQuickEntry.CMChildKey 

{-------------------------------------------------------------------------------
  Displays the Validated checkbox when a grid cell is clicked on.  If the click
      was on the box, also toggles the box.
}
procedure TfraQuickEntry.DisplayValidatedCheckbox(ARow: integer);
begin
  chkValidated.Visible := True;
  pcMain.ActivePage    := tsSpecimen;
  chkValidated.Enabled := not RowProcessed[ARow];

  if chkValidated.Enabled then
    chkValidated.SetFocus;

  TWinControlAccessor(chkValidated).OnMouseWheelDown := SpecimenMouseWheelDown;
  TWinControlAccessor(chkValidated).OnMouseWheelUp   := SpecimenMouseWheelUp;
  chkValidated.OnKeyDown := SpecimenControlKeyDown;
  FCurrentGridControl    := chkValidated;
  FtfchkValidatedDislocated := True;
end;  // TfraQuickEntry.DisplayValidatedCheckbox

{-------------------------------------------------------------------------------
}
procedure TfraQuickEntry.eSessionNameExit(Sender: TObject);
begin
  if eSessionName.Text<> FSessionName then
  begin
    ValidateValue(eSessionName.Text <> '',
                  Format(ResStr_PleaseEnterAValueForThe, [ResStr_SessionName]), eSessionName);
    try
      dmQuickEntry.UpdateSessionName(FSessionKey, EsessionName.Text, FSessionTimestamp);
    finally
      GetSessionDetails;
    end;
  end;
end;  // TfraQuickEntry.eSessionNameExit

{-------------------------------------------------------------------------------
  Fills a range of rows in the current column with the same value as the
      current cell.
}
procedure TfraQuickEntry.FillRegion(ATopRow, ABottomRow, ACol: integer);
var
  i: Integer;
  lTimestamp: TSqlSvrTimestamp;
  lstNewValue, lstNewDisplay: string;
  lstValue, lstDisplay, lFieldKey: string;
  liDataItemKey: Integer;
  liGridColumn: Integer;
  lCursor: TCursor;
  lRecorder: IRecorderMainForm;
  lProgress: Integer;
  lstFieldName: string;
begin
  inherited;
  if FCurrentGridControl<> chkValidated then
  begin
    lCursor := HourglassCursor;
    try
      lstFieldName := LeftStr(FDataStringGrid.FieldName[ACol], 16);
      with TSpecimenDataEntryControl(FSpecimenEntryControls.Objects[
          FSpecimenEntryControls.IndexOf(lstFieldName)]) do
      begin
        lstValue     := Value[0];
        lstDisplay   := Display[0];
        lFieldKey    := TemplateFieldKey;
        liGridColumn := GridColumn;
      end;
      i := ATopRow;
      lRecorder := dmGeneral.Recorder.RecorderMainForm;
      lRecorder.StartProgressBar;
      lRecorder.StatusText := ResStr_FillingRows;

      while i <= ABottomRow do
      begin
        lProgress := i * 100 div ABottomRow;
        // Update Recorder progress only when required
        if lRecorder.Progress <> lProgress then begin
          lRecorder.Progress := lProgress;
          Application.ProcessMessages;
        end;

        if not RowProcessed[i] then
        begin
          if Trim(HiddenValue[lFieldKey + DATA_ITEM_KEY, i, 0]) <> '' then
            liDataItemKey := StrToInt(Trim(HiddenValue[lFieldKey + DATA_ITEM_KEY, i, 0]))
          else
            // doesn't yet exist
            liDataItemKey := -1;

          try
            // Update or insert the data item as required
            if liDataItemKey > -1 then begin
              dmQuickEntry.UpdateDataItem(liDataItemKey, lstValue, lstDisplay,
                                          TimestampValue[lFieldKey +
                                          'Timestamp', i, 0]);
              // Ensure timestamp updated
              dmQuickEntry.RefreshDataItem(liDataItemKey, lstNewValue,
                                           lstNewDisplay, lTimestamp);
            end
            else begin
              dmQuickEntry.InsertDataItem(StrToInt(HiddenValue[DATA_ROW_KEY, i, 0]),
                                          lstValue, lstDisplay, lFieldKey,
                                          liDataItemKey, lTimestamp);
              // Items will not have been changed yet!
              lstNewDisplay := lstDisplay;
              lstNewValue := lstValue;
              HiddenValue[lFieldKey + DATA_ITEM_KEY, i, 0] := IntToStr(liDataItemKey);
            end;
            TimestampValue[lFieldKey + 'Timestamp', i, 0] := lTimestamp;
            // Update the data grid with the data from the database
            sgQuickEntry.Cells[liGridColumn, i] := lstNewDisplay;
            FGridValues[liGridColumn, i, 0] := lstNewDisplay;
            HiddenValue[lFieldKey + 'Value', i, 0] := lstNewValue;
          except
            on EUpdatedException do
            begin
              dmQuickEntry.RefreshDataItem(liDataItemKey, lstNewValue,
                                           lstNewDisplay, lTimestamp);
              TimestampValue[lFieldKey + 'Timestamp', i, 0] := lTimestamp;
              HiddenValue[lFieldKey + 'Value', i, 0] := lstNewValue;
              sgQuickEntry.Cells[liGridColumn, i] := lstDisplay;
              FGridValues[liGridColumn, i, 0] := lstDisplay;
            end;
            on EDeletedException do
            begin
              //assume the whole row has been deleted
              FDataStringGrid.DeleteRow(i);
              i:= i - 1;
              ABottomRow := ABottomRow - 1;
            end;
          end;
        end;
        i := i + 1;
      end;
    finally
      DefaultCursor(lCursor);
      lRecorder.Progress := 0;
      lRecorder.StopProgressBar;
      lRecorder.StatusText := '';
    end;
  end;
end;  // TfraQuickEntry.FillRegion

{-------------------------------------------------------------------------------
  Focuses the first item on the Show as Form view.
}
procedure TfraQuickEntry.FocusFirstItem;
begin
  with FSpecimenEntryControls do
    if (Count > 0) and chkAsForm.Checked then
      if TSpecimenDataEntryControl(Objects[0]).Control.CanFocus then
        TSpecimenDataEntryControl(Objects[0]).Control.SetFocus;
end;  // TfraQuickEntry.FocusFirstItem 

{-------------------------------------------------------------------------------
  Because this is on a frame, anchoring doesn't position the controls correctly
      when Recorder modifies the screen size just after construction.
}
procedure TfraQuickEntry.FrameResize(Sender: TObject);
begin
  inherited;
  // Ensure scrollboxes anchor correctly.
  sbGeneralControls.Height  := tsGeneral.Height  -  4;
  sbGeneralControls.Width   := tsGeneral.Width   -  1;
  sbSpecimenControls.Height := tsSpecimen.Height - 52;
  sbSpecimenControls.Width  := tsSpecimen.Width  -  1;
end;  // TfraQuickEntry.FrameResize

{-------------------------------------------------------------------------------
  If a control is changed on the general tab, unvalidate all validated rows
}
procedure TfraQuickEntry.GeneralEventHandlerChange(Sender: TObject);
var
  j: Integer;
begin
  if not RowProcessed[sgQuickEntry.Row] then
  begin
    chkValidated.Checked := False;
  end;

  for j := 1 to sgQuickEntry.RowCount -1 do
    if not RowProcessed[j] then
      RowValidated[j] := False;
end;  // TfraQuickEntry.GeneralEventHandlerChange

{-------------------------------------------------------------------------------
  Saves the value in the data control to the database.
}
procedure TfraQuickEntry.GeneralEventHandlerExit(Sender: TObject);
begin
  UpdateGeneralDataEntryControl(TGeneralDataEntryControl(TWinControl(Sender).Owner));
end;  // TfraQuickEntry.GeneralEventHandlerExit

{-------------------------------------------------------------------------------
}
function TfraQuickEntry.GetHiddenValue(AName: string; ARow: Integer; AItem: Integer): string;
begin
  Result := FDataStringGrid.InvisibleColValue[AName, ARow, AItem];
end;  // TfraQuickEntry.GetHiddenValue

{-------------------------------------------------------------------------------
}
function TfraQuickEntry.GetTimestampValue(AName: string; ARow: Integer; AItem: Integer): TSQLSvrTimestamp;
begin
  Result := FDataStringGrid.TimestampCol[AName, ARow, AItem]
end;  // TfraQuickEntry.GetTimestampValue

{-------------------------------------------------------------------------------
  Accessor method 
}
function TfraQuickEntry.GetRowProcessed(Index: Integer): Boolean;
begin
  Result := CompareText(HiddenValue[PROCESSED, Index, 0], STR_TRUE) = 0;
end;  // TfraQuickEntry.GetRowProcessed

{-------------------------------------------------------------------------------
}
function TfraQuickEntry.GetRowValidated(ARow: Integer): Boolean;
begin
  Result := sgQuickEntry.Cells[VALIDATED_COLUMN, ARow] = STR_TRUE;
end;  // TfraQuickEntry.GetRowValidated

{-------------------------------------------------------------------------------
  Gets details of the current session from its key. 
}
procedure TfraQuickEntry.GetSessionDetails;
var
  lrs, lrs2: _Recordset;
begin
  lrs := dmGeneral.GetRecordset('usp_QESession_Select', ['@QESessionKey', SessionKey]);
  with lrs do
    if not (bof and eof) then
    begin
      FTemplateKey      := Fields['QE_Template_Key'].Value;
      FTemplateType     := Fields['Template_Type'].Value;
      FSessionName      := Fields['Session_Name'].Value;
      FTemplateName     := Fields['Template_Name'].Value;
      FSubjectAreaKey   := VarToStr(Fields['Subject_Area_Key'].Value);
      FSessionTimestamp := Fields['Timestamp'].Value;
    end;
  lrs2 := dmGeneral.GetRecordset('usp_QESessionLastUser_Select', ['@QESessionKey', SessionKey]);
  with lrs2 do
    if not (bof and eof) then
      FLastUser := Fields['User_Name'].Value;

end;  // TfraQuickEntry.GetSessionDetails

{-------------------------------------------------------------------------------
  Drag over event handler for the grid.  Works out if the column can accept the
      offered data so the pointer can be set correctly.
}
procedure TfraQuickEntry.GridDragOver(APoint: TPoint; const ATable, AFieldKey: string; var
    Accept: boolean);
var
  lCol: Integer;
  lRow: Integer;
  lstFieldName: string;
  lDataEntryControl: TDataEntryControl;
  lClientPoint: TPoint;
begin
  lClientPoint := sgQuickEntry.ScreenToClient(APoint);
  // Find cell the mouse is over
  sgQuickEntry.MouseToCell(lClientPoint.X, lClientPoint.Y, lCol, lRow);
  if (lCol > 1) and (lCol < sgQuickEntry.ColCount) and
     (lRow > 0) and (lRow < sgQuickEntry.RowCount) then
  begin
    // Find the data entry control
    lstFieldName := LeftStr(FDataStringGrid.FieldName[lCol], 16);
    with FSpecimenEntryControls do
      lDataEntryControl := TDataEntryControl(Objects[IndexOf(lstFieldName)]);
    if lDataEntryControl.Control is TLinkedEdit then
      // Check table supported by field, and row not processed
      Accept := (CompareText(lDataEntryControl.Table, ATable) = 0)
    else
      // Location name accepts thesaurus concepts
      Accept := (lDataEntryControl.FieldName = fnLocationName) and
                (CompareText(TN_CONCEPT, ATable) = 0);
    // can't drop to processed rows
    Accept := Accept and not RowProcessed[lRow];
  end else
    Accept := False;
end;  // TfraQuickEntry.GridDragOver

{-------------------------------------------------------------------------------
  Advanced drop event handler that handles dropping at a specific location on
      the grid.
}
procedure TfraQuickEntry.GridDrop(const Sender: TObject; const iFormat: integer;
    const iSourceData: TKeyList; const iTextStrings : TStringList;
    const iIsPasteOperation: boolean; var ioHandled : boolean);
var
  lCol: Integer;
  lRow: Integer;
  lstFieldName: string;
  lDataEntryControl: TDataEntryControl;
  lClientPoint: TPoint;
begin
  lDataEntryControl := nil;
  if iIsPasteOperation then begin
    if Assigned(FCurrentGridControl) then
      lDataEntryControl := TDataEntryControl(FCurrentGridControl);
  end
  else begin
    lClientPoint := sgQuickEntry.ScreenToClient(Mouse.CursorPos);
    // Find cell the mouse is over
    sgQuickEntry.MouseToCell(lClientPoint.X, lClientPoint.Y, lCol, lRow);
    if (lCol > 1) and (lCol<sgQuickEntry.ColCount) and
       (lRow > 0) and (lRow<sgQuickEntry.RowCount) then
    begin
      // Find the data entry control
      lstFieldName := LeftStr(FDataStringGrid.FieldName[lCol],16);
      with FSpecimenEntryControls do
        lDataEntryControl := TDataEntryControl(Objects[IndexOf(lstFieldName)]);
    end
  end;
  if Assigned(lDataEntryControl) then
    with lDataEntryControl do begin
      // Use the data entry control to do the job of getting the value and display
      if lDataEntryControl.Control is TLinkedEdit then
        LinkedEditDataDropped(Sender, iFormat, iSourceData, iTextStrings, ioHandled)
      else
        LocationNameDropped(Sender, iFormat, iSourceData, iTextStrings, ioHandled);
      sgQuickEntry.Cells[lCol, lRow] := Display[0];
      FGridValues[lCol, lRow, 0] := Display[0];
      HiddenValue[TemplateFieldKey + 'Value', lRow, 0] := Value[0];
    end;
end;  // TfraQuickEntry.GridDrop 

{-------------------------------------------------------------------------------
  Initialises the properties of a newly created data entry control from a
      supplied recordset.
}
procedure TfraQuickEntry.InitDataEntryControl(ADataEntryControl:TDataEntryControl;
    ARecordset: _Recordset; AScrollbox: TScrollbox; var ARect: TRect);
begin  
  with ARecordset do
    AddNewEntryControl(AScrollbox, ARect,
        StrUtils.Ifthen(VarToStr(Fields['Alternative_Name'].Value) = '',
                        Fields['Field'].Value,
                        Fields['Alternative_Name'].Value),
        Fields['Data_Type'].Value,
        Fields['Field_Lookup_Key'].Value,
        Fields['QE_Template_Field_Key'].Value,
        Fields['Default_Size'].Value,
        ADataEntryControl);

  ADataEntryControl.SubjectAreaKey := FSubjectAreaKey;
  if (ARecordset.Fields['Is_Custom'].Value = 0) then
    InitStandardDataEntryControl(ADataEntryControl, ARecordset)
  else
  if (ARecordset.Fields['Is_Custom'].Value = 1) then
    InitMeasurementDataEntryControl(ADataEntryControl, ARecordset)
  else
  if (ARecordset.Fields['Is_Custom'].Value = 2) then
    InitNumberDataEntryControl(ADataEntryControl, ARecordset)
  else
  if (ARecordset.Fields['Is_Custom'].Value = 3) then
    InitMetadataDataEntryControl(ADataEntryControl, ARecordset);

  with ADataEntryControl do
  begin
    DefaultDisplay[0] := ARecordset.Fields['Default_Display'].Value;
    DefaultValue[0]   := ARecordset.Fields['Default_Value'].Value;
    if DataType in [CT_REGISTRATION, CT_ACCESSION] then
      // event handler to obtain a macro generated number
      OnGetMacroNumber := GetMacroNumber;
    if (DataType = CT_STORE)
      and (FieldName = fnCurrentContainerCollectionUnitKey)
      and (DefaultDisplay[0]<>'') then
        FStoreInitiallySupplied := True;
  end;

  ARect.Top    := ARect.Top + CONTROL_SPACING + CONTROL_HEIGHT;
  ARect.Bottom := ARect.Bottom + CONTROL_SPACING + CONTROL_HEIGHT;
end;  // TfraQuickEntry.InitDataEntryControl

{-------------------------------------------------------------------------------
  Contains intialisation code. Called from parent when parent is first shown. 
}
procedure TfraQuickEntry.Initialise;
var
  lCursor: TCursor;

  procedure RemoveOldControls(AControlList: TStringList);
  var
    i: Integer;
  begin
    for i := AControlList.Count - 1 downto 0 do
    begin
      if AControlList = FSpecimenEntryControls then
        TSpecimenDataEntryControl(AControlList.Objects[i]).Free
      else
        TGeneralDataEntryControl(AControlList.Objects[i]).Free;
    end;
    AControlList.Clear;
  end;

  procedure SetStoreIntialState();
  var
    lStoreControl: TDataEntryControl;
    lOtherStoreControl: TDataEntryControl;
    i, j: Integer;
  begin
      for i := 0 to FGeneralEntryControls.Count - 1 do
        if (TDataEntryControl(FGeneralEntryControls.Objects[i]).DataType = CT_STORE)
          And (TDataEntryControl(FGeneralEntryControls.Objects[i]).FieldName = fnCurrentContainerCollectionUnitKey) then
        begin
          lOtherStoreControl := TDataEntryControl(FGeneralEntryControls.Objects[i]);
          if lOtherStoreControl.Display[0]<>'' then
            FStoreInitiallySupplied := True
          else
          begin
            for j := 0 to FGeneralEntryControls.Count - 1 do
            if (TDataEntryControl(FGeneralEntryControls.Objects[j]).DataType = CT_STORE)
              And (TDataEntryControl(FGeneralEntryControls.Objects[j]).FieldName = fnUsualContainerCollectionUnitKey) then
            begin
              lStoreControl := TDataEntryControl(FGeneralEntryControls.Objects[j]);
              lOtherStoreControl.Display[0] := lStoreControl.Display[0];
            end;
          end;
        end;
  end;

begin
  dmGeneral.Recorder.RecorderMainForm.StatusText := ResStr_LoadingQuickEntrySession;
  lCursor := HourglassCursor;
  try
    FInitialised := False;
    eSessionName.Text := FSessionName;
    eLastUser.Caption := FLastUser;
    case FTemplateType of
      0: tsSpecimen.Caption := ResStr_Occurrence;
      1: tsSpecimen.Caption := ResStr_Specimen;
    end;

    //set up grid
    if Assigned(FDataStringGrid) then
      FDataStringGrid.Free;
    // remove all the old controls
    RemoveOldControls(FGeneralEntryControls);
    RemoveOldControls(FSpecimenEntryControls);
    RemoveOldControls(FHiddenEntryControls);
  
    FDataStringGrid := TMultiValueDataStringGrid.Create(sgQuickEntry);
    FDataStringGrid.Indicator              := True;
    FDataStringGrid.OnCustomEditCell       := StringGridCustomEdit;
    FDataStringGrid.OnRowChanged           := StringGridRowChanged;
    FDataStringGrid.OnCanEditCell          := StringGridCanEditCell;
    FDataStringGrid.OnCellBGColour         := StringGridCellBGColour;
    FDataStringGrid.OnCellTextColour       := StringGridCellTextColour;
    FDataStringGrid.OnRepositionCustomEdit := StringGridRepositionCustomEdit;
    AddFields;                                                                
    SetLength(FGridValues, FDataStringGrid.ColCount, FDataStringGrid.RowCount);
    LoadGeneralDefaults(FGeneralEntryControls);
    LoadGeneralDefaults(FHiddenEntryControls);
    PopulateFields;
    SetStoreIntialState;
    FtfRowChanging := False;
    btnValidateAll.Enabled     := AppSettings.AllowQuickEntryProcessing;
    btnProcessSelected.Enabled := AppSettings.AllowQuickEntryProcessing;
    btnProcessSession.Enabled  := AppSettings.AllowQuickEntryProcessing;
    FtfchkValidatedDislocated  := False;
    chkAsForm.Checked          := AppSettings.QuickEntryShowAsForm;

    // Do this to fix up flat look of controls to match grid or form
    chkAsForm.Checked := not chkAsForm.Checked;
    chkAsForm.Checked := not chkAsForm.Checked;

    FInitialised := True;
    // For a new, empty session, delete button is disabled
    btnRecDel.Enabled := FDataStringGrid.RowContainsData(1) or (sgQuickEntry.RowCount > 2);
  finally
    dmGeneral.Recorder.RecorderMainForm.StatusText := '';
    DefaultCursor(lCursor);
  end;
end;  // TfraQuickEntry.Initialise 

{-------------------------------------------------------------------------------
  Initialises a new measurement data entry control according to the recordset.
      This includes setting up the linked fields.
  The control passed in should be a measurement control.
}
procedure TfraQuickEntry.InitMeasurementDataEntryControl(ADataEntryControl:
    TDataEntryControl; ARecordset: _Recordset);
begin
  with ADataEntryControl, ARecordset do
  begin
    IsCustom                       := CUSTOM_MEASUREMENT;
    MeasurementAccuracy            := Fields['Measurement_Accuracy'].Value;
    MeasurementAppliesTo           := Fields['Measurement_Applies_To'].Value;
    MeasurementDuration            := Fields['Measurement_Duration'].Value;
    MeasurementMethodConceptKey    := Fields['Measurement_Method_Concept_Key'].Value;
    MeasurementParameterConceptKey := Fields['Measurement_Parameter_Concept_Key'].Value;
    MeasurementUnitConceptKey      := Fields['Measurement_Unit_Concept_Key'].Value;
    MeasurementIsSpecimen          := Fields['Measurement_Is_Specimen'].Value;
    MeasurementIsTaxonData         := Fields['Measurement_Is_TaxonData'].Value;
    MeasurementTaxonQualifierKey   := Fields['Taxon_Measurement_Qualifier_Key'].Value;
    MeasurementTaxonUnitKey        := Fields['Taxon_Measurement_Unit_Key'].Value;
  end;
end;  // TfraQuickEntry.InitMeasurementDataEntryControl

{-------------------------------------------------------------------------------
  Initialises a new metdata data entry control according to the recordset.
      This includes setting up the linked fields.
  The control passed in should be a metadata control.
}
procedure TfraQuickEntry.InitMetadataDataEntryControl(ADataEntryControl:
    TDataEntryControl; ARecordset: _Recordset);
begin
  with ADataEntryControl, ARecordset do
  begin
    IsCustom        := CUSTOM_METADATA;
    MetadataTypeKey := Fields['Metadata_Type_Key'].Value;
  end;
end;  // TfraQuickEntry.InitMetadataDataEntryControl

{-------------------------------------------------------------------------------
}
procedure TfraQuickEntry.InitNumberDataEntryControl(ADataEntryControl:
    TDataEntryControl; ARecordset: _Recordset);
begin
  with ADataEntryControl, ARecordset do
  begin
    IsCustom        := CUSTOM_NUMBER;
    FieldName       := StringToFieldName('Number');
    TableName       := StringToTableName('Collection_Unit_Number');
    NumberTypeKey   := Fields['Number_Type_Concept_Key'].Value;
    NumberPreferred := Fields['Number_Preferred'].Value;
  end;
end;  // TfraQuickEntry.InitNumberDataEntryControl

{-------------------------------------------------------------------------------
  Initialises a new data entry control according to the recordset.  This
      includes setting up the linked fields, and registration of concept group
      combo boxes.
  The control passed in should not be a measurement control. 
}
procedure TfraQuickEntry.InitStandardDataEntryControl(ADataEntryControl: TDataEntryControl;
    ARecordset: _Recordset);
begin
  with ADataEntryControl do
  begin
    IsCustom  := STANDARD_FIELD;
    FieldName := StringToFieldName(ARecordset.Fields['Field_Name'].Value);
    TableName := StringToTableName(ARecordset.Fields['Table_Name'].Value);
    if Trim(ARecordset.Fields['Field_lookup_Key'].Value)<> '' then
    begin
      FieldLookupKey := ARecordset.Fields['Field_lookup_Key'].Value;
      RegisterConceptGroupComboBox(TConceptGroupComboBox(Control), FieldLookupKey);
    end;
  end;
end;  // TfraQuickEntry.InitStandardDataEntryControl

{-------------------------------------------------------------------------------
  Loads default values for the general fields from the database.
}
procedure TfraQuickEntry.LoadGeneralDefaults(AControlList: TStringList);
var
  ltfOldNullStrictConvert: Boolean;
  lIdx: Integer;
  i: Integer;
  lTemplateField: TKeyString;
  lPreviousField: TKeyString;
  lNumber: String;
  entryCtrl: TGeneralDataEntryControl;
begin
  ltfOldNullStrictConvert := NullStrictConvert;
  NullStrictConvert       := False;
  FLoading                := True;
  try
    FGeneralRowKey := dmGeneral.GetStoredProcOutputParam(
                          'usp_QEGeneralDataRowKey_Get_ForSession',
                          ['@Key', SessionKey], '@Output');
    for i := 0 to AControlList.Count - 1 do
    begin
      entryCtrl := TGeneralDataEntryControl(AControlList.Objects[i]);
      if entryCtrl.DataType in [CT_CONCEPT, CT_INDIVIDUAL, CT_GEO_AREA] then
        TMultiValueLinkedEdit(entryCtrl.Control).DataRowKey := FGeneralRowKey;
    end;

    with dmGeneral.GetRecordset('usp_QEDataItem_Select_ForRow', ['@Key', FGeneralRowKey]) do
    begin
      I := 0;
      lPreviousField := '';

      while not Eof do
      begin
        lTemplateField := Fields['QE_Template_Field_Key'].Value;
        if lPreviousField <> lTemplateField then I := 0;
        lIdx := AControlList.IndexOf(lTemplateField);

        // if field not found, it has been removed from the template
        if lIdx > -1 then
          with TGeneralDataEntryControl(AControlList.Objects[lIdx]) do
          begin
            if DataType in [CT_CONCEPT, CT_INDIVIDUAL, CT_GEO_AREA] then
            begin
              TMultiValueLinkedEdit(Control).TemplateFieldKey := TemplateFieldKey;
            end;

            DataItemKey[i]    := Fields['QE_Data_Item_Key'].Value;
            DefaultValue[i]   := Fields['Data_Value'].Value;
            OldValue[i]       := DefaultValue[i];
            DefaultDisplay[i] := Fields['Data_Display'].Value;
            Timestamp[i]      := Fields['Timestamp'].Value;
            OldDisplay[i]     := DefaultDisplay[i];
            if DataType in [CT_CONCEPT, CT_INDIVIDUAL, CT_GEO_AREA] then
            begin
              TMultiValueLinkedEdit(Control).SetDisplayAndValue(
                  i,
                  DefaultDisplay[i],
                  DefaultValue[i]);
            end else begin
              Value[i]   := DefaultValue[i];
              Display[i] := DefaultDisplay[i];
            end;
          end;
        MoveNext;
        lPreviousField := lTemplateField;
        Inc(i);
      end;
      Close;
    end;

    for i := 0 to AControlList.Count - 1 do
      with TGeneralDataEntryControl(AControlList.Objects[i]) do
      begin
        if (DataType in [CT_REGISTRATION, CT_ACCESSION])
           and (DefaultValue[0] = '')
           and (Value[0] = '')
           and ((not Control.Enabled) or (not Visible)) then
        begin
          GetMacroNumber(TGeneralDataEntryControl(AControlList.Objects[i]), lNumber);
          Display[0] := lNumber;
          Value[0]   := lNumber;
          UpdateDataItem(FGeneralRowKey, i);
        end;
      end;
  finally
    NullStrictConvert := ltfOldNullStrictConvert;
    FLoading := False;
  end;
end;  // TfraQuickEntry.LoadGeneralDefaults 

{-------------------------------------------------------------------------------
  Sets the displays and values of the specimen controls with their
      corresponding values in the string grid.
}
procedure TfraQuickEntry.LoadRow(ARow: integer);
var
  i, j: Integer;
  lText: string;
  multiValueChangeEvent: TNotifyEvent;
begin
  multiValueChangeEvent := nil;
  FtfRowChanging := True;
  try
    chkValidated.Checked := RowValidated[ARow];
  finally
    FtfRowChanging := False
  end;
  chkValidated.Enabled := not RowProcessed[ARow];
  lblValidated.Enabled := not RowProcessed[ARow];

  for i := 0 to FSpecimenEntryControls.Count - 1 do
  begin
    with TSpecimenDataEntryControl(FSpecimenEntryControls.Objects[i]) do
    begin
      Reset;
      if DataType in [CT_CONCEPT, CT_INDIVIDUAL, CT_GEO_AREA] then
      begin
        TMultiValueLinkedEdit(Control).DataRowKey := StrToInt(HiddenValue[DATA_ROW_KEY, ARow, 0]);
        // Moving to a new row, so clear all the values from the old row
        TMultiValueLinkedEdit(Control).ClearItemList;
        // Temporarily suspend the OnChange event for the MultiValueLinkedControl (VI 17616)
        multiValueChangeEvent := TMultiValueLinkedEdit(Control).OnChange;
        TMultiValueLinkedEdit(Control).OnChange := nil;
      end;

      try
        // For some reason the TStringSparseList used when Display is being set
        // to the value of the cells would return ' ' instead of ''. This meant
        // the contents of the Linked Edits sometimes had an extra ' ' in it.
        // This was confusing if you then typed text after the space, then pressed
        // return to get the find dialog - the find dialog wouldn't get anything.
        if Length(FGridValues[GridColumn, ARow]) = 0 then
        begin
          if DataType in [CT_CONCEPT, CT_INDIVIDUAL, CT_GEO_AREA] then
          begin
            TMultiValueLinkedEdit(Control).SetDisplayAndValue(0, DefaultDisplay[0], DefaultValue[0]);
            TMultiValueLinkedEdit(Control).TemplateFieldKey := TemplateFieldKey;
          end else begin
            Display[0] := DefaultDisplay[0];
            Value[0]   := DefaultValue[0];
          end;
          DataItemKey[0] := -1;
          OldValue[0]    := Value[0];
          OldDisplay[0]  := Display[0];
          SetLength(FGridValues[GridColumn, ARow], 1);
          FGridValues[GridColumn, ARow, 0] := Display[0];
        end else begin
          for j := 0 to Length(FGridValues[GridColumn, ARow]) - 1 do
          begin
            lText := FGridValues[GridColumn, ARow, j];
            if lText = ' ' then lText := '';

            if DataType in [CT_CONCEPT, CT_INDIVIDUAL, CT_GEO_AREA] then
            begin
              TMultiValueLinkedEdit(Control).SetDisplayAndValue(j, lText, HiddenValue[TemplateFieldKey + 'Value', ARow, j]);
              TMultiValueLinkedEdit(Control).TemplateFieldKey := TemplateFieldKey;
            end else begin
              Display[j] := lText;
              Value[j]   := HiddenValue[TemplateFieldKey + 'Value', ARow, j];
            end;

            if HiddenValue[TemplateFieldKey + DATA_ITEM_KEY, ARow, j] <> '' then
              DataItemKey[j] := StrToInt(HiddenValue[TemplateFieldKey + DATA_ITEM_KEY, ARow, j])
            else
              DataItemKey[j] := -1;

            OldValue[j]   := Value[j];
            OldDisplay[j] := Display[j];
            Timestamp[j]  := TimestampValue[TemplateFieldKey + 'Timestamp', ARow, j];
          end;
        end;
        Enabled := not RowProcessed[ARow] and not Locked;
      finally
        if DataType in [CT_CONCEPT, CT_INDIVIDUAL, CT_GEO_AREA] then
          TMultiValueLinkedEdit(Control).OnChange := multiValueChangeEvent;
      end;
    end;
  end;
end;  // TfraQuickEntry.LoadRow

{-------------------------------------------------------------------------------
}
procedure TfraQuickEntry.SpecimenLocationEventHandlerChange(Sender: TObject);
begin
  LocationChanged(Sender, True);
  RowValidated[sgQuickEntry.Row] := False;
end;

{-------------------------------------------------------------------------------
If data has not been manually altered in the auxillary field then calling this
method results in data in the auxillary field being overwritten with data in the
master field.
}
procedure TfraQuickEntry.SpecimenStoreEventHandlerChange(Sender: TObject);
var
  lStoreControl: TDataEntryControl;
  lOtherStoreControl: TSpecimenDataEntryControl;
  i: Integer;
begin
  lOtherStoreControl := nil;
  if RecordNumber > 0 then
    if FStoreAlteredManually[RecordNumber] = 'N' then
    begin
      lStoreControl := TDataEntryControl(TWinControl(Sender).Owner);
      for i := 0 to FSpecimenEntryControls.Count - 1 do
        if (TDataEntryControl(FSpecimenEntryControls.Objects[i]).DataType = CT_STORE)
          And (TDataEntryControl(FSpecimenEntryControls.Objects[i]) <> lStoreControl) then
        begin
          lOtherStoreControl := TSpecimenDataEntryControl(FSpecimenEntryControls.Objects[i]);
          break;
        end;

      if Assigned(lOtherStoreControl) then
        begin
            lOtherStoreControl.Display[0] := lStoreControl.Display[0];
        end;

    end;

  RowValidated[sgQuickEntry.Row] := False;
end;

{-------------------------------------------------------------------------------
If this is called as a result of data entered automatically then the values in
the two fields will match.  If this is not the case then a FStoreAlteredManually
flag is set indicating that the field has been manually altered.
}
procedure TfraQuickEntry.SpecimenStoreEventManualHandlerChange(Sender: TObject);
var
  lStoreControl: TDataEntryControl;
  lOtherStoreControl: TDataEntryControl;
  i: Integer;
begin
  lOtherStoreControl := nil;
  if RecordNumber > 0 then
    if FStoreAlteredManually[RecordNumber] = 'N' then
    begin
      lStoreControl := TDataEntryControl(TWinControl(Sender).Owner);
      for i := 0 to FSpecimenEntryControls.Count - 1 do
        if (TDataEntryControl(FSpecimenEntryControls.Objects[i]).DataType = CT_STORE)
          And (TDataEntryControl(FSpecimenEntryControls.Objects[i]) <> lStoreControl) then
        begin
          lOtherStoreControl := TDataEntryControl(FSpecimenEntryControls.Objects[i]);
          break;
        end;

      if Assigned(lOtherStoreControl) then
        begin
          if lStoreControl.Display[0]<>lOtherStoreControl.Display[0] then
            FStoreAlteredManually[RecordNumber] := 'Y';
        end;
    end;

  RowValidated[sgQuickEntry.Row] := False;
end;

{-------------------------------------------------------------------------------
}
procedure TfraQuickEntry.GeneralLocationEventHandlerChange(Sender: TObject);
var
  j: Integer;
begin
  LocationChanged(Sender, False);
  if not RowProcessed[sgQuickEntry.Row] then
  begin
    chkValidated.Checked := False;
  end;

  for j := 1 to sgQuickEntry.RowCount -1 do
    if not RowProcessed[j] then
      RowValidated[j] := False;
end;

{-------------------------------------------------------------------------------
  When a usual store control is exited fire additionally fire the exited event
  on the associated control.
}
procedure TfraQuickEntry.GeneralStoreEventHandlerExit(Sender: TObject);
var
  lStoreControl: TDataEntryControl;
  lOtherStoreControl: TDataEntryControl;
  i: Integer;
begin
  lOtherStoreControl := nil;
  GeneralEventHandlerExit(Sender);
  lStoreControl := TDataEntryControl(TWinControl(Sender).Owner);
  for i := 0 to FGeneralEntryControls.Count - 1 do
    if (TDataEntryControl(FGeneralEntryControls.Objects[i]).DataType = CT_STORE)
      And (TDataEntryControl(FGeneralEntryControls.Objects[i]) <> lStoreControl) then
    begin
      lOtherStoreControl := TDataEntryControl(FGeneralEntryControls.Objects[i]);
      break;
    end;

    if Assigned(lOtherStoreControl) then
       GeneralEventHandlerExit(lOtherStoreControl.Control);
end;

{-------------------------------------------------------------------------------
If data has not been manually altered in the auxillary field then calling this
method results in data in the auxillary field being overwritten with data in the
master field.
}
procedure TfraQuickEntry.GeneralStoreEventHandlerChange(Sender: TObject);
var
  lStoreControl: TDataEntryControl;
  lOtherStoreControl: TDataEntryControl;
  i, j: Integer;
begin
    lOtherStoreControl := nil;
    if FStoreAlteredManually[0] = 'N' then
    begin
      lStoreControl := TDataEntryControl(TWinControl(Sender).Owner);
      for i := 0 to FGeneralEntryControls.Count - 1 do
        if (TDataEntryControl(FGeneralEntryControls.Objects[i]).DataType = CT_STORE)
          And (TDataEntryControl(FGeneralEntryControls.Objects[i]) <> lStoreControl) then
        begin
          lOtherStoreControl := TDataEntryControl(FGeneralEntryControls.Objects[i]);
          break;
        end;

      if Assigned(lOtherStoreControl) then
        begin
            lOtherStoreControl.Display[0] := lStoreControl.Display[0];
        end;

    end;

  if not RowProcessed[sgQuickEntry.Row] then
  begin
    chkValidated.Checked := False;
  end;

  for j := 1 to sgQuickEntry.RowCount -1 do
    if not RowProcessed[j] then
end;

{-------------------------------------------------------------------------------
If this is called as a result of data entered automatically then the values in
the two fields will match.  If this is not the case then a FStoreAlteredManually
flag is set indicating that the field has been manually altered.
}
procedure TfraQuickEntry.GeneralStoreEventManualHandlerChange(Sender: TObject);
var
  lStoreControl: TDataEntryControl;
  lOtherStoreControl: TDataEntryControl;
  i,j: Integer;
begin
    lOtherStoreControl := nil;
    if FStoreAlteredManually[0] = 'N' then
    begin
      lStoreControl := TDataEntryControl(TWinControl(Sender).Owner);
      for i := 0 to FGeneralEntryControls.Count - 1 do
        if (TDataEntryControl(FGeneralEntryControls.Objects[i]).DataType = CT_STORE)
          And (TDataEntryControl(FGeneralEntryControls.Objects[i]) <> lStoreControl) then
        begin
          lOtherStoreControl := TDataEntryControl(FGeneralEntryControls.Objects[i]);
          break;
        end;

      if Assigned(lOtherStoreControl) then
        begin
          if lStoreControl.Display[0]<>lOtherStoreControl.Display[0] then
            FStoreAlteredManually[0] := 'Y';
        end;
    end;

  if not RowProcessed[sgQuickEntry.Row] then
  begin
    chkValidated.Checked := False;
  end;

  for j := 1 to sgQuickEntry.RowCount -1 do
    if not RowProcessed[j] then
end;

{-------------------------------------------------------------------------------
}
procedure TfraQuickEntry.LocationChanged(
    Sender: TObject;
    ASpecimenTab: Boolean);
var
  lLocationControl: TDataEntryControl;
  lSpatialRefControl: TDataEntryControl;
  lRecordset: _Recordset;
  lLatLong: TLatLong;
  lNewSpatialRef: String;

  procedure UpdateSpatialRef;
  begin
    if lSpatialRefControl.Value[0] = '' then
    begin
      lRecordset := dmGeneral.GetRecordset('usp_Location_Select', ['@Key', lLocationControl.Value[0]]);
      if (not lRecordset.EOF) then
      begin
        lLatLong.Lat  := lRecordset.Fields['Lat'].Value;
        lLatLong.Long := lRecordset.Fields['Long'].Value;
        lNewSpatialRef := SpatialRefFuncs.ConvertFromLatLong(
            lLatLong,
            dmGeneral.Recorder.SpatialRefSystem);

        if lNewSpatialRef <> lSpatialRefControl.Display[0] then
        begin
          if (lSpatialRefControl is TGeneralDataEntryControl) and
            (lLocationControl is TGeneralDataEntryControl) then
          begin
            lSpatialRefControl.Display[0] := lNewSpatialRef;
            UpdateGeneralDataEntryControl(TGeneralDataEntryControl(lSpatialRefControl));
          end
          else if (lSpatialRefControl is TSpecimenDataEntryControl) and
            (lLocationControl is TSpecimenDataEntryControl) then
          begin
            lSpatialRefControl.Display[0] := lNewSpatialRef;
            lSpatialRefControl.Timestamp[0] := NULL;
            lSpatialRefControl.UpdateDataItem(
                StrToInt(HiddenValue[DATA_ROW_KEY, sgQuickEntry.Row, 0]),
                0);
            sgQuickEntry.Cells[TSpecimenDataEntryControl(lSpatialRefControl).GridColumn, sgQuickEntry.Row] := lSpatialRefControl.Display[0];
          end;
        end;
      end;
    end;
  end;

begin
  if not FLoading then
  begin
    lLocationControl   := TDataEntryControl(
        TWinControl(Sender).Owner);
    lSpatialRefControl := FindControl(
        CT_SPATIAL_REF,
        ASpecimenTab);
    if Assigned(lSpatialRefControl) then UpdateSpatialRef;
  end;
end;

{-------------------------------------------------------------------------------
}
procedure TfraQuickEntry.pmAddFromFileClick(Sender: TObject);
var
  i, j, k: Integer;
begin
  with TdlgQEImport.Create(nil, Self) do
    try
      if ShowModal = mrOk then
        for i := 2 to sgQuickEntry.ColCount - 1 do
        begin
          if Length(FGridValues[i]) < sgQuickEntry.RowCount then
            SetLength(FGridValues[i], sgQuickEntry.RowCount);

          for j := 1 to sgQuickEntry.RowCount - 1 do
            for k := 0 to Length(FGridValues[i, j]) - 1 do
              if not ValidData(i, j, k) then
              begin
                MessageDlg(ResStr_InvalidImportedData, mtInformation, [mbOk], 0);
                Exit;
              end;
        end;
    finally
      Release;
    end;
end;  // TfraQuickEntry.pmAddFromFileClick 

{+------------------------------------------------------------------------------
  Displays an input box requesting the user to specify the number of records to
      add.  If the user clicks OK on this dialog, then adds the specified
      number of blank records to the card.
}
procedure TfraQuickEntry.pmAddMultipleClick(Sender: TObject);
var
  liNumber: Integer;
  lstNumber: string;
  
  const
    MAX_ROWS = 9999;
begin
  inherited;
  lstNumber := '5';
  if InputQuery(ResStr_NewRows, ResStr_AskForNumberOfRows, lstNumber) then
  begin
    if IsInt(lstNumber) then begin
      liNumber := StrToInt(lstNumber);
      if (liNumber>0) and (liNumber<=MAX_ROWS) then begin
        AddRows(liNumber);
        Exit;
      end;
    end;
    MessageDlg(Format(ResStr_AskForPositiveInteger, [1, MAX_ROWS]), mtInformation, [mbOK],0);
    // Repeat the input query until cancelled or a suitable response
    pmAddMultipleClick(Sender);
  end;
end;  // TfraQuickEntry.pmAddMultipleClick 

{-------------------------------------------------------------------------------
}
procedure TfraQuickEntry.pmGridFillDownClick(Sender: TObject);
begin
  inherited;
  if (sgQuickEntry.Col > 1) and (sgQuickEntry.Col < sgQuickEntry.ColCount) and
     (sgQuickEntry.Row > 0) and (sgQuickEntry.Row < sgQuickEntry.RowCount) then
    FillRegion(sgQuickEntry.Row + 1, sgQuickEntry.RowCount - 1, sgQuickEntry.Col);
end;  // TfraQuickEntry.pmGridFillDownClick

{-------------------------------------------------------------------------------
}
procedure TfraQuickEntry.pmGridFillUpClick(Sender: TObject);
begin
  inherited;
  if (sgQuickEntry.Col > 1) and (sgQuickEntry.Col < sgQuickEntry.ColCount) and
     (sgQuickEntry.Row > 0) and (sgQuickEntry.Row < sgQuickEntry.RowCount) then
    FillRegion(1, sgQuickEntry.Row - 1, sgQuickEntry.Col);
end;  // TfraQuickEntry.pmGridFillUpClick

{-------------------------------------------------------------------------------
  Populates the Fields stringgrid.
}
procedure TfraQuickEntry.PopulateFields;
var
  i: Integer;
  lRow: Integer;
begin
  // First read the generic row information
  with dmGeneral.GetRecordset('usp_QEDataRow_Select_ForSession', ['@Key', FSessionKey]) do
  begin
    sgQuickEntry.RowCount := Max(2, RecordCount+1);
    lRow := 1;
    while not EOF do
    begin
      HiddenValue[DATA_ROW_KEY, lRow, 0] := Fields[DATA_ROW_KEY].Value;
      TimestampValue['RowTimestamp', lRow, 0] := Fields['Timestamp'].Value;
      RowValidated[lRow] := Fields['Validated'].Value;
      RowProcessed[lRow] := Fields['Processed'].Value;
      MoveNext;
      Inc(lRow);
    end;
    if RecordCount = 0 then begin
      // There is always one row in the grid, ensure it has default data
      AddRow;
      FDataStringGrid.DeleteRow(1);
    end else
      PopulateSessionItems;
  end;
  NumberOfRecords := sgQuickEntry.RowCount -1;
  i := 1;
  StringGridRowChanged(nil,i);
end;  // TfraQuickEntry.PopulateFields

{-------------------------------------------------------------------------------
  Populates the record type combo according to the domain, or for life sciences
      from the term list.
}
procedure TfraQuickEntry.PopulateRecordTypeCombo(AControl: TDataEntryControl);
var
  lWrapper: TInsertWrapper;
  lRecordset: _Recordset;
  lCombo: TLuxIDComboBox;
begin
  lCombo := TLuxIDComboBox(TSpecimenDataEntryControl(AControl).Control);
  if FIsLifeSciences and (not lCombo.Populated) then
    lCombo.OnPopulate := dmQuickEntry.FillRecordTypeCombo
  else begin
    // Use the insert wrapper to find the concept key
    lWrapper:= TInsertWrapper.Create;
    PopulateWrapperWithGeneralOrHiddenFields(lWrapper, FGeneralEntryControls);
    PopulateWrapperWithGeneralOrHiddenFields(lWrapper, FHiddenEntryControls);
    PopulateWrapperWithSpecimenFields(lWrapper);
    lRecordset := dmGeneral.GetRecordset('usp_DomainAndConceptGroup_Select_ForConcept',
                              ['@ConceptKey', lWrapper.Field[tnDetermination, fnConceptKey, 0]]);
    if not lRecordset.Eof then
      if FRecordTypePopulatedDomainKey <> lRecordset.Fields['Domain_Key'].Value then begin
        FRecordTypePopulatedDomainKey := lRecordset.Fields['Domain_Key'].Value;
        // Earth sciences, ensure combo populated from correct domain
        lCombo.Clear;
        with dmGeneral.GetRecordset('usp_RecordTypeConceptGroup_Select_ForDomain',
                                    ['@Key', FRecordTypePopulatedDomainKey]) do
          while not Eof do begin
            lCombo.Add(
                Fields['Concept_Key'].Value,
                string(Fields['Plaintext'].Value));
            MoveNext;
          end;
      end;
  end; //
end;  // TfraQuickEntry.PopulateRecordTypeCombo

{-------------------------------------------------------------------------------
  Fills the grid content with the data selected for an existing session. 
}
procedure TfraQuickEntry.PopulateSessionItems;
var
  lRow: Integer;
  lCol: Integer;
  lItem: Integer;
  lItemCount: Integer;
  i: Integer;
  lCellFound: Boolean;
  lTemplateFieldKey, lRowKey: String;
begin
  // Set start position in grid
  lRow := 1;
  lItem := 0;
  with dmGeneral.GetRecordset('usp_QEDataItem_Select_ForSession', ['@Key', FSessionKey]) do
  begin
    while not EOF do begin
      lTemplateFieldKey := Fields['QE_Template_Field_Key'].Value;
      lRowKey := Fields['QE_Data_Row_Key'].Value;
      lItemCount := dmGeneral.GetStoredProcOutputParam('usp_QEDataItem_NumberOfValues',
                                  ['@TemplateFieldKey', lTemplateFieldKey,
                                  '@DataRowKey', lRowKey], '@Count');
      // if a row exists but has no data, it is not in the dataset.  Find the next
      // row that has data
      while StrToInt(HiddenValue[DATA_ROW_KEY, lRow, 0]) <> Fields[DATA_ROW_KEY].Value do begin
        Inc(lRow);
        for lCol := 0 to Length(FGridValues)-1 do
          if (lRow >= Length(FGridValues[lCol])) then
            SetLength(FGridValues[lCol], sgQuickEntry.RowCount);
      end;
      lCellFound := False;
      // Store the display value
      for lCol := 2 to sgQuickEntry.ColCount-1 do
        if FDataStringGrid.FieldName[lCol] = lTemplateFieldKey + 'Display' then begin
          SetLength(FGridValues[lCol, lRow], lItemCount);
          for i := 0 to lItemCount - 1 do begin
            if FGridValues[lCol, lRow, i] = '' then begin
              FGridValues[lCol, lRow, i] := VarToStr(Fields['Data_Display'].Value);
              lItem := i;
              break;
            end;
          end;
          if Length(FGridValues[lCol, lRow]) > 1 then
            sgQuickEntry.Cells[lCol, lRow] := '<i>' + IntToStr(Length(FGridValues[lCol, lRow])) + ' items</i>'
          else
            sgQuickEntry.Cells[lCol, lRow] := VarToStr(Fields['Data_Display'].Value);
          // Set flag to indicate field not deleted from template
          lCellFound := True;
        end;
      if lCellFound then begin
        // Store the hidden value, timestamp etc
        lCol := FDataStringGrid.InvisibleColIndex[lTemplateFieldKey + 'Value'];
        SetLength(FGridValues[sgQuickEntry.ColCount + lCol, lRow], lItemCount);
        HiddenValue[lTemplateFieldKey + 'Value', lRow, lItem] := VarToStr(Fields['Data_Value'].Value);

        TimestampValue[lTemplateFieldKey + 'Timestamp', lRow, lItem] := Fields['Timestamp'].Value;

        lCol := FDataStringGrid.InvisibleColIndex[lTemplateFieldKey + DATA_ITEM_KEY];
        SetLength(FGridValues[sgQuickEntry.ColCount + lCol, lRow], lItemCount);
        HiddenValue[lTemplateFieldKey + DATA_ITEM_KEY, lRow, lItem] := Fields['QE_Data_Item_Key'].Value;
      end;
      MoveNext;
    end;
  end;
end;  // TfraQuickEntry.PopulateSessionItems

{-------------------------------------------------------------------------------
  Loads the Insertwrapper with the data on the general or hidden tab prior to inserting
      or validating a row.
}
procedure TfraQuickEntry.PopulateWrapperWithGeneralOrHiddenFields(iWrapper: TInsertWrapper;
    AControlList: TStringList);
var
  i, j: Integer;
  lCtrl: TDataEntryControl;
begin
  for i := 0 to AControlList.Count - 1 do
  begin
    lCtrl := TDataEntryControl(AControlList.Objects[i]);
    with lCtrl do begin
      for j := 0 to ItemCount - 1 do
      begin
        if (Value[j] = '') and (Display[j] <> '') then
          try
            ForceValue(j);
            UpdateGeneralDataEntryControl(TGeneralDataEntryControl(lCtrl));
          except
            on EQuickEntryControls do;
             // ignore error if user cancels - leave field unpopulated
          end;
        if Value[j] <> '' then
          case IsCustom of
            CUSTOM_MEASUREMENT:
              iWrapper.AddMeasurement(
                  MeasurementAppliesTo,
                  MeasurementMethodConceptKey,
                  MeasurementDuration,
                  MeasurementUnitConceptKey,
                  MeasurementParameterConceptKey,
                  Value[j],
                  MeasurementAccuracy,
                  TemplateFieldKey,
                  MeasurementIsSpecimen,
                  MeasurementIsTaxonData,
                  MeasurementTaxonQualifierKey,
                  MeasurementTaxonUnitKey);
            CUSTOM_NUMBER:
              iWrapper.AddNumber(NumberTypeKey, NumberPreferred, Value[j]);
            CUSTOM_METADATA:
              iWrapper.AddMetadata(MetadataTypeKey, Value[j]);
          else
            if (DataType = CT_CONCEPT) and (Control is TMultiValueLinkedEdit) then
              iWrapper.AddDeterminations(FieldName, j, TMultiValueLinkedEdit(Control).ItemKeys)
            else
              iWrapper.Field[TableName, FieldName, j] := Value[j];
          end;
      end;
    end;
  end;



end;  // TfraQuickEntry.PopulateWrapperWithGeneralOrHiddenFields

{-------------------------------------------------------------------------------
  Populates an insert wrapper with values in the specimen controls prior to
      processing or validating a row.
}
procedure TfraQuickEntry.PopulateWrapperWithSpecimenFields(iWrapper: TInsertWrapper);
var
  i, j: Integer;
  hiddenCtrl: TGeneralDataEntryControl;
  specimenCtrl: TSpecimenDataEntryControl;
begin
  for i := 0 to FSpecimenEntryControls.Count -1 do
  begin
    specimenCtrl := TSpecimenDataEntryControl(FSpecimenEntryControls.Objects[i]);
    with specimenCtrl do begin
      for j := 0 to ItemCount - 1 do
      begin
        if (Value[j] = '') and (Display[j] <> '') then
        begin
          ForceValue(j);
          FGridValues[i + 2, sgQuickEntry.Row, j] := Display[j];
          if Value[j] <> '' then
            UpdateDataItemAndSynch(specimenCtrl, sgQuickEntry.Row, j);
        end;

        if Value[j] <> '' then
          case IsCustom of
            CUSTOM_MEASUREMENT:
              iWrapper.AddMeasurement(
                  MeasurementAppliesTo,
                  MeasurementMethodConceptKey,
                  MeasurementDuration,
                  MeasurementUnitConceptKey,
                  MeasurementParameterConceptKey,
                  Value[j],
                  MeasurementAccuracy,
                  TemplateFieldKey,
                  MeasurementIsSpecimen,
                  MeasurementIsTaxonData,
                  MeasurementTaxonQualifierKey,
                  MeasurementTaxonUnitKey);
            CUSTOM_NUMBER:
              iWrapper.AddNumber(NumberTypeKey, NumberPreferred, Value[j]);
            CUSTOM_METADATA:
              iWrapper.AddMetadata(MetadataTypeKey, Value[j]);
          else
            if (DataType = CT_CONCEPT) and (Control is TMultiValueLinkedEdit) then
              iWrapper.AddDeterminations(FieldName, j, TMultiValueLinkedEdit(Control).ItemKeys)
            else
              iWrapper.Field[TableName, FieldName, j] := Value[j];
          end;
      end;
    end;
  end;

  // Registration and Accession handled differently if hidden.
  for i := 0 to FHiddenEntryControls.Count - 1 do
  begin
    hiddenCtrl := TGeneralDataEntryControl(FHiddenEntryControls.Objects[i]);
    with hiddenCtrl do
      if not Visible and (DataType in [CT_REGISTRATION, CT_ACCESSION]) then
        for j := 0 to ItemCount - 1 do
          with dmGeneral.GetRecordset(
              'usp_QEDataItem_Select_ForTemplateField',
              ['@TemplateFieldKey', TemplateFieldKey,
               '@DataRowKey', StrToInt(HiddenValue[DATA_ROW_KEY, sgQuickEntry.Row, 0])]) do
            iWrapper.Field[TableName, FieldName, j] := Fields['Data_Value'].Value;
  end;
end;  // TfraQuickEntry.PopulateWrapperWithSpecimenFields

{-------------------------------------------------------------------------------
  Processes a range of rows in the database.  If the ASelectedOnly is set to
      True, then rows must be selected to be processed.
}
procedure TfraQuickEntry.ProcessRegion(ATopRow, ABottomRow: integer;
    ASelectedOnly: boolean);
var
  i: Integer;
  lCursor: TCursor;
  lRowsToProcess: TList;
  lRow: Integer;
  lNonValidatedRows: Integer;
  lRowsUnValidated : Boolean;
  lRecMain: IRecorderMainForm;
begin
  lCursor := HourglassCursor;
  try
    lRowsToProcess := TList.Create;
    lNonValidatedRows := 0;
    lRowsUnValidated := False;
    try
      lRecMain := dmGeneral.Recorder.RecorderMainForm;
      lRecMain.StartProgressBar;
      lRecMain.Progress := 0;
      lRecMain.StatusText := ResStr_ProcessingData;
      // Precheck the rows to process, as any change to the grid resets the row selection
      for i := ATopRow to ABottomRow do
        if ((not ASelectedOnly) or (sgQuickEntry.Cells[0, i] = '1') or (i = sgQuickEntry.Row))
           and (not RowProcessed[i]) then
        begin
          if RowValidated[i] then
            lRowsToProcess.Add(Pointer(i))
          else
            Inc(lNonValidatedRows);
        end
        else if (not RowProcessed[i]) then
          lRowsUnValidated := true;

      for i := 0 to lRowsToProcess.Count - 1 do
      begin
        lRow := Integer(lRowsToProcess[i]);
        ProcessRow(lRow);
        lRecMain.Progress := i * 100 div lRowsToProcess.Count;
      end;
      if lNonValidatedRows > 0 then
        ShowInformation(Format(ResStr_InvalidRowFound,
                        [Format(IfThen(lNonValidatedRows = 1,
                                       ResStr_RowSingle, ResStr_RowPlural),
                                [lNonValidatedRows])]))
      else
      if lRowsToProcess.Count = 0 then
        ShowInformation(ResStr_NoRowsToProcess)
      else
      if (not lRowsUnValidated) then
        if messageDlg(ResStr_DiscardProcessedSession, mtConfirmation,
                      [mbYes, mbNo], 0) = mrYes then
          self.DiscardSession(self);

      FDataStringGrid.HideCustomEdit;
    finally
      lRowsToProcess.Free;
      lRecMain.StatusText := '';
      lRecMain.Progress   := 0;
      lRecMain.StopProgressBar;
    end;
  finally
    DefaultCursor(lCursor);
    LoadRow(sgQuickEntry.Row);
    UpdateDataEntryControlsState;
  end;
end;  // TfraQuickEntry.ProcessRegion 

{-------------------------------------------------------------------------------
  Processes a row. 
}
procedure TfraQuickEntry.ProcessRow(ARow: integer);
var
  ltfProcessed, ltfValidated: Boolean;
  lTimestamp: TSQLSvrTimestamp;
  liRowKey: Integer;
  lWrapper: TInsertWrapper;
begin
  //This fills all the fields but will not cause an
  //exception as the row has already been validated 
  lWrapper := TInsertWrapper.Create;
  ValidateRow(ARow, lWrapper, True);
  try
    liRowKey :=  StrToInt(Trim(HiddenValue[DATA_ROW_KEY, ARow, 0]));
    lWrapper.Execute;
    //set row to processed
    dmQuickEntry.UpdateDataRow(liRowKey, True, True,
                               TimestampValue['RowTimestamp', ARow, 0]);
    dmQuickEntry.RefreshDataRow(liRowKey, ltfValidated, ltfProcessed, lTimestamp);
    RowValidated[ARow] := ltfValidated;
    RowProcessed[ARow] := ltfProcessed;
    TimestampValue['RowTimestamp', ARow, 0] := lTimestamp;
  finally
    lWrapper.Free;
    TStringGridAccessorQuickEntryFrame(sgQuickEntry).InvalidateRow(ARow);
  end;
end;  // TfraQuickEntry.ProcessRow 

{-------------------------------------------------------------------------------
  Registers linked edit controls. 
}
procedure TfraQuickEntry.RegisterDragDropComponents;

  procedure RegisterComponents(AControlList: TStringList);
  var
    i: Integer;
  begin
    for i := 0 to AControlList.Count - 1 do
      TDataEntryControl(AControlList.Objects[i]).RegisterLinkedEditControl(Self);
  end;

begin
  inherited;
  RegisterComponents(FGeneralEntryControls);
  RegisterComponents(FSpecimenEntryControls);
  RegisterComponents(FHiddenEntryControls);
  RegisterDropComponent(sgQuickEntry, GridDrop, [], [dmGeneral.Recorder.JNCCFormat],
                        GridDragOver);
end;  // TfraQuickEntry.RegisterDragDropComponents 

{-------------------------------------------------------------------------------
  Moves chkValidated from the grid view to the form view.
}
procedure TfraQuickEntry.ReplaceChkValidated;
begin
  with TWinControlAccessor(chkValidated) do
  begin
    Parent           := sbSpecimenControls;
    Left             := 177;
    Top              := CONTROL_MARGIN;
    OnMouseWheelDown := nil;
    OnMouseWheelUp   := nil;
    Visible          := True;
  end;
  FtfchkValidatedDislocated := False;
end;  // TfraQuickEntry.ReplaceChkValidated 

{-------------------------------------------------------------------------------
}
procedure TfraQuickEntry.SaveRow(ARow: integer);
var
  i, j: Integer;
  lstUpdatedFields: string;
  ltfValidated, ltfProcessed: Boolean;
  lTimestamp: TSQLSvrTimestamp;
  liDataRowKey: Integer;
  lAcceptError: Boolean;
begin
  with FDataStringGrid do
  begin
    //If data item key is empty then the row is going to be deleted.
    if (Trim(HiddenValue[DATA_ROW_KEY, ARow, 0]) <> '') and not RowProcessed[ARow] then
    begin
      liDataRowKey := StrToInt(Trim(HiddenValue[DATA_ROW_KEY, ARow, 0]));
      try
        dmQuickEntry.UpdateDataRow(liDataRowKey, RowValidated[ARow], False,
                                   TimestampCol['RowTimestamp', ARow, 0]);
      finally
        dmQuickEntry.RefreshDataRow(liDataRowKey, ltfValidated, ltfProcessed, lTimestamp);
        RowProcessed[ARow] := ltfProcessed;
        RowValidated[ARow] := ltfValidated;
        TimestampCol['RowTimestamp', ARow, 0] := lTimestamp;
      end;

      lstUpdatedFields := '';

      for i := 0 to FSpecimenEntryControls.Count - 1 do
        with TSpecimenDataEntryControl(FSpecimenEntryControls.Objects[i]) do
          for j := 0 to ItemCount - 1 do
            if (OldValue[j] <> Value[j]) or
               (OldDisplay[j] <> Display[j]) or
               ((DataItemKey[j] = -1) and (Value[j] <> '')) then
              try
                UpdateDataItemAndSynch(
                    TSpecimenDataEntryControl(FSpecimenEntryControls.Objects[i]),
                    ARow,
                    j);
              except
                on EUpdatedException do
                  //build list of fields that've been updated
                  lstUpdatedFields := lstUpdatedFields +
                                      sgQuickEntry.Cells[GridColumn, 0] + ', ';
                on EOleException do
                begin
                  lAcceptError := false;
                  // Accept foreign key violations, it means a column has been removed
                  if dmGeneral.Connection.Errors.Count > 0 then
                    if dmGeneral.Connection.Errors[0].NativeError = FOREIGN_KEY_CONSTRAINT_FAIL then
                      lAcceptError := True;
                  if not lAcceptError then raise;
                end;
              end;

      if lstUpdatedFields <> '' then
      begin
        lstUpdatedFields := LeftStr(lstUpdatedFields, Length(lstUpdatedFields) - 2);
        raise EUpdatedException.CreateNonCritical(Format(ResStr_RecordsUpdated,
                                                         [lstUpdatedFields]));
      end;
    end;
  end;
end;  // TfraQuickEntry.SaveRow

{-------------------------------------------------------------------------------
  Updates a data control into the database, and ensure that the timestamp
      and key fields remain in synch.
}
procedure TfraQuickEntry.UpdateDataItemAndSynch(AControl: TDataEntryControl;
    ARow: integer; AIndex: Integer);
begin
  with AControl do
  begin
    if FieldName = fnSpatialRef then
      Timestamp[0] := NULL;

    UpdateDataItem(StrToInt(HiddenValue[DATA_ROW_KEY, ARow, 0]), 0);
    // Keep the hidden value in synch!
    if DataItemKey[AIndex] = -1 then
      HiddenValue[TemplateFieldKey + DATA_ITEM_KEY, ARow, AIndex] := ''
    else begin
      HiddenValue[TemplateFieldKey + DATA_ITEM_KEY, ARow, AIndex]  := IntToStr(DataItemKey[AIndex]);
      HiddenValue[TemplateFieldKey + 'Value', ARow, AIndex]        := Value[AIndex];
      TimestampValue[TemplateFieldKey + 'Timestamp', ARow, AIndex] := Timestamp[AIndex];
    end;
  end;
end;

{-------------------------------------------------------------------------------
}
procedure TfraQuickEntry.RefreshAfterMultivalueEdit(sender: TObject);
var
  i: Integer;
begin
  if sender is TDataEntryControl then
    with TDataEntryControl(sender) do
    begin
      for i := 0 to ItemCount - 1 do
      begin
        if DataItemKey[i] = -1 then
          HiddenValue[TemplateFieldKey + DATA_ITEM_KEY, sgQuickEntry.Row, i] := ''
        else begin
          HiddenValue[TemplateFieldKey + DATA_ITEM_KEY, sgQuickEntry.Row, i]  := IntToStr(DataItemKey[i]);
          HiddenValue[TemplateFieldKey + 'Value', sgQuickEntry.Row, i]        := Value[i];
          TimestampValue[TemplateFieldKey + 'Timestamp', sgQuickEntry.Row, i] := Timestamp[i];
        end;
      end;
    end;
end;

{-------------------------------------------------------------------------------
  Selects the text in an edit box or linked edit control. 
}
procedure TfraQuickEntry.SelectControl(AControl: TWinControl);
var
  lEdit: TCustomEdit;
begin
  lEdit := nil;
  if AControl is TCustomEdit then
    lEdit := TCustomEdit(AControl)
  else
  if AControl is TLinkedEdit then
    lEdit := TLinkedEdit(AControl).EditBox;

  if Assigned(lEdit) then begin
    lEdit.SelStart := 0;
    lEdit.SelLength := Length(lEdit.Text);
  end;
end;  // TfraQuickEntry.SelectControl 

{-------------------------------------------------------------------------------
  Sets the width of the last added column, using the data entry control type as
      a guide.
}
procedure TfraQuickEntry.SetColWidth(ADataEntryControl: TDataEntryControl);
begin
  SetControlWidth(ADataEntryControl);
  if ADataEntryControl.DefaultWidth <> -1 then
    sgQuickEntry.ColWidths[sgQuickEntry.ColCount - 1] :=
        Min(Max(Trunc(ADataEntryControl.Width * 0.6),
                sgQuickEntry.ColWidths[sgQuickEntry.ColCount - 1]),
            260);
end;  // TfraQuickEntry.SetColWidth 

{-------------------------------------------------------------------------------
  Converts the requested with of a control (in chars) to a width in pixels
      suitable for the on screen field.
}
procedure TfraQuickEntry.SetControlWidth(ADataEntryControl: TDataEntryControl);
begin
  with ADataEntryControl do
  begin
    if DefaultWidth <> -1 then begin
      // convert requested width in characters to a suitable width in pixels.
      // Allow twice as much for the first 30 characters
      Width := DefaultWidth * CHAR_WIDTH_ALLOWANCE * 2;
      if DefaultWidth > 30 then
        Width := Width - (DefaultWidth - 30) * CHAR_WIDTH_ALLOWANCE;
      // And allow extra for combo boxes
      if Control is TComboBox then
        Width := Width + GetSystemMetrics(SM_CXVSCROLL);
      Width := Min(MAX_FIELD_WIDTH, Width);
    end else
      Width := MAX_FIELD_WIDTH;
  end;
end;  // TfraQuickEntry.SetControlWidth

{-------------------------------------------------------------------------------
  Accessor method, sets the value in an invisible column.
}
procedure TfraQuickEntry.SetHiddenValue(AName: string; ARow: Integer; AItem: Integer;
    const Value: string);
begin
  FDataStringGrid.InvisibleColValue[AName, ARow, AItem] := Value;
end;  // TfraQuickEntry.SetHiddenValue

{-------------------------------------------------------------------------------
  Accessor method, sets the value in a timestamp column.
}
procedure TfraQuickEntry.SetTimestampValue(AName: string; ARow: Integer; AItem: Integer;
    const Value: TSQLSvrTimestamp);
begin
  FDataStringGrid.TimestampCol[AName, ARow, AItem] := Value;
end;  // TfraQuickEntry.SetTimestampValue

{-------------------------------------------------------------------------------
  Sets the enabled states of each button on the navigator according to the
      state of the current row.
}
procedure TfraQuickEntry.SetNavigatorButtonStates(ARow: integer);
begin
  btnRecNext.Enabled  := ARow < sgQuickEntry.RowCount - 1;
  btnRecLast.Enabled  := ARow < sgQuickEntry.RowCount - 1;
  btnRecPrev.Enabled  := ARow > 1;
  btnRecFirst.Enabled := ARow > 1;
  // disable delete button if on last remaining empty row
  btnRecDel.Enabled := FDataStringGrid.RowContainsData(1) or (sgQuickEntry.RowCount > 2);
end;  // TfraQuickEntry.SetNavigatorButtonStates

{-------------------------------------------------------------------------------
  Sets the NumberOfRecords on the label under the grid.  Has a side effect of
  resetting the FStoreAlteredManually list.
}
procedure TfraQuickEntry.SetNumberOfRecords(const Value: Integer);
var
  i: Integer;
  lStoreInitialised: String;
begin
  FNumberOFRecords := Value;
  FStoreAlteredManually := TStringList.Create;
  // Zero index is for the general tab.  Speciments start at index 1.  If there
  // is any pre-existing data in the current store field then remove auto update
  // link from the usual store field.
  if FStoreInitiallySupplied then
    lStoreInitialised := 'Y'
  else
    lStoreInitialised := 'N';

  for i:=0 to Value do
    FStoreAlteredManually.Add(lStoreInitialised);
  lblRecordNumber.Caption := Format(ResStr_RecordNumber, [FRecordNumber, FNumberOFRecords]);
end;  // TfraQuickEntry.SetNumberOfRecords

{-------------------------------------------------------------------------------
  Sets the display of the current record.
}
procedure TfraQuickEntry.SetRecordNumber(const Value: Integer);
begin
  FRecordNumber := Value;
  lblRecordNumber.Caption := Format(ResStr_RecordNumber, [FRecordNumber, FNumberOFRecords]);
end;  // TfraQuickEntry.SetRecordNumber

{-------------------------------------------------------------------------------
  Accessor method
}
procedure TfraQuickEntry.SetRowProcessed(Index: Integer; Value: Boolean);
begin
  HiddenValue[PROCESSED, Index, 0] := Sysutils.BoolToStr(Value);
end;  // TfraQuickEntry.SetRowProcessed

{-------------------------------------------------------------------------------
}
procedure TfraQuickEntry.SetRowValidated(ARow: Integer; AValue: Boolean);
begin
  sgQuickEntry.Cells[VALIDATED_COLUMN, ARow] := Sysutils.BoolToStr(AValue);
end;  // TfraQuickEntry.SetRowValidated

{-------------------------------------------------------------------------------
  Updates the session key. Likely to be called but once for each instance of
      the frame.
}
procedure TfraQuickEntry.SetSessionKey(const Value: LongInt);
begin
  FSessionKey := Value;
  GetSessionDetails;
  //Grid is not populated here. To populate, call Initialise.
end;  // TfraQuickEntry.SetSessionKey

{-------------------------------------------------------------------------------
  On Exit handler for the grid - saves the current row to the database. 
}
procedure TfraQuickEntry.sgQuickEntryExit(Sender: TObject);
begin
  SaveRow(sgQuickEntry.Row);
end;  // TfraQuickEntry.sgQuickEntryExit 

{-------------------------------------------------------------------------------
  Display the specimens or occurrences grid as a form view. 
}
procedure TfraQuickEntry.ShowAsForm;
var
  lIdx: Integer;
  lCtrl: TSpecimenDataEntryControl;
begin
  sbSpecimenControls.Visible := True;
  sgQuickEntry.Visible := False;
  FDataStringGrid.HideCustomEdit;
  FCurrentGridControl := nil;
  //take the validation checkbox from the stringgrid
  if FtfchkValidatedDislocated then
    ReplaceChkValidated;
  chkValidated.Checked := RowValidated[sgQuickEntry.Row];
  chkValidated.Enabled := not RowProcessed[sgQuickEntry.Row];

  for lIdx := 0 to FSpecimenEntryControls.Count - 1 do
  begin
    lCtrl := TSpecimenDataEntryControl(FSpecimenEntryControls.Objects[lIdx]);
    lCtrl.ReplaceEditControl;

    if lCtrl.DataType = CT_RECORD_TYPE then
      PopulateRecordTypeCombo(lCtrl);
    // Fixes a sizing buf that happen when re-parenting controls
    PostMessage(lCtrl.Control.Handle, WM_SIZE, 0, 0);
  end;

  // Setup readonly state as required, or user would be able to change processed items!
  UpdateDataEntryControlsState;

  FocusFirstItem;
end;  // TfraQuickEntry.ShowAsForm

{-------------------------------------------------------------------------------
  Display the specimens or occurrences data entry as a grid.
}
procedure TfraQuickEntry.ShowAsGrid;
var
  lIdx: Integer;
begin
  sbSpecimenControls.Visible := False;
  sgQuickEntry.Visible := True;
  for lIdx := 0 to FSpecimenEntryControls.Count - 1 do
    with TSpecimenDataEntryControl(FSpecimenEntryControls.Objects[lIdx]) do
    begin
      Control.Parent := FDataStringGrid.CustomEditPanel;
      Control.Visible := False;
      if Control is TCustomEdit then TEdit(Control).BorderStyle := bsNone else
      if Control is TLinkedEdit then TLinkedEdit(Control).BorderStyle := bsNone;
      // Fixes a sizing buf that happen when re-parenting controls
      PostMessage(Control.Handle, WM_SIZE, 0, 0);
    end;
  UpdateDataEntryControlsState;

  chkValidated.Parent := FDataStringGrid.CustomEditPanel;
  chkValidated.Visible := False;
  FtfchkValidatedDislocated := True;
end;  // TfraQuickEntry.ShowAsGrid

{-------------------------------------------------------------------------------
  Processes a key hit and determines whether any navigation should take place.
      This is not actually an event handler; it is called directly from
      CMChildKey.
}
procedure TfraQuickEntry.SpecimenControlKeyDown(Sender: TObject; var Key: Word;
    Shift: TShiftState);
var
  i: Integer;

  //This function works out whether left and right arrows should
  //cause the cell to change.
  function ArrowsShouldWork(lKey: Word): boolean;
  begin
    Result := False;
    if Sender is TCustomEdit then
      with TCustomEdit(Sender) do
      begin
        if (SelLength = Length(Text)) or
           (((lKey = VK_Right) and (SelStart = Length(Text))) or
           ((lKey = VK_Left) and (SelStart = 0) and (SelLength = 0))) then
          Result := true;
      end
    else
    if Sender is TLinkedEdit then
      with TLinkedEdit(Sender) do
      begin
        if (SelLength = Length(Text)) or
           (((lKey = VK_Right) and (SelStart = Length(Text))) or
           ((lKey = VK_Left) and (SelStart = 0) and (SelLength = 0))) then
          Result := true;
      end
    else
    if Sender is TLuxIDComboBox then
      with TLuxIDComboBox(Sender) do
      begin
        if not DroppedDown then
          Result := true;
      end
    else
    if Sender is TCheckBox then
      Result := true;
  end;

  // Move x or y direction in the grid.  Ensures that the SpecimenEventHandlerExit
  // is called, and ensures that it is not called again should the move result
  // in us landing on a processed cell (in which case the data entry control
  // disappears so the exit handler would trigger).
  procedure DoMove(ADeltaCol, ADeltaRow: integer);
  begin
    SpecimenEventHandlerExit(Sender);
    FExitHandlerDisabled := true;
    try
      if (sgQuickEntry.Row = sgQuickEntry.RowCount - 1) and (ADeltaRow>0) then
        AddRow
      else
        sgQuickEntry.Row := sgQuickEntry.Row + ADeltaRow;
      sgQuickEntry.Col := sgQuickEntry.Col + ADeltaCol;
      Key := 0;
    finally
      FExitHandlerDisabled := false;
    end;
  end;

begin
  case Key of
    VK_RIGHT:
      if ArrowsShouldWork(Key) then
        if sgQuickEntry.Col < sgQuickEntry.ColCount - 1 then
          DoMove(1,0)
        else
        begin
          DoMove(0, 1);
          sgQuickEntry.Col := 1;
        end;
    VK_LEFT:
      if ArrowsShouldWork(Key) then
        if sgQuickEntry.Col > 1 then
          DoMove(-1,0)
        else if sgQuickEntry.Row > 1 then
        begin
          DoMove(0, -1);
          sgQuickEntry.Col := sgQuickEntry.ColCount - 1;
        end;
    VK_UP:
      if not (Sender is TLuxIDComboBox) then
        if sgQuickEntry.Row > 1 then
          DoMove(0,-1);
    VK_DOWN:
      if not (Sender is TLuxIDComboBox) then
        DoMove(0, 1);
    192: // apostrophe - copy cell from above
      if (ssCtrl in Shift) and not (Sender = chkValidated)  then
        if not (sgQuickEntry.Row = 1) then
          with TSpecimenDataEntryControl(FCurrentGridControl.Owner) do
          begin
            for i := 0 to Length(FGridValues[sgQuickEntry.Col, sgQuickEntry.Row - 1]) - 1 do
            begin
              if Control is TMultiValueLinkedEdit then begin
                TMultiValueLinkedEdit(Control).SetDisplayAndValue(i,
                    FGridValues[sgQuickEntry.Col, sgQuickEntry.Row - 1, i],
                    HiddenValue[TemplateFieldKey + 'Value', sgQuickEntry.Row - 1, i]);
                // also need to copy the data
                dmGeneral.RunStoredProc(
                    'usp_QEDataItem_Insert_ForMultiValues',
                    ['@DataItemKey', Null,
                     '@DataRowKey', HiddenValue[DATA_ROW_KEY, sgQuickEntry.Row, 0],
                     '@TemplateFieldKey', TemplateFieldKey,
                     '@DataDisplay', FGridValues[sgQuickEntry.Col, sgQuickEntry.Row - 1, i],
                     '@DataValue', HiddenValue[TemplateFieldKey + 'Value', sgQuickEntry.Row - 1, i],
                     '@Position', i,
                     '@EnteredSessionID', dmGeneral.Recorder.CurrentSettings.SessionID
                    ]);
              end else begin
                Display[i] := FGridValues[sgQuickEntry.Col, sgQuickEntry.Row - 1, i];
                Value[i] := HiddenValue[TemplateFieldKey + 'Value', sgQuickEntry.Row - 1, i];
              end;
            end;
            SelectControl(TSpecimenDataEntryControl(FCurrentGridControl.Owner).Control);
            Key :=0;
          end;
  end;
end;  // TfraQuickEntry.SpecimenControlKeyDown

{-------------------------------------------------------------------------------
  Event handler called when the control section of a TSpecimenDataEntryControl
      is replaced on its panel.
}
procedure TfraQuickEntry.SpecimenControlReplaced(Sender:TObject);
begin
  if Sender is TSpecimenDataEntryControl then
    with TSpecimenDataEntryControl(Sender) do
    begin
      with TWinControlAccessor(Control) do
      begin
        PopupMenu := nil;
        OnMouseWheel := nil;
      end;
      if Control is TLinkedEdit then
        TLinkedEdit(Control).ShowDragDropBorder := True;
    end;
end;  // TfraQuickEntry.SpecimenControlReplaced

{-------------------------------------------------------------------------------
  Unvalidates the row when the data is changed.
}
procedure TfraQuickEntry.SpecimenEventHandlerChange(Sender: TObject);
begin
  RowValidated[sgQuickEntry.Row] := False;
end;  // TfraQuickEntry.SpecimenEventHandlerChange

{-------------------------------------------------------------------------------
  When a usual store control is exited fire additionally fire the exited event
  on the associated control.
}
procedure TfraQuickEntry.SpecimenStoreEventHandlerExit(Sender: TObject);
var
  lStoreControl: TDataEntryControl;
  lOtherStoreControl: TSpecimenDataEntryControl;
  i: Integer;
begin
  lOtherStoreControl := nil;
  SpecimenEventHandlerExit(Sender);

  lStoreControl := TDataEntryControl(TWinControl(Sender).Owner);
  for i := 0 to FSpecimenEntryControls.Count - 1 do
    if (TDataEntryControl(FSpecimenEntryControls.Objects[i]).DataType = CT_STORE)
      And (TDataEntryControl(FSpecimenEntryControls.Objects[i]) <> lStoreControl) then
    begin
      lOtherStoreControl := TSpecimenDataEntryControl(FSpecimenEntryControls.Objects[i]);
      break;
    end;

    if Assigned(lOtherStoreControl) then
       SpecimenEventHandlerExit(lOtherStoreControl.Control);
end;

{-------------------------------------------------------------------------------
  Updates the string grid with the value in the control.
}
procedure TfraQuickEntry.SpecimenEventHandlerExit(Sender: TObject);
var
  i, lOldLength: Integer;
begin
  if FExitHandlerDisabled then exit;
  if (Sender <> chkValidated) then
    with (TSpecimenDataEntryControl(TWinControl(Sender).Owner)) do
    begin
      lOldLength := Length(FGridValues[GridColumn, sgQuickEntry.Row]);
      SetLength(FGridValues[GridColumn, sgQuickEntry.Row], ItemCount);

      for i := 0 to ItemCount - 1 do
      begin
        // Refresh underlying data if anything changed.
        if ((OldValue[i] <> Value[i]) or
            (OldDisplay[i] <> Display[i]) or
            (FGridValues[GridColumn, sgQuickEntry.Row, i] <> Display[i]) or
            (lOldLength <> ItemCount)) and
           (Enabled) then
        begin
          if (OldValue[i] <> Value[i]) then
            HiddenValue[TemplateFieldKey + 'Value', sgQuickEntry.Row, i] := Value[i];
          FGridValues[GridColumn, sgQuickEntry.Row, i] := Display[i];
          if DataType in [CT_CONCEPT, CT_INDIVIDUAL, CT_GEO_AREA] then
          begin
            if ItemCount > 1 then
              sgQuickEntry.Cells[GridColumn, sgQuickEntry.Row] := '<i>' + TMultiValueLinkedEdit(Control).Text + '</i>'
            else
              sgQuickEntry.Cells[GridColumn, sgQuickEntry.Row] := TMultiValueLinkedEdit(Control).Text;
          end else
            sgQuickEntry.Cells[GridColumn, sgQuickEntry.Row] := Display[0];

          // Saftey check now incase we are deleting nodes when this happens
          if sgQuickEntry.Row<sgQuickEntry.RowCount then
            // Invalidate the row
            RowValidated[sgQuickEntry.Row] := False;
          if DataType in [CT_STAFF_RESPONSIBLE, CT_DEPARTMENT] then
            UpdateMacroNumber(TDataEntryControl(TWinControl(Sender).Owner));
        end else begin
          if not Enabled then
          begin
            if DataType in [CT_CONCEPT, CT_INDIVIDUAL, CT_GEO_AREA] then
            begin
              TMultiValueLinkedEdit(Control).SetDisplayAndValue(i, OldDisplay[i], OldValue[i]);
              TMultiValueLinkedEdit(Control).TemplateFieldKey := TemplateFieldKey;
              sgQuickEntry.Cells[GridColumn, sgQuickEntry.Row] := TMultiValueLinkedEdit(Control).Text;
            end else begin
              Value[i] := OldValue[i];
              Display[i] := OldDisplay[i];
              sgQuickEntry.Cells[GridColumn, sgQuickEntry.Row] := Display[0];
            end;
          end;
        end;
      end; // for
    end; // with
  // Update buttons. This will enable Delete on first record if not empty.
  // Will also disable Delete if empty.
  SetNavigatorButtonStates(sgQuickEntry.Row);
end;  // TfraQuickEntry.SpecimenEventHandlerExit

{-------------------------------------------------------------------------------
  Moves to the next row
}
procedure TfraQuickEntry.SpecimenMouseWheelDown(Sender: TObject; Shift:
    TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
  inherited;
  if sgQuickEntry.Row < sgQuickEntry.RowCount - 1 then
  begin
    SpecimenEventHandlerExit(Sender);
    sgQuickEntry.Row := sgQuickEntry.Row + 1;
    Handled := True;
  end;
end;  // TfraQuickEntry.SpecimenMouseWheelDown 

{-------------------------------------------------------------------------------
  Moves to the previous row. 
}
procedure TfraQuickEntry.SpecimenMouseWheelUp(Sender: TObject; Shift:
    TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
  inherited;
  if sgQuickEntry.Row > 1 then
  begin
    SpecimenEventHandlerExit(Sender);
    sgQuickEntry.Row := sgQuickEntry.Row - 1;
    Handled := True;
  end;
end;  // TfraQuickEntry.SpecimenMouseWheelUp

{-------------------------------------------------------------------------------
}
procedure TfraQuickEntry.StringGridCanEditCell(var ioAccept: Boolean; ACol, ARow: Integer);
begin
  ioAccept := not RowProcessed[ARow];
  if ioAccept and (ACol > 1) then
    ioAccept := not TSpecimenDataEntryControl(FSpecimenEntryControls.Objects[ACol-2]).Locked;
end;  // TfraQuickEntry.StringGridCanEditCell

{-------------------------------------------------------------------------------
  Tells the FDataStringGrid draw routine what colour to draw the row (processed
      rows are greyed out).
}
function TfraQuickEntry.StringGridCellBGColour(ACol, ARow: integer; var
    AHandled: boolean): TColor;
var
  i: Integer;
begin
  // Grid default
  Result := clWindow;
  AHandled := False;

  if Length(FGridValues[ACol]) <= ARow then
    SetLength(FGridValues[ACol], sgQuickEntry.RowCount);

  // Now change as needed.
  if (ARow >= sgQuickEntry.FixedRows) and (ACol >= sgQuickEntry.FixedCols) then
    if RowProcessed[ARow] then begin
      Result := MergeColours(clBtnFace, clWindow, 60);
      AHandled := True;
    end else
    for i := 0 to Length(FGridValues[ACol, ARow]) - 1 do
      if not ValidData(ACol, ARow, i) then begin
        Result := clRed;
        AHandled := True;
        break;
      end;
end;  // TfraQuickEntry.StringGridCellBGColour

{-------------------------------------------------------------------------------
  Tells the FDataStringGrid draw routine what colour to write the text (Locked
    columns have greyed out text).
}
function TfraQuickEntry.StringGridCellTextColour(ACol, ARow: integer; var AHandled:
        boolean): TColor;
begin
  Result := clWindowText;

  if (ACol > 1) and (ARow >= sgQuickEntry.FixedRows) and (not RowProcessed[ARow]) then
    if TSpecimenDataEntryControl(FSpecimenEntryControls.Objects[ACol-2]).Locked then
      Result := clGrayText;
end;

{-------------------------------------------------------------------------------
  Places the TSpecimenDataEntry control on the custom edit panel of
      FDataStringGrid so that the control can be used in both the Form and Grid
      views.
}
procedure TfraQuickEntry.StringGridCustomEdit(Panel: TPanel; ACol, ARow:
    integer);
var
  lstFieldName: string;
  lDataEntryControl: TDataEntryControl;
begin
  if not RowProcessed[ARow] then begin
    //get rid of existing controls on the panel
    if Assigned(FCurrentGridControl) then
      FCurrentGridControl.Visible := False;

    if ACol = VALIDATED_COLUMN then //validated
      DisplayValidatedCheckbox(ARow)
    else if not TSpecimenDataEntryControl(FSpecimenEntryControls.Objects[ACol-2]).Locked then begin
      //find out what type of column it is
      lstFieldName := LeftStr(FDataStringGrid.FieldName[ACol], 16);
      with FSpecimenEntryControls do
        lDataEntryControl := TDataEntryControl(Objects[IndexOf(lstFieldName)]);
      //move the control from the form view to the grid.
      if Assigned(lDataEntryControl.Control) then
      begin
        with lDataEntryControl.Control do
        begin
          Align := alClient;
          Visible := true;
          if CanFocus then SetFocus;
        end;

        FCurrentGridControl := lDataEntryControl.Control;
        with TWinControlAccessor(FCurrentGridControl) do
        begin
          PopupMenu := pmGrid;
          if not (FCurrentGridControl is TLuxIDComboBox) then
          begin
            OnMouseWheelDown := SpecimenMouseWheelDown;
            OnMouseWheelUp := SpecimenMouseWheelUp;
          end;
        end;
        if FCurrentGridControl is TLinkedEdit then
          with TLinkedEdit(FCurrentGridControl) do
          begin
            ShowDragDropBorder := False;
            EditSetFocus;
            EditBox.SelStart := Length(EditBox.Text);
          end
        else
        if FCurrentGridControl is TCustomEdit then
          with TCustomEdit(FCurrentGridControl) do
            SelStart := Length(Text);

        // The record type combo for occurrences is dynamic depending on the selected
        // determination concept group.
        if lDataEntryControl.DataType = CT_RECORD_TYPE then
          PopulateRecordTypeCombo(lDataEntryControl);
      end;
    end else begin
      Panel.Visible := False;
    end;
  end else
    Panel.Visible := False;
end;  // TfraQuickEntry.StringGridCustomEdit

{-------------------------------------------------------------------------------
  Handle the positioning of chkValidated to centre it in the cell.
}
procedure TfraQuickEntry.StringGridRepositionCustomEdit(Panel: TPanel; ACol,
    ARow: integer);
begin
  if ACol = VALIDATED_COLUMN then begin
    // don't change unless necessary to avoid redraws
    if chkValidated.Top <> (Panel.ClientHeight div 2) - (chkValidated.Height div 2) then
      chkValidated.Top := (Panel.ClientHeight div 2) - (chkValidated.Height div 2);
    if chkValidated.Left <> (Panel.ClientWidth div 2) - (chkValidated.Width div 2) - 1 then
      chkValidated.Left := (Panel.ClientWidth div 2) - (chkValidated.Width div 2) - 1;
  end else
  if FCurrentGridControl is TCustomComboBox then
    with TCustomComboBox(FCurrentGridControl) do begin
      Align   := alNone;
      Anchors := [akLeft, akTop, akRight];
      Left    := -2;
      Top     := -2;
      Width   := Parent.Width + 3;
    end
  else
  if FCurrentGridControl is TLinkedEdit then
    with TLinkedEdit(FCurrentGridControl) do
    begin
      Align   := alNone;
      Anchors := [akLeft, akTop, akRight, akBottom];
      Width   := Parent.Width;
    end
  else
  if FCurrentGridControl is TCustomEdit then
    with TCustomEdit(FCurrentGridControl) do
    // Ensure edit controls align with the text in normal grid cells
      if Align = alClient then
      begin
        Align   := alNone;
        Anchors := [akLeft, akTop, akRight, akBottom];
        Left    := 2;
        Top     := 2;
      end; // with
end;  // TfraQuickEntry.StringGridRepositionCustomEdit

{-------------------------------------------------------------------------------
  Event handler called when the row of the grid has changed. Saves what was
      entered in the previous row and puts the values in the new row into the
      Specimen controls.
}
procedure TfraQuickEntry.StringGridRowChanged(Sender: TObject; ARow: integer);
var
  ltfRowValidated: Boolean;
begin
  ltfRowValidated := RowValidated[sgQuickEntry.Row];
  try
    if (sgQuickEntry.Row > 0) and not RowProcessed[sgQuickEntry.Row] then
      try
        SaveRow(sgQuickEntry.Row);
      except
        on EDeletedException do begin
          MessageDlg(ResStr_RowDeleted, mtInformation, [mbOK],0);
          if ARow > sgQuickEntry.Row then ARow := ARow -1;
          FDataStringGrid.DeleteRow(sgQuickentry.Row);
        end;
      end;
  finally
    LoadRow(ARow);
    SetNavigatorButtonStates(ARow);
    RecordNumber := ARow;
    // If the row we're moving from was validated, make sure it stays validated.
    if ltfRowValidated then
      RowValidated[sgQuickEntry.Row] := True;
  end;
end;  // TfraQuickEntry.StringGridRowChanged

{-------------------------------------------------------------------------------
  Attempt to set the validation status of a row.  Will fail if the user has no
      rights or the row is invalid.
}
procedure TfraQuickEntry.TryToValidateRow(ARow: integer; AValidated: boolean);
var
  lWrapper: TInsertWrapper;
begin
  if Appsettings.AllowQuickEntryProcessing then
  begin
    SaveRow(sgQuickEntry.Row);
    if not FtfRowChanging then
    begin
      if AValidated then
      begin
        lWrapper := TInsertWrapper.Create;
        try
          ValidateRow(ARow, lWrapper, True);
        except
          on E:EQuickEntryException do begin
            RowValidated[ARow] := False;
            ShowInformation(E.Message);
            Abort;
          end;
        end; // try
      end;
      RowValidated[ARow] := AValidated;
    end;
  end;
end;  // TfraQuickEntry.TryToValidateRow

{-------------------------------------------------------------------------------
}
procedure TfraQuickEntry.UpdateDataEntryControlsState;
var
  i: Integer;
  lEditMode: TEditMode;
  lReadOnly: Boolean;
begin
  lReadOnly := not (chkValidated.Enabled or not chkAsForm.Checked);
  lEditMode := emBrowse;
  if chkValidated.Enabled or not chkAsForm.Checked then lEditMode := emEdit;

  for i:= 0 to FSpecimenEntryControls.Count - 1 do
    with TSpecimenDataEntryControl(FSpecimenEntryControls.Objects[i]) do
      // Enable control when on the grid. They are displayed only when needed.
      if Control is TLuxIDComboBox then
        TLuxIDComboBox(Control).ReadOnly := lReadOnly
      else
      if Control is TEdit then
        TEdit(Control).ReadOnly := lReadOnly
      else
      if Control is TVagueDateEdit then
        TVagueDateEdit(Control).ReadOnly := lReadOnly
      else
      if Control is TLinkedEdit then
        TLinkedEdit(Control).EditMode := lEditMode
      else
      if Control is TSpatialRefLinkedEdit then
        TSpatialRefLinkedEdit(Control).EditMode := lEditMode;
end;  // TfraQuickEntry.UpdateDataEntryControlsState

{-------------------------------------------------------------------------------
}
procedure TfraQuickEntry.UpdateGeneralDataEntryControl(AControl: TGeneralDataEntryControl);
var
  liSpecimenIndex: Integer;
  i: Integer;
begin
  for i := 0 to AControl.ItemCount - 1 do begin
    if (AControl.OldValue[i] <> AControl.Value[i]) or
       (AControl.OldDisplay[i] <> AControl.Display[i]) then  //only update database if it's changed
    begin
      try
        AControl.UpdateDataItem(FGeneralRowKey, i);
      finally //update controls whether or not record has been updated
        liSpecimenIndex := FSpecimenEntryControls.IndexOf(AControl.TemplateFieldKey);
        if liSpecimenIndex >= 0 then
        begin
          //change the defaults on the data entry control so that any new rows
          // are created with the defaults from General
          with TSpecimenDataEntryControl(FSpecimenEntryControls.Objects[liSpecimenIndex]) do
          begin
            DefaultDisplay[i] := AControl.Display[i];
            DefaultValue[i]   := AControl.Value[i];
          end;
        end;//if liSpecimenIndex >=0
      end;//try..finally
      if AControl.DataType in [CT_STAFF_RESPONSIBLE, CT_DEPARTMENT] then
        UpdateMacroNumber(AControl);
    end;//if lDataEntryControl.OldValue<> Text
  end;
end;  // TfraQuickEntry.UpdateGeneralDataEntryControl

{-------------------------------------------------------------------------------
}
function TfraQuickEntry.ValidData(ACol, ARow, AItem: Integer): Boolean;
var
  i: Integer;
begin
  Result := True;
  if FGridValues[ACol, ARow, AItem] <> '' then
    // Change colour if data cannot be found in list of valid options.
    with FSpecimenEntryControls do
      for i := 0 to Count - 1 do
        with TSpecimenDataEntryControl(Objects[i]) do
          if GridColumn = ACol then
            if Control is TLuxIDComboBox then begin
              if (TLuxIDComboBox(Control).IndexOf(FGridValues[ACol, ARow, AItem]) = -1)
                  and (FTaxonOccurrenceKey = '')
                  and (FOccurrenceKey = '') then
              begin
                Result := False;
                Break;
              end
            end else
            if Control is TVagueDateEdit then
              try
                VagueDateToString(StringToVagueDate(FGridValues[ACol, ARow, AItem]));
              except
                on EVagueDateError do begin
                  Result := False;
                  Break;
                end;
              end;
end;  // TfraQuickEntry.StringGridCellBGColour

{-------------------------------------------------------------------------------
  Ensures that a row can be used to populate fields in the main database.
  If AFocusInvalidControl is true, then when an invalid control is located, it
      is focused.
}
procedure TfraQuickEntry.ValidateRow(ARow: integer; iWrapper : TInsertWrapper;
    AFocusInvalidControl: boolean);
var
  lInvalidFields: TInvalidFields;
  ltn: TTableName;
  lLastErrorMessage: string;
  lLastInvalidControl: TDataEntryControl;

  // Force a value into any mandatory fields, and obtain validation error info
  // where appropriate
  procedure GetValuesFromControlList(AList: TStringList);
  var
    i, j: integer;
    lOldValue: string;
  begin
    if lInvalidFields[ltn] <> [] then
    begin
      for i := 0 to AList.Count - 1 do
      begin
        with TDataEntryControl(AList.Objects[i]) do
        begin
          for j := 0 to ItemCount - 1 do begin
            if (TableName = ltn) and (FieldName in lInvalidFields[ltn]) then
            begin
              try
                lOldValue := Value[j];
                ForceValue(j);
                iWrapper.Field[ltn, FieldName, j] := Value[j];
                FGridValues[i + 2, ARow, j]       := Display[j];
                // If value has been corrected
                if lOldValue[j] <> Value[j] then
                  lInvalidFields[ltn] := lInvalidFields[ltn] - [FieldName];
                //some fields come in sets, only one of which is required
                if (ltn = tnDetermination) and
                   (FieldName in [fnConceptKey, fnTaxonListItemKey]) then
                  lInvalidFields[ltn] := lInvalidFields[ltn] -
                                         [fnConceptKey, fnTaxonListItemKey];

                if (ltn = tnSample) and
                   (FieldName in [fnSpatialRef, fnLocationKey, fnLocationName]) then
                  lInvalidFields[ltn] := lInvalidFields[ltn] -
                                         [fnSpatialRef, fnLocationKey, fnLocationName]
              except
                on E: EQuickEntryControls do
                begin
                  lLastErrorMessage := E.Message;
                  lLastInvalidControl := TDataEntryControl(AList.Objects[i]);
                end;
              end;
            end;
          end;
        end;
      end; //for i := 0 to AList.Count -1
    end;
  end;

begin
  lLastInvalidControl := nil;
  iWrapper.Clear;
  sgQuickEntry.Row := ARow;
  //first, check the general controls to see if there are any default values
  PopulateWrapperWithGeneralOrHiddenFields(iWrapper, FGeneralEntryControls);
  PopulateWrapperWithGeneralOrHiddenFields(iWrapper, FHiddenEntryControls);
  // and then overwrite with the specimen row data
  PopulateWrapperWithSpecimenFields(iWrapper);
  if (FTaxonOccurrenceKey = '') and (FOccurrenceKey = '') then
    iWrapper.CreatedFromOccurrence := false
  else
    iWrapper.CreatedFromOccurrence := true;
  iWrapper.TaxonOccurrenceKey := FTaxonOccurrenceKey;
  iWrapper.OccurrenceKey := FOccurrenceKey;
    
  lInvalidFields := iWrapper.Validate;
  if iWrapper.HasAttachedData then
    for ltn := Low(TTableName) to High(TTableName) do
    begin
      GetValuesFromControlList(FGeneralEntryControls);
      GetValuesFromControlList(FHiddenEntryControls);
      GetValuesFromControlList(FSpecimenEntryControls);
      if lInvalidFields[ltn] <> [] then // still not valid, raise error
      begin
        if Assigned(lLastInvalidControl) then
        begin
          if not (lLastInvalidControl is TSpecimenDataEntryControl) then
          begin //Control is on the general tab
            if AFocusInvalidControl then begin
              pcMain.ActivePage := tsGeneral;
              if lLastInvalidControl.Control.CanFocus then
                lLastInvalidControl.Control.SetFocus;
            end;
            raise EQuickEntryException.CreateNonCritical(lLastErrorMessage);
          end
          else
          begin //Control is on the specimen tab
            with TSpecimenDataEntryControl(LLastInvalidControl) do
            begin
              if AFocusInvalidControl then begin
                pcMain.ActivePage := tsSpecimen; //show the control
                if lLastInvalidControl.Control.CanFocus then
                  lLastInvalidControl.Control.SetFocus;
              end;
              sgQuickEntry.Col := GridColumn;
              // Previous line messes it up, so fix it.
              if chkAsForm.Checked then ReplaceChkValidated;

              //some fields come in sets, only one of which is required
              //Alter warning accordingly
              if (ltn = tnSample) and
                 (FieldName in [fnSpatialRef, fnLocationKey, fnLocationName]) then
                raise EQuickEntryException.CreateNonCritical(ResStr_MissingSampleLocation)
              else
                raise EQuickEntryException.CreateNonCritical(lLastErrorMessage);
            end;//with LLastInvalidControl
          end;//if not (lLastInvalidControl is TSpecimenDataEntryControl)
        end
        else
        begin
          //The template doesn't have enough fields to enter a valid record.
          //This error will not occur if the template  has been created using
          //the QuickEntryManager
          raise EQuickEntryException.CreateNonCritical(ResStr_SelectedRowsInvalid);
        end;
      end;//if Assigned(lLastInvalidControl)
    end //for ltn := Low(TTableName) to High(TTableName)
  else
    // No data in record at all, so can't validate
    raise EQuickEntryException.CreateNonCritical(ResStr_SelectedRowsInvalid);
end;  // TfraQuickEntry.ValidateRow 

{-------------------------------------------------------------------------------
}
procedure TfraQuickEntry.WMValidateRow(var msg: TMessage);
begin
  try
    TryToValidateRow(msg.WParam, RowValidated[msg.WParam]);
    if (msg.WParam = sgQuickEntry.Row) and (sgQuickEntry.Col = VALIDATED_COLUMN) then
      chkValidated.Checked := RowValidated[msg.wParam];
  finally
    // prevent drag effects if there was a validation error
    FDataStringGrid.ForceMouseUp;
  end;
end;  // TfraQuickEntry.WMValidateRow

{-------------------------------------------------------------------------------
  If a validation checkbox is clicked, then ensure that the row validates.
}
procedure TfraQuickEntry.CheckBoxClick(ACol, ARow: integer);
begin
  // Need to delay the validation, as the grid sends this call during
  // mousedown or keydown, and we want the up event to occur first.
  if (ACol=VALIDATED_COLUMN) and (not FtfRowChanging) then
    PostMessage(Self.Handle, WM_VALIDATEROW, ARow, 0);
end;

{-------------------------------------------------------------------------------
  Retrieve an accession or registration number
}
procedure TfraQuickEntry.GetMacroNumber(Sender: TObject; var ANumber: string);
var
  lDepartmentName: string;
  lType: string;
begin
  Assert(Sender is TDataEntryControl);
  with Sender as TDataEntryControl do begin
    if DataType=CT_REGISTRATION then
      lType := 'Registration'
    else
      lType := 'Accession';
    lDepartmentName := GetDepartmentName(Sender is TSpecimenDataEntryControl);
    // Use the macro to generate a number.
    ANumber := dmGeneral.GetStoredProcOutputParam('usp_Macro_Get',
        ['@NumberType', lType,
        '@Dept', lDepartmentName],
        '@Macro');
  end;
end;

{-------------------------------------------------------------------------------
  Generate hidden registration and/or accession numbers for the specified row.
}
procedure TfraQuickEntry.GenerateMacroNumbersForHiddenFields(
  const ARowKey: String);
var
  I: Integer;
  lHiddenCtrl: TGeneralDataEntryControl;
  lMacroNumber: String;
begin
    for i := 0 to FHiddenEntryControls.Count - 1 do
    begin
      lHiddenCtrl := TGeneralDataEntryControl(FHiddenEntryControls.Objects[i]);
      if lHiddenCtrl.DataType in [CT_REGISTRATION, CT_ACCESSION] then
      begin
        GetMacroNumber(lHiddenCtrl, lMacroNumber);
        lHiddenCtrl.DataItemKey[0] := -1;
        lHiddenCtrl.Display[0]     := lMacroNumber;
        lHiddenCtrl.Value[0]       := lMacroNumber;
        lHiddenCtrl.UpdateDataItem(StrToInt(ARowKey), 0);
      end;
    end;
end;

{-------------------------------------------------------------------------------
  Retrieve the department name appropriate for a record.
}
function TfraQuickEntry.GetDepartmentName(AForSpecimenTab: boolean): string;
var
  lDepartmentUserID: string;
  lControl: TDataEntryControl;
begin
  lControl := FindControl(CT_DEPARTMENT, AForSpecimenTab);
  if Assigned(lControl) then begin
    if not (lControl.Value[0] = '') then
      Result := dmGeneral.GetStoredProcOutputParam(
                    'usp_DepartmentName_Get',
                    ['@NameKey', lControl.Value[0],
                    '@GetAcronym', 1],
                    '@FormattedName')
  end
  else
  begin
    lControl := FindControl(CT_STAFF_RESPONSIBLE, AForSpecimenTab);
    if Assigned(lControl) then
      lDepartmentUserID := lControl.Value[0]
    else
      lDepartmentUserID := AppSettings.UserID;

    // only use people from this organisation to get department name
    if dmGeneral.GetStoredProcOutputParam('usp_MemberOfHoldingOrg_Get',
          ['@Key', lDepartmentUserID], '@IsMember') then
      // Get the department name for the current user. Preferably get the acronym.
      Result := VarToStr(dmGeneral.GetStoredProcOutputParam
                                    ('usp_Department_Get_ForIndividual',
                                    ['@Key', lDepartmentUserID,
                                    '@GetAcronym', 1,
                                    '@ForceHoldingOrg', 1],
                                    '@Output'));
  end;
  // Set department to Unknown if no data available, wrapped in tags so we can
  // update it later if the department info is specified
  if Result = '' then Result := '#' + ResStr_Unknown + '#';
end;

{-------------------------------------------------------------------------------
}
procedure TfraQuickEntry.UpdateMacroNumber(AUpdatedControl: TDataEntryControl);
var
  lMacroNumberControl: TDataEntryControl;
  lDepartmentName: string;

    procedure RefreshMacroFieldOnRow(ARow: integer);
    var
      lDataItemKey: integer;
      lNewValue, lNewDisplay: string;
      lTimeStamp: TSQLSvrTimestamp;
    begin
      lDataItemKey := StrToInt(Trim(HiddenValue[lMacroNumberControl.TemplateFieldKey +
                    DATA_ITEM_KEY, ARow, 0]));
      // Ensure timestamp updated
      dmQuickEntry.RefreshDataItem(lDataItemKey, lNewValue,
                                   lNewDisplay, lTimestamp);
      TimestampValue[lMacroNumberControl.TemplateFieldKey + 'Timestamp', ARow, 0] := lTimestamp;
      // Update the data grid with the data from the database
      sgQuickEntry.Cells[TSpecimenDataEntryControl(lMacroNumberControl).GridColumn, ARow] := lNewDisplay;
      FGridValues[TSpecimenDataEntryControl(lMacroNumberControl).GridColumn, ARow, 0] := lNewDisplay;
      HiddenValue[lMacroNumberControl.TemplateFieldKey + 'Value', ARow, 0] := lNewValue;
    end;

    // update a single control's content if required.  Return true if updated
    function UpdateSingleControl(AControl: TDataEntryControl): boolean;
    begin
      if Pos('#' + ResStr_Unknown + '#', AControl.Display[0])>0 then begin
        AControl.Display[0] := StringReplace(AControl.Display[0], '#' + ResStr_Unknown + '#',
            lDepartmentName, []);
        AControl.Value[0] := StringReplace(AControl.Value[0], '#' + ResStr_Unknown + '#',
            lDepartmentName, []);
        Result := true;
      end
      else
        Result := false;
    end;

    procedure ProcessControl;
    var
      lRow: integer;
    begin
      if Assigned(lMacroNumberControl) then
      begin
        lDepartmentName := GetDepartmentName(lMacroNumberControl is TSpecimenDataEntryControl);
        if lDepartmentName <> '#' + ResStr_Unknown + '#' then
        begin
          if lMacroNumberControl is TGeneralDataEntryControl then
          begin
            if UpdateSingleControl(lMacroNumberControl) then
              UpdateGeneralDataEntryControl(TGeneralDataEntryControl(lMacroNumberControl));
          end else begin
            // for fields on the specimen tab, update all rows if the general tab changed,
            // or otherwise just the current one
            if AUpdatedControl is TGeneralDataEntryControl then
            begin
              // drop the current grid control, so it doesn't need a refresh
              FDataStringGrid.HideCustomEdit;
              FCurrentGridControl := nil;
              SaveRow(sgQuickEntry.Row);
              // perform search and replace, and return a list of the data items to update
              with dmGeneral.GetRecordset('usp_QESession_SearchAndReplace',
                  ['@QESessionKey',     FSessionKey,
                   '@TemplateFieldKey', lMacroNumberControl.TemplateFieldKey,
                   '@SearchText',       '#' + ResStr_Unknown + '#',
                   '@ReplaceText',      lDepartmentName]) do
              begin
                // walk through the data items and grid in synch, updating any that match
                lRow := 1;
                while not EOF do
                begin
                  while (lRow <= sgQuickEntry.RowCount) and
                        (HiddenValue[lMacroNumberControl.TemplateFieldKey + DATA_ITEM_KEY, lRow, 0] <> VarToStr(Fields[0].Value)) do
                    Inc(lRow);
                  RefreshMacroFieldOnRow(lRow);
                  MoveNext;
                end;
              end;
              LoadRow(sgQuickEntry.Row);
            end else
            begin
              if UpdateSingleControl(lMacroNumberControl) then
                SpecimenEventHandlerExit(lMacroNumberControl.Control);
            end;
          end;
        end;
      end;
    end;

begin
  // Do accession numbers - find controls that need to be updated
  lMacroNumberControl := FindControl(CT_ACCESSION, true);
  ProcessControl;
  if lMacroNumberControl is TSpecimenDataEntryControl then begin
    // ensure that a control repeated on both tabs is handled on both
    lMacroNumberControl := FindControl(CT_ACCESSION, false); // request from general tab
    ProcessControl;
  end;
  // repeat for registration numbers
  lMacroNumberControl := FindControl(CT_REGISTRATION, true);
  ProcessControl;
  if lMacroNumberControl is TSpecimenDataEntryControl then begin
    // ensure that a control repeated on both tabs is handled on both
    lMacroNumberControl := FindControl(CT_REGISTRATION, false); // request from general tab
    ProcessControl;
  end;
end;




{-------------------------------------------------------------------------------
  Identify the control containing the appropriate type of data (e.g. department)
}
function TfraQuickEntry.FindControl(ADataType: Integer; AForSpecimenTab: Boolean): TDataEntryControl;
var
  i: integer;
begin
  Result := nil;
  if AForSpecimenTab then
    for i := 0 to FSpecimenEntryControls.Count - 1 do
      if TDataEntryControl(FSpecimenEntryControls.Objects[i]).DataType = ADataType then
      begin
        Result := TDataEntryControl(FSpecimenEntryControls.Objects[i]);
        break; // from loop
      end;
  // if nothing on specimen tab or we are not looking there, then examine
  // general tab as well
  if not Assigned(Result) then
    for i := 0 to FGeneralEntryControls.Count - 1 do
      if TDataEntryControl(FGeneralEntryControls.Objects[i]).DataType = ADataType then
      begin
        Result := TDataEntryControl(FGeneralEntryControls.Objects[i]);
        break; // from loop
      end;

  // if still not found anything, look at the hidden controls
  if not Assigned(Result) then
    for i := 0 to FHiddenEntryControls.Count - 1 do
      if TDataEntryControl(FHiddenEntryControls.Objects[i]).DataType = ADataType then
      begin
        Result := TDataEntryControl(FHiddenEntryControls.Objects[i]);
        break; // from loop
      end;
end;

end.


