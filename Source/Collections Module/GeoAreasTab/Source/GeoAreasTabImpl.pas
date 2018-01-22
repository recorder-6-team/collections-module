unit GeoAreasTabImpl;

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ActiveX, AxCtrls, GeoAreasTab_TLB, StdVcl, Recorder2000_TLB, LuxembourgDataClasses,
  DataClasses, ADOInt, Variants, ResourceStrings, ExceptionForm,
  GeneralData, AddinSearchManager, BaseCompositeComponent, LinkedControls,
  StdCtrls, ImageListButton, Grids, DssStringGrid, AddinInterfaceDataModule,
  DropControlAssistor, LuxembourgConstants, ThesaurusBrowser_TLB;

resourcestring
  ResStr_GeoAreasAddinDescription = 'Geographic Areas Tab for Surveys. Version %s';
  ResStr_GeoAreasAddinName = 'Geographic Areas Tab';
  ResStr_GeoAreas = 'Geographic Area';
  ResStr_SupplyValidGeoAreaForAllRows =
      'Please supply a valid geographic area for all rows in the grid.';
  ResStr_ConceptNotInGeoArea = 'The selected concept is not from a valid geographic area.';

type
  EValidationFailure = class(TExceptionPath);

  TRequestDataRecipientMethod = procedure (const AKeyList: IKeyList) of object;

  TGeoAreaItem = class(TLuxGridDataItem)
  private
    FTerm: string;
    FTermKey: TKeyString;
    FConceptKey: TKeyString;
    FSEGeoAreaKey: TKeyString;
  protected
    procedure GetData(const Column: Integer; var AText: String; var ANameKey: TKeyString);
        override;
    procedure InitFromRecord(AFields: Fields); override;
    procedure SetData(const Column: Integer; const AText: String; const AKey: TKeyString);
        override;
    procedure ValidateData; override;
    function DoCheckUnique(var AKey, AText: String; ASearchType: Integer;
        const ASearchKey: String = ''; const AAdditionalCaption: String= ''): Boolean;
  public
    property Term: string read FTerm;
    property TermKey: TKeyString read FTermKey;
    property ConceptKey: TKeyString read FConceptKey write FConceptKey;
    property SEGeoAreaKey: TKeyString read FSEGeoAreaKey write FSEGeoAreaKey;
  end;
  
  TGeoAreaList = class(TLuxGridDataList)
  private
    FSurveyEventKey: TKeyString;
  protected
    procedure DoAddition(AItem: TLuxCachedDataItem); overload; override;
    procedure DoDeletion(AItem: TLuxCachedDataItem); override;
    procedure DoModification(AItem: TLuxCachedDataItem); overload; override;
    function GetRecordset: _Recordset; overload; override;
  public
    property SurveyEventKey: TKeyString read FSurveyEventKey write FSurveyEventKey;
  end;

  TGeoAreasTabImpl = class(TActiveForm, IGeoAreasTabImpl, IRecorderAddin, IAdditionalPage,
                  IAdditionalPage6, IRecorderFormEvents, IRequestor)
    sgAreas: TDSSStringGrid;
    btnAdd: TImageListButton;
    btnRemove: TImageListButton;
    eName: TLinkedEdit;
    lblPath: TLabel;
    procedure IRequestor.Update = UpdateRequestedData;
    procedure btnAddClick(Sender: TObject);
    procedure btnRemoveClick(Sender: TObject);
    procedure eNameExit(Sender: TObject);
    procedure eNameFindData(Sender: TObject);
    procedure eNameGetData(Sender: TObject);
    procedure eNameKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure sgAreasKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure sgAreasSelectCell(Sender: TObject; ACol, ARow: Integer;
      var CanSelect: Boolean);
    procedure eNameChange(Sender: TObject);
  private
    FEvents: IGeoAreasTabImplEvents;
    FGeoAreaList: TGeoAreaList;
    FLoading: Boolean;
    FSaving: Boolean;
    FSurveyEventKey: TKeyString;
    FCurrentKey: string;
    FCurrentTable: string;
    FEditMode: Boolean;
    FKey: TKeyString;
    FRequestDataRecipientLinkedEdit: TLinkedEdit;
    FRequestDataRecipientMethod: TRequestDataRecipientMethod;
    FDropControlAssistor: TDropControlAssistor;

    function CanAddRow: Boolean;
    function CheckForDuplicates: Boolean;
    function ConceptInGeographySubjectArea(const conceptKey: String): Boolean;
    procedure EnableControls(AEnabled: Boolean);

    procedure SaveData;
    procedure SetKey(Value: TKeyString);
    procedure ValidateData;

    procedure InitialSetup;

    procedure ActivateEvent(Sender: TObject);
    procedure ClickEvent(Sender: TObject);
    procedure CreateEvent(Sender: TObject);
    procedure DblClickEvent(Sender: TObject);
    procedure DeactivateEvent(Sender: TObject);
    procedure DestroyEvent(Sender: TObject);
    procedure KeyPressEvent(Sender: TObject; var Key: Char);
    procedure PaintEvent(Sender: TObject);
    function InitReturnData(const ADataType: String): IUnknown; overload;
    procedure UpdateTerm(const AKeyList: IKeyList);
    procedure ShowHierarchyPath(const AConceptKey: string);
  protected
    procedure WndProc(var Message: TMessage); override;
    procedure WMSetDragDrop(var Message: TMessage); message WM_SETDRAGDROP;
    procedure DragOverCheck(APoint: TPoint; const ATable, AFieldKey: String; var Accept: Boolean);
    procedure DropKeyword(const Sender: TObject; const iFormat: integer;
        const iSourceData: TKeyList; const iTextStrings: TstringList; var ioHandled: boolean);
    procedure UpdateRequestedData(const KeyList: IKeyList); safecall;
    function InitReturnData(ARecipientMethod: TRequestDataRecipientMethod; const
        ADataType: String): IUnknown; overload;
    procedure DefinePropertyPages(DefinePropertyPage: TDefinePropertyPage); override;
    procedure EventSinkChanged(const EventSink: IUnknown); override;
    function Get_Active: WordBool; safecall;
    function Get_AlignDisabled: WordBool; safecall;
    function Get_AutoScroll: WordBool; safecall;
    function Get_AutoSize: WordBool; safecall;
    function Get_AxBorderStyle: TxActiveFormBorderStyle; safecall;
    function Get_Caption: WideString; safecall;
    function Get_Color: OLE_COLOR; safecall;
    function Get_DoubleBuffered: WordBool; safecall;
    function Get_DropTarget: WordBool; safecall;
    function Get_Enabled: WordBool; safecall;
    function Get_Font: IFontDisp; safecall;
    function Get_HelpFile: WideString; safecall;
    function Get_KeyPreview: WordBool; safecall;
    function Get_PixelsPerInch: Integer; safecall;
    function Get_PrintScale: TxPrintScale; safecall;
    function Get_Scaled: WordBool; safecall;
    function Get_ScreenSnap: WordBool; safecall;
    function Get_SnapBuffer: Integer; safecall;
    function Get_Visible: WordBool; safecall;
    function Get_VisibleDockClientCount: Integer; safecall;
    procedure _Set_Font(var Value: IFontDisp); safecall;
    procedure Set_AutoScroll(Value: WordBool); safecall;
    procedure Set_AutoSize(Value: WordBool); safecall;
    procedure Set_AxBorderStyle(Value: TxActiveFormBorderStyle); safecall;
    procedure Set_Caption(const Value: WideString); safecall;
    procedure Set_Color(Value: OLE_COLOR); safecall;
    procedure Set_DoubleBuffered(Value: WordBool); safecall;
    procedure Set_DropTarget(Value: WordBool); safecall;
    procedure Set_Enabled(Value: WordBool); safecall;
    procedure Set_Font(const Value: IFontDisp); safecall;
    procedure Set_HelpFile(const Value: WideString); safecall;
    procedure Set_KeyPreview(Value: WordBool); safecall;
    procedure Set_PixelsPerInch(Value: Integer); safecall;
    procedure Set_PrintScale(Value: TxPrintScale); safecall;
    procedure Set_Scaled(Value: WordBool); safecall;
    procedure Set_ScreenSnap(Value: WordBool); safecall;
    procedure Set_SnapBuffer(Value: Integer); safecall;
    procedure Set_Visible(Value: WordBool); safecall;
    // IRecorderAddin                       
    function Get_Description: WideString; safecall;
    function Get_ImageFileName: WideString; safecall;
    function Get_Name: WideString; safecall;
    procedure Install(const iInstalledFilePath: WideString); safecall;
    // IAdditionalPage
    function Get_CurrentKey: WideString; safecall;
    function Get_CurrentTable: WideString; safecall;
    function Get_Form: WideString; safecall;
    function Get_PageCaption: WideString; safecall;
    procedure Set_CurrentKey(const Value: WideString); safecall;
    procedure Set_CurrentTable(const Value: WideString); safecall;
    // IAdditionalPage6
    function Get_TabIndex: Integer; safecall;
    // IRecorderFormEvents
    function Get_FormName: WideString; safecall;
    function CheckCanSave: WordBool; safecall;
    procedure DoAdd; safecall;
    procedure DoCancel; safecall;
    procedure DoDelete; safecall;
    procedure DoEditMode; safecall;
    procedure DoItemChange(const iTableName: WideString; const iKeyValue: WideString); safecall;
    procedure DoSave; safecall;
    procedure SetForm(const iForm: IRecorderForm); safecall;
  public
    procedure Initialize; override;
    destructor Destroy; override;
    property Key: TKeyString read FKey write SetKey;
  end;

implementation

uses
  ComObj, ComServ, VersionInfo, Registry, DropTarget, GeneralFunctions;

{$R *.DFM}

{-==============================================================================
    TGeoAreaItem
===============================================================================}
{-------------------------------------------------------------------------------
}
procedure TGeoAreaItem.GetData(const Column: Integer; var AText: String; var ANameKey:
    TKeyString);
begin
  AText := Term;
  ANameKey := ConceptKey;
end;  // TGeoAreaItem.GetData

{-------------------------------------------------------------------------------
}
procedure TGeoAreaItem.InitFromRecord(AFields: Fields);
begin
  SetItemKey(VarToStr(AFields['Survey_Event_Key'].Value));
  FTermKey := AFields['Term_Key'].Value;
  FTerm := AFields['Plaintext'].Value;
  FConceptKey := AFields['Concept_Key'].Value;
  FSEGeoAreaKey := AFields['Survey_Event_Geo_Area_Key'].Value;
end;  // TGeoAreaItem.InitFromRecord 

{-------------------------------------------------------------------------------
}
procedure TGeoAreaItem.SetData(const Column: Integer; const AText: String; const AKey:
    TKeyString);
begin
  FTerm := AText;
  FConceptKey := AKey;
  SetModified;
end;  // TGeoAreaItem.SetData 

{----------------------------------------0---------------------------------------
}
procedure TGeoAreaItem.ValidateData;
var
  lText, lKey: String;
begin
  if (FTermKey = '') and (FTerm <> '') then begin
    lText := FTerm;
    if DoCheckUnique(lKey, lText, 8, '', ResStr_Term) then begin
      // Found a unique exact match. Update fields accordingly.
      FTermKey := lKey;
      FTerm := lText;
    end;
  end;
  ValidateValue(FTermKey <> '', Format(ResStr_InvalidNameInGrid, [FTerm, 'Geo_Areas']));
end;  // TGeoAreaItem.ValidateData

{-------------------------------------------------------------------------------
  Search for a unique item and return a Key/Text values independently from any kind
  of controls.
}
function TGeoAreaItem.DoCheckUnique(var AKey, AText: String; ASearchType: Integer;
  const ASearchKey: String = ''; const AAdditionalCaption: String= ''): Boolean;
begin
  with TAddinSearchManager.Create do
    try
      SearchType := ASearchType;
      SearchKey  := ASearchKey;
      AdditionalCaption := AAdditionalCaption;
      // Get the key, if there is one coming back...
      AKey := FindUnique(AText);
      // Success?
      Result := AKey <> '';
      // If so, update Text too.
      if Result then AText := ResultText;
    finally
      Free;                     
    end;
end;

{-==============================================================================
    TGeoAreaList
===============================================================================}
{-------------------------------------------------------------------------------
}
procedure TGeoAreaList.DoAddition(AItem: TLuxCachedDataItem);
begin
  with TGeoAreaItem(AItem) do begin
    SEGeoAreaKey := VarToStr(dmGeneral.RunInsertStoredProc('Survey_Event_Geo_Area',
                    'usp_SurveyEventGeoArea_Insert',
                   ['@Key', SEGeoAreaKey,
                    '@SurveyEventKey', SurveyEventKey,
                    '@ConceptKey', ConceptKey,
                    '@EnteredSessionID', dmGeneral.Recorder.CurrentSettings.SessionID
                   ], '@Key'));
  end;
end;  // TGeoAreaList.DoAddition

{-------------------------------------------------------------------------------
}
procedure TGeoAreaList.DoDeletion(AItem: TLuxCachedDataItem);
begin
  with TGeoAreaItem(AItem) do
    dmGeneral.RunDeleteStoredProc('usp_SurveyEventGeoArea_Delete',
                                  ['@SurveyEventGeoAreaKey', SEGeoAreaKey]);
end;  // TGeoAreaList.DoDeletion

{-------------------------------------------------------------------------------
  We always want to run the Insert method, but for modifications, we had better run the Delete
      method as well beforehand.
}
procedure TGeoAreaList.DoModification(AItem: TLuxCachedDataItem);
begin
  DoDeletion(AItem);
  DoAddition(AItem);
end;  // TGeoAreaList.DoModification

{-------------------------------------------------------------------------------
}
function TGeoAreaList.GetRecordset: _Recordset;
begin
  Result := dmGeneral.GetRecordset('usp_SurveyEventGeoArea_Select', ['@Key', MasterKey]);
end;  // TGeoAreaList.GetRecordset

{-==============================================================================
    TGeoAreasTabImpl
===============================================================================}

procedure TGeoAreasTabImpl.DefinePropertyPages(DefinePropertyPage: TDefinePropertyPage);
begin
  { Define property pages here.  Property pages are defined by calling
    DefinePropertyPage with the class id of the page.  For example,
      DefinePropertyPage(Class_GeoAreasTabImplPage); }
end;

procedure TGeoAreasTabImpl.EventSinkChanged(const EventSink: IUnknown);
begin
  FEvents := EventSink as IGeoAreasTabImplEvents;
  inherited EventSinkChanged(EventSink);
end;

procedure TGeoAreasTabImpl.Initialize;
begin
  inherited Initialize;
  OnActivate := ActivateEvent;
  OnClick := ClickEvent;
  OnCreate := CreateEvent;
  OnDblClick := DblClickEvent;
  OnDeactivate := DeactivateEvent;
  OnDestroy := DestroyEvent;
  OnKeyPress := KeyPressEvent;
  OnPaint := PaintEvent; 

  FDropControlAssistor := TDropControlAssistor.Create(Self);
end;

function TGeoAreasTabImpl.Get_Active: WordBool;
begin
  Result := Active;
end;

function TGeoAreasTabImpl.Get_AlignDisabled: WordBool;
begin
  Result := AlignDisabled;
end;

function TGeoAreasTabImpl.Get_AutoScroll: WordBool;
begin
  Result := AutoScroll;
end;

function TGeoAreasTabImpl.Get_AutoSize: WordBool;
begin
  Result := AutoSize;
end;

function TGeoAreasTabImpl.Get_AxBorderStyle: TxActiveFormBorderStyle;
begin
  Result := Ord(AxBorderStyle);
end;

function TGeoAreasTabImpl.Get_Caption: WideString;
begin
  Result := WideString(Caption);
end;

function TGeoAreasTabImpl.Get_Color: OLE_COLOR;
begin
  Result := OLE_COLOR(Color);
end;

function TGeoAreasTabImpl.Get_DoubleBuffered: WordBool;
begin
  Result := DoubleBuffered;
end;

function TGeoAreasTabImpl.Get_DropTarget: WordBool;
begin
  Result := DropTarget;
end;

function TGeoAreasTabImpl.Get_Enabled: WordBool;
begin
  Result := Enabled;
end;

function TGeoAreasTabImpl.Get_Font: IFontDisp;
begin
  GetOleFont(Font, Result);
end;

function TGeoAreasTabImpl.Get_HelpFile: WideString;
begin
  Result := WideString(HelpFile);
end;

function TGeoAreasTabImpl.Get_KeyPreview: WordBool;
begin
  Result := KeyPreview;
end;

function TGeoAreasTabImpl.Get_PixelsPerInch: Integer;
begin
  Result := PixelsPerInch;
end;

function TGeoAreasTabImpl.Get_PrintScale: TxPrintScale;
begin
  Result := Ord(PrintScale);
end;

function TGeoAreasTabImpl.Get_Scaled: WordBool;
begin
  Result := Scaled;
end;

function TGeoAreasTabImpl.Get_ScreenSnap: WordBool;
begin
  Result := ScreenSnap;
end;

function TGeoAreasTabImpl.Get_SnapBuffer: Integer;
begin
  Result := SnapBuffer;
end;

function TGeoAreasTabImpl.Get_Visible: WordBool;
begin
  Result := Visible;
end;

function TGeoAreasTabImpl.Get_VisibleDockClientCount: Integer;
begin
  Result := VisibleDockClientCount;
end;

procedure TGeoAreasTabImpl._Set_Font(var Value: IFontDisp);
begin
  SetOleFont(Font, Value);
end;

procedure TGeoAreasTabImpl.ActivateEvent(Sender: TObject);
begin
  if FEvents <> nil then FEvents.OnActivate;
end;

procedure TGeoAreasTabImpl.ClickEvent(Sender: TObject);
begin
  if FEvents <> nil then FEvents.OnClick;
end;

procedure TGeoAreasTabImpl.CreateEvent(Sender: TObject);
begin
  if FEvents <> nil then FEvents.OnCreate;
end;

procedure TGeoAreasTabImpl.DblClickEvent(Sender: TObject);
begin
  if FEvents <> nil then FEvents.OnDblClick;
end;

procedure TGeoAreasTabImpl.DeactivateEvent(Sender: TObject);
begin
  if FEvents <> nil then FEvents.OnDeactivate;
end;

procedure TGeoAreasTabImpl.DestroyEvent(Sender: TObject);
begin
  if FEvents <> nil then FEvents.OnDestroy;
end;

procedure TGeoAreasTabImpl.KeyPressEvent(Sender: TObject; var Key: Char);
var
  TempKey: Smallint;
begin
  TempKey := Smallint(Key);
  if FEvents <> nil then FEvents.OnKeyPress(TempKey);
  Key := Char(TempKey);
end;

procedure TGeoAreasTabImpl.PaintEvent(Sender: TObject);
begin
  if FEvents <> nil then FEvents.OnPaint;
end;

procedure TGeoAreasTabImpl.Set_AutoScroll(Value: WordBool);
begin
  AutoScroll := Value;
end;

procedure TGeoAreasTabImpl.Set_AutoSize(Value: WordBool);
begin
  AutoSize := Value;
end;

procedure TGeoAreasTabImpl.Set_AxBorderStyle(
  Value: TxActiveFormBorderStyle);
begin
  AxBorderStyle := TActiveFormBorderStyle(Value);
end;

procedure TGeoAreasTabImpl.Set_Caption(const Value: WideString);
begin
  Caption := TCaption(Value);
end;

procedure TGeoAreasTabImpl.Set_Color(Value: OLE_COLOR);
begin
  Color := TColor(Value);
end;

procedure TGeoAreasTabImpl.Set_DoubleBuffered(Value: WordBool);
begin
  DoubleBuffered := Value;
end;

procedure TGeoAreasTabImpl.Set_DropTarget(Value: WordBool);
begin
  DropTarget := Value;
end;

procedure TGeoAreasTabImpl.Set_Enabled(Value: WordBool);
begin
  Enabled := Value;
end;

procedure TGeoAreasTabImpl.Set_Font(const Value: IFontDisp);
begin
  SetOleFont(Font, Value);
end;

procedure TGeoAreasTabImpl.Set_HelpFile(const Value: WideString);
begin
  HelpFile := String(Value);
end;

procedure TGeoAreasTabImpl.Set_KeyPreview(Value: WordBool);
begin
  KeyPreview := Value;
end;

procedure TGeoAreasTabImpl.Set_PixelsPerInch(Value: Integer);
begin
  PixelsPerInch := Value;
end;

procedure TGeoAreasTabImpl.Set_PrintScale(Value: TxPrintScale);
begin
  PrintScale := TPrintScale(Value);
end;

procedure TGeoAreasTabImpl.Set_Scaled(Value: WordBool);
begin
  Scaled := Value;
end;

procedure TGeoAreasTabImpl.Set_ScreenSnap(Value: WordBool);
begin
  ScreenSnap := Value;
end;

procedure TGeoAreasTabImpl.Set_SnapBuffer(Value: Integer);
begin
  SnapBuffer := Value;
end;

procedure TGeoAreasTabImpl.Set_Visible(Value: WordBool);
begin
  Visible := Value;
end;
{-------------------------------------------------------------------------------
  Destructor - cleanup
}
destructor TGeoAreasTabImpl.Destroy;
begin       
  FDropControlAssistor.Free;
  FGeoAreaList.Free;
  FRequestDataRecipientLinkedEdit.Free;
  inherited;
end;  // TGeoAreasTabImpl.Destroy

{-------------------------------------------------------------------------------
  Perform validation
}
function TGeoAreasTabImpl.CheckCanSave: WordBool;
begin
  Result := true;
  try
    ValidateData;
  except
    on E:EValidationFailure do begin
      ShowInformation(E.Message);
      Result := false;
    end;
  end; // try
end;  // TGeoAreasTabImpl.CheckCanSave

{-------------------------------------------------------------------------------
  Addition of a new record
}
procedure TGeoAreasTabImpl.DoAdd;
begin
  DoItemChange('', '');
  EnableControls(True);
end;  // TGeoAreasTabImpl.DoAdd

{-------------------------------------------------------------------------------
  Cancellation of an update to the entire reference
}
procedure TGeoAreasTabImpl.DoCancel;
begin
  FEditMode := False;
  EnableControls(False);
  FGeoAreaList.Refresh;
end;  // TGeoAreaList.DoCancel

{-------------------------------------------------------------------------------
  Deletion of a reference - delete associated data occurs through a cascading
      delete
}
procedure TGeoAreasTabImpl.DoDelete;
begin

end;  // TGeoAreaList.DoDelete

{-------------------------------------------------------------------------------
  Go into edit mode on the entire reference (not on an individual supplier
      record though)
}
procedure TGeoAreasTabImpl.DoEditMode;
begin
  FEditMode := true;
  FGeoAreaList.Refresh;
  EnableControls(True);
end;  // TGeoAreaList.DoEditMode

{-------------------------------------------------------------------------------
  Load information for a new source
}
procedure TGeoAreasTabImpl.DoItemChange(const iTableName, iKeyValue: WideString);
begin
  eName.Text := '';
  FGeoAreaList.MasterKey := iKeyValue;
  FSurveyEventKey := iKeyValue;
  FGeoAreaList.Refresh;
  lblPath.Caption := '';
end;  //TGeoAreasTabImpl.DoItemChange

{-------------------------------------------------------------------------------
  Persist the changes to the database.
}
procedure TGeoAreasTabImpl.DoSave;
begin
  FSaving := True;
  SaveData;
  EnableControls(false);
  FGeoAreaList.Refresh;
  FSaving := False;
end;  // TGeoAreasTabImpl.DoSave

{-------------------------------------------------------------------------------
  Accessor
}
function TGeoAreasTabImpl.Get_CurrentKey: WideString;
begin
  Result := FCurrentKey;
end;  // TGeoAreasTabImpl.Get_CurrentKey

{-------------------------------------------------------------------------------
  Accessor
}
function TGeoAreasTabImpl.Get_CurrentTable: WideString;
begin
  Result := FCurrentTable;
end;  // TGeoAreasTabImpl.Get_CurrentTable

{-------------------------------------------------------------------------------
  Description and version number of the addin
}
function TGeoAreasTabImpl.Get_Description: WideString;
var
  lKey: String;
  lPath: String;
begin
  lKey := Format(
      'CLSID\%s\InprocServer32',
      [GUIDToString(CLASS_GeoAreasTabImpl)]);
      
  lPath := GetRegStringValue(lKey, '');

  Result := Format(
      ResStr_GeoAreasAddinDescription,
      [VersionInfo.GetFileVersion(lPath)]);
end;  // TGeoAreasTabImpl.Get_Name

{-------------------------------------------------------------------------------
  Form that this tab is bound to
}
function TGeoAreasTabImpl.Get_Form: WideString;
begin
  Result := 'TfrmEventDetails';
end;  // TGeoAreasTabImpl.Get_Form 

{-------------------------------------------------------------------------------
  Retrieve the form that we want events for - occurrences
}
function TGeoAreasTabImpl.Get_FormName: WideString;
begin
  Result := 'TfrmEventDetails';
end;  // TGeoAreasTabImpl.Get_FormName

{-------------------------------------------------------------------------------
  Addin icon
}
function TGeoAreasTabImpl.Get_ImageFileName: WideString;
begin
  Result := 'Default.bmp';
end;  // TGeoAreasTabImpl.Get_ImageFileName

{-------------------------------------------------------------------------------
  Addin identifier
}
function TGeoAreasTabImpl.Get_Name: WideString;
begin
  Result := ResStr_GeoAreasAddinName;
end;  // TGeoAreasTabImpl.Get_Name

{-------------------------------------------------------------------------------
  Title of the tab page
}
function TGeoAreasTabImpl.Get_PageCaption: WideString;
begin
  Result := ResStr_GeoAreas;
  // Initial Setup is done now, to avoid doing anything when the com object
  // is instantiated just to query its interfaces
  InitialSetup;
end;  // TGeoAreasTabImpl.Get_PageCaption

{-------------------------------------------------------------------------------
  Index of the tab page
}
function TGeoAreasTabImpl.Get_TabIndex: Integer;
begin
  Result := 2;
end;  // TGeoAreasTabImpl.Get_Tab_Index

{-------------------------------------------------------------------------------
  Set up the controls
}
procedure TGeoAreasTabImpl.InitialSetup;
begin
  sgAreas.Cells[0,0] := 'Geo. Areas';
  FGeoAreaList := TGeoAreaList.Create(TGeoAreaItem, sgAreas);
  // Setting the control sets the type to custom.
  sgAreas.ColumnsInfo[0].WinControl := eName;
  EnableControls(False);

  FLoading := False;
  FSaving := False;
end;  // TGeoAreasTabImpl.InitialSetup

{-------------------------------------------------------------------------------
  Initialise controls that can be dropped onto.
}
procedure TGeoAreasTabImpl.WMSetDragDrop(var Message: TMessage);
begin
  FDropControlAssistor.RegisterDropComponent(
      sgAreas, DropKeyword, nil, [TN_CONCEPT], [CF_JNCCDATA], DragOverCheck);
end;

{-------------------------------------------------------------------------------
  Method called when addin initially installed
}
procedure TGeoAreasTabImpl.Install(const iInstalledFilePath: WideString);
begin
  // no action
end;  // TGeoAreasTabImpl.Install

{-------------------------------------------------------------------------------
  Accessor.
}
procedure TGeoAreasTabImpl.Set_CurrentKey(const Value: WideString);
begin
  FCurrentKey := Value;
end;  // TGeoAreasTabImpl.Set_CurrentKey

{-------------------------------------------------------------------------------
  Accessor
}
procedure TGeoAreasTabImpl.Set_CurrentTable(const Value: WideString);
begin
  FCurrentTable := Value;
end;  // TGeoAreasTabImpl.Set_CurrentTable 

{-------------------------------------------------------------------------------
  Take no action when Recorder passes the references form to us, as we don't
      make any changes to the existing form, just add new stuff.
}
procedure TGeoAreasTabImpl.SetForm(const iForm: IRecorderForm);
begin
  //
end;  // TGeoAreasTabImpl.SetForm

{-------------------------------------------------------------------------------
  Handle a click on the add button. 
}
procedure TGeoAreasTabImpl.btnAddClick(Sender: TObject);
begin
  inherited;
  if CanAddRow then begin
    FGeoAreaList.AddNew(TGeoAreaItem.CreateNew(FGeoAreaList));
    sgAreas.Row := FGeoAreaList.ItemCount;
  end else
    sgAreas.Row := sgAreas.RowCount - 1;
end;  // TGeoAreasTabImpl.btnAddClick

{-------------------------------------------------------------------------------
}
function TGeoAreasTabImpl.CanAddRow: Boolean;
begin
  with sgAreas do
    Result := ((Row = RowCount - 1) and (eName.Text <> '')) or
              (Cells[0, RowCount - 1] <> '');
end;  // TGeoAreasTabImpl.CanAddRow

{-------------------------------------------------------------------------------
  Handle a click on the remove button. 
}
procedure TGeoAreasTabImpl.btnRemoveClick(Sender: TObject);
begin
  inherited;
  with sgAreas do
    if (Cells[Col, Row] <> '') or (eName.Text <> '') then begin
      if MessageDlg(Format(ResStr_ConfirmRowDelete, [ResStr_Row]),
                    mtWarning, [mbYes, mbNo], 0) = mrYes then
        FGeoAreaList.DeleteItem(Row);
    end else
      FGeoAreaList.DeleteItem(Row);
end;  // TGeoAreasTabImpl.btnRemoveClick

{-------------------------------------------------------------------------------
  Set the colours of the controls in edit mode.
}
procedure TGeoAreasTabImpl.EnableControls(AEnabled: Boolean);
begin
  inherited;
  sgAreas.ReadOnly := not AEnabled;
  btnAdd.Enabled := AEnabled;
  btnRemove.Enabled := btnAdd.Enabled and (sgAreas.Cells[0, 1] <> '');
end;  // TGeoAreasTabImpl.EnableControls

{-------------------------------------------------------------------------------
  Make sure the contents of the Geo Area linked edit is parsed when the user exits it.
}
procedure TGeoAreasTabImpl.eNameExit(Sender: TObject);
begin
  inherited;
  btnRemove.Enabled := btnAdd.Enabled and ((sgAreas.Cells[0, 1] <> '') or (eName.Text <> ''));
end;  // TGeoAreasTabImpl.eNameExit
 
{-------------------------------------------------------------------------------
  Handler for OnFindData for the floating linked edit control on the string grid.
}
procedure TGeoAreasTabImpl.eNameFindData(Sender: TObject);
var
  lTermKey: TKeyString;
  lText, lKey: string;
  lSearchSuccess: Boolean;
begin
  inherited;
  if (eName.Key = '') and (eName.Text <> '') then begin
    lText := eName.Text;

    with TAddinSearchManager.Create do
      try
        SearchType := ST_TERMINSUBJECTAREA;
        SearchKey  := CG_GEO_SUBJECT;
        AdditionalCaption := '';
        // Get the key, if there is one coming back...
        lKey := RunSearch(lText);
        // Success?
        lSearchSuccess := lKey <> '';
        // If so, update Text too.
        if lSearchSuccess then lText := ResultText;
      finally
        Free;
      end;

    eName.Key := lKey;
    eName.Text := lText;
  end;

  lTermKey := eName.Key;
  if CheckForDuplicates then begin
    eName.Text := '';
    eName.Key := '';
  end;
end;  // TGeoAreasTabImpl.eNameFindData

{-------------------------------------------------------------------------------
  Get data from the Geo Areas module.
}
procedure TGeoAreasTabImpl.eNameGetData(Sender: TObject);
var
  lIntf: IUnknown;
  lThesaurus: IfrmThesaurusBrowser;
  lConceptGroupKey: TKeyString;
begin
  inherited;
  lIntf := InitReturnData(UpdateTerm, GUIDToString(CLASS_frmThesaurusBrowser));
  // Ask the thesaurus to display the materials concept group
  if Assigned(lIntf) then begin
    if Supports(lIntf, IID_IfrmThesaurusBrowser, lThesaurus) then begin
      lConceptGroupKey := VarToStr(dmGeneral.GetStoredProcOutputParam(
          'usp_FindFirstConceptGroupForSubject',
          ['@SubjectAreaKey', CG_GEO_SUBJECT],
          '@ConceptGroupKey'));
      lThesaurus.DisplayConceptGroup(lConceptGroupKey);
    end;    // if Supports(lIntf, IID_IfrmThesaurusBrowser, lThesaurus)
  end;    // if Assigned(lIntf)
end;  // TGeoAreasTabImpl.eNameGetData

{-------------------------------------------------------------------------------
}
procedure TGeoAreasTabImpl.eNameKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  inherited;
  if (Key in [VK_DELETE, VK_BACK]) and (eName.Text = '') then begin
    // Need to get rid of some messages in queue first.
    Application.ProcessMessages;
    // Now remove.
    FGeoAreaList.DeleteItem(sgAreas.Row);
    Key := 0;
  end;
end;  // TGeoAreasTabImpl.eNameKeyDown

{-------------------------------------------------------------------------------
  Allows an F9 - Request data link to be setup.  The method supplied is responsible for
      handling the item returned.  The data type supplied specifies which screen the data is
      requested from.
  Returns the addin instance if a COM object used to get data from.
}
function TGeoAreasTabImpl.InitReturnData(ARecipientMethod:
    TRequestDataRecipientMethod; const ADataType: String): IUnknown;
begin
  FRequestDataRecipientMethod := ARecipientMethod;
  FRequestDataRecipientLinkedEdit := nil;
  Result := InitReturnData(ADataType);
end;  // TGeoAreasTabImpl.InitReturnData

{-------------------------------------------------------------------------------
  Internal method for initialising a return data link.
  Returns the addin instance if a COM object used to get data from.
}
function TGeoAreasTabImpl.InitReturnData(const ADataType: String): IUnknown; 
var
  lGUID: TGUID;
begin
  try
    // If conversion fails, it's a normal string.
    lGUID := StringToGUID(ADataType);
    with CreateCOMObject(CLASS_AutoApplicationSettings) as IRecorder2000 do
      Result := RequestCOMData(Self as IRequestor, lGUID);
  except
    on EOleSysError do begin
      with CreateCOMObject(CLASS_AutoApplicationSettings) as IRecorder2000 do
        RequestData(Self as IRequestor, ADataType);
      Result := nil;
    end;
  end;
end;  // TGeoAreasTabImpl.InitReturnData

{-------------------------------------------------------------------------------
}
procedure TGeoAreasTabImpl.UpdateTerm(const AKeyList: IKeyList);
var
  lKey: TKeyString;
begin
  lKey := AKeyList.GetKeyItem(0).KeyField1;

  if ConceptInGeographySubjectArea(lKey) then begin
    eName.Key := lKey;
    eName.Text := VarToStr(dmGeneral.GetStoredProcOutputParam('usp_ConceptName_Get',
                       ['@Key', lKey], '@Caption'));
    if CheckForDuplicates then begin
      eName.Text := '';
      eName.Key := '';
    end;
  end else
    ShowInformation(ResStr_ConceptNotInGeoArea);
end;  // TGeoAreasTabImpl.UpdateTerm

{-------------------------------------------------------------------------------
}
procedure TGeoAreasTabImpl.sgAreasKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  inherited;
  with sgAreas do
    if (Row = RowCount - 1) and (Key = VK_DOWN) and not CanAddRow then Key := 0;
end;  // TGeoAreasTabImpl.sgAreasKeyDown

{-------------------------------------------------------------------------------
  Saves the data to the database.
}
procedure TGeoAreasTabImpl.SaveData;
begin
  inherited;

  FGeoAreaList.SurveyEventKey := FSurveyEventKey;
  FGeoAreaList.Update;
end;  // TGeoAreasTabImpl.SaveData

{-------------------------------------------------------------------------------
  Sets the MasterKey in the FGeoAreaList.
}
procedure TGeoAreasTabImpl.SetKey(Value: TKeyString);
begin
  FKey := Value;
  FGeoAreaList.MasterKey := Key;
end;  // TGeoAreasTabImpl.SetKey

{-------------------------------------------------------------------------------
}
procedure TGeoAreasTabImpl.ValidateData;
var
  lIdx: Integer;
begin
  inherited;
  for lIdx := 1 to sgAreas.RowCount - 1 do begin
    sgAreas.Row := lIdx;
    eNameFindData(Self);
    // if we have unlinked text, that is a failure
    if (eName.Key = '') and (eName.Text <> '') then
      raise EValidationFailure.CreateNonCritical(ResStr_SupplyValidGeoAreaForAllRows);
    // If the subject area of the item is not in the geographic areas subject
    // then this is also a failure.
    if (eName.Key <> '') and not ConceptInGeographySubjectArea(eName.Key) then
      raise EValidationFailure.CreateValidation(ResStr_ConceptNotInGeoArea, eName);
  end;
  FGeoAreaList.ValidateContent;
end;  // TGeoAreasTabImpl.ValidateData

{-------------------------------------------------------------------------------
  Implement IRequestor.Update.  Marshalls the returned data to the method originally supplied
      to handle the data.
}
procedure TGeoAreasTabImpl.UpdateRequestedData(const KeyList: IKeyList);
begin
  if Assigned(FRequestDataRecipientMethod) then
    FRequestDataRecipientMethod(KeyList);
end;  // TGeoAreasTabImpl.UpdateRequestedData

{-------------------------------------------------------------------------------
  Check dragged item is valid before allowing drop.
}
procedure TGeoAreasTabImpl.DragOverCheck(APoint: TPoint; const ATable, AFieldKey: String;
  var Accept: Boolean);
begin
  Accept := (not sgAreas.Readonly) and ConceptInGeographySubjectArea(AFieldKey);
end;  // TGeoAreasTabImpl.DragOverCheck

{-------------------------------------------------------------------------------
  Code to handle dropping onto the keywords grid
}
procedure TGeoAreasTabImpl.DropKeyword(const Sender: TObject; const iFormat:
    integer; const iSourceData: TKeyList; const iTextStrings: TstringList; var
    ioHandled: boolean);
begin
  if (iSourceData.Header.ItemCount>0) and (not sgAreas.Readonly) then
    if SameText(iSourceData.ItemTable[0], TN_CONCEPT) then
    begin
      btnAddClick(nil);
      eName.Text := VarToStr(dmGeneral.GetStoredProcOutputParam(
          'usp_Concept_Get',
          ['@Key', iSourceData.Items[0].KeyField1],
          '@Caption'));
      eName.Key := iSourceData.Items[0].KeyField1;
      if CheckForDuplicates then
        FGeoAreaList.DeleteItem(sgAreas.Row);
    end;
end;  // TGeoAreasTabImpl.DropKeyword;

{-------------------------------------------------------------------------------
  Ensures that the same item cannot be entered into the grid twice
}
function TGeoAreasTabImpl.CheckForDuplicates: Boolean;
var
  i: integer;
begin
  Result := False;
  for i := 0 to sgAreas.RowCount - 1 do
    if (sgAreas.Cells[0, i] = eName.Text) and (sgAreas.Row <> i) then begin
      Result := True;
      Break;
    end;
end;

{-------------------------------------------------------------------------------
}
function TGeoAreasTabImpl.ConceptInGeographySubjectArea(const conceptKey: String): Boolean;
begin
  Result := VarToStr(dmGeneral.GetStoredProcOutputParam(
      'usp_SubjectArea_Select_ForConcept_Get',
      ['@Key', conceptKey],
      '@Subject_Area_Key')) = CG_GEO_SUBJECT;
end;

{-------------------------------------------------------------------------------
  Ensure keystrokes received properly by the addin
}
procedure TGeoAreasTabImpl.WndProc(var Message: TMessage);
var
  msg: TMsg;
begin
  case Message.Msg of
    // TActiveXControl.TranslateAccelerator shall send navigation keys
    WM_GETDLGCODE:
        Message.Result := DLGC_WANTARROWS + DLGC_WANTTAB + DLGC_WANTCHARS;
    // dispatch navigation keys to currently active child control
    WM_KEYDOWN, WM_SYSKEYDOWN, WM_CHAR:
        if (activecontrol <> nil) then
        begin
          msg.hwnd := ActiveControl.handle;
          msg.message := Message.Msg;
          Msg.WParam := Message.WParam;
          Msg.LParam := Message.LParam;
          Msg.time := 0;
          Msg.pt := point(0,0);
          DispatchMessage(msg);
        end;
    else
      inherited; // standard messages
  end;
end;  // TGeoAreasTabImpl.WndProc

{-------------------------------------------------------------------------------
  Handles change of the contents of eName
}
procedure TGeoAreasTabImpl.eNameChange(Sender: TObject);
begin
  inherited;
  ShowHierarchyPath((Sender as TLinkedEdit).Key);
end;

{-------------------------------------------------------------------------------
  Handle the selection of a cell: When a cell is selected, display the
  hierarchy of the item in lblPath. 
}
procedure TGeoAreasTabImpl.sgAreasSelectCell(Sender: TObject; ACol,
  ARow: Integer; var CanSelect: Boolean);
var
  lConceptKey: String;
begin
  inherited;
  if (not FLoading) and (FGeoAreaList <> nil)
      and (FGeoAreaList.ItemCount > 0) then
  begin
    // Get the key of the concept in the selected row
    lConceptKey :=  (FGeoAreaList.Items[ARow - 1] as TGeoAreaItem).ConceptKey;
    ShowHierarchyPath(lConceptKey);
  end
  else
  begin
    lblPath.Caption := '';
  end;
end;  // TfraFieldDataGeoAreas.sgAreasSelectCell

{-------------------------------------------------------------------------------
  Shows the hierarchy of the given concept in lblPath 
}
procedure TGeoAreasTabImpl.ShowHierarchyPath(const AConceptKey: string);
var
  lCaption: String;
  lRecordset: _Recordset;
begin
  lRecordSet := dmGeneral.GetRecordSet(
      'usp_Concept_Select',
      ['@ConceptKey', AConceptKey]);
  try
    // Add the item to the hierarchy list
    if lRecordset.RecordCount > 0 then
    begin
      lCaption := Trim(lRecordSet.Fields['Item_Name'].Value);
      // Add the item's ancestors (if any) to the hierarchy list
      lRecordset := dmGeneral.GetRecordSet(
          'usp_Concept_Select_AllParentConcepts',
          ['@ConceptKey', AConceptKey]);
      if lRecordset.RecordCount > 0 then
      begin
        while not lRecordset.EOF do
        begin
          lCaption :=
              Trim(lRecordset.Fields['ItemName'].value) + ' - ' + lCaption;
          lRecordset.MoveNext;
        end  // while not lRecordset.EOF
      end;  // if lRecordset.RecordCount > 0
    end;  // if lRecordset.RecordCount > 0
  finally
    lRecordSet.Close;
  end;  // try .. finally
  // Set the width of the label and then set the caption in one go,
  // otherwise the width is reset due to autosizing
  lblPath.Width := 350;
  lblPath.Caption := lCaption;
end;  // TfraFieldDataGeoAreas.ShowHierarchyPath

initialization
  TActiveFormFactory.Create(
    ComServer,
    TActiveFormControl,
    TGeoAreasTabImpl,
    Class_GeoAreasTabImpl,
    2,
    '',
    OLEMISC_SIMPLEFRAME or OLEMISC_ACTSLIKELABEL,
    tmApartment);
end.
