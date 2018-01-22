{===============================================================================
  Unit:        QEMultiValuePopup.pas

  Defines:     TIndividualItem, TIndividualList, TfrmQEMultiValuePopup

  Description: Implements the Quick Entry Multivalue Popup screen

  Created:     October 2007

  Last revision information:
    $Revision: 11 $
    $Date: 7/12/11 14:45 $
    $Author: Andrewkemp $

===============================================================================}
unit QEMultiValuePopup;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, Grids, ImageListButton, InterfaceDataModule,
  DssStringGrid, BaseCompositeComponent, LinkedControls, LuxembourgDataClasses,
  DataClasses, ADOInt, ExceptionForm, ExtCtrls;

type
  TIndividualItem = class(TLuxGridDataItem)
  private
    FDataDisplay: string;
    FDataValue: TKeyString;
    FPosition: Integer;
  protected
    procedure GetData(const Column: Integer; var AText: String; var ANameKey: TKeyString); override;
    procedure InitFromRecord(AFields: Fields); override;
    procedure SetData(const Column: Integer; const AText: String; const AKey: TKeyString); override;
    procedure ValidateData; override;
  public
    property DataDisplay: string read FDataDisplay;
    property DataValue: TKeyString read FDataValue;
    property Position: Integer read FPosition;
  end;

  TIndividualList = class(TLuxGridDataList)
  private
    FDataRowKey: Integer;
    FDataType: Integer;
  protected
    procedure DoAddition(AItem: TLuxCachedDataItem); override;
    procedure DoDeletion(AItem: TLuxCachedDataItem); override;
    procedure DoModification(AItem: TLuxCachedDataItem); override;
    function GetRecordset: _Recordset; override;
  public
    property DataRowKey: Integer read FDataRowKey write FDataRowKey;
    property DataType: Integer read FDataType write FDataType;
  end;

  TfrmQEMultiValuePopup = class(TForm)
    sgValues: TDSSStringGrid;
    btnAdd: TImageListButton;
    btnDelete: TImageListButton;
    eName: TLinkedEdit;
    btnCancel: TImageListButton;
    btnOK: TImageListButton;
    Bevel1: TBevel;
    procedure btnOKClick(Sender: TObject);
    procedure btnAddClick(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
    procedure eNameExit(Sender: TObject);
    procedure eNameFindData(Sender: TObject);
    procedure eNameGetData(Sender: TObject);
    procedure eNameKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure sgValuesKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormActivate(Sender: TObject);
  private
    FIndividualList: TIndividualList;
    FText: String;
    FValue: String;
    FLoading: Boolean;
    FDataType: Integer;
    function CanAddRow: Boolean;
    function CheckForDuplicates: Boolean;
    procedure EnableControls(AEnabled: Boolean);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure LoadData(const AKey: TKeyString; ADataRowKey: Integer; ADataType: Integer); overload;
    procedure LoadData(const ADisplay, AValue: String); overload;
    property IndividualList: TIndividualList read FIndividualList;
  end;

{==============================================================================}
implementation

uses
  ApplicationSettings, BaseADODataModule, ResourceStrings, LuxembourgConstants,
  Validation, SearchManager, GeneralData;

{$R *.dfm}

{-==============================================================================
    TIndividualItem
===============================================================================}
{-------------------------------------------------------------------------------
}
procedure TIndividualItem.GetData(const Column: Integer; var AText: String; var ANameKey:
    TKeyString);
begin
  AText := DataDisplay;
  ANameKey := DataValue;
end;  // TIndividualItem.GetData

{-------------------------------------------------------------------------------
}
procedure TIndividualItem.InitFromRecord(AFields: Fields);
begin
  SetItemKey(VarToStr(AFields['QE_Data_Item_Key'].Value));
  FDataDisplay := VarToStr(AFields['Data_Display'].Value);
  FDataValue   := VarToStr(AFields['Data_Value'].Value);
  if AFields['Position'].Value = null then
    FPosition := 1
  else
    FPosition := AFields['Position'].Value;
end;  // TIndividualItem.InitFromRecord

{-------------------------------------------------------------------------------
}
procedure TIndividualItem.SetData(const Column: Integer; const AText: String; const AKey:
    TKeyString);
begin
  FDataDisplay := AText;
  FDataValue := AKey;
  SetModified;
end;  // TIndividualItem.SetData

{-------------------------------------------------------------------------------
}
procedure TIndividualItem.ValidateData;
var
  lText, lKey, lCaption: String;
begin
  if (FDataValue = '') and (FDataDisplay <> '') then begin
    lText := FDataDisplay;
    // Validation parameters depend on the data type.
    case TIndividualList(OwnerList).FDataType of
      CT_INDIVIDUAL: begin
        if DoCheckUnique(lKey, lText, stIndividual, '', ResStr_Individual) then begin
          // Found a unique exact match. Update fields accordingly.
          FDataValue := lKey;
          FDataDisplay := lText;
        end;
        lCaption := ResStr_Individuals;
      end;
      CT_GEO_AREA: begin
        if DoCheckUnique(lKey, lText, stTermInSubjectArea, CG_GEO_SUBJECT, ResStr_GeoArea) then begin
          // Found a unique exact match. Update fields accordingly.
          FDataValue := lKey;
          FDataDisplay := lText;
        end;
        lCaption := ResStr_GeoAreas;
      end;
      CT_CONCEPT: begin
        if DoCheckUnique(lKey, lText, stConcept, '', ResStr_Determination) then begin
          // Found a unique exact match. Update fields accordingly.
          FDataValue := lKey;
          FDataDisplay := lText;
        end;
        lCaption := ResStr_Determinations;
      end;
    end;
  end;
  ValidateValue(FDataValue <> '', Format(ResStr_InvalidNameInGrid, [FDataDisplay, lCaption]));
end;  // TIndividualItem.ValidateData

{-==============================================================================
    TIndividualList
===============================================================================}
{-------------------------------------------------------------------------------
}
procedure TIndividualList.DoAddition(AItem: TLuxCachedDataItem);
begin
  with TIndividualItem(AItem) do begin
    SetItemKey(dmGeneral.RunInsertStoredProc(
        'QE_Data_Item',
        'usp_QEDataItem_Insert_ForMultiValues',
        ['@DataRowKey', DataRowKey,
         '@TemplateFieldKey', MasterKey,
         '@DataDisplay', DataDisplay,
         '@DataValue', DataValue,
         '@Position', IndexOf(AItem) + 1,
         '@EnteredSessionID', dmGeneral.Recorder.CurrentSettings.SessionID
        ],
        '@DataItemKey'));
  end;
end;  // TIndividualList.DoAddition

{-------------------------------------------------------------------------------
}
procedure TIndividualList.DoDeletion(AItem: TLuxCachedDataItem);
begin
  dmGeneral.RunDeleteStoredProc('usp_QEDataItem_Delete',
                                ['@Key', AItem.ItemKey, '@Timestamp', AItem.Timestamp]);
end;  // TIndividualList.DoDeletion

{-------------------------------------------------------------------------------
  We always want to run the Insert method, but for modifications, we had better run the Delete
      method as well beforehand.
}
procedure TIndividualList.DoModification(AItem: TLuxCachedDataItem);
begin
  DoDeletion(AItem);
  DoAddition(AItem);
end;  // TIndividualList.DoModification

{-------------------------------------------------------------------------------
}
function TIndividualList.GetRecordset: _Recordset;
begin
  Result := dmGeneral.GetRecordset('usp_QEDataItem_Select_ForTemplateField',
                                   ['@TemplateFieldKey', MasterKey, '@DataRowKey', DataRowKey]);
end;  // TIndividualList.GetRecordset

{-==============================================================================
    TfrmQEMultiValuePopup
===============================================================================}
{-------------------------------------------------------------------------------
  Constructor.
}
constructor TfrmQEMultiValuePopup.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FIndividualList := TIndividualList.Create(TIndividualItem, sgValues);
  // Setting the control sets the type to custom.
  sgValues.ColumnsInfo[0].WinControl := eName;
end;  // TfrmQEMultiValuePopup.Create

{-------------------------------------------------------------------------------
  Denstructor.
}
destructor TfrmQEMultiValuePopup.Destroy;
begin
  FIndividualList.Free;
  inherited;
end;

{-------------------------------------------------------------------------------
  Handle a click on the close button.
}
procedure TfrmQEMultiValuePopup.btnOKClick(Sender: TObject);
begin
  FIndividualList.Update;
  ModalResult := mrOK;
end;

{-------------------------------------------------------------------------------
  Handle a click on the add button.
}
procedure TfrmQEMultiValuePopup.btnAddClick(Sender: TObject);
begin
  inherited;
  if CanAddRow then begin
    FIndividualList.AddNew(TIndividualItem.CreateNew(FIndividualList));
    sgValues.Row := FIndividualList.ItemCount;
  end else
    sgValues.Row := sgValues.RowCount - 1;
  eName.SetFocus;
end;  // TfrmQEMultiValuePopup.btnAddClick

{-------------------------------------------------------------------------------
}
function TfrmQEMultiValuePopup.CanAddRow: Boolean;
begin
  with sgValues do
    Result := ((Row = RowCount - 1) and (eName.Text <> '')) or
              (Cells[0, RowCount - 1] <> '');
end;  // TfrmQEMultiValuePopup.CanAddRow

{-------------------------------------------------------------------------------
  Handle a click on the remove button. 
}
procedure TfrmQEMultiValuePopup.btnDeleteClick(Sender: TObject);
begin
  inherited;
  with sgValues do
    if (Cells[Col, Row] <> '') or (eName.Text <> '') then begin
      if MessageDlg(Format(ResStr_ConfirmRowDelete, [ResStr_Row]),
                     mtWarning, [mbYes, mbNo], 0) = mrYes then
        FIndividualList.DeleteItem(Row);
    end else
      FIndividualList.DeleteItem(Row);
  EnableControls(True);
end;  // TfrmQEMultiValuePopup.btnDeleteClick

{-------------------------------------------------------------------------------
  Set the colours of the controls in edit mode.
}
procedure TfrmQEMultiValuePopup.EnableControls(AEnabled: Boolean);
begin
  sgValues.ReadOnly := not AEnabled;
  btnDelete.Enabled := (sgValues.Cells[0, 1] <> '');
end;  // TfrmQEMultiValuePopup.EnableControls

{-------------------------------------------------------------------------------
  Make sure the contents of the Geo Area linked edit is parsed when the user exits it.
}
procedure TfrmQEMultiValuePopup.eNameExit(Sender: TObject);
begin
  inherited;
  btnDelete.Enabled := (sgValues.Cells[0, 1] <> '') or (eName.Text <> '');
end;  // TfrmQEMultiValuePopup.eNameExit

{-------------------------------------------------------------------------------
  Handler for OnFindData for the floating linked edit control on the string grid.
}
procedure TfrmQEMultiValuePopup.eNameFindData(Sender: TObject);
begin
  inherited;
  case FDataType of
    CT_INDIVIDUAL:
      DoCheck(eName, stIndividual);
    CT_GEO_AREA:
      DoCheck(eName, stTermInSubjectArea, CG_GEO_SUBJECT);
    CT_CONCEPT:
      DoCheck(eName, stDeterminationEarthScience);
  end;
  if CheckForDuplicates then begin
    eName.Text := '';
    eName.Key := '';
  end;
  EnableControls(True);
end;  // TfrmQEMultiValuePopup.eNameFindData

{-------------------------------------------------------------------------------
  Get data from the Individual module.
}
procedure TfrmQEMultiValuePopup.eNameGetData(Sender: TObject);
var
  lKey, lText: string;
  lCheck: Boolean;
begin
  inherited;
  lCheck := False;
  lText := eName.Text;
  lKey := eName.Key;
  case FDataType of
    CT_INDIVIDUAL:
      lCheck := DoCheck(lKey, lText, stIndividual);
    CT_GEO_AREA:
      lCheck := DoCheck(lKey, lText, stTermInSubjectArea, CG_GEO_SUBJECT);
    CT_CONCEPT:
      lCheck := DoCheck(lKey, lText, stDeterminationEarthScience);
  end;
  eName.Key := lKey;
  if lCheck then
    eName.Text := lText; 
  EnableControls(True);
end;  // TfrmQEMultiValuePopup.eNameGetData

{-------------------------------------------------------------------------------
}
procedure TfrmQEMultiValuePopup.eNameKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  inherited;
  if (Key in [VK_DELETE, VK_BACK]) and (eName.Text = '') then begin
    // Need to get rid of some messages in queue first.
    Application.ProcessMessages;
    // Now remove.
    FIndividualList.DeleteItem(sgValues.Row);
    Key := 0;
  end;
end;  // TfrmQEMultiValuePopup.eNameKeyDown

{-------------------------------------------------------------------------------
  Ensures that the same item cannot be entered into the grid twice
}
function TfrmQEMultiValuePopup.CheckForDuplicates: Boolean;
var
  i: integer;
begin
  Result := False;
  for i := 0 to sgValues.RowCount - 1 do begin
    if (sgValues.Cells[0, i] = eName.Text) and (sgValues.Row <> i) then
      Result := True;
  end;
end;  // TfrmQEMultiValuePopup.CheckForDuplicates

{-------------------------------------------------------------------------------
  Load the data from the recordset.
}
procedure TfrmQEMultiValuePopup.LoadData(const AKey: TKeyString; ADataRowKey: Integer;
    ADataType: Integer);
begin
  FIndividualList.MasterKey  := AKey;
  FIndividualList.DataRowKey := ADataRowKey;
  FIndividualList.DataType   := ADataType;
  FIndividualList.Refresh;
  FDataType := ADataType;
end;

{-------------------------------------------------------------------------------
}
procedure TfrmQEMultiValuePopup.LoadData(const ADisplay, AValue: String);
begin
  // What we want to do here must wait until after the EnableControls method is run
  // in FormActivate, so we set some flags so we can do it then.
  FText    := ADisplay;
  FValue   := AValue;
  FLoading := True;
end;

{-------------------------------------------------------------------------------
}
procedure TfrmQEMultiValuePopup.sgValuesKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  with sgValues do
    if (Row = RowCount - 1) and (Key = VK_DOWN) and not CanAddRow then Key := 0;
end;

{-------------------------------------------------------------------------------
  Ensure the controls are enabled and focussed right. Goes here to avoid causing errors.
}
procedure TfrmQEMultiValuePopup.FormActivate(Sender: TObject);
begin
  EnableControls(True);
  if FLoading then begin
    eName.Text := FText;
    eName.Key  := FValue;
    eNameFindData(nil);
  end;
  if eName.Key <> '' then
    btnDelete.Enabled := True;
end;

end.
