//==============================================================================
//  Unit:        DataStringGrid
//
//  Implements:  TDataStringGrid,
//                 TFieldInfo, TUpdateRowEvent, TDeleteRowEvent
//
//  Description: Class that can be linked to a query and a string grid, to
//               provide a pseudo data aware grid.
//               Editing is handled to make the grid feel like an Access grid.
//               Use early bound field components attached to a query to setup
//               each column.  Use LookupDatasets to specify which columns are
//               represented as combo boxes.
//               Read only columns are shaded gray.
//               Use a small calculated column called Indicator if you want an
//               indicator.
//
//  Author:      John van Breda
//  Created:     25 February 2000
//
//  Changes:
//
//  Last Revision Details:
//    $Revision: 26 $
//    $Date: 7/05/08 11:03 $
//    $Author: Johnvanbreda $
//
//  Copyright © Dorset Software Services Ltd, 2001
//
//==============================================================================

unit DataStringGrid;

interface

uses
  SysUtils, Classes, Grids, StdCtrls, Controls, Windows, Forms, ADODB, ActiveX,
  ExtCtrls, Graphics, DataTypes, ExceptionForm, Dialogs, UserMessages;

type
  {-----------------------------------------------------------------------------
    Error class for unit.
  }
  EDataStringGridError = class(TExceptionPath)
  end;

  {-----------------------------------------------------------------------------
    The MoveRow method is inaccessible in TStringGrid, but casting a string grid
    as this class allows access to it.
  }
  TMoveableGrid = class(TStringGrid);

  {-----------------------------------------------------------------------------
    Class providing access to TStringGrid's protected members.
  }
  TStringGridAccessor = class(TStringGrid)
  end;

  // record to remember state of current cell, when we need to wind back }
  TCurrentCellState = record
    Text: string;
    SelStart: Integer;
    SelLength: Integer;
  end;


  { If implementing this event, must change ioRowKey to new key used for
      inserts }
  TUpdateRowEvent = procedure (var ioRowKey : string; iData : TStringList) of
      object;
  TDeleteRowEvent = procedure (iRowKey : string) of object;
  TValidateCellEvent = procedure (var ioAccept : boolean; ACol, ARow : integer)
      of object;
  TGridCellEvent = procedure (ACol, ARow : integer) of object;
  TCustomEditCell = procedure (Panel: TPanel; ACol, ARow: integer) of object;
  TRowChangedEvent = procedure (Sender: TObject; ARow: integer) of object;
  TCellColourEvent = function (ACol,ARow: integer; var AHandled: boolean):
      TColor of object;
  {-----------------------------------------------------------------------------
    String grid descendant that ignores the state of the shift key when the
    mouse is clicked.
  }
  TShiftlessStringGrid = class(TStringGrid)
  protected
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
  end;
  
  {-----------------------------------------------------------------------------
    Class that can be linked to a query and a string grid, to provide a
    pseudo data aware grid.  Editing is handled to make the grid feel like
    an Access grid.
  }
  TDataStringGrid = class(TObject)
  private
    FColDataSizes: TList;
    FCurrentCellState: TCurrentCellState;
    FCustomEditPanel: TPanel;
    FFieldNames: TList;
    FGrid: TStringGrid;
    FIndicator: Boolean;
    FInvisibleCols: TStringList;
    FMouseDown: Boolean;
    FMouseStartDragRow: Integer;
    FOldSelectCell: TSelectCellEvent;
    FOldRowMoved: TMovedEvent;
    FOnCellBGColour: TCellColourEvent;
    FOnCellTextColour: TCellColourEvent;
    FOnCheckBoxClick: TGridCellEvent;
    FOnCanEditCell: TValidateCellEvent;
    FOnCustomEditCell: TCustomEditCell;
    FOnDeleteRow: TDeleteRowEvent;
    FOnRepositionCustomEdit: TCustomEditCell;
    FOnRowChanged: TRowChangedEvent;
    FOnUpdateRow: TUpdateRowEvent;
    FOnValidateCell: TValidateCellEvent;
    FReadOnly: Boolean;
    FSaving: Boolean;
    FSelectionAnchorRow: Integer;
    FTimestampCols: TStringList;
    procedure BringCellIntoView(ACol, ARow: integer);
    procedure ClearTimestamps;
    function ControlKeyDown: Boolean;
    procedure DetectColumnWidth(iCol: integer);
    function GetCanEditCell(ACol, ARow: Integer): Boolean;
    function GetCellBGColour(ACol, ARow:integer): TColor;
    function GetCellTextColour(ACol, ARow: Integer): TColor;
    function GetColCount: Integer;
    function GetColumnWidth(Index: Integer): Integer;
    function GetCustomEdit(Index: Integer): Boolean;
    function GetDisplayName(Index: Integer): string;
    function GetFieldName(Index: Integer): string;
    function GetInPlaceEdit: TInplaceEdit;
    function GetInvisibleColCount: Integer;
    function GetInvisibleColIndex(AName: string): Integer;
    function GetInvisibleColValue(iColumnName: string; iRow: integer): string;
    function GetRowCount: Integer;
    function GetTimestampCol(iColumnName: String; IRow: integer):
        TSQLSvrTimestamp;
    function GetTimestampColCount: Integer;
    function GetTimestampColIndex(AName: string): Integer;
    procedure GridDrawCell(Sender: TObject; ACol, ARow: Longint; Rect: TRect;
        State: TGridDrawState);
    procedure GridKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure GridMouseDown(Sender: TObject; Button: TMouseButton; Shift:
        TShiftState; X, Y: Integer);
    procedure GridMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure GridMouseUp(Sender: TObject; Button: TMouseButton; Shift:
        TShiftState; X, Y: Integer);
    procedure GridRowMoved(Sender: TObject; FromIndex, ToIndex: Longint);
    procedure GridSelectCell(Sender: TObject; ACol, ARow: Integer; var
        CanSelect: Boolean);
    procedure GridTopLeftChanged(Sender: TObject);
    function IsCheckBoxCol(ACol: integer): Boolean;
    procedure ReadCurrentCellState;
    procedure RePositionEntryPanel(ACol, ARow: integer);
    procedure SelectRowsBetween(AFromRow, AToRow: integer);
    procedure SetColumnWidth(Index: Integer; Value: Integer);
    procedure SetCustomEdit(Index: Integer; const Value: Boolean);
    procedure SetDisplayName(Index: Integer; const Value: string);
    procedure SetIndicator(const Value: Boolean);
    procedure SetInvisibleColValue(iColumnName: string; iRow: integer; const
        Value: string);
    procedure SetOnCellBGColour(const Value: TCellColourEvent);
    procedure SetOnCellTextColour(const Value: TCellColourEvent);
    procedure SetOnCustomEditCell(Value: TCustomEditCell);
    procedure SetOnDeleteRow(const Value: TDeleteRowEvent);
    procedure SetOnRepositionCustomEdit(Value: TCustomEditCell);
    procedure SetOnRowChanged(const Value: TRowChangedEvent);
    procedure SetOnUpdateRow(const Value: TUpdateRowEvent);
    procedure SetOnValidateCell(const Value: TValidateCellEvent);
    procedure SetReadOnly(Value: Boolean);
    procedure SetSelectedRows(ARowClicked: integer);
    procedure SetTimestampCol(iColumnName: String; IRow: integer; Value:
        TSQLSvrTimestamp);
    function ShiftKeyDown: Boolean;
    procedure ValidateRow(iRow: integer);
  protected
    property InPlaceEdit: TInplaceEdit read GetInPlaceEdit;
  public
    constructor Create(iGrid : TStringGrid);
    destructor Destroy; override;
    procedure AddColumn(DisplayName: String; FieldName: String; ColumnWidth:
        integer; DataType: TOleEnum; Editable: boolean; Required: Boolean);
    procedure AddInvisibleCol(FieldName: String);
    procedure AddTimestampCol(FieldName: String);
    procedure AddToGrid(Sender : TObject);
    procedure DeleteRow(iRowIndex: integer); overload;
    procedure FillGrid(iRecordset: _Recordset);
    procedure ForceMouseUp;
    procedure HideCustomEdit;
    procedure MoveRow(iCurrentIndex, iNewIndex: Integer);
    procedure PopulateGrid(iRecordset: _Recordset);
    function RowContainsData(iRow: integer): Boolean;
    procedure UpdateWholeInvisibleColumn(const AColumnName, ANewValue: String);
    property ColCount: Integer read GetColCount;
    property ColumnWidth[Index: Integer]: Integer read GetColumnWidth write
        SetColumnWidth;
    property CustomEdit[Index: Integer]: Boolean read GetCustomEdit write
        SetCustomEdit;
    property CustomEditPanel: TPanel read FCustomEditPanel;
    property DisplayName[Index: Integer]: string read GetDisplayName write
        SetDisplayName;
    property FieldName[Index: Integer]: string read GetFieldName;
    property Grid: TStringGrid read FGrid;
    property Indicator: Boolean read FIndicator write SetIndicator;
    property InvisibleColCount: Integer read GetInvisibleColCount;
    property InvisibleColIndex[AName: string]: Integer read GetInvisibleColIndex;
    property InvisibleColValue[iColumnName: string; iRow: integer]: string read
        GetInvisibleColValue write SetInvisibleColValue;
    property OnCanEditCell: TValidateCellEvent read FOnCanEditCell write FOnCanEditCell;
    property OnCellBGColour: TCellColourEvent read FOnCellBGColour write
        SetOnCellBGColour;
    property OnCellTextColour: TCellColourEvent read FOnCellTextColour write
        SetOnCellTextColour;
    property OnCheckBoxClick: TGridCellEvent read FOnCheckBoxClick write
        FOnCheckBoxClick;
    property OnCustomEditCell: TCustomEditCell read FOnCustomEditCell write
        SetOnCustomEditCell;
    property OnDeleteRow: TDeleteRowEvent read FOnDeleteRow write
        SetOnDeleteRow;
    property OnRepositionCustomEdit: TCustomEditCell read
        FOnRepositionCustomEdit write SetOnRepositionCustomEdit;
    property OnRowChanged: TRowChangedEvent read FOnRowChanged write
        SetOnRowChanged;
    property OnUpdateRow: TUpdateRowEvent read FOnUpdateRow write
        SetOnUpdateRow;
    property OnValidateCell: TValidateCellEvent read FOnValidateCell write
        SetOnValidateCell;
    property RowCount: Integer read GetRowCount;
    property ReadOnly: Boolean read FReadOnly write SetReadOnly;
    property TimestampCol[iColumnName: String; IRow: integer]: TSQLSvrTimestamp
        read GetTimestampCol write SetTimestampCol;
    property TimestampColCount: Integer read GetTimestampColCount;
    property TimestampColIndex[AName: string]: Integer read GetTimestampColIndex;
  end;

  {-----------------------------------------------------------------------------
    Encapsulates information about a column appearing in the grid.
  }
  TFieldInfo = class(TObject)
  private
    FColumnWidth: Integer;
    FCustomEdit: Boolean;
    FDataType: TOleEnum;
    FDisplayName: string;
    FEditable: Boolean;
    FFieldName: string;
    FRequired: Boolean;
    procedure SetColumnWidth(const Value: Integer);
    procedure SetDataType(const Value: TOleEnum);
    procedure SetDisplayName(const Value: string);
    procedure SetEditable(const Value: Boolean);
    procedure SetFieldName(const Value: string);
    procedure SetRequired(const Value: Boolean);
  public
    constructor Create(ADisplayName, AFieldName : String; AColumnWidth: Integer;
        ADataType: TOleEnum; ARequired: boolean; AEditable: boolean);
    property ColumnWidth: Integer read FColumnWidth write SetColumnWidth;
    property CustomEdit: Boolean read FCustomEdit write FCustomEdit;
    property DataType: TOleEnum read FDataType write SetDataType;
    property DisplayName: string read FDisplayName write SetDisplayName;
    property Editable: Boolean read FEditable write SetEditable;
    property FieldName: string read FFieldName write SetFieldName;
    property Required: Boolean read FRequired write SetRequired;
  end;

  {-----------------------------------------------------------------------------
    A data string grid that allows cells to have more than one value.
  }
  TMultiValueDataStringGrid = class(TDataStringGrid)
  private
    procedure ClearTimestamps;
    function GetInvisibleColValue(iColumnName: string; iRow: integer; iItem: integer): string;
    function GetTimestampCol(iColumnName: String; IRow: integer; IItem: integer):
        TSQLSvrTimestamp;
    procedure SetInvisibleColValue(iColumnName: string; iRow: integer; iItem: integer; const
        Value: string);
    procedure SetTimestampCol(iColumnName: String; IRow: integer; IItem: integer; Value:
        TSQLSvrTimestamp);
  public
    constructor Create(iGrid : TStringGrid);
    destructor Destroy; override;
    procedure DeleteRow(iRowIndex: integer); overload;
    procedure UpdateWholeInvisibleColumn(const AColumnName: string; ANewValues: TStringList);
    property InvisibleColValue[iColumnName: string; iRow: integer; iItem: integer]: string
        read GetInvisibleColValue write SetInvisibleColValue; 
    property TimestampCol[iColumnName: String; IRow: integer; IItem: integer]: TSQLSvrTimestamp
        read GetTimestampCol write SetTimestampCol;
  end;

var
  STR_TRUE: String;
  STR_FALSE: String;

//==============================================================================
implementation

uses
  GeneralFunctions, DB, TypInfo, AdoInt, Variants, Types, ComObj, Math,
  GenFuncs, QuickEntryFrame, ResourceStrings, InterfaceDataModule;

{-==============================================================================
    TShiftlessStringGrid
===============================================================================}
{-------------------------------------------------------------------------------
  Override MouseDown to fool the grid into ignoring the shift key state, as we
      handle this in our own selection code.
}
procedure TShiftlessStringGrid.MouseDown(Button: TMouseButton; Shift:
    TShiftState; X, Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
end;  // TShiftlessStringGrid.MouseDown

{-==============================================================================
    TDataStringGrid
===============================================================================}
{-------------------------------------------------------------------------------
  Object initialisation
}
constructor TDataStringGrid.Create(iGrid : TStringGrid);
begin
  inherited Create;
  FSaving := false; // are we in middle of a save operation
  FReadOnly := false;
  FFieldNames := TList.Create;
  FInvisibleCols := TStringList.Create;
  FTimestampCols := TStringList.Create;
  FGrid := iGrid;
  FColDataSizes := TList.Create;

  FOldSelectCell         := FGrid.OnSelectCell;
  FGrid.OnSelectCell     := GridSelectCell;
  FGrid.OnDrawCell       := GridDrawCell;
  FGrid.OnTopLeftChanged := GridTopLeftChanged;
  FGrid.OnKeyDown        := GridKeyDown;
  FGrid.OnMouseMove      := GridMouseMove;
  FGrid.OnMouseDown      := GridMouseDown;
  FGrid.OnMouseUp        := GridMouseUp;
  FOldRowMoved           := FGrid.OnRowMoved;
  FGrid.OnRowMoved       := GridRowMoved;
  FGrid.DoubleBuffered := True;
  FMouseDown := False;

  FCustomEditPanel := TPanel.Create(nil);
  FCustomEditPanel.Parent := FGrid.Parent;
  FCustomEditPanel.Visible := False;
  FCustomEditPanel.Color := clWindow;
  FCustomEditPanel.BevelInner := bvNone;
  FCustomEditPanel.BevelOuter := bvNone;
  FCustomEditPanel.BorderStyle := bsSingle;
  FCustomEditPanel.Ctl3D := False;

  if FIndicator then
    FGrid.Col := 1; // select first true cell
  FSelectionAnchorRow := -1;
end;  // TDataStringGrid.Create 

{-------------------------------------------------------------------------------
}
destructor TDataStringGrid.Destroy;
var
  i: Integer;
begin
  for i := FFieldNames.Count - 1 downto 0 do
    if Assigned(FFieldNames[i]) then
      TFieldInfo(FFieldNames[i]).Free;
  FFieldNames.Free;

  FColDataSizes.Free;
  if Assigned(FCustomEditPanel) then
    FCustomEditPanel.Free;

  if Assigned(FInvisibleCols) then
  begin
    for i := FInvisibleCols.Count - 1 downto 0 do
      TStringList(FInvisibleCols.Objects[i]).Free;
    FInvisibleCols.Free;
  end;

  if Assigned(FTimestampCols) then
  begin
    ClearTimestamps;
    for i := FTimestampCols.Count - 1 downto 0 do
      TStringList(FTimestampCols.Objects[i]).Free;
    FTimestampCols.Free;
  end;
  inherited;
end;  // TDataStringGrid.Destroy 

{-------------------------------------------------------------------------------
  Adds a column to the string grid 
}
procedure TDataStringGrid.AddColumn(DisplayName: String; FieldName: String;
    ColumnWidth: integer; DataType: TOleEnum; Editable: boolean; Required:
    Boolean);
begin
  FFieldNames.Add(TFieldInfo.Create(DisplayName, FieldName, ColumnWidth,
                                    DataType, Required, Editable));
  FGrid.ColCount := FFieldNames.Count;
  FGrid.Cells[FGrid.ColCount - 1, 0] := DisplayName;
  DetectColumnWidth(FGrid.ColCount -1);
end;  // TDataStringGrid.AddColumn 

{-------------------------------------------------------------------------------
}
procedure TDataStringGrid.AddInvisibleCol(FieldName: String);
var
  lStringList: TStringList;
begin
  lStringList := TStringList.Create;
  FInvisibleCols.AddObject(FieldName, lStringList);
end;  // TDataStringGrid.AddInvisibleCol 

{-------------------------------------------------------------------------------
}
procedure TDataStringGrid.AddTimestampCol(FieldName: String);
var
  lStringList: TStringList;
begin
  lStringList := TStringList.Create;
  FTimestampCols.AddObject(FieldName, lStringList);
end;  // TDataStringGrid.AddTimestampCol

{-------------------------------------------------------------------------------
}
procedure TDataStringGrid.AddToGrid(Sender : TObject);
var
  lCol: Integer;
begin
  // focus first non-fixed col
  if FIndicator then
    lCol := 1
  else
    lCol := 0;
  while (lCol < FGrid.ColCount) and (not TFieldInfo(FFieldNames[lCol]).Editable) do
    Inc(lCol);
  FGrid.Col := lCol;
  if not RowContainsData(FGrid.RowCount-1) then
    FGrid.Row := FGrid.RowCount-1
  else begin
    FGrid.RowCount := FGrid.RowCount + 1;
    FGrid.Row := FGrid.RowCount-1;
  end;
  // enable editor
  FGrid.EditorMode := True;
end;  // TDataStringGrid.AddToGrid

{-------------------------------------------------------------------------------
}
procedure TDataStringGrid.BringCellIntoView(ACol, ARow: integer);
begin
  {Bring the selected cell into view}
  if ACol < FGrid.LeftCol then
    FGrid.LeftCol := ACol
  else if ACol > FGrid.LeftCol + FGrid.VisibleColCount then
    FGrid.LeftCol := ACol - FGrid.VisibleColCount;
  if ARow < FGrid.TopRow then
    FGrid.TopRow := ARow
  else if ARow > FGrid.TopRow + FGrid.VisibleRowCount then
    FGrid.TopRow := ARow - FGrid.VisibleRowCount;
end;  // TDataStringGrid.BringCellIntoView 

{-------------------------------------------------------------------------------
  Clears the timestamp string lists and frees associated variant objects.
}
procedure TDataStringGrid.ClearTimestamps;
var
  i: Integer;
  j: Integer;
begin
  for i := FTimestampCols.Count - 1 downto 0 do begin
    for j := 1 to TStringList(FTimestampCols.Objects[i]).Count-1 do
      // Free timestamp objects
      TVariantObject(TStringList(FTimestampCols.Objects[i]).Objects[j]).Free;
    TStringList(FTimestampCols.Objects[i]).Clear;
  end;
end;  // TDataStringGrid.ClearTimestamps 

{-------------------------------------------------------------------------------
  Returns true if either control key is down.
}
function TDataStringGrid.ControlKeyDown: Boolean;
begin
  Result := (GetKeyState(VK_LCONTROL)<0) or (GetKeyState(VK_RCONTROL)<0);
end;  // TDataStringGrid.ControlKeyDown

{-------------------------------------------------------------------------------
}
procedure TDataStringGrid.DeleteRow(iRowIndex: integer);
var
  i: Integer;
begin
  for i := 0 to FInvisibleCols.Count -1  do
    with TStringList(FInvisibleCols.Objects[i]) do
      if iRowIndex < Count then
        Delete(iRowIndex);

  for i := 0 to FTimestampCols.Count -1  do
    with TStringList(FTimestampCols.Objects[i]) do
      if iRowIndex < Count then begin
        TVariantObject(Objects[iRowIndex]).Free;
        Delete(iRowIndex);
      end;

  with FGrid do begin
    // move all rows up to fill the gap
    for i := iRowIndex to RowCount - 2 do
      Rows[i].Assign(Rows[i + 1]);

    if Row >= RowCount then
      Row := Row - 1;
    // Remove the row - always leave at least 1 row though
    if RowCount > FixedRows + 1 then
      RowCount := RowCount - 1
    else
      Rows[Row].Clear;
  end;
end;  // TDataStringGrid.DeleteRow

{-------------------------------------------------------------------------------
  Sets the width of a column to the width required to show the contents of all
      the visible cells.
}
procedure TDataStringGrid.DetectColumnWidth(iCol: integer);
var
  i: Integer;
  liwidth: Integer;
  lstText: PAnsiChar;
  lRect: TRect;
begin
  liwidth := 0;
  lRect.Left := 0;
  FGrid.Canvas.Font := FGrid.Font;
  for i := 0 to FGrid.FixedRows do
  begin
    //Find out how wide the column should be
    lstText := PAnsiChar(DuplicateCharacters(FGrid.Cells[icol, i], '&'));
    DrawText(FGrid.Canvas.Handle, lstText, StrLen(lstText),
             lRect, DT_CALCRECT or DT_TOP or dt_Left);
    liWidth := Max(liWidth, lRect.Right);
  end;

  for i := FGrid.TopRow to FGrid.TopRow + FGrid.VisibleRowCount - 1 do
  begin
    //Find out how wide the column should be
    lstText := PAnsiChar(DuplicateCharacters(FGrid.Cells[icol, i], '&'));
    DrawText(FGrid.Canvas.Handle, lstText, StrLen(lstText),
             lRect, DT_CALCRECT or DT_TOP or dt_Left);
    liWidth := Max(liWidth, lRect.Right);
  end;
  TFieldInfo(FFieldNames[icol]).ColumnWidth := liWidth + 4;
  FGrid.ColWidths[icol] := liWidth + 4;
end;  // TDataStringGrid.DetectColumnWidth

{-------------------------------------------------------------------------------
}
procedure TDataStringGrid.FillGrid(iRecordset: _Recordset);
var
  i: Integer;
  liColStart: Integer;
  ltfOldNullStrictConvert, ltfFirstRow: Boolean;
  lFieldInfo: TFieldInfo;
  lField: Field;
begin
  for i := 0 to FInvisibleCols.Count - 1 do
    with TStringList(FInvisibleCols.Objects[i]) do
    begin
      Clear;
      Add('First Row'); //leave one object to correspond to the top row
    end;

  ClearTimestamps;

  for i := 0 to FTimestampCols.Count - 1 do
    //leave one object to correspond to the top row
    TStringList(FTimestampCols.Objects[i]).Add('First Row');

  //automatically convert null strings to ''
  ltfOldNullStrictConvert := NullStrictConvert;
  NullStrictConvert := False;
  liColStart := IfThen(FIndicator, 1, 0);
  { Fill the grid }
  try
    ltfFirstrow := True;
    with iRecordset do begin
      FGrid.RowCount := 2; // don't set less than 2 rows, need to keep 1 fixed row.
      if not (Eof and bof) then
      begin
        MoveFirst;
        while not Eof do begin
          if ltfFirstRow then ltfFirstRow := False
                         else FGrid.RowCount := FGrid.RowCount + 1;

          for i := liColStart to FGrid.ColCount - 1 do begin
            lFieldInfo := TFieldInfo(FFieldNames[i]);
            lField := Fields[TFieldInfo(FFieldNames[i]).FieldName];

            if lFieldInfo.DataType = adBoolean then
              FGrid.Cells[i, FGrid.RowCount - 1] := SysUtils.BoolToStr(lField.Value)
            else
              FGrid.Cells[i, FGrid.RowCount - 1] := lField.Value;
          end;

          // Save the values of the invisible cols
          for i := 0 to FInvisibleCols.Count - 1 do begin
            lField := Fields[FinvisibleCols[i]];
            if lField.Type_ = adBoolean then
              TStringList(FInvisibleCols.Objects[i]).Add(SysUtils.BoolToStr(lField.Value))
            else
              TStringList(FInvisibleCols.Objects[i]).Add(lField.Value);
          end;

          for i := 0 to FTimestampCols.Count - 1 do
            TStringList(FTimestampCols.Objects[i]).AddObject(
                '',  TVariantObject.Create(Fields[FTimestampCols[i]].Value));
          MoveNext;
        end; // while
      end else
        FGrid.Rows[1].Clear;  // must be at least 1 blank row
    end; // with iRecordset

    //set column widths
    for i := liColStart to FGrid.ColCount - 1 do
      DetectColumnWidth(i);
  finally
    NullStrictConvert := ltfOldNullStrictConvert;
  end;
end;  // TDataStringGrid.FillGrid

{-------------------------------------------------------------------------------
  If an error occurs somewhere, it is a good idea to force the grid to see the
      mouse as up.  This prevents drag selection when the mouse is up.
}
procedure TDataStringGrid.ForceMouseUp;
begin
  FMouseDown := False;
end;  // TDataStringGrid.ForceMouseUp

{-------------------------------------------------------------------------------
}
function TDataStringGrid.GetCanEditCell(ACol, ARow: Integer): Boolean;
begin
  Result := FCustomEditPanel.Visible;
  if Assigned(FOnCanEditCell) then
    FOnCanEditCell(Result, ACol, ARow);
end;

{-------------------------------------------------------------------------------
}
function TDataStringGrid.GetCellBGColour(ACol, ARow:integer): TColor;
var
  lHandled: Boolean;
begin
  lHandled:= False;
  Result := clWindow;

  if Assigned(FOnCellBGColour) then
    Result:= FOnCellBGColour(ACol, ARow, lHandled);

  if not lHandled then
    if (ARow >= FGrid.FixedRows) and (ACol >= FGrid.FixedCols) then
    begin
      // normal cell - check if selected
      if FIndicator and (FGrid.Cells[0, ARow] = '1') then
        Result := MergeColours(clWindow, clHighlight, 80)
      else
        Result := clWindow;
    end
    else
    if FIndicator and (FGrid.Cells[0, ARow] = '1') then
      // highlighted indicator row
      Result := clHighlight
    else
      // normal indicator row
      Result := clBtnFace;
end;  // TDataStringGrid.GetCellBGColour

{-------------------------------------------------------------------------------
}
function TDataStringGrid.GetCellTextColour(ACol, ARow: Integer): TColor;
var
  lHandled: Boolean;
begin
  lHandled := False;
  Result := clWindowText;

  if Assigned(FOnCellTextColour) then
    Result := FOnCellTextColour(ACol, ARow, lHandled);
end;  // TDataStringGrid.GetCellTextColour

{-------------------------------------------------------------------------------
}
function TDataStringGrid.GetColCount: Integer;
begin
  Result := FGrid.ColCount + FInvisibleCols.Count + FTimestampCols.Count;
end;  // TDataStringGrid.GetColCount

{-------------------------------------------------------------------------------
}
function TDataStringGrid.GetColumnWidth(Index: Integer): Integer;
begin
  if FIndicator and  (Index = 0) then
    result := 10
  else
    result := TFieldInfo(FFieldNames[Index]).ColumnWidth;
end;  // TDataStringGrid.GetColumnWidth 

{-------------------------------------------------------------------------------
}
function TDataStringGrid.GetCustomEdit(Index: Integer): Boolean;
begin
  if FIndicator and  (Index = 0) then
    result := false
  else
    result := TFieldInfo(FFieldNames[Index]).CustomEdit;
end;  // TDataStringGrid.GetCustomEdit 

{-------------------------------------------------------------------------------
}
function TDataStringGrid.GetDisplayName(Index: Integer): string;
begin
  if FIndicator and  (Index = 0) then
    result := ''
  else
    result := TFieldInfo(FFieldNames[Index]).DisplayName;
end;  // TDataStringGrid.GetDisplayName 

{-------------------------------------------------------------------------------
}
function TDataStringGrid.GetFieldName(Index: Integer): string;
begin
  if FIndicator and  (Index = 0) then
    result := ''
  else
    Result := TFieldInfo(FFieldNames[Index]).FieldName;
end;  // TDataStringGrid.GetFieldName 

{-------------------------------------------------------------------------------
}
function TDataStringGrid.GetInPlaceEdit: TInplaceEdit;
begin
  Result := TStringGridAccessor(FGrid).InplaceEditor;
end;  // TDataStringGrid.GetInPlaceEdit

{-------------------------------------------------------------------------------
}
function TDataStringGrid.GetInvisibleColCount: Integer;
begin
  Result := FInvisibleCols.Count;
end;  // TDataStringGrid.GetInvisibleColCount

{-------------------------------------------------------------------------------
}
function TDataStringGrid.GetInvisibleColIndex(AName: string): Integer;
begin
  Result := FInvisibleCols.IndexOf(AName);
end;  // TDataStringGrid.GetInvisibleColIndex

{-------------------------------------------------------------------------------
}
function TDataStringGrid.GetInvisibleColValue(iColumnName: string; iRow:
    integer): string;
begin
  with TStringList(FInvisibleCols.Objects[FInvisibleCols.IndexOf(iColumnName)]) do
    if (iRow >= 0) and (iRow < Count) then
      result := Strings[iRow]
    else
      result := '';
end;  // TDataStringGrid.GetInvisibleColValue

{-------------------------------------------------------------------------------
}
function TDataStringGrid.GetRowCount: Integer;
begin
  Result := FGrid.RowCount;
end;  // TDataStringGrid.GetRowCount

{-------------------------------------------------------------------------------
}
function TDataStringGrid.GetTimestampCol(iColumnName: String; IRow: integer):
    TSQLSvrTimestamp;
begin
  with TStringList(FTimestampCols.Objects[FTimestampCols.IndexOf(iColumnName)]) do
    if (iRow >= 0) and (iRow < Count) then
      result := TVariantObject(Objects[iRow]).OLEVariant
    else
      result := null;
end;  // TDataStringGrid.GetTimestampCol

{-------------------------------------------------------------------------------
}
function TDataStringGrid.GetTimestampColCount: Integer;
begin
  Result := FTimestampCols.Count;
end;  // TDataStringGrid.GetTimestampColCount

{-------------------------------------------------------------------------------
}
function TDataStringGrid.GetTimestampColIndex(AName: string): Integer;
begin
  Result := FTimestampCols.IndexOf(AName);
end;  // TDataStringGrid.GetTimestampColIndex

{-------------------------------------------------------------------------------
}
procedure TDataStringGrid.GridDrawCell(Sender: TObject; ACol, ARow: Longint;
    Rect: TRect; State: TGridDrawState);
var
  lRectShape: TPoint;
  lTextRect: TRect;
begin
  with FGrid.Canvas do begin
    Brush.Color := GetCellBGColour(ACol, ARow);
    Pen.Color := clWindow;
    FillRect(Rect);
    if (IsCheckBoxCol(ACol)) and (ARow >= FGrid.FixedRows) then
      DrawCheckBox(
          FGrid.Canvas,Rect.Left + ((Rect.Right - Rect.Left - 12) div 2),
          Rect.Top + ((Rect.Bottom - Rect.Top - 12) div 2),
          CompareText(FGrid.Cells[ACol, ARow], STR_TRUE) = 0,
          GetCanEditCell(ACol, ARow))
    else
    if (ACol>0) or (not FIndicator) then begin
      Font.Color := GetCellTextColour(ACol, ARow);
      lTextRect.Top := Rect.Top + 2;
      lTextRect.Left := Rect.Left + 1;
      lTextRect.Right := Rect.Right -2;
      lTextRect.Bottom := Rect.Bottom -2;
      dmInterface.DrawTerm(FGrid.Canvas, lTextRect,
                           DuplicateCharacters(FGrid.Cells[ACol, ARow], '&'),
                           False);
    end;
    { Indicator col shows a triangle if current row }
    if (ACol = 0) and FIndicator and (ARow = FGrid.Row) then begin
      Pen.Color := clBtnText;
      Brush.Color := clBtnText;
      // find dimensions of the rect, so we can calculate the triangle to draw
      lRectShape.x := Rect.Right - Rect.Left;
      lRectShape.y := Rect.Bottom - Rect.Top;
      Polygon([Point(Rect.Left + lRectShape.x div 4, Rect.Top + lRectShape.y div 5 + 1),
               Point(Rect.Right - lRectShape.x div 3, Rect.Top + lRectShape.y div 2),
               Point(Rect.Left + lRectShape.x div 4, Rect.Bottom - lRectShape.y div 5 - 1)]);
    end;
  end; // with FGrid.Canvas
  if (ACol = FGrid.Col) and (ARow = FGrid.Row) then
    //make sure data entry cell is the right position & redraw it if the
    //column width has changed
    RePositionEntryPanel(ACol, ARow);
end;  // TDataStringGrid.GridDrawCell

{-------------------------------------------------------------------------------
}
procedure TDataStringGrid.GridKeyDown(Sender: TObject; var Key: Word; Shift:
    TShiftState);
var
  lTicked: Boolean;
begin
  if ReadOnly then Exit;

  with FGrid do
    if IsCheckBoxCol(Col) and (Row >= FixedRows) and
       (Key = VK_SPACE) and GetCanEditCell(Col, Row) then
    begin
      // Read old state
      lTicked := CompareText(FGrid.Cells[Col, Row], STR_TRUE) = 0;
      Cells[Col, Row] := SysUtils.BoolToStr(not lTicked);
      if Assigned(FOnCheckboxClick) then
        FOnCheckboxClick(Col, Row);
    end;
end;  // TDataStringGrid.GridKeyDown

{-------------------------------------------------------------------------------
  Drag operation begin. 
}
procedure TDataStringGrid.GridMouseDown(Sender: TObject; Button: TMouseButton;
    Shift: TShiftState; X, Y: Integer);
var
  lCol: Integer;
  lCellRect: TRect;
  lTicked: Boolean;
begin
  if Button = mbLeft then begin
    FMouseDown := True;
    FGrid.MouseToCell(X, Y, lCol, FMouseStartDragRow);
    if FIndicator and (lCol = 0) and (FMouseStartDragRow >= FGrid.FixedRows) then begin
      HideCustomEdit;
      if FGrid.Cells[0, FMouseStartDragRow] = '0' then begin
        if not ControlKeyDown then SetSelectedRows(FMouseStartDragRow);
        FGrid.Cells[0, FMouseStartDragRow] := '1';
      end else
        FGrid.Cells[0, FMouseStartDragRow] := '0';
      FGrid.Invalidate;
    end else
    if not ReadOnly and IsCheckBoxCol(lCol) and GetCanEditCell(lCol, FMouseStartDragRow) and
       (FMouseStartDragRow >= FGrid.FixedRows) then
    begin
      lCellRect := FGrid.CellRect(lCol, FMouseStartDragRow);
      if (X >= lCellRect.Left + ((lCellRect.Right - lCellRect.Left - 12) div 2)) and
         (X <= lCellRect.Left + ((lCellRect.Right - lCellRect.Left - 12) div 2) + 13) then
      begin
        lTicked := CompareText(FGrid.Cells[lCol, FMouseStartDragRow], STR_TRUE) = 0;
        FGrid.Cells[lCol, FMouseStartDragRow] := SysUtils.BoolToStr(not lTicked);
        if Assigned(FOnCheckboxClick) then
          FOnCheckboxClick(lCol, FMouseStartDragRow);
      end;
    end;
  end;
end;  // TDataStringGrid.GridMouseDown

{-------------------------------------------------------------------------------
}
procedure TDataStringGrid.GridMouseMove(Sender: TObject; Shift: TShiftState; X,
    Y: Integer);
var
  lCol: Integer;
  lRow: Integer;
begin
  if Indicator and FMouseDown then begin
    FGrid.MouseToCell(X, Y, lCol, lRow);
    if lRow <> FMouseStartDragRow then
      SelectRowsBetween(FMouseStartDragRow, lRow);
  end;
end;  // TDataStringGrid.GridMouseMove

{-------------------------------------------------------------------------------
  End drag select operation
}
procedure TDataStringGrid.GridMouseUp(Sender: TObject; Button: TMouseButton;
    Shift: TShiftState; X, Y: Integer);
begin
  FMouseDown := False;
end;  // TDataStringGrid.GridMouseUp

{-------------------------------------------------------------------------------
}
procedure TDataStringGrid.GridSelectCell(Sender: TObject; ACol, ARow: Integer;
    var CanSelect: Boolean);
begin
  BringCellIntoView(ACol, ARow);
  SetSelectedRows(ARow);
  if not Readonly then begin
    { Can't select a readonly column }
    if (not TFieldInfo(FFieldNames[ACol]).Editable) then
      CanSelect := False
    else
    if (TFieldInfo(FFieldNames[FGrid.Col]).Required) then begin
      if (FGrid.Cells[FGrid.Col, FGrid.Row] = '') and (FGrid.Row < FGrid.RowCount) and
         (not FSaving) and (FIndicator and (ACol = 0)) then
      begin
        CanSelect := False;  // force entry for required fields
        ShowInformation(Format(ResStr_PleaseEnterAValueForThe, [FGrid.Cells[FGrid.Col, 0]]));
      end;
    end;

    if CanSelect and Assigned(FOnValidateCell) then
      FOnValidateCell(CanSelect, FGrid.Col, FGrid.Row);  // external validation

    if CanSelect then
    begin
      // validate entire row if moving off it
      if ARow <> FGrid.Row then
        ValidateRow(FGrid.Row);
      // if in checkbox, then turn off editing
      if IsCheckBoxCol(ACol) then
        FGrid.Options := FGrid.Options - [goEditing]
      else
        FGrid.Options := FGrid.Options + [goEditing];
    end;

    // If changing row, invalidate to redraw the indicator
    if ARow <> FGrid.Row then
    begin
      if FIndicator then
      begin
        TStringGridAccessor(FGrid).InvalidateCell(0, FGrid.Row);
        TStringGridAccessor(FGrid).InvalidateCell(0, ARow);
      end;
      if Assigned(FOnRowChanged) then FOnRowChanged(Self, ARow);
    end;

    ReadCurrentCellState;
    if not (FIndicator and (ACol = 0)) then
    begin
      if TFieldInfo(FFieldNames[ACol]).CustomEdit then
      begin
        if Assigned(FOnCustomEditCell) and FGrid.Visible then
        begin
          with FCustomEditPanel do
          begin
            //put the edit panel in the right place and let the client
            //draw on it.
            Visible := True;
            RePositionEntryPanel(ACol, ARow);
            FGrid.Options := FGrid.Options - [goEditing];
            FOnCustomEditCell(FCustomEditPanel, ACol, ARow);
            //Client may have cancelled the edit panel. If so then
            //set focus back to the grid.
            if not FCustomEditPanel.Visible then
              if FGrid.CanFocus then FGrid.SetFocus;
          end;
        end;
      end else begin
        FCustomEditPanel.Visible := false;
        if FGrid.CanFocus then FGrid.SetFocus;
      end;
    end else begin
      FCustomEditPanel.Visible := False;
      if FGrid.CanFocus then FGrid.SetFocus;
    end;

    if Assigned(FOldSelectCell) then
      FOldSelectCell(Sender, ACol, ARow, CanSelect);
  end;
end;  // TDataStringGrid.GridSelectCell

{-------------------------------------------------------------------------------
}
procedure TDataStringGrid.GridTopLeftChanged(Sender: TObject);
begin
  RepositionEntryPanel(FGrid.Col, FGrid.Row);
end;  // TDataStringGrid.GridTopLeftChanged 

{-------------------------------------------------------------------------------
}
procedure TDataStringGrid.HideCustomEdit;
begin
  FCustomEditPanel.Visible:= false;
end;  // TDataStringGrid.HideCustomEdit

{-------------------------------------------------------------------------------
}
function TDataStringGrid.IsCheckBoxCol(ACol: integer): Boolean;
begin
  if not (FIndicator and (ACol = 0)) then
    if (FFieldNames.Count >Acol) and (Acol >= 0) then
      if TFieldInfo(FFieldNames[ACol]).DataType  = adBoolean then
        result := true
      else
        result := false
    else
      result := false
  else
    result := false;
end;  // TDataStringGrid.IsCheckBoxCol  

{-------------------------------------------------------------------------------
}
procedure TDataStringGrid.MoveRow(iCurrentIndex, iNewIndex: Integer);
begin
  with FGrid do begin
    TMoveableGrid(FGrid).MoveRow(iCurrentIndex, iNewIndex);
  end;
end;  // TDataStringGrid.MoveRow

{-------------------------------------------------------------------------------
}
procedure TDataStringGrid.PopulateGrid(iRecordset: _Recordset);
var
  i: Integer;
  lCountCols: Integer;
  lFixedCols: Integer;
  liColStart: Integer;
  lFieldInfo: TFieldInfo;
begin
  lFixedCols := FGrid.FixedCols; // original count
  FColDataSizes.Clear;
  liColStart := IfThen(FIndicator, 1, 0);
  lCountCols := liColStart;
  { Find the columns }
  for i := liColStart to FFieldNames.Count - 1 do begin
    lFieldInfo := TFieldInfo(FFieldNames[i]);
    Inc(lCountCols);
    FGrid.ColCount := lCountCols; // resets fixed cols
    FGrid.Cells[lCountCols - 1, 0] := lFieldInfo.DisplayName;
    FGrid.ColWidths[lCountCols - 1] := lFieldInfo.ColumnWidth;
    FColDataSizes.Add(Ptr(iRecordset.Fields[lFieldInfo.FieldName].DefinedSize));
  end;
  FGrid.FixedCols := lFixedCols; // so we set it back
  FillGrid(iRecordset);
end;  // TDataStringGrid.PopulateGrid 

{-------------------------------------------------------------------------------
}
procedure TDataStringGrid.ReadCurrentCellState;
begin
  FCurrentCellState.Text := FGrid.Cells[FGrid.Col, FGrid.Row];
  if Assigned(InPlaceEdit) then begin
    FCurrentCellState.SelLength := InPlaceEdit.SelLength;
    FCurrentCellState.SelStart := InPlaceEdit.SelStart;
  end else begin   // reset for safety
    FCurrentCellState.SelLength := 0;
    FCurrentCellState.SelStart := 0;
  end;
end;  // TDataStringGrid.ReadCurrentCellState

{-------------------------------------------------------------------------------
}
procedure TDataStringGrid.RePositionEntryPanel(ACol, ARow: integer);
var
  lRect: TRect;
begin
  lRect := FGrid.CellRect(ACol, ARow);
  lRect.TopLeft := FGrid.Parent.ScreenToClient(FGrid.ClientToScreen(lRect.TopLeft));
  lRect.BottomRight := FGrid.Parent.ScreenToClient(FGrid.ClientToScreen(lRect.BottomRight));
  FCustomEditPanel.SetBounds(lRect.Left - 1, lRect.Top - 1,
                             lRect.Right - lRect.Left + 2, lRect.Bottom - lRect.Top + 2);
  if Assigned(FOnRepositionCustomEdit) then
    FOnRepositionCustomEdit(FCustomEditPanel, ACol, ARow);
end;  // TDataStringGrid.RePositionEntryPanel

{-------------------------------------------------------------------------------
}
function TDataStringGrid.RowContainsData(iRow: integer): Boolean;
var
  i: Integer;
begin
  Result := False;

  // Skip column zero which is the Validated cell
  for i := 1 + Ord(Indicator) to FGrid.ColCount - 1 do
    Result := Result or (FGrid.Cells[i, iRow] <> '');
end;  // TDataStringGrid.RowContainsData

{-------------------------------------------------------------------------------
  Selects all the rows between the 2 rows (the row that drag started on, or was
      selected before the shift click to the current row).
}
procedure TDataStringGrid.SelectRowsBetween(AFromRow, AToRow: integer);
var
  i: Integer;
begin
  with FGrid do begin
    if (AFromRow < FixedRows) or (AToRow < FixedRows) then Exit;
    
    // If Control pressed, keep previous selection(s), otherwise, clear everything.
    if not ControlKeyDown then
      for i := FixedRows to RowCount - 1 do
        Cells[0, i] := '0';

    // Select what is wanted.
    for i := Min(AFromRow, AToRow) to Max(AFromRow, AToRow) do
      Cells[0, i] := '1';
  end;
  FGrid.Invalidate;
end;  // TDataStringGrid.SelectRowsBetween

{-------------------------------------------------------------------------------
}
procedure TDataStringGrid.SetColumnWidth(Index: Integer; Value: Integer);
begin
  if not(FIndicator and  (Index = 0)) then
  begin
    TFieldInfo(FFieldNames[Index]).ColumnWidth := Value;
    if FGrid.ColCount > Index then
      FGrid.ColWidths[Index] := Value;
  end;
end;  // TDataStringGrid.SetColumnWidth 

{-------------------------------------------------------------------------------
}
procedure TDataStringGrid.SetCustomEdit(Index: Integer; const Value: Boolean);
begin
  if not(FIndicator and  (Index = 0)) then
    TFieldInfo(FFieldNames[Index]).CustomEdit := Value;
end;  // TDataStringGrid.SetCustomEdit 

{-------------------------------------------------------------------------------
}
procedure TDataStringGrid.SetDisplayName(Index: Integer; const Value: string);
var
  lRect: TRect;
begin
  if not(FIndicator and  (Index = 0)) then
  begin
    TFieldInfo(FFieldNames[Index]).DisplayName := Value;
  
    if FGrid.ColCount > Index then
    begin
      FGrid.ColWidths[Index] := lRect.Right - lRect.Left + 4;
      DetectColumnWidth(Index);
    end;
  end;
end;  // TDataStringGrid.SetDisplayName 

{-------------------------------------------------------------------------------
}
procedure TDataStringGrid.SetIndicator(const Value: Boolean);
begin
  if (not FIndicator) and Value then
    FFieldNames.Insert(0, nil)
  else if FIndicator and (not Value) then
    FFieldNames.Delete(0);
  FIndicator := Value;
end;  // TDataStringGrid.SetIndicator 

{-------------------------------------------------------------------------------
}
procedure TDataStringGrid.SetInvisibleColValue(iColumnName: string; iRow:
    integer; const Value: string);
var
  i: Integer;
  lIdx: Integer;
begin
  lIdx := FInvisibleCols.IndexOf(iColumnName);
  if lIdx = -1 then
    raise EDataStringGridError(Format(ResStr_InvalidMethodCall,
                               ['TDataStringGrid.SetInvisibleColValue ' + iColumnName]));
  if (iRow>=0) then
    with TStringList(FInvisibleCols.Objects[lIdx]) do
    begin
      if (iRow< Count) then
        Strings[iRow] := Value
      else
        if iRow < FGrid.RowCount then
        begin
          //someone has added extra rows to the grid; add extra items to the
          // string list
          for i := Count to FGrid.RowCount -1 do
            Add('');
          Strings[iRow] := Value
        end
        else
          Raise EDataStringGridError.Create(Format('Invalid Index (%d)', [iRow]));
    end
  else
    Raise EDataStringGridError.Create(Format('Invalid Index (%d)', [iRow]));
end;  // TDataStringGrid.SetInvisibleColValue 

{-------------------------------------------------------------------------------
}
procedure TDataStringGrid.SetOnCellBGColour(const Value: TCellColourEvent);
begin
  FOnCellBGColour := Value;
end;  // TDataStringGrid.SetOnCellBGColour 

{-------------------------------------------------------------------------------
}
procedure TDataStringGrid.SetOnCellTextColour(const Value: TCellColourEvent);
begin
  FOnCellTextColour := Value;
end;  // TDataStringGrid.SetOnCellTextColour

{-------------------------------------------------------------------------------
}
procedure TDataStringGrid.SetOnCustomEditCell(Value: TCustomEditCell);
begin
  FOnCustomEditCell := Value;
end;  // TDataStringGrid.SetOnCustomEditCell 

{-------------------------------------------------------------------------------
}
procedure TDataStringGrid.SetOnDeleteRow(const Value: TDeleteRowEvent);
begin
  FOndeleteRow := Value;
end;  // TDataStringGrid.SetOnDeleteRow 

{-------------------------------------------------------------------------------
  Accessor method for OnRepositionCustomEdit event, triggered when the custom
      edit panel gets resized.
}
procedure TDataStringGrid.SetOnRepositionCustomEdit(Value: TCustomEditCell);
begin
  FOnRepositionCustomEdit := Value;
end;  // TDataStringGrid.SetOnRepositionCustomEdit 

{-------------------------------------------------------------------------------
}
procedure TDataStringGrid.SetOnRowChanged(const Value: TRowChangedEvent);
begin
  FOnRowChanged := Value;
end;  // TDataStringGrid.SetOnRowChanged

{-------------------------------------------------------------------------------
}
procedure TDataStringGrid.SetOnUpdateRow(const Value: TUpdateRowEvent);
begin
  FOnUpdateRow := Value;
end;  // TDataStringGrid.SetOnUpdateRow 

{-------------------------------------------------------------------------------
}
procedure TDataStringGrid.SetOnValidateCell(const Value: TValidateCellEvent);
begin
  FOnValidateCell := Value;
end;  // TDataStringGrid.SetOnValidateCell 

{-------------------------------------------------------------------------------
}
procedure TDataStringGrid.SetReadOnly(Value: Boolean);
begin
  FReadOnly := Value;
end;  // TDataStringGrid.SetReadOnly 

{-------------------------------------------------------------------------------
  When the indicator is clicked on, update the current selection range. 
}
procedure TDataStringGrid.SetSelectedRows(ARowClicked: integer);
var
  lIdx: Integer;
begin
  if FIndicator then begin
    // If this is a second click, now with the shift key down, select the range
    if ShiftKeyDown and (FSelectionAnchorRow > -1) then
      SelectRowsBetween(FSelectionAnchorRow, ARowClicked)
    else
    if ControlKeyDown and (FSelectionAnchorRow > -1) then begin
      // The anchor row should be selected
      FGrid.Cells[0, FSelectionAnchorRow] := '1';
      // Toggle the clicked row
      if FGrid.Cells[0, ARowClicked] = '1' then
        FGrid.Cells[0, ARowClicked] := '0'
      else
        FGrid.Cells[0, ARowClicked] := '1';
    end
    else begin
      // Set the anchor for future shift clicks - this click starts a new selection
      FSelectionAnchorRow := ARowClicked;
      for lIdx := 1 to FGrid.RowCount-1 do
        if FGrid.Cells[0, lIdx] <> '0' then
          FGrid.Cells[0, lIdx] := '0'; // not selected
    end;
    FGrid.Invalidate;
  end; // if
end;  // TDataStringGrid.SetSelectedRows

{-------------------------------------------------------------------------------
}
procedure TDataStringGrid.SetTimestampCol(iColumnName: String; IRow: integer;
    Value: TSQLSvrTimestamp);
var
  i: Integer;
begin
  if (iRow>=0) then
    with TStringList(FTimestampCols.Objects[FTimestampCols.
        IndexOf(iColumnName)]) do
    begin
      if (iRow< Count) then
        TVariantObject(Objects[iRow]).OLEVariant := Value
      else
        if iRow < FGrid.RowCount then
        begin
          //someone has added extra rows to the grid; add extra items to the
          // string list
          for i := Count to FGrid.RowCount -1 do
            AddObject('', TVariantObject.Create(''));
          TVariantObject(Objects[iRow]).OLEVariant := Value
        end
        else
          Raise EDataStringGridError.Create(Format('Invalid Index (%d)', [iRow]));
    end
  else
    Raise EDataStringGridError.Create(Format('Invalid Index (%d)', [iRow]));
end;  // TDataStringGrid.SetTimestampCol 

{-------------------------------------------------------------------------------
  Returns true if either shift key is down. 
}
function TDataStringGrid.ShiftKeyDown: Boolean;
begin
  Result := (GetKeyState(VK_LSHIFT) < 0) or (GetKeyState(VK_RSHIFT) < 0);
end;  // TDataStringGrid.ShiftKeyDown 

{-------------------------------------------------------------------------------
}
procedure TDataStringGrid.UpdateWholeInvisibleColumn(const AColumnName,
    ANewValue: String);
var
  lStringList: TStringList;
  i: Integer;
begin
  lStringList := TStringList(FInvisibleCols.Objects[FInvisibleCols.IndexOf(AColumnName)]);
  {Make sure that the string list has the same number of rows as the grid.}
  while lStringList.Count > FGrid.RowCount do
    lStringList.Delete(lStringList.Count -1);
  for i := 1 to lStringList.Count - 1 do
    lStringList[i] := ANewValue;
  while lStringList.Count < FGrid.RowCount do
    lStringList.Add(ANewValue);
end;  // TDataStringGrid.UpdateWholeInvisibleColumn 

{-------------------------------------------------------------------------------
}
procedure TDataStringGrid.ValidateRow(iRow: integer);
var
  lCol: Integer;
begin
  for lCol := 0 to FGrid.ColCount-1 do begin
  
    if Assigned(FGrid.Objects[lCol, FGrid.Row]) then
    begin
      // ignore read only or indicator col
      if ((lCol = 0) and FIndicator) then
        Continue;
      if  (not TFieldInfo(FFieldNames[lCol]).Editable) then
        Continue; // next col
    if (TFieldInfo(FFieldNames[lCol]).Required) and RowContainsData(
        iRow) then
      if (FGrid.Cells[lCol, iRow] = '') then begin
        FGrid.Col := lCol;
        FGrid.Row := iRow;
        FGrid.EditorMode := True;
  //        Raise EDataStringGridError.CreateValidation('Please enter a value for the ' + FGrid.Cells[lCol, 0], FGrid);
      end;
    end;
  end; // for lCol
end;  // TDataStringGrid.ValidateRow

{-==============================================================================
    TFieldInfo
===============================================================================}
{-------------------------------------------------------------------------------
}
constructor TFieldInfo.Create(ADisplayName, AFieldName : String; AColumnWidth:
    Integer; ADataType: TOleEnum; ARequired: boolean; AEditable: boolean);
begin
  FDisplayName := ADisplayName;
  FFieldName := AFieldName;
  FDataType := ADataType;
  FColumnWidth := AColumnWidth;
  FEditable := Aeditable;
  FRequired := ARequired;
end;  // TFieldInfo.Create 

{-------------------------------------------------------------------------------
}
procedure TFieldInfo.SetColumnWidth(const Value: Integer);
begin
  FColumnWidth := Value;
end;  // TFieldInfo.SetColumnWidth 

{-------------------------------------------------------------------------------
}
procedure TFieldInfo.SetDataType(const Value: TOleEnum);
begin
  FDataType := Value;
end;  // TFieldInfo.SetDataType 

{-------------------------------------------------------------------------------
}
procedure TFieldInfo.SetDisplayName(const Value: string);
begin
  FDisplayName := Value;
end;  // TFieldInfo.SetDisplayName 

{-------------------------------------------------------------------------------
}
procedure TFieldInfo.SetEditable(const Value: Boolean);
begin
  FEditable := Value;
end;  // TFieldInfo.SetEditable 

{-------------------------------------------------------------------------------
}
procedure TFieldInfo.SetFieldName(const Value: string);
begin
  FFieldName := Value;
end;  // TFieldInfo.SetFieldName 

{-------------------------------------------------------------------------------
}
procedure TFieldInfo.SetRequired(const Value: Boolean);
begin
  FRequired := Value;
end;  // TFieldInfo.SetRequired

{-==============================================================================
    TMultiValueDataStringGrid
===============================================================================}
{-------------------------------------------------------------------------------
}
constructor TMultiValueDataStringGrid.Create(iGrid: TStringGrid);
begin
  inherited Create(iGrid);
end;  // TMultiValueDataStringGrid.Create

{-------------------------------------------------------------------------------
}
destructor TMultiValueDataStringGrid.Destroy;
var
  i, j: Integer;
begin
  if Assigned(FInvisibleCols) then
  begin
    for i := FInvisibleCols.Count - 1 downto 0 do begin
      for j := TStringList(FInvisibleCols.Objects[i]).Count - 1 downto 0 do
        TStringList(TStringList(FInvisibleCols.Objects[i]).Objects[j]).Free;
    end;
  end;

  if Assigned(FTimestampCols) then
  begin
    ClearTimestamps;
    for i := FTimestampCols.Count - 1 downto 0 do begin
      for j := TStringList(FTimestampCols.Objects[i]).Count -1 downto 0 do
        TStringList(TStringList(FTimestampCols.Objects[i]).Objects[j]).Free;
    end;
  end;
  // The other things needed to be done with FInvisibleCols and FTimestampCols
  // are handled in the inherited method.
  inherited;
end;  // TMultiValueDataStringGrid.Destroy

{-------------------------------------------------------------------------------
  Clears the timestamp string lists and frees associated variant objects.
}
procedure TMultiValueDataStringGrid.ClearTimestamps;
var
  i: Integer;
  j: Integer;
  k: Integer;
begin
  for i := FTimestampCols.Count - 1 downto 0 do begin
    for j := 1 to TStringList(FTimestampCols.Objects[i]).Count-1 do begin
      for k := 1 to TStringList(TStringList(FTimestampCols.Objects[i]).Objects[j]).Count-1 do
        // Free timestamp objects
        TVariantObject(TStringList(TStringList(FTimestampCols.Objects[i]).Objects[j]).Objects[k]).Free;
      TStringList(TStringList(FTimestampCols.Objects[i]).Objects[j]).Clear;
    end;
    TStringList(FTimestampCols.Objects[i]).Clear;
  end;
end;  // TMultiValueDataStringGrid.ClearTimestamps

{-------------------------------------------------------------------------------
}
function TMultiValueDataStringGrid.GetInvisibleColValue(iColumnName: string; iRow: integer;
    iItem: integer): string;
begin 
  with TStringList(FInvisibleCols.Objects[FInvisibleCols.IndexOf(iColumnName)]) do
    if (iRow >= 0) and (iRow < Count) then begin
      //result := Strings[iRow]
      if (iItem >= 0) and (iItem < TStringList(Objects[iRow]).Count) then
        result := TStringList(Objects[iRow]).Strings[iItem]
      else
        result := '';
    end else
      result := '';
end;  // TMultiValueDataStringGrid.GetInvisibleColValue
 
{-------------------------------------------------------------------------------
}
function TMultiValueDataStringGrid.GetTimestampCol(iColumnName: String; IRow: integer;
  IItem: integer): TSQLSvrTimestamp;
begin   
  with TStringList(FTimestampCols.Objects[FTimestampCols.IndexOf(iColumnName)]) do
    if (iRow >= 0) and (iRow < Count) then begin
      //result := TVariantObject(Objects[iRow]).OLEVariant
      if (iItem >= 0) and (iItem < TStringList(Objects[iRow]).Count) then
        result := TVariantObject(TStringList(Objects[iRow]).Objects[iItem]).OLEVariant
      else
        result := null;
    end else
      result := null;
end;  // TMultiValueDataStringGrid.GetTimestampCol

{-------------------------------------------------------------------------------
}
procedure TMultiValueDataStringGrid.SetInvisibleColValue(iColumnName: string; iRow: integer;
    iItem: integer; const Value: string);
var
  i: Integer;
  lIdx: Integer;
begin
  lIdx := FInvisibleCols.IndexOf(iColumnName);
  if lIdx = -1 then
    raise EDataStringGridError(Format(ResStr_InvalidMethodCall,
                               ['TDataStringGrid.SetInvisibleColValue ' + iColumnName]));
  if (iRow>=0) then
    with TStringList(FInvisibleCols.Objects[lIdx]) do
    begin
      if (iRow < Count) then begin
        //Strings[iRow] := Value
        if (iItem < TStringList(Objects[iRow]).Count) then
          TStringList(Objects[iRow]).Strings[iItem] := Value
        else begin
          for i := TStringList(Objects[iRow]).Count to iItem do
            TStringList(Objects[iRow]).Add('');
          TStringList(Objects[iRow]).Strings[iItem] := Value;
        end;
      end else
        if iRow < FGrid.RowCount then
        begin
          //someone has added extra rows to the grid; add extra items to the
          // string list
          for i := Count to FGrid.RowCount -1 do
            AddObject(IntToStr(iRow), TStringList.Create);
          if (iItem < TStringList(Objects[iRow]).Count) then
            TStringList(Objects[iRow]).Strings[iItem] := Value
          else begin
            for i := TStringList(Objects[iRow]).Count to iItem do
              TStringList(Objects[iRow]).Add('');
            TStringList(Objects[iRow]).Strings[iItem] := Value;
          end;
        end
        else
          Raise EDataStringGridError.Create(Format('Invalid Index (%d)', [iRow]));
    end
  else
    Raise EDataStringGridError.Create(Format('Invalid Index (%d)', [iRow]));
end;  // TMultiValueDataStringGrid.SetInvisibleColValue

{-------------------------------------------------------------------------------
}
procedure TMultiValueDataStringGrid.SetTimestampCol(iColumnName: String; IRow: integer; IItem: integer; Value:
    TSQLSvrTimestamp);
var
  i: Integer;
begin
  if (iRow>=0) then
    with TStringList(FTimestampCols.Objects[FTimestampCols.
        IndexOf(iColumnName)]) do
    begin
      if (iRow < Count) then begin
        if not (Objects[iRow] is TStringList) then begin
          Objects[iRow] := TStringList.Create;
        end;
        if (iItem < TStringList(Objects[iRow]).Count) then
          TVariantObject(TStringList(Objects[iRow]).Objects[iItem]).OLEVariant := Value
        else begin
          for i := TStringList(Objects[iRow]).Count to iItem do
            TStringList(Objects[iRow]).AddObject('', TVariantObject.Create(''));
          TVariantObject(TStringList(Objects[iRow]).Objects[iItem]).OLEVariant := Value;
        end;
      end else
        if iRow < FGrid.RowCount then
        begin
          //someone has added extra rows to the grid; add extra items to the
          // string list
          for i := Count to FGrid.RowCount -1 do
            AddObject(IntToStr(iRow), TStringList.Create);
          if (iItem < TStringList(Objects[iRow]).Count) then
            TVariantObject(TStringList(Objects[iRow]).Objects[iItem]).OLEVariant := Value
          else begin
            for i := TStringList(Objects[iRow]).Count to iItem do
              TStringList(Objects[iRow]).AddObject('', TVariantObject.Create(''));
            TVariantObject(TStringList(Objects[iRow]).Objects[iItem]).OLEVariant := Value;
          end;
        end
        else
          Raise EDataStringGridError.Create(Format('Invalid Index (%d)', [iRow]));
    end
  else
    Raise EDataStringGridError.Create(Format('Invalid Index (%d)', [iRow]));
end;  // TMultiValueDataStringGrid.SetTimestampCol

{-------------------------------------------------------------------------------
}
procedure TMultiValueDataStringGrid.DeleteRow(iRowIndex: integer);
var
  i: Integer;
  j: Integer;
begin
  for i := 0 to FInvisibleCols.Count -1  do
    with TStringList(FInvisibleCols.Objects[i]) do
      if iRowIndex < Count then begin
        TStringList(Objects[iRowIndex]).Free;
        Delete(iRowIndex);
      end;

  for i := 0 to FTimestampCols.Count -1  do
    with TStringList(FTimestampCols.Objects[i]) do
      if iRowIndex < Count then begin
        for j := 0 to TStringList(Objects[iRowIndex]).Count do
          if (Objects[j] is TVariantObject) then
            TVariantObject(Objects[j]).Free;
        TStringList(Objects[iRowIndex]).Free;
        Delete(iRowIndex);
      end;

  with FGrid do begin
    // move all rows up to fill the gap
    for i := iRowIndex to RowCount - 2 do
      Rows[i].Assign(Rows[i + 1]);

    if Row >= RowCount then
      Row := Row - 1;
    // Remove the row - always leave at least 1 row though
    if RowCount > FixedRows + 1 then
      RowCount := RowCount - 1
    else
      Rows[Row].Clear;
  end;
end;  // TMultiValueDataStringGrid.DeleteRow

{-------------------------------------------------------------------------------
}
procedure TMultiValueDataStringGrid.UpdateWholeInvisibleColumn(const AColumnName: string;
    ANewValues: TStringList);
var
  lStringList: TStringList;
  i: Integer;
begin
  lStringList := TStringList(FInvisibleCols.Objects[FInvisibleCols.IndexOf(AColumnName)]);
  {Make sure that the string list has the same number of rows as the grid.}
  while lStringList.Count > FGrid.RowCount do
    lStringList.Delete(lStringList.Count -1);
  for i := 1 to lStringList.Count - 1 do
    lStringList.Objects[i] := ANewValues;
  while lStringList.Count < FGrid.RowCount do
    lStringList.AddObject(IntToStr(lStringList.Count), ANewValues);
end;  // TMultiValueDataStringGrid.UpdateWholeInvisibleColumn

{-------------------------------------------------------------------------------
  Trap the moving of a row and ensure our hidden information also moves.
}
procedure TDataStringGrid.GridRowMoved(Sender: TObject; FromIndex,
  ToIndex: Integer);
var
  i: Integer;
begin
  for i := 0 to FInvisibleCols.Count -1  do
    with TStringList(FInvisibleCols.Objects[i]) do
      Move(FromIndex, ToIndex);

  for i := 0 to FTimestampCols.Count -1  do
    with TStringList(FTimestampCols.Objects[i]) do
      Move(FromIndex, ToIndex);

  // fire event on the owning form if it is hooked up.
  if Assigned(FOldRowMoved) then
    FOldRowMoved(Sender, FromIndex, ToIndex);
end;

initialization
  STR_TRUE := SysUtils.BoolToStr(True);
  STR_FALSE := SysUtils.BoolToStr(False);

end.
