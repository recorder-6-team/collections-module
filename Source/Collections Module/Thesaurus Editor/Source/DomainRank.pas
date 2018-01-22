{===============================================================================
  Unit:        DomainRank.pas

  Defines:     TfraDomainRank

  Description: Tab page allowing the ranks available within a domain to be
               edited

  Model:       ThesaurusEditor.mpb

  Created:     13/11/2003

  Last revision information:
    $Revision: 12 $
    $Date: 15/12/06 10:59 $
    $Author: Ericsalmon $

===============================================================================}
unit DomainRank;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, BaseTabSheetFrameUnit, ExtCtrls, StdCtrls, Grids,
  ImageListButton, ColorBtn, InterfaceDataModule, LuxembourgDataClasses,
  ADOInt, DataClasses, DataTypes, DssStringGrid, ResourceStrings, ExceptionForm,
  RestrictedEdits, GeneralFunctions, LuxembourgConstants, DSSDataTypes;

type

  TDomainRankItem = class(TLuxGridDataItem)
  private
    FAbbreviation: string;
    FColor: string;
    FOrder: string;
    FRankName: string;
    FTimestamp: TSQLSvrTimestamp;
  protected
    procedure GetData(const Column: Integer; var AText: String; var AKey:
            TKeyString); override;
    function GetDisplayText(Column: Integer): string; override;
    procedure InitFromRecord(AFields: Fields); override;
    procedure SetData(const Column: Integer; const AText: String; const AKey:
            TKeyString); override;
    procedure ValidateData; override;
  public
    property Abbreviation: string read FAbbreviation write FAbbreviation;
    property Color: string read FColor write FColor;
    property Order: string read FOrder write FOrder;
    property RankName: string read FRankName write FRankName;
    property Timestamp: TSQLSvrTimestamp read FTimestamp write FTimestamp;
  end;
  
  TDomainRankList = class(TLuxGridDataList)
  protected
    procedure DoAddition(AItem: TLuxCachedDataItem); override;
    procedure DoDeletion(AItem: TLuxCachedDataItem); override;
    procedure DoModification(AItem: TLuxCachedDataItem); override;
    function GetRecordset: _Recordset; override;
  end;
  
  {-----------------------------------------------------------------------------
    Tab page allowing the ranks available within a domain to be edited.
  }
  TfraDomainRank = class(TBaseTabSheetFrame)
    btnAdd: TImageListButton;
    btnApplySort: TImageListButton;
    btnDelete: TImageListButton;
    btnEditColor: TColorButton;
    Label1: TLabel;
    sgRankEditor: TDSSStringGrid;
    procedure btnAddClick(Sender: TObject);
    procedure btnApplySortClick(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
    procedure btnEditColorKeyDown(Sender: TObject; var Key: Word; Shift:
            TShiftState);
    procedure sgRankEditorDrawCell(Sender: TObject; ACol, ARow: Integer; Rect:
            TRect; State: TGridDrawState);
    procedure sgRankEditorKeyDown(Sender: TObject; var Key: Word; Shift:
            TShiftState);
  private
    FDomainRankList: TDomainRankList;
    function CanAddRow: Boolean;
    procedure CustomGetData(AWinControl: TWinControl; var Text, Key: String);
    procedure CustomSetData(AWinControl: TWinControl; const Text, Key: String);
    procedure DeleteKeyPressed;
    procedure DisplayFindDialog;
  protected
    procedure DeleteData; override;
    procedure EnableControls(AEnabled: Boolean); override;
    procedure LoadData; override;
    procedure SaveData; override;
    procedure Sort; virtual;
    procedure ValidateData; override;
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
  end;
  
var
  fraDomainRank: TfraDomainRank;

implementation

{$R *.dfm}

uses GeneralData, SearchManager;

type
  TGridHack = class(TStringGrid)
  end;
  
{-==============================================================================
    TfraDomainRank
===============================================================================}
{-------------------------------------------------------------------------------
}
constructor TfraDomainRank.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  
  sgRankEditor.Rows[0].CommaText := ResStr_Order + ',' +
                                    ResStr_RankName + ',' +
                                    ResStr_Abbr + ',' +
                                    ResStr_Color;
  FDomainRankList := TDomainRankList.Create(TDomainRankItem, sgRankEditor);
  FDomainRankList.OnCustomGetData := CustomGetData;
  FDomainRankList.OnCustomSetData := CustomSetData;
  sgRankEditor.ColumnsInfo[3].WinControl := btnEditColor;
  btnEditColor.Visible := False;
  btnEditColor.OnKeyDown := btnEditColorKeyDown;
end;  // TfraDomainRank.Create 

{-------------------------------------------------------------------------------
}
destructor TfraDomainRank.Destroy;
begin
  FDomainRankList.Free;
  inherited;
end;  // TfraDomainRank.Destroy 

{-------------------------------------------------------------------------------
}
procedure TfraDomainRank.btnAddClick(Sender: TObject);
begin
  inherited;
  with sgRankEditor do begin
    if CanAddRow then begin
      FDomainRankList.AddNew(TDomainRankItem.CreateNew(FDomainRankList));
      Col := 0;
    end;
    Row := RowCount - 1;
    SetFocus;
  end;
end;  // TfraDomainRank.btnAddClick 

{-------------------------------------------------------------------------------
}
procedure TfraDomainRank.btnApplySortClick(Sender: TObject);
begin
  inherited;
  Sort;
end;  // TfraDomainRank.btnApplySortClick 

{-------------------------------------------------------------------------------
}
procedure TfraDomainRank.btnDeleteClick(Sender: TObject);
begin
  inherited;
  if MessageDlg(Format(ResStr_ConfirmRowDelete, [ResStr_Row]),
                                      mtWarning, [mbYes, mbNo], 0) = mrYes then
    FDomainRankList.DeleteItem(sgRankEditor.Row);
end;  // TfraDomainRank.btnDeleteClick 

{-------------------------------------------------------------------------------
}
procedure TfraDomainRank.btnEditColorKeyDown(Sender: TObject; var Key: Word;
        Shift: TShiftState);
begin
  inherited;
  case Key of
    VK_LEFT: begin
      sgRankEditor.Col := sgRankEditor.Col - 1;
      sgRankEditor.SetFocus;
    end;
    VK_UP:
      if sgRankEditor.Row > 1 then sgRankEditor.Row := sgRankEditor.Row - 1;
    VK_DOWN:
      if sgRankEditor.Row < sgRankEditor.RowCount - 1 then
        sgRankEditor.Row := sgRankEditor.Row + 1
      else if EditMode = emEdit then begin
        sgRankEditor.RowCount := sgRankEditor.RowCount + 1;
        sgRankEditor.Row := sgRankEditor.Row + 1
      end;
  else
    sgRankEditorKeyDown(sgRankEditor, Key, Shift);
  end;
end;  // TfraDomainRank.btnEditColorKeyDown 

{-------------------------------------------------------------------------------
}
function TfraDomainRank.CanAddRow: Boolean;
begin
  with sgRankEditor do
    Result := (Cells[0, RowCount - 1] <> '') or (Cells[1, RowCount - 1] <> '') or
              (Cells[2, RowCount - 1] <> '') or (Cells[3, RowCount - 1] <> '');
end;  // TfraDomainRank.CanAddRow 

{-------------------------------------------------------------------------------
}
procedure TfraDomainRank.CustomGetData(AWinControl: TWinControl; var Text, Key:
        String);
begin
  Text := IntToStr(btnEditColor.ActiveColor);
end;  // TfraDomainRank.CustomGetData 

{-------------------------------------------------------------------------------
}
procedure TfraDomainRank.CustomSetData(AWinControl: TWinControl; const Text,
        Key: String);
begin
  if Text <> '' then
    btnEditColor.ActiveColor := TColor(StrToInt(Text));
end;  // TfraDomainRank.CustomSetData 

{-------------------------------------------------------------------------------
}
procedure TfraDomainRank.DeleteData;
begin
  dmGeneral.RunDeleteStoredProc('usp_ConceptRanks_Delete', ['@Key', Key]);
end;  // TfraDomainRank.DeleteData 

{-------------------------------------------------------------------------------
}
procedure TfraDomainRank.DeleteKeyPressed;
begin
  with sgRankEditor do begin
    if (Cells[0, Row] = '') and (Cells[1, Row] = '') and (Cells[2, Row] = '') then
      btnDeleteClick(Self)
    else
      Cells[Col, Row] := '';
  end;
end;  // TfraDomainRank.DeleteKeyPressed 

{-------------------------------------------------------------------------------
  If 'return' is pressed when the second column of the grid is focussed this
          method is called. It shows the find dialog and allows the user to
          choose a concept rank. If the contents of the cell already has one
          exact match, the find dialog will not be shown.
}
procedure TfraDomainRank.DisplayFindDialog;
var
  lSearchResultKey, lSearchResultText: string;
  lRecordset: _recordset;
begin
  with TSearchManager.Create do begin
    SearchType := stConceptRank;
    lSearchResultKey := RunSearch(sgRankEditor.Cells[1, sgRankEditor.Row]);
    lSearchResultText := ResultText;
    Free;
  end;
  lRecordset := dmGeneral.GetRecordset('usp_ConceptRank_Select',
                                      ['@ConceptRankKey', lSearchResultKey]);
  with sgRankEditor do
    with TDomainRankItem(sgRankEditor.Objects[0,Row]) do
      with lRecordset do
        if not EOF then begin
          // Don't change the RankName variable here because then SetModified
          // won't be set.
          Abbreviation  := Fields['Abbreviation'].Value;
          Color         := Fields['Color_R'].Value shl 16
                            + Fields['Color_G'].Value shl 8
                            + Fields['Color_B'].Value;
          // Need to change the cell contents itself because SetData will be
          // called later and will use the contents to overwrite the value stored.
          Cells[1, Row] := Fields['Item_Name'].Value;
        end;
end;  // TfraDomainRank.DisplayFindDialog 

{-------------------------------------------------------------------------------
}
procedure TfraDomainRank.EnableControls(AEnabled: Boolean);
begin
  inherited;
  with sgRankEditor do begin
    ReadOnly := not AEnabled;
    if AEnabled and CanFocus then SetFocus;
  end;
  btnApplySort.Enabled := True;
end;  // TfraDomainRank.EnableControls 

{-------------------------------------------------------------------------------
}
procedure TfraDomainRank.LoadData;
begin
  inherited;
  FDomainRankList.MasterKey := Key;
  FDomainRankList.Refresh;
end;  // TfraDomainRank.LoadData 

{-------------------------------------------------------------------------------
}
procedure TfraDomainRank.SaveData;
begin
  FDomainRankList.MasterKey := Key;
  FDomainRankList.Update;
  FDomainRankList.Refresh;
end;  // TfraDomainRank.SaveData 

{-------------------------------------------------------------------------------
}
procedure TfraDomainRank.sgRankEditorDrawCell(Sender: TObject; ACol, ARow:
        Integer; Rect: TRect; State: TGridDrawState);
var
  lColorString: string;
begin
  inherited;
  if (ACol=3) and (ARow>0) then begin
    Rect := Classes.Rect(Rect.Left+2, Rect.Top+2, Rect.Right-4, Rect.Bottom-4);
    with sgRankEditor do begin
      if Assigned(TDomainRankItem(Cols[0].Objects[ARow])) then begin
        lColorString := TDomainRankItem(Cols[0].Objects[ARow]).Color;
        if lColorString <> '' then begin
          Canvas.Brush.Color := TColor(StrToInt(lColorString));
          Canvas.FillRect(Rect);
        end; // if lColorString <> ''
      end; // if Assigned
    end;  // with sgRankEditor
  end;  // if (ACol=3) and (ARow>0)
end;  // TfraDomainRank.sgRankEditorDrawCell 

{-------------------------------------------------------------------------------
}
procedure TfraDomainRank.sgRankEditorKeyDown(Sender: TObject; var Key: Word;
        Shift: TShiftState);
begin
  inherited;
  with sgRankEditor do
    if EditMode = emEdit then
      case Key of
        VK_DELETE: DeleteKeyPressed;
        VK_INSERT: btnAddClick(btnAdd);
        VK_DOWN:   if (Row = RowCount-1) then begin
                     Key := 0;
                     if CanAddRow then btnAddClick(btnAdd);
                   end;
        VK_RETURN: if Col = 1 then DisplayFindDialog;
      end;
end;  // TfraDomainRank.sgRankEditorKeyDown 

{-------------------------------------------------------------------------------
}
procedure TfraDomainRank.Sort;
  
  function SortOrder(Row: Integer): Integer;
  begin
    try
      Result := StrToInt(sgRankEditor.Cells[0, Row]);
    except
      on EConvertError do Result := 0;
    end;
  end;
  
  function CompareRows(I, J: Integer): Integer;
  begin
    Result := SortOrder(I) - SortOrder(J);
    if Result = 0 then
      Result := CompareText(sgRankEditor.Cells[1, I], sgRankEditor.Cells[1, J]);
  end;
  
  procedure ExchangeRows(I, J: Integer);
  begin
    if I > J then
      ExchangeRows(J, I)
    else if I < J then
    begin
      TGridHack(sgRankEditor).MoveRow(I, J);
      TGridHack(sgRankEditor).MoveRow(J - 1, I);
    end;
  end;
  
  procedure Quicksort(L, R: Integer);
  var
    I, J, P: Integer;
  begin
    I := L;
    J := R;
    P := (L + R) shr 1;
    while I <= J do
    begin
      while CompareRows(I, P) < 0 do Inc(I);
      while CompareRows(J, P) > 0 do Dec(J);
      if I <= J then
      begin
        ExchangeRows(I, J);
        if P = I then
          P := J
        else if P = J then
          P := I;
        Inc(I);
        Dec(J);
      end;
    end;
    if L < J then Quicksort(L, J);
    if I < R then Quicksort(I, R);
  end;
  
begin
  QuickSort(1, sgRankEditor.RowCount - 1);
end;  // TfraDomainRank.Sort 

{-------------------------------------------------------------------------------
}
procedure TfraDomainRank.ValidateData;
begin
  inherited;
  FDomainRankList.ValidateContent;
end;  // TfraDomainRank.ValidateData 

{-==============================================================================
    TDomainRankList
===============================================================================}
{-------------------------------------------------------------------------------
}
procedure TDomainRankList.DoAddition(AItem: TLuxCachedDataItem);
var
  lOrder: Variant;
begin
  with TDomainRankItem(AItem) do begin
    if Order <> ''  then lOrder := StrToInt(Order)
                    else lOrder := null;
    dmGeneral.RunInsertStoredProc(TN_CONCEPT_RANK,
                                  'usp_ConceptRank_Insert',
                                 ['@DomainKey', MasterKey,
                                  '@ItemName', RankName,
                                  '@SortOrder', lOrder,
                                  '@Abbreviation', Abbreviation,
                                  '@ColorR', (StrToInt(Color) shr 16) and $FF,
                                  '@ColorG', (StrToInt(Color) shr 8) and $FF,
                                  '@ColorB', StrToInt(Color) and $FF
                                 ], '@Key');
  end;
end;  // TDomainRankList.DoAddition 

{-------------------------------------------------------------------------------
}
procedure TDomainRankList.DoDeletion(AItem: TLuxCachedDataItem);
begin
  with TDomainRankItem(AItem) do
    dmGeneral.RunDeleteStoredProc('usp_ConceptRank_Delete',
                                  ['@Key', ItemKey, '@Timestamp', Timestamp]);
end;  // TDomainRankList.DoDeletion 

{-------------------------------------------------------------------------------
}
procedure TDomainRankList.DoModification(AItem: TLuxCachedDataItem);
var
  lOrder: Variant;
begin
  with TDomainRankItem(AItem) do begin
    if Order <> ''  then lOrder := StrToInt(Order)
                    else lOrder := null;
    dmGeneral.RunUpdateStoredProc('usp_ConceptRank_Update',
                                  ['@Key', ItemKey,
                                  '@DomainKey', MasterKey,
                                  '@ItemName', RankName,
                                  '@SortOrder', lOrder,
                                  '@Abbreviation', Abbreviation,
                                  '@ColorR', (StrToInt(Color) shr 16) and $FF,
                                  '@ColorG', (StrToInt(Color) shr 8) and $FF,
                                  '@ColorB', StrToInt(Color) and $FF,
                                  '@Timestamp', Timestamp
                                 ]);
  end;
end;  // TDomainRankList.DoModification 

{-------------------------------------------------------------------------------
}
function TDomainRankList.GetRecordset: _Recordset;
begin
  Result := dmGeneral.GetRecordset('usp_ConceptRanks_Select_ForDomain',
                                   ['@Key', MasterKey]);
end;  // TDomainRankList.GetRecordset 


{-==============================================================================
    TDomainRankItem
===============================================================================}
{-------------------------------------------------------------------------------
}
procedure TDomainRankItem.GetData(const Column: Integer; var AText: String; var
        AKey: TKeyString);
begin
  AKey := '';
  AText := '';
  case Column of
    0: AText := FOrder;
    1: AText := FRankName;
    2: AText := FAbbreviation;
    3: AText := FColor;
  end;
end;  // TDomainRankItem.GetData 

{-------------------------------------------------------------------------------
}
function TDomainRankItem.GetDisplayText(Column: Integer): string;
begin
  if Column = 3 then Result := ''
                else Result := inherited GetDisplayText(Column);
end;  // TDomainRankItem.GetDisplayText 

{-------------------------------------------------------------------------------
}
procedure TDomainRankItem.InitFromRecord(AFields: Fields);
begin
  SetItemKey(VarToStr(AFields['Item_Key'].Value));
  FRankName       := VarToStr(AFields['Item_Name'].Value);
  FAbbreviation   := VarToStr(AFields['Abbreviation'].Value);
  FColor          := String(AFields['Color_R'].Value shl 16
                          + AFields['Color_G'].Value shl 8
                          + AFields['Color_B'].Value);
  FOrder          := VarToStr(AFields['Sort_Order'].Value);
  FTimestamp      := AFields['Timestamp'].Value;
end;  // TDomainRankItem.InitFromRecord 

{-------------------------------------------------------------------------------
}
procedure TDomainRankItem.SetData(const Column: Integer; const AText: String;
        const AKey: TKeyString);
begin
  case Column of
    0: if FOrder <> AText then begin
         FOrder := AText;
         SetModified;
       end;
    1: if FRankName <> AText then begin
         FRankName := AText;
         SetModified;
       end;
    2: if FAbbreviation <> AText then begin
         FAbbreviation := AText;
         SetModified;
       end;
    3: if FColor <> AText then begin
         FColor := AText;
         SetModified;
       end;
  end;
end;  // TDomainRankItem.SetData 

{-------------------------------------------------------------------------------
}
procedure TDomainRankItem.ValidateData;
begin
  ValidateValue(FRankName <> '', Format(ResStr_MissingData, [ResStr_RankName]));
  ValidateValue(FAbbreviation <> '',
                            Format(ResStr_MissingData, [ResStr_Abbreviation]));
  ValidateValue(FColor <> '', Format(ResStr_MissingData, [ResStr_Color]));
  if FOrder <> '' then begin
    ValidateValue(IsInt(FOrder) = True, Format(ResStr_MustBeInteger, [ResStr_Order]));
    ValidateValue(StrToInt(FOrder) > 0, Format(ResStr_MustBePositive, [ResStr_Order]));
  end;
end;  // TDomainRankItem.ValidateData 

end.


