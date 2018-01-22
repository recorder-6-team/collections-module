{===============================================================================
  Unit:        SelectLineage

  Defines:

  Description:

  Created:

  Model:       ThesaurusNavigator.mpb

  Last revision information:
    $Revision: 8 $
    $Date: 8/08/11 13:34 $
    $Author: Simonlewis $

===============================================================================}

unit SelectLineage;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Grids, StdCtrls, ImageListButton, InterfaceDataModule,
  GeneralFunctions, ExceptionForm, ADODb, GeneralData, ResourceStrings,
  ApplicationSettings, ConceptRankCache;

type
  ESelectLineageException = class (TExceptionPath)
  end;
  
  {-----------------------------------------------------------------------------
    Dialog displayed when the user selects to display the ancestor hierarchy (
    lineage) of a node in the thesaurus browser and the item has more than one 
    possible lineage.  This occurs when the concept has more than one parent, 
    or any ancestor of the concept has more than one parent.  The dialog is 
    also displayed if the chkIncludeSynonyms was last set to checked, and there 
    are other synonyms of the concept on other concept groups.
  }
  TdlgSelectLineage = class (TForm)
    btnCancel: TImageListButton;
    btnOk: TImageListButton;
    chkIncludeSynonyms: TCheckBox;
    Label1: TLabel;
    sgLineage: TStringGrid;
    procedure chkIncludeSynonymsClick(Sender: TObject);
    procedure sgLineageDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: 
        TRect; State: TGridDrawState);
    procedure sgLineageSelectCell(Sender: TObject; ACol, ARow: Integer; var 
        CanSelect: Boolean);
    procedure btnOkClick(Sender: TObject);
  private
    FColumnConnectorTrack: array of integer;
    FColumnLineageIds: array of integer;
    FConceptKey: string;
    FConceptRankCache: TConceptRankCache;
    FConstructionComplete: Boolean;
    FHierarchyRelationTypeKey: string;
    FLineageRecordsets: TInterfaceList;
    FSelectedCol: Integer;
    FSelectedItemRank: string;
    procedure AddBottomRow;
    function DoDrawText(const AText: string; AGrid: TStringGrid; ACol, ARow: 
        integer; ARect: TRect): Integer;
    procedure DoFillRect(AGrid: TStringGrid; ACol, ARow: integer; ARect: TRect);
    procedure DrawConnectorLine(Sender: TObject; ACol, ARow: Integer; Rect: 
        TRect; ATextOutput: boolean);
    function FindRankKeyForNextRow: string;
    function GetNextRowSortOrder(ARankKey: Variant): Integer;
    function GetOutputConceptKey: string;
    function GetOutputLineageID: Integer;
    procedure InitialiseColumnConnectorTrack(ALineageCount: integer);
    procedure PopulateColumns;
    procedure SetConceptKey(const Value: string);
    procedure SetHierarchyRelationTypeKey(const Value: string);
    procedure SetSelectedItemRank(const Value: string);
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
    procedure SetRecordset(ARecordset: _Recordset);
    property ConceptKey: string read FConceptKey write SetConceptKey;
    property HierarchyRelationTypeKey: string read FHierarchyRelationTypeKey 
        write SetHierarchyRelationTypeKey;
    property OutputConceptKey: string read GetOutputConceptKey;
    property OutputLineageID: Integer read GetOutputLineageID;
    property SelectedItemRank: string read FSelectedItemRank write 
        SetSelectedItemRank;
  end;
  
//==============================================================================
implementation

{$R *.dfm}

{-==============================================================================
    TdlgSelectLineage
===============================================================================}
{-------------------------------------------------------------------------------
  Constructor initialises objects.  This does not populate the grid. 
}
constructor TdlgSelectLineage.Create(AOwner : TComponent);
begin
  FConstructionComplete := False;
  inherited Create(AOwner);
  
  FSelectedCol := sgLineage.Col;
  chkIncludeSynonyms.Checked := AppSettings.IncludeHierarchySynonyms;
  FConceptRankCache := TConceptRankCache.Create;
  FLineageRecordsets := TInterfaceList.Create;
  FConstructionComplete := True;
end;  // TdlgSelectLineage.Create 

{-------------------------------------------------------------------------------
  Cleanup code 
}
destructor TdlgSelectLineage.Destroy;
begin
  FConceptRankCache.Free;
  FLineageRecordsets.Free;
  
  inherited Destroy;
end;  // TdlgSelectLineage.Destroy 

{-------------------------------------------------------------------------------
  Adds the leaf items to the bottom row of the grid. 
}
procedure TdlgSelectLineage.AddBottomRow;
var
  lIdx: Integer;
begin
  sgLineage.Rowcount := sgLineage.Rowcount + 1;
  for lIdx := 1 to sgLineage.ColCount-1 do begin
    sgLineage.Cells[lIdx, sgLineage.RowCount-1] :=
        dmGeneral.GetStoredProcOutputParam('usp_ConceptTermAndAuthor_Get',
        ['@ConceptKey', sgLineage.Cells[lIdx, 0]], '@ConceptTermAndAuthor');
  end;
  // Use the originally selected item's rank as the rank for this row.
  sgLineage.Cells[0, sgLineage.RowCount-1] := SelectedItemRank;
  // set top row as fixed
  sgLineage.FixedRows := Min(1, sgLineage.RowCount-1);
end;  // TdlgSelectLineage.AddBottomRow 

{-------------------------------------------------------------------------------
  Toggles the display of synonyms on the grid on and off. 
}
procedure TdlgSelectLineage.chkIncludeSynonymsClick(Sender: TObject);
begin
  if FConstructionComplete then
    // repopulate using the new checkbox setting
    SetRecordset(dmGeneral.GetRecordset('usp_ConceptLineage_Get', [
          '@ConceptKey', ConceptKey,
          '@IncludeSynonyms', chkIncludeSynonyms.Checked
          ]));
end;  // TdlgSelectLineage.chkIncludeSynonymsClick 

{-------------------------------------------------------------------------------
  Draws the text onto the grid.
  Return result is the height of the box used. 
}
function TdlgSelectLineage.DoDrawText(const AText: string; AGrid: TStringGrid; 
    ACol, ARow: integer; ARect: TRect): Integer;
var
  lX, lY, lIdx: Integer;
  lCurrentWord: string;
  lTokens: TStringList;
  lLineStartToken: Integer;
  lCurrentLineWidth: Integer;
  
  // Outputs a single word
  procedure OutputCurrentWord;
  begin
    with AGrid do begin
      // don't draw spaces, as they clip the previuos italic word overhang
      if lCurrentWord <> ' ' then
        Canvas.TextOut(lX, lY, lCurrentWord);
      lX := lX + Canvas.TextWidth(lCurrentWord);
      lCurrentWord := '';
    end;
  end;
  
  // Outputs the current line
  procedure OutputLine(lMaxTokenIdx: integer);
  begin
    // centre the line
    lX := ARect.Left + ((ARect.Right - ARect.Left) - lCurrentLineWidth) div 2;
    while lLineStartToken <= lMaxTokenIdx do begin
      lCurrentWord := lTokens[lLineStartToken];
      if lCurrentWord='<i>' then
        AGrid.Canvas.Font.Style := [fsItalic]
      else if lCurrentWord='</i>' then
        AGrid.Canvas.Font.Style := []
      else
        OutputCurrentWord;
      Inc(lLineStartToken);
    end;
    // Update the Y pos
    lY := lY + Canvas.TextHeight('A')+2;
    lCurrentLineWidth := 0;
  end;
  
  // break words up into a list of tokens
  procedure ParseTokens;
  begin
    while lIdx<=Length(AText) do begin
      if CompareText(Copy(AText, lIdx, 3),'<i>')=0 then begin
        if lCurrentWord>'' then
          lTokens.Add(lCurrentWord);
        lCurrentWord := '';
        lTokens.Add('<i>');
        Inc(lIdx, 2);
      end
      else if CompareText(Copy(AText, lIdx, 4),'</i>')=0 then begin
        lTokens.Add(lCurrentWord);
        lCurrentWord := '';
        lTokens.Add('</i>');
        lTokens.Add(' '); // to allow for italic overhang
        Inc(lIdx, 3);
      end
      else if AText[lIdx] in [#9,#32,#13] then begin
        lTokens.Add(lCurrentWord);
        lCurrentWord := '';
        lTokens.Add(' ');
      end
      else
        if AText[lIdx]<>#10 then
          lCurrentWord := lCurrentWord + AText[lIdx];
      Inc(lIdx);
    end;
    lTokens.Add(lCurrentWord);
  end;
  
begin
  lY := ARect.Top+2;
  lIdx := 1; // first character
  lCurrentWord := '';
  lTokens := TStringList.Create;
  try
    ParseTokens;
    lCurrentLineWidth := 0;
    lLineStartToken := 0;
    for lIdx := 0 to lTokens.Count-1 do begin
      if lTokens[lIdx]='<i>' then
        AGrid.Canvas.Font.Style := [fsItalic]
      else if lTokens[lIdx]='</i>' then
        AGrid.Canvas.Font.Style := []
      else begin
        if lCurrentLineWidth + AGrid.Canvas.TextWidth(lTokens[lIdx]) >
            ARect.Right-ARect.Left then
          OutputLine(lIdx-1);
        lCurrentLineWidth := lCurrentLineWidth + AGrid.Canvas.TextWidth(
            lTokens[lIdx]);
      end;
    end;
    OutputLine(lTokens.Count-1);
  finally
    lTokens.Free;
  end;
  Result := lY - ARect.Top+2;
end;  // TdlgSelectLineage.DoDrawText 

{-------------------------------------------------------------------------------
  Fills the current grid drawing cell in the correct colour.  Also ensures 
      canvas font colour is set correctly. 
}
procedure TdlgSelectLineage.DoFillRect(AGrid: TStringGrid; ACol, ARow: integer; 
    ARect: TRect);
begin
  with AGrid do begin
    if (Col=ACol) and (ARow>0) then
      Canvas.Brush.Color := MergeColours(clHighlight, clWindow, 25);
    Canvas.Fillrect(ARect);
  end; // with
end;  // TdlgSelectLineage.DoFillRect 

{-------------------------------------------------------------------------------
}
procedure TdlgSelectLineage.DrawConnectorLine(Sender: TObject; ACol, ARow: 
    Integer; Rect: TRect; ATextOutput: boolean);
begin
  // draw link between item and its ancestor
  if (ARow>=2) and (FColumnConnectorTrack[ACol-1]<ARow) then
    with (Sender as TStringGrid) do begin
      if Col=ACol then
        Canvas.Pen.Color := clHighlightText
      else
        Canvas.Pen.Color := clHighlight;
      Canvas.MoveTo((Rect.Left + Rect.Right) div 2, Rect.Top-4);
      if ATextOutput then
        Canvas.LineTo((Rect.Left + Rect.Right) div 2, Rect.Top+2)
      else
        Canvas.LineTo((Rect.Left + Rect.Right) div 2, Rect.Bottom);
    end;
end;  // TdlgSelectLineage.DrawConnectorLine 

{-------------------------------------------------------------------------------
  Look through each 
      column's recordsets, find the one with the lowest rank (using the current record positions and comparing sort orders) and return that rank's key 
}
function TdlgSelectLineage.FindRankKeyForNextRow: string;
var
  lColIdx: Integer;
  lSortOrder: integer;
begin
  // Starting point for our search for for the lowest value
  lSortOrder := High(Integer);
  for lColIdx := 0 to FLineageRecordsets.Count-1 do begin
    if not (FLineageRecordsets[lColIdx] as _Recordset).EOF then
      if GetNextRowSortOrder((FLineageRecordsets[lColIdx] as _Recordset).
          Fields['Concept_Rank_Key'].Value)<lSortOrder then begin
        lSortOrder := GetNextRowSortOrder((FLineageRecordsets[lColIdx] as _Recordset).
          Fields['Concept_Rank_Key'].Value);
        Result := VarToStr((FLineageRecordsets[lColIdx] as
            _Recordset).Fields['Concept_Rank_Key'].Value);
      end;
  end; // for
end;  // TdlgSelectLineage.FindRankKeyForNextRow 

{-------------------------------------------------------------------------------
  Returns a Sort order for the supplied rank, or -1 if the rank is null. 
}
function TdlgSelectLineage.GetNextRowSortOrder(ARankKey: Variant): Integer;
begin
  if VarIsNull(ARankKey) then
    Result := -1
  else
    Result := FConceptRankCache.GetRank(ARankKey).SortOrder;
end;  // TdlgSelectLineage.GetNextRowSortOrder 

{-------------------------------------------------------------------------------
  Accessor method.  Concept key of the lineage that was selected. 
}
function TdlgSelectLineage.GetOutputConceptKey: string;
begin
  // Column's concept key is stored in the header cell for the column
  Result := sgLineage.Cells[sgLineage.Col, 0];
end;  // TdlgSelectLineage.GetOutputConceptKey 

{-------------------------------------------------------------------------------
  Accessor method.  Gets lineage ID of the currently selected line in the 
      dialog. 
}
function TdlgSelectLineage.GetOutputLineageID: Integer;
begin
  if sgLineage.Col>0 then
    Result := FColumnLineageIds[sgLineage.Col-1]
  else
    Result := -1;
end;  // TdlgSelectLineage.GetOutputLineageID 

{-------------------------------------------------------------------------------
  Initialises the array that tracks the first cell to include an item for each 
      column. 
}
procedure TdlgSelectLineage.InitialiseColumnConnectorTrack(ALineageCount: 
    integer);
var
  lIdx: Integer;
begin
  SetLength(FColumnConnectorTrack, ALineageCount);
  // Fill values so that no lines are drawn
  for lIdx := 0 to High(FColumnConnectorTrack) do
    FColumnConnectorTrack[lIdx] := High(Integer);
end;  // TdlgSelectLineage.InitialiseColumnConnectorTrack 

{-------------------------------------------------------------------------------
  Populates the columns, using the already loaded list of recordsets (1 for 
      each column) 
}
procedure TdlgSelectLineage.PopulateColumns;
var
  lColIdx: Integer;
  lRankKey: Variant;
  lRowIdx: Integer;
  lAllEOF: Boolean;
begin
  lRowIdx := 1;
  lAllEOF := False;
  while not lAllEOF do begin
    sgLineage.RowCount := lRowIdx;
    // Find the rank key with the next lowest sort order
    lRankKey := FindRankKeyForNextRow;
    // Output the columns that match the found rank into this row
    lAllEOF := True; // default until we find otherwise
    // First cell in row contains the rank
    sgLineage.Cells[0, lRowIdx] := VarToStr(lRankKey);
    for lColIdx := 0 to FLineageRecordsets.Count-1 do begin
      if not (FLineageRecordsets[lColIdx] as _Recordset).EOF then begin
        lAllEOF := False;
        if VarToStr((FLineageRecordsets[lColIdx] as _Recordset).
            Fields['Concept_Rank_Key'].Value) = lRankKey then begin
          sgLineage.Cells[lColIdx+1, lRowIdx] := (FLineageRecordsets[lColIdx] as
              _Recordset).Fields['Item_Name'].Value;
          (FLineageRecordsets[lColIdx] as _Recordset).MoveNext;
          // record if this item is the top of the line
          if FColumnConnectorTrack[lColIdx]=High(Integer) then
            FColumnConnectorTrack[lColIdx] := lRowIdx;
        end else
          sgLineage.Cells[lColIdx+1, lRowIdx] := '';
      end;
    end;
    Inc(lRowIdx);
  end; // while
  AddBottomRow;
end;  // TdlgSelectLineage.PopulateColumns 

{-------------------------------------------------------------------------------
  Accessor method.  Concept key that lineages are shown for.  Value is only 
      used if the user toggles the Include Synonyms option as a refetch of the 
      list of lineages is required. 
}
procedure TdlgSelectLineage.SetConceptKey(const Value: string);
begin
  FConceptKey := Value;
end;  // TdlgSelectLineage.SetConceptKey 

{-------------------------------------------------------------------------------
  Accessor method.  Relationship type that defines the hierarchical 
      parent/child relationship. 
}
procedure TdlgSelectLineage.SetHierarchyRelationTypeKey(const Value: string);
begin
  FHierarchyRelationTypeKey := Value;
end;  // TdlgSelectLineage.SetHierarchyRelationTypeKey 

{-------------------------------------------------------------------------------
  Recordset the dialog is initially populated from.  Since the list of lineages 
      will have already been obtained to test whether the dialog is required, 
      this allows the avoidance of a duplication of the query already run. 
}
procedure TdlgSelectLineage.SetRecordset(ARecordset: _Recordset);
var
  lColIdx: Integer;
  lCursor: TCursor;
begin
  if HierarchyRelationTypeKey='' then
    raise ESelectLineageException.Create(Format(ResStr_InvalidMethodCall,
        ['TdlgSelectLineage.SetRecordset']));
  lCursor := HourglassCursor;
  try
    with ARecordset do begin
      FLineageRecordsets.Clear;
      sgLineage.ColCount := RecordCount+1; // + 1 for rank column
      SetLength(FColumnLineageIds, RecordCount);
      InitialiseColumnConnectorTrack(RecordCount);
      MoveFirst;
      lColIdx := 1;
      while not EOF do begin
        FLineageRecordsets.Add(dmGeneral.GetRecordset(
            'usp_ConceptAncestors_Select', [
            '@ConceptKey', Fields['Concept_Key'].Value,
            '@LineageID', Fields['Lineage_ID'].Value,
            '@HierarchyRelationTypeKey', HierarchyRelationTypeKey]));
        // Store the concept key on each column heading
        sgLineage.Cells[lColIdx, 0] := Fields['Concept_Key'].Value;
        // and record the column lineage ID
        FColumnLineageIds[lColIdx-1] := Fields['Lineage_ID'].Value;
        Inc(lColIdx);
        MoveNext;
      end;
    end; // with
    PopulateColumns;
  finally
    DefaultCursor(lCursor);
  end; // try*)
end;  // TdlgSelectLineage.SetRecordset 

{-------------------------------------------------------------------------------
  Accessor method.  Rank of the originally selected item in the tree view. 
}
procedure TdlgSelectLineage.SetSelectedItemRank(const Value: string);
begin
  FSelectedItemRank := Value;
end;  // TdlgSelectLineage.SetSelectedItemRank 

{-------------------------------------------------------------------------------
  Draws the cell.  Enables multiline text if required.  Also draws the taxon 
      rank into the first column. 
}
procedure TdlgSelectLineage.sgLineageDrawCell(Sender: TObject; ACol, ARow: 
    Integer; Rect: TRect; State: TGridDrawState);
var
  lstOutputText: string;
  liTextHeight: Integer;
begin
  DoFillRect(Sender as TStringGrid, ACol, ARow, Rect);
  (Sender As TStringgrid).Canvas.Font.Style := [];
  if (ACol = 0) and (ARow > 0) then begin
    // Draw the rank image
    if (Sender As TStringgrid).Cells[ACol, ARow]<>'' then
      dmInterface.DrawRank((Sender As TStringgrid).Canvas,
          FConceptRankCache.GetRank((Sender As TStringgrid).Cells[ACol, ARow]),
          Rect, taCenter);
  end
  else if (ARow=0) and (ACol>0) then
    // Draw column titles
    DoDrawText(ResStr_Lineage + ' ' + IntToStr(ACol), Sender as TStringGrid,
        ACol, ARow, Rect)
  else begin
    lstOutputText := sgLineage.Cells[ACol,ARow];
    If Length(lstOutputText) > 0 then begin
      // actually draw the text
      liTextHeight := DoDrawText(lstOutputText, Sender as TStringGrid, ACol,
          ARow, Rect);
      if liTextHeight > (Sender As TStringgrid).RowHeights[ARow] then
        // adjust rowheight if required - if so the grid gets invalidated and will redraw
        (Sender As TStringgrid).RowHeights[ARow] := liTextHeight;
    end;
    DrawConnectorLine(Sender, ACol, ARow, Rect, lstOutputText<>'');
  end;
end;  // TdlgSelectLineage.sgLineageDrawCell 

{-------------------------------------------------------------------------------
  Selecting a cell selects the entire column. 
}
procedure TdlgSelectLineage.sgLineageSelectCell(Sender: TObject; ACol, ARow: 
    Integer; var CanSelect: Boolean);
begin
  if ACol<>FSelectedCol then begin
    FSelectedCol := ACol;
    sgLineage.Invalidate;
  end;
end;  // TdlgSelectLineage.sgLineageSelectCell 



procedure TdlgSelectLineage.btnOkClick(Sender: TObject);
begin
  // persist the IncludeSysnonyms checkbox state
  AppSettings.IncludeHierarchySynonyms := chkIncludeSynonyms.Checked;
end;

end.



































