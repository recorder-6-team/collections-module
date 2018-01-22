{===============================================================================
  Unit:        ConceptGroupQualityChecker

  Defines:     TfrmConceptGroupQualityChecker

  Description: MDI Child that allows concepts to be merged or de-duplicated.

  Created:     20/12/2006

  Last revision information:
    $Revision: 13 $
    $Date: 9/06/11 14:32 $
    $Author: Jamesbichard $

===============================================================================}

unit ConceptGroupQualityChecker;

interface

uses
  Windows, Messages, SysUtils, Forms, DomainConceptGroupSelector, Classes, Controls, ExtCtrls,
  StdCtrls, Graphics, Grids, InterfaceDataModule, ImageListButton, DssStringGrid,
  BaseMDIChildUnit, LuxembourgDataClasses, DataClasses, ADOInt, Dialogs,
  ComboListID, LuxIDComboBox, StrUtils, Variants, DataTypes, ConfirmHistoryUpdate,
  ListOrganiser, ConceptGroupGeneral, FilteredStringGrid, Menus, ImgList;

const
  IMG_UNSELECT_ALL = 22;
  IMG_SELECT_ALL   = 23;

type
  {-----------------------------------------------------------------------------
  }
  TConceptItem = class(TLuxGridDataItem)
  private
    FResolution: Integer;
    FSynonymGroup: String;
    FSynonymAuthority: String;
    FSharedTerm: String;
    FSourceConcept: String;
    FSourceGroup: String;
    FSourceAuthority: String;
    FSynonymConcept: String;
    FSynonymConceptKey: String;
    FSourceMeaningKey: String;
    FSynonymMeaningKey: String;
    FInPreferredGroup: Boolean;
    procedure SetResolution(Value: Integer);
  protected
    procedure GetData(const Column: Integer; var AText: String; var AKey: TKeyString);
        override;
    procedure InitFromRecord(AFields: Fields); override;
  public
    property SourceConcept: String read FSourceConcept;
    property SourceGroup: String read FSourceGroup;
    property SourceAuthority: String read FSourceAuthority;
    property SynonymConcept: String read FSynonymConcept;
    property SynonymConceptKey: String read FSynonymConceptKey;
    property SynonymGroup: String read FSynonymGroup;
    property SynonymAuthority: String read FSynonymAuthority;
    property SharedTerm: String read FSharedTerm;
    property Resolution: Integer read FResolution write SetResolution;
    property SourceMeaningKey: String read FSourceMeaningKey;
    property SynonymMeaningKey: String read FSynonymMeaningKey;
    property InPreferredGroup: Boolean read FInPreferredGroup;
  end;

  {-----------------------------------------------------------------------------
  }
  TConceptList = class(TLuxGridDataList)
  private
    FConceptGroupKey: String;
    FConceptGroupSearchKey: String;
    FPreferredSynonymGroup: String;
    FMaxRecords: Integer;
    FTimestamp: TSQLSvrTimestamp;
    FSessionId: String;
    FOpenFromImport: Boolean;
  protected
    function AllowedAddOnKeyDown: Boolean; override;
    function GetRecordset: _Recordset; override;
  public
    property ConceptGroupKey: String read FConceptGroupKey write FConceptGroupKey;
    property ConceptGroupSearchKey: String
      read FConceptGroupSearchKey write FConceptGroupSearchKey;
    property PreferredSynonymGroup: String
      read FPreferredSynonymGroup write FPreferredSynonymGroup;
    property MaxRecords: Integer read FMaxRecords write FMaxRecords;
    property Timestamp: TSQLSvrTimestamp read FTimestamp write FTimestamp;
    property SessionId: String read FSessionId write FSessionId;
    property OpenFromImport: Boolean read FOpenFromImport write FOpenFromImport;
  end;

  {-----------------------------------------------------------------------------
  }
  TTermItem = class(TLuxGridDataItem)
  private
    FTerm: String;
    FTermKey2: String;
    FConcept1: String;
    FConceptKey1: String;
    FGroup1: String;
    FAuthority1: String;
    FConcept2: String;
    FConceptKey2: String;
    FGroup2: String;
    FAuthority2: String;
    FResolve: Boolean;
  protected
    procedure GetData(const Column: Integer; var AText: String; var AKey: TKeyString); override;
    procedure InitFromRecord(AFields: Fields); override;
  public
    property Term: String read FTerm;
    property TermKey2: String read FTermKey2;
    property Concept1: String read FConcept1;
    property ConceptKey1: String read FConceptKey1;
    property Group1: String read FGroup1;
    property Authority1: String read FAuthority1;
    property Concept2: String read FConcept2;
    property ConceptKey2: String read FConceptKey2;
    property Group2: String read FGroup2;
    property Authority2: String read FAuthority2;
    property Resolve: Boolean read FResolve write FResolve;
  end;

  {-----------------------------------------------------------------------------
  }
  TTermList = class(TLuxGridDataList)
    FConceptGroupKey: String;
    FMaxRecords: Integer;
  protected
    function AllowedAddOnKeyDown: Boolean; override;
    function CanAddItemToList(AItem: TLuxCachedDataItem): Boolean; override;
    function GetRecordset: _Recordset; override;
  public
    property ConceptGroupKey: String read FConceptGroupKey write FConceptGroupKey;
    property MaxRecords: Integer read FMaxRecords write FMaxRecords;
  end;

  {-----------------------------------------------------------------------------
  }
  TfrmConceptGroupQualityChecker = class(TBaseMDIChild)
    sgScanResults: TFilteredStringGrid;
    pnlBetween: TPanel;
    chkResolveDuplicates: TCheckBox;
    sgSelectedItem: TDSSStringGrid;
    pnlButtons: TPanel;
    btnProceed: TImageListButton;
    Splitter: TSplitter;
    pnlBottom: TPanel;
    btnSelection: TImageListButton;
    cmbResolution: TComboBox;
    btnResolutionFilter: TPopupButton;
    pmResolutionFilter: TPopupMenu;
    mnuMakeSynonym: TMenuItem;
    mnuMakeHomonym: TMenuItem;
    mnuToBeProcessed: TMenuItem;
    procedure sgScanResultsDrawCell(Sender: TObject; ACol, ARow: Integer;
      ARect: TRect; State: TGridDrawState);
    procedure sgScanResultsClick(Sender: TObject);
    procedure sgSelectedItemDrawCell(Sender: TObject; ACol, ARow: Integer;
      ARect: TRect; State: TGridDrawState);
    procedure btnProceedClick(Sender: TObject);
    procedure SplitterMoved(Sender: TObject);
    procedure btnSelectionClick(Sender: TObject);
    procedure cmbResolutionExit(Sender: TObject);
    procedure cmbResolutionClick(Sender: TObject);
    function RepeatedMeaningPair(
        MeaningKey1: TKeyString;
        MeaningKey2: TKeyString): Boolean;
    function PromptForLastCheckUpdate: Boolean;
    procedure FilterMenuClick(Sender: TObject);
    procedure sgScanResultsDblClick(Sender: TObject);
    procedure sgScanResultsKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    FForSynonyms: Boolean;  // Indicates processing Potential Synonyms or Duplicate Terms.
    FConceptList: TConceptList;
    FTermList: TTermList;
    procedure SetupForDuplicates(const AConceptGroupKey: String; AMaxRecords: Integer);
    procedure SetupForSynonyms(
      const AConceptGroupKey: String;
      const AConceptGroupSearchKey: String;
      const APreferredSynonymGroup: String;
      AMaxRecords: Integer;
      AOpenFromImport: Boolean;
      ATimestamp: TSQLSvrTimestamp;
      ASessionId: String);
    procedure UpdateDetails(ARow: Integer);
    procedure PopulateResolutionList;
    procedure UpdateResolution(
      AResolution: Integer;
      AConceptItem: TConceptItem;
      APrompt: Boolean);
    procedure SetScanResultsReadOnly;
    procedure ToggleSelectedItem(ARow: Integer);
  protected
    procedure UpdateControlsAlignment; override;
  public
    constructor Create(
        AOwner: TComponent;
        const AForSynonyms: Boolean;
        const AConceptGroupKey: String;
        const AConceptGroupSearchKey: String;
        const APreferredSynonymGroup: String;
        AMaxRecords: Integer;
        ATimestamp: TSQLSvrTimestamp;
        AOpenFromImport: Boolean = false;
        ASessionId: String = ''); reintroduce; overload;
    destructor Destroy; override;
  end;

  

//==============================================================================
implementation

uses
  ThesaurusEditorMain, ResourceStrings, GeneralFunctions, GeneralData,
  Contnrs, ApplicationSettings;

{$R *.dfm}

const
  CELL_UNCHECKED = '0';
  CELL_CHECKED   = '1';

  RSN_TO_BE_PROCESSED = 0;
  RSN_MAKE_SYNONYM = 1;
  RSN_NOT_A_SYNONYM = 2;

  CHECKED_VALUES: Array[Boolean] of String = (CELL_UNCHECKED, CELL_CHECKED);

  COL_SOURCE_CONCEPT = 0;
  COL_SOURCE_AUTHORITY = 1;
  COL_SOURCE_GROUP = 2;
  COL_SYNONYM_CONCEPT = 3;
  COL_SYNONYM_AUTHORITY = 4;
  COL_SYNONYM_GROUP = 5;
  COL_SHARED_TERM = 6;
  COL_RESOLUTION = 7;

  AUTHORITY_COLUMN_WIDTH = 100;
  CONCEPT_GROUP_COLUMN_WIDTH = 100;
  TERM_COLUMN_WIDTH = 120;
  CHECKBOX_COLUMN_WIDTH = 80;
  COMBOBOX_COLUMN_WIDTH = 100;

{-==============================================================================
    TConceptItem
===============================================================================}

procedure TConceptItem.SetResolution(Value: Integer);
begin
  FResolution := Value;
  SetModified;
end;

{===============================================================================
-------------------------------------------------------------------------------}
procedure TConceptItem.GetData(const Column: Integer; var AText: String;
    var AKey: TKeyString);
begin
  inherited;

  case Column of
    COL_SOURCE_CONCEPT    : AText := SourceConcept;
    COL_SOURCE_AUTHORITY  : AText := SourceAuthority;
    COL_SOURCE_GROUP      : AText := SourceGroup;
    COL_SYNONYM_CONCEPT   : AText := SynonymConcept;
    COL_SYNONYM_AUTHORITY : AText := SynonymAuthority;
    COL_SYNONYM_GROUP     : AText := SynonymGroup;
    COL_SHARED_TERM       : AText := SharedTerm;
    COL_RESOLUTION        :
      begin
        case Resolution of
          0: AText := '';
          1: AText := ResStr_MakeSynonyms;
          2: AText := ResStr_NotASynonym;
        end;
        AKey := VarToStr(Resolution);
      end;
  end;
end;

{===============================================================================
Procedure Name: InitFromRecord
       Purpose: Initialises a ConceptItem object from a record.
-------------------------------------------------------------------------------}
procedure TConceptItem.InitFromRecord(AFields: Fields);
begin
  SetItemKey(AFields['SourceConceptKey'].Value);
  FSynonymConceptKey := AFields['SynonymConceptKey'].Value;

  FSourceConcept := AFields['SourceConcept'].Value;
  FSourceAuthority := AFields['SourceAuthority'].Value;
  FSourceGroup := AFields['SourceGroup'].Value;
  FSourceMeaningKey := AFields['SourceMeaningKey'].Value;

  FSynonymConcept := AFields['SynonymConcept'].Value;
  FSynonymAuthority := AFields['SynonymAuthority'].Value;
  FSynonymGroup := AFields['SynonymGroup'].Value;
  FSynonymMeaningKey := AFields['SynonymMeaningKey'].Value;

  FSharedTerm := AFields['SharedTerm'].Value;
  FInPreferredGroup := AFields['InPreferredGroup'].Value;

  FResolution := 0;
end;

{-==============================================================================
    TConceptList
===============================================================================}

{-------------------------------------------------------------------------------
}
function TConceptList.AllowedAddOnKeyDown: Boolean;
begin
  result := False;
end;

{-------------------------------------------------------------------------------
}
function TConceptList.GetRecordset: _Recordset;
begin
  result := dmGeneral.GetRecordset(
                'usp_PotentialSynonyms_Select_ForMerge',
                ['@ConceptGroupKey', ConceptGroupKey,
                 '@ConceptGroupSearchKey', ConceptGroupSearchKey,
                 '@PreferredSynonymGroup', PreferredSynonymGroup,
                 '@MaxRowCount', MaxRecords,
                 '@Timestamp', Timestamp,
                 '@SessionId', SessionId]);
end;

{-==============================================================================
    TTermItem
===============================================================================}

{===============================================================================
-------------------------------------------------------------------------------}
procedure TTermItem.GetData(const Column: Integer; var AText: String;
    var AKey: TKeyString);
begin
  inherited;

  case Column of
    0: AText := Term;
    1: AText := Concept1;
    2: AText := Authority1;
    3: AText := Group1;

    4: AText := Concept2;
    5: AText := Authority2;
    6: AText := Group2;

    7: AText := CHECKED_VALUES[Resolve];
  end;
end;

{===============================================================================
Procedure Name: InitFromRecord
       Purpose: Initialises a term item object from its database record.
-------------------------------------------------------------------------------}
procedure TTermItem.InitFromRecord(AFields: Fields);
begin
  SetItemKey(AFields['TermKey1'].Value);

  FTerm        := AFields['Term'].Value;
  FConcept1    := AFields['Preferred1'].Value;
  FAuthority1  := AFields['Authority1'].Value;
  FGroup1      := AFields['Group1'].Value;

  FConcept2    := AFields['Preferred2'].Value;
  FAuthority2  := AFields['Authority2'].Value;
  FGroup2      := AFields['Group2'].Value;

  FResolve     := True;

  FTermKey2    := AFields['TermKey2'].Value;
  FConceptKey1 := AFields['PreferredKey1'].Value;
  FConceptKey2 := AFields['PreferredKey2'].Value;
end;

{-==============================================================================
    TTermList
===============================================================================}

{-------------------------------------------------------------------------------
}
function TTermList.AllowedAddOnKeyDown: Boolean;
begin
  result := False;
end;

{-------------------------------------------------------------------------------
}
function TTermList.CanAddItemToList(AItem: TLuxCachedDataItem): Boolean;
var
  i: Integer;
  term1: TTermItem;
  term2: TTermItem;
begin
  result := True;
  for i := 0 to Count - 1 do begin
    term1 := TTermItem(Items[i]);
    term2 := TTermItem(AItem);
    if ((term1.ItemKey = term2.ItemKey)
        and (term1.TermKey2 = term2.TermKey2)
        and (term1.ConceptKey1 = term2.ConceptKey2)
        and (term1.ConceptKey2 = term2.ConceptKey1))
    or ((term1.ItemKey = term2.TermKey2)
        and (term1.TermKey2 = term2.ItemKey)
        and (term1.ConceptKey1 = term2.ConceptKey1)
        and (term1.ConceptKey2 = term2.ConceptKey2))
    or ((term1.ItemKey = term2.TermKey2)
        and (term1.TermKey2 = term2.ItemKey)
        and (term1.ConceptKey1 = term2.ConceptKey2)
        and (term1.ConceptKey2 = term2.ConceptKey1)) then
    begin
      result := False;
      Break;
    end;
  end;
end;

{-------------------------------------------------------------------------------
}
function TTermList.GetRecordset: _Recordset;
begin
  result := dmGeneral.GetRecordset(
      'usp_DuplicateTerms_Select_ForMerge',
      ['@ConceptGroupKey', ConceptGroupKey,
      '@MaxRowCount', MaxRecords]);
end;

{-==============================================================================
    TfrmConceptGroupQualityChecker
===============================================================================}

{-------------------------------------------------------------------------------
}
constructor TfrmConceptGroupQualityChecker.Create(
    AOwner: TComponent;
    const AForSynonyms: Boolean;
    const AConceptGroupKey: String;
    const AConceptGroupSearchKey: String;
    const APreferredSynonymGroup: String;
    AMaxRecords: Integer; 
    ATimestamp: TSQLSvrTimestamp;
    AOpenFromImport: Boolean = false;
    ASessionId: String = '');
var
  lCursor: TCursor;
  i: Integer;
begin
  inherited Create(AOwner);

  Self.Width := 900;

  FForSynonyms := AForSynonyms;
  if FForSynonyms then
    SetupForSynonyms(
      AConceptGroupKey,
      AConceptGroupSearchKey,
      APreferredSynonymGroup,
      AMaxRecords,
      AOpenFromImport,
      ATimestamp,
      ASessionId)
  else
    SetupForDuplicates(AConceptGroupKey, AMaxRecords);

  // Initialise sgSelectedItem, the grid that shows the diagrammatic view.

  sgSelectedItem.Cells[1, 0] := ResStr_Term;
  sgSelectedItem.Cells[2, 0] := ResStr_Authority;
  sgSelectedItem.Cells[3, 0] := ResStr_ConceptGroup;

  sgSelectedItem.Cells[5, 0] := ResStr_Term;
  sgSelectedItem.Cells[6, 0] := ResStr_Authority;
  sgSelectedItem.Cells[7, 0] := ResStr_ConceptGroup;

  // Set up the fixed column.
  sgSelectedItem.Cells[0, 1] := ResStr_PreferredTerm;
  sgSelectedItem.Cells[0, 2] := ResStr_Synonyms;

  sgSelectedItem.ColWidths[2] := AUTHORITY_COLUMN_WIDTH;
  sgSelectedItem.ColWidths[6] := AUTHORITY_COLUMN_WIDTH;

  sgSelectedItem.ColWidths[3] := CONCEPT_GROUP_COLUMN_WIDTH;
  sgSelectedItem.ColWidths[7] := CONCEPT_GROUP_COLUMN_WIDTH;

  sgSelectedItem.ColWidths[1] := TERM_COLUMN_WIDTH;
  sgSelectedItem.ColWidths[5] := TERM_COLUMN_WIDTH;

  // Using this width currently makes the two grids the same total width.
  sgSelectedItem.ColWidths[4] := 80;

  lCursor := HourglassCursor;
  try
    if FForSynonyms then
    begin
      FConceptList.Refresh;
      SetScanResultsReadOnly;
      //Ensure that all resolutions for any synonyms in the preferred concept
      //group (from spreadsheet import), as well as any equivalent concept-synonym
      //pairs, are marked as 'Make Synonym'.
      for i := 0 to FConceptList.Count - 1 do
      begin
        if (TConceptItem(FConceptList.GetItem(i)).Resolution <> RSN_MAKE_SYNONYM)
          and TConceptItem(FConceptList.GetItem(i)).InPreferredGroup then
          UpdateResolution(
            RSN_MAKE_SYNONYM,
            TConceptItem(FConceptList.GetItem(i)),
            false);
      end;
    end
    else
      FTermList.Refresh;
    sgScanResults.Row := 1;
    UpdateDetails(1);
  finally
    DefaultCursor(lCursor);
  end;
  btnSelection.Caption := ResStr_UnselectAll;
end;

{===============================================================================
Procedure Name: SetupForSynonyms
       Purpose: Sets up sgScanResults for "Potential Synonyms" mode.
-------------------------------------------------------------------------------}
procedure TfrmConceptGroupQualityChecker.SetupForSynonyms(
    const AConceptGroupKey: String;
    const AConceptGroupSearchKey: String;
    const APreferredSynonymGroup: String;
    AMaxRecords: Integer;
    AOpenFromImport: Boolean;
    ATimestamp: TSQLSvrTimestamp;
    ASessionId: String);
begin

  with sgScanResults do
  begin
    Cells[COL_SOURCE_CONCEPT, 0] := ResStr_Concept;
    Cells[COL_SOURCE_AUTHORITY, 0] := ResStr_Authority;
    Cells[COL_SOURCE_GROUP, 0] := ResStr_ConceptGroup;
    Cells[COL_SYNONYM_CONCEPT, 0] := ResStr_PotentialSynonym;
    Cells[COL_SYNONYM_AUTHORITY, 0] := ResStr_Authority;
    Cells[COL_SYNONYM_GROUP, 0] := ResStr_ConceptGroup;
    Cells[COL_SHARED_TERM, 0] := ResStr_SharedTerm;
    Cells[COL_RESOLUTION, 0] := ResStr_Resolution;

    ColWidths[COL_SOURCE_AUTHORITY] := AUTHORITY_COLUMN_WIDTH;
    ColWidths[COL_SYNONYM_AUTHORITY] := AUTHORITY_COLUMN_WIDTH;

    ColWidths[COL_SOURCE_GROUP] := CONCEPT_GROUP_COLUMN_WIDTH;
    ColWidths[COL_SYNONYM_GROUP] := CONCEPT_GROUP_COLUMN_WIDTH;

    ColWidths[COL_SOURCE_CONCEPT] := TERM_COLUMN_WIDTH;
    ColWidths[COL_SYNONYM_CONCEPT] := TERM_COLUMN_WIDTH;
    ColWidths[COL_SHARED_TERM] := TERM_COLUMN_WIDTH;

    ColWidths[7] := COMBOBOX_COLUMN_WIDTH;

    ColumnsInfo[COL_SOURCE_CONCEPT].ReadOnly := True;
    ColumnsInfo[COL_SOURCE_AUTHORITY].ReadOnly := True;
    ColumnsInfo[COL_SOURCE_GROUP].ReadOnly := True;
    ColumnsInfo[COL_SYNONYM_CONCEPT].ReadOnly := True;
    ColumnsInfo[COL_SYNONYM_AUTHORITY].ReadOnly := True;
    ColumnsInfo[COL_SYNONYM_GROUP].ReadOnly := True;
    ColumnsInfo[COL_SHARED_TERM].ReadOnly := True;

    ColumnsInfo[COL_RESOLUTION].WinControl := cmbResolution;
  end;
  chkResolveDuplicates.Visible := True;
  Caption := Caption + ' - ' + ResStr_PotentialSynonyms;

  btnSelection.Enabled := False;
  btnResolutionFilter.Visible := True;
  btnResolutionFilter.Init(7, pmResolutionFilter);
  sgScanResults.AddFilter(btnResolutionFilter);

  mnuMakeSynonym.Checked := True;
  mnuMakeHomonym.Checked := True;
  mnuToBeProcessed.Checked := True;

  FConceptList := TConceptList.Create(TConceptItem, sgScanResults);

  FConceptList.ConceptGroupKey := AConceptGroupKey;
  FConceptList.ConceptGroupSearchKey := AConceptGroupSearchKey;
  FConceptList.PreferredSynonymGroup := APreferredSynonymGroup;
  FConceptList.Timestamp := ATimestamp;
  FConceptList.SessionId := ASessionId;
  FConceptList.MaxRecords := AMaxRecords;
  FConceptList.OpenFromImport := AOpenFromImport;

  PopulateResolutionList;
end;

{===============================================================================
Procedure Name: SetupForDuplicates
       Purpose: Sets up sgScanResults for "Duplicate Terms" mode.
-------------------------------------------------------------------------------}
procedure TfrmConceptGroupQualityChecker.SetupForDuplicates(const AConceptGroupKey: String;
    AMaxRecords: Integer);
begin
  with sgScanResults do
  begin
    ReadOnly := True;

    Cells[0, 0] := ResStr_Term;
    Cells[1, 0] := ResStr_UsedByConcept;
    Cells[2, 0] := ResStr_Authority;
    Cells[3, 0] := ResStr_ConceptGroup;
    Cells[4, 0] := ResStr_DuplicatedByConcept;
    Cells[5, 0] := ResStr_Authority;
    Cells[6, 0] := ResStr_ConceptGroup;
    Cells[7, 0] := ResStr_ResolveTermsToSingleRecord;

    ColWidths[2] := AUTHORITY_COLUMN_WIDTH;
    ColWidths[5] := AUTHORITY_COLUMN_WIDTH;

    ColWidths[3] := CONCEPT_GROUP_COLUMN_WIDTH;
    ColWidths[6] := CONCEPT_GROUP_COLUMN_WIDTH;

    ColWidths[0] := TERM_COLUMN_WIDTH;
    ColWidths[1] := TERM_COLUMN_WIDTH;
    ColWidths[4] := TERM_COLUMN_WIDTH;

    ColWidths[7] := CHECKBOX_COLUMN_WIDTH;
  end;
  chkResolveDuplicates.Visible := False;
  pnlBetween.Height := 8;
  Caption := Caption + ' - ' + ResStr_DuplicateTerms;

  FTermList := TTermList.Create(TTermItem, sgScanResults);
  FTermList.ConceptGroupKey := AConceptGroupKey;
  FTermList.MaxRecords := AMaxRecords;
end;

{-------------------------------------------------------------------------------
}
destructor TfrmConceptGroupQualityChecker.Destroy();
begin
  frmThesaurusEditor.SetMDIButtonVisible(False);
  if Owner is TfrmListOrganiser then
    TfrmListOrganiser(Owner).fraListOrganiserTree.Selected
      := TfrmListOrganiser(Owner).fraListOrganiserTree.Selected;
  inherited;
end;

{-------------------------------------------------------------------------------
}
procedure TfrmConceptGroupQualityChecker.UpdateControlsAlignment;
begin
  btnSelection.Left := (pnlButtons.Width - btnProceed.Width - btnSelection.Width - 16) div 2;
  btnProceed.Left   := btnSelection.Left + btnSelection.Width + 16;
end;

{-------------------------------------------------------------------------------
}
procedure TfrmConceptGroupQualityChecker.sgScanResultsDrawCell(Sender: TObject; ACol,
  ARow: Integer; ARect: TRect; State: TGridDrawState);
var
  checked: Boolean;
begin
  with sgScanResults do begin
    if ARow = 0 then begin
      with Canvas do begin
        Font.Color  := clBtnText;
        Pen.Color   := clWindowText;
        Brush.Color := clBtnFace;
        InflateRect(ARect, 1, 1);
        Rectangle(ARect);
        DrawChoppedText(Cells[ACol, ARow], Canvas, ARect, 4);
        Brush.Color := clWindowText;
      end;
    end else
    if (ACol = 7) and (ARow > 0) and Assigned(Objects[0, ARow]) then begin
      if not Enabled then
        Canvas.Brush.Color := clBtnFace;

      Canvas.FillRect(ARect);
      if not FForSynonyms then
      begin
        checked := TTermItem(Objects[0, ARow]).Resolve;
        DrawCheckBox(
          Canvas,
          ARect.Left + (ColWidths[ACol] - 13) div 2,
          ARect.Top + (RowHeights[ARow] - 13) div 2,
          checked);
      end else
      begin
        DrawChoppedText(Cells[ACol, ARow], Canvas, ARect, 2);
        if  not (csDesigning in ComponentState)
          and (State * [gdSelected, gdFocused] <> []) // in State) and Focused
          and not (goRowSelect in Options)
          and (ValidParentForm(Self).ActiveControl = Self) then
        Windows.DrawFocusRect(Canvas.Handle, ARect);
      end;
    end else begin
      Canvas.FillRect(ARect);
      dmInterface.DrawTerm(
          Canvas,
          Rect(ARect.Left + 3, ARect.Top + 2, ARect.Right, ARect.Bottom),
          Cells[ACol, ARow],
          False);
    end;

    if (FForSynonyms and (ACol in [2, 5, 6]))
        or ((not FForSynonyms) and (ACol in [0, 3, 6])) then
    begin
      Canvas.Pen.Color := clWindowText;
      Canvas.MoveTo(ARect.Right, ARect.Top - 1);
      Canvas.LineTo(ARect.Right, ARect.Bottom + 1);
    end;     
  end;
end;

{-------------------------------------------------------------------------------
}
procedure TfrmConceptGroupQualityChecker.sgScanResultsClick(Sender: TObject);
var
  col, row: Integer;
begin
  col := sgScanResults.Col;
  row := sgScanResults.Row;

  if (col = COL_RESOLUTION) then
  begin
    if FForSynonyms then
      cmbResolution.ItemIndex := TConceptItem(sgScanResults.Objects[0, row]).Resolution
    else
      ToggleSelectedItem(row);
  end;
  UpdateDetails(row);
end;

{-------------------------------------------------------------------------------
}
procedure TfrmConceptGroupQualityChecker.ToggleSelectedItem(ARow: Integer);
var
  item: TObject;
begin
  with sgScanResults do begin
    item := Objects[0, ARow];
    if (ARow > 0) and Assigned(item) then
      TTermItem(item).Resolve := not TTermItem(item).Resolve;
    Refresh;
  end;
end;

{-------------------------------------------------------------------------------
}
procedure TfrmConceptGroupQualityChecker.UpdateDetails(ARow: Integer);
var
  rs: _Recordset;
  i: Integer;
  key1: String;
  key2: String;
begin
  if (ARow < 1) or not Assigned(sgScanResults.Objects[0, ARow]) then
    Exit;

  // Setup grid.
  if FForSynonyms then
  begin
    with TConceptItem(sgScanResults.Objects[0, ARow]) do
    begin
      key1 := ItemKey;
      key2 := SynonymConceptKey;

      sgSelectedItem.Cells[1, 1] := SourceConcept;
      sgSelectedItem.Cells[2, 1] := SourceAuthority;
      sgSelectedItem.Cells[3, 1] := SourceGroup;

      sgSelectedItem.Cells[5, 1] := SynonymConcept;
      sgSelectedItem.Cells[6, 1] := SynonymAuthority;
      sgSelectedItem.Cells[7, 1] := SynonymGroup;
    end;
  end else
    with TTermItem(sgScanResults.Objects[0, ARow]) do
    begin
      key1 := ConceptKey1;
      key2 := ConceptKey2;

      sgSelectedItem.Cells[1, 1] := Concept1;
      sgSelectedItem.Cells[2, 1] := Authority1;
      sgSelectedItem.Cells[3, 1] := Group1;

      sgSelectedItem.Cells[5, 1] := Concept2;
      sgSelectedItem.Cells[6, 1] := Authority2;
      sgSelectedItem.Cells[7, 1] := Group2;
    end;

  with sgSelectedItem do
  begin
    for i := RowCount - 1 downto 2 do
      Rows[i].Clear;
    RowCount := 2;

    // Populate synonyms of Source concept.
    rs := dmGeneral.GetRecordset(
              'usp_Concept_Select_Synonyms',
              ['@Concept_Key', key1]);
    while not rs.Eof do
    begin
      // Always start on a fresh row.
      RowCount := RowCount + 1;
      Cells[0, 2] := ResStr_Synonyms;
      Cells[1, RowCount - 1] := rs.Fields['Item_Name'].Value;
      Cells[2, RowCount - 1] := rs.Fields['Authority'].Value;
      Cells[3, RowCount - 1] := rs.Fields['Group_Name'].Value;
      rs.MoveNext;
    end;

    // Populate synonyms of potential synonym concept.
    rs := dmGeneral.GetRecordset(
              'usp_Concept_Select_Synonyms',
              ['@Concept_Key', key2]);
    i := 3;
    while not rs.Eof do
    begin
      if RowCount < i then
        RowCount := RowCount + 1;
      Cells[0, 2] := ResStr_Synonyms;
      Cells[5, i - 1] := rs.Fields['Item_Name'].Value;
      Cells[6, i - 1] := rs.Fields['Authority'].Value;
      Cells[7, i - 1] := rs.Fields['Group_Name'].Value;
      rs.MoveNext;
      Inc(i);
    end;
  end;
end;

{===============================================================================
Procedure Name: sgSelectedItemDrawCell
       Purpose: Handles drawing of the cells of sgSelectedItem, thus allowing
                the term-linking arrows to be rendered in the central column.
-------------------------------------------------------------------------------}
procedure TfrmConceptGroupQualityChecker.sgSelectedItemDrawCell(Sender: TObject; ACol,
  ARow: Integer; ARect: TRect; State: TGridDrawState);

  {=============================================================================
  Procedure Name: DrawLinks
         Purpose: Draws the arrows that illustrate the links between terms in
                  sgSelectedItem.
  -----------------------------------------------------------------------------}
  procedure DrawLinks;
  var
    i, j: Integer;
    pStart, pEnd: TPoint;
    lRect: TRect;
    startShift, endShift: Integer;
  begin
    with sgSelectedItem do
    begin
      startShift := ColWidths[0];  // Fixed column.
      endShift := ColWidths[0];
      // Need to account for horizontal scrolling of grid.
      for i := LeftCol to 3 do
        Inc(startShift, ColWidths[i]);
      for i := LeftCol to 4 do
        Inc(endShift, ColWidths[i]);

      // Don't try and draw anything if the arrow column isn't visible.
      if startShift >= endShift then
        Exit;

      for i := 1 to RowCount - 1 do
        for j := 1 to RowCount - 1 do
          // If terms match, draw arrow.
          if (Cells[1, i] <> '') and (Cells[1, i] = Cells[5, j]) then
          begin
            // Go from left to right
            Canvas.Pen.Color := clActiveCaption;
            Canvas.Brush.Color := clGradientActiveCaption;

            pStart := Point(startShift + 10, (DefaultRowHeight + 1) *
                (i - TopRow + 1) + DefaultRowHeight div 2);
            pEnd := Point(endShift - 5, (DefaultRowHeight + 1) *
                (j - TopRow + 1) + DefaultRowHeight div 2);

            // Draw left arrow.
            Canvas.Polygon([
                pStart,
                Point(pStart.X, pStart.Y - 5),
                Point(pStart.X - 5, pStart.Y),
                Point(pStart.X, pStart.Y + 5)]);

            // Draw right arrow.
            Canvas.Polygon([
                pEnd,
                Point(pEnd.X, pEnd.Y - 5),
                Point(pEnd.X + 5, pEnd.Y),
                Point(pEnd.X, pEnd.Y + 5)]);

            // Draw small bezier curve between the two arrows.
            Canvas.PolyBezier([
                pStart,
                Point(
                  pStart.X + ((pEnd.X - PStart.X) div 4),
                  pStart.Y
                ),
                Point(
                  pStart.X + 3 * ((pEnd.X - PStart.X) div 8),
                  pStart.Y + ((pEnd.Y - pStart.Y) div 4)
                ),
                Point(
                  pStart.X + ((pEnd.X - PStart.X) div 2),
                  pStart.Y + ((pEnd.Y - pStart.Y) div 2)
                ),
                Point(
                  pEnd.X - 3 * ((pEnd.X - PStart.X) div 8),
                  pEnd.Y - ((pEnd.Y - pStart.Y) div 4)
                ),
                Point(
                  pEnd.X - ((pEnd.X - PStart.X) div 4),
                  pEnd.Y
                ),
                pEnd]);
          end;
      // Can't be bothered spending more time on this, so just clear the cell everytime.
      lRect := CellRect(4, 0);
      InflateRect(lRect, 1, 1);
      Canvas.Pen.Color   := clBlack;
      Canvas.Brush.Color := clBtnFace;
      Canvas.Rectangle(lRect);
    end;
  end;

begin
  with sgSelectedItem do
  begin
    if (ACol = 0) and (ARow > 1) and (ARow < RowCount - 1) then
      with Canvas do
      begin
        Font.Color  := clBtnText;
        Pen.Color   := clBtnFace;
        Brush.Color := clBtnFace;
        OffsetRect(ARect, 0, 1);
      end
    else
    if (ACol = 4) and (ARow > 0) then
      with Canvas do
      begin
        Font.Color  := clWindow;
        Pen.Color   := clWindow;
        Brush.Color := clWindow;
        InflateRect(ARect, 0, 1);
        OffsetRect(ARect, 0, 1);
      end
    else
    if (ACol > 0) and (ARow = 1) then
      Canvas.Font.Style := [fsBold];

    // No highlight of selected row. Can't do anything with grid anyway.
    if (ACol > 0) and (ARow > 0) and (gdSelected in State) then
    begin
      Canvas.Font.Color  := clWindowText;
      Canvas.Brush.Color := clWindow;
    end;

    Canvas.FillRect(ARect);
    dmInterface.DrawTerm(
        Canvas,
        Rect(ARect.Left + 3, ARect.Top + 2, ARect.Right, ARect.Bottom),
        Cells[ACol, ARow],
        False);
  end;

  // Always draw arrows.
  DrawLinks;
end;

{-------------------------------------------------------------------------------
}
procedure TfrmConceptGroupQualityChecker.SplitterMoved(Sender: TObject);
begin
  sgScanResults.Refresh;
  sgSelectedItem.Refresh;
end;

{-------------------------------------------------------------------------------
}
procedure TfrmConceptGroupQualityChecker.btnSelectionClick(Sender: TObject);
var
  i: Integer;
begin
  if btnSelection.ImageIndex = IMG_SELECT_ALL then begin
    btnSelection.Caption := ResStr_UnselectAll;
    btnSelection.ImageIndex := IMG_UNSELECT_ALL;
    for i := 0 to FTermList.Count - 1 do
      TTermItem(FTermList[i]).Resolve := True;
  end else begin
    btnSelection.Caption := ResStr_SelectAll;
    btnSelection.ImageIndex := IMG_SELECT_ALL;
    for i := 0 to FTermList.Count - 1 do
      TTermItem(FTermList[i]).Resolve := False;
  end;
  sgScanResults.Refresh;
end;

{-------------------------------------------------------------------------------
  Updates the synonym/homonym relationships as specified by the user, and
  offers to sign off the concept group provided all potential synonyms across
  all domains have been resolved.
}
procedure TfrmConceptGroupQualityChecker.btnProceedClick(Sender: TObject);
var
  i: Integer;
  item: TConceptItem;
  term: TTermItem;
  lCursor: TCursor;
  lAllConceptsResolved: Boolean;
begin
  lCursor := HourglassCursor;
  lAllConceptsResolved := true;
  try
    if FForSynonyms then begin
      // For each pair selected concepts, run the proc to make them synonyms.
      for i := 0 to FConceptList.Count - 1 do begin
        if TConceptItem(FConceptList[i]).SourceConcept <> '' then
        begin
          item := TConceptItem(FConceptList[i]);
          if item.Resolution = 1 then begin
            dmGeneral.RunUpdateStoredProc(
                'usp_MakeConceptsSynonyms_Update',
                ['@ConceptKeySelected', item.ItemKey,
                 '@ConceptKeyPasted', item.SynonymConceptKey]);

            // Handle deduplication, if requested.
            if chkResolveDuplicates.Checked then
              dmGeneral.RunUpdateStoredProc(
                  'usp_DuplicateSynonymTerms_Merge',
                  ['@ConceptKey', item.ItemKey]);
          end
          else if item.Resolution = 2 then begin
            dmGeneral.RunStoredProc(
                'usp_HomonymPair_Insert',
                ['@Concept_Key_1', item.ItemKey,
                 '@Concept_Key_2', item.SynonymConceptKey]);
          end
          else
            lAllConceptsResolved := false;
        end;
      end;
    end else begin
      for i := 0 to FTermList.Count - 1 do begin
        term := TTermItem(FTermList[i]);
        if term.Resolve then
          dmGeneral.RunUpdateStoredProc(
              'usp_DuplicateTerms_Merge',
              ['@NewTermKey', term.ItemKey,
              '@OldTermKey', term.TermKey2]);
      end;
    end;
  finally
    DefaultCursor(lCursor);
  end;

  if PromptForLastCheckUpdate and lAllConceptsResolved then
  begin
    with TdlgConfirmHistoryUpdate.Create(nil) do
    try
      SetKeys(FConceptList.ConceptGroupKey, AppSettings.UserID);
      SetMessage(ResStr_OfferToMarkAsChecked);
      ShowModal;
    finally
      Free;
    end;
  end;

  // Finished, close the form.
  Close;
end;

procedure TfrmConceptGroupQualityChecker.cmbResolutionExit(
  Sender: TObject);
begin
  cmbResolution.Visible := false;
end;

{-------------------------------------------------------------------------------
}
procedure TfrmConceptGroupQualityChecker.PopulateResolutionList;
begin
  cmbResolution.Clear;
  cmbResolution.AddItem(ResStr_ToBeProcessed, TObject(RSN_TO_BE_PROCESSED));
  cmbResolution.AddItem(ResStr_MakeSynonyms, TObject(RSN_MAKE_SYNONYM));
  cmbResolution.AddItem(ResStr_NotASynonym, TObject(RSN_NOT_A_SYNONYM));
end;

{-------------------------------------------------------------------------------
}
procedure TfrmConceptGroupQualityChecker.cmbResolutionClick(
  Sender: TObject);
var
  lConceptItem: TConceptItem;
begin
  if cmbResolution.ItemIndex = -1 then Exit;

  with sgScanResults do begin
    lConceptItem := TConceptItem(Objects[0, Row]);
    if Assigned(lConceptItem) then
    begin
      if (lConceptItem.Resolution <>
          Integer(cmbResolution.Items.Objects[cmbResolution.ItemIndex])) then
        UpdateResolution(
          Integer(cmbResolution.Items.Objects[cmbResolution.ItemIndex]),
          lConceptItem,
          true);
    end;
  end;
end;

{-------------------------------------------------------------------------------
}
procedure TfrmConceptGroupQualityChecker.UpdateResolution(
  AResolution: Integer;
  AConceptItem: TConceptItem;
  APrompt: Boolean);
var
  lIdx: Integer;
begin
  if not RepeatedMeaningPair(
    AConceptItem.SynonymMeaningKey,
    AConceptItem.SourceMeaningKey) then
  begin
    AConceptItem.Resolution := AResolution;
    Exit;
  end;

  if APrompt then
  begin
    if MessageDlg(
      ResStr_ConfirmExtendResolutionForSynonyms,
      mtConfirmation,
      [mbYes, mbNo],
      0) <> mrYes  then Exit;
  end;

  begin
    AConceptItem.Resolution := AResolution;
    for lIdx := 0 to FConceptList.ItemCount - 1 do
    begin
      with TConceptItem(FConceptList.GetItem(lIdx)) do
      begin
        if ((SynonymMeaningKey = AConceptItem.SynonymMeaningKey)
          and (SourceMeaningKey = AConceptItem.SourceMeaningKey))
        or ((SynonymMeaningKey = AConceptItem.SourceMeaningKey)
          and (SourceMeaningKey = AConceptItem.SynonymMeaningKey))
        then
          Resolution := AResolution;
      end;
    end;
  end;
end;

{-------------------------------------------------------------------------------
  Returns true if the there are more than two concept/potential synonym pairs
  with the specified meaning keys. This is so that the a consistent resolution
  can be applied to each of these pairs.
}
function TfrmConceptGroupQualityChecker.RepeatedMeaningPair(
        MeaningKey1: TKeyString;
        MeaningKey2: TKeyString): Boolean;
var
  count: Integer;
  i: Integer;
begin
  Result := false;
  count := 0;
  for i := 0 to FConceptList.Count - 1 do
  begin
    if ((TConceptItem(FConceptList.GetItem(i)).SynonymMeaningKey
                = MeaningKey1)
                and (TConceptItem(FConceptList.GetItem(i)).SourceMeaningKey
                = MeaningKey2))
    or ((TConceptItem(FConceptList.GetItem(i)).SynonymMeaningKey
                = MeaningKey2)
                and (TConceptItem(FConceptList.GetItem(i)).SourceMeaningKey
                = MeaningKey1))
    then
      Inc(count);
    if count > 1 then
    begin
      Result := true;
      Break;
    end;
  end;
end;

{-------------------------------------------------------------------------------
  Checks if the user should be asked to update the check history of the concept
  group.
}
function TfrmConceptGroupQualityChecker.PromptForLastCheckUpdate: Boolean;
var
  lRowCount: Integer;
begin
  Result := FForSynonyms
            and (FConceptList.ConceptGroupSearchKey = '')
            and (FConceptList.ConceptGroupKey <> '')
            and (not FConceptList.OpenFromImport);
  if Result then
  begin
    lRowCount := dmGeneral.GetStoredProcOutputParam(
      'usp_PotentialSynonyms_Count',
      ['@ConceptGroupKey', FConceptList.ConceptGroupKey],
      '@RowCount');
    Result := (FConceptList.MaxRecords >= lRowCount);
  end;
end;

{-------------------------------------------------------------------------------
  Toggles the visibility of items whose resolution matches the menu item
  clicked
}
procedure TfrmConceptGroupQualityChecker.FilterMenuClick(
  Sender: TObject);
var
  i: Integer;
  lResolution: Integer;
begin
  TMenuItem(Sender).Checked := (not TMenuItem(Sender).Checked);

  if TMenuItem(Sender) = mnuToBeProcessed then
    lResolution := RSN_TO_BE_PROCESSED
  else if TMenuItem(Sender) = mnuMakeSynonym then
    lResolution := RSN_MAKE_SYNONYM
  else
    lResolution := RSN_NOT_A_SYNONYM;

  for i := 1 to FConceptList.ItemCount do
  begin
    with TConceptItem(FConceptList.GetItem(i - 1)) do
      if (Resolution = lResolution) then
      begin
        if TMenuItem(Sender).Checked and Hidden then
          Hidden := False
        else if (not TMenuItem(Sender).Checked) and (not Hidden) then
          Hidden := True;
      end;
  end;
  FConceptList.UpdateGrid;
  SetScanResultsReadOnly;
end;

{-------------------------------------------------------------------------------
  This is intended to disable the comboboxes in sgScanResults. Because of the
  way TDSSStringGrid is implemented, to do this involves making the grid read
  only.
}
procedure TfrmConceptGroupQualityChecker.SetScanResultsReadOnly;
begin
  sgScanResults.ReadOnly :=
    not (sgScanResults.Objects[0, sgScanResults.RowCount - 1] is TConceptItem);
end;

{-------------------------------------------------------------------------------
}
procedure TfrmConceptGroupQualityChecker.sgScanResultsDblClick(
  Sender: TObject);
begin
  if not FForSynonyms then
    ToggleSelectedItem(sgScanResults.Row);
end;

{-------------------------------------------------------------------------------
}
procedure TfrmConceptGroupQualityChecker.sgScanResultsKeyDown(
  Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (Key = VK_SPACE) and (not FForSynonyms) then
    ToggleSelectedItem(sgScanResults.Row);
end;

end.
