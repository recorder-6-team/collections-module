{===============================================================================
  Unit:        Find

  Defines:     TdlgFind

  Description: General purpose Find dialog.

  Model:       Find.mpb

  Created:     August 2003

  Last revision information:
    $Revision: 35 $
    $Date: 1/06/11 15:11 $
    $Author: Andrewkemp $

===============================================================================}
unit Find;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, InterfaceDataModule, GeneralData, StdCtrls, ImageListButton,
  ExtCtrls, ResourceStrings, ADODb, ComboListID, ExceptionForm, ComCtrls;

type
  EFindException = class(TExceptionPath)
  end;

  {-----------------------------------------------------------------------------
    Find dialog used by addins.  The dialog is invoked whenever find functionality is
    required by an addin.  The list of data items searched is configured specifically for
    each type of search run.
    Search query operations are run asynchronously, so that the user is always able to type
    text in to the Find box and the dialog remains 'responsive' at all times.
  }
  TdlgFind = class(TForm)
    bvlFrame: TBevel;
    Label1: TLabel;
    Label2: TLabel;
    btnOk: TImageListButton;
    btnCancel: TImageListButton;
    eSearchText: TEdit;
    lbMatches: TIDListBox;
    Animation: TAnimate;
    tmrSearch: TTimer;
    procedure btnOkClick(Sender: TObject);
    procedure eSearchTextChange(Sender: TObject);
    procedure eSearchTextKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure lbMatchesClick(Sender: TObject);
    procedure lbMatchesDblClick(Sender: TObject);
    procedure lbMatchesDrawItem(Control: TWinControl; Index: Integer; Rect: TRect; State:
        TOwnerDrawState);
    procedure lbMatchesKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormResize(Sender: TObject);
    procedure tmrSearchTimer(Sender: TObject);
  private
    FCurrentSearchTerm: String;
    FLastSearchPopulated: String;
    FParameters: array of Variant;
    FPopulating: Boolean;
    FRequestStopPopulating: Boolean;
    FResolveDuplicateStoredProcName: String;
    FResultKey: String;
    FResultText: String;
    FSearchComponents: TStringList;
    FStoredProcName: String;
    FTitle: String;
    procedure ClearListBox;
    procedure CloseDialogOk;
    procedure DisplaySearchAnimation(ASearching: boolean);
    procedure PopulateSearchList(ARecordset: _Recordset; const ASearchParam: String);
    function PopulationRequired(ASearchParam: String): Boolean;
    procedure SearchCallback(ATarget: TObject; ADataset: TCustomADODataset);
    procedure SetResolveDuplicateStoredProcName(const Value: String);
    procedure SetStoredProcName(const Value: String);
    procedure SetTitle(const Value: String);
    procedure StopPopulating;
    procedure AddToListBox(const ADisplayTerm, AItemKey: string);
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
    procedure AddSearchComponent(AFieldName: String);
    procedure PopulateFromExistingRecordset(ARecordset: _Recordset; const ASearchParam:
        String);
    procedure SetStoredProcParameters(AParameters: array of Variant);
    procedure StartSearch(const ASearchText: String);
    property ResolveDuplicateStoredProcName: String read FResolveDuplicateStoredProcName write
        SetResolveDuplicateStoredProcName;
    property ResultKey: String read FResultKey;
    property ResultText: String read FResultText;
    property StoredProcName: String read FStoredProcName write SetStoredProcName;
    property Title: String read FTitle write SetTitle;
  end;

//==============================================================================
implementation

{$R *.dfm}

type
  {-----------------------------------------------------------------------------
    A helper object that allows the processing on completion of an asynchronous
    database query to be marshalled back onto the main thread.
  }
  TFindResultsProcessor = class
  private
    FDataSet: TADODataSet;
    FDialog: TdlgFind;
  public
    constructor Create(DataSet: TADODataSet; Dialog: TdlgFind);
    procedure ExecuteAndDestroy;
  end;

{-==============================================================================
    TdlgFind
===============================================================================}
{-------------------------------------------------------------------------------
  Object initialisation.
}
constructor TdlgFind.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);

  FSearchComponents := TStringList.Create;
  FLastSearchPopulated := '';
  FPopulating := False;
  FRequestStopPopulating := False;
end;  // TdlgFind.Create 

{-------------------------------------------------------------------------------
}
destructor TdlgFind.Destroy;
begin
  FSearchComponents.Free;
  
  inherited Destroy;
end;  // TdlgFind.Destroy 

{-------------------------------------------------------------------------------
}
procedure TdlgFind.AddSearchComponent(AFieldName: String);
begin
  FSearchComponents.Add(AFieldName);
end;  // TdlgFind.AddSearchComponent 

{===============================================================================
Procedure Name: AddToListBox
       Purpose: Adds an item to the list of results that match the search.
    Parameters: ADisplayTerm: The text to show (including any italics <i></i>
                tags, which will be interpreted later by the control).
                AItemKey: The key string of the item being added.
-------------------------------------------------------------------------------}
procedure TdlgFind.AddToListBox(const ADisplayTerm, AItemKey: string);
var
  lDuplicateIdx: Integer;
  lTopIndex: Integer;
begin
  with lbMatches do begin
    // Lock the window, since adding an item tends to scroll the list box
    // and we reset the top index to fix this, but we don't want flicked
    LockWindowUpdate(Handle);
    try
      lTopIndex := TopIndex;
      if ResolveDuplicateStoredProcName = '' then
        lDuplicateIdx := -1
      else
        lDuplicateIdx := IndexOf(ADisplayTerm);

      if lDuplicateIdx = -1 then
        Add(ADisplayTerm, AItemKey)
      else begin
        // We have a duplicate, so first ensure the new term we add is fully specified
        Add(dmGeneral.GetStoredProcOutputParam(ResolveDuplicateStoredProcName,
                                               ['@ItemKey', AItemKey], '@Output'), AItemKey);

        // And also the original term that was already on the list
        Items[lDuplicateIdx] := dmGeneral.GetStoredProcOutputParam(
                                               ResolveDuplicateStoredProcName,
                                               ['@ItemKey', StrID[lDuplicateIdx]], '@Output');
        TopIndex := lTopIndex;
      end;
    finally
      LockWindowUpdate(0);
    end; // try
  end;
end;  // TdlgFind.AddToListBox

{-------------------------------------------------------------------------------
}
procedure TdlgFind.btnOkClick(Sender: TObject);
begin
  FResultKey := lbMatches.CurrentStrID;
  FResultText := lbMatches.CurrentItem;
end;  // TdlgFind.btnOkClick 

{-------------------------------------------------------------------------------
}
procedure TdlgFind.ClearListBox;
begin
  lbMatches.Clear;
end;  // TdlgFind.ClearListBox 

{-------------------------------------------------------------------------------
}
procedure TdlgFind.CloseDialogOk;
begin
  btnOkClick(nil);
  ModalResult := mrOk;
end;  // TdlgFind.CloseDialogOk 

{-------------------------------------------------------------------------------
}
procedure TdlgFind.DisplaySearchAnimation(ASearching: boolean);
begin
  if ASearching then
    Screen.Cursor := crAppStart
  else begin
    Screen.Cursor := crDefault;
    dmGeneral.CancelAsyncCommands(SearchCallback);
  end;
  Animation.Active := ASearching;
  Animation.Visible := ASearching;
end;  // TdlgFind.DisplaySearchAnimation 

{-------------------------------------------------------------------------------
}
procedure TdlgFind.eSearchTextChange(Sender: TObject);
begin
  tmrSearch.Enabled := False;
  // Only allow searching on something, even if it is '*'
  if eSearchText.Text = '' then begin
    DisplaySearchAnimation(False);
    StopPopulating;
    ClearListBox;
    FCurrentSearchTerm := '';
    FLastSearchPopulated := '';
    Exit;
  end;
  // If backspace used, then other pending queries ought to be cancelled.
  if Length(eSearchText.Text) < Length(FCurrentSearchTerm) then
    dmGeneral.CancelAsyncCommands(SearchCallback);

  // Threading and asynchronous queries cause a bit of mayhem with execution flow.
  // So delay the actual search by 1/2 second, leaving the user to type as fast as wanted
  // before actually doing any search.
  tmrSearch.Enabled := True;
end;  // TdlgFind.eSearchTextChange

{-------------------------------------------------------------------------------
}
procedure TdlgFind.eSearchTextKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  lKeyData: Word;
begin
  // If navigation key pressed, send a fake KEYDOWN message to the listbox,
  // so it can deal with it itself.
  if Key in [VK_DOWN, VK_UP, VK_PRIOR, VK_NEXT] then begin
    lKeyData := 0;
    if ssShift in Shift then lKeyData := lKeyData  + VK_SHIFT;
    if ssCtrl in Shift then lKeyData := lKeyData + VK_CONTROL;
    SendMessage(lbMatches.Handle, WM_KEYDOWN, Key, lKeyData);
    Key := 0;
  end else
  if (Key = VK_RETURN) and (btnOk.Enabled) then
    CloseDialogOk;
end;  // TdlgFind.eSearchTextKeyDown 

{-------------------------------------------------------------------------------
}
procedure TdlgFind.FormActivate(Sender: TObject);
begin
  eSearchText.SelStart := Length(eSearchText.Text);
end;  // TdlgFind.FormActivate 

{-------------------------------------------------------------------------------
}
procedure TdlgFind.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  dmGeneral.CancelAsyncCommands(SearchCallback);
end;  // TdlgFind.FormClose 

{-------------------------------------------------------------------------------
}
procedure TdlgFind.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if FPopulating then
    StopPopulating;
end;  // TdlgFind.FormCloseQuery 

{-------------------------------------------------------------------------------
}
procedure TdlgFind.FormResize(Sender: TObject);
begin
  lbMatches.Invalidate;
end;  // TdlgFind.FormResize

{-------------------------------------------------------------------------------
}
procedure TdlgFind.lbMatchesClick(Sender: TObject);
begin
  btnOk.Enabled := lbMatches.ItemIndex <> -1;
end;  // TdlgFind.lbMatchesClick

{-------------------------------------------------------------------------------
}
procedure TdlgFind.lbMatchesDblClick(Sender: TObject);
begin
  CloseDialogOk;
end;  // TdlgFind.lbMatchesDblClick 

{-------------------------------------------------------------------------------
}
procedure TdlgFind.lbMatchesDrawItem(Control: TWinControl; Index: Integer; Rect: TRect;
    State: TOwnerDrawState);
begin
  dmInterface.DrawTerm(lbMatches.Canvas, Rect, lbMatches.Items[Index], odSelected in State);
end;  // TdlgFind.lbMatchesDrawItem 

{-------------------------------------------------------------------------------
}
procedure TdlgFind.lbMatchesKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (Key = VK_RETURN) and (btnOk.Enabled) then
    CloseDialogOk;
end;  // TdlgFind.lbMatchesKeyDown 

{-------------------------------------------------------------------------------
}
procedure TdlgFind.PopulateFromExistingRecordset(ARecordset: _Recordset; const
    ASearchParam: String);
begin
  FCurrentSearchTerm := ASearchParam;
  FLastSearchPopulated := ASearchParam;
  if ARecordset.RecordCount>0 then begin
    ARecordset.MoveFirst;
    PopulateSearchList(ARecordset, ASearchParam);
  end;
end;  // TdlgFind.PopulateFromExistingRecordset 


{===============================================================================
Function Name: RemoveTags
      Purpose: Gets a copy of the specified display term, but with any <i></i>
               or <b/> tags stripped out. Used when sorting alphabetically.
   Parameters: DisplayTerm: The original term, which may contain tags.
-------------------------------------------------------------------------------}
function RemoveTags(DisplayTerm: String): String;
begin
  Result := StringReplace(DisplayTerm, '<i>', '', [rfIgnoreCase]);
  Result := StringReplace(Result, '</i>', '', [rfIgnoreCase]);
  Result := StringReplace(Result, '<b/>', '', [rfIgnoreCase]);
end;

{===============================================================================
Function Name: CompareWithoutTags
      Purpose: Called from CustomSort when sorting search result items
               alphabetically. Compares the display strings from two items, but
               ignores any <i></i> tags so that these do not affect the order.
   Parameters: List: The list of items to sort.
               Index1: The index of one of the two items being compared.
               Index2: The index of the other of the two items being compared.
      Returns: -1 if the item at Index1 should appear higher in the list, 1 if
               the other item should appear higher in the list, and 0 if
               (ignoring tags) they are identical.
-------------------------------------------------------------------------------}
function CompareWithoutTags(List: TStringList; Index1, Index2: Integer): Integer;
begin
  if (RemoveTags(List.ValueFromIndex[Index1]) =
      RemoveTags(List.ValueFromIndex[Index2])) then
    Result := 0
  else if (RemoveTags(List.ValueFromIndex[Index1]) <
      RemoveTags(List.ValueFromIndex[Index2])) then
    Result := -1
  else
    Result := 1;
end;

{-------------------------------------------------------------------------------
  Given a set of search result records, populates the list of matches with the
  appropriate values.

    ARecordSet: The set of records that contains the display terms and their
                associated key strings.
  ASearchParam: The search phrase that produced the results.
}
procedure TdlgFind.PopulateSearchList(ARecordset: _Recordset;
    const ASearchParam: String);
var
  StringsToSort: TStringList;
  i: Integer;
  lDisplayTerm: String;
  lContainsListPreferredField: Boolean;
begin
  StringsToSort := TStringList.Create;

  FPopulating := True;
  lContainsListPreferredField := False;
  try
    FLastSearchPopulated := ASearchParam;

    with ARecordset do
    begin
      // Determine if the record set has a 'List_Preferred' column
      if not Eof then
      try
        if Fields['List_Preferred'] <> nil then
            lContainsListPreferredField := true;
      except on E: exception do
        lContainsListPreferredField := false;
      end;

      while not Eof do
      begin
        if FRequestStopPopulating then
          break;
        // if another search starts whilst this one is running, then stop
        if FCurrentSearchTerm <> ASearchParam then
          Break // from loop
        else
        begin
          FLastSearchPopulated := ASearchParam;
          lDisplayTerm := Fields['DisplayTerm'].Value;

          // If we are searching for determinations and the current item is
          // 'list preferred', then add a self-terminating bold tag to the start
          // of the item. The search list will then display the item in bold.
          // However, it is only able to interpret this specific case; there
          // is no support for the general use of <b></b> tags like there is
          // with <i></i> tags for italics.
          if lContainsListPreferredField and (Fields['List_Preferred'].Value = 'true') then
            // List_Preferred can be null, so the above string comparison is necessary
            lDisplayTerm := '<b/>' + lDisplayTerm ;
          
          // Add the display term and the key as a name/value pair. The '='
          // character must not appear in the key, as it is used to delimit the
          // start of the value string - it can however therefore appear one or
          // more times in the display string.
          StringsToSort.Add(Fields['Item_Key'].Value + '=' + lDisplayTerm);

          MoveNext;
        end;  // if FCurrentSearchTerm <> ASearchParam
      end;  // while not Eof
    end;  // With ARecordset

    if StringsToSort.Count > 0 then
    begin
      // Run the alphabetical sort, ignoring any tags in the display terms.
      StringsToSort.CustomSort(CompareWithoutTags);

      for i := 0 to StringsToSort.Count - 1 do
        AddToListBox(StringsToSort.ValueFromIndex[i], StringsToSort.Names[i]);
    end;

    if (lbMatches.ItemIndex = -1) and (lbMatches.Count > 0) then
    begin
      lbMatches.ItemIndex := 0;
      lbMatchesClick(nil);
    end;
  finally
    FPopulating := False;
    StringsToSort.Free;
  end; // try
end;  // TdlgFind.PopulateSearchList

{-------------------------------------------------------------------------------
}
function TdlgFind.PopulationRequired(ASearchParam: String): Boolean;
var
  lLastSearchMatch: Integer;
  lThisSearchMatch: Integer;
  lCurrentText: String;
begin
  // FLastSearchPopulated and ASearchParam have the '*' converted to '%' already.
  // So if we need to do the same for current text, or it won't work.
  lCurrentText := StringReplace(eSearchText.Text, '*', '%', [rfReplaceAll]);

  // Don't search if nothing to do (i.e. this is an old recordset returning)
  // Find out how many characters of the current required search we have already done
  if (FLastSearchPopulated = Copy(lCurrentText, 1, Length(FLastSearchPopulated)))
      and (Length(lCurrentText)>=Length(FLastSearchPopulated)) then
    lLastSearchMatch := Length(FLastSearchPopulated)
  else
    lLastSearchMatch := 0;
  if ASearchParam = Copy(lCurrentText, 1, Length(ASearchParam)) then
    lThisSearchMatch := Length(ASearchParam)
  else
    lThisSearchMatch := 0;

  Result := lThisSearchMatch > lLastSearchMatch;
end;  // TdlgFind.PopulationRequired

{-------------------------------------------------------------------------------
}
procedure TdlgFind.SearchCallback(ATarget: TObject; ADataset: TCustomADODataset);
var
  Processor: TFindResultsProcessor;
begin
  if not (ADataset is TADODataset) then
    raise EFindException.Create(Format(ResStr_InvalidMethodCall, ['TdlgFind.SearchCallback']));

  Processor := TFindResultsProcessor.Create(TADODataSet(ADataSet), Self);
  TThread.Synchronize(nil, Processor.ExecuteAndDestroy);
end;  // TdlgFind.SearchCallback

{-------------------------------------------------------------------------------
}
procedure TdlgFind.SetResolveDuplicateStoredProcName(const Value: String);
begin
  FResolveDuplicateStoredProcName := Value;
end;  // TdlgFind.SetResolveDuplicateStoredProcName

{-------------------------------------------------------------------------------
}
procedure TdlgFind.SetStoredProcName(const Value: String);
begin
  FStoredProcName := Value;
end;  // TdlgFind.SetStoredProcName 

{-------------------------------------------------------------------------------
}
procedure TdlgFind.SetStoredProcParameters(AParameters: array of Variant);
var
  lIdx: Integer;
begin
  SetLength(FParameters, Length(AParameters) + 2);
  for lIdx := 0 to High(AParameters) do
    FParameters[lIdx] := AParameters[lIdx];
  // set up parameter for search text
  FParameters[High(FParameters) - 1] := '@SearchText';
end;  // TdlgFind.SetStoredProcParameters 

{-------------------------------------------------------------------------------
}
procedure TdlgFind.SetTitle(const Value: String);
begin
  FTitle := Value;
  
  Caption := ResStr_Find + ' ' + Value;
end;  // TdlgFind.SetTitle 

{-------------------------------------------------------------------------------
}
procedure TdlgFind.StartSearch(const ASearchText: String);
begin
  eSearchText.Text := ASearchText;
end;  // TdlgFind.StartSearch 

{-------------------------------------------------------------------------------
}
procedure TdlgFind.StopPopulating;
begin
  FRequestStopPopulating := True;
  while FPopulating do
    Application.ProcessMessages;
  FRequestStopPopulating := False;
end;  // TdlgFind.StopPopulating

{-------------------------------------------------------------------------------
}
procedure TdlgFind.tmrSearchTimer(Sender: TObject);
begin
  if eSearchText.Text <> FCurrentSearchTerm then begin
    // put parameter in for search text
    FParameters[High(FParameters)] := StringReplace(eSearchText.Text, '*', '%',
                                                    [rfReplaceAll]);
    dmGeneral.GetAsyncRecordset(FStoredProcName, FParameters, lbMatches, SearchCallback);
    DisplaySearchAnimation(True);
  end;
  // Stop the timer, until it is set off again in eSearchTextChange handler.
  tmrSearch.Enabled := False;
end;  // TdlgFind.tmrSearchTimer


{===============================================================================
    TFindResultsProcessor
}
{-------------------------------------------------------------------------------
  Initializes a TFindResultsProcessor with the given data set and find dialog.
}
constructor TFindResultsProcessor.Create(
  DataSet: TADODataSet;
  Dialog: TdlgFind);
begin
  FDataSet := DataSet;
  FDialog := Dialog;
end;

{-------------------------------------------------------------------------------
  Updates the find dialog with the results from the data set, and destroys
  the results processor.
}
procedure TFindResultsProcessor.ExecuteAndDestroy;
var
  SearchTerm: string;
begin
  SearchTerm := VarToStr(FDataset.Parameters.ParamByName('@SearchText').Value);

  if FDialog.PopulationRequired(SearchTerm) then
  begin
    FDialog.FCurrentSearchTerm := SearchTerm;
    FDialog.StopPopulating;
    FDialog.ClearListBox;
    FDialog.PopulateSearchList(FDataset.Recordset, SearchTerm);
    // Cancel other commands if this one is up to date with the current
    // search text
    if SearchTerm = FDialog.eSearchText.Text then
      FDialog.DisplaySearchAnimation(False);
  end;
  Destroy;
end;

end.
