unit SpecimenFinderDragFrame;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, BaseDragFrameUnit, StdCtrls, ExtCtrls, DataClasses, ADODB,
  SpecimenFinderSQLData, ImageListButton, ImgList, DragAndDropCriterion,
  ICriterionUnit, BooleanCriterionUnit, BooleanProperty, PreviousResultsCriterion;

const
  IMG_COLLECTION = 0;
  IMG_CONCEPT    = 1;
  IMG_LOCATION   = 2;
  IMG_NAME       = 3;
  IMG_SURVEY     = 4;
  IMG_SPECIMEN   = 5;
  IMG_TAXON      = 6;
  IMG_STORE      = 7;
  IMG_MOVEMENT   = 8;

type
  {-----------------------------------------------------------------------------
    Frame that contains the list box the user is able to drag items into when
    specifying the criteria for input into the Specimen Finder.
  }
  TfraSpecimenFinderDragFrame = class(TBaseDragFrame)
    ilSpecimenFinder: TImageList;
    Label1: TLabel;
    lbFinder: TListBox;
    pnlDataSelection: TPanel;
    pnlFinder: TPanel;
    pnlLabelSpacer: TPanel;
    rgDataSelectionMode: TRadioGroup;
    shpFinder: TShape;
    procedure lbFinderDrawItem(Control: TWinControl; Index: Integer; Rect:
        TRect; State: TOwnerDrawState);
    procedure lbFinderKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure rgDataSelectionModeClick(Sender: TObject);
  private
    FRefreshResults: Boolean;
    procedure DropDataItem(const Sender: TObject; const iFormat : integer;
        const iSourceData: TKeyList; const iTextStrings : TstringList; var ioHandled : boolean);
    function DragAndDropItemExists(DragAndDropCriterion: TDragAndDropCriterion): Boolean;
    function ResolveTableName(const ASourceData: TKeyList; AIndex: integer): string;


  protected
    procedure RegisterDragDropComponents; override;
  public
    procedure CleanUp;
    function GetResults: _Recordset;
    procedure RemoveSelectedItem;
    property RefreshResults: Boolean read FRefreshResults write FRefreshResults;
    procedure AddFinderDragAndDropItem(DragAndDropCriterion: TDragAndDropCriterion);
  end;

  EInvalidFinderType = class(Exception)
  end;

  ENoFinderData = class(Exception)
  end;

implementation

{$R *.dfm}

uses
  DropTarget, GeneralData, ResourceStrings, InterfaceDataModule,
      LuxembourgConstants, GeneralFunctions;

{-==============================================================================
    TfraSpecimenFinderDragFrame
===============================================================================}
{-------------------------------------------------------------------------------
  Perform cleanup operations required before destruction.  These must occur
      before the screen starts to destroy otherwise window handles are not
      available.
}
procedure TfraSpecimenFinderDragFrame.CleanUp;
var
  i: Integer;
begin
  //Remove Registered DrapDrop Components before destructor
  UnregisterDragDropComponents;
  
  for i := 0 to lbFinder.Count - 1 do
    TDragAndDropCriterion(lbFinder.Items.Objects[i]).Free;
end;  // TfraSpecimenFinderDragFrame.CleanUp

{-------------------------------------------------------------------------------
  Adds an item to lbFinder. Only proceeds if the item hasn't been added to
  the list already.
}
procedure TfraSpecimenFinderDragFrame.AddFinderDragAndDropItem(
    DragAndDropCriterion: TDragAndDropCriterion);
var
  Caption: String;
begin
    if not DragAndDropItemExists(DragAndDropCriterion) then
    begin
      // Convert table name to a stored proc name that retrieves the caption.
      Caption := dmGeneral.GetStoredProcOutputParam('usp_' +
          StringReplace(DragAndDropCriterion.TableName,
                          '_', '', [rfReplaceAll]) + '_Get',
                          ['@Key', DragAndDropCriterion.SearchValue],
                          '@Caption');
      lbFinder.AddItem(Caption, DragAndDropCriterion);
    end;

end;

{-------------------------------------------------------------------------------
  Drop handler for items being added to the list box.
}
procedure TfraSpecimenFinderDragFrame.DropDataItem(const Sender: TObject; const
    iFormat : integer; const iSourceData: TKeyList; const iTextStrings :
    TstringList; var ioHandled : boolean);
var
  lstKey, lstTableName: string;
  lNewCriterion: TDragAndDropCriterion;
  i: Integer;
  lSubLocation: TDragAndDropCriterion;
begin
  for i := 0 to iSourceData.Header.ItemCount - 1 do
  begin
    lstKey := iSourceData.Items[i].KeyField1;
    lstTableName := ResolveTableName(iSourceData, i);

    lNewCriterion := TDragAndDropCriterion.Create(
                            lstTableName,
                            lstKey);

    // Location table is a special case. We give the user the option to include
    // any sub-locations.
    if lNewCriterion.SearchType = ftLocation then
    begin
      // Procedure finds only child nodes that are locations, not features.
      with dmGeneral.GetRecordset(
          'usp_Locations_Select_AllSubLocationsForLevel', ['@Key', lstKey]) do
      begin
        if not Eof then
        begin
          // The first record is the selected location, that we definitely want
          // to include.
          AddFinderDragAndDropItem(lNewCriterion);
          MoveNext;

          // May be followed by child locations. Let the user choose whether to
          // include these also.
          if (not Eof) and (MessageDlg(ResStr_ConfirmExpandLocationHierarchy,
              mtConfirmation, [mbYes, mbNo], 0) = mrYes) then
          begin
            while not Eof do
            begin
              lSubLocation := TDragAndDropCriterion.Create(lstTableName, Fields['ItemKey'].Value);
              AddFinderDragAndDropItem(lSubLocation);
              MoveNext;
            end;
          end;
        end;
        Close;
      end;
    end
    else
      AddFinderDragAndDropItem(lNewCriterion);
  end;

  lbFinder.ItemIndex := lbFinder.Count - 1;

  FRefreshResults := True;
end;  // TfraSpecimenFinderDragFrame.DropDataItem 

{-------------------------------------------------------------------------------
  Returns a recordset with all the specimens matching the search criteria in
  lbFinder.
}
function TfraSpecimenFinderDragFrame.GetResults : _Recordset;
var
  lCriteria : array of ICriterion;
  i: Integer;
  lFinderSQLGenerator: TFinderSQLGenerator;
  lMainSql, lPreSql: String;
  lCursor: TCursor;
begin
  if lbFinder.Count > 0 then begin
    lCursor := HourglassCursor;
    lFinderSQLGenerator := TFinderSQLGenerator.Create;
    try
       SetLength(lCriteria, lbFinder.Count);
       for i := 0 to lbFinder.Count - 1 do
        if lbFinder.Items.Objects[i] is TDragAndDropCriterion then
        begin
          lCriteria[i] := TDragAndDropCriterion.Create(
                              TDragAndDropCriterion(lbFinder.Items.Objects[i]).TableName,
                              TDragAndDropCriterion(lbFinder.Items.Objects[i]).SearchValue);
        //Fill lCriteria with copies of the list objects, since these copies will
        //be disposed of when lCriteria is.
        end
        else if lbFinder.Items.Objects[i] is TBooleanCriterion then
        begin
          lCriteria[i] := TBooleanCriterion.Create(
                              TBooleanCriterion(lbFinder.Items.Objects[i]).PropertyType,
                              TBooleanCriterion(lbFinder.Items.Objects[i]).Operator,
                              TBooleanCriterion(lbFinder.Items.Objects[i]).Operand1,
                              TBooleanCriterion(lbFinder.Items.Objects[i]).Operand2,
                              TBooleanCriterion(lbFinder.Items.Objects[i]).HasProperty);

        end
        else
        begin
          lCriteria[i] := TPreviousResultsCriterion.Create(
                            TPreviousResultsCriterion(lbFinder.Items.Objects[i]).Keys);
          
        end;
       lMainSql := lFinderSQLGenerator.GenerateSql(
          lCriteria,
          rgDataSelectionMode.ItemIndex = 0,
          lPreSql);

       dmGeneral.ExecuteSQL(lPreSql, False);
       Result := dmGeneral.ExecuteSQL(lMainSql, True);
    finally
      lFinderSQLGenerator.Free;
      DefaultCursor(lCursor);

    end;
  end else
    Result := nil;


  FRefreshResults := False;
end;

{-------------------------------------------------------------------------------
  Returns true if a drag-and-drop criterion is already in lbFinder, otherwise
  false.
}
function TfraSpecimenFinderDragFrame.DragAndDropItemExists(
  DragAndDropCriterion: TDragAndDropCriterion): Boolean;
var
  i: Integer;
begin
  Result := false;
  for i := 0 to lbFinder.Count - 1 do
    if lbFinder.Items.Objects[i] is TDragAndDropCriterion then
    begin
      if SameText(TDragAndDropCriterion(lbFinder.Items.Objects[i]).SearchValue,
                    DragAndDropCriterion.SearchValue) and
            SameText(TDragAndDropCriterion(lbFinder.Items.Objects[i]).TableName,
                    DragAndDropCriterion.TableName) then
        Result := true;
    end;
end;   // TfraSpecimenFinderDragFrame.DragAndDropItemExists

{-------------------------------------------------------------------------------
  Draws the items in the list box, using correct term formatting. 
}
procedure TfraSpecimenFinderDragFrame.lbFinderDrawItem(Control: TWinControl;
    Index: Integer; Rect: TRect; State: TOwnerDrawState);
var
  liSelectedImageIndex: Integer;
  lCtrl: TListBox;
begin
  inherited;
  lCtrl := TListBox(Control);
  with lCtrl.Canvas do
    if odSelected in State then begin
      if odFocused in State then Brush.Color := clHighlight
                            else Brush.Color := clInactiveCaption;
      Font.Color := clHighlightText;
    end else begin
      Brush.Color := clWindow;
      Font.Color := clWindowText;
    end;
  lCtrl.Canvas.FillRect(Rect);

  if lCtrl.Items.Objects[Index] is TDragAndDropCriterion then
  begin
    case TDragAndDropCriterion(lCtrl.Items.Objects[Index]).SearchType of
      ftCollection:    liSelectedImageIndex := IMG_COLLECTION;
      ftConcept:       liSelectedImageIndex := IMG_CONCEPT;
      ftLocation:      liSelectedImageIndex := IMG_LOCATION;
      ftName:          liSelectedImageIndex := IMG_NAME;
      ftSurvey:        liSelectedImageIndex := IMG_SURVEY;
      ftTaxonListItem: liSelectedImageIndex := IMG_TAXON;
      ftStore:         liSelectedImageIndex := IMG_STORE;
      ftMovement:      liSelectedImageIndex := IMG_MOVEMENT;
    else
      liSelectedImageIndex := -1;
    end;

  end
  else if lCtrl.Items.Objects[Index] is TBooleanCriterion then
  begin
    case TBooleanCriterion(lCtrl.Items.Objects[Index]).PropertyType.Category of
      bpcCollections:                    liSelectedImageIndex := IMG_COLLECTION;
      bpcSpecimens, bpcSpecMeasurements: liSelectedImageIndex := IMG_SPECIMEN;
    else
      liSelectedImageIndex := -1;
    end;
  end
  else
  begin
    liSelectedImageIndex := IMG_SPECIMEN
  end;

  ilSpecimenFinder.Draw(lCtrl.Canvas, Rect.Left, Rect.Top, liSelectedImageIndex);
  Rect.Left := Rect.Left + ilSpecimenFinder.Width + 4;
  Rect.Top  := Rect.Top + 1;
  dmInterface.DrawTerm(lCtrl.Canvas, Rect, lCtrl.Items[Index], odSelected in State);

end;  // TfraSpecimenFinderDragFrame.lbFinderDrawItem 

{-------------------------------------------------------------------------------
  Check for Delete key press on the list box. 
}
procedure TfraSpecimenFinderDragFrame.lbFinderKeyDown(Sender: TObject; var Key:
    Word; Shift: TShiftState);
begin
  inherited;
  if Key = VK_DELETE then RemoveSelectedItem;
end;  // TfraSpecimenFinderDragFrame.lbFinderKeyDown 

{-------------------------------------------------------------------------------
  Register the list box to receive dragged data sources 
}
procedure TfraSpecimenFinderDragFrame.RegisterDragDropComponents;
begin
  RegisterDropComponent(lbFinder, DropDataItem, [TN_LOCATION, TN_CONCEPT,
      TN_TAXON_LIST_ITEM, TN_NAME, TN_INDIVIDUAL, TN_ORGANISATION, TN_SURVEY,
      TN_COLLECTION, TN_STORE, TN_MOVEMENT], [CF_JNCCDATA]);
end;  // TfraSpecimenFinderDragFrame.RegisterDragDropComponents 

{-------------------------------------------------------------------------------
  Removes an item from the list. 
}
procedure TfraSpecimenFinderDragFrame.RemoveSelectedItem;
var
  liSelectedIndex: Integer;
begin
  if lbFinder.ItemIndex = -1 then Exit;

  liSelectedIndex := lbFinder.ItemIndex;
  TDragAndDropCriterion(lbFinder.Items.Objects[liSelectedIndex]).Free;
  lbFinder.DeleteSelected;

  if liSelectedIndex < (lbFinder.Count - 1) then
    lbFinder.ItemIndex := liSelectedIndex
  else
    lbFinder.ItemIndex := (lbFinder.Count - 1);
  
  FRefreshResults := True;
end;  // TfraSpecimenFinderDragFrame.RemoveSelectedItem

{-------------------------------------------------------------------------------
  Items dropped as Individuals or Organisations are treated in the same way,
      using the Name table.  Also ensure MIXED_DATA keyslist are handled
      correctly.
}
function TfraSpecimenFinderDragFrame.ResolveTableName(const ASourceData: TKeyList;
  AIndex: integer): string;
begin
  Result := ASourceData.ItemTable[AIndex];

  if SameText(Result, TN_INDIVIDUAL) or SameText(Result, TN_ORGANISATION) then
    Result := TN_NAME;
end;  // TfraSpecimenFinderDragFrame.ResolveTableName

{-------------------------------------------------------------------------------
  When changing the selection mode, invalidate the results. 
}
procedure TfraSpecimenFinderDragFrame.rgDataSelectionModeClick(Sender: TObject);
begin
  inherited;
  FRefreshResults := True;
end;  // TfraSpecimenFinderDragFrame.rgDataSelectionModeClick

end.




