{===============================================================================
  Unit:        FrameFieldDataGeoAreas.pas

  Defines:     TfraFieldDataGeoAreas

  Description:

  Model:       CollectionsSpecimens.mpb

  Created:     September 2007

===============================================================================}

unit FrameFieldDataGeoAreas;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, BaseFullScreenFrameUnit, BaseDetailFrameUnit, ExtCtrls, StdCtrls,
  ImageListButton, Grids, GeneralFunctions, CompositeComponent, SpatialRef,
  VagueDateEdit, ComboListID, LuxIDComboBox, BaseCompositeComponent, LinkedControls,
  InterfaceDataModule, DssStringGrid, LuxembourgDataClasses, ADOInt, DataClasses,
  DataTypes, VagueDate, Recorder2000_TLB, BaseTabSheetFrameUnit,
  ExceptionForm, DropTarget, Menus, Math, DSSDataTypes, ValidationData;

resourcestring
  ResStr_ConceptNotInGeoArea = 'The selected concept is not from a valid geographic area.';

type
  TGeoAreaItem = class(TLuxGridDataItem)
  private
    FTerm: string;
    FConceptKey: TKeyString;
    FSEGeoAreaKey: TKeyString;
  protected
    function GetDisplayText(Column: Integer): String; override;
    procedure GetData(const Column: Integer; var AText: String; var ANameKey: TKeyString); override;
    procedure InitFromRecord(AFields: Fields); override;
    procedure SetData(const Column: Integer; const AText: String; const AKey: TKeyString); override;
    procedure ValidateData; override;
  public
    property Term: string read FTerm;
    property ConceptKey: TKeyString read FConceptKey write FConceptKey;
    property SEGeoAreaKey: TKeyString read FSEGeoAreaKey write FSEGeoAreaKey;
  end;

  TGeoAreaList = class(TLuxGridDataList)
  private
    FSurveyEventKey: TKeyString;
  protected
    procedure DoAddition(AItem: TLuxCachedDataItem); override;
    procedure DoDeletion(AItem: TLuxCachedDataItem); override;
    procedure DoModification(AItem: TLuxCachedDataItem); override;
    function GetRecordset: _Recordset; override;
  public
    property SurveyEventKey: TKeyString read FSurveyEventKey write FSurveyEventKey;
  end;
  {-----------------------------------------------------------------------------
    Tab page showing all the geo. areas a specimen's field data is linked to. The
    user can add, edit and remove linked geo. areas. Embedded on to TfraFieldData.
  }
  TfraFieldDataGeoAreas = class(TBaseTabSheetFrame, IRequestor)
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
    procedure eNameKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure sgAreasKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure sgAreasSelectCell(Sender: TObject; ACol, ARow: Integer;
      var CanSelect: Boolean);
    procedure eNameChange(Sender: TObject);
    procedure sgAreasMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure sgAreasMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    FGeoAreaList: TGeoAreaList;
    FLoading: Boolean;
    FSurveyEventKey: TKeyString;
    function CanAddRow: Boolean;
    function CheckForDuplicates: Boolean;
    function ConceptInGeographySubjectArea(const conceptKey: String): Boolean;
    function GeoAreaItemAtPoint(X, Y: Integer): TGeoAreaItem;
    procedure DragOverCheck(APoint: TPoint; const ATable, AFieldKey: String; var Accept: Boolean);
    procedure DropKeyword(const Sender: TObject; const iFormat: integer; const
        iSourceData: TKeyList; const iTextStrings: TstringList; var ioHandled: boolean);
    procedure UpdateTerm(const AKeyList: IKeyList);
    procedure ShowHierarchyPath(const AConceptKey: string);
  protected
    procedure EnableControls(AEnabled: Boolean); override;
    procedure LoadData; override;
    procedure SaveData; override;
    procedure SetKey(Value: TKeyString); override;
    procedure RegisterDragDropComponents; override;
    procedure ValidateData; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

//==============================================================================
implementation

uses
  ApplicationSettings, BaseADODataModule, ResourceStrings, LuxembourgConstants,
  Validation, SearchManager, GeneralData, FrameFieldDataGeneral, ThesaurusBrowser_TLB,
  ComObj, LuxembourgFunctions;

{$R *.dfm}

{-==============================================================================
    TGeoAreaItem
===============================================================================}
{-------------------------------------------------------------------------------
  Gets the text in the specified column, styled to look like a hyperlink.
}
function TGeoAreaItem.GetDisplayText(Column: Integer): String;
begin
  Result := inherited GetDisplayText(Column);
  Result := StringReplace(Result, '\', '\\', [rfReplaceAll]);
  Result := StringReplace(Result, '{', '\{', [rfReplaceAll]);
  Result := StringReplace(Result, '}', '\}', [rfReplaceAll]);
  Result := '{\rtf1{\colortbl\red0\green0\blue255;}{\ul\cf0 ' + Result + '}}';
end;

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
  FTerm         := AFields['Plaintext'].Value;
  FConceptKey   := AFields['Concept_Key'].Value;
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

{-------------------------------------------------------------------------------
}
procedure TGeoAreaItem.ValidateData;
var
  lText, lKey: String;
begin
  if (FConceptKey = '') and (FTerm <> '') then begin
    lText := FTerm;
    if DoCheckUnique(lKey, lText, stTermInConceptGroup, CG_GEO_AREAS, ResStr_GeoArea) then begin
      // Found a unique exact match. Update fields accordingly.
      FConceptKey := lKey;
      FTerm := lText;
    end else begin
      ValidateValue(DoCheck(lKey, lText, stTermInConceptGroup, CG_GEO_AREAS, ResStr_GeoArea),
                    Format(ResStr_MissingData, [ResStr_GeoArea]));
      // ValidateValue passed. User chose existing Material.
      FConceptKey := lKey;
      FTerm := lText;
    end;
  end;
end;  // TGeoAreaItem.ValidateData

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
  Result := dmGeneral.GetRecordset('usp_SurveyEventGeoArea_Select_ForFieldData',
                                     ['@Key', MasterKey]);
end;  // TGeoAreaList.GetRecordset

{-==============================================================================
    TfraFieldDataGeoAreas
===============================================================================}
{-------------------------------------------------------------------------------
  Constructor.
}
constructor TfraFieldDataGeoAreas.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  sgAreas.Cells[0,0] := ResStr_GeoArea;
  FGeoAreaList := TGeoAreaList.Create(TGeoAreaItem, sgAreas);
  // Setting the control sets the type to custom.
  sgAreas.ColumnsInfo[0].WinControl := eName;
  sgAreas.ColumnsInfo[0].RichTextContent := true;
  //UpdateMapWindowSelector;
  FLoading := False;
end;  // TfraFieldDataGeoAreas.Create 

{-------------------------------------------------------------------------------
  Destructor frees the list of the string grid. 
}
destructor TfraFieldDataGeoAreas.Destroy;
begin
  FGeoAreaList.Free;
  
  inherited Destroy;
end;  // TfraFieldDataGeoAreas.Destroy
 
{-------------------------------------------------------------------------------
  Handle a click on the add button. 
}
procedure TfraFieldDataGeoAreas.btnAddClick(Sender: TObject);
begin
  inherited;
  if CanAddRow then begin
    FGeoAreaList.AddNew(TGeoAreaItem.CreateNew(FGeoAreaList));
    sgAreas.Row := FGeoAreaList.ItemCount;
  end else
    sgAreas.Row := sgAreas.RowCount - 1;
  eName.SetFocus;
end;  // TfraFielddataGeoAreas.btnAddClick

{-------------------------------------------------------------------------------
}
function TfraFieldDataGeoAreas.CanAddRow: Boolean;
begin
  with sgAreas do
    Result := ((Row = RowCount - 1) and (eName.Text <> '')) or
              (Cells[0, RowCount - 1] <> '');
end;  // TfraFieldDataGeneral.CanAddRow 

{-------------------------------------------------------------------------------
  Handle a click on the remove button. 
}
procedure TfraFieldDataGeoAreas.btnRemoveClick(Sender: TObject);
begin
  inherited;
  with sgAreas do
    if (Cells[Col, Row] <> '') or (eName.Text <> '') then begin
      if MessageDlg(Format(ResStr_ConfirmRowDelete, [ResStr_Row]),
                    mtWarning, [mbYes, mbNo], 0) = mrYes then
        FGeoAreaList.DeleteItem(Row);
    end else
      FGeoAreaList.DeleteItem(Row);
  EnableControls(True);
end;  // TfraFieldDataGeoAreas.btnRemoveClick

{-------------------------------------------------------------------------------
  Handle the selection of a cell: When a cell is selected, display the
  hierarchy of the item in lblPath. 
}
procedure TfraFieldDataGeoAreas.sgAreasSelectCell(Sender: TObject; ACol,
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
procedure TfraFieldDataGeoAreas.ShowHierarchyPath(const AConceptKey: string);
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


{-------------------------------------------------------------------------------
  Handles change of the contents of eName
}
procedure TfraFieldDataGeoAreas.eNameChange(Sender: TObject);
begin
  inherited;
  ShowHierarchyPath((Sender as TLinkedEdit).Key)
end;

{-------------------------------------------------------------------------------
  Set the colours of the controls in edit mode.
}
procedure TfraFieldDataGeoAreas.EnableControls(AEnabled: Boolean);
begin
  inherited;
  sgAreas.ReadOnly := not AEnabled;
  btnRemove.Enabled := (EditMode = emEdit) and (sgAreas.Cells[0, 1] <> '');
end;  // TfraFieldDataGeoAreas.EnableControls

{-------------------------------------------------------------------------------
  Make sure the contents of the Geo Area linked edit is parsed when the user exits it.
}
procedure TfraFieldDataGeoAreas.eNameExit(Sender: TObject);
begin
  inherited;
  btnRemove.Enabled := (EditMode = emEdit) and
                       ((sgAreas.Cells[0, 1] <> '') or (eName.Text <> ''));
end;  // TfraFieldDataGeoAreas.eNameExit

{-------------------------------------------------------------------------------
  Handler for OnFindData for the floating linked edit control on the string grid.
}
procedure TfraFieldDataGeoAreas.eNameFindData(Sender: TObject);
begin
  inherited;
  DoCheck(eName, stTermInSubjectArea, CG_GEO_SUBJECT);
  if CheckForDuplicates then begin
    eName.Text := '';
    eName.Key := '';
  end;
end;  // TfraFieldDataGeoAreas.eNameFindData

{-------------------------------------------------------------------------------
  Get data from the Geo Areas module.
}
procedure TfraFieldDataGeoAreas.eNameGetData(Sender: TObject);
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
      lConceptGroupKey := dmGeneral.GetStoredProcOutputParam(
          'usp_FindFirstConceptGroupForSubject',
          ['@SubjectAreaKey', CG_GEO_SUBJECT],
          '@ConceptGroupKey');
      lThesaurus.DisplayConceptGroup(lConceptGroupKey);
    end;    // if Supports(lIntf, IID_IfrmThesaurusBrowser, lThesaurus)
  end;    // if Assigned(lIntf)
end;  // TfraFieldDataGeoAreas.eNameGetData

{-------------------------------------------------------------------------------
}
procedure TfraFieldDataGeoAreas.eNameKeyDown(Sender: TObject;
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
end;  // TfraFieldDataGeoAreas.eNameKeyDown  

{-------------------------------------------------------------------------------
  Load the data from the recordset.
}
procedure TfraFieldDataGeoAreas.LoadData;
begin
  FLoading := True;
  try
    inherited;
    FGeoAreaList.MasterKey := Key;
    FGeoAreaList.Refresh;
  finally
    FLoading := False;
  end;
end;  // TfraFieldDataGeoAreas.LoadData 

{-------------------------------------------------------------------------------
  Saves the data to the database.
}
procedure TfraFieldDataGeoAreas.SaveData;
begin
  inherited;
  FSurveyEventKey := dmGeneral.GetStoredProcOutputParam('usp_SurveyEventKey_Get_ForFieldData',
                                     ['@Key', Key,
                                     '@SurveyEventKey', FSurveyEventKey],
                                     '@SurveyEventKey');

  FGeoAreaList.SurveyEventKey := FSurveyEventKey;
  FGeoAreaList.Update;
end;  // TfraFieldDataGeoAreas.SaveData

{-------------------------------------------------------------------------------
  Sets the MasterKey in the FGeoAreaList.
}
procedure TfraFieldDataGeoAreas.SetKey(Value: TKeyString);
begin
  inherited SetKey(Value);
  FGeoAreaList.MasterKey := Key;
end;  // TfraFieldDataGeoAreas.SetKey

{-------------------------------------------------------------------------------
}
procedure TfraFieldDataGeoAreas.sgAreasKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  inherited;
  with sgAreas do
    if (Row = RowCount - 1) and (Key = VK_DOWN) and not CanAddRow then Key := 0;
end;  // TfraFieldDataGeoAreas.sgAreasKeyDown

{-------------------------------------------------------------------------------
}
procedure TfraFieldDataGeoAreas.UpdateTerm(const AKeyList: IKeyList);
var
  lKey: TKeyString;
begin
  lKey := AKeyList.GetKeyItem(0).KeyField1;

  if ConceptInGeographySubjectArea(lKey) then begin
    eName.Key := lKey;
    eName.Text := ConvertConceptKeyToCaption(lKey, TN_CONCEPT);
    if CheckForDuplicates then begin
      eName.Text := '';
      eName.Key := '';
    end;
  end else
    ShowInformation(ResStr_ConceptNotInGeoArea);
end;  // TfraFieldDataGeoAreas.UpdateTerm

{-------------------------------------------------------------------------------
}
procedure TfraFieldDataGeoAreas.ValidateData;
begin
  inherited;
  FGeoAreaList.ValidateContent;
end;  // TfraFieldDataGeoAreas.ValidateData

{-------------------------------------------------------------------------------
}
procedure TfraFieldDataGeoAreas.RegisterDragDropComponents;
begin
  RegisterDropComponent(sgAreas, DropKeyword, [TN_CONCEPT], [CF_JNCCDATA], DragOvercheck);
end;  // TfraFieldDataGeoareas.SetupDestinationControls

{-------------------------------------------------------------------------------
  Check concept is from geo subject area before allowing drop.
}
procedure TfraFieldDataGeoAreas.DragOverCheck(APoint: TPoint; const ATable, AFieldKey: String;
  var Accept: Boolean);
begin
  Accept := (not sgAreas.Readonly) and ConceptInGeographySubjectArea(AFieldKey);
end;  // TfraFieldDataGeoAreas.DragOverCheck

{-------------------------------------------------------------------------------
  Code to handle dropping onto the keywords grid
}
procedure TfraFieldDataGeoAreas.DropKeyword(const Sender: TObject; const iFormat:
    integer; const iSourceData: TKeyList; const iTextStrings: TstringList; var
    ioHandled: boolean);
begin
  if (iSourceData.Header.ItemCount>0) and (eName.Enabled) then
    if SameText(iSourceData.ItemTable[0], TN_CONCEPT) then
    begin
      btnAddClick(nil);
      eName.Text := dmGeneral.GetStoredProcOutputParam(
          'usp_Concept_Get',
          ['@Key', iSourceData.Items[0].KeyField1],
          '@Caption');
      eName.Key := iSourceData.Items[0].KeyField1;
      if CheckForDuplicates then
        FGeoAreaList.DeleteItem(sgAreas.Row);
    end;
end;  // TfraFieldDataGeoAreas.DropKeyword;

{-------------------------------------------------------------------------------
}
function TFraFieldDataGeoAreas.ConceptInGeographySubjectArea(const conceptKey: String): Boolean;
begin
  Result := VarToStr(dmGeneral.GetStoredProcOutputParam(
      'usp_SubjectArea_Select_ForConcept_Get',
      ['@Key', conceptKey],
      '@Subject_Area_Key')) = CG_GEO_SUBJECT;
end;

{-------------------------------------------------------------------------------
  Ensures that the same item cannot be entered into the grid twice
}
function TFraFieldDataGeoAreas.CheckForDuplicates: Boolean;
var
  i: integer;
begin
  Result := False;
  for i := 0 to sgAreas.RowCount - 1 do
    if (sgAreas.Cells[0, i] = eName.Text) and (sgAreas.Row <> i) then begin
      Result := True;
      Break;
    end;
end;  // TfraFieldDataGeoAreas.CheckForDuplicates

{-------------------------------------------------------------------------------
  Returns the geographic area item at the specified coördinates in the grid,
  or nil if there is no such item.
}
function TfraFieldDataGeoAreas.GeoAreaItemAtPoint(X, Y: Integer): TGeoAreaItem;
var
  lCellHit: TGridCoord;
  lItem: TGeoAreaItem;
  lText: string;
begin
  Result := nil;
  lCellHit := sgAreas.MouseCoord(X, Y);
  if (lCellHit.Y >= sgAreas.FixedRows) and
     (lCellHit.Y <= FGeoAreaList.ItemCount) then
  begin
    lItem := TGeoAreaItem(FGeoAreaList[lCellHit.Y - 1]);
    lText := RichTextToText(Self, lItem.GetDisplayText(lCellHit.X));
    if X < sgAreas.Canvas.TextWidth(lText) then Result := lItem;
  end;
end;

{-------------------------------------------------------------------------------
  Displays a 'hand' cursor when the user moves over a data cell in the grid.
}
procedure TfraFieldDataGeoAreas.sgAreasMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
  lCursor: TCursor;
begin
  inherited;
  lCursor := crDefault;
  if EditMode = emBrowse then
  begin
    if Assigned(GeoAreaItemAtPoint(X, Y)) then lCursor := crHandPoint;
  end;
  sgAreas.Cursor := lCursor;
end;

{-------------------------------------------------------------------------------
  Opens the thesaurus browser to the selected concept when the user clicks on
  a data cell in the grid.
}
procedure TfraFieldDataGeoAreas.sgAreasMouseDown(
  Sender: TObject;
  Button: TMouseButton;
  Shift: TShiftState;
  X, Y: Integer);
var
  lItem: TGeoAreaItem;
  lApp: IRecorder2000;
  lThesaurus: IfrmThesaurusBrowser;
begin
  inherited;
  if EditMode = emEdit then Exit;
  lItem := GeoAreaItemAtPoint(X, Y);

  if Assigned(lItem) then begin
    lApp := CreateCOMObject(CLASS_AutoApplicationSettings) as IRecorder2000;
    lThesaurus := lApp.RequestCOMData(
        Self as IRequestor,
        CLASS_frmThesaurusBrowser) as IfrmThesaurusBrowser;
    if Assigned(lThesaurus) then
    begin
      lThesaurus.DisplayConcept(lItem.ConceptKey);
    end;
  end;
end;

end.
