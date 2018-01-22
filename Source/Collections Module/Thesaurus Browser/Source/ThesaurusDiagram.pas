{===============================================================================
  Unit:        ThesaurusDiagram

  Defines:     TThesaurusDiagram

  Description: Thesaurus diagramming panel

  Created:     Nov 2003
in
  Model:       Thesaurus.mpb

  Last revision information:
    $Revision: 16 $
    $Date: 29/11/05 9:26 $
    $Author: Johnvanbreda $

===============================================================================}
unit ThesaurusDiagram;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, Menus, DiagramPreview, DataClasses, ExceptionForm,
  ThesaurusDiagramObjects, ThesaurusDiagramObjectLists, ADODb,
  XMLDoc, XMLIntf, DataTypes, Diagram;

resourcestring
  ResStr_ConceptProperties = 'Concept Properties';
  ResStr_RelationshipProperties =  'Relationship Properties';
  ResStr_ShowDiagramPreview = 'Show Diagram Preview';
  ResStr_AddInvalidConceptToDiagram =
      'An invalid concept key %s was added to the diagram';
  ResStr_DiagramFileFilters = 'Thesaurus Diagrams (*.tdg)|*.tdg';
  ResStr_ConceptNotFound = 'Concept not found on diagram';

type
  TOnSelectConceptEvent = procedure (Sender: TObject; const AConceptKey:
      string) of object;
  EThesaurusDiagramException = class(TExceptionPath)
  end;
  
  {--------------------------------------------------------------------------
  ---
    Class implementing a panel used to display thesaurus diagrams.  The
    following funcitonality is supported:
    Concepts dropped onto the diagram are represented as boxes with the Term.
    Item_Name displayed in the box.  When a concept is dropped, any
    relationships between this concept and others already on the diagram are
    already created.
    Relationships are displayed as a line between the centre point of 2
    related concepts.
    Clicking a concept or a relationship selects the item.  Selected
    concepts are show with a focus rectangle, selected relationships are
    shown with a dotted line alongside them on both sides.  Concepts are
    moved by dragging them with the mouse, in which case the relevant
    relationship positions are updated.
    Relationships have the main relationship label adjacent to the centre
    point of the line.  The forward relationship label appears near the
    concept at the 'From' end of the relationship, slightly above and to the
    right of the line.  The reverse relationship label appears near the
    concept at the 'To' end of the relationship, slightly below and to the
    left of the line.  All relationship labels are configured or disabled
    using the properties dialogs.  If more than one relationship exists
    between 2 concepts, then the labels are stacked above each other.
    The diagram has a title.  The title is configurable using
    TdlgDiagramSettings.
    The user configures the display properties of the objects on the diagram
    and the diagram itself using a right-click popup menu.  The following
    options are present:
    Concept Properties - displays TdlgDiagramConceptPropertiesManager
    Relationship Properties - displays
    TdlgDiagramRelationshipPropertiesManager
    Diagram Settings - displays TdlgDiagramSettings
    Show Diagram Preview - displays frmDiagramPreview.  This option's
    checked state is toggled to reflect the visibility of frmDiagramPreview.
  }
  TThesaurusDiagram = class(TDiagram, IAdditionalProperties)
  private
    FDiagramConceptGroupList: TDiagramConceptGroupList;
    FDiagramConceptList: TDiagramConceptList;
    FDiagramPreview: TfrmDiagramPreview;
    FDiagramRelationshipList: TDiagramRelationshipList;
    FDiagramRelationTypeList: TDiagramRelationTypeList;
    FOnSelectConcept: TOnSelectConceptEvent;
    FRelationshipSettings: IXMLNode;
    FRelationTypeSettings: IXMLNode;
    FSelectedConcept: TDiagramConcept;
    FSelectedRelationship: TDiagramRelationship;
    FXMLDoc: IXMLDocument;
    procedure AssignToConceptGroup(AConcept: TDiagramConcept);
    procedure ConceptPropsClick(Sender: TObject);
    function CreateConceptGroup(const AKey: string): TDiagramConceptGroup;
    procedure DrawConcepts(ACanvas: TCanvas);
    procedure DrawRelationships(ACanvas: TCanvas);
    function GetProperty(const AName: string): Variant;
    procedure GetRelationshipCallback(ATarget: TObject; ADataset:
        TCustomADODataset);
    procedure GetRelationshipsForConcept(const AConcept: TDiagramConcept);
    function GetThesaurusDiagramDisplayProperties:
        TThesaurusDiagramDisplayProperties;
    procedure InternalSave(const AFileName: string);
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    procedure RelationshipPropsClick(Sender: TObject);
    procedure SetOnSelectConcept(Value: TOnSelectConceptEvent);
    procedure SetSelectedConcept(Value: TDiagramConcept);
    procedure ShowDiagramPreviewClick(Sender: TObject);
    procedure WriteXMLToFile(const AFileName: string);
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    function GetNewObjectID: integer;
  protected
    procedure BeginDragOperation(AX, AY: integer); override;
    procedure CreatePopupMenu; override;
    procedure DoKeyDown(var Key: Word; Shift: TShiftState); override;
    procedure DoMouseDown(Button: TMouseButton; Shift: TShiftState; X, Y:
        Integer); override;
    procedure DoMouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure DoPaint; override;
    procedure InitialiseObjects; override;
  public
    destructor Destroy; override;
    procedure AddConcept(AConceptKey: string; AX, AY: integer);
    procedure AssignToRelationType(ARelationship: TDiagramRelationship; const
        ATypeKey, ATypeName: string);
    procedure DeleteSelectedConcept;
    procedure DropConcept(const Sender: TObject; const iFormat : integer; const
        iSourceData: TKeyList; const iTextStrings : TstringList; var ioHandled
        : boolean);
    function FindConceptByID(AID: integer): TDiagramConcept;
    procedure InitialiseConcept(AConcept: TDiagramConcept);
    function PreviewPopupItem: TMenuItem;
    procedure ReadSettings(ARelationship: TDiagramRelationship); overload;
    procedure ReadSettings(AType: TDiagramRelationType); overload;
    procedure ReadXML(const AFileName: string);
    procedure Save;
    procedure SaveAs;
    property DiagramConceptGroupList: TDiagramConceptGroupList read
        FDiagramConceptGroupList;
    property DiagramConceptList: TDiagramConceptList read FDiagramConceptList
        write FDiagramConceptList;
    property DiagramRelationshipList: TDiagramRelationshipList read
        FDiagramRelationshipList;
    property DiagramRelationTypeList: TDiagramRelationTypeList read
        FDiagramRelationTypeList;
    property OnSelectConcept: TOnSelectConceptEvent read FOnSelectConcept write
        SetOnSelectConcept;
    property SelectedConcept: TDiagramConcept read FSelectedConcept write
        SetSelectedConcept;
    property SelectedRelationship: TDiagramRelationship read
        FSelectedRelationship;
    property ThesaurusDiagramDisplayProperties:
        TThesaurusDiagramDisplayProperties read
        GetThesaurusDiagramDisplayProperties;
  end;
  

implementation

uses
  DiagramRelationshipPropertiesManager, DiagramConceptPropertiesManager,
  GeneralData, ResourceStrings, DiagramConceptProperties,
  BaseDiagramObjectProperties, DiagramConceptPropertiesInherited,
  DiagramXMLConstants, GeneralFunctions, LuxembourgConstants;

{-==============================================================================
    TThesaurusDiagram
===============================================================================}
{-------------------------------------------------------------------------------
}
destructor TThesaurusDiagram.Destroy;
begin
  dmGeneral.CancelAsyncCommands(GetRelationshipCallback);
  FDiagramConceptGroupList.Free;
  FDiagramRelationTypeList.Free;
  FDiagramPreview.Free;
  FDiagramConceptList.Free;
  FDiagramRelationshipList.Free;
  
  inherited Destroy;
end;  // TThesaurusDiagram.Destroy 

{-------------------------------------------------------------------------------
  Adds a concept to the diagram. 
}
procedure TThesaurusDiagram.AddConcept(AConceptKey: string; AX, AY: integer);
var
  lNewConcept: TDiagramConcept;
begin
  lNewConcept := TDiagramConcept.Create(Self);
  with lNewConcept do begin
    ConceptKey := AConceptKey;
    Left := AX - 50;
    Top  := AY - 15;
    ObjectID := GetNewObjectID;
    UpdateSize(Canvas, True);
  end;
  InitialiseConcept(lNewConcept);
  lNewConcept.ConceptDisplayProperties.ParentPen := True;
  lNewConcept.ConceptDisplayProperties.ParentBrush := True;
  lNewConcept.ConceptDisplayProperties.ParentFont := True;
end;  // TThesaurusDiagram.AddConcept

{-------------------------------------------------------------------------------
  Return the next highest object ID
}
function TThesaurusDiagram.GetNewObjectID: integer;
begin
  if FDiagramConceptList.Count>0 then
    Result := FDiagramConceptList.Items[FDiagramConceptList.Count-1].ObjectID+1
  else
    Result := 1;
end;

{-------------------------------------------------------------------------------
  Places the newly added concept in the appropriate concept group.  The concept
      group is created if required.
  The object passed in to this method is automatically freed if the concept is
      invalid.
}
procedure TThesaurusDiagram.AssignToConceptGroup(AConcept: TDiagramConcept);
var
  lIdx: Integer;
  lConceptGroupKey: string;
  lConceptGroup: TDiagramConceptGroup;
begin
  // find the concept group key
  with dmGeneral.GetRecordset('usp_concept_select', ['@ConceptKey',
      AConcept.ConceptKey]) do
    if not EOF then
      lConceptGroupKey := Fields['Concept_Group_Key'].Value
    else begin
      AConcept.Free;
      raise EThesaurusDiagramException.Create(Format(
          ResStr_AddInvalidConceptToDiagram, [AConcept.ConceptKey]));
    end;
  lConceptGroup := nil;
  // scan for existing group
  for lIdx := 0 to FDiagramConceptGroupList.Count-1 do begin
    if FDiagramConceptGroupList.Items[lIdx].ConceptGroupKey=lConceptGroupKey
        then begin
      lConceptGroup := FDiagramConceptGroupList.Items[lIdx];
      Break;
    end; // if
  end; // for
  if not Assigned(lConceptGroup) then
    // no existing group, so create one
    lConceptGroup := CreateConceptGroup(lConceptGroupKey);
  AConcept.ConceptGroup := lConceptGroup;
  AConcept.ConceptDisplayProperties.Parent := lConceptGroup.DisplayProperties;
end;  // TThesaurusDiagram.AssignToConceptGroup 

{-------------------------------------------------------------------------------
  Places the newly added relationship in the appropriate relationship type
      group.  The relationship type is created if required.
}
procedure TThesaurusDiagram.AssignToRelationType(ARelationship:
    TDiagramRelationship; const ATypeKey, ATypeName: string);
var
  lRelationType: TDiagramRelationType;
begin
  // Find the existing appropriate type, add it to the list if new
  lRelationType := FDiagramRelationTypeList.FindRelationType(ATypeKey,
      ATypeName);
  ARelationship.RelationType := lRelationType;
  ARelationship.RelationshipDisplayProperties.Parent :=
      lRelationType.DisplayProperties;
  ARelationship.RelationshipDisplayProperties.ParentLabelSettings := True;
  ARelationship.RelationshipDisplayProperties.ParentFont := True;
  ARelationship.RelationshipDisplayProperties.ParentPen := True;
end;  // TThesaurusDiagram.AssignToRelationType 

{-------------------------------------------------------------------------------
  Records the starting position of a drag operation on the title or a concept. 
}
procedure TThesaurusDiagram.BeginDragOperation(AX, AY: integer);
begin
  inherited;
  if not DiagramDisplayProperties.TitleSelected then
    FMouseDownItemStartPosition := Point(FSelectedConcept.Left,
      FSelectedConcept.Top);
end;  // TThesaurusDiagram.BeginDragOperation 

{-------------------------------------------------------------------------------
  Displays the Concept Properties Manager dialog. 
}
procedure TThesaurusDiagram.ConceptPropsClick(Sender: TObject);
begin
  with TdlgDiagramConceptPropertiesManager.Create(self) do
    try
      ShowModal;
    finally
      Free;
    end; //
end;  // TThesaurusDiagram.ConceptPropsClick 

{-------------------------------------------------------------------------------
  Creates a concept group for the diagram, sets its parent properties to the
      diagram concept display properties and adds it to the internal lists.
}
function TThesaurusDiagram.CreateConceptGroup(const AKey: string):
    TDiagramConceptGroup;
begin
  Result := TDiagramConceptGroup.Create;
  Result.ConceptGroupKey := AKey;
  Result.DisplayProperties.Parent :=
      ThesaurusDiagramDisplayProperties.ConceptDisplayProperties;
  Result.DisplayProperties.ParentPen := True;
  Result.DisplayProperties.ParentBrush := True;
  Result.DisplayProperties.ParentFont := True;
  FDiagramConceptGroupList.Add(Result);
end;  // TThesaurusDiagram.CreateConceptGroup 

{-------------------------------------------------------------------------------
  Creates the popup menu that appears on the diagram. 
}
procedure TThesaurusDiagram.CreatePopupMenu;
  
  procedure AddMenuItem(const ACaption: string; AMethod: TNotifyEvent);
  var
    lNewItem: TMenuItem;
  begin
    lNewItem := TMenuItem.Create(Self);
    lNewItem.Caption := ACaption;
    lNewItem.OnClick := AMethod;
    PopupMenu.Items.Add(lNewItem);
  end;
  
begin
  Inherited;
  AddMenuItem(ResStr_ConceptProperties, ConceptPropsClick);
  AddMenuItem(ResStr_RelationshipProperties, RelationshipPropsClick);
  AddMenuItem(ResStr_ShowDiagramPreview, ShowDiagramPreviewClick);
end;  // TThesaurusDiagram.CreatePopupMenu 

{-------------------------------------------------------------------------------
  Handle Delete key presses that remove a concept from the diagram. 
}
procedure TThesaurusDiagram.DeleteSelectedConcept;
var
  lIdx: Integer;
  lConceptToDelete: TDiagramConcept;
begin
  lConceptToDelete := FSelectedConcept;
  if Assigned(lConceptToDelete) then begin
    // remove any linked relationships
    with DiagramRelationshipList do
      for lIdx := Count-1 downto 0  do
        if (Items[lIdx].FromConcept=lConceptToDelete) or
            (Items[lIdx].ToConcept=lConceptToDelete) then
          FreeAndDelete(lIdx);
    // and remove the concept
    SelectedConcept := nil;
    DiagramConceptList.FreeAndDelete(DiagramConceptList.IndexOf(
        lConceptToDelete));
    Invalidate;
  end;
end;  // TThesaurusDiagram.DeleteSelectedConcept 

{-------------------------------------------------------------------------------
}
procedure TThesaurusDiagram.DoKeyDown(var Key: Word; Shift: TShiftState);
begin
  if (Key=VK_DELETE) then
    DeleteSelectedConcept;
end;  // TThesaurusDiagram.DoKeyDown 

{-------------------------------------------------------------------------------
}
procedure TThesaurusDiagram.DoMouseDown(Button: TMouseButton; Shift:
    TShiftState; X, Y: Integer);
var
  lIdx: Integer;
  lSelectionFound: Boolean;
begin
  lSelectionFound := False; // default
  PopupMenu.Items[1].Visible := False;
  PopupMenu.Items[2].Visible := False;
  // Look for a clicked on concept
  with DiagramConceptList do
    for lIdx := 0 to Count-1 do begin
      if not lSelectionFound then
        Items[lIdx].Selected := Items[lIdx].HitTest(X, Y)
      else
        Items[lIdx].Selected := False;
      if Items[lIdx].Selected then begin
        lSelectionFound := True;
        SelectedConcept := Items[lIdx];
        PopupMenu.Items[1].Visible := True;
      end;
    end;
  if not lSelectionFound then begin
    SelectedConcept := nil;
    // Look for a clicked on relationship if no selection found
    with DiagramRelationshipList do
      for lIdx := 0 to Count-1 do begin
        Items[lIdx].Selected := Items[lIdx].HitTest(X, Y) and (not
            lSelectionFound);
        if Items[lIdx].Selected then begin
          lSelectionFound := True;
          FSelectedRelationship := Items[lIdx];
          PopupMenu.Items[2].Visible := True;
        end; // for
      end; // for
  end;
  if not lSelectionFound then
    DiagramDisplayProperties.TitleSelected := DiagramDisplayProperties.
        TitleHitTest(X, Y)
  else
    DiagramDisplayProperties.TitleSelected := False;
  if Assigned(FSelectedConcept) or DiagramDisplayProperties.TitleSelected then
    BeginDragOperation(X, Y);
  RequestFocus;
  { TODO : Need to implement double click on diagram. }
end;  // TThesaurusDiagram.DoMouseDown 

{-------------------------------------------------------------------------------
  Track drag operations if the mouse moves over the control. 
}
procedure TThesaurusDiagram.DoMouseMove(Shift: TShiftState; X, Y: Integer);
begin
  if (ssLeft in Shift) then begin
    if (Abs(X-FMouseDownPoint.X)>4) or (Abs(Y-FMouseDownPoint.Y)>4) then begin
      if Assigned(FSelectedConcept) then begin
        FSelectedConcept.Left := FMouseDownItemStartPosition.X +
            X - FMouseDownPoint.X;
        FSelectedConcept.Top := FMouseDownItemStartPosition.Y +
            Y - FMouseDownPoint.Y;
        // Ensure Concept stays inside the diagram.
        FSelectedConcept.Left := Min(FDiagram.Width-FSelectedConcept.Width,
            FSelectedConcept.Left);
        FSelectedConcept.Left := Max(0, FSelectedConcept.Left);
        FSelectedConcept.Top := Min(FDiagram.Height-FSelectedConcept.Height,
            FSelectedConcept.Top);
        FSelectedConcept.Top := Max(0, FSelectedConcept.Top);
        Invalidate;
      end
      else
        inherited;
    end;
  end;
end;  // TThesaurusDiagram.DoMouseMove 

{-------------------------------------------------------------------------------
}
procedure TThesaurusDiagram.DoPaint;
begin
  inherited;
  DrawRelationships(Canvas);
  DrawConcepts(Canvas);
  // redraw the preview
  if Assigned(FDiagramPreview) then
    FDiagramPreview.Invalidate;
end;  // TThesaurusDiagram.DoPaint 

{-------------------------------------------------------------------------------
  Draws the concepts onto the diagram.  Use the supplied canvas to allow
      concepts to be drawn to the printer or a bitmap.
}
procedure TThesaurusDiagram.DrawConcepts(ACanvas: TCanvas);
var
  lIdx: Integer;
begin
  // for each concept, call the paint method
  with DiagramConceptList do
    for lIdx := 0 to Count-1 do
      Items[lIdx].Paint(ACanvas);
end;  // TThesaurusDiagram.DrawConcepts 

{-------------------------------------------------------------------------------
  Draws the relationships onto the diagram.  Use the supplied canvas to allow
      relationships to be drawn to the printer or a bitmap.
}
procedure TThesaurusDiagram.DrawRelationships(ACanvas: TCanvas);
var
  lIdx: Integer;
begin
  // for each relationship call the paint method
  with FDiagramRelationshipList do
    for lIdx := 0 to Count-1 do
      Items[lIdx].Paint(ACanvas);
end;  // TThesaurusDiagram.DrawRelationships 

{-------------------------------------------------------------------------------
  Drop event handler.  Public, so that when new diagrams are created they can
      be registered.
}
procedure TThesaurusDiagram.DropConcept(const Sender: TObject; const iFormat :
    integer; const iSourceData: TKeyList; const iTextStrings : TstringList; var
    ioHandled : boolean);
var
  lIdx: Integer;
  lPoint: TPoint;
begin
  // If multiple concepts dropped, use the following as an offset
  lPoint := FDiagram.ScreenToClient(Mouse.CursorPos);
  for lIdx := 0 to iSourceData.Header.ItemCount-1 do begin
    AddConcept(iSourceData.Items[lIdx].KeyField1, lPoint.X, lPoint.Y);
    Inc(lPoint.X, DiagramDisplayProperties.GridSize);
    Inc(lPoint.Y, DiagramDisplayProperties.GridSize);
  end;
end;  // TThesaurusDiagram.DropConcept 

{-------------------------------------------------------------------------------
  Find a concept using the diagram object ID.  Raises an exception if not
      found.
}
function TThesaurusDiagram.FindConceptByID(AID: integer): TDiagramConcept;
var
  lIdx: Integer;
begin
  Result := nil;
  with DiagramConceptList do
    for lIdx := 0 to Count-1 do
      if Items[lIdx].ObjectId=AID then
        Result := Items[lIdx];
  if not Assigned(Result) then
    raise EThesaurusDiagramException.Create(ResStr_ConceptNotFound);
end;  // TThesaurusDiagram.FindConceptByID 

{-------------------------------------------------------------------------------
}
function TThesaurusDiagram.GetProperty(const AName: string): Variant;
begin
  if AName = PROP_KEY then begin
    if Assigned(SelectedConcept) then
      Result := SelectedConcept.ConceptKey
    else
      Result := '';
  end
  else
    Result := Unassigned;
end;  // TThesaurusDiagram.GetProperty 

{-------------------------------------------------------------------------------
  Callback when scanning for relationships.
  Creates a relationship between the target concept and the one supplied in the
      recordset if the recordset contains any records.  If the recordset
      contains more than one record, then either multiple labels are attached
      to the relationship (if it points to a single concept), or multiple
      relationships are created.
}
procedure TThesaurusDiagram.GetRelationshipCallback(ATarget: TObject; ADataset:
    TCustomADODataset);
var
  lNewRelation: TDiagramRelationship;
  lToConcept: TDiagramConcept;
  lIdx: Integer;
  lReverse: Boolean;
begin
  lNewRelation := nil;
  with ADataset do begin
    First;
    while not EOF do begin
      for lIdx := 0 to DiagramConceptList.Count-1 do begin
        if DiagramConceptList.Items[lIdx].ConceptKey=
            FieldByName('To_Concept_Key').AsString then begin
          lToConcept := DiagramConceptList.Items[lIdx];
          lNewRelation := DiagramRelationshipList.FindRelationship(
              TDiagramConcept(ATarget).ObjectId, lToConcept.ObjectId,
              FieldByName('Thesaurus_Relation_Type_Key').AsString,
                  FieldByName('Item_Name').AsString,
              lReverse);
          lNewRelation.GetLabelsFromData(ADataset, lReverse);
        end; // if
      end; // for
      Next;
    end; // while
    if Assigned(lNewRelation) then
      Invalidate;
  end;
end;  // TThesaurusDiagram.GetRelationshipCallback 

{-------------------------------------------------------------------------------
  Retrieves all relationships between the newly added concept and any other
      concept on the diagram.
}
procedure TThesaurusDiagram.GetRelationshipsForConcept(const AConcept:
    TDiagramConcept);
var
  lFromKey: string;
  lToKeys: string;
  lIdx: Integer;
begin
  if not (AConcept is TDiagramConcept) then
    raise EThesaurusDiagramException.Create(Format(ResStr_InvalidMethodCall,
      ['TThesaurusDiagram.GetRelationshipsForConcept']));
  if DiagramConceptList.Count>0 then begin
    lFromKey := TDiagramConcept(AConcept).ConceptKey;
    lToKeys := '';
    // retrieve all the concepts in the entire diagram
    with DiagramConceptList do
      for lIdx := 0 to Count-1 do
        if Items[lIdx].ConceptKey <> lFromKey then begin
          if lToKeys<>'' then
            ltoKeys := lToKeys + '\';
          lToKeys := lToKeys + Items[lIdx].ConceptKey;
        end;
    dmGeneral.GetAsyncRecordset('usp_ConceptToConceptRelations_Select', [
        '@FromKey', lFromKey,
        '@ToKeys', lToKeys,
        '@IncludeInherited', 0],
        AConcept,
        GetRelationshipCallback);
    dmGeneral.GetAsyncRecordset('usp_ConceptToConceptRelations_Select', [
        '@FromKey', lFromKey,
        '@ToKeys', lToKeys,
        '@IncludeInherited', 1],
        AConcept,
        GetRelationshipCallback);
  end;
end;  // TThesaurusDiagram.GetRelationshipsForConcept 

{-------------------------------------------------------------------------------
  Accessor method to DiagramDisplayProperties which saves a type cast. 
}
function TThesaurusDiagram.GetThesaurusDiagramDisplayProperties:
    TThesaurusDiagramDisplayProperties;
begin
  Result := TThesaurusDiagramDisplayProperties(DiagramDisplayProperties);
end;  // TThesaurusDiagram.GetThesaurusDiagramDisplayProperties 

{-------------------------------------------------------------------------------
  Places a concept in the correct concept group and reads its relationships. 
}
procedure TThesaurusDiagram.InitialiseConcept(AConcept: TDiagramConcept);
begin
  GetRelationshipsForConcept(AConcept);
  AssignToConceptGroup(AConcept);
  DiagramConceptList.Add(AConcept);
  Invalidate;
end;  // TThesaurusDiagram.InitialiseConcept 

{-------------------------------------------------------------------------------
  Initialise owned objects 
}
procedure TThesaurusDiagram.InitialiseObjects;
begin
  inherited;
  FDiagramPreview := TfrmDiagramPreview.Create(self);
  FDiagramConceptGroupList := TDiagramConceptGroupList.Create(self);
  FDiagramConceptList := TDiagramConceptList.Create(self);
  FDiagramRelationTypeList := TDiagramRelationTypeList.Create(self);
  FDiagramRelationshipList := TDiagramRelationshipList.Create(self);
  FDiagramDisplayProperties := TThesaurusDiagramDisplayProperties.Create(self);
end;  // TThesaurusDiagram.InitialiseObjects 

{-------------------------------------------------------------------------------
  Performs the saving of a diagram to a specified file.  Remembers the file for
      the next save operation.
}
procedure TThesaurusDiagram.InternalSave(const AFileName: string);
begin
  FFileName := AFileName;
  WriteXMLToFile(FFileName);
end;  // TThesaurusDiagram.InternalSave 

{-------------------------------------------------------------------------------
  Retrieve the diagram preview popup menu item. 
}
function TThesaurusDiagram.PreviewPopupItem: TMenuItem;
begin
  Result := PopupMenu.Items[3];
end;  // TThesaurusDiagram.PreviewPopupItem 

{-------------------------------------------------------------------------------
}
function TThesaurusDiagram.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if GetInterface(IID, Obj) then
    Result := 0
  else
    Result := E_NOINTERFACE;
end;  // TThesaurusDiagram.QueryInterface 

{-------------------------------------------------------------------------------
  Retrieve the settings for a relationship from the loaded XML file settings (
      if the diagram was loaded).
}
procedure TThesaurusDiagram.ReadSettings(ARelationship: TDiagramRelationship);
var
  lIdx: Integer;
begin
  if Assigned(FRelationshipSettings) then begin
    for lIdx := 0 to FRelationshipSettings.ChildNodes.Count-1 do
      with FRelationshipSettings.ChildNodes[lIdx] do
        if NodeName=EL_RELATIONSHIP then begin
          if (Attributes[AT_FROMOBJECTID] = ARelationship.FromConcept.ObjectID)
              and (Attributes[AT_TOOBJECTID] = ARelationship.ToConcept.ObjectID)
              and HasChildNode(FRelationshipSettings.ChildNodes[lIdx],
              EL_RELATIONSHIPDISPLAY) then
            ARelationship.ReadXML(FRelationshipSettings.ChildNodes[lIdx]);
        end;
  end;
end;  // TThesaurusDiagram.ReadSettings 

{-------------------------------------------------------------------------------
  Retrieve the settings for a relationship type from the loaded XML file
      settings (if the diagram was loaded).
}
procedure TThesaurusDiagram.ReadSettings(AType: TDiagramRelationType);
var
  lIdx: Integer;
begin
  if Assigned(FRelationTypeSettings) then begin
    for lIdx := 0 to FRelationTypeSettings.ChildNodes.Count-1 do
      with FRelationTypeSettings.ChildNodes[lIdx] do
        if NodeName=EL_RELATIONTYPE then begin
          if (Attributes[AT_RELATIONTYPEKEY] = AType.RelationTypeKey) and
              HasChildNode(FRelationTypeSettings.ChildNodes[lIdx],
              EL_RELATIONSHIPDISPLAY) then
            AType.ReadXML(FRelationTypeSettings.ChildNodes[lIdx]);
        end;
  end;
end;  // TThesaurusDiagram.ReadSettings 

{-------------------------------------------------------------------------------
  Read the diagram from a supplied XML file 
}
procedure TThesaurusDiagram.ReadXML(const AFileName: string);
var
  lCursor: TCursor;
begin
  dmGeneral.Recorder.RecorderMainForm.StartProgressBar;
  lCursor := HourglassCursor;
  try
    FXMLDoc := TXMLDocument.Create(AFileName);
    with FXMLDoc do begin
      if HasChildNode(DocumentElement, EL_DIAGRAMDISPLAY) then
        FDiagramDisplayProperties.ReadXML(
            DocumentElement.ChildNodes[EL_DIAGRAMDISPLAY]);
      if HasChildNode(DocumentElement, EL_CONCEPTGROUPS) then
        FDiagramConceptGroupList.ReadXML(
            DocumentElement.ChildNodes[EL_CONCEPTGROUPS]);
      // Read the settings for relationship types and relations, so if they are still
      // loaded on the diagram, they can pick up the correct settings
      if HasChildNode(DocumentElement, EL_RELATIONTYPES) then
        FRelationTypeSettings :=
            ChildNodes[EL_THESAURUSDIAGRAM].ChildNodes[EL_RELATIONTYPES];
      if HasChildNode(DocumentElement, EL_RELATIONSHIPS) then
        FRelationshipSettings :=
            ChildNodes[EL_THESAURUSDIAGRAM].ChildNodes[EL_RELATIONSHIPS];
      if HasChildNode(DocumentElement, EL_CONCEPTS) then
        FDiagramConceptList.ReadXML(
            DocumentElement.ChildNodes[EL_CONCEPTS]);
    end; // with
    // Store the file name for Save operations
    FFileName := AFileName;
  finally
    dmGeneral.Recorder.RecorderMainForm.StatusText := '';
    dmGeneral.Recorder.RecorderMainForm.Progress := 0;
    dmGeneral.Recorder.RecorderMainForm.StopProgressBar;
    DefaultCursor(lCursor);
  end;
end;  // TThesaurusDiagram.ReadXML 

{-------------------------------------------------------------------------------
  Displays the Relationship Properties Manager 
}
procedure TThesaurusDiagram.RelationshipPropsClick(Sender: TObject);
begin
  with TdlgDiagramRelationshipPropertiesManager.Create(self) do
    try
      ShowModal;
    finally
      Free;
    end; //
end;  // TThesaurusDiagram.RelationshipPropsClick 

{-------------------------------------------------------------------------------
  Saves the diagram to the previously specified file.  If no file previously
      specified, then invokes SaveAs instead.
}
procedure TThesaurusDiagram.Save;
begin
  if FFileName='' then
    SaveAs
  else
    InternalSave(FFileName);
end;  // TThesaurusDiagram.Save 

{-------------------------------------------------------------------------------
  Saves the diagram to a new file name. 
}
procedure TThesaurusDiagram.SaveAs;
begin
  with TSaveDialog.Create(nil) do
    try
      Filter := ResStr_DiagramFileFilters;
      if Execute then begin
        if ExtractFileExt(FileName)='' then
          FileName := FileName + '.tdg';
        InternalSave(FileName);
      end;
    finally
      Free;
    end;
end;  // TThesaurusDiagram.SaveAs 

{-------------------------------------------------------------------------------
  Accessor method for event. 
}
procedure TThesaurusDiagram.SetOnSelectConcept(Value: TOnSelectConceptEvent);
begin
  FOnSelectConcept := Value;
end;  // TThesaurusDiagram.SetOnSelectConcept 

{-------------------------------------------------------------------------------
  Accessor method.  Change the currently selected concept. 
}
procedure TThesaurusDiagram.SetSelectedConcept(Value: TDiagramConcept);
begin
  if FSelectedConcept <> Value then
  begin
  
    if Assigned(FSelectedConcept) then
      FSelectedConcept.Selected := False;
    if Assigned(Value) then
      Value.Selected := True;
    Invalidate;
  
    FSelectedConcept := Value;
  
    // raise event
    if Assigned(FSelectedConcept) and Assigned(FOnSelectConcept) then
      FOnSelectConcept(Self, FSelectedConcept.ConceptKey)
    else if Assigned(FOnSelectConcept) then
      FOnSelectConcept(Self, '');
  
  end;
end;  // TThesaurusDiagram.SetSelectedConcept 

{-------------------------------------------------------------------------------
  Toggles the visibility of the diagram preview popup window. 
}
procedure TThesaurusDiagram.ShowDiagramPreviewClick(Sender: TObject);
begin
  PopupMenu.Items[3].Checked := Not PopupMenu.Items[3].Checked;
  FDiagramPreview.Visible := PopupMenu.Items[3].Checked;
end;  // TThesaurusDiagram.ShowDiagramPreviewClick 

{-------------------------------------------------------------------------------
  Creates the XML documentation to persist the diagram to a file.  The file is
      saved to the supplied file name.
}
procedure TThesaurusDiagram.WriteXMLToFile(const AFileName: string);
var
  lXMLDoc: IXMLDocument;
begin
  lXMLDoc := NewXMLDocument;
  // root node is always 'thesaurus_diagram'
  lXMLDoc.AddChild(EL_THESAURUSDIAGRAM);
  DiagramDisplayProperties.WriteXML(lXMLDoc.DocumentElement);
  FDiagramConceptGroupList.WriteXML(lXMLDoc.DocumentElement);
  FDiagramConceptList.WriteXML(lXMLDoc.DocumentElement);
  FDiagramRelationTypeList.WriteXML(lXMLDoc.DocumentElement);
  FDiagramRelationshipList.WriteXML(lXMLDoc.DocumentElement);
  lXMLDoc.SaveToFile(AFileName);
end;  // TThesaurusDiagram.WriteXMLToFile 

{-------------------------------------------------------------------------------
}
function TThesaurusDiagram._AddRef: Integer;
begin
  Result := -1;
end;  // TThesaurusDiagram._AddRef 

{-------------------------------------------------------------------------------
}
function TThesaurusDiagram._Release: Integer;
begin
  Result := -1;
end;  // TThesaurusDiagram._Release 


end.

