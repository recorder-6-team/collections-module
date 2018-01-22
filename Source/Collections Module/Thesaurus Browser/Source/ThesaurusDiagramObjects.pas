{===============================================================================
  Unit:        ThesaurusDiagramObjects

  Defines:     TBaseDiagramPersistantObject
               TBaseDiagramObject
               TDiagramConcept
               TDiagramRelationship
               TBaseDisplayProperties
               TConceptDisplayProperties
               TRelationshipDisplayProperties
               TDiagramConceptList
               TDiagramRelationshipList

  Description: Thesaurus diagramming display objects

  Created:     Dec 2003

  Model:       Thesaurus.mpb

  Last revision information:
    $Revision: 11 $
    $Date: 19/01/05 14:06 $
    $Author: Johnvanbreda $

===============================================================================}
unit ThesaurusDiagramObjects;

interface

uses
  SysUtils, Windows, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, XMLDoc, XMLIntf, ExceptionForm, ADODb, DiagramObjects;

type
  EThesaurusDiagramObjectsException = class(EDiagramObjectsException)
  end;
  
  {--------------------------------------------------------------------------
  ---
    Container for the properties of a concept.
  }
  TConceptDisplayProperties = class(TBoxDisplayProperties)
    function GetXMLNodeName: string; override;
  end;
  
  {--------------------------------------------------------------------------
  ---
    Container for the properties of a relationship.
  }
  TRelationshipDisplayProperties = class(TBaseDisplayProperties)
  private
    FParentLabelSettings: Boolean;
    FShowForwardTerm: Boolean;
    FShowMainTerm: Boolean;
    FShowReverseTerm: Boolean;
    function GetShowForwardTerm: Boolean;
    function GetShowMainTerm: Boolean;
    function GetShowReverseTerm: Boolean;
    procedure SetParentLabelSettings(Value: Boolean);
    procedure SetShowForwardTerm(Value: Boolean);
    procedure SetShowMainTerm(Value: Boolean);
    procedure SetShowReverseTerm(Value: Boolean);
  protected
    function GetDefaultFontSize: Integer; override;
  public
    constructor Create; overload;
    function HasOverride: Boolean; override;
    procedure ReadXML(AXMLNode: IXMLNode); override;
    procedure WriteXML(AXMLNode: IXMLNode); override;
    property ParentLabelSettings: Boolean read FParentLabelSettings write
        SetParentLabelSettings;
    property ShowForwardTerm: Boolean read GetShowForwardTerm write
        SetShowForwardTerm;
    property ShowMainTerm: Boolean read GetShowMainTerm write SetShowMainTerm;
    property ShowReverseTerm: Boolean read GetShowReverseTerm write
        SetShowReverseTerm;
  end;
  
  {--------------------------------------------------------------------------
  ---
    Base class for objects that are drawn onto diagrams
  }
  TBaseDiagramObject = class(TBaseDiagramPersistantObject)
  private
    FOwner: TObject;
    FSelected: Boolean;
    procedure SetSelected(Value: Boolean);
  protected
    FCaption: string;
  public
    constructor Create(AOwner: TObject); reintroduce; virtual;
    function HitTest(AX, AY: integer): Boolean; virtual; abstract;
    procedure Paint(ACanvas: TCanvas); virtual; abstract;
    property Caption: string read FCaption;
    property Owner: TObject read FOwner;
    property Selected: Boolean read FSelected write SetSelected;
  end;
  
  {--------------------------------------------------------------------------
  ---
    Class for concepts drawn onto diagrams
  }
  TDiagramConcept = class(TBaseDiagramObject)
  private
    FCaption: string;
    FConceptDisplayProperties: TConceptDisplayProperties;
    FConceptGroup: TObject;
    FConceptKey: string;
    FHeight: Integer;
    FLastCanvas: TCanvas;
    FLastFont: TFont;
    FLeft: Integer;
    FObjectID: Integer;
    FTop: Integer;
    FWidth: Integer;
    function FontsAreEqual(AFont1, AFont2: TFont): Boolean;
    procedure GetLabels;
    procedure SetConceptGroup(Value: TObject);
    procedure SetConceptKey(const Value: string);
    procedure SetHeight(Value: Integer);
    procedure SetLeft(Value: Integer);
    procedure SetObjectID(Value: Integer);
    procedure SetTop(Value: Integer);
    procedure SetWidth(Value: Integer);
  public
    constructor Create(AOwner: TObject); overload; override;
    destructor Destroy; override;
    function HitTest(AX, AY: integer): Boolean; override;
    procedure Paint(ACanvas: TCanvas); override;
    procedure ReadXML(AXMLNode: IXMLNode); override;
    procedure UpdateSize(ACanvas: TCanvas; AForceUpdate: boolean);
    procedure WriteXML(AXMLNode: IXMLNode); override;
    property Caption: string read FCaption;
    property ConceptDisplayProperties: TConceptDisplayProperties read
        FConceptDisplayProperties;
    property ConceptGroup: TObject read FConceptGroup write SetConceptGroup;
    property ConceptKey: string read FConceptKey write SetConceptKey;
    property Height: Integer read FHeight write SetHeight;
    property Left: Integer read FLeft write SetLeft;
    property ObjectID: Integer read FObjectID write SetObjectID;
    property Top: Integer read FTop write SetTop;
    property Width: Integer read FWidth write SetWidth;
  end;
  
  {--------------------------------------------------------------------------
  ---
    Class for relationships drawn onto diagrams
  }
  TDiagramRelationship = class(TBaseDiagramObject)
  private
    FFromConcept: TDiagramConcept;
    FFromConceptForwardLabels: TStringList;
    FFromConceptReverseLabels: TStringList;
    FMainTermLabels: TStringList;
    FRelationshipDisplayProperties: TRelationshipDisplayProperties;
    FRelationType: TObject;
    FToConcept: TDiagramConcept;
    FToConceptForwardLabels: TStringList;
    FToConceptReverseLabels: TStringList;
    function BoundingBoxTest(AX, AY: integer): Boolean;
    procedure DrawDirectionTermLabels(ACanvas: TCanvas);
    procedure DrawMainTermLabels(ACanvas: TCanvas);
    procedure DrawTermLabelBlock(ACanvas: TCanvas; ALabelList: TStringList;
        AForward, AFromEnd: boolean);
    function GetOctant: Integer;
    procedure InitialiseObjects;
    procedure SetRelationType(Value: TObject);
  public
    constructor Create(AOwner: TObject; const AFromConcept, AToConcept:
        TDiagramConcept); reintroduce; overload;
    destructor Destroy; override;
    procedure GetLabelsFromData(ADataset: TCustomADODataset; AReverse: boolean);
    function HitTest(AX, AY: integer): Boolean; override;
    procedure Paint(ACanvas: TCanvas); override;
    procedure ReadXML(AXMLNode: IXMLNode); override;
    procedure WriteXML(AXMLNode: IXMLNode); override;
    function XPosEnd: Integer;
    function XPosStart: Integer;
    function YPosEnd: Integer;
    function YPosStart: Integer;
    property FromConcept: TDiagramConcept read FFromConcept;
    property FromConceptForwardLabels: TStringList read
        FFromConceptForwardLabels;
    property FromConceptReverseLabels: TStringList read
        FFromConceptReverseLabels;
    property MainTermLabels: TStringList read FMainTermLabels;
    property RelationshipDisplayProperties: TRelationshipDisplayProperties read
        FRelationshipDisplayProperties;
    property RelationType: TObject read FRelationType write SetRelationType;
    property ToConcept: TDiagramConcept read FToConcept;
    property ToConceptForwardLabels: TStringList read FToConceptForwardLabels;
    property ToConceptReverseLabels: TStringList read FToConceptReverseLabels;
  end;
  
  TThesaurusDiagramDisplayProperties = class(TDiagramDisplayProperties)
  private
    FConceptDisplayProperties: TConceptDisplayProperties;
    FRelationshipDisplayProperties: TRelationshipDisplayProperties;
  public
    constructor Create(ADiagram: TObject); overload;
    destructor Destroy; override;
    procedure ReadXML(AXMLNode: IXMLNode); override;
    procedure WriteXML(AXMLNode: IXMLNode); override;
    property ConceptDisplayProperties: TConceptDisplayProperties read
        FConceptDisplayProperties;
    property RelationshipDisplayProperties: TRelationshipDisplayProperties read
        FRelationshipDisplayProperties;
  end;
  

implementation

uses
  ResourceStrings, ThesaurusDiagram, GeneralData, ApplicationSettings,
  GeneralFunctions, InterfaceDataModule, DiagramXMLConstants,
  ThesaurusDiagramObjectLists;

const
  BOX_SPACING = 5;

{-==============================================================================
    TRelationshipDisplayProperties
===============================================================================}
{-------------------------------------------------------------------------------
}
constructor TRelationshipDisplayProperties.Create;
begin
  inherited Create;
  
  // Set defaults
  ShowMainTerm := True;
  ShowForwardTerm := True;
  ShowReverseTerm := True;
end;  // TRelationshipDisplayProperties.Create 

{-------------------------------------------------------------------------------
  Retrieve the default font size for this type of object 
}
function TRelationshipDisplayProperties.GetDefaultFontSize: Integer;
begin
  Result := 7;
end;  // TRelationshipDisplayProperties.GetDefaultFontSize 

{-------------------------------------------------------------------------------
}
function TRelationshipDisplayProperties.GetShowForwardTerm: Boolean;
begin
  if FParentLabelSettings then
    Result := TRelationshipDisplayProperties(Parent).ShowForwardTerm
  else
    Result := FShowForwardTerm;
end;  // TRelationshipDisplayProperties.GetShowForwardTerm 

{-------------------------------------------------------------------------------
}
function TRelationshipDisplayProperties.GetShowMainTerm: Boolean;
begin
  if FParentLabelSettings then
    Result := TRelationshipDisplayProperties(Parent).ShowMainTerm
  else
    Result := FShowMainTerm;
end;  // TRelationshipDisplayProperties.GetShowMainTerm 

{-------------------------------------------------------------------------------
}
function TRelationshipDisplayProperties.GetShowReverseTerm: Boolean;
begin
  if FParentLabelSettings then
    Result := TRelationshipDisplayProperties(Parent).ShowReverseTerm
  else
    Result := FShowReverseTerm;
end;  // TRelationshipDisplayProperties.GetShowReverseTerm 

{-------------------------------------------------------------------------------
}
function TRelationshipDisplayProperties.HasOverride: Boolean;
begin
  Result := inherited HasOverride or (not ParentLabelSettings);
end;  // TRelationshipDisplayProperties.HasOverride 

{-------------------------------------------------------------------------------
  Initialise object from XML 
}
procedure TRelationshipDisplayProperties.ReadXML(AXMLNode: IXMLNode);
begin
  ReadBaseDisplayPropsXML(AXMLNode);
  // Read the label details as this only applies to a relationship
  FParentLabelSettings := not HasChildNode(AXMLNode, EL_LABELS);
  if not FParentLabelSettings then
    with AXMLNode.ChildNodes[EL_LABELS] do begin
      ShowMainTerm := HasAttribute(AT_MAINTERM);
      ShowForwardTerm := HasAttribute(AT_FORWARDTERM);
      ShowReverseTerm := HasAttribute(AT_REVERSETERM);
    end;
end;  // TRelationshipDisplayProperties.ReadXML 

{-------------------------------------------------------------------------------
}
procedure TRelationshipDisplayProperties.SetParentLabelSettings(Value: Boolean);
begin
  if Value and not Assigned(Parent) then
    raise EThesaurusDiagramObjectsException.Create(Format(
        ResStr_InvalidMethodCall,
        ['TRelationshipDisplayProperties.SetParentLabelSettings']));
  
  FParentLabelSettings := Value;
end;  // TRelationshipDisplayProperties.SetParentLabelSettings 

{-------------------------------------------------------------------------------
}
procedure TRelationshipDisplayProperties.SetShowForwardTerm(Value: Boolean);
begin
  FShowForwardTerm := Value;
end;  // TRelationshipDisplayProperties.SetShowForwardTerm 

{-------------------------------------------------------------------------------
}
procedure TRelationshipDisplayProperties.SetShowMainTerm(Value: Boolean);
begin
  FShowMainTerm := Value;
end;  // TRelationshipDisplayProperties.SetShowMainTerm 

{-------------------------------------------------------------------------------
}
procedure TRelationshipDisplayProperties.SetShowReverseTerm(Value: Boolean);
begin
  FShowReverseTerm := Value;
end;  // TRelationshipDisplayProperties.SetShowReverseTerm 

{-------------------------------------------------------------------------------
  Output the XML for the relationship display settings. 
}
procedure TRelationshipDisplayProperties.WriteXML(AXMLNode: IXMLNode);
var
  lPropertiesNode: IXMLNode;
begin
  lPropertiesNode := AXMLNode.AddChild(EL_RELATIONSHIPDISPLAY);
  // write the stuff that is common to all display properties
  WriteBaseDisplayPropsXML(lPropertiesNode);
  // write the Relationship specific stuff
  if not FParentLabelSettings then
    with lPropertiesNode.AddChild(EL_LABELS) do begin
      if ShowMainTerm then
        Attributes[AT_MAINTERM] := 1;
      if ShowForwardTerm then
        Attributes[AT_FORWARDTERM] := 1;
      if ShowReverseTerm then
        Attributes[AT_REVERSETERM] := 1;
    end;
end;  // TRelationshipDisplayProperties.WriteXML 

{-==============================================================================
    TBaseDiagramObject
===============================================================================}
{-------------------------------------------------------------------------------
  Initialise object. 
}
constructor TBaseDiagramObject.Create(AOwner: TObject);
begin
  inherited Create;
  // Ensure owner is a diagram
  if not (AOwner is TThesaurusDiagram) then
    raise EThesaurusDiagramObjectsException.Create(Format(
        ResStr_InvalidMethodCall,
        ['TBaseDiagramObject.Create']));
  FOwner := AOwner;
end;  // TBaseDiagramObject.Create 

{-------------------------------------------------------------------------------
}
procedure TBaseDiagramObject.SetSelected(Value: Boolean);
begin
  if FSelected <> Value then
  begin
  
    // Force a repaint to reflect new state
    TThesaurusDiagram(FOwner).Invalidate;
  
    FSelected := Value;

  
  end;
end;  // TBaseDiagramObject.SetSelected 

{-==============================================================================
    TDiagramConcept
===============================================================================}
{-------------------------------------------------------------------------------
  Initialise object 
}
constructor TDiagramConcept.Create(AOwner: TObject);
begin
  inherited Create(AOwner);
  
  FConceptDisplayProperties := TConceptDisplayProperties.Create;
  FLastFont := TFont.Create;
end;  // TDiagramConcept.Create 

{-------------------------------------------------------------------------------
}
destructor TDiagramConcept.Destroy;
begin
  FConceptDisplayProperties.Free;
  FLastFont.Free;
  
  inherited Destroy;
end;  // TDiagramConcept.Destroy 

{-------------------------------------------------------------------------------
  Performs a deep comparison of 2 font objects and returns trus if they are the
      same.
}
function TDiagramConcept.FontsAreEqual(AFont1, AFont2: TFont): Boolean;
begin
  Result := (AFont1.Charset=AFont2.Charset)
      and (AFont1.Color=AFont2.Color)
      and (AFont1.Name=AFont2.Name)
      and (AFont1.Size=AFont2.Size)
      and (AFont1.Style=AFont2.Style)
end;  // TDiagramConcept.FontsAreEqual 

{-------------------------------------------------------------------------------
  Obtains the caption for the concept. 
}
procedure TDiagramConcept.GetLabels;
begin
  FCaption := dmGeneral.GetStoredProcOutputParam('usp_ConceptItemName_Get',
      ['@Key', FConceptKey,
      '@IncludeCommonName', AppSettings.DisplayCommonNames,
      '@IncludeAuthor', 1,
      '@Formatted', 1], '@ItemName');
end;  // TDiagramConcept.GetLabels 

{-------------------------------------------------------------------------------
  Check if a mouse click is on the box. 
}
function TDiagramConcept.HitTest(AX, AY: integer): Boolean;
begin
  Result := (AX>=Left) and (AX<=Left+Width)
      and (AY>=Top) and (AY<=Top+Height);
end;  // TDiagramConcept.HitTest 

{-------------------------------------------------------------------------------
}
procedure TDiagramConcept.Paint(ACanvas: TCanvas);
var
  lRect: TRect;
begin
  with ACanvas do begin
    // Set display properties
    Brush.Assign(FConceptDisplayProperties.Brush);
    Pen.Assign(FConceptDisplayProperties.Pen);
    Font.Assign(FConceptDisplayProperties.Font);
    // Ensure box size is correct
    UpdateSize(ACanvas, False);
    // Draw the box
    if Selected then begin
      // Change colour a bit if focused
      Brush.Color := MergeColours(Brush.Color, clHighlight, 80);
      Font.Color := GetContrastColour(Brush.Color);
    end;
    Rectangle(Left, Top, Left + Width, Top + Height);
    if Selected then
      DrawFocusRect(Rect(Left, Top, Left + Width, Top + Height));
    // Get text rectangle allowing for space round edge
    lRect := Rect(Left + BOX_SPACING, Top + BOX_SPACING,
        Left + Width - BOX_SPACING, Top + Height - BOX_SPACING);
    // Output the text
    dmInterface.DrawWrappedTerm(ACanvas, lRect, FCaption, False, False);
  end; // with
end;  // TDiagramConcept.Paint 

{-------------------------------------------------------------------------------
}
procedure TDiagramConcept.ReadXML(AXMLNode: IXMLNode);
begin
  ConceptKey := AXMLNode.Attributes[AT_CONCEPTKEY];
  ObjectID := AXMLNode.Attributes[AT_OBJECTID];
  FLeft := AXMLNode.Attributes[AT_LEFT];
  FTop := AXMLNode.Attributes[AT_TOP];
  if HasChildNode(AXMLNode, EL_CONCEPTDISPLAY) then
    FConceptDisplayProperties.ReadXML(AXMLNode.ChildNodes[EL_CONCEPTDISPLAY]);
  // read the concept relationships and place in correct concept group
  TThesaurusDiagram(Owner).InitialiseConcept(Self);
end;  // TDiagramConcept.ReadXML 

{-------------------------------------------------------------------------------
  Accessor method 
}
procedure TDiagramConcept.SetConceptGroup(Value: TObject);
begin
  if not (Value is TDiagramConceptGroup) then
    raise EThesaurusDiagramObjectsException.Create(Format(
        ResStr_InvalidMethodCall,
        ['TDiagramConcept.SetConceptGroup']));
  
  FConceptGroup := Value;
end;  // TDiagramConcept.SetConceptGroup 

{-------------------------------------------------------------------------------
  Accessor method.  Retrieves the concept's label. 
}
procedure TDiagramConcept.SetConceptKey(const Value: string);
begin
  FConceptKey := Value;
  
  GetLabels;
end;  // TDiagramConcept.SetConceptKey 

{-------------------------------------------------------------------------------
  Accessor method 
}
procedure TDiagramConcept.SetHeight(Value: Integer);
begin
  with TThesaurusDiagram(FOwner).DiagramDisplayProperties do
    // snap to grid always rounding up.
    if SnapToGrid then
      FHeight := ((Value + GridSize-1) div GridSize) * GridSize
    else
      FHeight := Value;
end;  // TDiagramConcept.SetHeight 

{-------------------------------------------------------------------------------
  Accessor method 
}
procedure TDiagramConcept.SetLeft(Value: Integer);
begin
  with TThesaurusDiagram(FOwner).DiagramDisplayProperties do
    // snap to nearest
    if SnapToGrid then
      FLeft := ((Value + GridSize div 2) div GridSize) * GridSize
    else
      FLeft := Value;
end;  // TDiagramConcept.SetLeft 

{-------------------------------------------------------------------------------
  Accessor method. 
}
procedure TDiagramConcept.SetObjectID(Value: Integer);
begin
  FObjectID := Value;
end;  // TDiagramConcept.SetObjectID 

{-------------------------------------------------------------------------------
  Accessor method 
}
procedure TDiagramConcept.SetTop(Value: Integer);
begin
  with TThesaurusDiagram(FOwner).DiagramDisplayProperties do
    // snap to nearest
    if SnapToGrid then
      FTop := ((Value + GridSize div 2) div GridSize) * GridSize
    else
      FTop := Value;
end;  // TDiagramConcept.SetTop 

{-------------------------------------------------------------------------------
  Accessor method 
}
procedure TDiagramConcept.SetWidth(Value: Integer);
begin
  with TThesaurusDiagram(FOwner).DiagramDisplayProperties do
    // snap to grid always rounding up.
    if SnapToGrid then
      FWidth := ((Value + GridSize-1) div GridSize) * GridSize
    else
      FWidth := Value;
end;  // TDiagramConcept.SetWidth 

{-------------------------------------------------------------------------------
  Updates the size of the concept box after a change which might affect this,
      such as a change to fonts.  If AForceUpdate is false, then only updates
      if the canvas has changed since the last paint.
}
procedure TDiagramConcept.UpdateSize(ACanvas: TCanvas; AForceUpdate: boolean);
var
  lRect: TRect;
begin
  lRect := Rect(Left, Top, Left + 100, Top);
  if (ACanvas <> FLastCanvas) or AForceUpdate or
      (not FontsAreEqual(FConceptDisplayProperties.Font, FLastFont)) then begin
    // Ensure correct font used
    ACanvas.Font.Assign(FConceptDisplayProperties.Font);
    FLastCanvas := ACanvas;
    // Use the draw code to measure the box, without actually drawing anything
    dmInterface.DrawWrappedTerm(ACanvas, lRect, FCaption, False, True);
    Width := lRect.Right - lRect.Left + 2 * BOX_SPACING;
    Height := lRect.Bottom - lRect.Top + 2 * BOX_SPACING;
    FLastFont.Assign(FConceptDisplayProperties.Font);
  end;
end;  // TDiagramConcept.UpdateSize 

{-------------------------------------------------------------------------------
  Outputs the details to the XML file.  The Node passed in should be the
      Concepts node.
}
procedure TDiagramConcept.WriteXML(AXMLNode: IXMLNode);
var
  lConceptNode: IXMLNode;
begin
  lConceptNode := AXMLNode.AddChild(EL_CONCEPT);
  with lConceptNode do begin
    Attributes[AT_CONCEPTKEY] := FConceptKey;
    Attributes[AT_LEFT]:= FLeft;
    Attributes[AT_TOP] := FTop;
    Attributes[AT_OBJECTID] := ObjectID;
  end;
  // Output the properties if required
  if FConceptDisplayProperties.HasOverride then
    FConceptDisplayProperties.WriteXML(lConceptNode);
end;  // TDiagramConcept.WriteXML 

{-==============================================================================
    TDiagramRelationship
===============================================================================}
{-------------------------------------------------------------------------------
  Object Initialisation 
}
constructor TDiagramRelationship.Create(AOwner: TObject; const AFromConcept,
    AToConcept: TDiagramConcept);
begin
  inherited Create(AOwner);
  FFromConcept := AFromConcept;
  FToConcept := AToConcept;
  FRelationshipDisplayProperties := TRelationshipDisplayProperties.Create;
  InitialiseObjects;
  FCaption := FFromConcept.Caption + ' -> ' + FToConcept.Caption;
end;  // TDiagramRelationship.Create 

{-------------------------------------------------------------------------------
  Object finalisation 
}
destructor TDiagramRelationship.Destroy;
begin
  FMainTermLabels.Free;
  FFromConceptForwardLabels.Free;
  FFromConceptReverseLabels.Free;
  FToConceptForwardLabels.Free;
  FToConceptReverseLabels.Free;
  FRelationshipDisplayProperties.Free;
  
  inherited Destroy;
end;  // TDiagramRelationship.Destroy 

{-------------------------------------------------------------------------------
  Checks if the clicked point falls in the total bounding box occupied by the
      relationship.  Used as a first quick check before doing an accurate hit
      test.  A 5 pixel leeway is allowed for incase the line is horizontal.
}
function TDiagramRelationship.BoundingBoxTest(AX, AY: integer): Boolean;
  
  const
    ACCURACY=5;
  
begin
  Result :=
      (((AX > XPosStart - ACCURACY) and
        (AX < XPosEnd + ACCURACY)) or
       ((AX < XPosStart + ACCURACY) and
        (AX > XPosEnd - ACCURACY))) and
      (((AY > YPosStart - ACCURACY) and
        (AY < YPosEnd + ACCURACY)) or
       ((AY < YPosStart + ACCURACY) and
        (AY > YPosEnd - ACCURACY)));
end;  // TDiagramRelationship.BoundingBoxTest 

{-------------------------------------------------------------------------------
  Draws the directional term labels, if required. 
}
procedure TDiagramRelationship.DrawDirectionTermLabels(ACanvas: TCanvas);
begin
  if FRelationshipDisplayProperties.ShowForwardTerm then
  begin
    DrawTermLabelBlock(ACanvas, FFromConceptForwardLabels, True, True);
    DrawTermLabelBlock(ACanvas, FToConceptForwardLabels, True, False);
  end;
  if FRelationshipDisplayProperties.ShowReverseTerm then
  begin
    DrawTermLabelBlock(ACanvas, FFromConceptReverseLabels, False, True);
    DrawTermLabelBlock(ACanvas, FToConceptReverseLabels, False, False);
  end;
end;  // TDiagramRelationship.DrawDirectionTermLabels 

{-------------------------------------------------------------------------------
  Draws each of the main term labels, centred over the relationship.  Ignored
      if properties specify not to do so.
}
procedure TDiagramRelationship.DrawMainTermLabels(ACanvas: TCanvas);
var
  lTopMidPoint: TPoint;
  lIdx: Integer;
begin
  if FRelationshipDisplayProperties.ShowMainTerm then begin
    // find line centre
    lTopMidPoint := Point((XPosStart + XPosEnd) div 2, (YPosStart + YPosEnd)
        div 2);
    // shift label block up to account for number of rows
    lTopMidPoint.Y := lTopMidPoint.Y - (ACanvas.TextHeight('A') *
        FMainTermLabels.Count) div 2;
    for lIdx := 0 to FMainTermLabels.Count-1 do
      ACanvas.TextOut(
          lTopMidPoint.X - ACanvas.TextWidth(FMainTermLabels[lIdx]) div 2,
          lTopMidPoint.Y + lIdx * ACanvas.TextHeight('A'),
          FMainTermLabels[lIdx]);
  end;
end;  // TDiagramRelationship.DrawMainTermLabels 

{-------------------------------------------------------------------------------
  Draws a single block of directional term labels (e.g. the From concept's
      Forward terms).  If AFromEnd is true then the labels are placed near the
      From concept, otherwise they are near the To concept.  If AForward is
      true, then labels are above the line, otherwise they are below.
}
procedure TDiagramRelationship.DrawTermLabelBlock(ACanvas: TCanvas; ALabelList:
    TStringList; AForward, AFromEnd: boolean);
var
  lDistanceAlongLine: Double;
  lRect: TRect;
  lAnchorPoint: TPoint;
  lAngle: Double;
begin
  if ALabelList.Count>0 then begin
    // Get point 1/4 or 3/4 along line according to which end the labels go at
    if AFromEnd then
      lDistanceAlongLine := 0.25
    else
      lDistanceAlongLine := 0.75;
    ACanvas.Font.Assign(FRelationshipDisplayProperties.Font);
    lRect := Rect(0,0,0,0);
    // Calculate the box size
    dmInterface.DrawWrappedTerm(ACanvas, lRect, ALabelList.Text, False, True);
    // Find a point centred on the line, roughly where we want the label
    lAnchorPoint := Point(
        Trunc(XPosStart + (XPosEnd-XPosStart)*lDistanceAlongLine),
        Trunc(YPosStart + (YPosEnd-YPosStart)*lDistanceAlongLine));
    if (XPosEnd-XPosStart)<>0 then
      lAngle := arctan((YPosEnd-YPosStart)/(XPosEnd-XPosStart))
    else
      if (YPosEnd<YPosStart) then
        lAngle := 0.5*Pi
      else
        lAngle := 1.5*Pi;
    if AForward then begin
      lAnchorPoint.X := Trunc(lAnchorPoint.X + ((lRect.Right-lRect.Left) div
          2)*
          sin(lAngle)*1.1)-(lRect.Right-lRect.Left) div 2;
      lAnchorPoint.Y := Trunc(lAnchorPoint.Y - ((lRect.Bottom-lRect.Top) div
          2)*
          cos(lAngle)*1.1)-(lRect.Bottom-lRect.Top) div 2;
    end
    else begin
      lAnchorPoint.X := Trunc(lAnchorPoint.X - ((lRect.Right-lRect.Left) div
          2)*
          sin(lAngle)*1.1)-(lRect.Right-lRect.Left) div 2;
      lAnchorPoint.Y := Trunc(lAnchorPoint.Y + ((lRect.Bottom-lRect.Top) div
          2)*
          cos(lAngle)*1.1)-(lRect.Bottom-lRect.Top) div 2;
    end;
    lRect := Rect(lAnchorPoint.X, lAnchorPoint.Y, lAnchorPoint.X+(
        lRect.Right-lRect.Left),
        lAnchorPoint.Y+(lRect.Bottom-lRect.Top));
    dmInterface.DrawWrappedTerm(ACanvas, lRect, ALabelList.Text, False,
        False);
  end;
end;  // TDiagramRelationship.DrawTermLabelBlock 

{-------------------------------------------------------------------------------
}
procedure TDiagramRelationship.GetLabelsFromData(ADataset: TCustomADODataset;
    AReverse: boolean);
begin
  with ADataset do begin
    MainTermLabels.Add(FieldByName('Item_Name').AsString);
    // Direction field indicates that labels are reversed.
    // AReverse indicates entire relationship is reversed.
    if not AReverse then begin
      if FieldByName('Direction').AsString='Forward' then begin
        FromConceptForwardLabels.Add(FieldByName('Forward_Term').AsString);
        ToConceptReverseLabels.Add(FieldByName('Reverse_Term').AsString);
      end
      else begin
        FromConceptForwardLabels.Add(FieldByName('Reverse_Term').AsString);
        ToConceptReverseLabels.Add(FieldByName('Forward_Term').AsString);
      end;
    end
    else begin
      if FieldByName('Direction').AsString='Forward' then begin
        FromConceptReverseLabels.Add(FieldByName('Forward_Term').AsString);
        ToConceptForwardLabels.Add(FieldByName('Reverse_Term').AsString);
      end
      else begin
        FromConceptReverseLabels.Add(FieldByName('Reverse_Term').AsString);
        ToConceptForwardLabels.Add(FieldByName('Forward_Term').AsString);
      end;
    end;
  end; // with
end;  // TDiagramRelationship.GetLabelsFromData 

{-------------------------------------------------------------------------------
  Retrieves the octant the line angle is in.  1 = WNW, 2 = NNW, 3 = NNE, 4 =
      ENE, 5=ESE, 6=SSE, 7=SSW, 8=WSW.
}
function TDiagramRelationship.GetOctant: Integer;
var
  lEast: Boolean;
  lSouth: Boolean;
  lVerticalComponentGreater: Boolean;
begin
  // Is there an easterly component to the line angle
  lEast := (XPosStart <= XPosEnd);
  // Is there a southerly component to the line angle
  lSouth := (YPosStart <= YPosEnd);
  // Which is greater?
  lVerticalComponentGreater := Abs(YPosStart-YPosEnd)>Abs(XPosStart-XPosEnd);
  if (not lEast) and (not lSouth) and (not lVerticalComponentGreater) then
      Result := 1
  else if (not lEast) and (not lSouth) and lVerticalComponentGreater then
      Result := 2
  else if lEast and (not lSouth) and lVerticalComponentGreater then Result := 3
  else if lEast and (not lSouth) and (not lVerticalComponentGreater) then
      Result := 4
  else if lEast and lSouth and (not lVerticalComponentGreater) then Result := 5
  else if lEast and lSouth and lVerticalComponentGreater then Result := 6
  else if (not lEast) and lSouth and lVerticalComponentGreater then Result := 7
  else Result := 8;
end;  // TDiagramRelationship.GetOctant 

{-------------------------------------------------------------------------------
  Test if the line is clicked on.  Works by first checking if the point is
      anywhere near the line, then calculates if a circle 5 pixels around the
      clicked point overlaps the line.
}
function TDiagramRelationship.HitTest(AX, AY: integer): Boolean;
var
  lXOffset: Integer;
  lOctant: Integer;
  lYOffset: Integer;
  lExpectedOffset: Integer;
  
  const
    ACCURACY = 5;
  
begin
  Result := False; // default
  if BoundingBoxTest(AX, AY) then begin
    lOctant := GetOctant;
    // calculate the actual offset in each axis
    lXOffset := AX - XPosStart;
    lYOffset := AY - YPosStart;
    // do the measrurement the most accurate way round
    case lOctant of
      2, 3, 6, 7: begin
        // line points mostly north
        lExpectedOffset := Trunc((XPosEnd - XPosStart) * (lYOffset / (YPosEnd -
            YPosStart)));
        Result := Abs(lExpectedOffset-lXOffset)<ACCURACY;
      end;
      4, 5, 8, 1: begin
        // line points mostly east or west
        // calculate the expected offset in the other axis
        lExpectedOffset := Trunc((YPosEnd - YPosStart) * (lXOffset / (XPosEnd -
            XPosStart)));
        Result := Abs(lExpectedOffset-lYOffset)<ACCURACY;
      end;
    end; // case
  end;
end;  // TDiagramRelationship.HitTest 

{-------------------------------------------------------------------------------
  Initialise the owned objects 
}
procedure TDiagramRelationship.InitialiseObjects;
begin
  // String lists to hold the labels
  FMainTermLabels := TStringList.Create;
  FFromConceptForwardLabels := TStringList.Create;
  FFromConceptReverseLabels := TStringList.Create;
  FToConceptForwardLabels := TStringList.Create;
  FToConceptReverseLabels := TStringList.Create;
end;  // TDiagramRelationship.InitialiseObjects 

{-------------------------------------------------------------------------------
  Draw a line from the centre of one concept to the centre of the other.  Then,
      add the labels.
}
procedure TDiagramRelationship.Paint(ACanvas: TCanvas);
  
  procedure DrawLine;
  begin
    with TThesaurusDiagram(Owner) do begin
      ACanvas.MoveTo(XPosStart, YPosStart);
      ACanvas.LineTo(XPosEnd, YPosEnd);
    end;
  end;
  
begin
  // Draw an extra weighted line if selected
  if FSelected then begin
    ACanvas.Pen.Color := clHighlight;
    ACanvas.Pen.Width := RelationshipDisplayProperties.Pen.Width + 2;
    DrawLine;
  end;
  // Draw the normal line
  ACanvas.Pen.Assign(RelationshipDisplayProperties.Pen);
  ACanvas.Font.Assign(RelationshipDisplayProperties.Font);
  DrawLine;
  // Draw the labels
  DrawMainTermLabels(ACanvas);
  DrawDirectionTermLabels(ACanvas);
end;  // TDiagramRelationship.Paint 

{-------------------------------------------------------------------------------
  Object initialisation from an XML node. 
}
procedure TDiagramRelationship.ReadXML(AXMLNode: IXMLNode);
begin
  FFromConcept := TThesaurusDiagram(Owner).FindConceptByID(
      AXMLNode.Attributes[AT_FROMOBJECTID]);
  FToConcept := TThesaurusDiagram(Owner).FindConceptByID(
      AXMLNode.Attributes[AT_TOOBJECTID]);
  if HasChildNode(AXMLNode, EL_RELATIONSHIPDISPLAY) then
    FRelationshipDisplayProperties.ReadXML(
        AXMLNode.ChildNodes[EL_RELATIONSHIPDISPLAY]);
end;  // TDiagramRelationship.ReadXML 

{-------------------------------------------------------------------------------
  Accessor method.  Note data is stored as a TObject to avoid circular
      references.
}
procedure TDiagramRelationship.SetRelationType(Value: TObject);
begin
  if not (Value is TDiagramRelationType) then
    raise EThesaurusDiagramObjectsException.Create(Format(
        ResStr_InvalidMethodCall,
        ['TDiagramRelationship.SetRelationType']));
  { TODO : Set up a macro to handle invalid method calls }
  
  FRelationType := Value;
end;  // TDiagramRelationship.SetRelationType 

{-------------------------------------------------------------------------------
}
procedure TDiagramRelationship.WriteXML(AXMLNode: IXMLNode);
var
  lRelationshipNode: IXMLNode;
begin
  lRelationshipNode := AXMLNode.AddChild(EL_RELATIONSHIP);
  with lRelationshipNode do begin
    Attributes[AT_FROMOBJECTID] := FromConcept.ObjectID;
    Attributes[AT_TOOBJECTID] := ToConcept.ObjectID;
  end;
  // Write out the properties if required
  if FRelationshipDisplayProperties.HasOverride then
    FRelationshipDisplayProperties.WriteXML(lRelationshipNode);
end;  // TDiagramRelationship.WriteXML 

{-------------------------------------------------------------------------------
  Retrieves the X Pos of the end of the line 
}
function TDiagramRelationship.XPosEnd: Integer;
begin
  Result := FToConcept.Left + FToConcept.Width div 2;
end;  // TDiagramRelationship.XPosEnd 

{-------------------------------------------------------------------------------
  Retrieves the X Pos of the start of the line 
}
function TDiagramRelationship.XPosStart: Integer;
begin
  Result := FFromConcept.Left + FFromConcept.Width div 2;
end;  // TDiagramRelationship.XPosStart 

{-------------------------------------------------------------------------------
  Retrieves the Y Pos of the end of the line 
}
function TDiagramRelationship.YPosEnd: Integer;
begin
  Result := FToConcept.Top + FToConcept.Height div 2;
end;  // TDiagramRelationship.YPosEnd 

{-------------------------------------------------------------------------------
  Retrieves the Y Pos of the start of the line 
}
function TDiagramRelationship.YPosStart: Integer;
begin
  Result := FFromConcept.Top + FFromConcept.Height div 2;
end;  // TDiagramRelationship.YPosStart 

{-==============================================================================
    TThesaurusDiagramDisplayProperties
===============================================================================}
{-------------------------------------------------------------------------------
}
constructor TThesaurusDiagramDisplayProperties.Create(ADiagram: TObject);
begin
  inherited Create(ADiagram);
  
  FConceptDisplayProperties := TConceptDisplayProperties.Create;
  FRelationshipDisplayProperties := TRelationshipDisplayProperties.Create;
end;  // TThesaurusDiagramDisplayProperties.Create 

{-------------------------------------------------------------------------------
}
destructor TThesaurusDiagramDisplayProperties.Destroy;
begin
  FConceptDisplayProperties.Free;
  FRelationshipDisplayProperties.Free;
  
  inherited Destroy;
end;  // TThesaurusDiagramDisplayProperties.Destroy 

{-------------------------------------------------------------------------------
}
procedure TThesaurusDiagramDisplayProperties.ReadXML(AXMLNode: IXMLNode);
begin
  inherited ReadXML(AXMLNode);
  
  with AXMLNode do begin
    if HasChildNode(AXMLNode, EL_CONCEPTDISPLAY) then
      FConceptDisplayProperties.ReadXML(ChildNodes[EL_CONCEPTDISPLAY]);
    if HasChildNode(AXMLNode, EL_RELATIONSHIPDISPLAY) then
      FRelationshipDisplayProperties.ReadXML(
          ChildNodes[EL_RELATIONSHIPDISPLAY]);
  end;
end;  // TThesaurusDiagramDisplayProperties.ReadXML 

{-------------------------------------------------------------------------------
}
procedure TThesaurusDiagramDisplayProperties.WriteXML(AXMLNode: IXMLNode);
var
  lNode: IXMLNode;
begin
  inherited WriteXML(AXMLNode);

  // default properties for all concepts and relationships
  if HasChildNode(AXMLNode, EL_DIAGRAMDISPLAY) then begin
    lNode := AXMLNode.ChildNodes[EL_DIAGRAMDISPLAY];
    ConceptDisplayProperties.WriteXml(lNode);
    RelationshipDisplayProperties.WriteXml(lNode);
  end;
end;  // TThesaurusDiagramDisplayProperties.WriteXML 


{ TConceptDisplayProperties }

function TConceptDisplayProperties.GetXMLNodeName: string;
begin
  Result := EL_CONCEPTDISPLAY;
end;

end.
