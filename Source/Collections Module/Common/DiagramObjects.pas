unit DiagramObjects;

interface

uses
  SysUtils, Windows, Messages, Classes, Graphics, Controls, Forms, Dialogs,
  XMLIntf, XMLDoc, DiagramXMLConstants, ExceptionForm;

resourcestring
  ResStr_NewDiagram = 'New Diagram';

type
  EDiagramObjectsException = class(TExceptionPath)
  end;
  
  {--------------------------------------------------------------------------
  ---
    Base class for objects that can be loaded and saved from a diagram.
  }
  TBaseDiagramPersistantObject = class(TObject)
  protected
    procedure ReadFontFromXML(AXMLNode: IXMLNode; AFont: TFont);
    procedure WriteFontToXML(AXMLNode: IXMLNode; AFont: TFont);
  public
    procedure ReadXML(AXMLNode: IXMLNode); virtual; abstract;
    procedure WriteXML(AXMLNode: IXMLNode); virtual; abstract;
  end;
  
  {--------------------------------------------------------------------------
  ---
    Base class for the containers for a set of object properties on the
    diagram.
  }
  TBaseDisplayProperties = class(TBaseDiagramPersistantObject)
  private
    FFont: TFont;
    FParent: TBaseDisplayProperties;
    FParentFont: Boolean;
    FParentPen: Boolean;
    FPen: TPen;
    function GetFont: TFont;
    function GetPen: TPen;
    procedure SetParent(Value: TBaseDisplayProperties);
    procedure SetParentFont(Value: Boolean);
    procedure SetParentPen(Value: Boolean);
  protected
    function GetDefaultFontSize: Integer; virtual; abstract;
    procedure ReadBaseDisplayPropsXML(AXMLNode: IXMLNode);
    procedure WriteBaseDisplayPropsXML(AXMLNode: IXMLNode);
  public
    constructor Create;
    destructor Destroy; override;
    function HasOverride: Boolean; virtual;
    property Font: TFont read GetFont;
    property Parent: TBaseDisplayProperties read FParent write SetParent;
    property ParentFont: Boolean read FParentFont write SetParentFont;
    property ParentPen: Boolean read FParentPen write SetParentPen;
    property Pen: TPen read GetPen;
  end;

  TBoxDisplayProperties = class(TBaseDisplayProperties)
  private
    FBrush: TBrush;
    FParentBrush: Boolean;
    function GetBrush: TBrush;
    procedure SetParentBrush(Value: Boolean);
  protected
    function GetDefaultFontSize: Integer; override;
    function GetXMLNodeName: string; virtual; abstract;
  public
    destructor Destroy; override;
    function HasOverride: Boolean; override;
    procedure ReadXML(AXMLNode: IXMLNode); override;
    procedure WriteXML(AXMLNode: IXMLNode); override;
    property Brush: TBrush read GetBrush;
    property ParentBrush: Boolean read FParentBrush write SetParentBrush;
  end;

  {--------------------------------------------------------------------------
  ---
    Container for the display properties that apply across a diagram.
  }
  TDiagramDisplayProperties = class(TBaseDiagramPersistantObject)
  private
    FColor: TColor;
    FDiagram: TObject;
    FGridSize: Integer;
    FShowGrid: Boolean;
    FShowTitle: Boolean;
    FSnapToGrid: Boolean;
    FTitle: string;
    FTitleFont: TFont;
    FTitleHeight: Integer;
    FTitleRect: TRect;
    FTitleSelected: Boolean;
    FTitleWidth: Integer;
    function GetTitlePosX: Integer;
    function GetTitlePosY: Integer;
    procedure InitialiseObjects(ADiagram: TObject);
    procedure SetColor(Value: TColor);
    procedure SetGridSize(Value: Integer);
    procedure SetShowGrid(Value: Boolean);
    procedure SetShowTitle(Value: Boolean);
    procedure SetSnapToGrid(Value: Boolean);
    procedure SetTitle(const Value: string);
    procedure SetTitleHeight(Value: Integer);
    procedure SetTitlePosX(Value: Integer);
    procedure SetTitlePosY(Value: Integer);
    procedure SetTitleSelected(Value: Boolean);
    procedure SetTitleWidth(Value: Integer);
  public
    constructor Create(ADiagram: TObject); reintroduce; overload;
    destructor Destroy; override;
    procedure ReadXML(AXMLNode: IXMLNode); override;
    function TitleHitTest(AX, AY: integer): Boolean;
    procedure WriteXML(AXMLNode: IXMLNode); override;
    property Color: TColor read FColor write SetColor;
    property Diagram: TObject read FDiagram;
    property GridSize: Integer read FGridSize write SetGridSize;
    property ShowGrid: Boolean read FShowGrid write SetShowGrid;
    property ShowTitle: Boolean read FShowTitle write SetShowTitle;
    property SnapToGrid: Boolean read FSnapToGrid write SetSnapToGrid;
    property Title: string read FTitle write SetTitle;
    property TitleFont: TFont read FTitleFont write FTitleFont;
    property TitleHeight: Integer read FTitleHeight write SetTitleHeight;
    property TitlePosX: Integer read GetTitlePosX write SetTitlePosX;
    property TitlePosY: Integer read GetTitlePosY write SetTitlePosY;
    property TitleSelected: Boolean read FTitleSelected write SetTitleSelected;
    property TitleWidth: Integer read FTitleWidth write SetTitleWidth;
  end;
  

implementation

uses
  ResourceStrings, Diagram, GeneralFunctions;

{-==============================================================================
    TBaseDiagramPersistantObject
===============================================================================}
{-------------------------------------------------------------------------------
  Reads the details of a font from an XML node.  The XML node must contain a
      child node called font.
}
procedure TBaseDiagramPersistantObject.ReadFontFromXML(AXMLNode: IXMLNode;
    AFont: TFont);
begin
  with AXMLNode.ChildNodes[EL_FONT] do begin
    AFont.Name := Attributes[AT_NAME];
    AFont.Size := Attributes[AT_SIZE];
    AFont.Style := [];
    if HasAttribute(AT_BOLD) then
      AFont.Style := AFont.Style + [fsBold];
    if HasAttribute(AT_ITALIC) then
      AFont.Style := AFont.Style + [fsItalic];
    if HasAttribute(AT_UNDERLINE) then
      AFont.Style := AFont.Style + [fsUnderline];
    if HasAttribute(AT_STRIKEOUT) then
      AFont.Style := AFont.Style + [fsStrikeout];
    AFont.Color := Attributes[AT_COLOUR];
    if HasAttribute(AT_CHARSET) then
      AFont.Charset := Attributes[AT_CHARSET];
  end;
end;  // TBaseDiagramPersistantObject.ReadFontFromXML 

{-------------------------------------------------------------------------------
  Writes the details of a font into an XML element. 
}
procedure TBaseDiagramPersistantObject.WriteFontToXML(AXMLNode: IXMLNode;
    AFont: TFont);
begin
  with AXMLNode.AddChild(EL_FONT) do begin
    Attributes[AT_NAME] := AFont.Name;
    Attributes[AT_SIZE] := AFont.Size;
    if fsBold in AFont.Style then
      Attributes[AT_BOLD]:=1;
    if fsItalic in AFont.Style then
      Attributes[AT_ITALIC]:=1;
    if fsUnderline in AFont.Style then
      Attributes[AT_UNDERLINE]:=1;
    if fsStrikeout in AFont.Style then
      Attributes[AT_STRIKEOUT]:=1;
    Attributes[AT_COLOUR] := AFont.Color;
    if AFont.Charset <> DEFAULT_CHARSET then
      Attributes[AT_CHARSET] := AFont.Charset;
  end; // with
end;  // TBaseDiagramPersistantObject.WriteFontToXML 

{-==============================================================================
    TBaseDisplayProperties
===============================================================================}
{-------------------------------------------------------------------------------
  Object initialisation 
}
constructor TBaseDisplayProperties.Create;
begin
  inherited Create;
end;  // TBaseDisplayProperties.Create 

{-------------------------------------------------------------------------------
  Object finalisation 
}
destructor TBaseDisplayProperties.Destroy;
begin
  FPen.Free;
  FFont.Free;
  
  inherited Destroy;
end;  // TBaseDisplayProperties.Destroy 

{-------------------------------------------------------------------------------
  Accessor method.  If ParentFont, then retreives the font from the next layer
      up.  If retrieving own font, then will instantiate the font and
      initialise it if required.
}
function TBaseDisplayProperties.GetFont: TFont;
begin
  if FParentFont then
    Result := FParent.Font
  else begin
    if not Assigned(FFont) then begin
      FFont := TFont.Create;
      // for a new font object, use the default settings
      if Assigned(FParent) then
        FFont.Assign(FParent.Font)
      else begin
        FFont.Name := 'Arial';
        FFont.Size := GetDefaultFontSize;
      end;
    end; // if
    Result := FFont;
  end;
end;  // TBaseDisplayProperties.GetFont 

{-------------------------------------------------------------------------------
  Accessor method.  If ParentPen, then retreives the pen from the next layer
      up.  If retrieving own pen, then will instantiate the pen and initialise
      it if required.
}
function TBaseDisplayProperties.GetPen: TPen;
begin
  if FParentPen then
    Result := FParent.Pen
  else begin
    if not Assigned(FPen) then begin
      FPen := TPen.Create;
      // for a new pen object, use the default settings
      if Assigned(FParent) then
        FPen.Assign(FParent.Pen)
      else begin
        FPen.Color := clWindowText;
        FPen.Width := 1;
      end;
    end; // if
    Result := FPen;
  end;
end;  // TBaseDisplayProperties.GetPen 

{-------------------------------------------------------------------------------
  Returns true if the properties override any of the parent settings. 
}
function TBaseDisplayProperties.HasOverride: Boolean;
begin
  Result := (not ParentPen) or (not ParentFont) or (not Assigned(FParent));
end;  // TBaseDisplayProperties.HasOverride 

{-------------------------------------------------------------------------------
  Read the basic information about the display properties from an XML node. 
}
procedure TBaseDisplayProperties.ReadBaseDisplayPropsXML(AXMLNode: IXMLNode);
begin
  FParentFont := not HasChildNode(AXMLNode, EL_FONT);
  if not FParentFont then
    ReadFontFromXML(AXMLNode, Font);
  FParentPen := not HasChildNode(AXMLNode, EL_PEN);
  // Output pen details if required
  if not FParentPen then
    with AXMLNode.ChildNodes[EL_PEN] do begin
      Pen.Color := Attributes[AT_COLOUR];
      Pen.Width := Attributes[AT_WIDTH];
    end; // with
end;  // TBaseDisplayProperties.ReadBaseDisplayPropsXML 

{-------------------------------------------------------------------------------
}
procedure TBaseDisplayProperties.SetParent(Value: TBaseDisplayProperties);
begin
  // Ensure parent is of the same type
  if not (Value is ClassType) then
    raise EDiagramObjectsException.Create(Format(
        ResStr_InvalidMethodCall,
        ['TBaseDisplayProperties.SetParent']));
  
  FParent := Value;
end;  // TBaseDisplayProperties.SetParent 

{-------------------------------------------------------------------------------
}
procedure TBaseDisplayProperties.SetParentFont(Value: Boolean);
begin
  if Value and not Assigned(Parent) then
    raise EDiagramObjectsException.Create(Format(
        ResStr_InvalidMethodCall,
        ['TBaseDisplayProperties.SetParentFont']));
  
  FParentFont := Value;
end;  // TBaseDisplayProperties.SetParentFont 

{-------------------------------------------------------------------------------
}
procedure TBaseDisplayProperties.SetParentPen(Value: Boolean);
begin
  if Value and not Assigned(Parent) then
    raise EDiagramObjectsException.Create(Format(
        ResStr_InvalidMethodCall,
        ['TBaseDisplayProperties.SetParentPen']));
  
  FParentPen := Value;
end;  // TBaseDisplayProperties.SetParentPen 

{-------------------------------------------------------------------------------
  Writes the XML out that is common to all properties (the pen and the font).
      Must be called from the WriteXML override in the leaf class to insert the
      XML in the correct place.
}
procedure TBaseDisplayProperties.WriteBaseDisplayPropsXML(AXMLNode: IXMLNode);
begin
  // Output font details if required
  if not FParentFont then
    WriteFontToXML(AXMLNode, Font);
  // Output pen details if required
  if not FParentPen then
    with AXMLNode.AddChild(EL_PEN) do begin
      Attributes[AT_COLOUR] := Pen.Color;
      Attributes[AT_WIDTH] := Pen.Width;
    end; // with
end;  // TBaseDisplayProperties.WriteBaseDisplayPropsXML 

{-==============================================================================
    TDiagramDisplayProperties
===============================================================================}
{-------------------------------------------------------------------------------
}
constructor TDiagramDisplayProperties.Create(ADiagram: TObject);
begin
  inherited Create;
  
  InitialiseObjects(ADiagram);
end;  // TDiagramDisplayProperties.Create 

{-------------------------------------------------------------------------------
}
destructor TDiagramDisplayProperties.Destroy;
begin
  FTitleFont.Free;
  
  inherited Destroy;
end;  // TDiagramDisplayProperties.Destroy 

{-------------------------------------------------------------------------------
  Accessor method 
}
function TDiagramDisplayProperties.GetTitlePosX: Integer;
begin
  Result := FTitleRect.Left;
end;  // TDiagramDisplayProperties.GetTitlePosX 

{-------------------------------------------------------------------------------
  Accessor method 
}
function TDiagramDisplayProperties.GetTitlePosY: Integer;
begin
  Result := FTitleRect.Top;
end;  // TDiagramDisplayProperties.GetTitlePosY 

{-------------------------------------------------------------------------------
  Object initialisation. 
}
procedure TDiagramDisplayProperties.InitialiseObjects(ADiagram: TObject);
begin
  FDiagram := ADiagram;
  Color := clWindow;
  FShowGrid := True;
  FSnapToGrid := True;
  FGridSize := 8;
  FShowTitle := True;
  FTitleFont := TFont.Create;
  FTitleFont.Name := 'Arial';
  FTitleFont.Size := 20;
  FTitleFont.Color := clGraytext;
  FTitle := ResStr_NewDiagram;
  FTitleRect.TopLeft := Point(20,20);
end;  // TDiagramDisplayProperties.InitialiseObjects 

{-------------------------------------------------------------------------------
}
procedure TDiagramDisplayProperties.ReadXML(AXMLNode: IXMLNode);
begin
  with AXMLNode do begin
    Color := Attributes[AT_COLOUR];
    ShowGrid := HasAttribute(AT_SHOWGRID);
    SnapToGrid := HasAttribute(AT_SNAPTOGRID);
    GridSize := Attributes[AT_GRIDSIZE];
    ShowTitle := HasChildNode(AXMLNode, EL_TITLE);
    if ShowTitle then begin
      Title := ChildNodes[EL_TITLE].Attributes[AT_TITLE];
      TitlePosX := ChildNodes[EL_TITLE].Attributes[AT_TITLEPOSX];
      TitlePosY := ChildNodes[EL_TITLE].Attributes[AT_TITLEPOSY];
      ReadFontFromXML(AXMLNode.ChildNodes[EL_TITLE], TitleFont);
    end; // with title node
  end;
end;  // TDiagramDisplayProperties.ReadXML 

{-------------------------------------------------------------------------------
}
procedure TDiagramDisplayProperties.SetColor(Value: TColor);
begin
  FColor := Value;
end;  // TDiagramDisplayProperties.SetColor 

{-------------------------------------------------------------------------------
}
procedure TDiagramDisplayProperties.SetGridSize(Value: Integer);
begin
  if FGridSize <> Value then
  begin
    FGridSize := Value;
    if ShowGrid then TDiagram(FDiagram).Invalidate;
  end;
end;  // TDiagramDisplayProperties.SetGridSize 

{-------------------------------------------------------------------------------
}
procedure TDiagramDisplayProperties.SetShowGrid(Value: Boolean);
begin
  if FShowGrid <> Value then
  begin
    FShowGrid := Value;
    TDiagram(FDiagram).Invalidate;
  end;
end;  // TDiagramDisplayProperties.SetShowGrid 

{-------------------------------------------------------------------------------
}
procedure TDiagramDisplayProperties.SetShowTitle(Value: Boolean);
begin
  if FShowTitle <> Value then
  begin
  
  FShowTitle := Value;
  
    TDiagram(FDiagram).Invalidate;
  
  end;
end;  // TDiagramDisplayProperties.SetShowTitle 

{-------------------------------------------------------------------------------
}
procedure TDiagramDisplayProperties.SetSnapToGrid(Value: Boolean);
begin
  FSnapToGrid := Value;
end;  // TDiagramDisplayProperties.SetSnapToGrid 

{-------------------------------------------------------------------------------
  Accessor method.  Change value invalidates diagram. 
}
procedure TDiagramDisplayProperties.SetTitle(const Value: string);
begin
  if FTitle <> Value then
  begin
  
    FTitle := Value;
  
    if ShowTitle then TDiagram(FDiagram).Invalidate;
  
  end;
end;  // TDiagramDisplayProperties.SetTitle 

{-------------------------------------------------------------------------------
  Accessor method, note this value is set by the diagram each time the title is
      painted.
}
procedure TDiagramDisplayProperties.SetTitleHeight(Value: Integer);
begin
  FTitleHeight := Value;
end;  // TDiagramDisplayProperties.SetTitleHeight 

{-------------------------------------------------------------------------------
  Accessor method, change value invalidates diagram. 
}
procedure TDiagramDisplayProperties.SetTitlePosX(Value: Integer);
begin
  if Value<>FTitleRect.Left then begin
    FTitleRect.Left := Value;
    TDiagram(FDiagram).Invalidate;
  end;
end;  // TDiagramDisplayProperties.SetTitlePosX 

{-------------------------------------------------------------------------------
  Accessor method, change value invalidates diagram. 
}
procedure TDiagramDisplayProperties.SetTitlePosY(Value: Integer);
begin
  if Value<>FTitleRect.Top then begin
    FTitleRect.Top := Value;
    TDiagram(FDiagram).Invalidate;
  end;
end;  // TDiagramDisplayProperties.SetTitlePosY 

{-------------------------------------------------------------------------------
}
procedure TDiagramDisplayProperties.SetTitleSelected(Value: Boolean);
begin
  if FTitleSelected <> Value then
  begin
  
    FTitleSelected := Value;
  
    TDiagram(FDiagram).Invalidate;
  
  end;
end;  // TDiagramDisplayProperties.SetTitleSelected 

{-------------------------------------------------------------------------------
  Accessor method, note this value is set by the diagram each time the title is
      painted.
}
procedure TDiagramDisplayProperties.SetTitleWidth(Value: Integer);
begin
  FTitleWidth := Value;
end;  // TDiagramDisplayProperties.SetTitleWidth 

{-------------------------------------------------------------------------------
  Check if a coordinate is over the title or not. 
}
function TDiagramDisplayProperties.TitleHitTest(AX, AY: integer): Boolean;
begin
  Result := (AX >= TitlePosX)
      and (AX <= TitlePosX + TitleWidth)
      and (AY >= TitlePosY)
      and (AY <= TitlePosY + TitleHeight)
      and ShowTitle;
end;  // TDiagramDisplayProperties.TitleHitTest 

{-------------------------------------------------------------------------------
  Write the display properties of the diagram to an XML node. 
}
procedure TDiagramDisplayProperties.WriteXML(AXMLNode: IXMLNode);
var
  lNode: IXMLNode;
  lTitleNode: IXMLNode;
begin
  lNode := AXMLNode.AddChild(EL_DIAGRAMDISPLAY);
  // basic diagram properties
  with lNode do begin
    Attributes[AT_COLOUR] := Color;
    if ShowGrid then Attributes[AT_SHOWGRID] := '1';
    if SnapToGrid then Attributes[AT_SNAPTOGRID] := '1';
    Attributes[AT_GRIDSIZE] := GridSize;
  end; // with lNode
  // diagram title properties
  if ShowTitle then begin
    lTitleNode := lNode.AddChild(EL_TITLE);
    with lTitleNode do begin
      Attributes[AT_TITLE]     := Title;
      Attributes[AT_TITLEPOSX] := TitlePosX;
      Attributes[AT_TITLEPOSY] := TitlePosY;
    end; // with lTitleNode
    WriteFontToXML(lTitleNode, TitleFont);
  end;
end;  // TDiagramDisplayProperties.WriteXML 



{-==============================================================================
    TBoxDisplayProperties
===============================================================================}
{-------------------------------------------------------------------------------
  Object finalisation 
}
destructor TBoxDisplayProperties.Destroy;
begin
  FBrush.Free;
  
  inherited Destroy;
end;  // TBoxDisplayProperties.Destroy 

{-------------------------------------------------------------------------------
  Accessor method.  If ParentBrush, then retreives the brush from the next
      layer up.  If retrieving own brush, then will instantiate the brush and
      initialise it if required.
}
function TBoxDisplayProperties.GetBrush: TBrush;
begin
  if FParentBrush then
    Result := TBoxDisplayProperties(Parent).Brush
  else begin
    if not Assigned(FBrush) then begin
      FBrush := TBrush.Create;
      // for a new brush object, use the default settings
      if Assigned(Parent) then
        FBrush.Assign(TBoxDisplayProperties(Parent).Brush)
      else begin
        FBrush.Color := MergeColours(clWindow, clHighlight, 90);
        FBrush.Style := bsSolid;
      end;
    end; // if
    Result := FBrush;
  end;
end;  // TBoxDisplayProperties.GetBrush 

{-------------------------------------------------------------------------------
  Retrieve the default font size for this type of object 
}
function TBoxDisplayProperties.GetDefaultFontSize: Integer;
begin
  Result := 10;
end;  // TBoxDisplayProperties.GetDefaultFontSize 

{-------------------------------------------------------------------------------
}
function TBoxDisplayProperties.HasOverride: Boolean;
begin
  Result := inherited HasOverride or (not ParentBrush);
end;  // TBoxDisplayProperties.HasOverride 

{-------------------------------------------------------------------------------
  Initialise from XML.  Parent properties should be set first. 
}
procedure TBoxDisplayProperties.ReadXML(AXMLNode: IXMLNode);
begin
  ReadBaseDisplayPropsXML(AXMLNode);
  // Read the Brush details as this applies only to a concept
  FParentBrush := not HasChildNode(AXMLNode, EL_BRUSH);
  if not FParentBrush then
    with AXMLNode.ChildNodes[EL_BRUSH] do
      Brush.Color := Attributes[AT_COLOUR];
end;  // TBoxDisplayProperties.ReadXML 

{-------------------------------------------------------------------------------
}
procedure TBoxDisplayProperties.SetParentBrush(Value: Boolean);
begin
  FParentBrush := Value;
end;  // TBoxDisplayProperties.SetParentBrush 

{-------------------------------------------------------------------------------
}
procedure TBoxDisplayProperties.WriteXML(AXMLNode: IXMLNode);
var
  lPropertiesNode: IXMLNode;
begin
  lPropertiesNode := AXMLNode.AddChild(GetXMLNodeName);
  // write the stuff that is common to all display properties
  WriteBaseDisplayPropsXML(lPropertiesNode);
  // write the Concept specific stuff
  if not FParentBrush then
    with lPropertiesNode.AddChild(EL_BRUSH) do
      Attributes[AT_COLOUR] := Brush.Color;
end;  // TBoxDisplayProperties.WriteXML 


end.
