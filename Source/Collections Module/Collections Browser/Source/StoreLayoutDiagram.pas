{===============================================================================
  Unit:        StoreLayoutDiagram.pas

  Defines:     TGrabHandle,
               TEdge,
               TRotateHandle,
               TStore,
               TStoreLayoutDiagram

  Description: Classes specific to diagramming the layout of a store

  Model:       StoreLayoutDiagram

  Created:     September 2004

  Last revision information:
    $Revision: 11 $
    $Date: 25/11/05 15:54 $
    $Author: Johnvanbreda $

===============================================================================}
unit StoreLayoutDiagram;

interface

uses
  Sysutils, Classes, Types, Controls, Graphics, Contnrs, Diagram, StdCtrls,
  Math, Windows, Forms, Dialogs, DiagramObjects, XMLDoc, XMLIntf, ComboListID,
  ExceptionForm;

resourcestring
  ResStr_StoreProperties = 'Store Properties...';
  ResStr_StoreDeleted = 'Store has been deleted';

const
  crRotate = 1;
  HANDLE_SIZE=4;

  EL_STOREDIAGRAM = 'store_diagram';
  EL_STOREDISPLAY = 'store_display';
  EL_STORE = 'store';
  AT_KEY='key';
  AT_X = 'x';
  AT_Y = 'y';
  AT_WIDTH = 'width';
  AT_HEIGHT = 'height';
  AT_ANGLE = 'angle';

type
  EStoreLayoutDiagram = class(TExceptionPath);

  TStoreDisplayProperties = class(TBoxDisplayProperties)
  protected
    function GetXMLNodeName: String; override;
  end;

  {-----------------------------------------------------------------------------
    Class for handles that are attached to the selected object, allowing the object to be
    resized or rotated.
  }
  TGrabHandle = class(TCustomControl)
  private
    FMouseDown: Boolean;
    FMouseDownPoint: TPoint;
    FObjectCentre: TPoint;
    FOnDrag: TMouseMoveEvent;
    function GetObjectCentreX: Integer;
    function GetObjectCentreY: Integer;
    procedure SetObjectCentreX(const Value: Integer);
    procedure SetObjectCentreY(const Value: Integer);
    procedure SetOnDrag(const Value: TMouseMoveEvent);
  protected
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure Paint; override;
    procedure SetCursor; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    property ObjectCentreX: Integer read GetObjectCentreX write SetObjectCentreX;
    property ObjectCentreY: Integer read GetObjectCentreY write SetObjectCentreY;
    property OnDrag: TMouseMoveEvent read FOnDrag write SetOnDrag;
  end;
  
  {-----------------------------------------------------------------------------
    Subclass of TGrabHandle for the handle that allows rotation of an object.
  }
  TRotateHandle = class(TGrabHandle)
  protected
    procedure Paint; override;
    procedure SetCursor; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  {-----------------------------------------------------------------------------
    Container for the details of an edge of an object.  This class provides information
    about an object to the hit detection algorithm.
  }
  TEdge = class(TObject)
  private
    FNPoint: TPoint;
    FSPoint: TPoint;
    procedure SetNPoint(const Value: TPoint);
    procedure SetSPoint(const Value: TPoint);
  public
    property NPoint: TPoint read FNPoint write SetNPoint;
    property SPoint: TPoint read FSPoint write SetSPoint;
  end;
  
  TShapeCoords = array of TPoint;

  TControlSize = TPoint;

  {-----------------------------------------------------------------------------
    Class managing the store objects on the diagram.
  }
  TStore = class(TObject)
  private
    FAngle: Double;
    FCaption: String;
    FDisplayProperties: TStoreDisplayProperties;
    FDrawCentreX: Integer;
    FDrawCentreY: Integer;
    FEdges: TObjectList;
    FHeight: Integer;
    FKey: String;
    FPos: TPoint;
    FRotateHandle: TGrabHandle;
    FSelected: Boolean;
    FSizeHandles: array[0..7] of TGrabHandle;
    FWidth: Integer;
    procedure CentreViewPoint(var AShape: TShapeCoords; AViewPoint, APos: TPoint;
        AControlSize: TControlSize);
    procedure ConstructEdges(const AShape: TShapeCoords);
    function GetPosX: Integer;
    function GetPosY: Integer;
    function GetShapeCentre(const AShape: TShapeCoords): TPoint;
    function GetSizeHandle(Index: integer): TGrabHandle;
    function GetSizeHandleCount: Integer;
    procedure PositionRotateHandle(const AShape: TShapeCoords);
    procedure RotateShape(var AShape: TShapeCoords; AAngle: double);
    procedure ScaleShape(var AShape: TShapeCoords; AZoom: double);
    procedure SetAngle(const Value: Double);
    procedure SetCaption(const Value: String);
    procedure SetDrawCentreX(const Value: Integer);
    procedure SetDrawCentreY(const Value: Integer);
    procedure SetHeight(const Value: Integer);
    procedure SetKey(const Value: String);
    procedure SetPosX(const Value: Integer);
    procedure SetPosY(const Value: Integer);
    procedure SetSelected(const Value: Boolean);
    procedure SetWidth(const Value: Integer);
  public
    constructor Create(AOwner: TComponent); reintroduce; virtual;
    destructor Destroy; override;
    procedure CreateStoreRect(AViewPoint: TPoint; AZoom: double; AControlSize: TControlSize;
        var ARect: TShapeCoords);
    function IsHit(const X, Y: integer): Boolean;
    procedure LoadCaption(const AZoom: double);
    procedure Paint(ADiagram: TDiagram; AViewPoint: TPoint; AZoom: double; AControlSize:
        TControlSize);
    procedure ReadXML(AXMLNode: IXMLNode);
    procedure ScaleFont(AFont: TFont; AZoom: double);
    procedure WriteXML(AXMLNode: IXMLNode);
    property Angle: Double read FAngle write SetAngle;
    property Caption: String read FCaption write SetCaption;
    property DisplayProperties: TStoreDisplayProperties read FDisplayProperties;
    property DrawCentreX: Integer read FDrawCentreX write SetDrawCentreX;
    property DrawCentreY: Integer read FDrawCentreY write SetDrawCentreY;
    property Height: Integer read FHeight write SetHeight;
    property Key: String read FKey write SetKey;
    property PosX: Integer read GetPosX write SetPosX;
    property PosY: Integer read GetPosY write SetPosY;
    property RotateHandle: TGrabHandle read FRotateHandle;
    property Selected: Boolean read FSelected write SetSelected;
    property SizeHandle[Index: integer]: TGrabHandle read GetSizeHandle;
    property SizeHandleCount: Integer read GetSizeHandleCount;
    property Width: Integer read FWidth write SetWidth;
  end;

  TNavigateEvent = procedure(Sender: TObject; const AKey: string) of object;

  {-----------------------------------------------------------------------------
    Diagram subclass specialised to store layout diagrams.
  }
  TStoreLayoutDiagram = class(TDiagram)
    procedure StorePropsClick(Sender: TObject);
  private
    FCentreWhenMouseDown: TPoint;
    FMouseDownPos: TPoint;
    FReadOnly: Boolean;
    FSelectedStore: TStore;
    FStoreListBox: TIDListBox;
    FStores: TObjectList;
    FViewPoint: TPoint;
    FZoom: Double;
    FOnNavigate: TNavigateEvent;
    function CreateStore: TStore;
    procedure DragRotateHandle(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure DragSizeHandle(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure KeepSelectedStoreOnPage;
    procedure SetReadOnly(Value: Boolean);
    procedure SetStoreListBox(Value: TIDListBox);
    procedure SetZoom(const Value: Double);
    procedure SnapToGrid;
    function StoreAtPos(AX, AY: integer): TStore;
    function GetInternalHeight: integer;
    function GetInternalWidth: integer;
    procedure SetOnNavigate(const Value: TNavigateEvent);
  protected
    procedure CreatePopupMenu; override;
    procedure DiagramSettingsClick(Sender: TObject); override;
    procedure DoDblClick(X, Y: Integer); override;
    procedure DoDragOver(Source: TObject; X, Y: Integer; State: TDragState; var Accept:
        Boolean); override;
    procedure DoKeyDown(var Key: Word; Shift: TShiftState); override;
    procedure DoMouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure DoMouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure DoMouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure DoPaint; override;
    function GridOffsetX: Integer; override;
    function GridOffsetY: Integer; override;
    procedure InitialiseObjects; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DeleteSelectedConcept;
    procedure DoDragDrop(Source: TObject; X, Y: Integer); override;
    procedure ReadXML(const AXML: string);
    function WriteXML: String;
    property ReadOnly: Boolean read FReadOnly write SetReadOnly default False;
    property StoreListBox: TIDListBox read FStoreListBox write SetStoreListBox;
    property Stores: TObjectList read FStores;
    property Zoom: Double read FZoom write SetZoom;
    property InternalWidth: integer read GetInternalWidth;
    property InternalHeight: integer read GetInternalHeight;
    property OnNavigate: TNavigateEvent read FOnNavigate write SetOnNavigate;
  end;
  
implementation

uses
  GeneralFunctions, StoreDisplayProperties, DiagramXMLConstants,
  GeneralData, Variants, Menus, DiagramSettings;

{$R RotateIcon.res}

{-==============================================================================
    TGrabHandle
===============================================================================}
{-------------------------------------------------------------------------------
}
constructor TGrabHandle.Create(AOwner: TComponent);
begin
  inherited;
  Width := HANDLE_SIZE*2;
  Height := HANDLE_SIZE*2;
end;  // TGrabHandle.Create 

{-------------------------------------------------------------------------------
}
function TGrabHandle.GetObjectCentreX: Integer;
begin
  Result := FObjectCentre.X;
end;  // TGrabHandle.GetObjectCentreX 

{-------------------------------------------------------------------------------
}
function TGrabHandle.GetObjectCentreY: Integer;
begin
  Result := FObjectCentre.Y;
end;  // TGrabHandle.GetObjectCentreY 

{-------------------------------------------------------------------------------
}
procedure TGrabHandle.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  FMouseDownPoint := ClientToScreen(Point(X, Y));
  FMouseDown := True;
end;  // TGrabHandle.MouseDown 

{-------------------------------------------------------------------------------
}
procedure TGrabHandle.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if FMouseDown then
    if Assigned(FOnDrag) then
      FOnDrag(Self, Shift, X + Left, Y + Top);
  SetCursor;
end;  // TGrabHandle.MouseMove 

{-------------------------------------------------------------------------------
}
procedure TGrabHandle.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  FMouseDown := False;
end;  // TGrabHandle.MouseUp 

{-------------------------------------------------------------------------------
}
procedure TGrabHandle.Paint;
begin
  Canvas.Brush.Color := clGradientActiveCaption;
  Canvas.Polygon([
      Point(0, 0),
      Point(Width-1, 0),
      Point(Width-1, Height-1),
      Point(0, Height-1)]);
end;  // TGrabHandle.Paint 

{-------------------------------------------------------------------------------
}
procedure TGrabHandle.SetCursor;
var
  h: Double;
  a: Double;
begin
  // Get hypotenuse
  h := Sqrt(Sqr(Abs(Left - FObjectCentre.X)) + Sqr(Abs(Top - FObjectCentre.Y)));
  // Get angle
  a := ArcCos((Left - FObjectCentre.X)/h);
  if (a < PI/8) or (Abs(PI-a)<PI/8) then
    Cursor := crSizeWE
  else if (a>(3/8 * PI)) and (a<(5/8 * PI)) then
    Cursor := crSizeNS
  else if (Left - FObjectCentre.X<0)=(Top - FObjectCentre.Y<0) then
    Cursor := crSizeNWSE
  else
    Cursor := crSizeNESW;
end;  // TGrabHandle.SetCursor 

{-------------------------------------------------------------------------------
}
procedure TGrabHandle.SetObjectCentreX(const Value: Integer);
begin
  FObjectCentre.X := Value;
end;  // TGrabHandle.SetObjectCentreX 

{-------------------------------------------------------------------------------
}
procedure TGrabHandle.SetObjectCentreY(const Value: Integer);
begin
  FObjectCentre.Y := Value;
end;  // TGrabHandle.SetObjectCentreY 

{-------------------------------------------------------------------------------
}
procedure TGrabHandle.SetOnDrag(const Value: TMouseMoveEvent);
begin
  FOnDrag := Value;
end;  // TGrabHandle.SetOnDrag 

{-==============================================================================
    TRotateHandle
===============================================================================}
{-------------------------------------------------------------------------------
}
constructor TRotateHandle.Create(AOwner: TComponent);
begin
  inherited;
end;  // TRotateHandle.Create 

{-------------------------------------------------------------------------------
}
procedure TRotateHandle.Paint;
begin
  Canvas.Brush.Color := clGradientActiveCaption;
  Canvas.Ellipse(Rect(0, 0, Width, Height));
end;  // TRotateHandle.Paint 

{-------------------------------------------------------------------------------
}
procedure TRotateHandle.SetCursor;
begin
  inherited;
  Cursor := crRotate;
end;  // TRotateHandle.SetCursor 

{-==============================================================================
    TEdge
===============================================================================}
{-------------------------------------------------------------------------------
}
procedure TEdge.SetNPoint(const Value: TPoint);
begin
  FNPoint := Value;
end;  // TEdge.SetNPoint 

{-------------------------------------------------------------------------------
}
procedure TEdge.SetSPoint(const Value: TPoint);
begin
  FSPoint := Value;
end;  // TEdge.SetSPoint 

{-==============================================================================
    TStore
===============================================================================}
{-------------------------------------------------------------------------------
}
constructor TStore.Create(AOwner: TComponent);
var
  i: Integer;
begin
  inherited Create;
  FRotateHandle := TRotateHandle.Create(AOwner);
  FRotateHandle.Visible := False;
  for i := 0 to High(FSizeHandles) do begin
    FSizeHandles[i] := TGrabHandle.Create(AOwner);
    FSizeHandles[i].Visible := False;
  end;
  FEdges := TObjectList.Create;
  FDisplayProperties := TStoreDisplayProperties.Create;
end;  // TStore.Create 

{-------------------------------------------------------------------------------
}
destructor TStore.Destroy;
var
  i: Integer;
begin
  FEdges.Free;
  FDisplayProperties.Free;
  for i := 0 to High(FSizeHandles) do
    FSizeHandles[i].Free;
  FRotateHandle.Free;
  inherited;
end;  // TStore.Destroy 

{-------------------------------------------------------------------------------
  Moves the shape across the canvas to take account of the centre of the view port. 
}
procedure TStore.CentreViewPoint(var AShape: TShapeCoords; AViewPoint, APos: TPoint;
    AControlSize: TControlSize);
var
  i: Integer;
begin
  for i := 0 to High(AShape) do begin
    AShape[i].X := AShape[i].X + AControlSize.X div 2 - AViewPoint.X + APos.X;
    AShape[i].Y := AShape[i].Y + AControlSize.Y div 2 - AViewPoint.Y + APos.Y;
  end;
end;  // TStore.CentreViewPoint 

{-------------------------------------------------------------------------------
  Record the list of edges that this object has.  This allows relatively simple hit detection
      when the mouse is clicked on the diagram.
}
procedure TStore.ConstructEdges(const AShape: TShapeCoords);
var
  i: Integer;
  
  procedure SetEdge(ACoord1, ACoord2: TPoint);
  begin
    with TEdge(FEdges[i]) do
      if ACoord1.Y < ACoord2.Y then begin// first point is southernmost
        SPoint := ACoord1;
        NPoint := ACoord2;
      end else
      if ACoord1.Y > ACoord2.Y then begin// second point is southernmost
        SPoint := ACoord2;
        NPoint := ACoord1;
      end else {horizontal line}
      if ACoord1.X > ACoord2.X then begin// first point is easternmost
        SPoint := ACoord1;
        NPoint := ACoord2;
      end else begin// second point is easternmost
        SPoint := ACoord2;
        NPoint := ACoord1;
      end;
  end;
  
begin
  if FEdges.Count<>Length(AShape) then
    // first time, so create the Edge objects
    for i := 0 to High(AShape) do
      FEdges.Add(TEdge.Create);
  // Now set the edge coords
  for i := 0 to High(AShape)-1 do
    SetEdge(AShape[i], AShape[i+1]);
  SetEdge(AShape[High(AShape)], AShape[0]);
end;  // TStore.ConstructEdges 

{-------------------------------------------------------------------------------
}
procedure TStore.CreateStoreRect(AViewPoint: TPoint; AZoom: double; AControlSize: TControlSize;
    var ARect: TShapeCoords);
begin
  SetLength(ARect, 4);
  
  ARect[0] := Point(0-Width div 2, 0-Height div 2);
  ARect[1] := Point(Width div 2, 0-Height div 2);
  ARect[2] := Point(Width div 2, Height div 2);
  ARect[3] := Point(0-Width div 2, Height div 2);
  RotateShape(ARect, Angle);
  CentreViewPoint(ARect, AViewPoint, Point(PosX, PosY),
      Point(Trunc(AControlSize.X/AZoom), Trunc(AControlSize.Y/AZoom)));
  ScaleShape(ARect, AZoom);
end;  // TStore.CreateStoreRect 

{-------------------------------------------------------------------------------
}
function TStore.GetPosX: Integer;
begin
  Result := FPos.X;
end;  // TStore.GetPosX 

{-------------------------------------------------------------------------------
}
function TStore.GetPosY: Integer;
begin
  Result := FPos.Y;
end;  // TStore.GetPosY 

{-------------------------------------------------------------------------------
  Calculates the centre of the shape. 
}
function TStore.GetShapeCentre(const AShape: TShapeCoords): TPoint;
var
  lTotalX, lTotalY: Integer;
  i: Integer;
begin
  lTotalX := 0;
  lTotalY := 0;
  for i := 0 to High(AShape) do begin
    Inc(lTotalX, AShape[i].X);
    Inc(lTotalY, AShape[i].Y);
  end;
  Result.X := lTotalX div Length(AShape);
  Result.Y := lTotalY div Length(AShape);
end;  // TStore.GetShapeCentre 

{-------------------------------------------------------------------------------
}
function TStore.GetSizeHandle(Index: integer): TGrabHandle;
begin
  Result := FSizeHandles[Index];
end;  // TStore.GetSizeHandle 

{-------------------------------------------------------------------------------
}
function TStore.GetSizeHandleCount: Integer;
begin
  Result := Length(FSizeHandles);
end;  // TStore.GetSizeHandleCount 

{-------------------------------------------------------------------------------
  Detect if a click on the diagram at point x, y is a click on this store. 
}
function TStore.IsHit(const X, Y: integer): Boolean;
var
  i, lEdgeCount: Integer;
  lEdge: TEdge;
  lSNRatio, lEWRatio: Double;
  
  function preI: Integer;
  begin
    if i = 0 then Result := FEdges.Count - 1
    else Result := i - 1;
  end;
  
  function postI: Integer;
  begin
    if i = FEdges.Count - 1 then Result := 0
    else Result := i + 1;
  end;
  
  procedure GetEdges(out preEdge, postEdge: TEdge);
  begin
    preEdge := TEdge(FEdges[preI]);
    postEdge := TEdge(FEdges[postI]);
  end;
  
  procedure IncCount;
  var preEdge, postEdge: TEdge;
      preSouth, postSouth: Boolean;
  begin
    if (lEdge.SPoint.Y = Y) then begin
      GetEdges(preEdge, postEdge);
      if (lEdge.NPoint.Y = Y) then {lEdge is horizontal} begin
        if X <= lEdge.SPoint.X then {point is in lEdge} Inc(lEdgeCount)
        else begin {Assuming pre and post edges are not horizontal}
          if preEdge.NPoint.Y = Y
            then preSouth := preEdge.SPoint.Y < Y
            else preSouth := preEdge.NPoint.Y < Y;
          if postEdge.NPoint.Y = Y
            then postSouth := postEdge.SPoint.Y < Y
            else postSouth := postEdge.NPoint.Y < Y;
          if preSouth = postSouth then // No Cut
          else Inc(lEdgeCount);
        end;
      end else begin
        if ((lEdge.SPoint.X = preEdge.SPoint.X) and (lEdge.SPoint.Y = preEdge.SPoint.Y)) or
           ((lEdge.SPoint.X = preEdge.NPoint.X) and (lEdge.SPoint.Y = preEdge.NPoint.Y)) then
          // Do nothing - dealt with by previous edge
        else
          if postEdge.SPoint.Y = postEdge.NPoint.Y then
            //Do nothing - postEdge is horizontal, dealt with by the horizontal edge
          else
            if (lEdge.SPoint.X = postEdge.SPoint.X) and (lEdge.SPoint.Y = postEdge.SPoint.Y)
                then
              //Do nothing - Touching a point
            else
              //Cutting the join  -increment the count
              Inc(lEdgeCount);
      end;
    end else if (lEdge.NPoint.Y = Y) then begin
      GetEdges(preEdge, postEdge);
      if ((lEdge.NPoint.X = preEdge.SPoint.X) and (lEdge.NPoint.Y = preEdge.SPoint.Y)) or
         ((lEdge.NPoint.X = preEdge.NPoint.X) and (lEdge.NPoint.Y = preEdge.NPoint.Y)) then
        // Do nothing - dealt with by previous edge
      else
        if postEdge.SPoint.Y = postEdge.NPoint.Y then
          //Do nothing - postEdge is horizontal, dealt with by the horizontal edge
        else
          if (lEdge.NPoint.X = postEdge.NPoint.X) and (lEdge.NPoint.Y = postEdge.NPoint.Y) then
            //Do nothing - Touching a point
          else
            //Cutting the join  -increment the count
            Inc(lEdgeCount);
    end else Inc(lEdgeCount);
  end;
  
begin
  lEdgeCount := 0;
      // count of edges that intersect with a horizontal line from the point westwards to infinity
  { Loop through our edges }
  for i := 0 to FEdges.Count-1 do begin
    lEdge := TEdge(FEdges[i]);
    { Ignore unless our point lies somewhere along the edge's range of latitudes }
    if (lEdge.SPoint.Y <= Y) and (lEdge.NPoint.Y >= Y) then begin
      if lEdge.SPoint.Y = lEdge.NPoint.Y then begin
         { Edge is horizontal line, NPoint is westernmost }
         if (lEdge.NPoint.X <= X) then
           IncCount;
      end else begin
        if lEdge.SPoint.X = lEdge.NPoint.X then begin
          { lEdge line - is it to the left? }
          if lEdge.SPoint.X <= X then
            IncCount;
        end else begin
          { Count edges that lie to left of point }
          { If definitely to the left then include }
          if (X > lEdge.SPoint.X) and (X > lEdge.NPoint.X) then
            IncCount
          else if (X > lEdge.SPoint.X) or (X > lEdge.NPoint.X) then begin
            { this line crosses near our point at an angle, so need to calculate if left or
                right }
            lSNRatio := (Y - lEdge.SPoint.Y) / (lEdge.NPoint.Y - lEdge.SPoint.Y);
            lEWRatio := (X - lEdge.SPoint.X) / (lEdge.NPoint.X - lEdge.SPoint.X);
            { Count edge if point lies further along the east west direction than the north
                south }
            if ((lEWRatio > lSNRatio) and (lEdge.NPoint.X > lEdge.SPoint.X))
                or ((lEWRatio < lSNRatio) and (lEdge.NPoint.X < lEdge.SPoint.X)) then
              IncCount;
          end;
        end;
      end; // not a horizontal line
    end; // if in correct latitude
  end; // for
  Result := lEdgeCount mod 2 = 1; // if odd number of lines found, must be a hit
end;  // TStore.IsHit 

{-------------------------------------------------------------------------------
  Draw the store rectangle and caption.  Also, draw the grab handles if the store is selected. 
}
procedure TStore.Paint(ADiagram: TDiagram; AViewPoint: TPoint; AZoom: double; AControlSize:
    TControlSize);
var
  lRect: TShapeCoords;
  lTextRect: TRect;
  i: Integer;
begin
  CreateStoreRect(AViewPoint, AZoom, AControlSize, lRect);
  lTextRect := Rect(
      Min(lRect[0].X, Min(lRect[1].X, Min(lRect[2].X, lRect[3].X))),
      Min(lRect[0].Y, Min(lRect[1].Y, Min(lRect[2].Y, lRect[3].Y))),
      Max(lRect[0].X, Max(lRect[1].X, Max(lRect[2].X, lRect[3].X))),
      Min(lRect[0].Y, Min(lRect[1].Y, Min(lRect[2].Y, lRect[3].Y))));
          // deliberate 0 height rect
  with ADiagram.Canvas do begin
    Font.Assign(DisplayProperties.Font);
    ScaleFont(ADiagram.Canvas.Font, AZoom);
    Brush.Assign(DisplayProperties.Brush);
    Pen.Assign(DisplayProperties.Pen);
    // Hide pen if zero width
    if Pen.Width=0 then
      Pen.Color := Brush.Color;
    if Selected then begin
      // Change colour a bit if focused
      Brush.Color := MergeColours(Brush.Color, clHighlight, 80);
      Font.Color := GetContrastColour(Brush.Color);
    end;
    Polygon(lRect);
  end;

  for i := 0 to High(lRect) do begin
    SizeHandle[i*2].Left := lRect[i].X-HANDLE_SIZE-ADiagram.HorzScrollBar.Position;
    SizeHandle[i*2].Top := lRect[i].Y-HANDLE_SIZE-ADiagram.VertScrollBar.Position;
  end;
  for i := 0 to High(lRect)-1 do begin
    SizeHandle[i*2+1].Left := (lRect[i].X + lRect[i+1].X) div 2 -
        HANDLE_SIZE-ADiagram.HorzScrollBar.Position;
    SizeHandle[i*2+1].Top := (lRect[i].Y + lRect[i+1].Y) div 2 -
        HANDLE_SIZE-ADiagram.VertScrollBar.Position;
  end;
  SizeHandle[High(lRect)*2+1].Left := (lRect[0].X + lRect[High(lRect)].X) div 2 -
      HANDLE_SIZE-ADiagram.HorzScrollBar.Position;
  SizeHandle[High(lRect)*2+1].Top := (lRect[0].Y + lRect[High(lRect)].Y) div 2 -
      HANDLE_SIZE-ADiagram.VertScrollBar.Position;
  ConstructEdges(lRect);
  PositionRotateHandle(lRect);
  for i := 0 to SizeHandleCount-1 do begin
    SizeHandle[i].ObjectCentreX := DrawCentreX;
    SizeHandle[i].ObjectCentreY := DrawCentreY;
  end;
  SetBKMode(ADiagram.Canvas.Handle, 1);
  // Calculate text dimensions
  DrawText(ADiagram.Canvas.Handle, PAnsiChar(FCaption), Length(FCaption),
      lTextRect, DT_WORDBREAK + DT_CENTER + DT_NOCLIP + DT_CALCRECT);
  // Centre the text
  lTextRect.Top :=
      lTextRect.Top - (lTextRect.Bottom-
      Max(lRect[0].Y, Max(lRect[1].Y, Max(lRect[2].Y, lRect[3].Y)))) div 2;
  lTextRect.Right := Max(lRect[0].X, Max(lRect[1].X, Max(lRect[2].X, lRect[3].X)));
  DrawText(ADiagram.Canvas.Handle, PAnsiChar(FCaption), Length(FCaption),
      lTextRect, DT_WORDBREAK + DT_CENTER + DT_NOCLIP);
end;  // TStore.Paint

{-------------------------------------------------------------------------------
  Calculates the location for the rotation grab handle that's appropriate for the shape. 
}
procedure TStore.PositionRotateHandle(const AShape: TShapeCoords);
var
  lCentre: TPoint;
  a, b, c: Double;
begin
  //Position the rotate handle, 20 pixels outside midway point of first vector
  lCentre := GetShapeCentre(AShape);
  DrawCentreX := lCentre.X;
  DrawCentreY := lCentre.Y;
  a := SizeHandle[1].Left-DrawCentreX;
  b := SizeHandle[1].Top-DrawCentreY;
  c := Sqrt(Sqr(a) + Sqr(b));
  RotateHandle.Left := Trunc(DrawCentreX + a * ((c + 20) / c))+HANDLE_SIZE div 2;
  RotateHandle.Top := Trunc(DrawCentreY + b * ((c + 20) / c)) + HANDLE_SIZE div 2;
end;  // TStore.PositionRotateHandle 

{-------------------------------------------------------------------------------
  Reads the XML from the node to position and set properties of the store. 
}
procedure TStore.ReadXML(AXMLNode: IXMLNode);
begin
  Key := AXMLNode.Attributes[AT_KEY];
  PosX := AXMLNode.Attributes[AT_X];
  PosY := AXMLNode.Attributes[AT_Y];
  Width := AXMLNode.Attributes[AT_WIDTH];
  Height := AXMLNode.Attributes[AT_HEIGHT];
  Angle := AXMLNode.Attributes[AT_ANGLE];
  if HasChildNode(AXMLNode, FDisplayProperties.GetXMLNodeName) then
    FDisplayProperties.ReadXML(AXMLNode.ChildNodes[FDisplayProperties.GetXMLNodeName]);
end;  // TStore.ReadXML 

{-------------------------------------------------------------------------------
  Rotates the shape by the supplied angle. 
}
procedure TStore.RotateShape(var AShape: TShapeCoords; AAngle: double);
var
  i: Integer;
begin
  for i := 0 to High(AShape) do begin
    AShape[i] := Point(
                Trunc(AShape[i].X * cos(AAngle) + AShape[i].Y * sin(AAngle)),
                Trunc(AShape[i].Y * cos(AAngle) - AShape[i].X * sin(AAngle)));
  end;
end;  // TStore.RotateShape 

{-------------------------------------------------------------------------------
  Set the font size appropriate to the zoom scale. 
}
procedure TStore.ScaleFont(AFont: TFont; AZoom: double);
begin
  AFont.Size := Round(AFont.Size * AZoom);
  if AFont.Size<=7 then begin
    AFont.Name := 'Small Fonts';
    AFont.Size := Max(5, AFont.Size);
  end;
end;  // TStore.ScaleFont 

{-------------------------------------------------------------------------------
  Scales the shape by the appropriate zoom factor. 
}
procedure TStore.ScaleShape(var AShape: TShapeCoords; AZoom: double);
var
  i: Integer;
begin
  for i := 0 to High(AShape) do begin
    AShape[i].X := Trunc(AShape[i].X * AZoom);
    AShape[i].Y := Trunc(AShape[i].Y * AZoom);
  end;
end;  // TStore.ScaleShape 

{-------------------------------------------------------------------------------
}
procedure TStore.SetAngle(const Value: Double);
begin
  FAngle := Value;
end;  // TStore.SetAngle 

{-------------------------------------------------------------------------------
}
procedure TStore.SetCaption(const Value: String);
begin
  FCaption := Value;
end;  // TStore.SetCaption 

{-------------------------------------------------------------------------------
}
procedure TStore.SetDrawCentreX(const Value: Integer);
begin
  FDrawCentreX := Value;
end;  // TStore.SetDrawCentreX 

{-------------------------------------------------------------------------------
}
procedure TStore.SetDrawCentreY(const Value: Integer);
begin
  FDrawCentreY := Value;
end;  // TStore.SetDrawCentreY 

{-------------------------------------------------------------------------------
}
procedure TStore.SetHeight(const Value: Integer);
begin
  FHeight := Value;
end;  // TStore.SetHeight 

{-------------------------------------------------------------------------------
}
procedure TStore.SetKey(const Value: String);
begin
  FKey := Value;
end;  // TStore.SetKey

{-------------------------------------------------------------------------------
}
procedure TStore.LoadCaption(const AZoom: double);
begin
  with dmGeneral.GetRecordset('usp_Store_Select', ['@Key', Key]) do begin
    if EOF and BOF then
      raise EStoreLayoutDiagram.Create(ResStr_StoreDeleted);
    if VarIsNull(Fields['Current_Location_Code'].Value) then
      Caption := Fields['Item_Name'].Value
    else if AZoom<1 then
      Caption := Fields['Current_Location_Code'].Value
    else
      Caption := Fields['Item_Name'].Value + ' - ' + Fields['Current_Location_Code'].Value;
  end;
end;

{-------------------------------------------------------------------------------
}
procedure TStore.SetPosX(const Value: Integer);
begin
  FPos.X := Value;
end;  // TStore.SetPosX 

{-------------------------------------------------------------------------------
}
procedure TStore.SetPosY(const Value: Integer);
begin
  FPos.Y := Value;
end;  // TStore.SetPosY 

{-------------------------------------------------------------------------------
}
procedure TStore.SetSelected(const Value: Boolean);
var
  i: Integer;
begin
  FSelected := Value;
  for i := 0 to High(FSizeHandles) do
    FSizeHandles[i].Visible := Value;
  FRotateHandle.Visible := Value;
end;  // TStore.SetSelected 

{-------------------------------------------------------------------------------
}
procedure TStore.SetWidth(const Value: Integer);
begin
  FWidth := Value;
end;  // TStore.SetWidth 

{-------------------------------------------------------------------------------
  Outputs details of the store's display to the XML node. 
}
procedure TStore.WriteXML(AXMLNode: IXMLNode);
var
  lStoreNode: IXMLNode;
begin
  lStoreNode := AXMLNode.AddChild(EL_STORE);
  lStoreNode.Attributes[AT_KEY] := Key;
  lStoreNode.Attributes[AT_X] := PosX;
  lStoreNode.Attributes[AT_Y] := PosY;
  lStoreNode.Attributes[AT_WIDTH] := Width;
  lStoreNode.Attributes[AT_HEIGHT] := Height;
  lStoreNode.Attributes[AT_ANGLE] := Angle;
  DisplayProperties.WriteXML(lStoreNode);
end;  // TStore.WriteXML 

{-==============================================================================
    TStoreLayoutDiagram
===============================================================================}
{-------------------------------------------------------------------------------
}
constructor TStoreLayoutDiagram.Create(AOwner: TComponent);
var
  lCursor: HCursor;
begin
  inherited;
  FViewPoint := Point(0, 0);
  FZoom := 0.5;
  FStores := TObjectList.Create;
  DoubleBuffered := True;
  // Ignore potential failure of loadcursor, since we will just get a pointer instead
  lCursor := LoadCursor(hInstance, 'ROTATE');
  Screen.Cursors[crRotate] := lCursor;
  FReadOnly := False;
  TabStop := True;
  FDiagram.TabStop := True;
end;  // TStoreLayoutDiagram.Create 

{-------------------------------------------------------------------------------
}
destructor TStoreLayoutDiagram.Destroy;
begin
  FStores.Free;
  inherited;
end;  // TStoreLayoutDiagram.Destroy 

{-------------------------------------------------------------------------------
  Adds the Store Properties item to the popup menu. 
}
procedure TStoreLayoutDiagram.CreatePopupMenu;
  
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
  if not ReadOnly then
    AddMenuItem(ResStr_StoreProperties, StorePropsClick);
end;  // TStoreLayoutDiagram.CreatePopupMenu

{-------------------------------------------------------------------------------
  Creates a store and adds it to the internal list. 
}
function TStoreLayoutDiagram.CreateStore: TStore;
var
  i: Integer;
begin
  Result := TStore.Create(Self);
  for i := 0 to Result.SizeHandleCount-1 do begin
    Result.SizeHandle[i].Parent := Self;
    Result.SizeHandle[i].OnDrag := DragSizeHandle;
  end;
  Result.RotateHandle.OnDrag := DragRotateHandle;
  Result.RotateHandle.Parent := self;
  FStores.Add(Result);
end;  // TStoreLayoutDiagram.CreateStore 

{-------------------------------------------------------------------------------
  Removes the currently selected concept. 
}
procedure TStoreLayoutDiagram.DeleteSelectedConcept;
begin
  if Assigned(FSelectedStore) and (not ReadOnly) then begin
    if Assigned(FStoreListBox) then
      FStoreListBox.Add(FSelectedStore.Caption, FSelectedStore.Key);
    FStores.Remove(FSelectedStore);
    Invalidate;
    Dirty := True;
  end;
end;  // TStoreLayoutDiagram.DeleteSelectedConcept 

{-------------------------------------------------------------------------------
}
procedure TStoreLayoutDiagram.DiagramSettingsClick(Sender: TObject);
var
  lDialog: TdlgDiagramSettings;
begin
  lDialog := TdlgDiagramSettings.Create(nil, DiagramDisplayProperties);
  with lDialog do
    try
      HideTitle;
      if ShowModal=mrOk then
        Dirty := True;
    finally
      Free;
    end; //
end;  // TStoreLayoutDiagram.DiagramSettingsClick 

{-------------------------------------------------------------------------------
}
procedure TStoreLayoutDiagram.DoDblClick(X, Y: Integer);
begin
  if not ReadOnly then begin
    if Assigned(FSelectedStore) then
      StorePropsClick(nil)
    else
      DiagramSettingsClick(nil);
  end
  else begin
    if Assigned(FSelectedStore) and Assigned(FOnNavigate) then
      FOnNavigate(Self, FSelectedStore.Key);
  end;
end;  // TStoreLayoutDiagram.DoDblClick

{-------------------------------------------------------------------------------
}
procedure TStoreLayoutDiagram.DoDragDrop(Source: TObject; X, Y: Integer);
begin
  inherited;
  if Source is TIDListBox then
    with CreateStore do begin
      PosX := X + FViewPoint.X - FDiagram.Width div 2;
      PosY := Y + FViewPoint.Y - FDiagram.Height div 2;
      Width := 100;
      Height := 100;
      Key := TIDListBox(Source).CurrentStrID;
      LoadCaption(Zoom);
      TIDListBox(Source).Delete(TIDListBox(Source).ItemIndex);
      Invalidate;
      Dirty := True;
    end;
end;  // TStoreLayoutDiagram.DoDragDrop 

{-------------------------------------------------------------------------------
  Drag is accepted when the item is a store coming from the list box. 
}
procedure TStoreLayoutDiagram.DoDragOver(Source: TObject; X, Y: Integer; State: TDragState;
    var Accept: Boolean);
begin
  if not ReadOnly then begin
    if (Source is TIDListBox) then
      Accept := (TIDListBox(Source).ItemIndex<>-1) and
             (TIDListBox(Source).Name='lbAvailableStores');
  end;
end;  // TStoreLayoutDiagram.DoDragOver

{-------------------------------------------------------------------------------
  If delete key pressed when a store focused, remove the store from the diagram and add it
      back to the list of available stores.
}
procedure TStoreLayoutDiagram.DoKeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited;
  if (not ReadOnly) and (Key=VK_DELETE) then
    DeleteSelectedConcept;
end;  // TStoreLayoutDiagram.DoKeyDown

{-------------------------------------------------------------------------------
}
procedure TStoreLayoutDiagram.DoMouseDown(Button: TMouseButton; Shift: TShiftState; X, Y:
    Integer);
var
  i: Integer;
begin
  inherited;
  FSelectedStore := StoreAtPos(X, Y);
  if not ReadOnly then begin
    for i := 0 to FStores.Count-1 do
      TStore(FStores[i]).Selected := FStores[i]=FSelectedStore;
    if Assigned(FSelectedStore) then begin
      if Assigned(PopupMenu) then
        PopupMenu.Items[1].Visible := True;
      FMouseDownPos := Point(X, Y);
      FCentreWhenMouseDown := Point(FSelectedStore.PosX, FSelectedStore.PosY);
   end else if Assigned(PopupMenu) then
      PopupMenu.Items[1].Visible := False;
    Invalidate;
    RequestFocus;
  end;
end;  // TStoreLayoutDiagram.DoMouseDown

{-------------------------------------------------------------------------------
}
procedure TStoreLayoutDiagram.DoMouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if not ReadOnly then begin
    if Assigned(StoreAtPos(X,Y)) and (not ReadOnly) then
      FDiagram.Cursor := crSizeAll
    else
      FDiagram.Cursor := crDefault;
    if (ssLeft in Shift) and Assigned(FSelectedStore) then begin
      FSelectedStore.PosX := Trunc((X - FMouseDownPos.X)/FZoom + FCentreWhenMouseDown.X);
      FSelectedStore.PosY := Trunc((Y - FMouseDownPos.Y)/FZoom + FCentreWhenMouseDown.Y);
      KeepSelectedStoreOnPage;
      SnapToGrid;
      Invalidate;
      Dirty := True;
    end;
  end;
end;  // TStoreLayoutDiagram.DoMouseMove

{-------------------------------------------------------------------------------
}
procedure TStoreLayoutDiagram.DoMouseUp(Button: TMouseButton; Shift: TShiftState; X, Y:
    Integer);
begin
  inherited;
end;  // TStoreLayoutDiagram.DoMouseUp

{-------------------------------------------------------------------------------
}
procedure TStoreLayoutDiagram.DoPaint;
var
  i: Integer;
begin
  if FZoom<>1 then
    FDiagramDisplayProperties.ShowGrid := False;
  inherited;
  for i := 0 to FStores.Count-1 do
    TStore(FStores[i]).Paint(Self, FViewPoint, FZoom,
        Point(FDiagram.ClientWidth, FDiagram.ClientHeight));
end;  // TStoreLayoutDiagram.DoPaint 

{-------------------------------------------------------------------------------
}
procedure TStoreLayoutDiagram.DragRotateHandle(Sender: TObject; Shift: TShiftState; X, Y:
    Integer);
var
  lVect: TPoint;
begin
  if Assigned(FSelectedStore) then begin
    lVect := Point(X - FSelectedStore.DrawCentreX, Y-FSelectedStore.DrawCentreY);
    if lVect.Y <> 0 then begin
      FSelectedStore.Angle := ArcTan(lVect.X / lVect.Y);
      if lVect.Y>0 then
        FSelectedStore.Angle := FSelectedStore.Angle + PI;
    end
    else if lVect.X>0 then
      FSelectedStore.Angle := 0-(PI/2)
    else
      FSelectedStore.Angle := 0-(PI * 1.5);
    Invalidate;
    Dirty := True;
  end;
end;  // TStoreLayoutDiagram.DragRotateHandle 

{-------------------------------------------------------------------------------
  Recalculates the size of the store box when a sizing grab handle is dragged. 
}
procedure TStoreLayoutDiagram.DragSizeHandle(Sender: TObject; Shift: TShiftState; X, Y:
    Integer);
var
  a, b, c, d, e: Double;
begin
  if Assigned(FSelectedStore) and (Sender is TGrabHandle) then begin
    a := TGrabHandle(Sender).Left - FSelectedStore.DrawCentreX + HANDLE_SIZE;
    b := TGrabHandle(Sender).Top - FSelectedStore.DrawCentreY + HANDLE_SIZE;
    c := X - a - FSelectedStore.DrawCentreX;
    d := Y - b - FSelectedStore.DrawCentreY;
    e := (a*c + b*d) / Sqrt(Sqr(a) + Sqr(b));
    // Resize in X axis unless grab handle is top or bottom
    if (Sender <> FSelectedStore.SizeHandle[1]) and
       (Sender <> FSelectedStore.SizeHandle[5]) then begin
      FSelectedStore.Width := Max(10, FSelectedStore.Width + Trunc(e));
    end;
    // Resize in Y axis unless grab handle is left or right
    if (Sender <> FSelectedStore.SizeHandle[3]) and
       (Sender <> FSelectedStore.SizeHandle[7]) then
      FSelectedStore.Height := Max(10, FSelectedStore.Height + Trunc(e));
    SnapToGrid;
    // Force repaint to update positions of grab handles.
    Repaint;
    Dirty := True;
  end;
end;  // TStoreLayoutDiagram.DragSizeHandle 

{-------------------------------------------------------------------------------
  Because the diagram is centred on the viewpoint, we must calculate the offset for the grid
      so that the centre of the viewpoint has a grid dot.  Return the X offset.
}
function TStoreLayoutDiagram.GridOffsetX: Integer;
begin
  Result := (FDiagram.Width div 2) mod FDiagramDisplayProperties.GridSize;
end;  // TStoreLayoutDiagram.GridOffsetX 

{-------------------------------------------------------------------------------
  Because the diagram is centred on the viewpoint, we must calculate the offset for the grid
      so that the centre of the viewpoint has a grid dot.  Return the Y offset.
}
function TStoreLayoutDiagram.GridOffsetY: Integer;
begin
  Result := (FDiagram.Height div 2) mod FDiagramDisplayProperties.GridSize;
end;  // TStoreLayoutDiagram.GridOffsetY 

{-------------------------------------------------------------------------------
}
procedure TStoreLayoutDiagram.InitialiseObjects;
begin
  inherited;
  FDiagramDisplayProperties := TDiagramDisplayProperties.Create(self);
  FDiagramDisplayProperties.ShowTitle := False;
end;  // TStoreLayoutDiagram.InitialiseObjects 

{-------------------------------------------------------------------------------
  Ensures that the selected store has not been dragged off the page. 
}
procedure TStoreLayoutDiagram.KeepSelectedStoreOnPage;
var
  lRect: TShapeCoords;
  lBoundsRect: TRect;
  i: Integer;
begin
  if Assigned(FSelectedStore) then begin
    FSelectedStore.CreateStoreRect(FViewPoint, FZoom,
        Point(FDiagram.Width, FDiagram.Height), lRect);
    // Find the outer bounding rectangle of the shape
    lBoundsRect := Rect(lRect[0].X, lRect[0].Y, lRect[0].X, lRect[0].Y);
    for i := 1 to High(lRect) do begin
      lBoundsRect.Left   := Min(lBoundsRect.Left, lRect[i].X);
      lBoundsRect.Right  := Max(lBoundsRect.Right, lRect[i].X);
      lBoundsRect.Top    := Min(lBoundsRect.Top, lRect[i].Y);
      lBoundsRect.Bottom := Max(lBoundsRect.Bottom, lRect[i].Y);
    end;
    if lBoundsRect.Left<0 then
      FSelectedStore.PosX := FSelectedStore.PosX - lBoundsRect.Left;
    if lBoundsRect.Right > FDiagram.Width then
      FSelectedStore.PosX := FSelectedStore.PosX + FDiagram.Width - lBoundsRect.Right;
    if lBoundsRect.Top<0 then
      FSelectedStore.PosY := FSelectedStore.PosY - lBoundsRect.Top;
    if lBoundsRect.Bottom > FDiagram.Height then
      FSelectedStore.PosY := FSelectedStore.PosX + FDiagram.Height - lBoundsRect.Bottom;
  end;
end;  // TStoreLayoutDiagram.KeepSelectedStoreOnPage

{-------------------------------------------------------------------------------
}
procedure TStoreLayoutDiagram.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited;
  if not ReadOnly then DoKeyDown(Key, Shift);
end;  // TStoreLayoutDiagram.KeyDown

{-------------------------------------------------------------------------------
  Reads the diagram from an XML string.
}
procedure TStoreLayoutDiagram.ReadXML(const AXML: string);
var
  lStore: TStore;
var
  lXMLDoc: IXMLDocument;
  i: Integer;
begin
  // clear the existing stores list
  FStores.Clear;
  // reset the diagram display
  FDiagramDisplayProperties.Free;
  FDiagramDisplayProperties := TDiagramDisplayProperties.Create(self);
  FDiagramDisplayProperties.ShowTitle := False;
  if AXML<>'' then begin
    lXMLDoc := TXMLDocument.Create(nil);
    with lXMLDoc do begin
      LoadFromXML(AXML);
      if HasChildNode(DocumentElement, EL_DIAGRAMDISPLAY) then
          FDiagramDisplayProperties.ReadXML(
              DocumentElement.ChildNodes[EL_DIAGRAMDISPLAY]);
      for i := 0 to DocumentElement.ChildNodes.Count-1 do
        if DocumentElement.ChildNodes[i].NodeName=EL_STORE then begin
          lStore := CreateStore;
          with lStore do begin
            ReadXML(DocumentElement.ChildNodes[i]);
            try
              LoadCaption(Zoom);
            except
              on E:EStoreLayoutDiagram do
                if E.Message=ResStr_StoreDeleted then
                  FStores.Remove(lStore);
                else
                  raise;
            end; // try
          end; // with
        end; // if
    end; // with lXMLDoc
  end; // if AXML<>''
  TScrollbox(Self).HorzScrollBar.Position := 0;
  TScrollbox(Self).VertScrollBar.Position := 0;
  Invalidate;
end;  // TStoreLayoutDiagram.ReadXML

{-------------------------------------------------------------------------------
  Accessor, disables internal diagram if readonly.
}
procedure TStoreLayoutDiagram.SetReadOnly(Value: Boolean);
begin
  FReadOnly := Value;
  // Popup menu not active when read only
  if not FReadOnly then begin
    PopupMenu.Free;
    PopupMenu := nil;
  end;
end;  // TStoreLayoutDiagram.SetReadOnly

{-------------------------------------------------------------------------------
  Accessor method 
}
procedure TStoreLayoutDiagram.SetStoreListBox(Value: TIDListBox);
begin
  FStoreListBox := Value;
end;  // TStoreLayoutDiagram.SetStoreListBox 

{-------------------------------------------------------------------------------
}
procedure TStoreLayoutDiagram.SetZoom(const Value: Double);
begin
  FZoom := Value;
  if Value<>1 then
    FDiagramDisplayProperties.ShowGrid := False;
  SetPageSize(FZoom);
end;  // TStoreLayoutDiagram.SetZoom 

{-------------------------------------------------------------------------------
  If snap to grid is turned on, then the selected store is realigned so that either the left
      or right, and either the top or bottom, lock to a grid coordinate.
}
procedure TStoreLayoutDiagram.SnapToGrid;
begin
  if FDiagramDisplayProperties.SnapToGrid and Assigned(FSelectedStore) then
    with FDiagramDisplayProperties do begin
      // snap the size
      FSelectedStore.Width := ((FSelectedStore.Width + GridSize div 2) div GridSize) *
          GridSize;
      FSelectedStore.Height := ((FSelectedStore.Height + GridSize div 2) div GridSize) *
          GridSize;
      // snap the location
      if FSelectedStore.PosX<0 then
        FSelectedStore.PosX := 0-((Abs(FSelectedStore.PosX) + GridSize div 2) div GridSize)
            * GridSize
      else
        FSelectedStore.PosX := ((FSelectedStore.PosX + GridSize div 2) div GridSize) *
            GridSize;
      if FSelectedStore.PosY<0 then
        FSelectedStore.PosY := 0-((Abs(FSelectedStore.PosY) + GridSize div 2) div GridSize)
            * GridSize
      else
        FSelectedStore.PosY := ((FSelectedStore.PosY + GridSize div 2) div GridSize) *
            GridSize;
    end;
end;  // TStoreLayoutDiagram.SnapToGrid 

{-------------------------------------------------------------------------------
  Retrieves the store at position x,y, or nil if no store at the position. 
}
function TStoreLayoutDiagram.StoreAtPos(AX, AY: integer): TStore;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to FStores.Count-1 do
    if TStore(FStores[i]).IsHit(AX, AY) then begin
      Result := TStore(FStores[i]);
      Break;
   end;
end;  // TStoreLayoutDiagram.StoreAtPos 

{-------------------------------------------------------------------------------
}
procedure TStoreLayoutDiagram.StorePropsClick(Sender: TObject);
begin
  if Assigned(FSelectedStore) then
    with TdlgStoreDisplayProperties.Create(FDiagram) do
      try
        DisplayProperties := FSelectedStore.DisplayProperties;
        if ShowModal = mrOk then
          Dirty := True;
      finally
        Free;
      end;
end;  // TStoreLayoutDiagram.StorePropsClick 

{-------------------------------------------------------------------------------
  Writes the specification of the diagram into the XML node. 
}
function TStoreLayoutDiagram.WriteXML: String;
var
  lXMLDoc: IXMLDocument;
  i: Integer;
begin
  lXMLDoc := NewXMLDocument;
  // root node is always 'store_diagram'
  lXMLDoc.AddChild(EL_STOREDIAGRAM);
  DiagramDisplayProperties.WriteXML(lXMLDoc.DocumentElement);
  for i := 0 to FStores.Count-1 do
    TStore(FStores[i]).WriteXML(lXMLDoc.DocumentElement);
  Result := lXmlDoc.XML.Text;
end;  // TStoreLayoutDiagram.WriteXML 

{ TStoreDisplayProperties }

{-==============================================================================
    TStoreDisplayProperties
===============================================================================}
{-------------------------------------------------------------------------------
}
function TStoreDisplayProperties.GetXMLNodeName: String;
begin
  Result := EL_STOREDISPLAY;
end;  // TStoreDisplayProperties.GetXMLNodeName 

function TStoreLayoutDiagram.GetInternalHeight: integer;
begin
  Result := FDiagram.Height;
end;

function TStoreLayoutDiagram.GetInternalWidth: integer;
begin
  Result := FDiagram.Width;
end;

procedure TStoreLayoutDiagram.SetOnNavigate(const Value: TNavigateEvent);
begin
  FOnNavigate := Value;
end;

end.
