unit Diagram;

interface

uses
  SysUtils, Windows, Messages, Classes, Graphics, Controls, Forms, Dialogs,
  DiagramObjects, Menus;

resourcestring
  ResStr_DiagramSettings = 'Diagram Settings...';
  ResStr_ImageFileFilters =
      'Bitmap files|*.bmp|JPEG files|*.jpg|GIF files|*.gif';
  ResStr_PrintingDiagram = 'Printing Diagram...';

type
  TDiagram = class;

  TInternalDiagram = class(TCustomControl)
  private
    FDiagramContainer: TDiagram;
    FMousePos: TPoint;
    procedure SetDiagramContainer(Value: TDiagram);
  protected
    procedure DblClick; override;
    procedure DragOver(Source: TObject; X, Y: Integer; State: TDragState; var
        Accept: Boolean); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y:
        Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
        override;
    procedure Paint; override;
  public
    procedure DragDrop(Source: TObject; X, Y: Integer); override;
    property DiagramContainer: TDiagram read FDiagramContainer write
        SetDiagramContainer;
  end;

  TDiagram = class(TScrollbox)
  private
    FPageSizeSet: Boolean;
    FDirty: boolean;
    procedure DrawGrid(ACanvas: TCanvas);
    function GetCanvas: TCanvas;
    function GetImage: TBitmap;
    function GetPrintSettings: Integer;
    procedure SetDirty(const Value: boolean);
  protected
    FDiagram: TInternalDiagram;
    FDiagramDisplayProperties: TDiagramDisplayProperties;
    FFileName: string;
    FMouseDownItemStartPosition: TPoint;
    FMouseDownPoint: TPoint;
    procedure BeginDragOperation(AX, AY: integer); virtual;
    procedure CreatePopupMenu; virtual;
    procedure CreateWnd; override;
    procedure DiagramSettingsClick(Sender: TObject); virtual;
    procedure DoDblClick(X, Y: Integer); virtual;
    procedure DoDragDrop(Source: TObject; X, Y: Integer); virtual;
    procedure DoDragOver(Source: TObject; X, Y: Integer; State: TDragState; var
        Accept: Boolean); virtual;
    procedure DoKeyDown(var Key: Word; Shift: TShiftState); virtual; abstract;
    procedure DoMouseDown(Button: TMouseButton; Shift: TShiftState; X, Y:
        Integer); virtual; abstract;
    procedure DoMouseMove(Shift: TShiftState; X, Y: Integer); virtual;
    procedure DoMouseUp(Button: TMouseButton; Shift: TShiftState; X, Y:
        Integer); virtual;
    procedure DoPaint; virtual;
    function GridOffsetX: Integer; virtual;
    function GridOffsetY: Integer; virtual;
    procedure InitialiseObjects; virtual;
    procedure RequestFocus;
    procedure SetPageSize(const AZoom: double);
  public
    constructor Create(AOwner: TComponent); overload; override;
    destructor Destroy; override;
    procedure Invalidate; override;
    procedure Print;
    procedure SaveImg;
    property Canvas: TCanvas read GetCanvas;
    property DiagramDisplayProperties: TDiagramDisplayProperties read
        FDiagramDisplayProperties;
    property FileName: string read FFileName;
    property Dirty: boolean read FDirty write SetDirty;
  end;
  

implementation

uses
  ResourceStrings, Printers, JPEG, GeneralFunctions, DiagramSettings,
  GeneralData, GIFImage;

{-==============================================================================
    TInternalDiagram
===============================================================================}
{-------------------------------------------------------------------------------
  Forward the double click events to the diagram. 
}
procedure TInternalDiagram.DblClick;
begin
  inherited;
  DiagramContainer.DoDblClick(FMousePos.X, FMousePos.Y);
end;  // TInternalDiagram.DblClick 

{-------------------------------------------------------------------------------
  Forward the drag drop events to the diagram. 
}
procedure TInternalDiagram.DragDrop(Source: TObject; X, Y: Integer);
begin
  inherited;
  DiagramContainer.DoDragDrop(Source, X, Y);
end;  // TInternalDiagram.DragDrop 

{-------------------------------------------------------------------------------
  Forward the drag over events to the diagram. 
}
procedure TInternalDiagram.DragOver(Source: TObject; X, Y: Integer; State:
    TDragState; var Accept: Boolean);
begin
  inherited;
  DiagramContainer.DoDragOver(Source, X, Y, State, Accept);
end;  // TInternalDiagram.DragOver 

{-------------------------------------------------------------------------------
  Forward the key down events to the diagram. 
}
procedure TInternalDiagram.KeyDown(var Key: Word; Shift: TShiftState);
begin
  DiagramContainer.DoKeyDown(Key, Shift);
end;  // TInternalDiagram.KeyDown 

{-------------------------------------------------------------------------------
  Forward the mouse down events to the diagram. 
}
procedure TInternalDiagram.MouseDown(Button: TMouseButton; Shift: TShiftState;
    X, Y: Integer);
begin
  DiagramContainer.DoMouseDown(Button, Shift, X, Y);
end;  // TInternalDiagram.MouseDown 

{-------------------------------------------------------------------------------
  Track drag operations if the mouse moves over the control.  Forward the mouse
      move events to the diagram.
}
procedure TInternalDiagram.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  FMousePos := Point(X, Y);
  DiagramContainer.DoMouseMove(Shift, X, Y);
end;  // TInternalDiagram.MouseMove 

{-------------------------------------------------------------------------------
  Cancel concept drag operations when mouse released.  Forward the mouse move
      events to the diagram.
}
procedure TInternalDiagram.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
    Y: Integer);
begin
  DiagramContainer.DoMouseUp(Button, Shift, X, Y);
end;  // TInternalDiagram.MouseUp 

{-------------------------------------------------------------------------------
  Forward the paint events to the diagram. 
}
procedure TInternalDiagram.Paint;
begin
  DiagramContainer.DoPaint;
end;  // TInternalDiagram.Paint 

{-------------------------------------------------------------------------------
  Accessor, specifies the scrollbox that contains the diagram. 
}
procedure TInternalDiagram.SetDiagramContainer(Value: TDiagram);
begin
  FDiagramContainer := Value;
end;  // TInternalDiagram.SetDiagramContainer 

{-==============================================================================
    TDiagram
===============================================================================}
{-------------------------------------------------------------------------------
  Object initialisation.  If a concept key is passed in, then a concept is
      created in the centre of the diagram.
}
constructor TDiagram.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  InitialiseObjects;
end;  // TDiagram.Create 

{-------------------------------------------------------------------------------
}
destructor TDiagram.Destroy;
begin
  FDiagramDisplayProperties.Free;
  
  inherited Destroy;
end;  // TDiagram.Destroy 

{-------------------------------------------------------------------------------
  Records the starting position of a drag operation on the title or a concept. 
}
procedure TDiagram.BeginDragOperation(AX, AY: integer);
begin
  FMouseDownPoint := Point(AX, AY);
  // note the start position of the object being dragged
  if DiagramDisplayProperties.TitleSelected then
    FMouseDownItemStartPosition := Point(DiagramDisplayProperties.TitlePosX,
      DiagramDisplayProperties.TitlePosY);
end;  // TDiagram.BeginDragOperation 

{-------------------------------------------------------------------------------
  Creates the popup menu that appears on the diagram. 
}
procedure TDiagram.CreatePopupMenu;
  
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
  PopupMenu := TPopupMenu.Create(Self);
  AddMenuItem(ResStr_DiagramSettings, DiagramSettingsClick);
end;  // TDiagram.CreatePopupMenu 

{-------------------------------------------------------------------------------
  Once we have a handle, we can set the page size to A4 because we can get the
      pixels per inch.
}
procedure TDiagram.CreateWnd;
begin
  inherited;
  if not FPageSizeSet then begin
    SetPageSize(1);
  end;
end;  // TDiagram.CreateWnd 

{-------------------------------------------------------------------------------
  Display the diagram settings dialog. 
}
procedure TDiagram.DiagramSettingsClick(Sender: TObject);
var
  lDialog: TdlgDiagramSettings;
begin
  lDialog := TdlgDiagramSettings.Create(nil, DiagramDisplayProperties);
  with lDialog do
    try
      if ShowModal=mrOk then
        Dirty := true;
    finally
      Free;
    end; //
end;  // TDiagram.DiagramSettingsClick 

{-------------------------------------------------------------------------------
}
procedure TDiagram.DoDblClick(X, Y: Integer);
begin
  // No implementation - override if required.
end;  // TDiagram.DoDblClick 

{-------------------------------------------------------------------------------
}
procedure TDiagram.DoDragDrop(Source: TObject; X, Y: Integer);
begin
  // no implementation
end;  // TDiagram.DoDragDrop 

{-------------------------------------------------------------------------------
}
procedure TDiagram.DoDragOver(Source: TObject; X, Y: Integer; State: TDragState;
    var Accept: Boolean);
begin
  // no implementation
end;  // TDiagram.DoDragOver 

{-------------------------------------------------------------------------------
  Track drag operations if the mouse moves over the control. 
}
procedure TDiagram.DoMouseMove(Shift: TShiftState; X, Y: Integer);
begin
  if ssLeft in Shift then begin
    if (Abs(X-FMouseDownPoint.X)>4) or (Abs(Y-FMouseDownPoint.Y)>4) then begin
      if DiagramDisplayProperties.TitleSelected then begin
        with DiagramDisplayProperties do begin
          TitlePosX := FMouseDownItemStartPosition.X + X - FMouseDownPoint.X;
          TitlePosY := FMouseDownItemStartPosition.Y + Y - FMouseDownPoint.Y;
          // Ensure title stays inside the diagram.
          TitlePosX := Min(FDiagram.Width-TitleWidth, TitlePosX);
          TitlePosX := Max(0, TitlePosX);
          TitlePosY := Min(FDiagram.Height-TitleHeight, TitlePosY);
          TitlePosY := Max(0, TitlePosY);
          Dirty := True;
        end; // with
      end;
    end;
  end;
end;  // TDiagram.DoMouseMove 

{-------------------------------------------------------------------------------
  Cancel concept drag operations when mouse released. 
}
procedure TDiagram.DoMouseUp(Button: TMouseButton; Shift: TShiftState; X, Y:
    Integer);
begin
  // No implementation - override if required
end;  // TDiagram.DoMouseUp 

{-------------------------------------------------------------------------------
  Draw the diagram grid. 
}
procedure TDiagram.DoPaint;
begin
  DrawGrid(Canvas);
end;  // TDiagram.DoPaint 

{-------------------------------------------------------------------------------
  Fills the diagram background, draws the title and the grid, if required. 
}
procedure TDiagram.DrawGrid(ACanvas: TCanvas);
var
  lX: Integer;
  lY: Integer;
begin
  // Fill backgroud
  Canvas.Brush.Color := DiagramDisplayProperties.Color;
  Canvas.FillRect(Rect(0, 0, FDiagram.Width, FDiagram.Height));
  with DiagramDisplayProperties do begin
    // Draw grid if required
    if ShowGrid then begin
      Canvas.Pen.Color := clWindowText;
      Canvas.Pen.Width := 1;
      for lY := 0 to FDiagram.Height div GridSize do
        for lX := 0 to FDiagram.Width div GridSize do begin
          Canvas.MoveTo(lX * GridSize + GridOffsetX, lY * GridSize +
              GridOffsetY);
          Canvas.LineTo(lX * GridSize + GridOffsetX, lY * GridSize+1 +
              GridOffsetY);
        end;
    end;
    // Draw title if required
    if ShowTitle then begin
      Canvas.Font.Color := MergeColours(Color, clWindowText, 50);
      Canvas.Font.Assign(TitleFont);
      TitleWidth := Canvas.TextWidth(Title);
      TitleHeight := Canvas.TextHeight(Title);
      if TitleSelected then
        Canvas.DrawFocusRect(Rect(
            TitlePosX - 2,
            TitlePosY - 2,
            TitlePosX + TitleWidth + 2,
            TitlePosY + TitleHeight + 2));
      Canvas.TextOut(TitlePosX, TitlePosY, Title);
    end;
  end;
end;  // TDiagram.DrawGrid 

{-------------------------------------------------------------------------------
  Accessor for the diagram component's canvas. 
}
function TDiagram.GetCanvas: TCanvas;
begin
  Result := FDiagram.Canvas;
end;  // TDiagram.GetCanvas 

{-------------------------------------------------------------------------------
  Retrieve the diagram as a bitmap. 
}
function TDiagram.GetImage: TBitmap;
begin
  Result := TBitmap.Create;
  try
    Result.Width := FDiagram.ClientWidth;
    Result.Height := FDiagram.ClientHeight;
    Result.Canvas.Brush := Brush;
    Result.Canvas.FillRect(ClientRect);
    Result.Canvas.Lock;
    try
      FDiagram.PaintTo(Result.Canvas.Handle, 0, 0);
    finally
      Result.Canvas.Unlock;
    end;
  except
    Result.Free;
    raise;
  end;
end;  // TDiagram.GetImage 

{-------------------------------------------------------------------------------
  Allow the user to select the printer.  Result is the number of copies to
      print.
}
function TDiagram.GetPrintSettings: Integer;
begin
  with TPrintDialog.Create(nil) do
    try
      Options := [poWarning, poHelp];
      if Execute then
        Result := Copies
      else
        Result := 0;
    finally
      Free;
    end; // try
end;  // TDiagram.GetPrintSettings 

{-------------------------------------------------------------------------------
  Return the offset for the grid from the left.  Used if the grid does not
      align with the diagram edge.
}
function TDiagram.GridOffsetX: Integer;
begin
  Result := 0;
end;  // TDiagram.GridOffsetX 

{-------------------------------------------------------------------------------
  Return the offset for the grid from the top.  Used if the grid does not align
      with the diagram edge.
}
function TDiagram.GridOffsetY: Integer;
begin
  Result := 0;
end;  // TDiagram.GridOffsetY 

{-------------------------------------------------------------------------------
  Initialise owned objects 
}
procedure TDiagram.InitialiseObjects;
begin
  // Embed the diagram onto a scrollbox
  FDiagram := TInternalDiagram.Create(Self);
  FDiagram.Parent := Self;
  FDiagram.DiagramContainer := Self;
  FDiagram.DoubleBuffered := True;
  FPageSizeSet := false;
  TabStop := True;
  CreatePopupMenu;
  FDirty := False;
end;  // TDiagram.InitialiseObjects 

{-------------------------------------------------------------------------------
  Override invalidate to ensure the diagram itself is invalidated. 
}
procedure TDiagram.Invalidate;
begin
  inherited;
  FDiagram.Invalidate;
end;  // TDiagram.Invalidate 

{-------------------------------------------------------------------------------
  Prints the diagram 
}
procedure TDiagram.Print;
var
  FormImage: TBitmap;
  Info: PBitmapInfo;
  InfoSize: DWORD;
  Image: Pointer;
  ImageSize: DWORD;
  Bits: HBITMAP;
  DIBWidth, DIBHeight: LongInt;
  PrintWidth, PrintHeight: LongInt;
  lPixelsPerInch: Integer;
  lNumCopies: Integer;
begin
  lNumCopies := GetPrintSettings;
  if lNumCopies >0 then begin
    Printer.Copies := lNumCopies;
    Printer.BeginDoc;
    lPixelsPerInch := GetDeviceCaps(Canvas.Handle,LOGPIXELSY);
    dmGeneral.Recorder.RecorderMainForm.StatusText := ResStr_PrintingDiagram;
    try
      FormImage := GetImage;
      Canvas.Lock;
      try
        { Paint bitmap to the printer }
        Bits := FormImage.Handle;
        GetDIBSizes(Bits, InfoSize, ImageSize);
        Info := AllocMem(InfoSize);
        try
          Image := AllocMem(ImageSize);
          try
            GetDIB(Bits, 0, Info^, Image^);
            with Info^.bmiHeader do
            begin
              DIBWidth := biWidth;
              DIBHeight := biHeight;
            end;
            PrintWidth := MulDiv(DIBWidth, GetDeviceCaps(Printer.Handle,
              LOGPIXELSX), lPixelsPerInch);
            PrintHeight := MulDiv(DIBHeight, GetDeviceCaps(Printer.Handle,
              LOGPIXELSY), lPixelsPerInch);
            StretchDIBits(Printer.Canvas.Handle, 0, 0, PrintWidth, PrintHeight,
                0,
                0,
              DIBWidth, DIBHeight, Image, Info^, DIB_RGB_COLORS, SRCCOPY);
          finally
            FreeMem(Image, ImageSize);
          end;
        finally
          FreeMem(Info, InfoSize);
        end;
      finally
        Canvas.Unlock;
        FormImage.Free;
      end;
    finally
      Printer.EndDoc;
      dmGeneral.Recorder.RecorderMainForm.StatusText := '';
    end;
  end;
end;  // TDiagram.Print 

{-------------------------------------------------------------------------------
  Focus this control, by searching for the form it is embedded onto and setting
      its active control.
}
procedure TDiagram.RequestFocus;
begin
  ValidParentForm(Self).ActiveControl := Self;
  ValidParentForm(Self).ActiveControl := FDiagram;
  if ValidParentForm(Self).ActiveControl <> FDiagram then
    dmGeneral.Log('Nothing worked')
  else
    dmGeneral.Log('ActiveControl set');
  if FDiagram.Focused then
    dmGeneral.Log('Self focused');
  if ValidParentForm(Self).Active then
    dmGeneral.Log('Form is active');
end;  // TDiagram.RequestFocus 

{-------------------------------------------------------------------------------
  Saves the diagram to an image file format specified by the user (jpg, bmp or
      gif).
}
procedure TDiagram.SaveImg;
var
  lBmp: TBitmap;
  lGraphic: TGraphic;
  
  const
    SPACING = 10;   // pixels gap around the edge
  
begin
  with TSaveDialog.Create(nil) do
    try
      Filter := ResStr_ImageFileFilters;
      if Execute then begin
        // Prepare a bitmap
        lBmp := GetImage;
        try
          case FilterIndex of
            1: begin // BMP
              if ExtractFileExt(FileName)='' then FileName := FileName + '.bmp';
              lGraphic := lBmp;
            end;
            2: begin // JPG
              if ExtractFileExt(FileName)='' then FileName := FileName + '.jpg';
              lGraphic:= TJPEGImage.Create;
              lGraphic.Assign(lBmp);
            end;
            else begin // GIF
              if ExtractFileExt(FileName)='' then FileName := FileName + '.gif';
              lGraphic:= TGIFImage.Create;
              lGraphic.Assign(lBmp);
            end;
          end; // case
          lGraphic.SaveToFile(FileName);
          // If we created a graphic to assign the bitmap to, then free it
          if lGraphic <> lBmp then
            lGraphic.Free;
        finally
          lBmp.Free;
        end; // try
      end;
    finally
      Free;
    end;
end;  // TDiagram.SaveImg 

{-------------------------------------------------------------------------------
}
procedure TDiagram.SetPageSize(const AZoom: double);
var
  lWidthInInches: Double;
  lHeightInInches: Double;
begin
  // Set page size to A4, or to the printer page size if there is one
  lWidthInInches := 8.27;
  lHeightInInches := 11.69;
  if Printer.Printers.Count>0 then
    try
      with Printer do begin
        lWidthInInches := PageWidth / GetDeviceCaps(Handle, LOGPIXELSX);
        lHeightInInches := PageHeight / GetDeviceCaps(Handle, LOGPIXELSY);
      end
    except
      on Exception do ; // ignore if the default printer is not connected
    end;
  FDiagram.Width := Trunc(GetDeviceCaps(FDiagram.Canvas.Handle, LOGPIXELSX) *
      lWidthInInches * AZoom);
  FDiagram.Height := Trunc(GetDeviceCaps(FDiagram.Canvas.Handle, LOGPIXELSY) *
      lHeightInInches * AZoom);
  FPageSizeSet := True;
end;  // TDiagram.SetPageSize



procedure TDiagram.SetDirty(const Value: boolean);
begin
  FDirty := Value;
end;

end.
