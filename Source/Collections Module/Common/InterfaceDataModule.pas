{===============================================================================
  Unit:        InterfaceDataModule

  Defines:     TdmInterface

  Description: Data module for interface stuff such as image lists

  Created:     July 2003

  Model:       <none>

  Last revision information:
    $Revision: 22 $
    $Date: 27/09/10 16:19 $
    $Author: Robertjohnson $

===============================================================================}

unit InterfaceDataModule;

interface

uses
  SysUtils, Classes, ImgList, Controls, Graphics, Windows, GeneralFunctions,
  Menus, RapTree, ExGrid, StrUtils;

type
  {-----------------------------------------------------------------------------
    Class to hold details of a single concept rank
  }
  TRank = class (TObject)
  private
    FAbbreviation: string;
    FColour: TColor;
    FSortOrder: integer;
    FRankKey: string;
    procedure SetAbbreviation(const Value: string);
    procedure SetColour(Value: TColor);
    procedure SetSortOrder(const Value: integer);
    procedure SetRankKey(const Value: string);
  public
    property Abbreviation: string read FAbbreviation write SetAbbreviation;
    property RankKey: string read FRankKey write SetRankKey;
    property Colour: TColor read FColour write SetColour;
    property SortOrder: integer read FSortOrder write SetSortOrder;
  end;

  {-----------------------------------------------------------------------------
    Interface data module class
  }
  TdmInterface = class(TDataModule)
    ilButtons: TImageList;
    ilBrowserNodes: TImageList;
    ilOtherNodes: TImageList;
    ilMenuItems: TImageList;
  private
    function GetAlignedTextRect(ACanvas: TCanvas; ARect: TRect;
        const AText: string; AAlignment: TAlignment;ABorderWidth: integer): TRect;
    function PaintWord(ACanvas: TCanvas; AX, AY: integer; const ATerm: string;
             ACalcRect: boolean): integer;
  public
    procedure DrawRank(const ACanvas: TCanvas; const ARank: TRank; var ARect:
        TRect; AAlignment: TAlignment);
    procedure DrawRankDot(const ACanvas: TCanvas; var ARect: TRect; const
        AColor: TColor);
    procedure DrawTerm(ACanvas: TCanvas; ARect: TRect; const ATerm: string;
        ASelected: boolean);
    procedure DrawWrappedTerm(ACanvas: TCanvas; var ARect: TRect;
      const ATerm: string; ASelected, ACalcRect: boolean);
    procedure TermMenuDrawItem(Sender: TObject; ACanvas: TCanvas;
        ARect: TRect; Selected: Boolean);
    procedure RepaintNode(ANode: TFlyNode; ATree: TRapidTree);
  end;

var
  dmInterface: TdmInterface;

//==============================================================================
implementation

{$R *.dfm}

type
  TRapidTreeAccessor = class(TRapidTree);

{-==============================================================================
    TRank
===============================================================================}
{-------------------------------------------------------------------------------
  Accessor method
}
procedure TRank.SetAbbreviation(const Value: string);
begin

  FAbbreviation := Value;
end;  // TRank.SetAbbreviation

{-------------------------------------------------------------------------------
  Accessor method
}
procedure TRank.SetColour(Value: TColor);
begin
  FColour := Value;
end;  // TRank.SetColour

{-------------------------------------------------------------------------------
  Accessor method
}
procedure TRank.SetRankKey(const Value: string);
begin
  FRankKey := Value;
end;

{-------------------------------------------------------------------------------
  Accessor method
}
procedure TRank.SetSortOrder(const Value: integer);
begin
  FSortOrder := Value;
end;


{-==============================================================================
    TdmInterface
===============================================================================}
{-------------------------------------------------------------------------------
  Draws a rank symbol onto the canvas.  Updates the rect.Left to allow for the
      space occupied by the symbol.
}
procedure TdmInterface.DrawRank(const ACanvas: TCanvas; const ARank:
    TRank; var ARect: TRect; AAlignment: TAlignment);
var
  lRankRect: TRect;
  lOrigBrushColour: TColor;
  lOrigFontColour: TColor;
  lOrigFontName: string;
  lOrigFontSize: integer;
begin
  with ACanvas do begin
    lOrigFontName := Font.Name;
    Font.Name := 'Small Fonts';
    lOrigFontSize := Font.Size;
    Font.Size := 6;
    lOrigBrushColour := Brush.Color;
    Brush.Color := ARank.Colour;
    lOrigFontColour := Font.Color;
    Font.Color := GetContrastColour(ARank.Colour);
    Pen.Color := clWindowText;
    try
      lRankRect := GetAlignedTextRect(ACanvas, ARect, ARank.Abbreviation, AAlignment, 3);
      Rectangle(lRankRect);
      TextOut(lRankRect.Left+2, lRankRect.Top + 1, ARank.Abbreviation);
      ARect.Left := lRankRect.Right + 3;
    finally
      Font.Name := lOrigFontName;
      Font.Size := lOrigFontSize;
      Brush.Color := lOrigBrushColour;
      Font.Color := lOrigFontColour;
    end; // try
  end; // with aCanvas
end;  // TfraThesaurusNavigator.DrawRank


{-------------------------------------------------------------------------------
  Draw a dot onto the canvas for the nomenclatural status rank's colour.
  Moves the rectangle left position.
}
procedure TdmInterface.DrawRankDot(const ACanvas: TCanvas;
  var ARect: TRect; const AColor: TColor);
var
  lOrigBrushColor: TColor;
  lOrigPenColor: TColor;
const
  CIRCLE_SIZE=6;
begin
  with ACanvas do begin
    lOrigBrushColor := Brush.Color;
    lOrigPenColor := Brush.Color;
    try
      Brush.Color := AColor;
      Pen.Color := AColor;
      Ellipse(ARect.Left-CIRCLE_SIZE-3,
              ARect.Top + (ARect.Bottom-ARect.Top) div 2 - CIRCLE_SIZE div 2-5,
              ARect.Left-3,
              ARect.Top + (ARect.Bottom-ARect.Top) div 2 + CIRCLE_SIZE div 2-5);
    finally
      Brush.Color := lOrigBrushColor;
      Pen.Color := lOrigPenColor;
    end; // try
  end;
end;


{-------------------------------------------------------------------------------
  Draws a term onto the canvas, allowing for the <i> formatting
}
procedure TdmInterface.DrawTerm(ACanvas: TCanvas; ARect: TRect;
  const ATerm: string; ASelected: boolean);
var
  lText, lOutText, lTag, lFontName: String;
  i, lWidth, lTagPos, lXPos: Integer;
  lList: TStringList;
  lItalicState: Boolean;

  procedure SetItalic(Active: Boolean);
  begin
    with ACanvas.Font do
      if Active then begin
        if CompareText(Name, 'MS Sans Serif') = 0 then Name := 'Arial';
        Style := Style + [fsItalic];
      end else begin
        Name := lFontName;
        Style := Style - [fsItalic];
      end;
  end;

  procedure SetBold(Active: Boolean);
  begin
    with ACanvas.Font do
      if Active then begin
        Style := Style + [fsBold];
      end else begin
        Style := Style - [fsBold];
      end;
  end;

begin
  lList := TStringList.Create;
  try
    // If there is a <b/> tag at the start of the item, then display it in bold.
    lText := ATerm;
    if AnsiLeftStr(lText, 4) = '<b/>' then
    begin
      lText := StringReplace(lText, '<b/>', '', [rfIgnoreCase]);
      SetBold(true);
    end;
    lFontName := ACanvas.Font.Name;

    lTag := '<i>';
    lTagPos := Pos(lTag, lText);
    lItalicState := lTagPos = 1;
    while lTagPos > 0 do begin
      // Don't add empty items.
      if lTagPos > 1 then lList.Add(Copy(lText, 1, lTagPos - 1));
      lText := Copy(lText, lTagPos + Length(lTag), 255);
      if lTag = '<i>' then lTag := '</i>' else lTag := '<i>';
      lTagPos := Pos(lTag, lText);
    end;
    // Add last one, if anything.
    if lText <> '' then lList.Add(lText);

    SetItalic(lItalicState);
    ACanvas.FillRect(ARect);
    lXPos := ARect.Left + 1;
    i := 0;
    while (i < lList.Count) do begin
      lOutText := GetTextWithinLimit(ACanvas, lList[i], ARect.Right - lXPos);
      lWidth :=  ACanvas.TextWidth(lList[i]);
      // Can't fit more text in.
      if (lXPos + lWidth > ARect.Right) or (RightStr(lOutText, 3) = '...') then begin
        ACanvas.TextOut(lXPos, ARect.Top, lOutText);
        Exit
      end else begin
        // Check next part can be at least partially displayed.
        if i < lList.Count - 1 then begin
          SetItalic(not lItalicState);
          // Can't display next part, have to 'rework' current to show '...'
          if GetTextWithinLimit(ACanvas, lList[i + 1], ARect.Right - lXPos - lWidth) = '' then
            lOutText := '';
          SetItalic(lItalicState);
        end;
        if lOutText = '' then
          lList[i] := lList[i] + '...'
        else begin
          lOutText := GetTextWithinLimit(ACanvas, lList[i], ARect.Right - lXPos);
          ACanvas.TextOut(lXPos, ARect.Top, lOutText);
          Inc(lXPos, lWidth);
          lItalicState := not lItalicState;
          SetItalic(lItalicState);
          // Move on to next part.
          Inc(i);
        end;
      end;
    end;
  finally
    lList.Free;
    SetItalic(False);
    SetBold(False);
  end;
end;


{-------------------------------------------------------------------------------
  Draws a term onto the canvas, allowing for the <i> formatting, and word-
    wrapping.  If ACalcRect is true, then the text is not drawn, it is
    used to calculate the rectangle, in which case if the rectangle is not wide
    enough for the widest word, it is widened.  If the text contains explicit
    line feeds, then text is only wrapped when these are encountered, not
    automatically
}
procedure TdmInterface.DrawWrappedTerm(ACanvas: TCanvas; var ARect: TRect;
  const ATerm: string; ASelected: boolean; ACalcRect: boolean);
var
  lWordWidth: integer;
  lRemainingTerm: string;
  lWord: string;
  lX, lY: integer;
  lPossibleLineSeparator: string;

    procedure FindRectangleWidth;
    begin
      // find the width of each word.
      lRemainingTerm := Trim(ATerm);
      while lRemainingTerm<>'' do begin
        if Pos(lPossibleLineSeparator, lRemainingTerm)>0 then
          lWord := Copy(lRemainingTerm, 1, Pos(lPossibleLineSeparator, lRemainingTerm))
        else
          lWord := lRemainingTerm;
        lWordWidth := PaintWord(ACanvas, 0, 0, lWord, True);
        if lWordWidth>ARect.Right - ARect.Left then
          ARect.Right := ARect.Left + lWordWidth;
        lRemainingTerm := Copy(lRemainingTerm, Length(lWord)+1, High(Integer));
      end;
    end;

    procedure DrawOutput;
    var
      lbmpTemp : Graphics.TBitmap;
      lFontStyle : TFontStyles;
    begin
      // Create a bitmap to draw text on, so we can then centre it once a line is complete
      lbmpTemp := Graphics.TBitmap.Create;
      lbmpTemp.Width := ARect.Right - ARect.Left;
      lbmpTemp.Height := ACanvas.TextHeight('A')+2;
      lbmpTemp.Canvas.Brush.Assign(ACanvas.Brush);
      lbmpTemp.Canvas.Font.Assign(ACanvas.Font);
      lbmpTemp.Canvas.FillRect(Rect(0, 0, lbmpTemp.Width, lbmpTemp.Height));
      try
        // Draw the output
        lX := 0;
        lY := ARect.Top;
        lRemainingTerm := Trim(ATerm);
        while lRemainingTerm<>'' do begin
          if Pos(lPossibleLineSeparator, lRemainingTerm)>0 then
            lWord := Copy(lRemainingTerm, 1,
                Pos(lPossibleLineSeparator, lRemainingTerm)-1) + ' '
          else
            lWord := lRemainingTerm;
          lFontStyle := lbmpTemp.Canvas.Font.Style;
          lWordWidth := PaintWord(lbmpTemp.Canvas, lX, 0, lWord, True);
          // Preserve the font style for the start of the actual draw operation
          if not ACalcRect then
            lbmpTemp.Canvas.Font.Style := lFontStyle;
          if lX + lWordWidth > ARect.Right-ARect.Left then begin
            // Draw the bitmap, and centre it
            lbmpTemp.Width := lX;
            if not ACalcRect then
              ACanvas.Draw(ARect.Left + (ARect.Right - ARect.Left) div 2 - lX div 2,
                  lY, lbmpTemp);
            // Clear bitmap for next line
            lbmpTemp.Width := ARect.Right - ARect.Left;
            if not ACalcRect then
              lbmpTemp.Canvas.FillRect(Rect(0, 0, lbmpTemp.Width, lbmpTemp.Height));
            // New Line
            lX := 0;
            lY := lY + lbmpTemp.Canvas.TextHeight('A')+2;
          end;
          if not ACalcRect then
            // actually paint the word
            PaintWord(lbmpTemp.Canvas, lX, 0, lWord, False);
          lX := lX + lWordWidth;
          lRemainingTerm := Copy(lRemainingTerm, Length(lWord)+
              Length(lPossibleLineSeparator), High(Integer));
        end;
        // Draw the final lines bitmap, and centre it
        lbmpTemp.Width := lX;
        if not ACalcRect then
          ACanvas.Draw(ARect.Left + (ARect.Right - ARect.Left) div 2 - lX div 2,
              lY, lbmpTemp);
      finally
        lbmpTemp.Free;
      end;
    end;

begin
  // use spaces or line feeds to wrap text?
  if Pos(#13#10, ATerm)>0 then
    lPossibleLineSeparator:=#13#10
  else
    lPossibleLineSeparator:=' ';
  // If the word has own formatting, then font cannot start italic
  if Pos('<i>', ATerm)>0 then
    ACanvas.Font.Style := ACanvas.Font.Style - [fsItalic];
  if ACalcRect then
    FindRectangleWidth;
  DrawOutput;
  // Find the very bottom of the text, allowing for the last line
  if ACalcRect then
    ARect.Bottom := lY + ACanvas.TextHeight('A');
end;


{-------------------------------------------------------------------------------
  Returns a rectangle aligned within the supplied rectangle, suitable for
      drawing the text into.  Pixels are added to the rectangle in each
      dimension to allow for a border.
}
function TdmInterface.GetAlignedTextRect(ACanvas: TCanvas; ARect: TRect;
  const AText: string; AAlignment: TAlignment; ABorderWidth: integer): TRect;
var
  lRectTop, lRectBottom: integer;
begin
  lRectTop := (ARect.Top+ARect.Bottom) div 2 - ACanvas.TextHeight(AText) div 2 - ABorderWidth;
  lRectBottom := (ARect.Top+ARect.Bottom) div 2 + ACanvas.TextHeight(AText) div 2 + ABorderWidth;
  case AAlignment of
    taLeftJustify :
      Result := Classes.Rect(ARect.Left,
          lRectTop,
          ARect.Left+ACanvas.TextWidth(AText)+ABorderWidth*2,
          lRectBottom);
    taRightJustify :
      Result := Classes.Rect(ARect.Left+ACanvas.TextWidth(AText)+ABorderWidth*2,
          lRectTop,
          ARect.Right,
          lRectBottom);
    taCenter :
      Result := Classes.Rect(
          (ARect.Left+ARect.Right) div 2 - ACanvas.TextWidth(AText) div 2 - ABorderWidth,
          lRectTop,
          (ARect.Left+ARect.Right) div 2 + ACanvas.TextWidth(AText) div 2 + ABorderWidth,
          lRectBottom);
  end; // case
end;

{-------------------------------------------------------------------------------
  Draw code that can be attached to TMenuItem.OnDrawItem if term formatting
      is required.
}
procedure TdmInterface.TermMenuDrawItem(Sender: TObject; ACanvas: TCanvas;
  ARect: TRect; Selected: Boolean);
begin
  ARect.Top := ARect.Top + 2;
  
  //Draw image if it exists
  if TMenuItem(Sender).ImageIndex > -1 then begin
    TMenuItem(Sender).GetImageList.Draw(ACanvas, ARect.Left, ARect.Top,
        TMenuItem(Sender).ImageIndex);
    ARect.Left := ARect.Left + TMenuItem(Sender).GetImageList.Width + 4;
  end;

  DrawTerm(ACanvas, ARect, TMenuItem(Sender).Caption, Selected);
end;

{-------------------------------------------------------------------------------
  Paints a single node onto the hierarchy canvas.  Used when refreshing a
      common name.
}
procedure TdmInterface.RepaintNode(ANode: TFlyNode; ATree: TRapidTree);
var
  lDummy: boolean;
begin
  // If node is not off the top of the tree view
  if ANode.GetRow >= ATree.TopItem.GetRow then
    TRapidTreeAccessor(ATree).DrawCell(ATree.Canvas, 0, ANode.GetRow,
        ATree.DisplayRect(ANode, False), [], lDummy);
end;  // TdmInterface.RepaintNode


{-------------------------------------------------------------------------------
  Paints a word, and finds the width of a word that will be painted onto the
      canvas.
}
function TdmInterface.PaintWord(ACanvas: TCanvas; AX, AY: integer; const ATerm: string;
             ACalcRect: boolean): integer;
var
  lRemainingTerm: string;
  lStartItalicPos, lEndItalicPos: integer;
  lCurrentWord: string;
begin
  Result := 0;
  lRemainingTerm := ATerm;
  while lRemainingTerm <> '' do begin
    lStartItalicPos := Pos('<i>', lRemainingTerm);
    lEndItalicPos := Pos('</i>', lRemainingTerm);
    // Find the first formatting tag
    if lStartItalicPos=0 then lStartItalicPos := High(Integer);
    if lEndItalicPos=0 then lEndItalicPos := High(Integer);
    lCurrentWord := Copy(lRemainingTerm, 1, Min(lStartItalicPos, lEndItalicPos)-1);
    if not ACalcRect then
      ACanvas.TextOut(AX + Result, AY, lCurrentWord);
    Result := Result + ACanvas.TextWidth(lCurrentWord);
    if lStartItalicPos <lEndItalicPos then begin
      lRemainingTerm := Copy(lRemainingTerm, Length(lCurrentWord)+4, High(Integer));
      ACanvas.Font.Style := ACanvas.Font.Style + [fsItalic];
    end
    else if lEndItalicPos < lStartItalicPos then begin
      lRemainingTerm := Copy(lRemainingTerm, Length(lCurrentWord)+5, High(Integer));
      ACanvas.Font.Style := ACanvas.Font.Style - [fsItalic];
    end
    else
      lRemainingTerm := '';
  end; // while
end;

initialization

{-------------------------------------------------------------------------------
  InterfaceDataModule requires a finalization section to control the order
  of destruction. There was a problem destroying the toolbar because
  it and its image list were being destroyed incorrectly. The following
  code removes that error.
}
finalization
  if Assigned(dmInterface) then
  begin
    FreeAndNil(dmInterface.ilButtons);
    FreeAndNil(dmInterface.ilBrowserNodes);
    FreeAndNil(dmInterface.ilOtherNodes);
    FreeAndNil(dmInterface.ilMenuItems);
  end;
end.
