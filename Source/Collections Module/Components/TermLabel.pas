{===============================================================================
  Unit:        TermLabel

  Defines:     TTermLabel

  Description: TTermLabel extends TLabel to allow Terms to be displayed using
               their italic formatting tags.

  Model:       Components.mpb

  Created: 27/10/2003

  Last revision information:
    $Revision: 4 $
    $Date: 4/02/04 10:46 $
    $Author: Andrewkemp $

===============================================================================}

unit TermLabel;

interface

uses
  Windows, StdCtrls, Classes, Graphics;

type
  TTermLabel = class (TLabel)
  private
    procedure DrawTerm(var ARect: TRect; const ATerm: string; Flags: Integer);
  protected
    procedure DoDrawText(var Rect: TRect; Flags: Longint); override;
  public
    constructor Create(AOwner: TComponent); override;
  end;
  
//==============================================================================
implementation

uses
  SysUtils;

{-==============================================================================
    TTermLabel
===============================================================================}
{-------------------------------------------------------------------------------
}
constructor TTermLabel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  // Force Arial font. Italic is not very readable if left on MS Sans Serif.
  Font.Name := 'Arial';
  Font.Color := clBlue;
end;  // TTermLabel.Create 

{-------------------------------------------------------------------------------
}
procedure TTermLabel.DoDrawText(var Rect: TRect; Flags: Longint);
begin
  DrawTerm(Rect, GetLabelText, Flags);
end;  // TTermLabel.DoDrawText 

{-------------------------------------------------------------------------------
}
procedure TTermLabel.DrawTerm(var ARect: TRect; const ATerm: string; Flags: Integer);
var
  lCurrentText: String;
  lCurrentPos, lFormatPos: Integer;
  lXPos: Integer;
begin
  Flags := DrawTextBiDiModeFlags(Flags);
  Canvas.Font := Font;
  with Canvas do begin
    lCurrentPos  := 1;
    lCurrentText := StringReplace(ATerm, '<b/>', '', [rfReplaceAll]);
    lFormatPos   := Pos('<i>', lCurrentText);
    lXPos        := ARect.Left + 1;
  
    while lFormatPos > 0 do begin
      if (Flags and DT_CALCRECT = 0) then
        TextOut(lXPos, ARect.Top, Copy(lCurrentText, lCurrentPos, lFormatPos - 1));
      lXPos        := lXPos + TextWidth(Copy(lCurrentText, lCurrentPos, lFormatPos - 1));
      lCurrentText := Copy(lCurrentText, lFormatPos + 3, 255);
      lFormatPos   := Pos('</i>', lCurrentText);
      if lFormatPos = 0 then lFormatPos := Length(lCurrentText) + 1;
      Font.Style   := [fsItalic];
      if (Flags and DT_CALCRECT = 0) then
        TextOut(lXPos, ARect.Top, Copy(lCurrentText, lCurrentPos, lFormatPos - 1));
      lXPos        := lXPos + TextWidth(Copy(lCurrentText, lCurrentPos, lFormatPos - 1))
          + 2;
      lCurrentText := Copy(lCurrentText, lFormatPos + 4, 255);
      lFormatPos   := Pos('<i>', lCurrentText);
      Font.Style   := [];
    end;
    if (Flags and DT_CALCRECT = 0) then
      TextOut(lXPos, ARect.Top, lCurrentText)
    else
      ARect.Right := lXPos + TextWidth(lCurrentText);
  end;
end;  // TTermLabel.DrawTerm 

end.
