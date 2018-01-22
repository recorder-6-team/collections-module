{===============================================================================

  Copyright © Dorset Software Services Ltd, 1996

  Component:
    TComboSelect - Jason Scott 08/07/1996
    TSelectListBox - Jason Scott 02/07/1996

  Updates:
    TComboSelect - Ian Addis 04/08/2000
    TListSelect - Eric Salmon 13/03/2000

  Packages:
    InHouse4, Delphi 4 package for in house components.
    InHouse5, Delphi 5 package for in house components.

  Description:
    TComboSelect:
    The user starts typing in the text box and the component looks for the first
    matching item in its list. The text is then updated to show the item with
    the potential ending highlighted, i.e. if the user types "be" and the list
    contains "begin" and "beware", the text box will be updated to contain
    "begin" with "ing" highlighted. If the user then types a "w", the typed text
    changes to "bew" and the selected item becomes "beware" and the text box is
    updated to "beware" with "are" highlighted. Simple!

    TListSelect:
    Allows user to type over list and automatically select item starting with
    characters typed. Selecting an item using mouse or navigation keys (up,
    down, left, right, home, end, page up, page down) will reset the search
    string, so when starting to type again, the selection will move to the first
    item starting with that character (if there is one).

  Additional information:
    To keep this component D4/D5 compatible, it is inherited from TComboBox and
    not TCustomComboBox. The reason being new propreties and events in Delphi5,
    and new D5 properties cannot be published in D4.

===============================================================================}

unit ComboListSelect;

interface

uses
  SysUtils, Windows, Classes, StdCtrls;

type
  TComboSelect = class(TComboBox)
  private
    FTextPart : String;
    FBackSpace: Boolean;
    FSelection: Boolean;
  protected
    procedure KeyPress(var Key: Char); override;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
  end;  // TComboSelect

  //----------------------------------------------------------------------------
  TListSelect = class(TCustomListBox)
  private
    FTextPart : String;
    FCharTyped: Char;
  protected
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
  public
    constructor Create (AnOwner: TComponent); override;
  end;  // TListSelect

{==============================================================================}
implementation

//==============================================================================
{ TComboSelect }

procedure TComboSelect.KeyPress(var Key: Char);
begin
  inherited KeyPress(Key);
  FBackSpace := Key = #8;
  FSelection := SelLength > 0;
end;  // TComboSelect.KeyPress

//------------------------------------------------------------------------------
procedure TComboSelect.KeyUp(var Key: Word; Shift: TShiftState);
var lIdx: Integer;
begin
  inherited KeyUp(Key, Shift);
  if (Key = VK_HOME) and (ssShift in Shift) then
  begin
    SelStart := 0;
    SelLength := Length(Text);
  end else
  if (Key <> VK_SHIFT) and (Key <> VK_DELETE) and (Key <> VK_LEFT) and (Key <> VK_RIGHT) then
  begin
    FTextPart := Text;
    if FBackSpace then
      if Length(FTextPart) = 1 then begin
        FTextPart := '';
        Text      := '';
        ItemIndex := -1;
      end else
        // Only remove an extra character if there was a selection, as Backspace
        // only removes the highlighted part.
        if FSelection then
          FTextPart := Copy(FTextPart, 1, Length(FTextPart) - 1);

    // Look for a match if there is some text
    if FTextPart <> '' then begin
      ItemIndex := -1;
      for lIdx := 0 to Items.Count - 1 do
        if Pos(UpperCase(FTextPart), UpperCase(Items[lIdx])) = 1 then begin
          ItemIndex := lIdx;
          Text      := Items[lIdx];
          // Set highlighted part only if DropDown or Simple combo box.
          // Text Selection not possible on other types.
          if (Style = csDropDown) or (Style = csSimple) then
          begin
            SelStart  := Length(FTextPart);
            SelLength := Length(Text) - Length(FTextPart);
          end;
          Break;
        end;
    end;
  end;
end;  // TComboSelect.KeyUp

//==============================================================================
//==============================================================================
{ TListSelect }

constructor TListSelect.Create (AnOwner: TComponent);
begin
  inherited Create(AnOwner);
  MultiSelect := False;
  FTextPart   := '';
end;  // TListSelect.Create

//------------------------------------------------------------------------------
procedure TListSelect.KeyPress(var key: Char);
begin
  inherited KeyPress(Key);
  // Any character or backspace
  if Key in [#8, #32..#255] then
    FCharTyped := Key
  else
    FCharTyped := #0;
end;  // TListSelect.KeyPress

//------------------------------------------------------------------------------
procedure TListSelect.KeyUp(var Key: Word; Shift: TShiftState);
var lIdx : Integer;
    lText: String;
begin
  inherited KeyUp(Key, Shift);
  if Key in [$20..$28] then begin
    FCharTyped := #0;
    FTextPart  := '';
  end;

  if (FCharTyped <> #0) then begin
    lText := FTextPart;
    if FCharTyped = #8 then begin
      if Length(FTextPart) = 1 then begin
        FTextPart := '';
        if Items.Count > 0 then ItemIndex :=  0
                           else ItemIndex := -1;
      end else
        FTextPart := Copy(FTextPart, 1, Length(FTextPart) - 1);
    end else
      FTextPart := FTextPart + FCharTyped;

    // Look for a match if there is some text
    if FTextPart <> '' then begin
      for lIdx := 0 to Items.Count - 1 do
        if Pos(UpperCase(FTextPart), UpperCase(Items[lIdx])) = 1 then begin
          ItemIndex := lIdx;
          Exit;
        end;
        // Nothing found for this text, restore to previous, ItemIndex unchanged
        FTextPart := lText;
    end;
  end;
end;  // TListSelect.KeyUp

//==============================================================================
end.
