{===============================================================================
  Unit:        TLuxIDComboBox

  Defines:     TLuxIDComboBox

  Description: Base class for Luxembourg specific IDComboBoxes. Supports a
               "No selection" item, which is added automatically (if specified)
               just before the items are populated.

  Model:       Components

  Created:     August 2003

  Last revision information:
    $Revision: 6 $
    $Date: 10/02/04 17:16 $
    $Author: Johnvanbreda $

===============================================================================}

unit LuxIDComboBox;

interface

uses
  Windows, Messages, Classes, StdCtrls, ComboListID, Sysutils;

const
  WM_CHECKITEMINDEX = WM_APP + 1;

type
  TLuxIDComboBox = class (TIDComboBox)
  private
    FHasNoSelectionItem: Boolean;
    FNoSelectionItemText: String;
    procedure CheckHint;
    procedure SetNoSelectionItemText(const Value: String);
    procedure WMCheckItemIndex(var Message: TMessage); message WM_CHECKITEMINDEX;
  protected
    procedure CloseUp; override;
    procedure DoPopulate; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: char); override;
    procedure Select; override;
    procedure SetItemIndex(const Value: Integer); override;
    procedure SetReadOnly(const Value: Boolean); override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property HasNoSelectionItem: Boolean read FHasNoSelectionItem write 
        FHasNoSelectionItem default False;
    property NoSelectionItemText: String read FNoSelectionItemText write 
        SetNoSelectionItemText;
    property Style default csDropDownList;
  end;
  
//==============================================================================
implementation

{-==============================================================================
    TLuxIDComboBox
===============================================================================}
{-------------------------------------------------------------------------------
}
constructor TLuxIDComboBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Style := csDropdownList;
  FNoSelectionItemText := '';
  FHasNoSelectionItem := False;
end;  // TLuxIDComboBox.Create 

{-------------------------------------------------------------------------------
  Sets a hint for the combo box if the text is too wide. 
}
procedure TLuxIDComboBox.CheckHint;
begin
  // Set the combo box hint if too wide
  if Canvas.TextWidth(Text)>ClientWidth then begin
    Hint := Text;
    ShowHint := True;
  end
  else
    Hint := '';
end;  // TLuxIDComboBox.CheckHint 

{-------------------------------------------------------------------------------
}
procedure TLuxIDComboBox.CloseUp;
begin
  inherited CloseUp;
  if Populated and (Style = csDropDownList) and
     FHasNoSelectionItem and (ItemIndex = 0) then
    ItemIndex := -1;
end;  // TLuxIDComboBox.CloseUp 

{-------------------------------------------------------------------------------
}
procedure TLuxIDComboBox.DoPopulate;
begin
  // Populate the list, that will set the IDFormat
  inherited DoPopulate;
  
  // Then insert the "no selection" item at the top, if there is one,
  // using the correct key type
  if FHasNoSelectionItem then
    if IDFormat = idfLongint then
      Insert(0, FNoSelectionItemText, -1)
    else
      Insert(0, FNoSelectionItemText, '');
end;  // TLuxIDComboBox.DoPopulate 

{-------------------------------------------------------------------------------
}
procedure TLuxIDComboBox.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);
  
  if not ReadOnly and (Style = csDropDownList) and (Key in [VK_BACK, VK_DELETE]) then
    ItemIndex := -1;
end;  // TLuxIDComboBox.KeyDown 

{-------------------------------------------------------------------------------
}
procedure TLuxIDComboBox.KeyPress(var Key: char);
begin
  inherited KeyPress(Key);
  
  // Get rid of the beep, for Return/Ctrl+Return
  if Key in [#13, #10] then Key := #0;
end;  // TLuxIDComboBox.KeyPress 

{-------------------------------------------------------------------------------
}
procedure TLuxIDComboBox.Select;
begin
  inherited Select;
  
  CheckHint;
  // Add a delay to allow user to use mouse wheel to go through list.
  PostMessage(Handle, WM_CHECKITEMINDEX, ItemIndex, 0);
end;  // TLuxIDComboBox.Select 

{-------------------------------------------------------------------------------
}
procedure TLuxIDComboBox.SetItemIndex(const Value: Integer);
begin
  inherited;
  CheckHint;
end;  // TLuxIDComboBox.SetItemIndex 

{-------------------------------------------------------------------------------
}
procedure TLuxIDComboBox.SetNoSelectionItemText(const Value: String);
begin
  if FNoSelectionItemText <> Value then
    if Value = '' then begin
      FNoSelectionItemText := '';
      FHasNoSelectionItem := False;
    end else begin
      FNoSelectionItemText := Value;
      FHasNoSelectionItem := True;
    end;
end;  // TLuxIDComboBox.SetNoSelectionItemText 

{-------------------------------------------------------------------------------
}
procedure TLuxIDComboBox.SetReadOnly(const Value: Boolean);
begin
  inherited SetReadOnly(Value);
  
  if Populated and (Style = csDropDownList) and
     FHasNoSelectionItem and (ItemIndex = 0) then
    ItemIndex := -1;
end;  // TLuxIDComboBox.SetReadOnly 

{-------------------------------------------------------------------------------
}
procedure TLuxIDComboBox.WMCheckItemIndex(var Message: TMessage);
begin
  if not ReadOnly and Populated and (Style = csDropDownList) and
     not DroppedDown and (Message.WParam = 0) and FHasNoSelectionItem then
    ItemIndex := -1;
end;  // TLuxIDComboBox.WMCheckItemIndex 

end.
