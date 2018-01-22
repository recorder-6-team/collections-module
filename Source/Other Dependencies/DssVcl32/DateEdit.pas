{===============================================================================

  Copyright © Dorset Software Services Ltd, 2000

  Component:
    TDateEdit - Eric Salmon 17/03/2000

  Updates:

  Packages:
    InHouse5, Delphi 5 package for in house components.

  Description:
    This component allows a date to be selected only from a DateTimePicker,
    being available when the user cliks the dropdown button, as the text is
    ReadOnly. This has the advantage to always return a valid date.
    The extra feature is that the text can be left blank, a functionality not
    available with the standard DateTimePicker.

  Additional information:
    Delphi 5 only. This is because it is inherited from TCustomEdit and Delphi 5
    introduces new events and properties in the component hierarchy. To leave
    as much functionalities available as possible, all TEdit published events
    and properties are also published here, except for the Text and ReadOnly
    ones, which are the reason for inheriting from TCustomEdit in the first
    place.

    The DateTimePicker is only created at runtime.

===============================================================================}

unit DateEdit;

interface

uses
  Windows, SysUtils, Classes, Controls, StdCtrls, ComCtrls;

type
  TDateEdit = class(TCustomEdit)
  private
    FCalendar:TDateTimePicker;
    FCalendarDate: TDateTime;
    procedure SetupCalendar;
    procedure DateChange(Sender: TObject);
    procedure SetCalendarDate(const Value: TDateTime);
  protected
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure Loaded; override;
    procedure SetEnabled(Value: Boolean); override;
  public
    constructor Create(AOwner:TComponent); override;

  published
    property CalendarDate : TDateTime read FCalendarDate write SetCalendarDate;
    // Text and ReadOnly properties are not published, but are
    // available as public from ancestors.
    property Anchors;
    property AutoSelect;
    property AutoSize;
    property BiDiMode;
    property BorderStyle;
    property CharCase;
    property Color;
    property Constraints;
    property Ctl3D;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled write SetEnabled;
    property Font;
    property HideSelection;
    property ImeMode;
    property ImeName;
    property MaxLength;
    property OEMConvert;
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PasswordChar;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnChange;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
  end;

//==============================================================================
implementation

//==============================================================================
constructor TDateEdit.Create(AOwner:TComponent);
begin
  inherited Create(AOwner);
  ReadOnly:=true;
  // Show day's date in the format it will appear at runtime
  Text:=DateToStr(Date);
end;  // Create

//------------------------------------------------------------------------------
// Need to use Loaded, as it doesn;t appear to work otherwise.
// Position of the Edit control is only properly set after Loaded.
procedure TDateEdit.Loaded;
begin
  inherited;
  // Create the DateTimePicker at runtime only
  if not (csDesigning in ComponentState) then
    SetupCalendar;
end;  // Loaded

//------------------------------------------------------------------------------
procedure TDateEdit.SetupCalendar;
begin
  FCalendar:=TDateTimePicker.Create(Self);
  with FCalendar do begin
    Parent:=Self.Parent;
    Visible:=true;
    TabStop:=false;
    // Change font size because of a visual 'bug'(?). This fix seems to cure it.
    Font.Size:=1;
    // Position over the end of the Edit box.
    SetBounds(Self.Left+Self.Width-18,Self.Top,18,Self.Height);
    Anchors:=[akTop,akRight,akBottom];
    // Link the event
    OnChange:=DateChange;
    BringToFront;
  end;
end;  // SetupCalendar

//------------------------------------------------------------------------------
procedure TDateEdit.DateChange(Sender: TObject);
begin
  inherited;
  Text:=DateToStr(FCalendar.Date);
end;  // DateChange

//------------------------------------------------------------------------------
procedure TDateEdit.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if Key in [VK_DELETE, VK_BACK] then
    Text := '';
  inherited KeyDown(Key,Shift);
end;  // KeyDown

//==============================================================================
procedure TDateEdit.SetEnabled(Value: Boolean);
begin
  inherited;
  FCalendar.Enabled:=Value;
end;  // SetEnabled

//==============================================================================
procedure TDateEdit.SetCalendarDate(const Value: TDateTime);
begin
  If Value <> 0 then
    FCalendar.Date := Value;
end;

end.
