{===============================================================================
  Unit:        SpatialRef

  Defines:     TSpatialRef

  Description:

  Model:

  Created:

  Last revision information:
    $Revision: 7 $
    $Date: 16/04/04 13:37 $
    $Author: Anthonysimpson $

===============================================================================}

unit SpatialRef;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, Buttons, IDComboBox, CompositeComponent, Registry,
  LuxembourgConstants, ImgList, ImageListButton, Menus, ExceptionForm, ResourceStrings,
  DataTypes, Recorder2000_TLB;

type
  ESpatialRefError = class(TExceptionPath);

  // Used to send a set of spatial ref properties to results of functions
  TSpatialRefValues = record
    DisplayRef: String;
    DisplaySystem: String;
    EnteredRef: String;
    EnteredSystem: String;
    Qualifier: String;
  end;

  TInvalidSpatialRefEvent = procedure(Sender: TObject; var Handled: Boolean) of object;

  TSpatialRef = class (TCompositeComponent, ILatLong)
  private
    FBevel: TBevel;
    FDisplayRef: String;
    FDragDropShape: TShape;
    FDropDownButton: TButton;
    FDropDownMenu: TPopupMenu;
    FEnteredRef: String;
    FEnteredSystem: String;
    FGetButton: TImageListButton;
    FListAdded: Boolean;
    FModified: Boolean;
    FOnFind: TNotifyEvent;
    FOnInvalidSpatialRef: TInvalidSpatialRefEvent;
    FQualifierCombo: TIDComboBox;
    FSpatialRefEdit: TEdit;
    FLatitude: double;
    FLongitude: double;
    procedure DropDownClick(Sender: TObject);
    function GetColor: TColor;
    function GetDisplayRef: String;
    function GetEnteredRef: String;
    function GetEnteredSystem: String;
    function GetImageIndex: Integer;
    function GetImageList: TCustomImageList;
    function GetOnGetFromMap: TNotifyEvent;
    function GetQualifier: String;
    function GetSpatialRef: String;
    function GetValues: TSpatialRefValues;
    procedure SetColor(Value: TColor);
    procedure SetDisplayRef(const Value: String);
    procedure SetEnteredRef(const Value: String);
    procedure SetEnteredSystem(const Value: String);
    procedure SetFont(const Value: TFont);
    procedure SetImageIndex(const Value: Integer);
    procedure SetImageList(Value: TCustomImageList);
    procedure SetOnFind(const Value: TNotifyEvent);
    procedure SetOnGetFromMap(const Value: TNotifyEvent);
    procedure SetQualifier(const Value: String);
    procedure SetSpatialRef(const Value: String);
    procedure SetupButtons;
    procedure SetupCombo;
    procedure SetupControl;
    procedure SetupEdit;
    procedure SetupShape;
    procedure SetValues(const Value: TSpatialRefValues);
    procedure SpatialRefEditKeyPressed(Sender: TObject; var Key: Char);
    procedure WMSize(var Message: TMessage); message WM_SIZE;
    function GetReadOnly: Boolean;
    procedure SetReadOnly(Value: Boolean);
    function DisplaySystem: string;
    function Get_Latitude: Double; safecall;
    function Get_Longitude: Double; safecall;
    function Get_ErrorMsg: WideString; safecall;
  protected
    procedure DoExit; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Clear;
    procedure UpdateMapWindowSelector;
    property ControlQualifier: TIDComboBox read FQualifierCombo;
    property ControlSpatialRef: TEdit read FSpatialRefEdit;
    property DisplayRef: String read GetDisplayRef write SetDisplayRef;
    property EnteredRef: String read GetEnteredRef write SetEnteredRef;
    property EnteredSystem: String read GetEnteredSystem write SetEnteredSystem;
    property Modified: Boolean read FModified write FModified;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly;
    property Qualifier: String read GetQualifier write SetQualifier;
    property Values: TSpatialRefValues read GetValues write SetValues;
  published
    property Color read GetColor write SetColor default clWindow;
    property DestCol default clRed;
    property DropDownMenu: TPopupMenu read FDropDownMenu write FDropDownMenu;
    property Font write SetFont;
    property ImageIndex: Integer read GetImageIndex write SetImageIndex default -1;
    property ImageList: TCustomImageList read GetImageList write SetImageList;
    property OnEnter;
    property OnExit;
    property OnFind: TNotifyEvent read FOnFind write SetOnFind;
    property OnGetFromMap: TNotifyEvent read GetOnGetFromMap write SetOnGetFromMap;
    property OnInvalidSpatialRef: TInvalidSpatialRefEvent read FOnInvalidSpatialRef write FOnInvalidSpatialRef;
    property SourceCol default clBlue;
    property SpatialRef: String read GetSpatialRef write SetSpatialRef;
  end;

//==============================================================================
implementation

uses
  ComObj;

{-==============================================================================
    TSpatialRef
===============================================================================}
{-------------------------------------------------------------------------------
}
constructor TSpatialRef.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  
  // All controls are parented to the main one, so don't need to free each individually.
  SetupControl;
  SetupEdit;
  SetupShape;
  SetupButtons;
  SetupCombo;
  FListAdded := False;

  // Set the defaults.
  DestCol    := clRed;
  SourceCol  := clBlue;
  SetReadOnly(True);
  ImageIndex := -1;
end;  // TSpatialRef.Create 

{-------------------------------------------------------------------------------
}
procedure TSpatialRef.Clear;
begin
  FSpatialRefEdit.Text      := '';
  FQualifierCombo.ItemIndex := -1;
  Modified                  := True;
end;  // TSpatialRef.Clear

{-------------------------------------------------------------------------------
}
procedure TSpatialRef.DoExit;
var
  lRecorder: IRecorder2000;
  lSystem: String;
  lLatLong: ILatLong;
  lHandled: Boolean;
begin
  if FSpatialRefEdit.Modified then begin
    Modified := True;
    lRecorder := CreateOLEObject('Recorder2000.AutoApplicationSettings') as IRecorder2000;
    with lRecorder.RecorderFunctions do
      if FSpatialRefEdit.Text = '' then begin
        SetDisplayRef('');
        SetEnteredRef('');
        SetEnteredSystem(IdentifySpatialRefSystem(FSpatialRefEdit.Text));
      end else begin
        // This try/except is necessary to catch a critical error that shouldn't be critical.
        try
          lLatLong := DecodeSpatialRef(FSpatialRefEdit.Text);
        except on E:Exception do
          raise ESpatialRefError.CreateNonCritical(E.Message)
        end;
        if lLatLong.ErrorMsg = '' then begin
          lSystem := IdentifySpatialRefSystem(FSpatialRefEdit.Text);
          SetEnteredSystem(lSystem);
          SetEnteredRef(Uppercase(FSpatialRefEdit.Text));
        end else begin
          lHandled := False;
          // Give opportunity to intercept and handle the problem.
          if Assigned(FOnInvalidSpatialRef) then
            FOnInvalidSpatialRef(Self, lHandled);
          // If the error has been handled, don't raise, otherwise do.
          if not lHandled then
            raise ESpatialRefError.CreateValidation(lLatLong.ErrorMsg, FSpatialRefEdit);
        end;
      end; 
  end; // if
  inherited;
end;  // TSpatialRef.DoExit

{-------------------------------------------------------------------------------
}
procedure TSpatialRef.DropDownClick(Sender: TObject);
var
  lPos: TPoint;
begin
  if Assigned(FDropDownMenu) then begin
    lPos := FGetButton.ClientToScreen(Point(0, FGetButton.Height));
    FDropDownMenu.Popup(lPos.X, lPos.Y);
  end;
end;  // TSpatialRef.DropDownClick 

{-------------------------------------------------------------------------------
}
function TSpatialRef.GetColor: TColor;
begin
  Result := FSpatialRefEdit.Color;
end;  // TSpatialRef.GetColor

{-------------------------------------------------------------------------------
}
function TSpatialRef.GetDisplayRef: String;
begin
  Result := FDisplayRef;
end;  // TSpatialRef.GetDisplayRef 

{-------------------------------------------------------------------------------
}
function TSpatialRef.GetEnteredRef: String;
begin
  // The checks are necessary because FEnteredRef is updated only when focus
  // leaves the component, not just the Edit box. And if the Spatial Ref is
  // cleared after FEnteredRef has been set, FEnteredRef might not reflect that
  // when this function is called, as the focus might still be on the Edit Box.
  if (FEnteredRef<>'') and (FSpatialRefEdit.Text<>'') then
    Result := FEnteredRef
  else
    Result := FSpatialRefEdit.Text;
end;  // TSpatialRef.GetEnteredRef 

{-------------------------------------------------------------------------------
}
function TSpatialRef.GetEnteredSystem: String;
begin
  if FEnteredSystem = '' then
    Result := (CreateOLEObject('Recorder2000.AutoApplicationSettings') as
        IRecorder2000).RecorderFunctions.IdentifySpatialRefSystem(FSpatialRefEdit.text)
  else
    Result := FEnteredSystem;
end;  // TSpatialRef.GetEnteredSystem 

{-------------------------------------------------------------------------------
}
function TSpatialRef.GetImageIndex: Integer;
begin
  Result := FGetButton.ImageIndex;
end;  // TSpatialRef.GetImageIndex 

{-------------------------------------------------------------------------------
}
function TSpatialRef.GetImageList: TCustomImageList;
begin
  Result := FGetButton.ImageList;
end;  // TSpatialRef.GetImageList 

{-------------------------------------------------------------------------------
}
function TSpatialRef.GetOnGetFromMap: TNotifyEvent;
begin
  Result := FGetButton.OnClick;
end;  // TSpatialRef.GetOnGetFromMap 

{-------------------------------------------------------------------------------
}
function TSpatialRef.GetReadOnly: Boolean;
begin
  Result := FSpatialRefEdit.ReadOnly;
end;  // TSpatialRef.GetReadOnly

{-------------------------------------------------------------------------------
}
function TSpatialRef.GetQualifier: String;
begin
  Result := FQualifierCombo.Text;
end;  // TSpatialRef.GetQualifier 

{-------------------------------------------------------------------------------
}
function TSpatialRef.GetSpatialRef: String;
begin
  Result := FSpatialRefEdit.Text;
end;  // TSpatialRef.GetSpatialRef 

{-------------------------------------------------------------------------------
}
function TSpatialRef.GetValues: TSpatialRefValues;
begin
  Result.EnteredRef    := GetEnteredRef;
  Result.EnteredSystem := GetEnteredSystem;
  Result.Qualifier     := GetQualifier;
  Result.DisplayRef    := GetDisplayRef;
  Result.DisplaySystem := DisplaySystem;
end;  // TSpatialRef.GetValues 

{-------------------------------------------------------------------------------
}
procedure TSpatialRef.SetColor(Value: TColor);
begin
  FSpatialRefEdit.Color := Value;
  FQualifierCombo.Color := Value;
end;  // TSpatialRef.SetColor

{-------------------------------------------------------------------------------
}
procedure TSpatialRef.SetDisplayRef(const Value: String);
begin
  FDisplayRef          := Value;
  FSpatialRefEdit.Text := FDisplayRef;
  Modified             := True;
end;  // TSpatialRef.SetDisplayRef

{-------------------------------------------------------------------------------
}
procedure TSpatialRef.SetEnteredRef(const Value: String);
var
  lLatLong: ILatLong;
begin
  FEnteredRef := Value;
  Modified    := True;

  if (FEnteredSystem=DisplaySystem) or (Value = '') then
    DisplayRef := FEnteredRef
  else begin
    with (CreateOLEObject('Recorder2000.AutoApplicationSettings') as IRecorder2000) do
    begin
      lLatLong := RecorderFunctions.DecodeSpatialRef(FEnteredRef);
      FLatitude := lLatLong.Latitude;
      FLongitude := lLatLong.Longitude;
      DisplayRef := RecorderFunctions.EncodeSpatialRef(Self as ILatLong);
    end;
  end;
end;  // TSpatialRef.SetEnteredRef

{-------------------------------------------------------------------------------
}
procedure TSpatialRef.SetEnteredSystem(const Value: String);
begin
  FEnteredSystem := Value;
  Modified := True;
end;  // TSpatialRef.SetEnteredSystem 

{-------------------------------------------------------------------------------
}
procedure TSpatialRef.SetFont(const Value: TFont);
begin
  inherited Font := Value;
  FSpatialRefEdit.Font.Assign(Value);
  FQualifierCombo.Font.Assign(Value);
  SendMessage(Self.Handle, WM_Size, 0, 0);
end;  // TSpatialRef.SetFont 

{-------------------------------------------------------------------------------
}
procedure TSpatialRef.SetImageIndex(const Value: Integer);
begin
  FGetButton.ImageIndex := Value;
end;  // TSpatialRef.SetImageIndex 

{-------------------------------------------------------------------------------
}
procedure TSpatialRef.SetImageList(Value: TCustomImageList);
begin
  FGetButton.ImageList := Value;
end;  // TSpatialRef.SetImageList 

{-------------------------------------------------------------------------------
}
procedure TSpatialRef.SetOnFind(const Value: TNotifyEvent);
begin
  FOnFind := Value;
end;  // TSpatialRef.SetOnFind 

{-------------------------------------------------------------------------------
}
procedure TSpatialRef.SetOnGetFromMap(const Value: TNotifyEvent);
begin
  FGetButton.OnClick := Value;
end;  // TSpatialRef.SetOnGetFromMap

{-------------------------------------------------------------------------------
}
procedure TSpatialRef.SetReadOnly(Value: Boolean);
begin
  FSpatialRefEdit.ReadOnly := Value;
  FGetButton.Enabled       := not Value;
  FDropDownButton.Enabled  := not Value;
  FQualifierCombo.Enabled  := not Value;
end;  // TSpatialRef.SetReadOnly

{-------------------------------------------------------------------------------
}
procedure TSpatialRef.SetQualifier(const Value: String);
begin
  FQualifierCombo.Text := Value;
  Modified := True;
end;  // TSpatialRef.SetQualifier 

{-------------------------------------------------------------------------------
}
procedure TSpatialRef.SetSpatialRef(const Value: String);
begin
  EnteredRef := Value;
  Modified             := True;
end;  // TSpatialRef.SetSpatialRef 

{-------------------------------------------------------------------------------
}
procedure TSpatialRef.SetupButtons;
begin
  FGetButton := TImageListButton.Create(Self);
  with FGetButton do begin
    Parent   := Self;
    Height   := 23;
    Width    := 24;
    Caption  := '';
    Hint     := 'Find spatial reference from the map';
    Visible  := True;
    TabOrder := 1;
  end;
  FDropDownButton := TButton.Create(Self);
  with FDropDownButton do begin
    Parent     := Self;
    Height     := 23;
    Width      := 16;
    ParentFont := False;
    Font.Name  := 'Marlett';  // MS font.
    Font.Style := [];
    Caption    := '6';  // Dropdown arrow
    Visible    := True;
    TabOrder   := 2;
    OnClick    := DropDownClick;
  end;
end;  // TSpatialRef.SetupButtons 

{-------------------------------------------------------------------------------
}
procedure TSpatialRef.SetupCombo;
begin
  FQualifierCombo := TIDComboBox.Create(Self);
  with FQualifierCombo do begin
    Parent   := Self;
    Top      := 6;
    Width    := 129;
    Sorted   := True;
    Hint     := 'Qualifier';
    Visible  := True;
    TabOrder := 3;
  end;
end;  // TSpatialRef.SetupCombo 

{-------------------------------------------------------------------------------
}
procedure TSpatialRef.SetupControl;
begin
  Caption       := '';
  Width         := 277;
  Height        := 33;
  FBevel        := TBevel.Create(Self);
  FBevel.Parent := Self;
  FBevel.Align  := alClient;
  FBevel.Shape  := bsFrame;
end;  // TSpatialRef.SetupControl 

{-------------------------------------------------------------------------------
}
procedure TSpatialRef.SetupEdit;
begin
  FSpatialRefEdit := TEdit.Create(Self);
  with FSpatialRefEdit do begin
    Parent     := Self;
    Left       := 6;
    Top        := 6;
    Charcase   := ecUppercase;
    Hint       := 'Spatial reference';
    Visible    := True;
    TabOrder   := 0;
    OnKeyPress := SpatialRefEditKeyPressed;
  end;
end;  // TSpatialRef.SetupEdit 

{-------------------------------------------------------------------------------
}
procedure TSpatialRef.SetupShape;
begin
  FDragDropShape := TShape.Create(Self);
  with FDragDropShape do begin
    Parent  := Self;
    Tag     := DEST_ONLY;
    Visible := True;
  end;
end;  // TSpatialRef.SetupShape 

{-------------------------------------------------------------------------------
}
procedure TSpatialRef.SetValues(const Value: TSpatialRefValues);
begin
  SetEnteredRef(Value.EnteredRef);
  SetEnteredSystem(Value.EnteredSystem);
  SetQualifier(Value.Qualifier);
  SetDisplayRef(Value.DisplayRef);
  Modified := True;
end;  // TSpatialRef.SetValues 

{-------------------------------------------------------------------------------
}
procedure TSpatialRef.SpatialRefEditKeyPressed(Sender: TObject; var Key: Char);
begin
  Modified := True;
  if Key = #13 then
    if Assigned(FOnFind) then FOnFind(Self);
end;  // TSpatialRef.SpatialRefEditKeyPressed 

{-------------------------------------------------------------------------------
  Will force the state of the buttons to be refreshed, without duplicating any code.
}
procedure TSpatialRef.UpdateMapWindowSelector;
begin
  SetEditMode(EditMode);
end;  // TSpatialRef.UpdateMapWindowSelector

{-------------------------------------------------------------------------------
}
procedure TSpatialRef.WMSize(var Message: TMessage);
begin
  // Resize control to have edit box vertically centered, unless below minimum height.
  if FSpatialRefEdit.Height + 12 < FGetButton.Height then
    Self.Height := FGetButton.Height + 10
  else
    Self.Height := FSpatialRefEdit.Height + 12;
  
  FBevel.SetBounds(0, 0, Width, Height);
  
  FQualifierCombo.Left  := Self.Width - FQualifierCombo.Width - 6;
  FDropDownButton.Left  := FQualifierCombo.Left - FDropDownButton.Width - 4;
  FDropDownButton.Top   := (Self.Height - FGetButton.Height) div 2;
  FGetButton.Left       := FDropDownButton.Left - FGetButton.Width + 1;
  FGetButton.Top        := FDropDownButton.Top;
  FSpatialRefEdit.Width := FGetButton.Left - 8;
  FDragDropShape.SetBounds(5, 5, FSpatialRefEdit.Width + 2, FSpatialRefEdit.Height + 2);
  
  if not FListAdded then begin
    FListAdded := True;
    with FQualifierCombo do begin
      AddWithID('Internal Map', 0);
      AddWithID('GPS', 1);
      AddWithID('Original Recorder', 2);
      AddWithID('Estimated from Map', 3);
      AddWithID('Site Centroid', 4);
    end;
  end;
end;  // TSpatialRef.WMSize 

function TSpatialRef.DisplaySystem: string;
begin
  Result := (CreateOLEObject(
      'Recorder2000.AutoApplicationSettings') as IRecorder2000).SpatialRefSystem;
end;

{-------------------------------------------------------------------------------
}
function TSpatialRef.Get_ErrorMsg: WideString;
begin
  Result := '';
end;

{-------------------------------------------------------------------------------
}
function TSpatialRef.Get_Latitude: Double;
begin
  Result := FLatitude;
end;

{-------------------------------------------------------------------------------
}
function TSpatialRef.Get_Longitude: Double;
begin
  Result := FLongitude;
end;

end.
