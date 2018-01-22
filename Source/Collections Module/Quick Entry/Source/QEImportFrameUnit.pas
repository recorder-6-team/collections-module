{===============================================================================
  Unit:        QEImportFrameUnit.pas

  Defines:     TQEImportFrame

  Description: Page in the Quick Entry import wizard.

  Model:       -

  Created:     September 2004

  Last revision information:
    $Revision: 6 $
    $Date: 13/12/07 17:01 $
    $Author: Ericsalmon $

===============================================================================}
unit QEImportFrameUnit;

interface

uses
  Classes, Controls, Forms, QEImportDefinition;

resourcestring
  ResStr_FileOpenError = 'An error occurred when opening the selected file:'
      + #10#10'%0:s.';

type
  TQEImportFrame = class;
  TQEImportFrameClass = class of TQEImportFrame;

  TQEImportFrame = class(TFrame)
  protected
    FAllowCancel: Boolean;
    FDefinition: TQEImportDefinition;
    FIsComplete: Boolean;
    FIsValid: Boolean;
    FOnChangeStatus: TNotifyEvent;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure DoChangeStatus; virtual;
    function GetIsLast: Boolean; virtual;
    function GetNext: TQEImportFrameClass; virtual; abstract;
    procedure SetAllowCancel(Value: Boolean); virtual;
    procedure SetIsComplete(Value: Boolean); virtual;
    procedure SetIsValid(Value: Boolean); virtual;
    property Definition: TQEImportDefinition read FDefinition;
  public
    constructor Create(Definition: TQEImportDefinition); reintroduce; virtual;
    function CancelQuery: Boolean; virtual;
    procedure LoadContent; virtual;
    procedure SaveContent; virtual;
    procedure Validate; virtual;
    property AllowCancel: Boolean read FAllowCancel;
    property IsComplete: Boolean read FIsComplete;
    property IsLast: Boolean read GetIsLast;
    property IsValid: Boolean read FIsValid;
    property Next: TQEImportFrameClass read GetNext;
  published
    property OnChangeStatus: TNotifyEvent read FOnChangeStatus write
        FOnChangeStatus;
  end;

implementation

{$R *.dfm}

{-==============================================================================
    TQEImportFrame
===============================================================================}
{-------------------------------------------------------------------------------
}
constructor TQEImportFrame.Create(Definition: TQEImportDefinition);
begin
  FDefinition := Definition;
  inherited Create(Definition.Owner);
  FAllowCancel := True;
  FIsValid := True;
end;  // TQEImportFrame.Create 

{+------------------------------------------------------------------------------
  If necessary, prompt the user for confirmation to cancel this frame.
}
function TQEImportFrame.CancelQuery: Boolean;
begin
  Result := True
end;  // TQEImportFrame.CancelQuery 

{+------------------------------------------------------------------------------
  Provide a parent window handle so that child controls can be materialised.
}
procedure TQEImportFrame.CreateParams(var Params: TCreateParams);
begin
  inherited;
  if (Parent = nil) and IsLibrary and not (csDestroying in ComponentState) then
    Params.WndParent := FDefinition.ParentHandle;
end;  // TQEImportFrame.CreateParams 

{-------------------------------------------------------------------------------
}
procedure TQEImportFrame.DoChangeStatus;
begin
  if Assigned(OnChangeStatus) then OnChangeStatus(Self);
end;  // TQEImportFrame.DoChangeStatus

{+------------------------------------------------------------------------------
  Is this the last page?
}
function TQEImportFrame.GetIsLast: Boolean;
begin
  Result := not Assigned(Next);
end;  // TQEImportFrame.GetIsLast

{+------------------------------------------------------------------------------
  Load any state when the frame is displayed.
}
procedure TQEImportFrame.LoadContent;
begin
end;  // TQEImportFrame.LoadContent

{+------------------------------------------------------------------------------
  Save any state in preparation for leaving the frame.
}
procedure TQEImportFrame.SaveContent;
begin
end;  // TQEImportFrame.SaveContent

{+------------------------------------------------------------------------------
  Set the AllowCancel property.
}
procedure TQEImportFrame.SetAllowCancel(Value: Boolean);
begin
  if Value <> AllowCancel then
  begin
    FAllowCancel := Value;
    DoChangeStatus;
  end;
end;  // TQEImportFrame.SetAllowCancel 

{+------------------------------------------------------------------------------
  Set the IsComplete property.
}
procedure TQEImportFrame.SetIsComplete(Value: Boolean);
begin
  if Value <> IsComplete then
  begin
    FIsComplete := Value;
    DoChangeStatus;
  end;
end;  // TQEImportFrame.SetIsComplete 

{+------------------------------------------------------------------------------
  Set the IsValid property.
}
procedure TQEImportFrame.SetIsValid(Value: Boolean);
begin
  FIsValid := Value;
end;  // TQEImportFrame.SetIsValid 

{+------------------------------------------------------------------------------
  Notify the user of any invalid input values and update IsValid as required.
}
procedure TQEImportFrame.Validate;
begin
end;  // TQEImportFrame.Validate 

end.
