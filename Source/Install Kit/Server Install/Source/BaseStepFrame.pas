{===============================================================================
  Unit:        BaseStepFrame

  Defines:     TfraBaseStep

  Description: Base class for steps followed during the install.

  Created:     September 2003

  Last revision information:
    $Revision: 1 $
    $Date: 10/03/04 9:14 $
    $Author: Johnvanbreda $

===============================================================================}

unit BaseStepFrame;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Settings;

type
  TStepFrameClass = class of TfraBaseStep;

  TfraBaseStep = class (TFrame)
  private
    FOnChangedContent: TNotifyEvent;
    FSettings: TSettings;
  protected
    procedure ChangedContent;
    function GetFinalCaption: String; virtual;
    function GetHasNext: Boolean; virtual;
    function GetHasPrevious: Boolean; virtual;
    function GetIsFinal: Boolean; virtual;
    function GetNext: TStepFrameClass; virtual;
    function GetNextCaption: String; virtual;
    function GetPrevious: TStepFrameClass; virtual;
    procedure LoadContent; virtual;
    procedure SaveContent; virtual;
  public
    constructor Create(AOwner: TComponent; ASettings: TSettings); reintroduce; virtual;
    destructor Destroy; override;
    function Execute: Boolean; virtual;
    procedure ValidateContent; virtual;
    property FinalCaption: String read GetFinalCaption;
    property HasNext: Boolean read GetHasNext;
    property HasPrevious: Boolean read GetHasPrevious;
    property IsFinal: Boolean read GetIsFinal;
    property Next: TStepFrameClass read GetNext;
    property NextCaption: String read GetNextCaption;
    property OnChangedContent: TNotifyEvent read FOnChangedContent write FOnChangedContent;
    property Previous: TStepFrameClass read GetPrevious;
    property Settings: TSettings read FSettings;
  end;

//==============================================================================
implementation

{$R *.dfm}

{-==============================================================================
    TfraBaseStep
===============================================================================}
{-------------------------------------------------------------------------------
}
constructor TfraBaseStep.Create(AOwner: TComponent; ASettings: TSettings);
begin
  inherited Create(AOwner);
  FSettings := ASettings;
  LoadContent;
end;  // TfraBaseStep.Create

{-------------------------------------------------------------------------------
}
destructor TfraBaseStep.Destroy;
begin
  SaveContent;
  inherited;
end;  // TfraBaseStep.Destroy

{-------------------------------------------------------------------------------
}
procedure TfraBaseStep.ChangedContent;
begin
  if Assigned(FOnChangedContent) then
    FOnChangedContent(Self);
end;  // TfraBaseStep.ChangedContent

{-------------------------------------------------------------------------------
}
function TfraBaseStep.Execute: Boolean;
begin
  Result := False;
end;  // TfraBaseStep.Execute

{-------------------------------------------------------------------------------
}
function TfraBaseStep.GetFinalCaption: String;
begin
  Result := '&Cancel';
end;  // TfraBaseStep.GetFinalCaption

{-------------------------------------------------------------------------------
}
function TfraBaseStep.GetHasNext: Boolean;
begin
  Result := False;
end;  // TfraBaseStep.GetHasNext

{-------------------------------------------------------------------------------
}
function TfraBaseStep.GetHasPrevious: Boolean;
begin
  Result := False;
end;  // TfraBaseStep.GetHasPrevious

{-------------------------------------------------------------------------------
}
function TfraBaseStep.GetIsFinal: Boolean;
begin
  Result := False;
end;  // TfraBaseStep.GetIsFinal

{-------------------------------------------------------------------------------
}
function TfraBaseStep.GetNext: TStepFrameClass;
begin
  Result := nil;
end;  // TfraBaseStep.GetNext

{-------------------------------------------------------------------------------
}
function TfraBaseStep.GetNextCaption: String;
begin
  Result := '&Next >';
end;  // TfraBaseStep.GetNextCaption

{-------------------------------------------------------------------------------
}
function TfraBaseStep.GetPrevious: TStepFrameClass;
begin
  Result := nil;
end;  // TfraBaseStep.GetPrevious

{-------------------------------------------------------------------------------
}
procedure TfraBaseStep.LoadContent;
begin
  // Override as and when required.
end;  // TfraBaseStep.LoadContent

{-------------------------------------------------------------------------------
}
procedure TfraBaseStep.SaveContent;
begin
  // Override as and when required.
end;  // TfraBaseStep.SaveContent

{-------------------------------------------------------------------------------
}
procedure TfraBaseStep.ValidateContent;
begin
  // Override as and when required.
end;  // TfraBaseStep.ValidateContent

end.
