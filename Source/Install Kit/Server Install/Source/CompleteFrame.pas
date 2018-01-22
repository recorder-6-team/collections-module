{===============================================================================
  Unit:        CompleteFrame

  Defines:     TfraComplete

  Description:

  Model:

  Created:     February 2004

  Last revision information:
    $Revision: 1 $
    $Date: 10/03/04 9:14 $
    $Author: Johnvanbreda $

===============================================================================}

unit CompleteFrame;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, BaseStepFrame, StdCtrls;

type
  TfraComplete = class(TfraBaseStep)
    lblWelcomeTitle: TLabel;
    Label2: TLabel;
    Label1: TLabel;
  protected
    function GetFinalCaption: String; override;
    function GetIsFinal: Boolean; override;
  end;

//==============================================================================
implementation

{$R *.dfm}

{-==============================================================================
    TfraComplete
===============================================================================}
{-------------------------------------------------------------------------------
}
function TfraComplete.GetFinalCaption: String;
begin
  Result := '&Finish';
end;  // TfraComplete.GetFinalCaption

{-------------------------------------------------------------------------------
}
function TfraComplete.GetIsFinal: Boolean;
begin
  Result := True;
end;  // TfraComplete.GetIsFinal

end.
