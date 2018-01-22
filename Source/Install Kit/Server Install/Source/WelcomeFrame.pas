{===============================================================================
  Unit:        WelcomeFrame

  Defines:     TfraWelcome

  Description: Welcome screen.

  Created:     September 2003

  Last revision information:
    $Revision: 2 $
    $Date: 14/05/07 16:42 $
    $Author: Johnvanbreda $

===============================================================================}

unit WelcomeFrame;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, BaseStepFrame, StdCtrls;

type
  TfraWelcome = class (TfraBaseStep)
    Label2: TLabel;
    Label3: TLabel;
    lblWelcomeText: TLabel;
    lblWelcomeTitle: TLabel;
  protected
    function GetHasNext: Boolean; override;
    function GetNext: TStepFrameClass; override;
  end;
  
//==============================================================================
implementation

{$R *.dfm}

uses
  DatabaseFrame, ServerDetailsFrame;

{-==============================================================================
    TfraWelcome
===============================================================================}
{-------------------------------------------------------------------------------
}
function TfraWelcome.GetHasNext: Boolean;
begin
  Result := True;
end;  // TfraWelcome.GetHasNext

{-------------------------------------------------------------------------------
}
function TfraWelcome.GetNext: TStepFrameClass;
begin
  // If not installed on this machine, then we need to ask for the instance
  // and database as not in registry
  if Settings.NeedServerDetails then
    Result := TfraServerDetails
  else
    Result := TfraDatabase;
end;  // TfraWelcome.GetNext 

end.

