{===============================================================================
  Unit:        UpgradeFrame

  Defines:     TfraUpgrade

  Description: Progress splash screen for Recorder database upgrades.

  Created:

  Last revision information:
    $Revision: 1 $
    $Date: 27/08/04 12:51 $
    $Author: Johnvanbreda $

===============================================================================}
unit BaseFrameUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, StdCtrls, Settings, ExtCtrls;

type
  {-----------------------------------------------------------------------------
    Base frame for each page in the upgrade wizard.
  }
  TBaseFrame = class (TFrame)
  public
    procedure ApplySettings(ASettings: TSettings); virtual;
    function CreateNextFrame(AOwner: TComponent): TBaseFrame; virtual;
    procedure Execute(ASettings: TSettings); virtual;
  end;
  
//==============================================================================

implementation

{$R *.dfm}

{-==============================================================================
    TBaseFrame
===============================================================================}
{-------------------------------------------------------------------------------
  Any empty stub for the ApplySettings is used rather than an abstract method because this 
      allows the derived frames to not implement this method if no action required. 
}
procedure TBaseFrame.ApplySettings(ASettings: TSettings);
begin
end;  // TBaseFrame.ApplySettings 

{-------------------------------------------------------------------------------
  Base method for returning the next frame in sequence. 
}
function TBaseFrame.CreateNextFrame(AOwner: TComponent): TBaseFrame;
begin
  Result := nil;
end;  // TBaseFrame.CreateNextFrame 

{-------------------------------------------------------------------------------
  Virtual execute method - empty rather than abstract so that it does not have to be 
      implemented.
  This is called as soon as a frame is shown, allowing it to perform any actions. 
}
procedure TBaseFrame.Execute(ASettings: TSettings);
begin
  // do nothing
end;  // TBaseFrame.Execute 

end.





