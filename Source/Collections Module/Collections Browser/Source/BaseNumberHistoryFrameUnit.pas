{===============================================================================
  Unit:        BaseNumberHistoryFrameUnit.pas

  Defines:     TBaseNumberHistoryFrame

  Description:

  Created:     May 2003

  Last revision information:
    $Revision: 1 $
    $Date: 15/09/03 10:42 $
    $Author: Markaddis $

===============================================================================}

unit BaseNumberHistoryFrameUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, BaseFullScreenFrameUnit, StdCtrls, ImageListButton, Grids, ExtCtrls,
  BaseDetailFrameUnit;

type
  {-----------------------------------------------------------------------------
    Base class for all number history tab pages.
  }
  TBaseNumberHistoryFrame = class (TBaseFullScreenFrame)
    Label22: TLabel;
    Label24: TLabel;
    lblNumber: TLabel;
    mmNotes: TMemo;
  protected
    procedure RegisterControls; override;
  end;
  
//==============================================================================
implementation

{$R *.dfm}

{ TBaseNumberHistoryFrame }

{-==============================================================================
    TBaseNumberHistoryFrame
===============================================================================}
{-------------------------------------------------------------------------------
}
procedure TBaseNumberHistoryFrame.RegisterControls;
begin
  RegisterControl(mmNotes,'Notes');
end;  // TBaseNumberHistoryFrame.RegisterControls 

end.



