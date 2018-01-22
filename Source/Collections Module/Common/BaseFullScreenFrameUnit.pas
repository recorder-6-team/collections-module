{===============================================================================
  Unit:        BaseFullScreenFrame.pas

  Defines:     TBaseFullScreenFrame

  Description: Contains a Bevel to show some border around the content.

  Model:       CollectionBrowserFramework.mpb

  Created:     June 2003

  Last revision information:
    $Revision: 4 $
    $Date: 4/02/04 10:44 $
    $Author: Anthonysimpson $

===============================================================================}

unit BaseFullScreenFrameUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, BaseDetailFrameUnit, ExtCtrls;

type
  {-----------------------------------------------------------------------------
    Base class for details screens that consist of a single frame (as opposed to details 
    frames that contain a page control with several tab pages).
    This class enforces consistency of appearance by placing a bevel around the frame.  The 
    bevel remains in the same place for all descendant frames.
    The frame is otherwise empty.  Controls are added to this frame by details pages descended 
    from it.
  }
  TBaseFullScreenFrame = class (TBaseDetailFrame)
    bvlBorder: TBevel;
  protected
    procedure SaveData; override;
  end;
  
//==============================================================================
implementation

{-==============================================================================
    TBaseFullScreenFrame
===============================================================================}
{-------------------------------------------------------------------------------
  Frames that use this base class don't automatically do any validation. Therefore, there is a 
      save data method here that calls ValidateContent. All frames that use this base class 
      should use 'inherited' in their SaveData methods to ensure they come here are do 
      validation.
}
procedure TBaseFullScreenFrame.SaveData;
begin
  ValidateContent;
end;  // TBaseFullScreenFrame.SaveData 

{$R *.dfm}

end.



