{===============================================================================
  Unit:        BaseTabSheetFrameUnit.pas

  Defines:     TBaseTabSheetFrame

  Description: Base frame for frames loaded on TPageControlFrame descendents.

  Model:       CollectionBrowserFramework.mpb

  Created:     May 2003

  Last revision information:
    $Revision: 9 $
    $Date: 4/02/04 14:02 $
    $Author: Andrewkemp $

===============================================================================}

unit BaseTabSheetFrameUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, BaseDetailFrameUnit, ExtCtrls, DataTypes, ExceptionForm;

type
  EBrowserFrameError = class (TExceptionPath)
  end;
  
  {-----------------------------------------------------------------------------
    Base frame for details frames that are displayed as a tab on a tabbed notebook.  This 
    class enforces consistency of appearance by placing a bevel around the frame.  The 
    bevel remains in the same place for all descendant frames.
    The frame is otherwise empty.  Controls are added to this frame by details pages 
    descended from it.
  }
  TBaseTabSheetFrame = class (TBaseDetailFrame)
    bvlBorder: TBevel;
  private
    FMasterFrameType: TMasterFrameType;
  protected
    procedure SetMasterFrameType(Value: TMasterFrameType); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    property MasterFrameType: TMasterFrameType read FMasterFrameType write 
        SetMasterFrameType;
  end;
  
//==============================================================================
implementation

{$R *.dfm}

{-==============================================================================
    TBaseTabSheetFrame
===============================================================================}
{-------------------------------------------------------------------------------
}
constructor TBaseTabSheetFrame.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  
  FMasterFrameType := mftUnspecified;
end;  // TBaseTabSheetFrame.Create 

{-------------------------------------------------------------------------------
}
procedure TBaseTabSheetFrame.SetMasterFrameType(Value: TMasterFrameType);
begin
  // Defaults to unspecified.
  FMasterFrameType := Value;
end;  // TBaseTabSheetFrame.SetMasterFrameType 

end.

