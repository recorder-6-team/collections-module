{===============================================================================
  Unit:        FrameCondition.pas

  Defines:     TfraCondition

  Description:

  Created:     May 2003

  Last revision information:
    $Revision: 8 $
    $Date: 9/02/04 17:31 $
    $Author: Bencollier $

===============================================================================}

unit FrameCondition;

interface

uses
  Windows, SysUtils, Classes, Graphics, Controls, Forms, ComCtrls,
  BasePageControlFrameUnit, BaseTabSheetFrameUnit, ResourceStrings;

type
  {-----------------------------------------------------------------------------
    Details page control for viewing and editing a condition check.  This frame consists 
    of an empty tab control onto which the following tabs are embedded in this order: 
    TfraConditionGeneral, TfraSources.
  }
  TfraCondition = class (TBasePageControlFrame)
  protected
    procedure AddTabs; override;
  end;
  
//==============================================================================
implementation

{$R *.dfm}

uses
  FrameConditionGeneral, FrameSources;

{-==============================================================================
    TfraCondition
===============================================================================}
{-------------------------------------------------------------------------------
  Setup the page control to contain a general tab and a sources tab. 
}
procedure TfraCondition.AddTabs;
begin
  AddTab('General', TfraConditionGeneral);
  AddTab('Sources', TfraSources);
end;  // TfraCondition.AddTabs 

end.


