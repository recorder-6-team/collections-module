{===============================================================================
  Unit:        Fact

  Defines:     TfraFact

  Description:

  Created:     June 2003

  Last revision information:
    $Revision: 2 $
    $Date: 29/12/03 15:49 $
    $Author: Anthonysimpson $

===============================================================================}

unit Fact;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, BasePageControlFrameUnit, ComCtrls, BaseTabSheetFrameUnit;

type
  {-----------------------------------------------------------------------------
    Details page control for viewing and editing a thesaurus fact.  This frame 
    consists of an empty tab control onto which the following tabs are embedded 
    in this order: TfraFactGeneral, TfraSources.
  }
  TfraFact = class (TBasePageControlFrame)
  protected
    procedure AddTabs; override;
    function GetCaption: string; override;
  end;
  
//==============================================================================
implementation

{$R *.dfm}

uses
  FactGeneral, FrameSources, ResourceStrings;

{-==============================================================================
    TfraFact
===============================================================================}
{-------------------------------------------------------------------------------
}
procedure TfraFact.AddTabs;
begin
  AddTab(ResStr_General, TfraFactGeneral);
  AddTab(ResStr_Sources, TfraSources);
end;  // TfraFact.AddTabs 

{-------------------------------------------------------------------------------
}
function TfraFact.GetCaption: string;
begin
  if Tabs[0].ContainedFrame is TfraFactGeneral then
    Result := TfraFactGeneral(Tabs[0].ContainedFrame).Caption
  else
    raise EBrowserFrameError.Create(ResStr_InvalidTabPage);
end;  // TfraFact.GetCaption 

end.



