{===============================================================================
  Unit:        SubjectArea

  Defines:     TfraSubjectArea

  Description:

  Created:     June 2003

  Last revision information:
    $Revision: 2 $
    $Date: 28/11/03 10:53 $
    $Author: Anthonysimpson $

===============================================================================}

unit SubjectArea;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, BasePageControlFrameUnit, ComCtrls;

type
  {-----------------------------------------------------------------------------
    Details page control for viewing and editing a subject area.  This frame 
    consists of an empty tab control onto which the following tabs are embedded 
    in this order: TfraSubjectAreaGeneral, TfraSources.  The frame is embedded 
    onto the List Organiser screen when the user is viewing or editing subject 
    area details.
  }
  TfraSubjectArea = class (TBasePageControlFrame)
  protected
    procedure AddTabs; override;
    function GetCaption: String; override;
  end;
  
//==============================================================================
implementation

{$R *.dfm}

uses
  SubjectAreaGeneral, FrameSources, BaseTabSheetFrameUnit, ResourceStrings;

//==============================================================================
{-==============================================================================
    TfraSubjectArea
===============================================================================}
{-------------------------------------------------------------------------------
}
procedure TfraSubjectArea.AddTabs;
begin
  AddTab(ResStr_General, TfraSubjectAreaGeneral);
  AddTab(ResStr_Sources, TfraSources);
end;  // TfraSubjectArea.AddTabs

{-------------------------------------------------------------------------------
}
function TfraSubjectArea.GetCaption: String;
begin
  if pcDetails.Pages[0].Controls[0] is TfraSubjectAreaGeneral then
    Result := TfraSubjectAreaGeneral(pcDetails.Pages[0].Controls[0]).Caption
  else
    raise EBrowserFrameError.Create(ResStr_InvalidTabPage);
end;  // TfraHistory.GetCaption

end.


