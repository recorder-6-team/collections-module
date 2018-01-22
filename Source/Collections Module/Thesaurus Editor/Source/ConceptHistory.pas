{===============================================================================
  Unit:        ConceptHistory

  Defines:     TfraConceptHistory

  Description:

  Created:     June 2003

  Last revision information:
    $Revision: 2 $
    $Date: 29/12/03 15:49 $
    $Author: Anthonysimpson $

===============================================================================}

unit ConceptHistory;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, BasePageControlFrameUnit, ComCtrls, BaseTabSheetFrameUnit;

type
  {-----------------------------------------------------------------------------
    Concepts apply to one or more ranges of concept group versions within a 
    concept group.  For example, a concept may apply from versions 1 to 3, and 
    from 6 to 8, but not 4 to 5.  This frame allows the boundaries of a single 
    range of versions that apply to the concept to be defined.  It is embedded 
    into TfrmConcepts when the user is editing a version range.
  }
  TfraConceptHistory = class (TBasePageControlFrame)
  protected
    procedure AddTabs; override;
    function GetCaption: string; override;
  end;
  
//==============================================================================
implementation

{$R *.dfm}

uses
  ConceptHistoryDetails, FrameSources, ResourceStrings;

//==============================================================================
{ TBasePageControlFrame1 }

{-==============================================================================
    TfraConceptHistory
===============================================================================}
{-------------------------------------------------------------------------------
}
procedure TfraConceptHistory.AddTabs;
begin
  AddTab(ResStr_General, TfraConceptHistoryDetails);
  AddTab(ResStr_Sources, TfraSources);
end;  // TfraConceptHistory.AddTabs 

{-------------------------------------------------------------------------------
}
function TfraConceptHistory.GetCaption: string;
begin
  if Tabs[0].ContainedFrame is TfraConceptHistoryDetails then
    Result := TfraConceptHistoryDetails(Tabs[0].ContainedFrame).Caption
  else
    raise EBrowserFrameError.Create(ResStr_InvalidTabPage);
end;  // TfraConceptHistory.GetCaption 

end.
