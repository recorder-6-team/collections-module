{===============================================================================
  Unit:        Synonym

  Defines:     TfraSynonym

  Description: Container frame for synonyms

  Created:     30/6/2003

  Last revision information:
    $Revision: 4 $
    $Date: 2/04/04 11:10 $
    $Author: Johnvanbreda $

===============================================================================}
unit Synonym;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, BasePageControlFrameUnit, ComCtrls, BaseTabSheetFrameUnit,
  DataClasses, LuxembourgConstants, GeneralData;

type
  {-----------------------------------------------------------------------------
    Details page control for viewing and editing the core details of a synonym 
    on a list.  This frame consists of an empty tab control onto which the 
    following tabs are embedded in this order: TfraConceptGeneral, 
    TfraTermVersionGeneral.  TfraTermVersionGeneral is ommitted if the synonym 
    concept has no term version information.  In this case the concept, term 
    and term version that these tabs are bound to are those of the selected 
    synonym, not the concept selected in the browser hierarchy.
    This page control and its contents are always read only.  They cannot be
    used to edit the synonym details.
  }
  TfraSynonym = class (TBasePageControlFrame)
  protected
    procedure AddTabs; override;
    function GetCaption: string; override;
    procedure LoadData; override;
  end;

var
  fraSynonym: TfraSynonym;

implementation

uses
  ConceptGeneral, TermVersionGeneral, ResourceStrings;

{$R *.dfm}

{-==============================================================================
    TfraSynonym
===============================================================================}
{-------------------------------------------------------------------------------
}
procedure TfraSynonym.AddTabs;
begin
  AddTab(ResStr_General, TfraConceptGeneral);
  AddTab(ResStr_VersionDetails, TfraTermVersionGeneral);
end;  // TfraSynonym.AddTabs 

{-------------------------------------------------------------------------------
}
function TfraSynonym.GetCaption: string;
begin
  if Tabs[0].ContainedFrame is TfraConceptGeneral then
    Result := TfraConceptGeneral(Tabs[0].ContainedFrame).Caption
  else
    raise EBrowserFrameError.Create(ResStr_InvalidTabPage);
end;  // TfraSynonym.GetCaption 

{-------------------------------------------------------------------------------
  Synonyms that have a Term Version Key should show the Term Version tab page. Otherwise it shouldn't be shown. 
}
procedure TfraSynonym.LoadData;
var
  lTermVersionKey: TKeyString;
begin
  inherited;
  with dmGeneral.GetRecordset('usp_Concept_Select', ['@ConceptKey', Key]) do
    if not EOF then lTermVersionKey := VarToStr(Fields['Term_Version_Key'].Value);
  pcDetails.Pages[1].TabVisible := lTermVersionKey <> '';
end;  // TfraSynonym.LoadData 



end.


