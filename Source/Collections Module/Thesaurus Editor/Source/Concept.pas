{===============================================================================
  Unit:        Concept

  Defines:     TfraConcept

  Description: Container frame for concept tabs

  Created:     June 2003

  Last revision information:
    $Revision: 6 $
    $Date: 2/09/11 16:03 $
    $Author: Jamesbichard $

===============================================================================}

unit Concept;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, BasePageControlFrameUnit, ComCtrls, BaseTabSheetFrameUnit,
  TermGenerationFunctions;

type
  {-----------------------------------------------------------------------------
    Details page control for viewing and editing the core details of a concept. 
    This frame consists of an empty tab control onto which the following tabs 
    are embedded in this order: TfraConceptGeneral, TfraMetadata, TfraSources.
    TfraMetadata is linked to the Concept table.
  }
  TfraConcept = class (TBasePageControlFrame)
  protected
    procedure AddTabs; override;
    function GetCaption: string; override;
    procedure LoadPage(APageIndex: Integer; ALoadFrameData: Boolean = True); override;
  public
    procedure RefreshRankCombo;
  end;

//==============================================================================
implementation

{$R *.dfm}

uses
  ConceptGeneral, FrameSources, ResourceStrings, SearchTerms;

//==============================================================================
{ TBasePageControlFrame1 }

{-==============================================================================
    TfraConcept
===============================================================================}
{-------------------------------------------------------------------------------
}
procedure TfraConcept.AddTabs;
begin
  AddTab(ResStr_General, TfraConceptGeneral);
  AddTab(ResStr_Sources, TfraSources);
  AddTab(ResStr_SearchTerms, TfraSearchTerms);
end;  // TfraConcept.AddTabs

{-------------------------------------------------------------------------------
}
function TfraConcept.GetCaption: string;
begin
  if Tabs[0].ContainedFrame is TfraConceptGeneral then
    Result := TfraConceptGeneral(Tabs[0].ContainedFrame).Caption
  else
    raise EBrowserFrameError.Create(ResStr_InvalidTabPage);
end;  // TfraConcept.GetCaption

{-------------------------------------------------------------------------------
  If the Rank combo is loaded, then refresh its list.
}
procedure TfraConcept.RefreshRankCombo;
begin
  if Tabs[0].ContainedFrame is TfraConceptGeneral then
    TfraConceptGeneral(Tabs[0].ContainedFrame).cmbRankPopulate(nil);
end;

{-------------------------------------------------------------------------------
  Search terms tab has to be reloaded whenever it is selected to get the most
  up to date search terms
}
procedure TfraConcept.LoadPage(APageIndex: Integer; ALoadFrameData: Boolean = True);
var
  lParams: Array of Variant;
  lTermGeneratorKey: string;
  procedure UpdateAutomaticSearchTerms(
      AParams: Array of Variant;
      ATermGeneratorKey: string);
  begin
    with TfraSearchTerms(Tabs[2].ContainedFrame) do
    begin
      mmAutomaticTerms.Clear;
      with GetSearchTerms(ATermGeneratorKey, AParams) do
      begin
        while not EOF do
        begin
          mmAutomaticTerms.Lines.Add(VarToStr(Fields['Search_Term'].Value));
          MoveNext;
        end;
      end;
    end;
  end;
begin
  inherited;
  if (Tabs[2].ContainedFrame is TfraSearchTerms) and
      (Tabs[0].ContainedFrame is TfraConceptGeneral) and (APageIndex = 2) then
  begin
    with TfraConceptGeneral(Tabs[0].ContainedFrame) do
    begin
      lParams := VarArrayOf([
        '@Plaintext', eTerm.Text,
        '@AuthorAndDate', eAuthorAndDate.Text,
        '@Attributes', eAttributes.Text,
        '@RankKey', cmbRank.CurrentStrID,
        '@ParentConceptKey', ParentKey,
        '@PublishedTerm', ePublishedTerm.Text]);
      lTermGeneratorKey := FindTermGeneratorKey;
    end;

    UpdateAutomaticSearchTerms(lParams, lTermGeneratorKey);
  end;
end;

end.



