{===============================================================================
  Unit:        ThesaurusBrowserDetails

  Defines:     TfraThesaurusBrowserDetails

  Description: Page control onto which the Details and Relationships tabs for
               the details part of the Thesaurus Browser are loaded.

  Created:     September 2003

  Last revision information:
    $Revision: 2 $
    $Date: 12/09/03 17:27 $
    $Author: Johnvanbreda $

===============================================================================}
unit ThesaurusBrowserDetails;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, BasePageControlFrameUnit, BaseDetailFrameUnit, ComCtrls,
  ConceptHTMLDetail, ConceptRelationships, ResourceStrings, DebuggerTracker;

type
  {-----------------------------------------------------------------------------
    Page control onto which the Details and Relationships tabs for the details 
    part of the Thesaurus Browser are loaded.
  }
  TfraThesaurusBrowserDetails = class (TBasePageControlFrame)
  protected
    procedure AddTabs; override;
  end;
  

implementation

{$R *.dfm}

{-==============================================================================
    TfraThesaurusBrowserDetails
===============================================================================}
{-------------------------------------------------------------------------------
  Create the required tabs. 
}
procedure TfraThesaurusBrowserDetails.AddTabs;
begin
  AddTab(ResStr_Details, TfraConceptHTMLDetail);
  AddTab(ResStr_Relationships, TfraConceptRelationships);
end;  // TfraThesaurusBrowserDetails.AddTabs 

end.



