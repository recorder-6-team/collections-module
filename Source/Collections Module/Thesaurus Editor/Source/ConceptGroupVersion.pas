{===============================================================================
  Unit:        ConceptGroupVersion

  Defines:     TfraConceptGroupVersion

  Description:

  Created:     June 2003

  Last revision information:
    $Revision: 3 $
    $Date: 14/01/04 17:23 $
    $Author: Anthonysimpson $

===============================================================================}

unit ConceptGroupVersion;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, BasePageControlFrameUnit, ComCtrls, BaseTabSheetFrameUnit,
  BaseDetailFrameUnit, LuxembourgConstants;

type
  {-----------------------------------------------------------------------------
    Details page control for viewing and editing a concept group version.  This 
    frame consists of an empty tab control onto which the following tabs are 
    embedded in this order: TfraConceptGroupVersionGeneral, TfraMetadata, 
    TfraSources.
    TfraMetadata is linked to the Concept_Group_Version table.
  }
  TfraConceptGroupVersion = class (TBasePageControlFrame)
  protected
    procedure AddTabs; override;
    function GetCaption: string; override;
    procedure SetFrameProperties(AFrame: TBaseDetailFrame); override;
  end;
  
//==============================================================================
implementation

{$R *.dfm}

uses
  ConceptGroupVersionGeneral, FrameMetadata, FrameSources, ResourceStrings;

{-==============================================================================
    TfraConceptGroupVersion
===============================================================================}
{-------------------------------------------------------------------------------
}
procedure TfraConceptGroupVersion.AddTabs;
begin
  AddTab(ResStr_General, TfraConceptGroupVersionGeneral);
  AddTab(ResStr_Metadata, TfraMetadata);
  AddTab(ResStr_Sources, TfraSources);
end;  // TfraConceptGroupVersion.AddTabs 

{-------------------------------------------------------------------------------
}
function TfraConceptGroupVersion.GetCaption: string;
begin
  if pcDetails.Pages[0].Controls[0] is TfraConceptGroupVersionGeneral then
    Result := TfraConceptGroupVersionGeneral(pcDetails.Pages[0].Controls[0]).Caption
  else
    raise EBrowserFrameError.Create(ResStr_InvalidTabPage);
end;  // TfraConceptGroupVersion.GetCaption 

{-------------------------------------------------------------------------------
}
procedure TfraConceptGroupVersion.SetFrameProperties(AFrame: TBaseDetailFrame);
begin
  if AFrame is TfraMetadata then
    TfraMetadata(AFrame).TableName := TN_CONCEPT_GROUP_VERSION;
end;  // TfraConceptGroupVersion.SetFrameProperties 


end.




