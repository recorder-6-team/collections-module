{===============================================================================
  Unit:        ConceptGroup

  Defines:     TfraConceptGroup

  Description:

  Created:     June 2003

  Last revision information:
    $Revision: 3 $
    $Date: 14/01/04 17:22 $
    $Author: Anthonysimpson $

===============================================================================}

unit ConceptGroup;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, BasePageControlFrameUnit, ComCtrls, BaseTabSheetFrameUnit,
  DataClasses, BaseDetailFrameUnit, LuxembourgConstants;

type
  {-----------------------------------------------------------------------------
    Details page control for viewing and editing a concept group .  This frame 
    consists of an empty tab control onto which the following tabs are embedded 
    in this order: TfraConceptGroupGeneral, TfraMetadata, TfraSources.
    TfraMetadata is linked to the Concept_Group table.
  }
  TfraConceptGroup = class (TBasePageControlFrame)
  protected
    procedure AddTabs; override;
    function GetCaption: string; override;
    procedure SetFrameProperties(AFrame: TBaseDetailFrame); override;
  public
    function GetChildKey: TKeyString;
  end;
  
//==============================================================================
implementation

{$R *.dfm}

uses
  ConceptGroupGeneral, FrameMetadata, FrameSources, ResourceStrings;

//==============================================================================
{-==============================================================================
    TfraConceptGroup
===============================================================================}
{-------------------------------------------------------------------------------
}
procedure TfraConceptGroup.AddTabs;
begin
  AddTab(ResStr_General, TfraConceptGroupGeneral);
  AddTab(ResStr_Metadata, TfraMetadata);
  AddTab(ResStr_Sources, TfraSources);
end;  // TfraConceptGroup.AddTabs 

{-------------------------------------------------------------------------------
}
function TfraConceptGroup.GetCaption: string;
begin
  if pcDetails.Pages[0].Controls[0] is TfraConceptGroupGeneral then
    Result := TfraConceptGroupGeneral(pcDetails.Pages[0].Controls[0]).Caption
  else
    raise EBrowserFrameError.Create(ResStr_InvalidTabPage);
end;  // TfraConceptGroup.GetCaption 

{-------------------------------------------------------------------------------
}
function TfraConceptGroup.GetChildKey: TKeyString;
begin
  Result := TfraConceptGroupGeneral(pcDetails.Pages[0].Controls[0]).ChildKey;
end;  // TfraConceptGroup.GetChildKey 

{-------------------------------------------------------------------------------
}
procedure TfraConceptGroup.SetFrameProperties(AFrame: TBaseDetailFrame);
begin
  if AFrame is TfraMetadata then
    TfraMetadata(AFrame).TableName := TN_CONCEPT_GROUP;
end;  // TfraConceptGroup.SetFrameProperties 

end.

