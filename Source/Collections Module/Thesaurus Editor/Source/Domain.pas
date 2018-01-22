{===============================================================================
  Unit:        Domain

  Defines:     TfraDomain

  Description: Page control in which Domain details are displayed

  Created:     June 2003

  Last revision information:
    $Revision: 3 $
    $Date: 14/01/04 17:23 $
    $Author: Anthonysimpson $

===============================================================================}

unit Domain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, BasePageControlFrameUnit, ComCtrls, BaseTabSheetFrameUnit,
  ResourceStrings, BaseDetailFrameUnit, LuxembourgConstants;

type
  {-----------------------------------------------------------------------------
    Details page control for viewing and editing a domain.  This frame consists 
    of an empty tab control onto which the following tabs are embedded in this 
    order: TfraDomainGeneral, TfraMetadata, TfraSources.
    TfraMetadata is linked to the Domain table.
  }
  TfraDomain = class (TBasePageControlFrame)
  protected
    procedure AddTabs; override;
    function GetCaption: string; override;
    procedure SetFrameProperties(AFrame: TBaseDetailFrame); override;
  end;
  
//==============================================================================
implementation

{$R *.dfm}

uses
  DomainGeneral, DomainRank, FrameMetadata, FrameSources;

//==============================================================================
{-==============================================================================
    TfraDomain
===============================================================================}
{-------------------------------------------------------------------------------
}
procedure TfraDomain.AddTabs;
begin
  AddTab(ResStr_General, TfraDomainGeneral);
  AddTab(ResStr_Ranks, TfraDomainRank);
  AddTab(ResStr_Metadata, TfraMetadata);
  AddTab(ResStr_Sources, TfraSources);
end;  // TfraDomain.AddTabs 

{-------------------------------------------------------------------------------
}
function TfraDomain.GetCaption: string;
begin
  if pcDetails.Pages[0].Controls[0] is TfraDomainGeneral then
    Result := TfraDomainGeneral(pcDetails.Pages[0].Controls[0]).Caption
  else
    raise EBrowserFrameError.Create(ResStr_InvalidTabPage);
end;  // TfraDomain.GetCaption 

{-------------------------------------------------------------------------------
}
procedure TfraDomain.SetFrameProperties(AFrame: TBaseDetailFrame);
begin
  if AFrame is TfraMetadata then
    TfraMetadata(AFrame).TableName := TN_DOMAIN;
end;  // TfraDomain.SetFrameProperties 

end.


