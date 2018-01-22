{===============================================================================
  Unit:        LocalDomain

  Defines:     TfraDomainVersion

  Description:

  Created:     June 2003

  Last revision information:
    $Revision: 3 $
    $Date: 14/01/04 17:26 $
    $Author: Anthonysimpson $

===============================================================================}

unit LocalDomain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, BasePageControlFrameUnit, ComCtrls, BaseTabSheetFrameUnit,
  BaseDetailFrameUnit, LuxembourgConstants;

type
  {-----------------------------------------------------------------------------
    Details page control for viewing and editing a local domain.  This frame 
    consists of an empty tab control onto which the following tabs are embedded 
    in this order: TfraLocalDomainGeneral, TfraMetadata, TfraSources.
    TfraMetadata is linked to the Local_Domain table.
  }
  TfraLocalDomain = class (TBasePageControlFrame)
  protected
    procedure AddTabs; override;
    function GetCaption: string; override;
    procedure SetFrameProperties(AFrame: TBaseDetailFrame); override;
  end;
  
//==============================================================================
implementation

{$R *.dfm}

uses
  LocalDomainGeneral, DomainHyperlinks, FrameMetadata, FrameSources,
  ResourceStrings;

//==============================================================================
{ TfraDomainVersion }

{-==============================================================================
    TfraLocalDomain
===============================================================================}
{-------------------------------------------------------------------------------
}
procedure TfraLocalDomain.AddTabs;
begin
  AddTab(ResStr_General, TfraLocalDomainGeneral);
  AddTab(ResStr_Hyperlinks, TfraDomainHyperlinks);
  AddTab(ResStr_Metadata, TfraMetadata);
  AddTab(ResStr_Sources, TfraSources);
end;  // TfraLocalDomain.AddTabs 

{-------------------------------------------------------------------------------
}
function TfraLocalDomain.GetCaption: string;
begin
  if pcDetails.Pages[0].Controls[0] is TfraLocalDomainGeneral then
    Result := TfraLocalDomainGeneral(pcDetails.Pages[0].Controls[0]).Caption
  else
    raise EBrowserFrameError.Create(ResStr_InvalidTabPage);
end;  // TfraLocalDomain.GetCaption 

{-------------------------------------------------------------------------------
}
procedure TfraLocalDomain.SetFrameProperties(AFrame: TBaseDetailFrame);
begin
  if AFrame is TfraMetadata then
    TfraMetadata(AFrame).TableName := TN_LOCAL_DOMAIN;
end;  // TfraLocalDomain.SetFrameProperties 

end.

