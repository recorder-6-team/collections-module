{===============================================================================
  Unit:        Designation

  Defines:     TfraDesignation

  Description:

  Created:     June 2003

  Last revision information:
    $Revision: 3 $
    $Date: 14/01/04 17:23 $
    $Author: Anthonysimpson $

===============================================================================}

unit Designation;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, BasePageControlFrameUnit, ComCtrls, BaseDetailFrameUnit,
  BaseTabSheetFrameUnit, LuxembourgConstants;

type
  {-----------------------------------------------------------------------------
    Details page control for viewing and editing a designation.  This frame 
    consists of an empty tab control onto which the following tabs are embedded 
    in this order: TfraDesignationGeneral, TfraMetadata, TfraSources.
  }
  TfraDesignation = class (TBasePageControlFrame)
  protected
    procedure AddTabs; override;
    function GetCaption: string; override;
    procedure SetFrameProperties(AFrame: TBaseDetailFrame); override;
  end;
  
//==============================================================================
implementation

{$R *.dfm}

uses
  DesignationGeneral, FrameSources, FrameMetadata, ResourceStrings;

{-==============================================================================
    TfraDesignation
===============================================================================}
{-------------------------------------------------------------------------------
}
procedure TfraDesignation.AddTabs;
begin
  AddTab(ResStr_General, TfraDesignationGeneral);
  AddTab(ResStr_Metadata, TfraMetadata);
  AddTab(ResStr_Sources, TfraSources);
end;  // TfraDesignation.AddTabs 

{-------------------------------------------------------------------------------
}
function TfraDesignation.GetCaption: string;
begin
  if Tabs[0].ContainedFrame is TfraDesignationGeneral then
    Result := TfraDesignationGeneral(Tabs[0].ContainedFrame).Caption
  else
    raise EBrowserFrameError.Create(ResStr_InvalidTabPage);
end;  // TfraDesignation.GetCaption 

{-------------------------------------------------------------------------------
}
procedure TfraDesignation.SetFrameProperties(AFrame: TBaseDetailFrame);
begin
  if AFrame is TfraMetadata then
    TfraMetadata(AFrame).TableName := TN_CONCEPT_DESIGNATION;
end;  // TfraDesignation.SetFrameProperties 

end.


