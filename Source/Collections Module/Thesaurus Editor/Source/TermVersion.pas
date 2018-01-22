{===============================================================================
  Unit:        TermVersion

  Defines:     TfraTermVersion

  Description: Page control containing tabs for editing a term version

  Created:     June 2003

  Last revision information:
    $Revision: 6 $
    $Date: 24/06/05 11:20 $
    $Author: Johnvanbreda $

===============================================================================}

unit TermVersion;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, BasePageControlFrameUnit, ComCtrls, BaseTabSheetFrameUnit,
  BaseDetailFrameUnit, LuxembourgConstants;

type
  {-----------------------------------------------------------------------------
    Details page control for viewing and editing a term version.  This frame 
    consists of an empty tab control onto which the following tabs are embedded 
    in this order: TfraTermVersionGeneral, TfraSources.
    This is only editable for the term version linked to the current concept, 
    if viewing other related term versions of the concept then the page control 
    and its contents are always read only.
  }
  TfraTermVersion = class (TBasePageControlFrame)
  protected
    procedure AddTabs; override;
    function GetCaption: string; override;
    procedure SetFrameProperties(AFrame: TBaseDetailFrame); override;
    procedure ValidateTabContents; override;
  end;
  

implementation

{$R *.dfm}

uses
  TermVersionGeneral, FrameMetadata, FrameSources, ResourceStrings,
  ExceptionForm;

{-==============================================================================
    TfraTermVersion
===============================================================================}
{-------------------------------------------------------------------------------
  Create the page control's tabs. 
}
procedure TfraTermVersion.AddTabs;
begin
  AddTab(ResStr_General, TfraTermVersionGeneral);
  AddTab(ResStr_Metadata, TfraMetadata);
  AddTab(ResStr_Sources, TfraSources);
end;  // TfraTermVersion.AddTabs 

{-------------------------------------------------------------------------------
}
function TfraTermVersion.GetCaption: string;
begin
  if Tabs[0].ContainedFrame is TfraTermVersionGeneral then
    Result := TfraTermVersionGeneral(Tabs[0].ContainedFrame).Caption
  else
    raise EBrowserFrameError.Create(ResStr_InvalidTabPage);
end;  // TfraTermVersion.GetCaption 

{-------------------------------------------------------------------------------
}
procedure TfraTermVersion.SetFrameProperties(AFrame: TBaseDetailFrame);
begin
  if AFrame is TfraMetadata then
    TfraMetadata(AFrame).TableName := TN_TERM_VERSION;
end;  // TfraTermVersion.SetFrameProperties 

{-------------------------------------------------------------------------------
}
procedure TfraTermVersion.ValidateTabContents;
var
  I: Integer;
  lGeneral: TfraTermVersionGeneral;
  lMetadata: TfraMetadata;
  lSources: TfraSources;
begin
  inherited;
  
  for I := 0 to 2 do
    if not Tabs[I].IsLoaded then
      LoadPage(I);
            
  lGeneral := Tabs[0].ContainedFrame as TfraTermVersionGeneral;
  lMetadata := Tabs[1].ContainedFrame as TfraMetadata;
  lSources := Tabs[2].ContainedFrame as TfraSources;

  ValidateValue(
      lGeneral.HasData or not (lMetadata.HasData or lSources.HasData),
      ResStr_TermVersionHasNoMasterData,
      lGeneral.eLabel);
end;  // TfraTermVersion.ValidateTabContents

end.


