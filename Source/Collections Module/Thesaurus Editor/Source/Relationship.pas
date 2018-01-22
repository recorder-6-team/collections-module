{===============================================================================
  Unit:        Relationship

  Defines:     TfraRelationship

  Description:

  Model:       ThesaurusEditor.mpb

  Created:     June 2003

  Last revision information:
    $Revision: 4 $
    $Date: 22/12/03 11:00 $
    $Author: Anthonysimpson $

===============================================================================}

unit Relationship;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, BasePageControlFrameUnit, ComCtrls, BaseDetailFrameUnit,
  BaseTabSheetFrameUnit, DataTypes, DataClasses;

type
  {-----------------------------------------------------------------------------
    Details page control for viewing and editing a relationship between 2 
    concepts or meanings.  This frame consists of an empty tab control onto 
    which the following tabs are embedded in this order: 
    TfraRelationshipGeneral, TfraSources.
  }
  TfraRelationship = class (TBasePageControlFrame)
  private
  protected
    procedure AddTabs; override;
    function GetCaption: string; override;
    procedure SetFrameProperties(AFrame: TBaseDetailFrame); override;
  public
    function GetRelationshipType: TAppliesTo;
  end;
  
//==============================================================================
implementation

{$R *.dfm}

uses
  RelationshipGeneral, FrameSources, ResourceStrings;

{-==============================================================================
    TfraRelationship
===============================================================================}
{-------------------------------------------------------------------------------
}
procedure TfraRelationship.AddTabs;
begin
  AddTab(ResStr_General, TfraRelationshipGeneral);
  AddTab(ResStr_Sources, TfraSources);
end;  // TfraRelationship.AddTabs 

{-------------------------------------------------------------------------------
}
function TfraRelationship.GetCaption: string;
begin
  if Tabs[0].ContainedFrame is TfraRelationshipGeneral then
    Result := TfraRelationshipGeneral(Tabs[0].ContainedFrame).Caption
  else
    raise EBrowserFrameError.Create(ResStr_InvalidTabPage);
end;  // TfraRelationship.GetCaption 

{-------------------------------------------------------------------------------
}
procedure TfraRelationship.SetFrameProperties(AFrame: TBaseDetailFrame);
begin
  inherited;
  
  // Set the master frame type here.
end;  // TfraRelationship.SetFrameProperties 

{-------------------------------------------------------------------------------
}
function TfraRelationship.GetRelationshipType: TAppliesTo;
begin
  if Tabs[0].ContainedFrame is TfraRelationshipGeneral then
    Result := TfraRelationshipGeneral(Tabs[0].ContainedFrame).RelationshipAppliesTo
  else
    raise EBrowserFrameError.Create(ResStr_InvalidTabPage);
end;

end.

