{===============================================================================
  Unit:        FrameMovement.pas

  Defines:     TfraMovement

  Description: Container frame for movements

  Created:     May 2003

  Last revision information:
    $Revision: 9 $
    $Date: 9/02/04 17:46 $
    $Author: Bencollier $

===============================================================================}

unit FrameMovement;

interface

uses
  Windows, SysUtils, Classes, Graphics, Controls, Forms, ComCtrls,
  BasePageControlFrameUnit, BaseTabSheetFrameUnit, ResourceStrings,
  BaseDetailFrameUnit;

type
  {-----------------------------------------------------------------------------
    Contains details of a selected movement node (including accessions, loans and other 
    movement types).  Empty page control onto which the following tab pages are embedded as 
    required:
    TfraMovementGeneral
    TfraMovementCommunications
    TfraSources
  }
  TfraMovement = class (TBasePageControlFrame)
  protected
    procedure SetFrameProperties(AFrame: TBaseDetailFrame); override;
  public
    procedure AddTabs; override;
  end;
  
//==============================================================================
implementation

{$R *.dfm}

uses
  FrameMovementGeneral, FrameMovementCommunications, FrameSources,
  FrameMetadata, LuxembourgConstants;

{-==============================================================================
    TfraMovement
===============================================================================}
{-------------------------------------------------------------------------------
  Load all contained frames in separate tabsheets. 
}
procedure TfraMovement.AddTabs;
begin
  AddTab(ResStr_General, TfraMovementGeneral);
  AddTab(ResStr_Communications, TfraMovementCommunications);
  AddTab(ResStr_Sources, TfraSources);
end;  // TfraMovement.AddTabs 

{-------------------------------------------------------------------------------
}
procedure TfraMovement.SetFrameProperties(AFrame: TBaseDetailFrame);
begin
  if AFrame is TfraMetadata then
    TfraMetadata(AFrame).TableName := TN_MOVEMENT;
end;  // TfraMovement.SetFrameProperties 

end.




