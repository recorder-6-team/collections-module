{===============================================================================
  Unit:        FrameMovementDetails.pas

  Defines:     TfraMovementDetails

  Description:

  Created:     July 2003

  Last revision information:
    $Revision: 6 $
    $Date: 31/03/04 12:23 $
    $Author: Anthonysimpson $

===============================================================================}

unit FrameMovementDetails;

interface

uses
  Windows, SysUtils, Classes, Graphics, Controls, Forms, ComCtrls,
  BasePageControlFrameUnit, BaseTabSheetFrameUnit, ResourceStrings,
  LuxembourgConstants;

type
  {-----------------------------------------------------------------------------
    Generic screen for material movement details.  This screen consists of an empty page 
    control onto which the following tabs are loaded as required:
    TfraMaterialMovementDetails (except for Lost, Disposed and Destroyed movement types)
    TfraMaterialToUnknownDest (all other movement types)
    TfraMovementDetailsCollectionUnits (this tab page is only loaded if there are 2 
    Movement_Of_Material records for the same Movement_Direction_Key.  Otherwise the movement 
    of material is assumed to apply to all the material associated with the movement 
    direction. This BasePageControlFrame is only used when two tab pages are required. In 
    cases where just one tabpage is required, that tabpage will be directly associated with 
    the node. This stops a single page appearing as a tabpage - i.e. as a whole frame.
  }
  TfraMovementDetails = class (TBasePageControlFrame)
  protected
    procedure LoadData; override;
  public
    procedure AddTabs; override;
  end;
  
//==============================================================================
implementation

{$R *.dfm}
uses
  FrameMaterialMovement, FrameMaterialToUnknownDestination,
  FrameMovementDetailsCollectionUnits, BaseDetailFrameUnit, GeneralData;
  //PrototypeMain;

{-==============================================================================
    TfraMovementDetails
===============================================================================}
{-------------------------------------------------------------------------------
}
procedure TfraMovementDetails.AddTabs;
begin
  AddTab('General', TfraMaterialMovementDetails);
  AddTab('General', TfraMaterialToUnknownDest);
  AddTab('Materials Involved', TfraMovementDetailsCollectionUnits);
end;  // TfraMovementDetails.AddTabs 

{-------------------------------------------------------------------------------
  The caption of the second frame needs to change depending on whether the leaf node was an 
      Acquisition Details or Disposal Details node. 
}
procedure TfraMovementDetails.LoadData;
var
  lMovementOutbound: Boolean;
  lMovementType: Integer;
begin
  // Depending on the movement type, either the TfraMaterialMovementDetails or
  // TfraMaterialToUnknownDest tab pages should be showed.
  lMovementType := AdditionalProperties.GetProperty(PROP_MOVEMENT_TYPE);
  pcDetails.Pages[0].TabVisible := not (lMovementType in [4, 7]);
  pcDetails.Pages[1].TabVisible := not pcDetails.Pages[0].TabVisible;
  inherited;

  // If there are more than two acquisition/disposal details nodes present,
  // then the TfraMovementDetailsCollectionUnits tabpage should be loaded.
  pcDetails.Pages[2].TabVisible :=
        (dmGeneral.GetStoredProcOutputParam('usp_MovementMaterialCount_Get',
                                            ['@Key', Key],
                                            '@Count'))
                          > 1;

  // If the TfraMovementDetailsCollectionUnits tabpage is loaded, then the
  // correct caption for the tab page should be shown.
  if pcDetails.Pages[2].TabVisible then begin
    lMovementOutbound := StrToBool(AdditionalProperties.GetProperty
                                                      (PROP_MOVEMENT_OUTBOUND));
    if lMovementOutbound then pcDetails.Pages[2].Caption := ResStr_DisposedMaterials
                         else pcDetails.Pages[2].Caption := ResStr_AcquiredMaterials;
  end;
end;  // TfraMovementDetails.LoadData

end.

