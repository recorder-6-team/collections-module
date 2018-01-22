{===============================================================================
  Unit:        FrameMovementDetailsCollectionUnits.pas

  Defines:     TfraMovementDetailsCollectionUnits

  Description:

  Model:       CollectionsMovements

  Created:     July 2003

  Last revision information:
    $Revision: 4 $
    $Date: 3/02/04 9:47 $
    $Author: Anthonysimpson $

===============================================================================}
unit FrameMovementDetailsCollectionUnits;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, BaseTabSheetFrameUnit, ExtCtrls, StdCtrls, CheckLst, exgrid,
  RapTree, DataTypes, DataClasses, GeneralData, LuxembourgConstants,
  BaseCollectionUnitsIncluded;

type
  {-----------------------------------------------------------------------------
    Tab page displaying the total list of collections, stores and specimens that are involved 
    in a top-level movement.  The tab page is associated with the details for a material 
    movement within the top-level movement, and items that are not specifically involved in 
    the material movement are displayed in an unchecked state.  This allows the user to 
    control which items are included in a material movement, for example when there are 
    several acquisitions in a movement each involving a different subset of the items involved 
    in the movement.
    This form inherits from TBaseIncludedCollectionUnits.
  }
  TfraMovementDetailsCollectionUnits = class (TBaseIncludedCollectionUnits)
  private
    function GetParams: TVariantArray;
  protected
    procedure RegisterControls; override;
    procedure SaveCheckBoxData(ACheckListBox: TCheckListBox); override;
  end;
  
implementation

{$R *.dfm}


{ TfraMovementDetailsCollectionUnits }

{-==============================================================================
    TfraMovementDetailsCollectionUnits
===============================================================================}
{-------------------------------------------------------------------------------
  Get the parameters for the recordset. 
}
function TfraMovementDetailsCollectionUnits.GetParams: TVariantArray;
begin
  Result := VarArrayOf(['@Key', Key, '@MovementKey', ParentKey]);
end;  // TfraMovementDetailsCollectionUnits.GetParams 

{-------------------------------------------------------------------------------
  Register the recordset. 
}
procedure TfraMovementDetailsCollectionUnits.RegisterControls;
begin
  inherited;
  // Register recordsets used
  RegisterRecordset('usp_CollectionUnits_Select_ForMovement', GetParams);
end;  // TfraMovementDetailsCollectionUnits.RegisterControls 

{-------------------------------------------------------------------------------
  Save the contents of the listcheck boxes. The insert or delete procs are only called if a 
      change has been made. 
}
procedure TfraMovementDetailsCollectionUnits.SaveCheckBoxData(ACheckListBox: TCheckListBox);
var
  x: Integer;
  lParams: Array Of Variant;
begin
  lParams := nil; // To stop a warning being shown when compiling.
  with ACheckListBox.Items do begin
    for x := 0 to Count - 1 do begin
      // Check if a change has been made.
      if TKeyAndModified(Objects[x]).Checked <> ACheckListBox.Checked[x] then begin
        lParams := VarArrayOf(['@MovementOfMaterialKey', Key,
                     '@CollectionUnitKey', TKeyAndModified(Objects[x]).Key]);
        if ACheckListBox.Checked[x] then
          dmGeneral.RunDeleteStoredProc(
                                'usp_MovementOfMaterialExclusion_Delete',
                                lParams)
        else
          dmGeneral.RunInsertStoredProc(TN_MOVEMENT_OF_MATERIAL_EXCLUSION,
                                'usp_MovementOfMaterialExclusion_Insert',
                                lParams,
                                '@Key');
      end;  // if
    end;  // for
  end;  // with
end;  // TfraMovementDetailsCollectionUnits.SaveCheckBoxData 


end.





