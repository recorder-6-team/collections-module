{===============================================================================
  Unit:        FrameTaskCollectionUnits.pas

  Defines:     TfraTaskCollectionUnits

  Description:

  Model:       CollectionsJobsAndTasks

  Created:     July 2003

  Last revision information:
    $Revision: 3 $
    $Date: 12/03/04 11:09 $
    $Author: Anthonysimpson $

===============================================================================}
unit FrameTaskCollectionUnits;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, BaseTabSheetFrameUnit, ExtCtrls, StdCtrls, CheckLst, exgrid,
  RapTree, DataTypes, DataClasses, GeneralData, LuxembourgConstants,
  BaseCollectionUnitsIncluded;

type

  {-----------------------------------------------------------------------------
    Tab page control showing the list of all collection units included in the condition 
    check the task is linked to.  The user is able to check or uncheck collection units 
    to indicate their inclusion in the task.  For example, a condition check for the 
    contents of a room results in a task for a specific item in the room. 
    This form inherits from TBaseIncludedCollectionUnits.
  }
  TfraTaskCollectionUnits = class (TBaseIncludedCollectionUnits)
  private
    function GetParams: TVariantArray;
  protected
    procedure RegisterControls; override;
    procedure SaveCheckBoxData(ACheckListBox: TCheckListBox); override;
  end;
  
implementation

{$R *.DFM}

{-==============================================================================
    TfraTaskCollectionUnits
===============================================================================}
{-------------------------------------------------------------------------------
  Get the parameters for the recordset. 
}
function TfraTaskCollectionUnits.GetParams: TVariantArray;
begin
  Result := VarArrayOf(['@TaskKey', Key]);
end;  // TfraTaskCollectionUnits.GetParams 

{-------------------------------------------------------------------------------
  Register the correct recordset. 
}
procedure TfraTaskCollectionUnits.RegisterControls;
begin
  inherited;
  // Register recordsets used
  RegisterRecordset('usp_CollectionUnits_Select_ForTask', GetParams);
end;  // TfraTaskCollectionUnits.RegisterControls 

{-------------------------------------------------------------------------------
  Save the contents of the check boxes to the database. 
}
procedure TfraTaskCollectionUnits.SaveCheckBoxData(ACheckListBox: TCheckListBox);
var
  x: Integer;
  lParams: Array Of Variant;
begin
  lParams := nil; // To stop a warning being shown when compiling.
  with ACheckListBox.Items do begin
    for x := 0 to Count - 1 do begin
      // Check if a change has been made.
      if TKeyAndModified(Objects[x]).Checked <> ACheckListBox.Checked[x] then begin
        lParams := VarArrayOf(['@ConservationTaskKey', Key,
                     '@CollectionUnitKey', TKeyAndModified(Objects[x]).Key]);
        if ACheckListBox.Checked[x] then
          dmGeneral.RunInsertStoredProc(TN_COLLECTION_UNIT_TASK,
                                'usp_CollectionUnitTask_Insert',
                                lParams,
                                '@Key')
        else
          dmGeneral.RunDeleteStoredProc('usp_CollectionUnitTask_Delete',
                                lParams);
      end;  // if
    end;  // for
  end;  // with
end;  // TfraTaskCollectionUnits.SaveCheckBoxData 

end.
