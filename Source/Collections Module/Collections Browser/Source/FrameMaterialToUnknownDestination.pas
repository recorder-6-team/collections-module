{===============================================================================
  Unit:        FrameMaterialToUnknownDestination.pas

  Defines:     TfraMaterialToUnknownDestination

  Description:

  Model:       CollectionsMovements

  Created:     October 2003

  Last revision information:
    $Revision: 11 $
    $Date: 4/02/04 11:34 $
    $Author: Anthonysimpson $

===============================================================================}

unit FrameMaterialToUnknownDestination;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, BaseMaterialMovementDetailsFrameUnit, StdCtrls, ImageListButton,
  ExtCtrls, VagueDateEdit, BaseCompositeComponent, LinkedControls,
  GeneralData, LuxembourgConstants, ResourceStrings;

ResourceString
  FstMaterial = 'Material has been ';
  FstLost = 'lost';
  FstDisposed = 'disposed';
  FstDestroyed = 'destroyed';

type
  {-----------------------------------------------------------------------------
    This tab page is displayed instead of TfraMaterialMovementDetails if the destination of 
    the material is unknown (e.g. when material is lost).
    This tab control is inherited from TBaseMaterialMovementScreen and includes the components 
    documented on that base class.  The documentation here details the additional 
    functionality in this tab page only.
  }
  TfraMaterialToUnknownDest = class (TBaseMaterialMovementFrame)
    lblCaption: TLabel;
  protected
    procedure DeleteData; override;
    procedure RegisterControls; override;
  public
    procedure DoRegisterControls; override;
    function GetCaption: String; override;
    procedure SaveData; override;
  end;
  
//==============================================================================
implementation

{$R *.dfm}

{-==============================================================================
    TfraMaterialToUnknownDest
===============================================================================}
{-------------------------------------------------------------------------------
  Delete the record used to load the frame. 
}
procedure TfraMaterialToUnknownDest.DeleteData;
begin
  dmGeneral.RunDeleteStoredProc('usp_MovementMaterialDetail_Delete',
                               ['@Key', Key, '@Timestamp', FTimestamp]);
end;  // TfraMaterialToUnknownDest.DeleteData 

{-------------------------------------------------------------------------------
  Register the recordset. 
}
procedure TfraMaterialToUnknownDest.DoRegisterControls;
begin
  // Register recordsets used
  RegisterRecordset('usp_MovementMaterialDetail_Select');
end;  // TfraMaterialToUnknownDest.DoRegisterControls 

{-------------------------------------------------------------------------------
  Get the caption. 
}
function TfraMaterialToUnknownDest.GetCaption: String;
begin
  Result := ResStr_DisposalDetails + ' - ' + eDate.text;
end;  // TfraMaterialToUnknownDest.GetCaption 

{-------------------------------------------------------------------------------
  Register the label as an async control. 
}
procedure TfraMaterialToUnknownDest.RegisterControls;
begin
  inherited RegisterControls;
  
  RegisterAsyncControl(lblCaption, 'usp_MovementOfMaterial_Get',
                       ['@Key', 'Movement_Of_Material_Key'], '@Caption');
end;  // TfraMaterialToUnknownDest.RegisterControls 

{-------------------------------------------------------------------------------
  Save the frame's data to the database. 
}
procedure TfraMaterialToUnknownDest.SaveData;
var
  lParams: Array Of Variant;
begin
  inherited;     // For validation to be called.
  lParams := VarArrayOf(['@Key', Key,
                         '@ParentKey', ParentKey,
                         '@ContactNameKey', eContact.Key,
                         '@VagueDateStart', eDate.VagueDate.StartDate,
                         '@VagueDateEnd', eDate.VagueDate.EndDate,
                         '@VagueDateType', eDate.VagueDate.DateTypeString,
                         '@Completed',chkCompleted.checked,
                         '@ReceiverNameKey', '',
                         '@ValueAmount', '',
                         '@CurrencyConceptKey', '',
                         '@AcquisitionMethodConceptKey', '',
                         '@Notes', mmNotes.Text,
                         '@Outbound', FOutbound,
                         '@Timestamp', FTimestamp
                         ]);
   // Empty key means new record.
  if Key = '' then
    Key := VarToStr(dmGeneral.RunInsertStoredProc(TN_MOVEMENT_OF_MATERIAL,
                                                  'usp_MovementMaterialDetail_Insert',
                                                  lParams, '@Key'))
  else
    dmGeneral.RunUpdateStoredProc('usp_MovementMaterialDetail_Update', lParams);
end;  // TfraMaterialToUnknownDest.SaveData 

end.

