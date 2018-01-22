{===============================================================================
  Unit:        FrameMaterialHostedMaterial.pas

  Defines:     TfraMaterialHostedMaterial

  Description:

  Created:     October 2003

  Last revision information:
    $Revision: 6 $
    $Date: 3/08/04 14:59 $
    $Author: Ericsalmon $

===============================================================================}

unit FrameMaterialHostedMaterial;

interface

uses
  SysUtils, Windows, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, BaseMaterialMovementDetailsFrameUnit, ResourceStrings,
  Variants, GeneralData, LuxembourgConstants;

type
  TfraMaterialHostedMaterial = class (TBaseMaterialMovementFrame)
  protected
    procedure DeleteData; override;
  public
    procedure DoRegisterControls; override;
    function GetCaption: String; override;
    procedure SaveData; override;
  end;

//==============================================================================
implementation

{-==============================================================================
    TfraMaterialHostedMaterial
===============================================================================}
{-------------------------------------------------------------------------------
  Delete the record used to load the frame. 
}
procedure TfraMaterialHostedMaterial.DeleteData;
begin
  dmGeneral.RunDeleteStoredProc('usp_MovementMaterialDetail_Delete',
                               ['@Key', Key, '@Timestamp', FTimestamp]);
end;  // TfraMaterialHostedMaterial.DeleteData 

{-------------------------------------------------------------------------------
  Register the correct recordset. 
}
procedure TfraMaterialHostedMaterial.DoRegisterControls;
begin
  // Register recordsets used
  RegisterRecordset('usp_MovementMaterialDetail_Select');
end;  // TfraMaterialHostedMaterial.DoRegisterControls 

{-------------------------------------------------------------------------------
  Get the caption. 
}
function TfraMaterialHostedMaterial.GetCaption: String;
begin
  Result := ResStr_HostedMaterial + ' - ' + eDate.text;
end;  // TfraMaterialHostedMaterial.GetCaption 

{-------------------------------------------------------------------------------
  Save the frame's data to database. 
}
procedure TfraMaterialHostedMaterial.SaveData;
var
  lParams: Array Of Variant;
begin
  inherited;   // For validation to be called.
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
                          '@Outbound', FOutbound,
                          '@Notes', mmNotes.Text,
                          '@Timestamp', FTimestamp
                          ]);
   // Empty key means new record.
  if Key = '' then
     Key := VarToStr(dmGeneral.RunInsertStoredProc(TN_MOVEMENT_OF_MATERIAL,
                                                   'usp_MovementMaterialDetail_Insert',
                                                   lParams,
                                                   '@Key'))
  else
    dmGeneral.RunUpdateStoredProc('usp_MovementMaterialDetail_Update',
        lParams);
end;  // TfraMaterialHostedMaterial.SaveData 


end.
