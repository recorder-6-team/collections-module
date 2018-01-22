{===============================================================================
  Unit:        ProjectSpecificAccess

  Defines:     <nothing>

  Description: Bridge between application and Addinxxx units.

  Model:       <none>

  Last revision information:
    $Revision: 2 $
    $Date: 7/11/07 12:31 $
    $Author: Johnvanbreda $

===============================================================================}

unit ProjectSpecificAccess;

interface

uses
  Sysutils, AddinSearchManager, AddinInterfaceDataModule, AddinGeneralData,
  BaseADODataModule;

function dmGeneral: TdmBaseADO;

var
  dmInterface: TdmAddinInterface;

//==============================================================================
implementation

{-------------------------------------------------------------------------------
}
function dmGeneral: TdmBaseADO;
begin
  Result := TdmAddinGeneral.Create(nil);
end;

initialization
  dmInterface := TdmAddinInterface.Create(nil);

finalization
  FreeAndNil(dmInterface);

end.