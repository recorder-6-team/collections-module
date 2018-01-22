{===============================================================================
  Unit:        ApplicationSettings

  Defines:     AppSettings global function

  Description: Function providing access to the Collections Module Settings
               COM object in a simplified way.

  Model:       None

  Created:     Nov 2003

  Last revision information:
    $Revision: 22 $
    $Date: 18/05/06 10:56 $
    $Author: Johnvanbreda $

===============================================================================}

unit ApplicationSettings;

interface

uses
  SysUtils, Windows, ExceptionForm, DataTypes, CollectionsModuleManager_TLB,
  ComObj;

resourcestring
  ResStr_ServerNotAvailable = 'Cannot connect to the database server.  Please ensure '+
      'that the server is started.';

type
  EApplicationSettings = class(TExceptionPath);

// Global accessor function
function AppSettings: ICollectionsModuleSettings;
procedure DiscardAppSettings;

//==============================================================================
implementation

uses
  VagueDate, GeneralData, BaseADODataModule;

var
  // module level instance variable, accessed via AppSettings function
  mAppSettings: ICollectionsModuleSettings;

{-------------------------------------------------------------------------------
}
function VagueDateSeasonNames: String;
begin
  Result := '';
  with dmGeneral.GetRecordset('usp_SeasonNames_Select', ['@Language', AppSettings.ISOLanguage]) do
  begin
    while not Eof do begin
      Result := Result + ',' + Fields['PlainText'].Value;
      MoveNext;
    end;
    Close;
  end;
  if Result <> '' then Result := Copy(Result, 2, 255);
end;

{-------------------------------------------------------------------------------
}
function AppSettings: ICollectionsModuleSettings;
begin
  try
    if not Assigned(mAppSettings) then
      mAppSettings := CreateComObject(CLASS_CollectionsModuleSettings) as
                      ICollectionsModuleSettings;
  except on E:EOleSysError do
    raise EApplicationSettings.CreateNonCritical(ResStr_ServerNotAvailable);
  end;
  Result := mAppSettings;
  VagueDate.GetApplicationSuppliedSeasonNames := VagueDateSeasonNames;
end;

{-------------------------------------------------------------------------------
}
procedure DiscardAppSettings;
begin
  mAppSettings := nil;
end;

initialization
  mAppSettings := nil;

end.
