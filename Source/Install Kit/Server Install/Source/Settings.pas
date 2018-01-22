{===============================================================================
  Unit:        Settings

  Defines:     TSettings

  Description: Holds entered data.

  Created:     September 2003

  Last revision information:
    $Revision: 4 $
    $Date: 14/05/07 16:42 $
    $Author: Johnvanbreda $

===============================================================================}

unit Settings;

interface

uses
  Classes, BarCode, SysUtils;

type
  TSettings = class (TObject)
  private
    FBarCodeFormat: Integer;
    FDatabaseName: String;
    FJobSeed: String;
    FLanguages: TStringList;
    FOrgAcronym: String;
    FOrgFullName: String;
    FPassword: String;
    FServerName: String;
    FUsername: String;
    FUseTrusted: Boolean;
    FInstallationPath: String;
    FNeedServerDetails: Boolean;
    FSiteID: string;
    procedure SetBarCodeFormat(const Value: Integer);
    procedure SetDatabaseName(const Value: String);
    procedure SetJobSeed(const Value: String);
    procedure SetOrgAcronym(const Value: String);
    procedure SetOrgFullName(const Value: String);
    procedure SetPassword(const Value: String);
    procedure SetServerName(const Value: String);
    procedure SetUsername(const Value: String);
    procedure SetUseTrusted(const Value: Boolean);
    procedure SetInstallationPath(const Value: String);
    procedure SetNeedServerDetails(const Value: Boolean);
    procedure SetSiteID(const Value: string);
  public
    constructor Create;
    destructor Destroy; override;
    procedure EstablishConnection(const AConnectionString: String);
    property BarCodeFormat: Integer read FBarCodeFormat write SetBarCodeFormat;
    property DatabaseName: String read FDatabaseName write SetDatabaseName;
    property InstallationPath: String read FInstallationPath write SetInstallationPath;
    property JobSeed: String read FJobSeed write SetJobSeed;
    property Languages: TStringList read FLanguages;
    property NeedServerDetails: Boolean read FNeedServerDetails write
        SetNeedServerDetails;
    property OrgAcronym: String read FOrgAcronym write SetOrgAcronym;
    property OrgFullName: String read FOrgFullName write SetOrgFullName;
    property SAPassword: String read FPassword write SetPassword;
    property SAUsername: String read FUsername write SetUsername;
    property ServerName: String read FServerName write SetServerName;
    property SiteID: string read FSiteID write SetSiteID;
    property UseTrusted: Boolean read FUseTrusted write SetUseTrusted;
  end;

//==============================================================================
implementation

uses
  GeneralData;

{-==============================================================================
    TSettings
===============================================================================}
{-------------------------------------------------------------------------------
}
constructor TSettings.Create;
begin
  FLanguages := TStringList.Create;
  // default username for 'sa'
  FUsername := 'sa';
  FJobSeed := '1';
  FNeedServerDetails := False;
end;  // TSettings.Create 

{-------------------------------------------------------------------------------
}
destructor TSettings.Destroy;
begin
  FLanguages.Free;
  if TdmGeneral.Allocated then TdmGeneral.Discard;
  inherited;
end;  // TSettings.Destroy

{-------------------------------------------------------------------------------
}
procedure TSettings.EstablishConnection(const AConnectionString: String);
begin
  TdmGeneral.CreateStandalone(nil, AConnectionString);
end;  // TSettings.EstablishConnection

{-------------------------------------------------------------------------------
}
procedure TSettings.SetBarCodeFormat(const Value: Integer);
begin
  FBarCodeFormat := Value;
end;  // TSettings.SetBarCodeFormat

{-------------------------------------------------------------------------------
}
procedure TSettings.SetDatabaseName(const Value: String);
begin
  FDatabaseName := Value;
end;  // TSettings.SetDatabaseName 

{-------------------------------------------------------------------------------
}
procedure TSettings.SetInstallationPath(const Value: String);
begin
  FInstallationPath := Value;
end;  // TSettings.SetInstallationPath

{-------------------------------------------------------------------------------
}
procedure TSettings.SetJobSeed(const Value: String);
begin
  FJobSeed := Value;
end;  // TSettings.SetJobSeed 

{-------------------------------------------------------------------------------
}
procedure TSettings.SetNeedServerDetails(const Value: Boolean);
begin
  FNeedServerDetails := Value;
end;

{-------------------------------------------------------------------------------
}
procedure TSettings.SetOrgAcronym(const Value: String);
begin
  FOrgAcronym := Value;
end;  // TSettings.SetOrgAcronym 

{-------------------------------------------------------------------------------
}
procedure TSettings.SetOrgFullName(const Value: String);
begin
  FOrgFullName := Value;
end;  // TSettings.SetOrgFullName 

{-------------------------------------------------------------------------------
}
procedure TSettings.SetPassword(const Value: String);
begin
  FPassword := Value;
end;  // TSettings.SetPassword 

{-------------------------------------------------------------------------------
}
procedure TSettings.SetServerName(const Value: String);
begin
  FServerName := Value;
end;  // TSettings.SetServerName 

{-------------------------------------------------------------------------------
}
procedure TSettings.SetSiteID(const Value: string);
begin
  FSiteID := Value;
end;

{-------------------------------------------------------------------------------
}
procedure TSettings.SetUsername(const Value: String);
begin
  FUsername := Value;
end;  // TSettings.SetUsername 

{-------------------------------------------------------------------------------
}
procedure TSettings.SetUseTrusted(const Value: Boolean);
begin
  FUseTrusted := Value;
end;  // TSettings.SetUseTrusted 

end.
