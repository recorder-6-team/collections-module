{===============================================================================
  Unit:           Settings

  Defines:        TSettings

  Description:    Class to hold settings that are persistent across the
                  application

  Created:        29/5/2003

  Last revision information:
    $Revision: 1 $
    $Date: 27/08/04 12:50 $
    $Author: Johnvanbreda $

===============================================================================}
unit Settings;

interface

uses
  SysUtils, Windows, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, ADODb, Registry, ExceptionForm, GeneralFunctions;

resourcestring
  ResStr_BadInstall = 'Recorder is not correclty installed on this machine.  Upgrade cannot proceed.';

type
  ESettingsError = class (TExceptionPath)
  end;
  
  TSettings = class (TObject)
  private
    FAddinPath: String;
    FBaseMapPath: string;
    FDatabaseName: string;
    FInstallationPath: string;
    FMapFilePath: string;
    FPassword: string;
    FServerName: string;
    FTrustedConnection: Boolean;
    FUserName: string;
    procedure ReadRegistry;
    procedure SetPassword(const Value: string);
    procedure SetTrustedConnection(Value: Boolean);
    procedure SetUserName(const Value: string);
  public
    constructor Create;
    function GetConnectionString: string;
    property AddinPath: String read FAddinPath;
    property BaseMapPath: string read FBaseMapPath;
    property InstallationPath: string read FInstallationPath;
    property MapFilePath: string read FMapFilePath;
    property Password: string read FPassword write SetPassword;
    property TrustedConnection: Boolean read FTrustedConnection write SetTrustedConnection;
    property UserName: string read FUserName write SetUserName;
  end;
  
//==============================================================================
implementation

const
  // Connection String for main SQL Server DB
  CONNECTION_STRING = 'Provider=SQLOLEDB.1;%sPersist Security Info=False;' +
                      'Data Source=%s;Initial Catalog=%s;OLE DB Services=-2';
  //OLE DB Services=-2 turns off connection pooling which can cause confusing errors
  //when sp_SetAppRole is used.
  INTEGRATED_SECURITY = 'Integrated Security=SSPI;';
  SQL_SECURITY = 'User ID=%s;password=%s;';

{-==============================================================================
    TSettings
===============================================================================}
{-------------------------------------------------------------------------------
}
constructor TSettings.Create;
begin
  inherited Create;
  
  ReadRegistry;
end;  // TSettings.Create 

{-------------------------------------------------------------------------------
  Returns a connection string for access to the NBNData volume.
  Depends on the ATrustedConnection parameter (if true then integrated security used). 
}
function TSettings.GetConnectionString: string;
var
  lstSecurityInfo: string;
begin
  if FTrustedConnection then
    lstSecurityInfo := INTEGRATED_SECURITY
  else
    lstSecurityInfo := Format(SQL_SECURITY, [FUserName, FPassword]);
  Result := Format(CONNECTION_STRING, [lstSecurityInfo, FServerName, FDatabaseName]);
end;  // TSettings.GetConnectionString 

{-------------------------------------------------------------------------------
  Reads the database name and server name from the registry. 
}
procedure TSettings.ReadRegistry;
var
  lReg: TRegistry;
const
  REG_PATH = 'SOFTWARE\Dorset Software\Recorder 6';
begin
  lReg := TRegistry.Create;
  try
    // Read off Lcoal Machine key.
    lReg.Rootkey := HKEY_LOCAL_MACHINE;
    if not lReg.OpenKeyReadOnly(REG_PATH) then begin
      ShowInformation(ResStr_BadInstall);
      Application.Terminate;
    end;
    FAddinPath := lReg.ReadString('Addin Path');
    FServerName := lReg.ReadString('Server Name');
    FDatabaseName := lReg.ReadString('Database Name');
    FInstallationPath := lReg.ReadString('Installation Path');

    // Read off Current User key.
    lReg.RootKey := HKEY_CURRENT_USER;
    if not lReg.OpenKeyReadOnly(REG_PATH + '\Settings') then begin
      ShowInformation(ResStr_BadInstall);
      Application.Terminate;
    end;
    FBaseMapPath := lReg.ReadString('Base Map Path');
    FMapFilePath := lReg.ReadString('Map File Path');
  finally
    lReg.Free;
  end;
end;  // TSettings.ReadRegistry

{-------------------------------------------------------------------------------
}
procedure TSettings.SetPassword(const Value: string);
begin
  FPassword := Value;
end;  // TSettings.SetPassword

{-------------------------------------------------------------------------------
}
procedure TSettings.SetTrustedConnection(Value: Boolean);
begin
  FTrustedConnection := Value;
end;  // TSettings.SetTrustedConnection 

{-------------------------------------------------------------------------------
}
procedure TSettings.SetUserName(const Value: string);
begin
  FUserName := Value;
end;  // TSettings.SetUserName 

end.
