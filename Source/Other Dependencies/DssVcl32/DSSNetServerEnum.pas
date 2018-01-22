unit DSSNetServerEnum;

interface

uses
  SysUtils, Windows, Classes, LmCons, LmErr, LmApiBuf, LmServer;

type
  // Possible values for PlatformId
  TPlatformId = (pidDOS, pidOS2, pidNT, pidOSF, pidVMS);

  // Set of possible values for Server Type in call to NetServerEnum
  TServerType = (stWorkstation, stServer, stSQLServer, stDomainCtrl, stDomainBackCtrl, stTimeSource,
                 stAFP, stNovell, stDomainMember, stPrintQServer, stDialInServer, stXenixServer,
                 stNT, stWFW, stServerMFPN, stServerNT, stPotentialBrowser, stBackupBrowser,
                 stMasterBrowser, stDomainMaster, stServerOSF, stServerVMS, stWindows, 
                 stClusterNT, stTerminalServer, stLocalListOnly,
                 stDomainEnum, stAll);
  TServerTypes = set of TServerType;

  // Type of information to return
  TLevel = (lvServerInfo100, lvServerInfo101);

  // Class to store information returned. Contains info about 1 server
  TServerInfo = class
  private
    FServerInfoType: TLevel;
    FPlatformId    : TPlatformId;
    FServerName    : String;
    FVersionMajor  : LongInt;
    FVersionMinor  : LongInt;
    FServerType    : TServerTypes;
    FComment       : String;
    function GetPlatformId: TPlatformId;
    function GetName: String;
    function GetVersionMajor: LongInt;
    function GetVersionMinor: LongInt;
    function GetServerType: TServerTypes;
    function GetComment: String;

    constructor Create(const pLevel: TLevel; const pPlatformId: DWord; const pName: String); overload;
    constructor Create(const pLevel: TLevel; const pPlatformId: DWord; const pName: String;
      const pVersionMajor: DWord; const pVersionMinor: DWord; const pServerType: DWord;
      const pComment: String); overload;
    function BuildServerTypesSet(const pServerType: DWord): TServerTypes;
  public
    property PlatformId: TPlatformId read GetPlatformId;    // SERVER_INFO_100 & SERVER_INFO_101
    property Name: String read GetName;                     // SERVER_INFO_100 & SERVER_INFO_101
    property VersionMajor: LongInt read GetVersionMajor;    // SERVER_INFO_101 only
    property VersionMinor: LongInt read GetVersionMinor;    // SERVER_INFO_101 only
    property ServerType: TServerTypes read GetServerType;   // SERVER_INFO_101 only
    property Comment: String read GetComment;               // SERVER_INFO_101 only
  end;

  // Class to get the list of servers on the network
  TDSSNetServerEnum = class(TComponent)
  private
    FDomain     : String;
    FLevel      : TLevel;
    FServerTypes: TServerTypes;
    FServers    : TList;
    function GetDomain: String;
    procedure SetDomain(const Value: String);
    function GetLevel: TLevel;
    procedure SetLevel(const Value: TLevel);
    function GetServerTypes: TServerTypes;
    procedure SetServerTypes(const Value: TServerTypes);
    function BuildMask(const pTypes: TServerTypes): DWord;
    function GetServerInfo(Index: integer): TServerInfo;
    procedure ClearServerList;
    function GetCount: Integer;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure EnumerateServers;
    property Count: Integer read GetCount;
    property Items[Index: integer]: TServerInfo read GetServerInfo;
  published
    property Domain: String read GetDomain write SetDomain;
    property Level: TLevel read GetLevel write SetLevel;
    property ServerTypes: TServerTypes read GetServerTypes write SetServerTypes;
  end;

procedure Register;

//==================================================================================================
implementation
{$IFDEF DELPHI7UP}
  {$R *.dcr}
{$ENDIF}

//{ TDSSNetServerEnum }

const
  EST_INDEX_OUT_OF_BOUNDS = 'List index out of bounds (%d)';

//==================================================================================================
procedure Register;
begin
  RegisterComponents ('In House API',[TDSSNetServerEnum]);
end;

//==================================================================================================
//==================================================================================================
constructor TDSSNetServerEnum.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDomain := '';
  FLevel := lvServerInfo100;
  FServerTypes := [stAll];
  FServers := TList.Create;
end;

//==================================================================================================
destructor TDSSNetServerEnum.Destroy;
begin
  ClearServerList;
  FServers.Free;
  inherited;
end;

//==================================================================================================
procedure TDSSNetServerEnum.ClearServerList;
var liIdx: Integer;
begin
  for liIdx := 0 to FServers.Count -1 do
    TServerInfo(FServers.Items[liIdx]).Free;
  FServers.Clear;
end;

//==================================================================================================
procedure TDSSNetServerEnum.EnumerateServers;
var lRetCode      : NET_API_STATUS;
    lpBuffer      : Pointer;
    lpEntry100    : PServerInfo100;
    lpEntry101    : PServerInfo101;
    liEntriesRead : DWord;
    liTotalEntries: DWord;
    liIdx         : integer;
    liMask        : DWord;
    liLevel       : DWord;
    lszDomain     : PWideChar;
    loServerInfo  : TServerInfo;
begin
  // Clear before populating list again
  ClearServerList;
  // Build ServerType mask
  liMask := BuildMask(ServerTypes);
  // Work out Level
  liLevel := 100;  // Default for lvServerInfo100;
  if Level = lvServerInfo101 then liLevel := 101;
  // Prepare Domain (or not)
  lszDomain := nil;
  if Domain <> '' then begin
    GetMem(lszDomain, Length(Domain) + 1);
    StringToWideChar(Domain, lszDomain, Length(Domain) + 1);
  end;

  try
    // Call API function
    lRetCode := NetServerEnum(nil, liLevel, lpBuffer, MAX_PREFERRED_LENGTH, liEntriesRead, liTotalEntries,
                              liMask, lszDomain, nil);
    if lRetCode = NERR_Success then begin
      // Set list capacity
      FServers.Capacity := liEntriesRead;

      case Level of
        lvServerInfo100:
          begin
            lpEntry100 := lpBuffer;
            for liIdx := 0 to liEntriesRead - 1 do begin
              // Create ad populate ServerInfo object
              with lpEntry100^ do
                loServerInfo := TServerInfo.Create(Level, sv100_platform_id,
                                                          WideCharToString(sv100_name));
              // Add to list
              FServers.Add(loServerInfo);
              // Move on to next entry
              Inc(lpEntry100);
            end;
          end;
        lvServerInfo101:
          begin
            lpEntry101 := lpBuffer;
            for liIdx := 0 to liEntriesRead - 1 do begin
              // Create ad populate ServerInfo object
              with lpEntry101^ do
                loServerInfo := TServerInfo.Create(Level, sv101_platform_id,
                                                          WideCharToString(sv101_name),
                                                          sv101_version_major,
                                                          sv101_version_minor,
                                                          sv101_type,
                                                          WideCharToString(sv101_comment));
              // Add to list
              FServers.Add(loServerInfo);
              // Move on to next entry
              Inc(lpEntry101);
            end;
          end;
      end;  // end case
    end;
    // Call this one, even if nothing returned
    NetApiBufferFree(lpBuffer);
  finally
    if Domain<>'' then FreeMem(lszDomain);
  end;
end;

//==================================================================================================
function TDSSNetServerEnum.BuildMask(const pTypes: TServerTypes): DWord;
  procedure CheckServerType(const AServerType: TServerType; const AServerTypeConstant: DWord);
  begin
    if AServerType in pTypes then Result := Result or AServerTypeConstant;
  end;
begin
  Result := 0;
  CheckServerType(stWorkstation,      SV_TYPE_WORKSTATION);
  CheckServerType(stServer,           SV_TYPE_SERVER);
  CheckServerType(stSQLServer,        SV_TYPE_SQLSERVER);
  CheckServerType(stDomainCtrl,       SV_TYPE_DOMAIN_CTRL);
  CheckServerType(stDomainBackCtrl,   SV_TYPE_DOMAIN_BAKCTRL);
  CheckServerType(stTimeSource,       SV_TYPE_TIME_SOURCE);
  CheckServerType(stAFP,              SV_TYPE_AFP);
  CheckServerType(stNovell,           SV_TYPE_NOVELL);
  CheckServerType(stDomainMember,     SV_TYPE_DOMAIN_MEMBER);
  CheckServerType(stPrintQServer,     SV_TYPE_PRINTQ_SERVER);
  CheckServerType(stDialInServer,     SV_TYPE_DIALIN_SERVER);
  CheckServerType(stXenixServer,      SV_TYPE_XENIX_SERVER);
  CheckServerType(stNT,               SV_TYPE_NT);
  CheckServerType(stWFW,              SV_TYPE_WFW);
  CheckServerType(stServerMFPN,       SV_TYPE_SERVER_MFPN);
  CheckServerType(stServerNT,         SV_TYPE_SERVER_NT);
  CheckServerType(stPotentialBrowser, SV_TYPE_POTENTIAL_BROWSER);
  CheckServerType(stBackupBrowser,    SV_TYPE_BACKUP_BROWSER);
  CheckServerType(stMasterBrowser,    SV_TYPE_MASTER_BROWSER);
  CheckServerType(stDomainMaster,     SV_TYPE_DOMAIN_MASTER);
  CheckServerType(stServerOSF,        SV_TYPE_SERVER_OSF);
  CheckServerType(stServerVMS,        SV_TYPE_SERVER_VMS);
  CheckServerType(stWindows,          SV_TYPE_WINDOWS);
  CheckServerType(stClusterNT,        SV_TYPE_CLUSTER_NT);
  CheckServerType(stTerminalServer,   SV_TYPE_TERMINALSERVER);
  CheckServerType(stLocalListOnly,    SV_TYPE_LOCAL_LIST_ONLY);
  CheckServerType(stDomainEnum,       SV_TYPE_DOMAIN_ENUM);
  CheckServerType(stAll,              SV_TYPE_ALL);
end;

//==================================================================================================
function TDSSNetServerEnum.GetDomain: String;
begin
  Result := FDomain;
end;

//==================================================================================================
function TDSSNetServerEnum.GetLevel: TLevel;
begin
  Result := FLevel;
end;

//==================================================================================================
function TDSSNetServerEnum.GetServerTypes: TServerTypes;
begin
  Result := FServerTypes;
end;

//==================================================================================================
procedure TDSSNetServerEnum.SetDomain(const Value: String);
begin
  FDomain := Value;
end;

//==================================================================================================
procedure TDSSNetServerEnum.SetLevel(const Value: TLevel);
begin
  FLevel := Value;
end;

//==================================================================================================
procedure TDSSNetServerEnum.SetServerTypes(const Value: TServerTypes);
begin
  FServerTypes := Value;
end;

//==================================================================================================
function TDSSNetServerEnum.GetServerInfo(Index: integer): TServerInfo;
begin
  if not (Index in [0..FServers.Count - 1]) then
    raise EListError.CreateFmt(EST_INDEX_OUT_OF_BOUNDS,[Index]);
  Result := TServerInfo(FServers.Items[Index]);
end;

//==================================================================================================
function TDSSNetServerEnum.GetCount: Integer;
begin
  Result := FServers.Count;
end;

//==================================================================================================
//==================================================================================================
{ TServerInfo }
//==================================================================================================
//==================================================================================================
constructor TServerInfo.Create(const pLevel: TLevel; const pPlatformId: DWord;
  const pName: String);
begin
  FServerInfoType := pLevel;
  case pPlatformId of
    PLATFORM_ID_DOS: FPlatformID := pidDOS;
    PLATFORM_ID_OS2: FPlatformID := pidOS2;
    PLATFORM_ID_NT : FPlatformID := pidNT;
    PLATFORM_ID_OSF: FPlatformID := pidOSF;
    PLATFORM_ID_VMS: FPlatformID := pidVMS;
  end;
  FServerName     := pName;
  FVersionMajor   := -1;
  FVersionMinor   := -1;
  FServerType     := [];
  FComment        := '';
end;

//==================================================================================================
constructor TServerInfo.Create(const pLevel: TLevel; const pPlatformId: DWord;
  const pName: String; const pVersionMajor: DWord; const pVersionMinor: DWord;
  const pServerType: DWord; const pComment: String);
begin
  FServerInfoType := pLevel;
  case pPlatformId of
    PLATFORM_ID_DOS: FPlatformID := pidDOS;
    PLATFORM_ID_OS2: FPlatformID := pidOS2;
    PLATFORM_ID_NT : FPlatformID := pidNT;
    PLATFORM_ID_OSF: FPlatformID := pidOSF;
    PLATFORM_ID_VMS: FPlatformID := pidVMS;
  end;
  FServerName     := pName;
  FVersionMajor   := pVersionMajor;
  FVersionMinor   := pVersionMinor;
  FServerType     := BuildServerTypesSet(pServerType);
  FComment        := pComment;
end;

//==================================================================================================
function TServerInfo.BuildServerTypesSet(const pServerType: DWord): TServerTypes;
  procedure CheckServerType(const AServerTypeConstant: DWord; const AServerType: TServerType);
  begin
    if pServerType and AServerTypeConstant = AServerTypeConstant then Include(Result, AServerType);
  end;
begin
  Result := [];
  CheckServerType(SV_TYPE_WORKSTATION,       stWorkstation);
  CheckServerType(SV_TYPE_SERVER,            stServer);
  CheckServerType(SV_TYPE_SQLSERVER,         stSQLServer);
  CheckServerType(SV_TYPE_DOMAIN_CTRL,       stDomainCtrl);
  CheckServerType(SV_TYPE_DOMAIN_BAKCTRL,    stDomainBackCtrl);
  CheckServerType(SV_TYPE_TIME_SOURCE,       stTimeSource);
  CheckServerType(SV_TYPE_AFP,               stAFP);
  CheckServerType(SV_TYPE_NOVELL,            stNovell);
  CheckServerType(SV_TYPE_DOMAIN_MEMBER,     stDomainMember);
  CheckServerType(SV_TYPE_PRINTQ_SERVER,     stPrintQServer);
  CheckServerType(SV_TYPE_DIALIN_SERVER,     stDialInServer);
  CheckServerType(SV_TYPE_XENIX_SERVER,      stXenixServer);
  CheckServerType(SV_TYPE_NT,                stNT);
  CheckServerType(SV_TYPE_WFW,               stWFW);
  CheckServerType(SV_TYPE_SERVER_MFPN,       stServerMFPN);
  CheckServerType(SV_TYPE_SERVER_NT,         stServerNT);
  CheckServerType(SV_TYPE_POTENTIAL_BROWSER, stPotentialBrowser);
  CheckServerType(SV_TYPE_BACKUP_BROWSER,    stBackupBrowser);
  CheckServerType(SV_TYPE_MASTER_BROWSER,    stMasterBrowser);
  CheckServerType(SV_TYPE_DOMAIN_MASTER,     stDomainMaster);
  CheckServerType(SV_TYPE_SERVER_OSF,        stServerOSF);
  CheckServerType(SV_TYPE_SERVER_VMS,        stServerVMS);
  CheckServerType(SV_TYPE_WINDOWS,           stWindows);
  CheckServerType(SV_TYPE_CLUSTER_NT,        stClusterNT);
  CheckServerType(SV_TYPE_TERMINALSERVER,    stTerminalServer);
  CheckServerType(SV_TYPE_LOCAL_LIST_ONLY,   stLocalListOnly);
  CheckServerType(SV_TYPE_DOMAIN_ENUM,       stDomainEnum);
  CheckServerType(SV_TYPE_ALL,               stAll);
end;

//==================================================================================================
function TServerInfo.GetPlatformId: TPlatformId;
begin
  Result := FPlatformId;
end;

//==================================================================================================
function TServerInfo.GetName: String;
begin
  Result := FServerName;
end;

//==================================================================================================
function TServerInfo.GetVersionMajor: LongInt;
begin
  Result := -1;
  // Return comment only if store info is ServerInfo101
  if FServerInfoType = lvServerInfo101 then Result := FVersionMajor;
end;

//==================================================================================================
function TServerInfo.GetVersionMinor: LongInt;
begin
  Result := -1;
  // Return comment only if store info is ServerInfo101
  if FServerInfoType = lvServerInfo101 then Result := FVersionMinor;
end;

//==================================================================================================
function TServerInfo.GetServerType: TServerTypes;
begin
  Result := [];
  // Return comment only if store info is ServerInfo101
  if FServerInfoType = lvServerInfo101 then Result := FServerType;
end;

//==================================================================================================
function TServerInfo.GetComment: String;
begin
  Result := '';
  // Return comment only if store info is ServerInfo101
  if FServerInfoType = lvServerInfo101 then Result := FComment;
end;

//==================================================================================================
end.
