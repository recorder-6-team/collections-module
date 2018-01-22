{===============================================================================
  Unit:        CollectionsModuleSettings

  Defines:     TCollectionsModuleSettings

  Description: Provides access to data that is shared across the entire session,
               including all Collections Module addins.

  Created:     Nov 2003

  Model:       CollectionsModuleManager.mpb
===============================================================================}
unit CollectionsModuleSettings;

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  ComObj, ActiveX, CollectionsModuleManager_TLB, StdVcl,  DataTypes, Registry,
  Recorder2000_TLB, ExceptionForm, Windows, Sysutils, Graphics, ADODB,
  ADODB_TLB, LoginScreen, Controls, Forms, LuxembourgConstants, dialogs;

type
  {-----------------------------------------------------------------------------
    Class execption type
  }
  ECollectionsModuleSettings = class (TExceptionPath)
  end;
  
  {-----------------------------------------------------------------------------
    Rather than use a standard TAutoObjectFactory, which creates any number of instances, this 
    class overrides CreateComObject to ensure only one instance of TCollectionsModuleSettings 
    is ever created.
  }
  TSingletonAutoObjectFactory = class (TAutoObjectFactory)
  public
    function CreateComObject(const Controller: IUnknown): TComObject; override;
  end;

  {-----------------------------------------------------------------------------
    COM class for providing access to and persistence of session data.
  }
  TCollectionsModuleSettings = class (TAutoObject, ICollectionsModuleSettings)
  private
    FAddDomainMask: LongWord;
    FAllowFinance: Boolean;
    FAllowMovementEdit: Boolean;
    FAllowQuickEntry: Boolean;
    FAllowQuickEntryProcessing: Boolean;
    FConnection: TADOConnection;
    FConnectionObject: _Connection;
    FDefaultViewType: String;
    FDictImagesPath: String;
    FDisplayCommonNames: Boolean;
    FDomainMask: LongWord;
    FDragDestinationColour: TColor;
    FDragSourceColour: TColor;
    FEditDomainMask: LongWord;
    FIncludeHierarchySynonyms: Boolean;
    FInitialisationSuccessful: Boolean;
    FInstallationPath: String;
    FAddinPath: string;
    FISOCurrency: String;
    FISOLanguage: TLanguageKeyString;
    FLastThesaurusConceptGroup: String;
    FLocalImagesPath: String;
    FMandatoryColour: TColor;
    FPreferredSynonymsOnly: Boolean;
    FGroupDeterminationsByDomain: Boolean;
    FUseOriginalSpecimenNames: Boolean;
    FShowGroupInQuickEntry: Boolean;
    FShowRecorderSpecimensTab: Boolean;
    FQuickEntryImportTypeIndex: Integer;
    FQuickEntryShowAsForm: Boolean;
    FSecurityLevel: Boolean;
    FSessionID: string;
    FSessionOpened: Boolean;
    FSpecimenFinderSortOrderIndex: Integer;
    FStandardReportTemplatePath: String;
    FSpecimenImagePath: string;
    FUserAccessLevel: SmallInt;
    FUserID: String;
    procedure CreateConnection;
    procedure LoginRequired;
    procedure OpenConnection(const AConnectionString: string);
    function ReadBoolDefault(ARegistry: TRegistry; const AValue: string; ADefault: boolean): 
        Boolean;
    function ReadDomainMaskFromDB: LongWord;
    procedure ReadDomainSettings;
    procedure ReadRegistrySettings;
    procedure ReadSecuritySettings;
    function RecorderAvailable: Boolean;
    function RecorderIntf: IRecorder2000;
    procedure WriteRegistrySettings;
    procedure CloseSession;
    procedure OpenSession;
    function ReadIntegerDefault(AReg: TRegistry; const AValue: string; ADefault:
        integer): Integer;
    procedure SetApplicationSecurity;
    procedure ClearTempFiles;
  protected
    procedure AddItemToPersistentList(const AListName, AItem: WideString; AMaxLength: Integer);
        safecall;
    function GetItemsFromPersistentList(const AListName: WideString): WideString; safecall;
    function Get_AddDomainMask: Integer; safecall;
    function Get_AllowAdd(ADomainMask: Integer): WordBool; safecall;
    function Get_AllowEdit(ADomainMask: Integer): WordBool; safecall;
    function Get_AllowFinance: WordBool; safecall;
    function Get_AllowMovementEdit: WordBool; safecall;
    function Get_AllowQuickEntry: WordBool; safecall;
    function Get_AllowQuickEntryProcessing: WordBool; safecall;
    function Get_Connection: Connection; safecall;
    function Get_DefaultViewType: WideString; safecall;
    function Get_DictImagesPath: WideString; safecall;
    function Get_DisableDragDropFrames: WordBool; safecall;
    function Get_DisplayCommonNames: WordBool; safecall;
    function Get_DomainMask: Integer; safecall;
    function Get_DragDestinationColour: Integer; safecall;
    function Get_DragSourceColour: Integer; safecall;
    function Get_EditDomainMask: Integer; safecall;
    function Get_IncludeHierarchySynonyms: WordBool; safecall;
    function Get_InstallationPath: WideString; safecall;
    function Get_ISOCurrency: WideString; safecall;
    function Get_ISOLanguage: WideString; safecall;
    function Get_LastThesaurusConceptGroup: WideString; safecall;
    function Get_LocalImagesPath: WideString; safecall;
    function Get_MandatoryColour: Integer; safecall;
    function Get_QuickEntryImportTypeIndex: Integer; safecall;
    function Get_QuickEntryShowAsForm: WordBool; safecall;
    function Get_SessionID: WideString; safecall;
    function Get_SpecimenFinderSortOrderIndex: Integer; safecall;
    function Get_StandardReportTemplatePath: WideString; safecall;
    function Get_UserAccessLevel: SmallInt; safecall;
    function Get_UserID: WideString; safecall;
    function Get_SpecimenImagePath: WideString; safecall;
    function Get_AddinPath: WideString; safecall;
    function Get_PreferredSynonymsOnly: WordBool; safecall;
    function Get_GroupDeterminationsByDomain: WordBool; safecall;
    function Get_UseOriginalSpecimenNames: WordBool; safecall;
    function Get_ShowGroupInQuickEntry: WordBool; safecall;
    function Get_ShowRecorderSpecimensTab: WordBool; safecall;
    procedure Refresh; safecall;
    procedure Set_DefaultViewType(const Value: WideString); safecall;
    procedure Set_DomainMask(Value: Integer); safecall;
    procedure Set_IncludeHierarchySynonyms(Value: WordBool); safecall;
    procedure Set_LastThesaurusConceptGroup(const Value: WideString); safecall;
    procedure Set_QuickEntryImportTypeIndex(Value : Integer); safecall;
    procedure Set_QuickEntryShowAsForm(Value: WordBool); safecall;
    procedure Set_SpecimenFinderSortOrderIndex(Value: Integer); safecall;
    procedure Set_UserID(const Value: WideString); safecall;
    procedure Set_StandardReportTemplatePath(const Value: WideString); safecall;
    procedure Set_SpecimenImagePath(const Value: WideString); safecall;
    procedure Set_PreferredSynonymsOnly(Value: WordBool); safecall;
    procedure Set_GroupDeterminationsByDomain(Value: WordBool); safecall;
    procedure Set_UseOriginalSpecimenNames(Value: WordBool); safecall;
    procedure Set_ShowGroupInQuickEntry(Value: WordBool); safecall;
    procedure Set_ShowRecorderSpecimensTab(Value: WordBool); safecall;
  public
    destructor Destroy; override;
    procedure Initialize; override;
  end;
  
//==============================================================================
implementation

uses
  ComServ, Classes, DB, ResourceStrings, GeneralData, DSSDataTypes,
  GeneralFunctions, StrUtils;

var
  mComObject: TCOMObject;

{-==============================================================================
    TSingletonAutoObjectFactory
===============================================================================}
{-------------------------------------------------------------------------------
  Override CreateComObject to ensure only one instance. 
}
function TSingletonAutoObjectFactory.CreateComObject(const Controller: IUnknown): TComObject;
begin
  if not Assigned(mComObject) then
    mComObject := inherited CreateComObject(Controller);
  Result := mComObject;
end;  // TSingletonAutoObjectFactory.CreateComObject 

{-==============================================================================
    TCollectionsModuleSettings
===============================================================================}
{-------------------------------------------------------------------------------
  When destroying the object, persist the registry settings.
}
destructor TCollectionsModuleSettings.Destroy;
begin
  if FInitialisationSuccessful then
    WriteRegistrySettings;
  if not RecorderAvailable then
    CloseSession;
  // Close and free the connection
  if FConnection.Connected then FConnection.Close;
  FConnection.Free;
  FConnectionObject := nil;
  TdmGeneral.Discard;
  inherited;
end;  // TCollectionsModuleSettings.Destroy

{-------------------------------------------------------------------------------
  Adds a new item to a comma separated list of keys in the registry.  Pushes the last one out 
      if number of items exceeds AMaxLength 
}
procedure TCollectionsModuleSettings.AddItemToPersistentList(const AListName, AItem: 
    WideString; AMaxLength: Integer);
var
  lStringList: TStringList;
  liIndex: Integer;
begin
  with TRegistry.Create do
    try
      Rootkey := HKEY_CURRENT_USER;
      if OpenKey(LIST_HISTORY_REGISTRY_KEY, True) then
      begin
        lStringList := TStringList.Create;
        try
          lStringList.Delimiter := ',';
          lStringList.QuoteChar := '''';
          lStringList.CommaText := ReadString(AListName);
          //check for an existing item with the same key
          liIndex := lStringList.IndexOf(AItem);
          if liIndex >=0 then lStringList.Delete(liIndex);
  
          //insert the new item
          lStringList.Insert(0, AItem);
          //truncate list to the maximum length
          while lStringList.Count > AMaxLength do
            lStringList.Delete(lStringList.Count -1);
          WriteString(AListName, lStringList.CommaText);
        finally
          lStringList.Free;
        end;
        CloseKey;
      end
      else
        raise ECollectionsModuleSettings.CreateNonCritical(ResStr_CannotOpenRegistry);
    finally
      Free;
    end;
end;  // TCollectionsModuleSettings.AddItemToPersistentList 

{-------------------------------------------------------------------------------
  Populates the log out time in Session (if not sharing Recorder's session)
}
procedure TCollectionsModuleSettings.CloseSession;
var
  lCmd: TADOCommand;
begin
  if FSessionOpened then begin
    lCmd := TADOCommand.Create(nil);
    try
      //Avoid using dmGeneral.GetRecordset as potentially it could infinitely loop
      //by calling back to AppSettings.
      with lCmd do begin
        Connection := FConnection;
        CommandType := cmdStoredProc;
        CommandText := 'usp_Session_Close';
        Parameters.CreateParameter('@SessionID', ftString, pdInput, 16, FSessionID);
        Execute;
      end;
    finally
      lCmd.Free;
    end;
  end;
end;  // TCollectionsModuleSettings.CloseSession


{-------------------------------------------------------------------------------
  Creates an ADO connection.  Either uses the Recorder application to provide connection 
      details, or reads them from the registry. 
}
procedure TCollectionsModuleSettings.CreateConnection;
var
  lTrusted: Boolean;
  lServerName, lDbName, lSecurity: String;
begin
  FConnection := TADOConnection.Create(nil);
  if RecorderAvailable then
    OpenConnection(RecorderIntf.ConnectionString)
  else begin
    { TODO : Build proper connection string }
    with TRegistry.Create do
      try
        RootKey := HKEY_LOCAL_MACHINE;
        if OpenKeyReadOnly(JNCC_REG_ROOT_PATH) then begin
          if ValueExists('Server Name') then begin
            lServerName := ReadString('Server Name');
            lDbName     := ReadString('Database Name');
            lTrusted    := ReadBool('Trusted Security');

            if lTrusted then
              lSecurity := 'Integrated Security=SSPI;'
            else
              lSecurity := 'User ID=NBNUser;password=NBNPassword;';
            OpenConnection(Format('Provider=SQLOLEDB.1;%sPersist Security Info=False;' +
                                  'Data Source=%s;Initial Catalog=%s;OLE DB Services=-2',
                                  [lSecurity, lServerName, lDbName]));
          end;
          CloseKey;
        end;
      finally
        Free;
      end;
  end;
end;  // TCollectionsModuleSettings.CreateConnection

{-------------------------------------------------------------------------------
  Returns a comma separated list of items from the registry.  Used in combo boxes where the
      last ten entered values are remembered.
}
function TCollectionsModuleSettings.GetItemsFromPersistentList(const AListName: WideString): 
    WideString;
begin
  with TRegistry.Create do
    try
      Rootkey := HKEY_CURRENT_USER;
      if OpenKey(LIST_HISTORY_REGISTRY_KEY, True) then
        Result := ReadString(AListName)
      else
        raise ECollectionsModuleSettings.CreateNonCritical(ResStr_CannotOpenRegistry);
    finally
      Free;
    end;
end;  // TCollectionsModuleSettings.GetItemsFromPersistentList 

{-------------------------------------------------------------------------------
}
function TCollectionsModuleSettings.Get_AddDomainMask: Integer;
begin
  Result := FAddDomainMask;
end;  // TCollectionsModuleSettings.Get_AddDomainMask 

{-------------------------------------------------------------------------------
}
function TCollectionsModuleSettings.Get_AllowAdd(ADomainMask: Integer): WordBool;
begin
  Result := (ADomainMask And FAddDomainMask) > 0;
end;  // TCollectionsModuleSettings.Get_AllowAdd

{-------------------------------------------------------------------------------
}
function TCollectionsModuleSettings.Get_AllowEdit(ADomainMask: Integer): WordBool;
begin
  Result := (ADomainMask And FEditDomainMask) > 0;
end;  // TCollectionsModuleSettings.Get_AllowEdit 

{-------------------------------------------------------------------------------
  Accessor method. 
}
function TCollectionsModuleSettings.Get_AllowFinance: WordBool;
begin
  Result := FAllowFinance;
end;  // TCollectionsModuleSettings.Get_AllowFinance 

{-------------------------------------------------------------------------------
}
function TCollectionsModuleSettings.Get_AllowMovementEdit: WordBool;
begin
  Result := FAllowMovementEdit;
end;  // TCollectionsModuleSettings.Get_AllowMovementEdit 

{-------------------------------------------------------------------------------
  Accessor method. 
}
function TCollectionsModuleSettings.Get_AllowQuickEntry: WordBool;
begin
  Result := FAllowQuickEntry;
end;  // TCollectionsModuleSettings.Get_AllowQuickEntry 

{-------------------------------------------------------------------------------
  Accessor method. 
}
function TCollectionsModuleSettings.Get_AllowQuickEntryProcessing: WordBool;
begin
  Result := FAllowQuickEntryProcessing;
end;  // TCollectionsModuleSettings.Get_AllowQuickEntryProcessing 

{-------------------------------------------------------------------------------
}
function TCollectionsModuleSettings.Get_Connection: Connection;
begin
  Result := FConnection.ConnectionObject as Connection;
end;  // TCollectionsModuleSettings.Get_Connection 

{-------------------------------------------------------------------------------
  COM Accessor method.  Reads the node context string for the view type. 
}
function TCollectionsModuleSettings.Get_DefaultViewType: WideString;
begin
  Result := FDefaultViewType;
end;  // TCollectionsModuleSettings.Get_DefaultViewType 

{-------------------------------------------------------------------------------
  Accessor method.
}
function TCollectionsModuleSettings.Get_DictImagesPath: WideString;
begin
  Result := FDictImagesPath;
end;  // TCollectionsModuleSettings.Get_DictImagesPath 

{-------------------------------------------------------------------------------
  Accessor method. 
}
function TCollectionsModuleSettings.Get_DisableDragDropFrames: WordBool;
begin
  if RecorderAvailable then
    Result := RecorderIntf.CurrentSettings.DisableDragDropFrames
  else
    Result := False;
end;  // TCollectionsModuleSettings.Get_DisableDragDropFrames 

{-------------------------------------------------------------------------------
  Accessor method. 
}
function TCollectionsModuleSettings.Get_DisplayCommonNames: WordBool;
begin
  Result := FDisplayCommonNames;
end;  // TCollectionsModuleSettings.Get_DisplayCommonNames 

{-------------------------------------------------------------------------------
  Accessor method. 
}
function TCollectionsModuleSettings.Get_DomainMask: Integer;
begin
  Result := FDomainMask;
end;  // TCollectionsModuleSettings.Get_DomainMask 

{-------------------------------------------------------------------------------
  Accessor method. 
}
function TCollectionsModuleSettings.Get_DragDestinationColour: Integer;
begin
  if RecorderAvailable then
    // Use the 'live' setting if available
    Result := RecorderIntf.CurrentSettings.DragDestinationColour
  else
    // otherwise use the one from the registry
    Result := FDragDestinationColour;
end;  // TCollectionsModuleSettings.Get_DragDestinationColour 

{-------------------------------------------------------------------------------
  Accessor method. 
}
function TCollectionsModuleSettings.Get_DragSourceColour: Integer;
begin
  if RecorderAvailable then
    // Use the 'live' setting if available
    Result := RecorderIntf.CurrentSettings.DragSourceColour
  else
    // otherwise use the one from the registry
    Result := FDragSourceColour;
end;  // TCollectionsModuleSettings.Get_DragSourceColour 

{-------------------------------------------------------------------------------
}
function TCollectionsModuleSettings.Get_EditDomainMask: Integer;
begin
  Result := FEditDomainMask;
end;  // TCollectionsModuleSettings.Get_EditDomainMask 

{-------------------------------------------------------------------------------
  Accessor method. 
}
function TCollectionsModuleSettings.Get_IncludeHierarchySynonyms: WordBool;
begin
  Result := FIncludeHierarchySynonyms;
end;  // TCollectionsModuleSettings.Get_IncludeHierarchySynonyms 

{-------------------------------------------------------------------------------
  Accessor method. 
}
function TCollectionsModuleSettings.Get_InstallationPath: WideString;
begin
  Result := FInstallationPath;
end;  // TCollectionsModuleSettings.Get_InstallationPath 

{-------------------------------------------------------------------------------
  Accessor method, returns the iso currency code according to the regional settings. 
}
function TCollectionsModuleSettings.Get_ISOCurrency: WideString;
begin
  if FISOCurrency = '' then
    FISOLanguage := GetLocaleStr(LOCALE_USER_DEFAULT, LOCALE_SINTLSYMBOL, 'Error');
  Result := FISOCurrency;
end;  // TCollectionsModuleSettings.Get_ISOCurrency 

{-------------------------------------------------------------------------------
  Accessor method, returns the ISO language code according to the regional settings. 
}
function TCollectionsModuleSettings.Get_ISOLanguage: WideString;
begin
  if FISOLanguage = '' then
    FISOLanguage := GetLocaleStr(LOCALE_USER_DEFAULT, LOCALE_SABBREVLANGNAME, 'Error');
  Result := FISOLanguage;
end;  // TCollectionsModuleSettings.Get_ISOLanguage 

{-------------------------------------------------------------------------------
  Accessor method. 
}
function TCollectionsModuleSettings.Get_LastThesaurusConceptGroup: WideString;
begin
  Result := FLastThesaurusConceptGroup;
end;  // TCollectionsModuleSettings.Get_LastThesaurusConceptGroup 

{-------------------------------------------------------------------------------
  Accessor method.
}
function TCollectionsModuleSettings.Get_LocalImagesPath: WideString;
begin
  Result := FLocalImagesPath;
end;  // TCollectionsModuleSettings.Get_LocalImagesPath 

{-------------------------------------------------------------------------------
  Accessor method. 
}
function TCollectionsModuleSettings.Get_MandatoryColour: Integer;
begin
  if RecorderAvailable then
    // Use the 'live' setting if available
    Result := RecorderIntf.CurrentSettings.MandatoryColour
  else
    // otherwise use the one from the registry
    Result := FMandatoryColour;
end;  // TCollectionsModuleSettings.Get_MandatoryColour 

{-------------------------------------------------------------------------------
}
function TCollectionsModuleSettings.Get_QuickEntryImportTypeIndex: Integer;
begin
  Result := FQuickEntryImportTypeIndex;
end;  // TCollectionsModuleSettings.Get_QuickEntryImportTypeIndex 

{-------------------------------------------------------------------------------
  Accessor method. 
}
function TCollectionsModuleSettings.Get_QuickEntryShowAsForm: WordBool;
begin
  Result := FQuickEntryShowAsForm;
end;  // TCollectionsModuleSettings.Get_QuickEntryShowAsForm 

{-------------------------------------------------------------------------------
  Accessor method.
}
function TCollectionsModuleSettings.Get_SessionID: WideString;
begin
  Result := FSessionID;
end;  // TCollectionsModuleSettings.Get_SessionID

{-------------------------------------------------------------------------------
  Accessor method.
}
function TCollectionsModuleSettings.Get_SpecimenFinderSortOrderIndex: Integer;
begin
  Result := FSpecimenFinderSortOrderIndex;
end;  // TCollectionsModuleSettings.Get_SpecimenFinderSortOrderIndex

{-------------------------------------------------------------------------------
  COM Accessor method 
}
function TCollectionsModuleSettings.Get_StandardReportTemplatePath: WideString;
begin
  Result := FStandardReportTemplatePath;
end;  // TCollectionsModuleSettings.Get_StandardReportTemplatePath

{-------------------------------------------------------------------------------
}
function TCollectionsModuleSettings.Get_UserAccessLevel: SmallInt;
begin
  Result := FUserAccessLevel;
end;  // TCollectionsModuleSettings.Get_UserAccessLevel 

{-------------------------------------------------------------------------------
  Accessor method. 
}
function TCollectionsModuleSettings.Get_UserID: WideString;
begin
  if FUserID='' then
    if RecorderAvailable then
      FUserID := RecorderIntf.CurrentSettings.UserIDKey
    else
      LoginRequired;
  Result := FUserID;
end;  // TCollectionsModuleSettings.Get_UserID

{-------------------------------------------------------------------------------
  Initialise the object and load all the settings.
}
procedure TCollectionsModuleSettings.Initialize;
begin
  // Set a flag to ensure that if contructing the object fails, settings are not saved
  FInitialisationSuccessful := False;
  inherited;
  FSessionOpened := False;
  CreateConnection;
  if RecorderAvailable then begin
    FSessionID := RecorderIntf.CurrentSettings.SessionID;
    // force the user id to be obtained as this prevents a later crash
    Get_UserID;
  end
  else
    OpenSession;
  Refresh;
  SetApplicationSecurity;
  ClearTempFiles;  
  FInitialisationSuccessful := True;
end;  // TCollectionsModuleSettings.Initialize

{-------------------------------------------------------------------------------
  Displays a login prompt and obtains the user ID.  Required when the user has not logged into 
      Recorder. 
}
procedure TCollectionsModuleSettings.LoginRequired;
begin
  with TfrmLoginScreen.Create(nil) do
    try
      case ShowModal of
        mrOk      : FUserID := UserID;
        mrCancel  : FUserID := '';
      end;
    finally
      Free;
    end; // try
end;  // TCollectionsModuleSettings.LoginRequired 

{-------------------------------------------------------------------------------
  Opens the connection using the supplied connection string 
}
procedure TCollectionsModuleSettings.OpenConnection(const AConnectionString: string);
begin
  with FConnection do begin
    ConnectionString := AConnectionString;
    CommandTimeout   := 0;
    LoginPrompt      := False;
    Open;
  end;
  FConnectionObject := FConnection.ConnectionObject as _Connection;
end;  // TCollectionsModuleSettings.OpenConnection

{-------------------------------------------------------------------------------
  Creates a Session record and sets FSessionID.
}
procedure TCollectionsModuleSettings.OpenSession;
var
  lCmd: TADOCommand;
begin
  lCmd := TADOCommand.Create(nil);
  try
    //Avoid using dmGeneral.GetRecordset as potentially it could infinitely loop
    //by calling back to AppSettings.
    with lCmd do begin
      Connection      := FConnection;
      CommandType     := cmdStoredProc;
      CommandTimeOut  := 0;
      CommandText     := 'usp_Session_Insert';
      ExecuteOptions := [eoExecuteNoRecords];
      Parameters.CreateParameter('@Key', ftString, pdOutput, 16, FSessionID);
      Parameters.CreateParameter('@UserID', ftString, pdInput, 16, Get_UserID);
      Execute;
      FSessionID := lCmd.Parameters.ParamByName('@Key').Value;
      FSessionOpened := true;
    end;
  finally
    lCmd.Free;
  end;
end;  // TCollectionsModuleSettings.OpenSession

{-------------------------------------------------------------------------------
  Reads a boolean value from the registery, if not found, then returns the supplied default.
}
function TCollectionsModuleSettings.ReadBoolDefault(ARegistry: TRegistry; const AValue: string;
    ADefault: boolean): Boolean;
begin
  if ARegistry.ValueExists(AValue) then
    Result := ARegistry.ReadBool(AValue)
  else
    Result := ADefault;
end;  // TCollectionsModuleSettings.ReadBoolDefault 

{-------------------------------------------------------------------------------
}
function TCollectionsModuleSettings.ReadDomainMaskFromDB: LongWord;
var
  lCmd: TADOCommand;
  lDomainMask: LongWord;
begin
  //Defaults user to being able to see no domains
  lDomainMask := 0;
  lCmd := TADOCommand.Create(nil);
  try
    //Avoid using dmGeneral.GetStoredProcOutputParam as potentially it could
    //infinitely loop by calling back to AppSettings.
    with lCmd do begin
      Connection := FConnection;
      CommandType := cmdStoredProc;
      CommandText := 'usp_Domain_Mask_Get';
      Parameters.CreateParameter('@Name_Key', ftString, pdInput, 16, FUserID);
      Parameters.CreateParameter('@UserDomainMask', ftInteger, pdOutput, 16, lDomainMask);
      ExecuteOptions := [eoExecuteNoRecords];
      Execute;
      lDomainMask := Parameters.ParamValues['@UserDomainMask'];
    end;
  finally
    lCmd.Free;
  end;
  Result := LongWord(lDomainMask);
end;  // TCollectionsModuleSettings.ReadDomainMaskFromDB 

{-------------------------------------------------------------------------------
}
procedure TCollectionsModuleSettings.ReadDomainSettings;
var
  lCmd: TADOCommand;
begin
  // FUserID should have a value by now from the login prompt because Get_UserID
  // was called.
  if FUserID <> '' then begin
    lCmd := TADOCommand.Create(nil);
    try
      //Avoid using dmGeneral.GetRecordset as potentially it could infinitely loop
      //by calling back to AppSettings.
      with lCmd do begin
        Connection := FConnection;
        CommandType := cmdStoredProc;
        CommandText := 'usp_UserDomainAccess_Get';
        Parameters.CreateParameter('@UserID', ftString, pdInput, 16, FUserID);
      end;
  
        with lCmd.Execute do begin
          if RecordCount <> 1 then
            raise ECollectionsModuleSettings.Create(ResStr_UserSecuritySettingsNotFound);
          MoveFirst;
          FAddDomainMask := Fields['Add_Domain_Mask'].Value;
          FEditDomainMask := Fields['Edit_Domain_Mask'].Value;
          Close;
        end;
    finally
      lCmd.Free;
    end;
  end
  else // if FUserID <> ''
    Application.Terminate;
end;  // TCollectionsModuleSettings.ReadDomainSettings 

{-------------------------------------------------------------------------------
  Read an integer from the registry, or use a default value if missing.
}
function TCollectionsModuleSettings.ReadIntegerDefault(AReg: TRegistry; const
    AValue: string; ADefault: integer): Integer;
begin
  if AReg.ValueExists(AValue) then
    result := AReg.ReadInteger(AValue)
  else
    result := ADefault;
end;

{-------------------------------------------------------------------------------
  Read settings from the registry. 
}
procedure TCollectionsModuleSettings.ReadRegistrySettings;
var
  reg: TRegistry;
begin
  reg := TRegistry.Create;
  with reg do
    try
      RootKey := HKEY_CURRENT_USER;
      // Recorder User settings
      if OpenKeyReadOnly(USER_SETTINGS_REG_PATH) then
      begin
        FDictImagesPath           := ReadString(OPT_DICT_IMAGES_PATH);
        FDisplayCommonNames       := ReadBool(OPT_DISPLAY_COMMON_NAMES);
        FIncludeHierarchySynonyms := ReadBoolDefault(reg, OPT_INCLUDE_HIERARCHY_SYNONYMS, False);
        FLocalImagesPath          := ReadString(OPT_LOCAL_IMAGES_PATH);
        ReadBinaryData(OPT_DRAG_DEST_COLOUR, FDragDestinationColour, SizeOf(TColor));
        ReadBinaryData(OPT_DRAG_SOURCE_COLOUR, FDragSourceColour, SizeOf(TColor));
        ReadBinaryData(OPT_MANDATORY_COLOUR, FMandatoryColour, SizeOf(TColor));
        CloseKey;
      end;

      // Collections Module User settings
      if OpenKeyReadOnly(COLLECTIONS_BROWSER_REG_PATH) then
      begin
        FIncludeHierarchySynonyms := ReadBoolDefault(reg, OPT_INCLUDE_HIERARCHY_SYNONYMS, False);
        if not ValueExists(OPT_QE_IMPORT_TYPE_INDEX) then
          FQuickEntryImportTypeIndex := 1  // '.CSV'
        else
          FQuickEntryImportTypeIndex := ReadInteger(OPT_QE_IMPORT_TYPE_INDEX);

        FQuickEntryShowAsForm      := ReadBoolDefault(reg, OPT_QE_SHOW_AS_FORM, False);
        FLastThesaurusConceptGroup := ReadString(OPT_LAST_CONCEPT_GROUP);
        FDefaultViewType           := ReadString(OPT_VIEW_TYPE);
        // For the specimen finder sort order use a default value for first usage.
        FSpecimenFinderSortOrderIndex := ReadIntegerDefault(reg, OPT_SPECFIND_SORT_INDEX,0);
        FGroupDeterminationsByDomain :=
            ReadBoolDefault(reg, OPT_GROUP_DETERMINATIONS_BY_DOMAIN, False);
        FUseOriginalSpecimenNames :=
            ReadBoolDefault(reg, OPT_USE_ORIGINAL_SPECIMEN_NAMES, False);
        FShowGroupInQuickEntry :=
            ReadBoolDefault(reg, OPT_QUICK_ENTRY_DETERMINATION_LISTS, False);
        FShowRecorderSpecimensTab :=
            ReadBoolDefault(reg, OPT_SHOW_RECORDER_SPECIMENS_TAB, False);

        FDomainMask := ReadDomainMaskFromDB;
        
        // Apply Domain Filter if one exists
        if ValueExists(OPT_DOMAIN_MASK) then
          // Old-style setting: a single mask for all Recorder accounts
          FDomainMask := FDomainMask and not ReadInteger(OPT_DOMAIN_MASK)
        else if OpenKeyReadOnly('Users\' + FUserID) then
        begin
          // Per user id settings
          if ValueExists(OPT_DOMAIN_MASK) then
            FDomainMask := FDomainMask and not ReadInteger(OPT_DOMAIN_MASK)
        end;

        CloseKey;
      end;

      // Recorder Machine settings
      RootKey := HKEY_LOCAL_MACHINE;
      if OpenKeyReadOnly(RECORDER_MACHINE_SETTINGS_REG_PATH) then
      begin
        FInstallationPath := ReadString(OPT_INSTALLATION_PATH);
        FAddinPath        := ReadString(OPT_ADDINS_PATH);
        CloseKey;
      end;

      // Collections Module machine settings
      if OpenKeyReadOnly(COLLECTIONS_MACHINE_SETTINGS_REG_PATH) then
      begin
        FStandardReportTemplatePath := ReadString(OPT_STD_RPT_TEMPLATE_PATH);
        FSpecimenImagePath          := ReadString(OPT_SPECIMEN_IMAGE_PATH);
        FPreferredSynonymsOnly      :=
            ReadBoolDefault(reg, OPT_PREFERRED_SYNONYMS_ONLY, False);
        CloseKey;
      end;
    finally
      Free;
    end; // try
end;  // TCollectionsModuleSettings.ReadRegistrySettings 

{-------------------------------------------------------------------------------
  Read the user's security from the USER table. 
}
procedure TCollectionsModuleSettings.ReadSecuritySettings;
var
  lCmd: TADOCommand;
begin
  // FUserID should have a value by now from the login prompt because Get_UserID
  // was called.
  if FUserID <> '' then
  begin
    lCmd := TADOCommand.Create(nil);
    try
      //Avoid using dmGeneral.GetRecordset as potentially it could infinitely loop
      //by calling back to AppSettings.
      with lCmd do begin
        Connection  := FConnection;
        CommandType := cmdStoredProc;
        CommandText := 'usp_Security_Settings_Get';
        Parameters.CreateParameter('@UserID', ftString, pdInput, 16, FUserID);
      end;

      with lCmd.Execute do
      begin
        if RecordCount <> 1 then
          raise ECollectionsModuleSettings.Create(ResStr_UserSecuritySettingsNotFound);
        MoveFirst;
        FSecurityLevel             := Fields['Security_Level'].Value;
        FAllowQuickEntry           := Fields['Allow_Quick_Entry'].Value;
        FAllowQuickEntryProcessing := Fields['Allow_Quick_Entry_Processing'].Value;
        FAllowMovementEdit         := Fields['Allow_Movement_Edit'].Value;
        FAllowFinance              := Fields['Allow_Finance'].Value;
        FUserAccessLevel           := SmallInt(Fields['Security_Level'].Value)-1; //Zero-based
        Close;
      end;
    finally
      lCmd.Free;
    end;
  end
  else // if FUserID <> ''
    Application.Terminate;
end;  // TCollectionsModuleSettings.ReadSecuritySettings

{-------------------------------------------------------------------------------
}
function TCollectionsModuleSettings.RecorderAvailable: Boolean;
var
  lHandle: THandle;
begin
  lHandle := OpenMutex(MUTEX_ALL_ACCESS, false, 'Recorder 2000');
  Result  := lHandle <> 0;
  CloseHandle(lHandle);
end;  // TCollectionsModuleSettings.RecorderAvailable

{-------------------------------------------------------------------------------
  Retrieve an instance of the Recorder COM server 
}
function TCollectionsModuleSettings.RecorderIntf: IRecorder2000;
begin
  if RecorderAvailable then
    Result := CreateComObject(CLASS_AutoApplicationSettings) as IRecorder2000
  else
    Result := nil;
end;  // TCollectionsModuleSettings.RecorderIntf 

{-------------------------------------------------------------------------------
}
procedure TCollectionsModuleSettings.Refresh;
begin
  ReadSecuritySettings;
  ReadRegistrySettings;
  ReadDomainSettings;
end;  // TCollectionsModuleSettings.Refresh 

{-------------------------------------------------------------------------------
  Set application role on the connection so the user gets the correct rights
}
procedure TCollectionsModuleSettings.SetApplicationSecurity;
var
  lstAppRole, lstAppRolePwd : string;
  VarRecsAffected: OleVariant; // ignored value
begin
  // find appropriate access level
  case TUserAccessLevel(FUserAccessLevel) of
    ualReadOnly: begin
      lstAppRole := 'R2k_ReadOnly';
      lstAppRolePwd := '1lqm4jozq';
    end;
    ualRecordOnly: begin
      lstAppRole := 'R2k_RecordCardsOnly';
      lstAppRolePwd := 'e095nf73d';
    end;
    ualAddOnly: begin
      lstAppRole := 'R2k_AddOnly';
      lstAppRolePwd := '9gjr74mc8';
    end;
    ualFullUser: begin
      lstAppRole := 'R2k_FullEdit';
      lstAppRolePwd := 'd93nmc741';
    end;
    ualAdmin: begin
      lstAppRole := 'R2k_Administrator';
      lstAppRolePwd := '4mzodgft7';
    end;
  else
    //
    Exit;
  end;
  FConnection.ConnectionObject.Execute(
      'EXEC sp_setapprole ''' + lstAppRole + ''', ''' + lstAppRolePwd + '''',
      VarRecsAffected,
      adCmdText + adExecuteNoRecords);
end;  // SetApplicationRole

{-------------------------------------------------------------------------------
  COM Accessor method. 
}
procedure TCollectionsModuleSettings.Set_DefaultViewType(const Value: WideString);
begin
  FDefaultViewType := Value;
end;  // TCollectionsModuleSettings.Set_DefaultViewType 

{-------------------------------------------------------------------------------
}
procedure TCollectionsModuleSettings.Set_DomainMask(Value: Integer);
begin
  FDomainMask := Value;
end;  // TCollectionsModuleSettings.Set_DomainMask 

{-------------------------------------------------------------------------------
  Accessor method. 
}
procedure TCollectionsModuleSettings.Set_IncludeHierarchySynonyms(Value: WordBool);
begin
  FIncludeHierarchySynonyms := Value;
end;  // TCollectionsModuleSettings.Set_IncludeHierarchySynonyms 

{-------------------------------------------------------------------------------
  Accessor method
}
procedure TCollectionsModuleSettings.Set_LastThesaurusConceptGroup(const Value: WideString);
begin
  FLastThesaurusConceptGroup := Value;
end;  // TCollectionsModuleSettings.Set_LastThesaurusConceptGroup 

{-------------------------------------------------------------------------------
}
procedure TCollectionsModuleSettings.Set_QuickEntryImportTypeIndex(Value:
    Integer);
begin
  FQuickEntryImportTypeIndex := Value;
end;  // TCollectionsModuleSettings.Set_QuickEntryImportTypeIndex

{-------------------------------------------------------------------------------
}
procedure TCollectionsModuleSettings.Set_QuickEntryShowAsForm(Value: WordBool);
begin
  FQuickEntryShowAsForm := Value;
end;  // TCollectionsModuleSettings.Set_QuickEntryShowAsForm

{-------------------------------------------------------------------------------
}
procedure TCollectionsModuleSettings.Set_SpecimenFinderSortOrderIndex(Value: Integer);
begin
  FSpecimenFinderSortOrderIndex := Value;
end;  // TCollectionsModuleSettings.Set_SpecimenFinderSortOrderIndex

{-------------------------------------------------------------------------------
}
procedure TCollectionsModuleSettings.Set_UserID(const Value: WideString);
begin
  FUserID := Value;
end;  // TCollectionsModuleSettings.Set_UserID 

{-------------------------------------------------------------------------------
  Persist Collections Browser settings back to the registry. 
}
procedure TCollectionsModuleSettings.WriteRegistrySettings;
begin
  with TRegistry.Create do
    try
      RootKey := HKEY_CURRENT_USER;
      if OpenKey(COLLECTIONS_BROWSER_REG_PATH, True) then
      begin
        WriteBool(OPT_INCLUDE_HIERARCHY_SYNONYMS, FIncludeHierarchySynonyms);
        WriteInteger(OPT_QE_IMPORT_TYPE_INDEX, FQuickEntryImportTypeIndex);
        WriteBool(OPT_QE_SHOW_AS_FORM, FQuickEntryShowAsForm);
        WriteString(OPT_LAST_CONCEPT_GROUP, FLastThesaurusConceptGroup);
        // OPT_DOMAIN_MASK is now stored under the per-user id sub-key
        DeleteValue(OPT_DOMAIN_MASK);
        WriteString(OPT_VIEW_TYPE, FDefaultViewType);
        WriteInteger(OPT_SPECFIND_SORT_INDEX, FSpecimenFinderSortOrderIndex);
        WriteBool(
            OPT_GROUP_DETERMINATIONS_BY_DOMAIN,
            FGroupDeterminationsByDomain);
        WriteBool(OPT_USE_ORIGINAL_SPECIMEN_NAMES, FUseOriginalSpecimenNames);
        WriteBool(OPT_QUICK_ENTRY_DETERMINATION_LISTS, FShowGroupInQuickEntry);
        WriteBool(OPT_SHOW_RECORDER_SPECIMENS_TAB, FShowRecorderSpecimensTab);

        if OpenKey('Users\' + FUserID, True) then
        begin
          WriteInteger(OPT_DOMAIN_MASK, not FDomainMask);
        end;
      end;

      RootKey := HKEY_LOCAL_MACHINE;
      if OpenKey(COLLECTIONS_MACHINE_SETTINGS_REG_PATH, True) then
      begin
        WriteString(OPT_STD_RPT_TEMPLATE_PATH, FStandardReportTemplatePath);
        WriteString(OPT_SPECIMEN_IMAGE_PATH, FSpecimenImagePath);
        WriteBool(OPT_PREFERRED_SYNONYMS_ONLY, FPreferredSynonymsOnly);
      end;
    finally
      Free;
    end; // try
end;  // TCollectionsModuleSettings.WriteRegistrySettings

{-------------------------------------------------------------------------------
  Accessor method
}
function TCollectionsModuleSettings.Get_SpecimenImagePath: WideString;
begin
  Result := FSpecimenImagePath;
end;

{-------------------------------------------------------------------------------
  Accessor method
}
procedure TCollectionsModuleSettings.Set_SpecimenImagePath(const Value: WideString);
begin
  FSpecimenImagePath := IncludeTrailingPathDelimiter(Value);
end;

{-------------------------------------------------------------------------------
  Accessor method
}
procedure TCollectionsModuleSettings.Set_StandardReportTemplatePath(
  const Value: WideString);
begin
  FStandardReportTemplatePath := IncludeTrailingPathDelimiter(Value);
end;

{-------------------------------------------------------------------------------
  Accessor method
}
function TCollectionsModuleSettings.Get_AddinPath: WideString;
begin
  Result := FAddinPath;
end;

{-------------------------------------------------------------------------------
  Accessor method
}
function TCollectionsModuleSettings.Get_PreferredSynonymsOnly: WordBool;
begin
  Result := FPreferredSynonymsOnly;
end;

{-------------------------------------------------------------------------------
  Accessor method
}
procedure TCollectionsModuleSettings.Set_PreferredSynonymsOnly(Value: WordBool);
begin
  FPreferredSynonymsOnly := Value;
end;

{-------------------------------------------------------------------------------
  Accessor method
}
function TCollectionsModuleSettings.Get_GroupDeterminationsByDomain: WordBool;
begin
  Result := FGroupDeterminationsByDomain;
end;

{-------------------------------------------------------------------------------
  Accessor method
}
procedure TCollectionsModuleSettings.Set_GroupDeterminationsByDomain(
  Value: WordBool);
begin
  FGroupDeterminationsByDomain := Value;
end;

{-------------------------------------------------------------------------------
  Accessor method
}
function TCollectionsModuleSettings.Get_UseOriginalSpecimenNames: WordBool;
begin
  Result := FUseOriginalSpecimenNames;
end;

{-------------------------------------------------------------------------------
  Accessor method
}
procedure TCollectionsModuleSettings.Set_UseOriginalSpecimenNames(
  Value: WordBool);
begin
  FUseOriginalSpecimenNames := Value;
end;

{-------------------------------------------------------------------------------
  Accessor method
}
function TCollectionsModuleSettings.Get_ShowGroupInQuickEntry: WordBool;
begin
  Result := FShowGroupInQuickEntry;
end;

{-------------------------------------------------------------------------------
  Accessor method
}
procedure TCollectionsModuleSettings.Set_ShowGroupInQuickEntry(
  Value: WordBool);
begin
  FShowGroupInQuickEntry := Value;
end;

{-------------------------------------------------------------------------------
  Accessor method
}
function TCollectionsModuleSettings.Get_ShowRecorderSpecimensTab: WordBool;
begin
  Result := FShowRecorderSpecimensTab;
end;

{-------------------------------------------------------------------------------
  Accessor method
}
procedure TCollectionsModuleSettings.Set_ShowRecorderSpecimensTab(
  Value: WordBool);
begin
  FShowRecorderSpecimensTab := Value;

  // This is the last property that is set when TCollectionsOptionsPage.Save is
  // called (the last thing it does is call TfraGeneralOptions.Save, which sets
  // the property in its final statement) so we can write our settings to the
  // registry here to ensure that they are all saved as soon as possible,
  // instead of deferring it until Recorder exits. This means that we do not
  // lose any changes in the event that Recorder crashes instead of shutting
  // down cleanly. A specific ICollectionsModuleSettings.Save method would be
  // a better way to do this, but I did not want to change the interface
  // definition because MNHNL do not re-register the libraries on all their
  // client machines when upgrading.
  WriteRegistrySettings;
end;

{-------------------------------------------------------------------------------
  Removes any temporary files created by the collections module
}
procedure TCollectionsModuleSettings.ClearTempFiles;
var
  lTempFolder: string;
begin
  lTempFolder := IncludeTrailingPathDelimiter(GetWindowsTempDir);
  DeleteFiles(lTempFolder, FN_TEMP_FILE_PREFIX + '*.*');
end;

initialization
  TSingletonAutoObjectFactory.Create(ComServer, TCollectionsModuleSettings,
      Class_CollectionsModuleSettings,
    ciMultiInstance, tmApartment);
end.
