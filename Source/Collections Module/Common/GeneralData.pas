{===============================================================================
  Unit:        GeneralData.pas

  Defines:     TdmGeneral

  Description: A class providing a handle to Recorder and a connection
               used throughout addins.

  Model:       CollectionBrowserFramework.mpb

  Created:     August 2003

  Last revision information:
    $Revision: 47 $
    $Date: 9/11/09 17:15 $
    $Author: Andrewkemp $

===============================================================================}

unit GeneralData;

interface

uses
  SysUtils, Windows, Classes, Controls, DB, ADODB, Recorder2000_TLB, Variants, VagueDate,
  ExceptionForm, BaseADODataModule, Contnrs, Menus;

type
  EGeneralDataException = class(TExceptionPath)
  end;

  {-----------------------------------------------------------------------------
    Class for database access.
  }
  TdmGeneral = class(TdmBaseADO)
  private
    FAllowAllDomains: Boolean;
    function GetDomainMask: Integer;
    procedure MapWindowMenuClick(Sender: TObject);
    procedure SetAllowAllDomains(Value: Boolean);
    procedure SetVagueDateNulls(var AParams: Array of Variant);
  protected
    function GetRecorderRunning: Boolean; virtual;
    procedure SetupCommandParameters(AParameters: TParameters; AParams: Array of Variant);
        override;
  public
    constructor Create(AOwner: TComponent); override;
    constructor CreateStandalone(AOwner: TComponent; const AConnectionString: String);
    class function Allocated: Boolean;
    class procedure Discard;
    function GetVagueDateStringFromRecordset(ARecordset: _Recordset; const ASpecifier: String
        = ''): String;
    function Recorder: IRecorder2000;
    procedure UpdateMapMenu(Sender: TObject; AMenuItem: TMenuItem; ASetDefault: Boolean =
        False; AUserClickEvent: TNotifyEvent = nil);
    property AllowAllDomains: Boolean read FAllowAllDomains write SetAllowAllDomains;
    property RecorderRunning: Boolean read GetRecorderRunning;
  end;

function dmGeneral: TdmGeneral;

//==============================================================================
implementation

{$R *.dfm}

uses
  ComObj, ResourceStrings, ApplicationSettings, LuxembourgConstants, Forms,
  CollectionsModuleManager_TLB;

var
  mdmGeneral: TdmGeneral;
  mInAccessorFunction: boolean;

const
  // Parameters automatically populated when setting up an ADO command object.
  // The values are read from AppSettings.
  PARAM_USER_ID                      = '@UserID';
  PARAM_USER_DOMAIN_MASK             = '@UserDomainMask';
  PARAM_SESSION_ID                   = '@SessionID';
  PARAM_SHORT_DATE_FORMAT            = '@ShortDateFormat';
  PARAM_SHOW_COMMON_NAMES            = '@ShowCommonNames';
  PARAM_SHOW_ORIGINAL_SPECIMEN_NAMES = '@ShowOriginalSpecimenNames';

//==============================================================================
function dmGeneral: TdmGeneral;
begin
  if not Assigned(mdmGeneral) then begin
    mInAccessorFunction := True;
    try
      mdmGeneral := TdmGeneral.Create(nil);
    finally
      mInAccessorFunction := False;
    end;
    // Don't link appsettings and GeneralData in the constructor, otherwise they
    // create each other and enter a loop
    mdmGeneral.Connection.ConnectionObject := AppSettings.Connection As _Connection;
  end;
  Result := mdmGeneral;
end;

{-==============================================================================
    TdmGeneral
===============================================================================}
{-------------------------------------------------------------------------------
  Object initialisation.  This constructor can only be called from the global accessor
      function so that the object reference can be stored in the mGeneralData variable.
}
constructor TdmGeneral.Create(AOwner: TComponent);
begin
  // This constructor must only be called from within the accessor function
  if not mInAccessorFunction then
    raise EGeneralDataException.Create(Format(ResStr_InvalidMethodCall,
        ['TdmGeneral.Create']));
  inherited;
  FAllowAllDomains := False;
end;  // TdmGeneral.Create 

{-------------------------------------------------------------------------------
}
constructor TdmGeneral.CreateStandalone(AOwner: TComponent; const AConnectionString: String);
begin
  mdmGeneral := inherited Create(AOwner);
  mdmGeneral.OpenConnection(AConnectionString);
  FAllowAllDomains := False;
end;  // TdmGeneral.CreateStandalone 

{-------------------------------------------------------------------------------
}
class function TdmGeneral.Allocated: Boolean;
begin
  Result := Assigned(mdmGeneral);
end;  // TdmGeneral.Allocated 

{-------------------------------------------------------------------------------
}
class procedure TdmGeneral.Discard;
begin
  mdmGeneral.Free;
  mdmGeneral := nil;
end;  // TdmGeneral.Discard 

{-------------------------------------------------------------------------------
  Retrieve the domain mask, overrriding with a mask for all domains when running in the
      Thesaurus Editor.
}
function TdmGeneral.GetDomainMask: Integer;
begin
  if AllowAllDomains then
    Result := $FFFF
  else
    Result := Integer(AppSettings.DomainMask);
end;  // TdmGeneral.GetDomainMask 

{-------------------------------------------------------------------------------
}
function TdmGeneral.GetRecorderRunning: Boolean;
var
  lHandle: THandle;
begin
  lHandle:=OpenMutex(MUTEX_ALL_ACCESS, false, 'Recorder 2000');
  Result := lHandle <> 0;
  CloseHandle(lHandle);
end;  // TdmGeneral.GetRecorderRunning

{-------------------------------------------------------------------------------
}
function TdmGeneral.GetVagueDateStringFromRecordset(ARecordset: _Recordset; const ASpecifier:
    String = ''): String;
var
  lVagueDate: TVagueDate;
  lPrefix: String;
begin
  // if a specified supplied, it must have an underscore after it
  if ASpecifier = '' then
    lPrefix := ''
  else
    lPrefix := ASpecifier + '_';
  
  with ARecordset do
    if VarIsNull(Fields[lPrefix + VAGUE_DATE_TYPE].Value) then
      Result := ''
    else begin
      if not VarIsNull(Fields[lPrefix + VAGUE_DATE_START].Value) then
        lVagueDate.StartDate := Fields[lPrefix + VAGUE_DATE_START].Value;
      if not VarIsNull(Fields[lPrefix + VAGUE_DATE_END].Value) then
        lVagueDate.EndDate := Fields[lPrefix + VAGUE_DATE_END].Value;
      lVagueDate.DateTypeString := Fields[lPrefix + VAGUE_DATE_TYPE].Value;
      Result := VagueDateToString(lVagueDate);
    end;
end;  // TdmGeneral.GetVagueDateStringFromRecordset 

{-------------------------------------------------------------------------------
}
procedure TdmGeneral.MapWindowMenuClick(Sender: TObject);
var
  lMap: IAvailableMap;
begin
  lMap := Recorder.CurrentSettings.AvailableMap[TMenuItem(Sender).Tag];
  if Assigned(lMap) then lMap.Display;
end;  // TdmGeneral.MapWindowMenuClick 

{-------------------------------------------------------------------------------
}
function TdmGeneral.Recorder: IRecorder2000;
begin
  if RecorderRunning then
    Result := CreateOLEObject('Recorder2000.AutoApplicationSettings') as IRecorder2000
  else
    raise EGeneralDataException.Create(Format(ResStr_InvalidMethodCall, [
        'TdmGeneral.Recorder']));
end;  // TdmGeneral.Recorder

{-------------------------------------------------------------------------------
  Accessor method.  When true, domain mask for user is overriden allowing all domains to be
      accessed.
}
procedure TdmGeneral.SetAllowAllDomains(Value: Boolean);
begin
  FAllowAllDomains := Value;
end;  // TdmGeneral.SetAllowAllDomains 

{-------------------------------------------------------------------------------
  Method to populate the parameters collection of a command object with the values given in
      the AParams array.
}
procedure TdmGeneral.SetupCommandParameters(AParameters: TParameters; AParams: Array of
    Variant);
var
  lParam: TParameter;
begin
  if AParameters.Count > 0 then begin
    // Deal with default parameters
    lParam := AParameters.FindParam(PARAM_SESSION_ID);
    if Assigned(lParam) then lParam.Value := AppSettings.SessionID;
    lParam := AParameters.FindParam(PARAM_USER_ID);
    if Assigned(lParam) then lParam.Value := AppSettings.UserID;
    lParam := AParameters.FindParam(PARAM_USER_DOMAIN_MASK);
    if Assigned(lParam) then lParam.Value := GetDomainMask;
    lParam := AParameters.FindParam(PARAM_SHOW_COMMON_NAMES);
    if Assigned(lParam) then lParam.Value := AppSettings.DisplayCommonNames;
    lParam := AParameters.FindParam(PARAM_SHOW_ORIGINAL_SPECIMEN_NAMES);
    if Assigned(lParam) then
        lParam.Value := AppSettings.UseOriginalSpecimenNames;
    lParam := AParameters.FindParam(PARAM_SHORT_DATE_FORMAT);
    if Assigned(lParam) then lParam.Value := ShortDateFormat;
    lParam := AParameters.FindParam(PARAM_RECORDS_AFFECTED);
    if Assigned(lParam) then lParam.Direction := pdOutput;
    SetVagueDateNulls(AParams);
    inherited;
  end;
end;  // TdmGeneral.SetupCommandParameters 

{-------------------------------------------------------------------------------
  Scan parameters sent to stored procedures.  If vague date type parameters are found, then
      ensure that the fields that should be null are set correctly.  For example, for Y- type
      dates the End date field should be null.
}
procedure TdmGeneral.SetVagueDateNulls(var AParams: Array of Variant);
var
  lIdx: Integer;
  lParamPrefix: String;
  lType: String;
  
  // Search for a parameter, and null its associated value
  procedure NullParam(const AName: string);
  var
    lNullIdx: integer;
  begin
    lNullIdx := 0;
    while lNullIdx <= High(AParams) do begin
      if VarIsStr(AParams[lNullIdx]) then
        if CompareText(AName, AParams[lNullIdx])=0 then
          AParams[lNullIdx+1] := null;
      Inc(lNullIdx, 2);
    end;
  end;
  
begin
  lIdx := 0;
  while lIdx <= High(AParams) do begin
    if CompareText(Copy(AParams[lIdx], Length(AParams[lIdx])- 12, 13),
        'VagueDateType')=0 then begin
      lParamPrefix := Copy(AParams[lIdx], 1, Length(AParams[lIdx])-4);
      // Found a vague date parameter.  Ensure that fields that should be null are
      lType := VarToStr(AParams[lIdx+1]);
      if Length(lType) = 2 then begin
        if lType[1] = '-' then
          NullParam(lParamPrefix + 'Start')
        else if lType[2] = '-' then
          NullParam(lParamPrefix + 'End');
      end
      else if VarIsNull(lType) then begin
        NullParam(lParamPrefix + 'Start');
        NullParam(lParamPrefix + 'End');
      end
      else if (lType='U') or (lType='') then begin
        NullParam(lParamPrefix + 'Start');
        NullParam(lParamPrefix + 'End');
      end;
    end;
    Inc(lIdx, 2);
  end;
end;  // TdmGeneral.SetVagueDateNulls 

{-------------------------------------------------------------------------------
}
procedure TdmGeneral.UpdateMapMenu(Sender: TObject; AMenuItem: TMenuItem; ASetDefault: Boolean
    = False; AUserClickEvent: TNotifyEvent = nil);
var
  i: Integer;
  lMenu: TMenuItem;
  lClickEvent: TNotifyEvent;
begin
  if Assigned(AUserClickEvent) then lClickEvent := AUserClickEvent
                               else lClickEvent := MapWindowMenuClick;
  AMenuItem.Clear;
  // Add as many items as there are initialised maps.
  for i := 0 to Recorder.CurrentSettings.AvailableMapCount - 1 do begin
    lMenu := TMenuItem.Create(AMenuItem);
    // Skip the "Default" marker
    lMenu.Caption    := Recorder.CurrentSettings.AvailableMap[i].Title;
    lMenu.ImageIndex := 20;
    lMenu.Tag        := i;
    lMenu.Default    := ASetDefault and Recorder.CurrentSettings.AvailableMap[i].IsDefault;
    lMenu.OnClick    := lClickEvent;
    AMenuItem.Add(lMenu);
  end;
  AMenuItem.Enabled := AMenuItem.Count > 0;
end;  // TdmGeneral.UpdateMapMenu

//==============================================================================
initialization
  mdmGeneral := nil;
  mInAccessorFunction := False;
end.




