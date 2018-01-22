unit ServerDetailsFrame;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, BaseStepFrame, StdCtrls, Registry, Settings, ExtCtrls,
  ExceptionForm;

resourcestring
  ResStr_InstanceRequired = 'Please specify the name of the server instance.';
  ResStr_DBRequired = 'Please specify the name of the database.';


type
  EServerDetails = class(TExceptionPath);

  TfraServerDetails = class(TfraBaseStep)
    Label1: TLabel;
    cmbInstances: TComboBox;
    Label2: TLabel;
    eDBName: TEdit;
    Label3: TLabel;
  private
    procedure GetLocalInstanceNames;
  protected
    procedure SaveContent; override;
    procedure LoadContent; override;
  public
    constructor Create(AOwner: TComponent; ASettings: TSettings); override;
    function GetHasNext: Boolean; override;
    function GetNext: TStepFrameClass; override;
    function GetHasPrevious: Boolean; override;
    function GetPrevious: TStepFrameClass; override;
    procedure ValidateContent; override;
  end;

implementation

{$R *.dfm}

uses
  WelcomeFrame, DatabaseFrame;

const
  REG_KEY_MICROSOFT       = '\Software\Microsoft';
  REG_KEY_SQLSERVER       = REG_KEY_MICROSOFT + '\Microsoft SQL Server';
  REG_INSTANCES       = 'InstalledInstances';
  DEFAULT_LOCAL_INSTANCE = 'MSSQLSERVER';

{-------------------------------------------------------------------------------
  Initialise combo box with local instances
}
constructor TfraServerDetails.Create(AOwner: TComponent; ASettings: TSettings);
begin
  inherited;
  GetLocalInstanceNames;
end;

{-------------------------------------------------------------------------------
}
function TfraServerDetails.GetHasNext: Boolean;
begin
  Result := true;
end;

{-------------------------------------------------------------------------------
}
function TfraServerDetails.GetHasPrevious: Boolean;
begin
  Result := true;
end;

{-------------------------------------------------------------------------------
  Retrieve local SQL Server instances into the combo
}
procedure TfraServerDetails.GetLocalInstanceNames;
var
  lValueLen: DWORD;
  lzBuffer, lzName: PChar;
  lComputerName: Array[0..MAX_COMPUTERNAME_LENGTH + 1] of Char;
  lSize: Cardinal;
begin
  lSize := MAX_COMPUTERNAME_LENGTH + 1;
  GetComputerName(lComputerName, lSize);
  with TRegistry.Create do
    try
      RootKey := HKEY_LOCAL_MACHINE;
      // Locate key for local instances
      if OpenKey(REG_KEY_SQLSERVER, False) then
        // Value is multiline, so not just ReadString, or we'll miss the others!
        if ValueExists(REG_INSTANCES) then begin
          // Get size of buffer
          RegQueryValueEx(CurrentKey, PChar(REG_INSTANCES), nil,
                          nil, nil, @lValueLen);
          // Allocate buffer
          GetMem (lzBuffer, lValueLen);
          try
            // Get the instance names
            RegQueryValueEx(CurrentKey, PChar(REG_INSTANCES), nil, nil,
                            PBYTE(lzBuffer), @lValueLen);
            // Need to "walk" through the buffer to get each instance name
            lzName := lzBuffer;
            cmbInstances.Items.Clear;
            while lzName^ <> #0 do begin
              if lzName = DEFAULT_LOCAL_INSTANCE then
                cmbInstances.Items.Add(lComputerName)
              else
                cmbInstances.Items.Add(lComputerName + '\' + lzName);
              Inc(lzName, StrLen(lzName) + 1);
            end;
          finally
            FreeMem(lzBuffer);
          end;
        CloseKey;
      end;
    finally
      Free;
    end;
end;  // TSettings.GetLocalInstanceNames

{-------------------------------------------------------------------------------
}
function TfraServerDetails.GetNext: TStepFrameClass;
begin
  Result := TfraDatabase;
end;

{-------------------------------------------------------------------------------

}
function TfraServerDetails.GetPrevious: TStepFrameClass;
begin
  Result := TfraWelcome;
end;

{-------------------------------------------------------------------------------
}
procedure TfraServerDetails.LoadContent;
begin
  inherited;
  if Settings.DatabaseName <> '' then
    eDBName.Text := Settings.DatabaseName;
  cmbInstances.Text := Settings.ServerName;
end;

{-------------------------------------------------------------------------------
}
procedure TfraServerDetails.SaveContent;
begin
  inherited;
  Settings.DatabaseName := eDBName.Text;
  Settings.ServerName := cmbInstances.Text;
end;  // TfraDatabase.SaveContentsword.Text;

{-------------------------------------------------------------------------------
}
procedure TfraServerDetails.ValidateContent;
begin
  inherited;
  if cmbInstances.Text='' then
    raise EServerDetails.CreateNonCritical(ResStr_InstanceRequired);
  if eDBName.Text='' then
    raise EServerDetails.CreateNonCritical(ResStr_DBRequired);
end;

end.
