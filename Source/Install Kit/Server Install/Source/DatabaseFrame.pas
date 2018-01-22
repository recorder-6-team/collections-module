{===============================================================================
  Unit:        DatabaseFrame

  Defines:     TfraDatabase

  Description: Installs the Collections database in existing NBNData database.

  Created:     September 2003

  Last revision information:
    $Revision: 4 $
    $Date: 14/05/07 16:42 $
    $Author: Johnvanbreda $

===============================================================================}

unit DatabaseFrame;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, BaseStepFrame, StdCtrls, ExceptionForm, ExtCtrls;

type
  EConnectError = class(TExceptionPath);

  TfraDatabase = class (TfraBaseStep)
    Label14: TLabel;
    Label15: TLabel;
    Label3: TLabel;
    rbSQLLogin: TRadioButton;
    Label16: TLabel;
    Label17: TLabel;
    ePassword: TEdit;
    eUsername: TEdit;
    rbTrustedLogin: TRadioButton;
    Label1: TLabel;
    lblSALogin: TLabel;
    lblNTLogin: TLabel;
    procedure ContentChange(Sender: TObject);
    procedure lblSALoginClick(Sender: TObject);
    procedure lblNTLoginClick(Sender: TObject);
    procedure rbSQLLoginEnter(Sender: TObject);
  protected
    function GetHasNext: Boolean; override;
    function GetHasPrevious: Boolean; override;
    function GetNext: TStepFrameClass; override;
    function GetNextCaption: String; override;
    function GetPrevious: TStepFrameClass; override;
    procedure LoadContent; override;
    procedure SaveContent; override;
  public
    procedure ValidateContent; override;
  end;

//==============================================================================
implementation

{$R *.dfm}

uses
  Settings, WelcomeFrame, ServerDetailsFrame, DatabaseInstallFrame;

const
  // Connection string for main SQL Server DB
  CONNECTION_STRING = 'Provider=SQLOLEDB.1;%sPersist Security Info=False;' +
                      'Data Source=%s;Initial Catalog=%s;OLE DB Services=-2';
  // OLE DB Services=-2 turns off connection pooling which can cause confusing errors
  // when sp_SetAppRole is used.
  INTEGRATED_SECURITY = 'Integrated Security=SSPI;';
  SQL_SECURITY = 'User ID=%s;password=%s;';

resourcestring
  ResStr_ConnectionFailedSA =
    'Unable to establish a connection to the server.'#13 +
    'Please ensure the username and password are valid.';

  ResStr_ConnectionFailedTrusted =
    'Unable to establish a connection to the server.'#13 +
    'Please ensure you are authorised access to the server.';

{-==============================================================================
    TfraDatabase
===============================================================================}
{-------------------------------------------------------------------------------
}
procedure TfraDatabase.ContentChange(Sender: TObject);
begin
  inherited;
  if (Sender = eUsername) or (Sender = ePassword) then rbSQLLogin.Checked := True;
  if Sender = rbSQLLogin then eUserName.SetFocus;
  ChangedContent;
end;  // TfraDatabase.ContentChange 

{-------------------------------------------------------------------------------
}
function TfraDatabase.GetHasNext: Boolean;
begin
  Result := rbTrustedLogin.Checked or (eUsername.Text <> '');
end;  // TfraDatabase.GetHasNext 

{-------------------------------------------------------------------------------
}
function TfraDatabase.GetHasPrevious: Boolean;
begin
  Result := True;
end;  // TfraDatabase.GetHasPrevious 

{-------------------------------------------------------------------------------
}
function TfraDatabase.GetNext: TStepFrameClass;
begin
  Result := TfraDatabaseInstall;
end;  // TfraDatabase.GetNext 

{-------------------------------------------------------------------------------
}
function TfraDatabase.GetNextCaption: String;
begin
  Result := '&Install';
end;  // TfraDatabase.GetNextCaption

{-------------------------------------------------------------------------------
}
function TfraDatabase.GetPrevious: TStepFrameClass;
begin
  if Settings.NeedServerDetails then
    Result := TfraServerDetails
  else
    Result := TfraWelcome;
end;  // TfraDatabase.GetPrevious

{-------------------------------------------------------------------------------
}
procedure TfraDatabase.lblNTLoginClick(Sender: TObject);
begin
  inherited;
  rbTrustedLogin.SetFocus;
  rbTrustedLogin.Checked := True;
  ChangedContent;
end;  // TfraDatabase.lblNTLoginClick

{-------------------------------------------------------------------------------
}
procedure TfraDatabase.lblSALoginClick(Sender: TObject);
begin
  inherited;
  rbSQLLogin.Checked := True;
  ChangedContent;
end;  // TfraDatabase.lblSALoginClick

{-------------------------------------------------------------------------------
}
procedure TfraDatabase.LoadContent;
begin
  inherited;
  // Text boxes first, because of ContentChange method.
  eUsername.Text := Settings.SAUsername;
  ePassword.Text := Settings.SAPassword;
  rbTrustedLogin.Checked := Settings.UseTrusted;
  rbSQLLogin.Checked := not Settings.UseTrusted;
end;  // TfraDatabase.LoadContent

{-------------------------------------------------------------------------------
}
procedure TfraDatabase.rbSQLLoginEnter(Sender: TObject);
begin
  inherited;
  if rbSQLLogin.Checked then eUserName.SetFocus;
end;  // TfraDatabase.rbSQLLoginEnter

{-------------------------------------------------------------------------------
}
procedure TfraDatabase.SaveContent;
begin
  inherited;
  Settings.UseTrusted := rbTrustedLogin.Checked;
  Settings.SAUsername := eUsername.Text;
  Settings.SAPassword := ePassword.Text;
end;  // TfraDatabase.SaveContent

{-------------------------------------------------------------------------------
}
procedure TfraDatabase.ValidateContent;
var lSecurity: String;
begin
  try
    if rbTrustedLogin.Checked then
      lSecurity := INTEGRATED_SECURITY
    else
      lSecurity := Format(SQL_SECURITY, [eUserName.Text, ePassword.Text]);

    Settings.EstablishConnection(Format(CONNECTION_STRING,
                                        [lSecurity,
                                         Settings.ServerName,
                                         Settings.DatabaseName]));
  except
    if rbTrustedLogin.Checked then
      raise EConnectError.CreateNonCritical(ResStr_ConnectionFailedTrusted)
    else
      raise EConnectError.CreateNonCritical(ResStr_ConnectionFailedSA);
  end;
end;  // TfraDatabase.ValidateContent


end.
