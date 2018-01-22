{===============================================================================
  Unit:        LoginFrame

  Defines:     TfraLogin

  Description: Login details for the upgrade process

  Created:

  Last revision information:
    $Revision: 1 $
    $Date: 27/08/04 12:50 $
    $Author: Johnvanbreda $

===============================================================================}
unit LoginFrame;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, UpgradeFrame, BaseFrameUnit, Settings;

type
  {-----------------------------------------------------------------------------
    Frame containing user controls to obtain the user's database login.
  }
  TfraLogin = class (TBaseFrame)
    ePassword: TEdit;
    eUsername: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label5: TLabel;
    rbSQL: TRadioButton;
    rbTrusted: TRadioButton;
    procedure rbSQLClick(Sender: TObject);
  public
    procedure ApplySettings(ASettings: TSettings); override;
    function CreateNextFrame(AOwner: TComponent): TBaseFrame; override;
  end;
  
//==============================================================================

implementation

{$R *.dfm}

{-==============================================================================
    TfraLogin
===============================================================================}
{-------------------------------------------------------------------------------
  Applies the user's login information to the Settings object. 
}
procedure TfraLogin.ApplySettings(ASettings: TSettings);
begin
  ASettings.TrustedConnection := rbTrusted.Checked;
  if not rbTrusted.Checked then begin
    ASettings.Username := eUsername.Text;
    ASettings.Password := ePassword.Text;
  end;
  rbSQLClick(nil);
end;  // TfraLogin.ApplySettings 

{-------------------------------------------------------------------------------
}
function TfraLogin.CreateNextFrame(AOwner: TComponent): TBaseFrame;
begin
  Result := TfraUpgrade.Create(AOwner);
end;  // TfraLogin.CreateNextFrame 

{-------------------------------------------------------------------------------
}
procedure TfraLogin.rbSQLClick(Sender: TObject);
begin
  eUsername.Enabled := rbSQL.Checked;
  ePassword.Enabled := rbSQL.Checked;
end;  // TfraLogin.rbSQLClick 

end.
