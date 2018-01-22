{===============================================================================
  Unit:        UpgradeFrame

  Defines:     TfraUpgrade

  Description: Progress splash screen for Recorder database upgrades.

  Created:

  Last revision information:
    $Revision: 2 $
    $Date: 18/10/05 15:36 $
    $Author: Johnvanbreda $

===============================================================================}
unit UpgradeFrame;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, StdCtrls, Settings, BaseFrameUnit, ExtCtrls;

type
  TfraUpgrade = class (TBaseFrame)
    lblWait: TLabel;
    pbProgress: TProgressBar;
  private
    procedure ProcessDatabase(ASettings: TSettings);
  public
    procedure Execute(ASettings: TSettings); override;
  end;
  
//==============================================================================

implementation

uses
  Main, DBUpgrader, FileUpgrader, APIUtils;

{$R *.dfm}

{-==============================================================================
    TfraUpgrade
===============================================================================}
{-------------------------------------------------------------------------------
}
procedure TfraUpgrade.Execute(ASettings: TSettings);
begin
  Repaint;
  // Proceed with Database.
  lblWait.Caption := 'Please wait, upgrading Collections database...';
  Application.ProcessMessages;
  ProcessDatabase(ASettings);
  frmMain.btnProceed.Click;
end;  // TfraUpgrade.Execute

{-------------------------------------------------------------------------------
  Performs the script by script upgrade of the database 
}
procedure TfraUpgrade.ProcessDatabase(ASettings: TSettings);
begin
  with TDBUpgrader.Create(ASettings) do
    try
      RunScripts(pbProgress);
    finally
      Free;
    end;
end;  // TfraUpgrade.ProcessDatabase 


end.



