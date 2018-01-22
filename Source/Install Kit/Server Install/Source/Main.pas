{===============================================================================
  Unit:        Main

  Defines:     TfrmMain
                                                   
  Description: Wizard for Collection Addin install.

  Created:     September 2003

  Last revision information:
    $Revision: 7 $
    $Date: 3/10/08 16:17 $
    $Author: Ericsalmon $

===============================================================================}

unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, BaseStepFrame, Settings, XPMenu, Buttons;

type
  TfrmMain = class (TForm)
    btnBack: TButton;
    btnCancel: TButton;
    btnNext: TButton;
    imgBack: TImage;
    procedure btnBackClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure btnNextClick(Sender: TObject);
  private
    FCurrentStep: TfraBaseStep;
    FSettings: TSettings;
    FXPMenu: TXPMenu;
    procedure ChangeStep(AFrameClass: TStepFrameClass);
    procedure ReadRegistrySettings;
    procedure StepContentChanged(Sender: TObject);
    procedure SynchronizeNavigationButtons;
    procedure PositionCurrentStep;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  frmMain: TfrmMain;

//==============================================================================
implementation

{$R *.dfm}

uses
  Registry, WelcomeFrame;

{-==============================================================================
    TfrmMain
===============================================================================}
{-------------------------------------------------------------------------------
}
constructor TfrmMain.Create(AOwner: TComponent);
var
  lDrive: String;
begin
  inherited;
  lDrive := ExtractFileDrive(Application.ExeName);

  if not (GetDriveType(PChar(lDrive)) in [DRIVE_REMOVABLE, DRIVE_FIXED, DRIVE_CDROM]) then
  begin
    MessageDlg('The Server install program for Luxembourg NHM Collections addin for Recorder 6 ' +
               'must be run from a local drive.'#13#13'Please see the help file for more information.',
               mtInformation, [mbOk], 0);
    Application.Terminate;
    Exit;
  end;

  FXPMenu := TXPMenu.Create(Self);
  FXPMenu.XPControls := [xcButton, xcBitBtn];
  FXPMenu.Active := True;

  FSettings := TSettings.Create;
  ReadRegistrySettings;
  ChangeStep(TfraWelcome);
end;  // TfrmMain.Create

{-------------------------------------------------------------------------------
}
destructor TfrmMain.Destroy;
begin
  FreeAndNil(FCurrentStep);
  FSettings.Free;
  inherited;
end;  // TfrmMain.Destroy 

{-------------------------------------------------------------------------------
}
procedure TfrmMain.btnBackClick(Sender: TObject);
begin
  ChangeStep(FCurrentStep.Previous);
end;  // TfrmMain.btnBackClick 

{-------------------------------------------------------------------------------
}
procedure TfrmMain.btnCancelClick(Sender: TObject);
begin
  Close;
end;  // TfrmMain.btnCancelClick

{-------------------------------------------------------------------------------
}
procedure TfrmMain.btnNextClick(Sender: TObject);
begin
  if FCurrentStep.IsFinal then
    Close
  else begin
    FCurrentStep.ValidateContent;
    ChangeStep(FCurrentStep.Next);
  end;
end;  // TfrmMain.btnNextClick

{-------------------------------------------------------------------------------
}
procedure TfrmMain.ChangeStep(AFrameClass: TStepFrameClass);
begin
  if Assigned(FCurrentStep) then FreeAndNil(FCurrentStep);

  FCurrentStep := AFrameClass.Create(Self, FSettings);
  PositionCurrentStep;
  FCurrentStep.OnChangedContent := StepContentChanged;
  SynchronizeNavigationButtons;
  // Place the navigation buttons at the beginning of the tab sequence.
  // The reason is because of the frame's controls are moved to the main form with their
  // tab order relative to the frame.
  btnCancel.TabOrder := 0;
  btnNext.TabOrder := 0;
  btnBack.TabOrder := 0;
  // And then focus on the first focusable control after the Cancel button.
  if btnBack.Enabled then  // Welcome page has no focusable controls AND Back button disabled.
    SelectNext(btnCancel, True, True);

  Refresh;
  if FCurrentStep.Execute then ChangeStep(FCurrentStep.Next);
end;  // TfrmMain.ChangeStep

{-------------------------------------------------------------------------------
}
procedure TfrmMain.PositionCurrentStep;
var
  i: Integer;
begin
  for i := 0 to FCurrentStep.ControlCount - 1 do begin
    // Before the control "disappears" from the frame.
    if FCurrentStep.Controls[0] is TLabel then
      TLabel(FCurrentStep.Controls[0]).Transparent := True;
    // Now move it.
    with FCurrentStep.Controls[0] do begin
      Left := Left + Self.ClientWidth - FCurrentStep.Width - 4;
      Top := Top + 8;
      Parent := Self;
    end;
  end;
end;  // TfrmMain.PositionCurrentStep

{-------------------------------------------------------------------------------
}
procedure TfrmMain.ReadRegistrySettings;
begin
  with TRegistry.Create do
    try
      RootKey := HKEY_LOCAL_MACHINE;
      if OpenKey('Software\Dorset Software\Recorder 6', False) then begin
        FSettings.DatabaseName := ReadString('Database Name');
        FSettings.ServerName := ReadString('Server Name');
        FSettings.InstallationPath := IncludeTrailingPathDelimiter(ReadString('Installation Path'));
      end else begin
        FSettings.NeedServerDetails := true;
      end;
    finally
      Free;
    end;
end;  // TfrmMain.ReadRegistrySettings 

{-------------------------------------------------------------------------------
}
procedure TfrmMain.StepContentChanged(Sender: TObject);
begin
  SynchronizeNavigationButtons;
end;  // TfrmMain.StepContentChanged 

{-------------------------------------------------------------------------------
}
procedure TfrmMain.SynchronizeNavigationButtons;
begin
  btnNext.Enabled   := FCurrentStep.HasNext;
  btnBack.Enabled   := FCurrentStep.HasPrevious;
  btnNext.Caption   := FCurrentStep.NextCaption;
  btnCancel.Caption := FCurrentStep.FinalCaption;  // Cancel or Finish button.
  if FCurrentStep.IsFinal then btnCancel.SetFocus;
end;  // TfrmMain.SynchronizeNavigationButtons


end.

