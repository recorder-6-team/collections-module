unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, XPMenu;

resourcestring
  ResStr_Finish = 'Finish';

const
  REG_ADDIN_PATH = '\Software\Dorset Software\Recorder 6\Installed Addins';

type
  TfrmMain = class(TForm)
    imgBack: TImage;
    btnProceed: TButton;
    btnCancel: TButton;
    procedure btnProceedClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
  private
    FCurrentFrame: TFrame;
    FXPMenu: TXPMenu;
    procedure PositionCurrentFrame;
  public
    constructor Create(AOwner: TComponent); override;
  end;

var
  frmMain: TfrmMain;

//==============================================================================
implementation

{$R *.dfm}

uses
  WelcomeFrame, InstallFrame, CompleteFrame, Registry, GeneralFunctions;

constructor TfrmMain.Create(AOwner: TComponent);
begin
  inherited;
  FXPMenu := TXPMenu.Create(self);
  FXPMenu.XPControls := [xcButton];
  FXPMenu.Active := True;
  FCurrentFrame := TfraWelcome.Create(Self);
  PositionCurrentFrame;
end;

procedure TfrmMain.PositionCurrentFrame;
var
  lIdx: integer;
begin
  for lIdx := FCurrentFrame.ControlCount-1 downto 0 do
    with FCurrentFrame.Controls[lIdx] do begin
      Left := Left + 164;
      Top := Top + 4;
      Parent := Self;
    end;
end;

procedure TfrmMain.btnProceedClick(Sender: TObject);
begin
  btnCancel.Enabled := False;
  if FCurrentFrame is TfraComplete then
    Close
  else begin
    FCurrentFrame.Free;
    FCurrentFrame := TfraInstall.Create(Self);
    PositionCurrentFrame;
    btnProceed.Enabled := False;
    TfraInstall(FCurrentFrame).DoInstallation;
    FCurrentFrame.Free;
    FCurrentFrame := TfraComplete.Create(self);
    PositionCurrentFrame;

    btnCancel.Enabled := True;
    btnCancel.Caption := ResStr_Finish;
    btnCancel.SetFocus;
  end;
end;


procedure TfrmMain.btnCancelClick(Sender: TObject);
begin
  Close;
end;

end.
