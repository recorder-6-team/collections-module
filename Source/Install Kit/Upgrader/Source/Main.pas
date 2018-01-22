{===============================================================================
  Unit:         Main

  Defines:      TfrmMain

  Description:  Progress splahs screen for Recorder database upgrades.

  Created:      29/5/2003

  Last revision information:
    $Revision: 1 $
    $Date: 27/08/04 12:50 $
    $Author: Johnvanbreda $

===============================================================================}
unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, DBUpgrader, Settings, LoginFrame, Buttons,
  BaseFrameUnit, GeneralFunctions, XPMan, ExtCtrls;

type
  {-----------------------------------------------------------------------------
    Upgrader main form.
    Each step of the process is displayed by embedding a frame onto the dialog.
  }
  TfrmMain = class (TForm)
    btnCancel: TBitBtn;
    btnProceed: TBitBtn;
    imgBackdrop: TImage;
    XPManifest: TXPManifest;
    procedure btnCancelClick(Sender: TObject);
    procedure btnProceedClick(Sender: TObject);
  private
    FCurrentFrame: TBaseFrame;
    FSettings: TSettings;
    procedure EmbedCurrentFrame;
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
    property Settings: TSettings read FSettings;
  end;
  
var
  frmMain: TfrmMain;

//==============================================================================
implementation

{$R *.dfm}

{-==============================================================================
    TfrmMain
===============================================================================}
{-------------------------------------------------------------------------------
  Initialises the form.  Embeds the first frame onto the form (the login frame). 
}
constructor TfrmMain.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  
  FSettings := TSettings.Create;
  FCurrentFrame := TfraLogin.Create(Self);
  EmbedCurrentFrame;
  FCurrentFrame.ApplySettings(FSettings);
end;  // TfrmMain.Create 

{-------------------------------------------------------------------------------
  Frees owned objects. 
}
destructor TfrmMain.Destroy;
begin
  FSettings.Free;
  
  inherited Destroy;
end;  // TfrmMain.Destroy 

{-------------------------------------------------------------------------------
}
procedure TfrmMain.btnCancelClick(Sender: TObject);
begin
  Close;
end;  // TfrmMain.btnCancelClick 

{-------------------------------------------------------------------------------
}
procedure TfrmMain.btnProceedClick(Sender: TObject);
var
  lLastFrame: TBaseFrame;
begin
  btnProceed.Enabled := False;
  btnCancel.Enabled := False;
  // apply the settings and move to the next frame
  FCurrentFrame.ApplySettings(FSettings);
  lLastFrame := FCurrentFrame;
  FCurrentFrame := lLastFrame.CreateNextFrame(Self);
  lLastFrame.Free;
  if Assigned(FCurrentFrame) then
    EmbedCurrentFrame
  else begin
    ShowInformation('Recorder upgrade complete');
    Close;
  end;
end;  // TfrmMain.btnProceedClick 

{-------------------------------------------------------------------------------
  Embeds the current frame onto the form and calls its execute method. 
}
procedure TfrmMain.EmbedCurrentFrame;
begin
  with FCurrentFrame do begin
    Parent := Self;
    SetBounds(160, 8, 332, 297);
    Execute(FSettings);
  end;
end;  // TfrmMain.EmbedCurrentFrame 

end.

