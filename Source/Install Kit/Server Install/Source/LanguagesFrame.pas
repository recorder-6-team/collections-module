{===============================================================================
  Unit:        LanguagesFrame

  Defines:     TfraLanguages

  Description: Languages selection

  Created:     September 2003

  Last revision information:
    $Revision: 2 $
    $Date: 23/03/04 10:11 $
    $Author: Ericsalmon $

===============================================================================}

unit LanguagesFrame;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, BaseStepFrame, StdCtrls, Buttons, CheckLst, Settings, XPMenu;

type
  TfraLanguages = class (TfraBaseStep)
    clbLanguages: TCheckListBox;
    btnMoveUp: TBitBtn;
    btnMoveDown: TBitBtn;
    Label2: TLabel;
    Label3: TLabel;
    procedure btnMoveDownClick(Sender: TObject);
    procedure btnMoveUpClick(Sender: TObject);
    procedure clbLanguagesClick(Sender: TObject);
  private
    FXPMenu: TXPMenu;
    procedure MoveLanguage(ADirection: Integer);
  protected
    function GetHasNext: Boolean; override;
    function GetHasPrevious: Boolean; override;
    function GetNext: TStepFrameClass; override;
    function GetPrevious: TStepFrameClass; override;
    procedure LoadContent; override;
    procedure SaveContent; override;
  public
    constructor Create(AOwner: TComponent; ASettings: TSettings); override;
    destructor Destroy; override;
  end;
  
//==============================================================================
implementation

{$R *.dfm}

uses
  BarcodeFrame, JobSeedFrame;

{-==============================================================================
    TfraLanguages
===============================================================================}
{-------------------------------------------------------------------------------
}
constructor TfraLanguages.Create(AOwner: TComponent; ASettings: TSettings);
begin
  inherited;
  FXPMenu := TXPMenu.Create(Owner);
  FXPMenu.XPControls := [xcButton, xcBitBtn];
  FXPMenu.Active := True;
end;  // TfraBaseStep.Create

{-------------------------------------------------------------------------------
}
destructor TfraLanguages.Destroy;
begin
  FXPMenu.Active := False; // Seems to stop annoying random access violation.
  FXPMenu.Free;
  inherited;
end;  // TfraBaseStep.Destroy

{-------------------------------------------------------------------------------
}
procedure TfraLanguages.btnMoveDownClick(Sender: TObject);
begin
  inherited;
  MoveLanguage(1);
end;  // TfraLanguages.btnMoveDownClick 

{-------------------------------------------------------------------------------
}
procedure TfraLanguages.btnMoveUpClick(Sender: TObject);
begin
  inherited;
  MoveLanguage(-1);
end;  // TfraLanguages.btnMoveUpClick 

{-------------------------------------------------------------------------------
}
procedure TfraLanguages.clbLanguagesClick(Sender: TObject);
begin
  inherited;
  MoveLanguage(0);
end;  // TfraLanguages.clbLanguagesClick 

{-------------------------------------------------------------------------------
}
function TfraLanguages.GetHasNext: Boolean;
begin
  Result := True;
end;  // TfraLanguages.GetHasNext 

{-------------------------------------------------------------------------------
}
function TfraLanguages.GetHasPrevious: Boolean;
begin
  Result := True;
end;  // TfraLanguages.GetHasPrevious 

{-------------------------------------------------------------------------------
}
function TfraLanguages.GetNext: TStepFrameClass;
begin
  Result := TfraJobSeed;
end;  // TfraLanguages.GetNext 

{-------------------------------------------------------------------------------
}
function TfraLanguages.GetPrevious: TStepFrameClass;
begin
  Result := TfraBarcode;
end;  // TfraLanguages.GetPrevious 

{-------------------------------------------------------------------------------
}
procedure TfraLanguages.LoadContent;
var
  i: Integer;
begin
  inherited;
  with clbLanguages do
    for i := 0 to Settings.Languages.Count - 1 do begin
      ItemIndex := Items.IndexOf(Settings.Languages[i]);
      if ItemIndex <> -1 then begin
        Checked[ItemIndex] := True;
        while ItemIndex <> i do MoveLanguage(-1);
      end;
    end;
  clbLanguages.ItemIndex := 0;
  MoveLanguage(0);
end;  // TfraLanguages.LoadContent 

{-------------------------------------------------------------------------------
}
procedure TfraLanguages.MoveLanguage(ADirection: Integer);
begin
  with clbLanguages do begin
    Items.Exchange(ItemIndex, ItemIndex + ADirection);
    btnMoveUp.Enabled := ItemIndex > 0;
    btnMoveDown.Enabled := ItemIndex < Count - 1;
  end;
end;  // TfraLanguages.MoveLanguage 

{-------------------------------------------------------------------------------
}
procedure TfraLanguages.SaveContent;
var
  i: Integer;
begin
  inherited;
  Settings.Languages.Clear;
  with clbLanguages do
    for i := 0 to Items.Count - 1 do
      if Checked[i] then Settings.Languages.Add(Items[i]);
end;  // TfraLanguages.SaveContent 

end.

