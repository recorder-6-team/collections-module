{===============================================================================
  Unit:        SpecimenRelocation

  Defines:     TdlgSpecimenRelocation

  Description: Gives the user the option for a specimen to be permanently or only
               temporarily moved to a different location.

  Created:     September 2008

  Last revision information:
    $Revision: 3 $
    $Date: 2/10/08 10:31 $
    $Author: Ericsalmon $

===============================================================================}
unit SpecimenRelocation;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, ImageListButton;

resourcestring
  ResStr_RelocationInfo = 'Select the type of relocation you wish to apply to the specimen ''%s'':';

type
  TdlgSpecimenRelocation = class(TForm)
    btnOK: TImageListButton;
    btnCancel: TImageListButton;
    Bevel1: TBevel;
    chkTemporary: TRadioButton;
    chkPermanent: TRadioButton;
    Label1: TLabel;
    Label2: TLabel;
    lblRelocationInfo: TLabel;
    btnApplyToAll: TImageListButton;
    procedure btnApplyToAllClick(Sender: TObject);
  private
    FApplyToAll: Boolean;
    procedure SetRelocationInfo(const Value: String);
  public
    procedure HideApplyToAllOption;
    property ApplyToAll: Boolean read FApplyToAll;
    property RelocationInfo: String write SetRelocationInfo;
  end;

//==============================================================================
implementation

{$R *.dfm}

{-------------------------------------------------------------------------------
}
procedure TdlgSpecimenRelocation.btnApplyToAllClick(Sender: TObject);
begin
  FApplyToAll := True;
end;

{-------------------------------------------------------------------------------
}
procedure TdlgSpecimenRelocation.HideApplyToAllOption;
begin
  btnOk.Left := btnApplyToAll.Left;
  btnApplyToAll.Visible := False;
end;

{-------------------------------------------------------------------------------
}
procedure TdlgSpecimenRelocation.SetRelocationInfo(const Value: String);
begin
  lblRelocationInfo.Caption := Format(ResStr_RelocationInfo, [Value]);
end;

end.
