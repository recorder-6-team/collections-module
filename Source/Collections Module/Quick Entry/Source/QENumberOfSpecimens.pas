{===============================================================================
  Unit:        QENumberOfSpecimens.pas

  Defines:     TdlgQENumberOfSpecimens

  Description: Dialog that allows the user to specify the number of specimens
                that are to be created in a new Quick Entry session that has
                been created from an existing observation.

  Created:     January 2011

  Last revision information:
    $Revision: 2 $
    $Date: 26/01/11 16:48 $
    $Author: Robertjohnson $

===============================================================================}

unit QENumberOfSpecimens;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, RestrictedEdits, ImageListButton, ImgList;

type
  TdlgQENumberOfSpecimens = class(TForm)
    lblNumberOfspecimens: TLabel;
    txtNumberOfSpecimens: TNumberEdit;
    btnOk: TImageListButton;
    btnCancel: TImageListButton;
    procedure btnOkClick(Sender: TObject);
  private
    FNumberEntered: Integer;
    FIsValid: Boolean;
    procedure Validate;
  public
    property NumberOfSpecimens: Integer read FNumberEntered;
  end;

var
  dlgQENumberOfSpecimens: TdlgQENumberOfSpecimens;

implementation

uses ResourceStrings;

{$R *.dfm}

{-------------------------------------------------------------------------------
  If there is a valid integer in the text box, then close the form,
  otherwise display a dialog requesting a valid integer.
}
procedure TdlgQENumberOfSpecimens.btnOkClick(Sender: TObject);
begin
  Validate;
  If FIsValid then
    ModalResult := mrOk
  else
    MessageDlg(ResStr_SuppyValidNumber, mtInformation, [mbOK], 0);
end;

{-------------------------------------------------------------------------------
  Sets FIsValid depending on whether the number entered in the text box
  is a valid integer. If so, then sets FNumberEntered.
}
procedure TdlgQENumberOfSpecimens.Validate;
begin
  try
    FNumberEntered := StrToInt(txtNumberOfSpecimens.Text);
    FIsValid := true;
  except on EConvertError do
    FIsValid := false;
  end;
end;

end.
