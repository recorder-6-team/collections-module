{===============================================================================
  Unit:        CurrentUserEdit

  Defines:     TCurrentUserEdit

  Description: Edit control which, when F11 is pressed, puts the current users
               username in the textbox.

  Created:     October 2012

  Last revision information:
    $Revision: 2 $
    $Date: 24/10/12 14:28 $
    $Author: Alexanderpadley $

===============================================================================}

unit UserEdit;

interface

uses
  ADODB, LinkedControls, Windows, StdCtrls, Controls, Forms, Classes, ApplicationSettings,
  Variants, SysUtils, DataTypes, GeneralData, Dialogs;

type
  TUserEdit = class(TLinkedEdit)
  public
    constructor Create(AOwner: TComponent); override;
    procedure GetCurrentUserF11(Sender: TObject; var KeyPressed: Word; Shift: TShiftState);
  protected
    function GetCurrentUserParams: TVariantArray;
  private
    FCurrentUsername : String;
  end;
procedure Register;
  
//==============================================================================
implementation

constructor TUserEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  OnKeyDown := GetCurrentUserF11;
end;  // TCurrentUserEdit.Create

procedure TUserEdit.GetCurrentUserF11(
  Sender: TObject;
  var KeyPressed: Word;
  Shift: TShiftState);
var
  recordset : _Recordset;
begin
  if (KeyPressed = VK_F11) and (not EditBox.ReadOnly) then
  begin
    Key := AppSettings.UserID;
    if (FCurrentUsername = Null) or (FCurrentUsername = '') then
    begin
      recordset := dmGeneral.GetRecordset('usp_User_Select', GetCurrentUserParams);
      FCurrentUsername := VarToStr(recordset.Fields['User_Name'].Value);
    end;
    Text := FCurrentUsername;
  end;
end;

function TUserEdit.GetCurrentUserParams: TVariantArray;
begin
  Result := VarArrayOf(['@NameKey', AppSettings.UserID]);
end;  // TfraSpecimenGeneral.GetCurrentUserParams

procedure Register;
begin
  RegisterComponents('UserEdit', [TUserEdit]);
end;

end.

