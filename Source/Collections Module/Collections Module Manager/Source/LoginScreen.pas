{===============================================================================
  Unit:        LoginScreen

  Defines:     TfrmLoginScreen

  Description: Login screen for the Thesaurus Editor.

  Created:     January 2004

  Last revision information:
    $Revision: 3 $
    $Date: 19/06/09 15:52 $
    $Author: Ericsalmon $

===============================================================================}
unit LoginScreen;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ImageListButton, ExtCtrls, Mask, ComboListID,
  LuxIDComboBox, DataClasses, ResourceStrings;

type
  TfrmLoginScreen = class (TForm)
    Bevel1: TBevel;
    btnCancel: TImageListButton;
    btnOK: TImageListButton;
    cmbUsers: TLuxIDComboBox;
    ePassword: TMaskEdit;
    Label1: TLabel;
    Label2: TLabel;
    procedure btnOKClick(Sender: TObject);
    procedure cmbUsersPopulate(Sender: TObject);
  private
    FPasswordList: TStringList;
    FUserID: TKeyString;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property UserID: TKeyString read FUserID;
  end;
  
{-------------------------------------------------------------------------------
}
implementation

uses
  GeneralData, ADODB, Registry, InterfaceDataModule, LuxembourgConstants;

{$R *.dfm}

{-==============================================================================
    TfrmLoginScreen
===============================================================================}
{-------------------------------------------------------------------------------
  Constructor. Creates the string list.
}
constructor TfrmLoginScreen.Create(AOwner: TComponent);
begin
  inherited;
  FPasswordList := TStringList.Create;
  // Load users now.
  cmbUsers.PopulateContent;
end;  // TfrmLoginScreen.Create

{-------------------------------------------------------------------------------
  Destructor. Destroys the string list.
}
destructor TfrmLoginScreen.Destroy;
begin
  FPasswordList.Free;
  inherited;
end;  // TfrmLoginScreen.Destroy 

{-------------------------------------------------------------------------------
  Checks to see if the password the user has entered is the same as the one stored in the 
      stringlist for the selected user. 
}
procedure TfrmLoginScreen.btnOKClick(Sender: TObject);
begin
  inherited;
  if FPasswordList.Values[cmbUsers.CurrentStrID] <> ePassword.Text then begin
    ShowMessage(ResStr_PasswordIncorrect);
    ePassword.Text := '';
  end
  else if ePassword.Text <> '' then begin
    FUserID := cmbUsers.CurrentStrID;
    ModalResult := mrOK;
  end;
end;  // TfrmLoginScreen.btnOKClick 

{-------------------------------------------------------------------------------
  Populate the combo box containing the users. A connection must be created manually. 
}
procedure TfrmLoginScreen.cmbUsersPopulate(Sender: TObject);
var
  lRecordset: _Recordset;
  lTrusted: Boolean;
  lServerName, lDbName, lSecurity, lConnectionString: String;
  lConnection: TADOConnection;
begin
  with TRegistry.Create do
    try
      RootKey := HKEY_LOCAL_MACHINE;
      if OpenKeyReadOnly(JNCC_REG_ROOT_PATH) then begin
        if ValueExists('Server Name') then begin
          lServerName := ReadString('Server Name');
          lDbName     := ReadString('Database Name');
          lTrusted    := ReadBool('Trusted Security');
          if lTrusted then
            lSecurity := 'Integrated Security=SSPI;'
          else
            lSecurity := 'User ID=NBNUser;password=NBNPassword;';
  
          lConnectionString := Format('Provider=SQLOLEDB.1;%sPersist Security Info=False;' +
                                'Data Source=%s;Initial Catalog=%s;OLE DB Services=-2',
                                [lSecurity, lServerName, lDbName]);
        end;
        CloseKey;
      end;
    finally
      Free;
    end;
  lConnection := TADOConnection.Create(nil);
  with lConnection do begin
    try
      ConnectionString := lConnectionString;
      Connected := true;
      with TADOCommand.Create(nil) do begin
        try
          Connection := lConnection;
          CommandType := cmdStoredProc;
          CommandTimeout := 0;
          CommandText := 'usp_Users_Select';
          Parameters.Refresh;
          Parameters.ParamValues['@MinimumSecurityLevel'] := 4;
          lRecordset := Execute;
        finally
          Free;
        end;
      end;
      with lRecordset do begin
        while not Eof do begin
          cmbUsers.Add(VarToStr(Fields['User_Name'].Value),
                       VarToStr(Fields['Name_Key'].Value));
          FPasswordList.Add(VarToStr(Fields['Name_Key'].Value) + '=' +
                            VarToStr(Fields['Password'].Value));
          MoveNext;
        end; // while
        Close;
      end; // with
      finally
        if State = [stOpen] then Close;
        Free;
    end;
  end;
end;  // TfrmLoginScreen.cmbUsersPopulate 

end.
