{===============================================================================
  Unit:         ConfirmHistoryUpdate

  Defines:      TdlgConfirmHistoryUpdate

  Description:  Confirmation dialog which creates a Concept Group Quality Check
                record for the specified user and concept group.

  Created:      May 2011

  Last revision information:
    $Revision: 1 $
    $Date: 24/05/11 16:35 $
    $Author: Jamesbichard $

===============================================================================}

unit ConfirmHistoryUpdate;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ResourceStrings;

type
  TdlgConfirmHistoryUpdate = class(TForm)
    lblMessage: TLabel;
    btnYes: TButton;
    btnNo: TButton;
    procedure btnYesClick(Sender: TObject);
  private
    { Private declarations }
    FConceptGroupKey: String;
    FUserKey: String;
  public
    { Public declarations }
    procedure SetKeys(AConceptGroupKey: String; AUserKey: String);
    procedure SetMessage(AMessage: String);
  end;

var
  dlgConfirmHistoryUpdate: TdlgConfirmHistoryUpdate;

implementation

uses GeneralData;

{$R *.dfm}

{-------------------------------------------------------------------------------
  Sets the values of the concept group and user keys
}
procedure TdlgConfirmHistoryUpdate.SetKeys(AConceptGroupKey: String; AUserKey: String);
begin
  FConceptGroupKey := AConceptGroupKey;
  FUserKey :=AUserKey;
end;

{-------------------------------------------------------------------------------
}
procedure TdlgConfirmHistoryUpdate.SetMessage(AMessage: String);
begin
  lblMessage.Caption := AMessage;
end;

{-------------------------------------------------------------------------------
}
procedure TdlgConfirmHistoryUpdate.btnYesClick(Sender: TObject);
begin
  dmGeneral.RunStoredProc('usp_ConceptGroupQualityCheck_Insert',
                            [
                              '@Concept_Group_Key', FConceptGroupKey,
                              '@Checked_By_User', FUserKey
                            ]);
end;

end.
