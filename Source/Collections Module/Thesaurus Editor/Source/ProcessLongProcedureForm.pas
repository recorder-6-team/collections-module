{===============================================================================
  Unit:        ProcessLongProcedureForm

  Defines:     TfrmProcessLongProcedure

  Description: A form to display when performing very long stored procedures, so
               that the user can't fiddle with any of the real forms, and doesn't
               think that the app has crashed.

  Created:     30/1/2009

  Last revision information:
    $Revision: 1 $
    $Date: 2/02/09 11:09 $
    $Author: Pauldavies $

===============================================================================}

unit ProcessLongProcedureForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, ADODB;

resourcestring
  ResStr_CannotClose = 'You cannot cancel this operation.';
  ResStr_PleaseWait = 'Please wait, this may take several minutes';

type
  TfrmProcessLongProcedure = class(TForm)
    lblMessage: TLabel;
    lblTime: TLabel;
    tmrShowDots: TTimer;
    Bevel1: TBevel;
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure tmrShowDotsTimer(Sender: TObject);
  private
    FProcessingComplete: Boolean;
    FDotCount: Integer;
    FMessage: String;
    FError: ADODB.Error;
    procedure ProcessingComplete(ATarget: TObject; AError: ADODB.Error);
  public
    property Error: ADODB.Error read FError;
    procedure RunStoredProc(displayMessage, procedureName: string; parameters: array of variant);
  end;

var
  frmProcessLongProcedure: TfrmProcessLongProcedure;

implementation

uses GeneralData;

{$R *.dfm}

{-------------------------------------------------------------------------------
  Creates the form and sets the procedure running.
}
procedure TfrmProcessLongProcedure.RunStoredProc(displayMessage, procedureName: string; parameters: array of variant);
begin
  FProcessingComplete := False;
  FDotCount := 0;
  FMessage := displayMessage;
  lblMessage.Caption := displayMessage;
  // If the message label is wider than the time label, the top label will overflow
  // the form, so make it a bit wider.
  if lblMessage.Width > lblTime.Width then
    Width := Width + lblMessage.Width - lblTime.Width;
  dmGeneral.RunAsyncStoredProc(procedureName, parameters, nil, ProcessingComplete);
  ShowModal
end;

{-------------------------------------------------------------------------------
  When the procedure is finished, close the form.
}
procedure TfrmProcessLongProcedure.ProcessingComplete(ATarget: TObject; AError: ADODB.Error);
begin
  FProcessingComplete := True;
  FError := AError;
  Close;
end;

{-------------------------------------------------------------------------------
  If the user tries to close the form before the procedure is complete, gives
  them a message explaining that the form cannot close and then cancels the close
  operation.
}
procedure TfrmProcessLongProcedure.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if not FProcessingComplete then
  begin
    ShowMessage(ResStr_CannotClose);
    CanClose := False;
  end;
end;

{-------------------------------------------------------------------------------
  Displays an increasing row of dots after the message, just to make it look like
  it's doing something.
}
procedure TfrmProcessLongProcedure.tmrShowDotsTimer(Sender: TObject);
begin
  FDotCount := FDotCount + 1;
  if FDotCount > 3 then
  begin
    FDotCount := 0;
    lblTime.Caption := ResStr_PleaseWait;
  end else
    lblTime.Caption := lblTime.Caption + '.';
end;

end.
