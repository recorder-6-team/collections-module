{===============================================================================
  Unit:        ChecklistTransferForm

  Defines:     TdlgChecklistTransferForm

  Description: Checklist transfer dialog

  Created:     Jan 2004

  Last revision information:
    $Revision: 7 $
    $Date: 27/05/11 13:31 $
    $Author: Jamesbichard $

===============================================================================}
unit ChecklistTransferForm;

interface

uses
  Forms, DataClasses, ADODB, DomainConceptGroupSelector, Controls,
  StdCtrls, ComboListID, Classes, ImageListButton, DataTypes, ExtCtrls;

type
  {-----------------------------------------------------------------------------
    Base form class for the dialog that handles interchange of concpt groups
    and taxon dictionary lists.
  }
  TdlgChecklistTransferForm = class(TForm)
    btnCancel: TImageListButton;
    btnOk: TImageListButton;
    cmbChecklist: TIDComboBox;
    fraDomainConceptGroupsSelector: TfraDomainConceptGroupsSelector;
    gbChecklist: TGroupBox;
    gbConceptGroup: TGroupBox;
    Label1: TLabel;
    ProgressTimer: TTimer;
    procedure btnOkClick(Sender: TObject);
    procedure cmbChecklistClick(Sender: TObject);
    procedure cmbChecklistPopulate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure fraDomainConceptGroupsSelectorcmbConceptGroupsClick(Sender:
            TObject);
    procedure fraDomainConceptGroupsSelectorcmbDomainsSelect(Sender: TObject);
    procedure ProgressTimerTimer(Sender: TObject);
  private
    procedure CreateStatusCommand;
  protected
    FExecuting: Boolean;
    FJobID: Integer;
    FStatusCommand: TADOCommand;
    FValid: Boolean;
    procedure CloseTransfer; virtual;
    function GetChecklistKey: TKeyString; virtual;
    function GetConceptGroupKey: TKeyString; virtual;
    procedure Notify; virtual; abstract;
    procedure RunTransfer; virtual;
    procedure SetValid(Value: Boolean); virtual;
    procedure StartTransfer; virtual;
    procedure TransferComplete(ATarget: TObject; AError: Error);
    function TransferProcedure: string; virtual; abstract;
    procedure UpdateButtons; virtual;
    procedure Validate; virtual; abstract;
    property ChecklistKey: TKeyString read GetChecklistKey;
    property ConceptGroupKey: TKeyString read GetConceptGroupKey;
    property Valid: Boolean read FValid write SetValid;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure PollProgress;
  end;
  
implementation

{$R *.dfm}

uses
  DB, GeneralData, ThesaurusEditorMain, Variants, Dialogs, SysUtils,
  ResourceStrings;

{-==============================================================================
    TdlgChecklistTransferForm
===============================================================================}
{-------------------------------------------------------------------------------
}
constructor TdlgChecklistTransferForm.Create(AOwner: TComponent);
begin
  inherited;
  fraDomainConceptGroupsSelector.PopulateDomainCombo;
  FExecuting := False;
end;  // TdlgChecklistTransferForm.Create 

{-------------------------------------------------------------------------------
}
destructor TdlgChecklistTransferForm.Destroy;
begin
  dmGeneral.CancelAsyncCommands(TransferComplete);
  FStatusCommand.Free;
  frmThesaurusEditor.SetStatus('');
  frmThesaurusEditor.SetProgress(0, 100);
  inherited;
end;  // TdlgChecklistTransferForm.Destroy 

{-------------------------------------------------------------------------------
}
procedure TdlgChecklistTransferForm.btnOkClick(Sender: TObject);
begin
  Validate;
  if Valid then RunTransfer;
end;  // TdlgChecklistTransferForm.btnOkClick 

{-------------------------------------------------------------------------------
}
procedure TdlgChecklistTransferForm.CloseTransfer;
begin
  dmGeneral.RunStoredProc(
          'usp_Import_Export_Job_Close',
          ['@import_export_id', FJobID]);
end;  // TdlgChecklistTransferForm.CloseTransfer 

{-------------------------------------------------------------------------------
}
procedure TdlgChecklistTransferForm.cmbChecklistClick(Sender: TObject);
begin
  UpdateButtons;
end;  // TdlgChecklistTransferForm.cmbChecklistClick 

{-------------------------------------------------------------------------------
}
procedure TdlgChecklistTransferForm.cmbChecklistPopulate(Sender: TObject);
var
  Checklists: TADODataset;
begin
  Checklists := TADODataset.Create(nil);
  try
    Checklists.Recordset := dmGeneral.GetRecordset('usp_TaxonList_Select', []);
    Checklists.Open;
    while not Checklists.Eof do
    begin
      cmbChecklist.Add(
          Checklists.FieldByName('ITEM_NAME').AsString,
          Checklists.FieldByName('TAXON_LIST_KEY').AsString);
      Checklists.Next;
    end;
  finally
    Checklists.Free;
  end;
end;  // TdlgChecklistTransferForm.cmbChecklistPopulate 

{-------------------------------------------------------------------------------
}
procedure TdlgChecklistTransferForm.CreateStatusCommand;
begin
  FStatusCommand := TADOCommand.Create(nil);
  FStatusCommand.Connection := dmGeneral.Connection;
  FStatusCommand.CommandType := cmdStoredProc;
  FStatusCommand.CommandText := 'usp_Import_Export_Job_GetProgress';
  FStatusCommand.CommandTimeout := 0;
  FStatusCommand.ExecuteOptions := [eoExecuteNoRecords];
  FStatusCommand.Parameters.CreateParameter(
          '@job_id', ftInteger, pdInput, 0, FJobID);
  FStatusCommand.Parameters.CreateParameter(
          '@status_message', ftString, pdOutput, 200, Null);
  FStatusCommand.Parameters.CreateParameter(
          '@total_records', ftInteger, pdOutput, 0, Null);
  FStatusCommand.Parameters.CreateParameter(
          '@records_processed', ftInteger, pdOutput, 0, Null);
end;  // TdlgChecklistTransferForm.CreateStatusCommand 

{-------------------------------------------------------------------------------
}
procedure TdlgChecklistTransferForm.FormCloseQuery(Sender: TObject; var
        CanClose: Boolean);
var
  Rc: Integer;
begin
  if not FExecuting then
    CanClose := True
  else
  begin
    Rc := MessageDlg(
              ResStr_QueryAbortTransfer,
              mtConfirmation,
              [mbYes, mbNo],
              0);
    CanClose := (Rc = mrYes);
  end;
  if not CanClose then ModalResult := mrNone;
end;  // TdlgChecklistTransferForm.FormCloseQuery 

{-------------------------------------------------------------------------------
}
procedure
        TdlgChecklistTransferForm.fraDomainConceptGroupsSelectorcmbConceptGroupsClick(Sender: TObject);
begin
  UpdateButtons;
end;  // TdlgChecklistTransferForm.fraDomainConceptGroupsSelectorcmbConceptGroupsClick

{-------------------------------------------------------------------------------
}
procedure
        TdlgChecklistTransferForm.fraDomainConceptGroupsSelectorcmbDomainsSelect(Sender: TObject);
begin
  fraDomainConceptGroupsSelector.cmbDomainsSelect(Sender);
  UpdateButtons;
end;  // TdlgChecklistTransferForm.fraDomainConceptGroupsSelectorcmbDomainsSelect

{-------------------------------------------------------------------------------
}
function TdlgChecklistTransferForm.GetChecklistKey: TKeyString;
begin
  Result := cmbChecklist.CurrentStrID;
end;  // TdlgChecklistTransferForm.GetChecklistKey 

{-------------------------------------------------------------------------------
}
function TdlgChecklistTransferForm.GetConceptGroupKey: TKeyString;
begin
  Result := fraDomainConceptGroupsSelector.ConceptGroupKey;
end;  // TdlgChecklistTransferForm.GetConceptGroupKey 

{-------------------------------------------------------------------------------
}
procedure TdlgChecklistTransferForm.PollProgress;
begin
  CreateStatusCommand;
  ProgressTimer.Enabled := True;
end;  // TdlgChecklistTransferForm.PollProgress 

{-------------------------------------------------------------------------------
}
procedure TdlgChecklistTransferForm.ProgressTimerTimer(Sender: TObject);
var
  lTime: TDateTime;
begin
  // Note the time.  If this query takes too long, increase the timer delay
  lTime := now;
  FStatusCommand.Execute;
  // Ensure this query takes up less than 1/4 of running time
  if (now-lTime) * 60 * 60 * 24 * 1000 > ProgressTimer.Interval div 4 then
    ProgressTimer.Interval := Trunc((now-lTime) * 60 * 60 * 24 * 1000 * 4);
  frmThesaurusEditor.SetStatus(
      FStatusCommand.Parameters.ParamValues['@status_message']);
  frmThesaurusEditor.SetProgress(
      FStatusCommand.Parameters.ParamValues['@records_processed'],
      FStatusCommand.Parameters.ParamValues['@total_records']);
end;  // TdlgChecklistTransferForm.ProgressTimerTimer 

{-------------------------------------------------------------------------------
  Perform the import/export. 
}
procedure TdlgChecklistTransferForm.RunTransfer;
begin
  FExecuting := True;
  btnOk.Enabled := False; // prevent re-entry during processing
  cmbChecklist.Enabled := False;
  fraDomainConceptGroupsSelector.Enabled := False;
  Screen.Cursor := crHourglass;
  StartTransfer;
  PollProgress;
end;  // TdlgChecklistTransferForm.RunTransfer 

{-------------------------------------------------------------------------------
}
procedure TdlgChecklistTransferForm.SetValid(Value: Boolean);
begin
  FValid := Value;
end;  // TdlgChecklistTransferForm.SetValid 

{-------------------------------------------------------------------------------
  Initialise the transfer job, and set it running. 
}
procedure TdlgChecklistTransferForm.StartTransfer;
begin
  FJobID := dmGeneral.GetStoredProcOutputParam(
                    'usp_Import_Export_Job_Prepare',
                    [],
                    '@import_export_id');
  
  dmGeneral.RunAsyncStoredProc(TransferProcedure,
      ['@Job_Id', FJobID,
      '@taxon_list_key', ChecklistKey,
      '@concept_group_key', ConceptGroupKey],
      nil, TransferComplete);
  
end;  // TdlgChecklistTransferForm.StartTransfer 

{-------------------------------------------------------------------------------
  Handle the completion of the transfer process. 
}
procedure TdlgChecklistTransferForm.TransferComplete(ATarget: TObject; AError:
        Error);
begin
  FExecuting := False;
  ProgressTimer.Enabled := False;
  CloseTransfer;
  Screen.Cursor := crDefault;
  frmThesaurusEditor.SetStatus('');
  frmThesaurusEditor.SetProgress(0, 100);
  if ModalResult <> mrCancel then
  begin
    ModalResult := mrOk;
    if Assigned(AError) then
      ShowMessage(ResStr_ExportFailure + AError.Source + #13#10 + AError.Description)
    else
      Notify;
  end;
end;  // TdlgChecklistTransferForm.TransferComplete 

{-------------------------------------------------------------------------------
  Set enabled state of "Ok" button according to whether or not transfer has
          been fully specified by the user.
}
procedure TdlgChecklistTransferForm.UpdateButtons;
begin
  btnOk.Enabled := (ConceptGroupKey <> '') and (cmbChecklist.ItemIndex <> -1);
end;  // TdlgChecklistTransferForm.UpdateButtons 

end.



