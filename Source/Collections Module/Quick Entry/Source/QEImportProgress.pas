{===============================================================================
  Unit:        QEImportProgress.pas

  Defines:     TfraQEImportProgress

  Description: Wizard page displayed during the import of data into a quick
               entry session.  

  Model:       -

  Created:     September 2004

  Last revision information:
    $Revision: 2 $
    $Date: 9/09/04 16:16 $
    $Author: Andrewkemp $

===============================================================================}
unit QEImportProgress;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, 
  Dialogs, QEImportFrameUnit, ComCtrls, StdCtrls, ADOInt;

resourcestring
  ResStr_ConfirmCancelImport = 'Are you sure that you want to stop the import'
      + ' process?  Any records that have already been inserted will not be'
      + ' removed.';
  ResStr_ImportComplete = 'The import process has been completed.';

type
  TfraQEImportProgress = class(TQEImportFrame)
    pbProgress: TProgressBar;
    lblProgress: TLabel;
  protected
    FImportThread: TThread;
    FImportData: _Recordset;
    function GetNext: TQEImportFrameClass; override;
    procedure Resize; override;
    procedure ThreadTerminate(Sender: TObject); virtual;
    procedure UpdateProgress; virtual;
  public
    destructor Destroy; override;
    function CancelQuery: Boolean; override;
    procedure LoadContent; override;
  end;

var
  fraQEImportProgress: TfraQEImportProgress;



  
implementation

{$R *.dfm}

uses
  GeneralData, QEImportDefinition;

type
  TImportThread = class(TThread)
  protected
    FFrame: TfraQEImportProgress;
    FFinished: Boolean;
    procedure Execute; override;
  public
    constructor Create(Frame: TfraQEImportProgress);
    property Finished: Boolean read FFinished;
  end;

{-==============================================================================
    TImportThread
===============================================================================}
{-------------------------------------------------------------------------------
}
constructor TImportThread.Create(Frame: TfraQEImportProgress);
begin
  FFrame := Frame;
  inherited Create(True);
end;

{-------------------------------------------------------------------------------
}
procedure TImportThread.Execute;
var
  I: Integer;
  lValues: array of Variant;
begin
  SetLength(lValues, FFrame.FImportData.Fields.Count - 1);
  
  with FFrame.FImportData do
    while not (Eof or Terminated) do
    begin
      for I := 0 to Fields.Count - 2 do  // -2 => skip ID field
        lValues[I] := Fields[I].Value;

      FFrame.Definition.ImportValues(lValues);  
      Synchronize(FFrame.UpdateProgress);      
      MoveNext;  
    end;

  FFinished := not Terminated;
end;  // TImportThread.Execute

{-==============================================================================
    TfraQEImportProgress
===============================================================================}
{-------------------------------------------------------------------------------
}
destructor TfraQEImportProgress.Destroy;
begin
  FImportThread.Free;
  inherited;
end;  // TfraQEImportProgress.Destroy

{-------------------------------------------------------------------------------
}
function TfraQEImportProgress.CancelQuery: Boolean;
begin
  FImportThread.Suspend;
  try
    Result := mrYes = MessageDlg(
        ResStr_ConfirmCancelImport, 
        mtConfirmation, 
        [mbYes, mbNo], 
        0);
  finally 
    FImportThread.Resume;
  end;
end;  // TfraQEImportProgress.CancelQuery

{-------------------------------------------------------------------------------
}
function TfraQEImportProgress.GetNext: TQEImportFrameClass;
begin
  Result := nil;
end;  // TfraQEImportProgress.GetNext

{-------------------------------------------------------------------------------
}
procedure TfraQEImportProgress.LoadContent;
var
  lSql: String;
begin
  lSql := 'SELECT * FROM ' + QE_IMPORT_TABLE_NAME;

  if not (Definition.ImportType in itDatabase) then
    // table always contains first row from source; skip it if we don't want it
    if Definition.ImportFieldNamesFromData or (Definition.FirstRow > 1) then
      lSql := lSql + ' WHERE ' + QE_IMPORT_ID_FIELD + ' > 1';
  
  FImportData := dmGeneral.ExecuteSql(lSql, True);
  pbProgress.Max := FImportData.RecordCount;
  FImportThread := TImportThread.Create(Self);
  FImportThread.OnTerminate := ThreadTerminate;
  FImportThread.Resume;    
end;  // TfraQEImportProgress.LoadContent

{-------------------------------------------------------------------------------
}
procedure TfraQEImportProgress.Resize;
var
  lOffset: Integer;
begin
  lOffset := pbProgress.Top - lblProgress.Top;
  pbProgress.Top := (ClientHeight - pbProgress.Height) div 2;
  lblProgress.Top := pbProgress.Top - lOffset;
end;  // TfraQEImportProgress.Resize 

{-------------------------------------------------------------------------------
}
procedure TfraQEImportProgress.ThreadTerminate(Sender: TObject);
begin
  if TImportThread(FImportThread).Finished then
  begin
    lblProgress.Caption := ResStr_ImportComplete;
    SetIsComplete(True);
    SetAllowCancel(False);
  end;
end;  // TfraQEImportProgress.ThreadTerminate

{-------------------------------------------------------------------------------
}
procedure TfraQEImportProgress.UpdateProgress;
begin
  pbProgress.Position := FImportData.AbsolutePosition;
end;  // TfraQEImportProgress.UpdateProgress

end.
