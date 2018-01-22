{===============================================================================
  Unit:        ReportConfiguration

  Defines:     TdlgReportConfiguration
               TCountThread
               TComboGrid

  Description: Dialog allowing report sections to be configured

  Created:     06/08/2003

  Last revision information:
    $Revision: 21 $
    $Date: 7/05/08 16:21 $
    $Author: Johnvanbreda $

===============================================================================}
unit ReportConfiguration;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, InterfaceDataModule, StdCtrls, Buttons, Grids, ImageListButton,
  GeneralFunctions, ExtCtrls, Contnrs, GeneralData, ADODB, ReportObjects;

resourcestring
  ResStr_Error = 'Error';
  ResStr_CountError = 'Error occurred counting data: ';

type
  TStringArray = array of string;

  TComboGrid = class(TStringGrid)
    procedure WMCommand(var Message: TWMCommand); message WM_COMMAND;
  end;
  
  {--------------------------------------------------------------------------
  ---
    Thread class that initiates the report counting procedure.
  }
  TCountThread = class(TThread)
  private
    FCollectionsReport: TCollectionsReport;
    FGrid: TStringGrid;
    procedure SetCollectionsReport(Value: TCollectionsReport);
    procedure SetGrid(Value: TStringGrid);
    procedure UpdateGrid;
  protected
    procedure Execute; override;
  public
    property CollectionsReport: TCollectionsReport read FCollectionsReport
        write SetCollectionsReport;
    property Grid: TStringGrid read FGrid write SetGrid;
  end;
  
  {--------------------------------------------------------------------------
  ---
    Dialog that is displayed when a user selects a report containing more
    than one section.  The user is able to review the sections to include in
    the report, check the expected record counts and set the sort orders
    where relevant.
  }
  TdlgReportConfiguration = class(TForm)
    Bevel1: TBevel;
    btnCancel: TImageListButton;
    btnGetOutputPath: TButton;
    btnOk: TImageListButton;
    cmbSortOrder: TComboBox;
    eOutputPath: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    mmSummary: TMemo;
    procedure btnGetOutputPathClick(Sender: TObject);
    procedure btnOkClick(Sender: TObject);
    procedure cmbSortOrderChange(Sender: TObject);
    procedure eOutputPathChange(Sender: TObject);
    procedure sgBlocksDrawCell(Sender: TObject; ACol, ARow: Integer; Rect:
        TRect; State: TGridDrawState);
    procedure sgBlocksMouseUp(Sender: TObject; Button: TMouseButton; Shift:
        TShiftState; X, Y: Integer);
    procedure sgBlocksSelectCell(Sender: TObject; ACol, ARow: Integer; var
        CanSelect: Boolean);
  private
    FAsyncQueriesRemain: Boolean;
    FCountThread: TCountThread;
    FDialogVisible: Boolean;
    FFileTypes: TStringList;
    FIsListReport: Boolean;
    FReport: TCollectionsReport;
    FSaveButtonPressed: Boolean;
    FSaveDialogFilter: string;
    sgBlocks: TComboGrid;
    procedure CountRecords;
    procedure DisplayError(AErrorNumber: integer; AFileType: string = '');
    procedure SetupAllowedFileTypes;
  public
    constructor Create(AOwner : TComponent; AReport: TCollectionsReport;
        const AWarning: string);
        reintroduce; overload;
    destructor Destroy; override;
    procedure ShowSaveDialog;
    property AsyncQueriesRemain: Boolean read FAsyncQueriesRemain write
        FAsyncQueriesRemain;
    property SaveButtonPressed: Boolean read FSaveButtonPressed write
        FSaveButtonPressed;
  end;
  
//==============================================================================
implementation

uses ResourceStrings, ApplicationSettings, Types;

{$R *.dfm}

function ActivateRecorder(AHandle: HWND; param: Pointer): BOOL; stdcall;
var
  lWindowCaption: array[0..255] of Char;
  lClassName : array[0..255] of char;
begin
  GetWindowText(AHandle, lWindowCaption,SizeOf(lWindowCaption));
  GetClassName(AHandle, lClassname, 255);
  if (Copy(lWindowCaption, 1, Length(Application.Title)) = Application.Title)
      and
      (lClassname = 'TfrmMain') then begin
    SetActiveWindow(AHandle);
    Result := False;
  end
  else
    Result := True;
end;

{-==============================================================================
    TCountThread
===============================================================================}
{-------------------------------------------------------------------------------
  Start the counting process. 
}
procedure TCountThread.Execute;
begin
  FCollectionsReport.UpdateCounts;
  Synchronize(UpdateGrid);
end;  // TCountThread.Execute 

{-------------------------------------------------------------------------------
  Accessor method 
}
procedure TCountThread.SetCollectionsReport(Value: TCollectionsReport);
begin
  FCollectionsReport := Value;
end;  // TCountThread.SetCollectionsReport 

{-------------------------------------------------------------------------------
}
procedure TCountThread.SetGrid(Value: TStringGrid);
begin
  FGrid := Value;
end;  // TCountThread.SetGrid 

{-------------------------------------------------------------------------------
}
procedure TCountThread.UpdateGrid;
var
  i: Integer;
begin
  for i := 1 to FGrid.RowCount-1 do
    with TReportBlock(FGrid.Objects[0,i]) do begin
      FGrid.Cells[1,i] := IntToStr(RecordCount);
      if RecordCount=0 then IncludeInReport := False;
    end;
  FGrid.Invalidate;
end;  // TCountThread.UpdateGrid 

{-==============================================================================
    TdlgReportConfiguration
===============================================================================}
{-------------------------------------------------------------------------------
}
constructor TdlgReportConfiguration.Create(AOwner : TComponent; AReport:
    TCollectionsReport; const AWarning: string);
var
  lRowIndex: Integer;
  lReportBlock: TReportBlock;
begin
  inherited Create(AOwner);
  // if there was a warning generated when preparing the list of items for
  // the report, include it in the summary
  mmSummary.Lines.Text := AWarning;
  FReport := AReport;
  FIsListReport := not (AReport is TDetailsReport);
  sgBlocks := TComboGrid.Create(Self);
  with sgBlocks do begin
    Parent := Self;
    Left := 12;
    Top := 143;
    Width := 407;
    Height := 201;
    Anchors := [akLeft, akTop, akRight, akBottom];
    ColCount := 4;
    DefaultRowHeight := 21;
    FixedCols := 2;
    RowCount := 2;
    Options := [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine];
    TabOrder := 6;
    OnDrawCell := sgBlocksDrawCell;
    OnMouseUp := sgBlocksMouseUp;
    OnSelectCell := sgBlocksSelectCell;
    ColWidths[0] := 135;
    ColWidths[1] := 81;
    ColWidths[2] := 64;
    ColWidths[3] := 102;
  end;
  sgBlocks.Rows[0].CommaText := 'Section,"Record Count",Included,"Sort Order"';
  
  sgBlocks.RowCount := ReportSettings.AllReportBlocks.Count+1;
  for lRowIndex := 0 to ReportSettings.AllReportBlocks.Count-1 do begin
    lReportBlock := TReportBlock(
        ReportSettings.AllReportBlocks.Objects[lRowIndex]);
    sgBlocks.Rows[lRowIndex + 1].CommaText := '"' + lReportBlock.Title +
        '",-, 1';
    sgBlocks.Objects[0, lRowIndex+1] := lReportBlock;
  end;
  SetupAllowedFileTypes;
  CountRecords;
end;  // TdlgReportConfiguration.Create 

{-------------------------------------------------------------------------------
}
destructor TdlgReportConfiguration.Destroy;
begin
  FCountThread.Free;
  FFileTypes.Free;
  inherited;
end;  // TdlgReportConfiguration.Destroy 

{-------------------------------------------------------------------------------
  There were big problems if this button was pressed when Async queries were
      still coming back - it would access violate for some unknown reason we
      never worked out. To correct this, now if the button is pressed it will
      wait until the current async query is returned, then stop any more async
      queries being launched and then show the save dialog. When the save
      dialog is closed, any remaining async queries will be finished.
  
  It is the Report Generator and its descendants that deals with the Async
      queries, so it is that that needs to know when to pause/progress. As the
      Report Generator creates this Report Configuration form it can easily
      access the SaveButtonPressed property so it can check to see the state of
      the button.
}
procedure TdlgReportConfiguration.btnGetOutputPathClick(Sender: TObject);
begin
  if FAsyncQueriesRemain then
    FSaveButtonPressed := True
  else
    ShowSaveDialog;
end;  // TdlgReportConfiguration.btnGetOutputPathClick 

{-------------------------------------------------------------------------------
}
procedure TdlgReportConfiguration.btnOkClick(Sender: TObject);
var
  lFileString: string;
  liCounter: Integer;
  lErrorNumber: Integer;
  lSectionsSelected: Boolean;
begin
  ReportSettings.Summary := Trim(mmSummary.Text);
  ReportSettings.ReportPath := Trim(eOutputPath.Text);
  lErrorNumber := 0;
  lSectionsSelected := false;
  lFileString := ReportSettings.ReportPath;
  while Pos('.', lFileString) > 0 do
    lFileString := UpperCase(Trim(Copy(lFileString, Pos('.', lFileString) + 1, 255)));
  ReportSettings.ReportFilesDirectory := FFileTypes.Values[lFileString];

  for liCounter := 1 to sgBlocks.RowCount-1 do begin
    if sgBlocks.Cells[2, liCounter] = '1' then begin
      lSectionsSelected := true;
      Break;
    end;
  end;

  if not lSectionsSelected then
    lErrorNumber := 1
  else if (lFileString = '') then
    lErrorNumber := 2
  else if (FFileTypes.Count = 0) then
    lErrorNumber := 3
  else
    for liCounter := 0 to FFileTypes.Count - 1 do
      if FFileTypes.IndexOfName(lFileString) = -1 then
        lErrorNumber := 4;

  if lErrorNumber > 0 then begin
    DisplayError(lErrorNumber, lFileString);
    ModalResult := mrNone;
  end else
  if FileExists(eOutputPath.Text)
  and (MessageDlg(Format(ResStr_ConfirmFileOverwrite, [eOutputPath.Text]),
                  mtConfirmation, [mbYes, mbNo], 0) <> mrYes) then
    ModalResult := mrNone;
end;  // TdlgReportConfiguration.btnOkClick

{-------------------------------------------------------------------------------
}
procedure TdlgReportConfiguration.cmbSortOrderChange(Sender: TObject);
begin
  if Sender is TComboBox then
    if TComboBox(Sender).Visible then
      TReportBlock(sgBlocks.Objects[0, TComboBox(Sender).Tag]).
          SelectedOrderIndex := TComboBox(Sender).ItemIndex;
end;  // TdlgReportConfiguration.cmbSortOrderChange 

{-------------------------------------------------------------------------------
  Start the thread that counts the records on the grid. 
}
procedure TdlgReportConfiguration.CountRecords;
begin
  FCountThread := TCountThread.Create(True);
  FCountThread.CollectionsReport := FReport;
  FCountThread.Grid := sgBlocks;
  FCountThread.Resume;
end;  // TdlgReportConfiguration.CountRecords 

{-------------------------------------------------------------------------------
}
procedure TdlgReportConfiguration.DisplayError(AErrorNumber: integer;
    AFileType: string = '');
begin
  case AErrorNumber of
    1: MessageDlg(ResStr_PleaseIncludeAReportSection, mtInformation, [mbOK], 0);
    2: MessageDlg(ResStr_EnterAValidReport, mtInformation, [mbOK], 0);
    3: MessageDlg(ResStr_NoValidOutputFileTypes, mtError, [mbOK], 0);
    4: MessageDlg(''''+AFileType+''''+ResStr_UnknownFileFormat, mtWarning,
        [mbok], 0);
  end;
end;  // TdlgReportConfiguration.DisplayError 

{-------------------------------------------------------------------------------
}
procedure TdlgReportConfiguration.eOutputPathChange(Sender: TObject);
begin
  btnOk.Enabled := Trim(eOutputPath.Text) <> '';
end;  // TdlgReportConfiguration.eOutputPathChange 

{-------------------------------------------------------------------------------
}
procedure TdlgReportConfiguration.SetupAllowedFileTypes;
var
  lFileTypeFile: TStringList;
  lFileTypeValue: string;
  lFileTypePath: string;
  lSearchRec: TSearchRec;
  lStandardReportTemplatePath: string;
begin
  FFileTypes := TStringList.Create;
  lFileTypeFile := TStringlist.Create;
  
  lStandardReportTemplatePath := IncludeTrailingPathDelimiter(
                                     AppSettings.StandardReportTemplatePath);
  try
    if FindFirst(lStandardReportTemplatePath + '*.*', faDirectory,
        lSearchRec) = 0 then begin
      while FindNext(lSearchRec) = 0 do begin
        // Get the ini file
        lFileTypePath := lStandardReportTemplatePath + lSearchRec.Name + '\';
        if FileExists(lFileTypePath + 'FileType.ini') then begin
          lFileTypeFile.LoadFromFile(lFileTypePath + 'FileType.ini');
          lFileTypeValue := Trim(lFileTypeFile.Strings[0]);
  
          if FSaveDialogFilter = '' then
            FSaveDialogFilter := lFileTypeValue
          else
            FSaveDialogFilter := FSaveDialogFilter + '|' + lFileTypeValue;
  
          FFileTypes.Add(UpperCase(Copy(lFileTypeValue,
                                        Pos('|*.', lFileTypeValue) + 3, 255))
                         + '=' + lFileTypePath);
        end;
      end;
    end;
  finally
    lFileTypeFile.Free;
    FindClose(lSearchRec);
  end;
  
  if FFileTypes.Count = 0 then
      MessageDlg(ResStr_NoValidOutputFileTypes, mtInformation, [mbOK], 0);
end;  // TdlgReportConfiguration.SetupAllowedFileTypes 

{-------------------------------------------------------------------------------
}
procedure TdlgReportConfiguration.sgBlocksDrawCell(Sender: TObject; ACol, ARow:
    Integer; Rect: TRect; State: TGridDrawState);
var
  lImagePos: TPoint;
begin
  if (ARow>0) and (ACol=1) then begin
    if TReportBlock(sgBlocks.Objects[0, ARow]).CountError then begin
      sgBlocks.Canvas.FillRect(Rect);
      DrawChoppedText('#' + ResStr_Error + '#', sgBlocks.Canvas, Rect, 4);
    end;
  end
  else if (ARow>0) and (ACol=2) then begin
    // draw inclusion checkbox
    sgBlocks.Canvas.FillRect(Rect);
    lImagePos.X := Rect.Left + ((Rect.Right - Rect.Left) - 12) div 2;
    lImagePos.Y := Rect.Top + ((Rect.Bottom - Rect.Top) - 12) div 2;
    DrawCheckBox(sgBlocks.Canvas, lImagePos.X, lImagePos.Y,
                 TReportBlock(sgBlocks.Objects[0, ARow]).IncludeInReport,
                 TReportBlock(sgBlocks.Objects[0, ARow]).RecordCount>0);
  end
  else if (ARow>0) and (ACol=3) then begin
    sgBlocks.Canvas.FillRect(Rect);
    with TReportBlock(sgBlocks.Objects[0, ARow]) do begin
      if not IncludeInReport then
        sgBlocks.Canvas.Font.Color := clBtnFace;
      if ReportBlockOrderCount>0 then
        DrawChoppedText(ReportBlockOrder[SelectedOrderIndex].ItemName,
            sgBlocks.Canvas, Rect, 4);
    end;
  end;
end;  // TdlgReportConfiguration.sgBlocksDrawCell 

{-------------------------------------------------------------------------------
  Toggle the inclusion state of a report block when clicking on a checkbox in
      the grid.
}
procedure TdlgReportConfiguration.sgBlocksMouseUp(Sender: TObject; Button:
    TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  lCol, lRow: Integer;
begin
  sgBlocks.MouseToCell(X, Y, lCol, lRow);
  if (lCol=2) and (lRow>0) then
    with TReportBlock(sgBlocks.Objects[0, lRow]) do begin
      IncludeInReport := not IncludeInReport;
      sgBlocks.Invalidate;
    end;
end;  // TdlgReportConfiguration.sgBlocksMouseUp 

{-------------------------------------------------------------------------------
}
procedure TdlgReportConfiguration.sgBlocksSelectCell(Sender: TObject; ACol,
    ARow: Integer; var CanSelect: Boolean);
var
  lCellRect: TRect;
  i: Integer;
begin
  dmGeneral.Recorder.RecorderMainForm.StatusText := '';
  if (ARow>0) and (ACol=3) then begin
    with cmbSortOrder do begin
      Parent := sgBlocks;
      Tag    := ARow;
      lCellRect := sgBlocks.CellRect(3, ARow);
      SetBounds(lCellRect.Left,
                lCellRect.Top,
                lCellRect.Right-lCellRect.Left,
                lCellRect.Bottom-lCellRect.Top);
      Clear;
    end;
    with TReportBlock(sgBlocks.Objects[0, ARow]) do begin
      for i := 0 to ReportBlockOrderCount-1 do
        cmbSortOrder.Items.Add(ReportBlockOrder[i].ItemName);
      cmbSortOrder.Visible := (ReportBlockOrderCount>0) and IncludeInReport;
      if cmbSortOrder.Visible then begin
        cmbSortOrder.ItemIndex := SelectedOrderIndex;
        cmbSortOrder.Height := cmbSortOrder.Height +
            Max(cmbSortOrder.DropDownCount, 8)*cmbSortOrder.ItemHeight;
      end;
    end;
  end
  else begin
    cmbSortOrder.Visible := False;
    if (ARow>0) and (ACol=2) then begin
      if TReportBlock(sgBlocks.Objects[0, ARow]).CountError then
        dmGeneral.Recorder.RecorderMainForm.StatusText :=
            ResStr_CountError + TReportBlock(sgBlocks.Objects[0, ARow]).CountErrorHint;
    end;
  end;
end;  // TdlgReportConfiguration.sgBlocksSelectCell

{-------------------------------------------------------------------------------
  This will show the Save Dialog window. 
}
procedure TdlgReportConfiguration.ShowSaveDialog;
var
  lParent: TWinControl;
begin
  with TSaveDialog.Create(nil) do begin
    try
      Title := ResStr_SaveReportFile;
      Filter := FSaveDialogFilter;
      FDialogVisible := True;
      if Execute then begin
        FDialogVisible := False;
        if Pos('.', FileName) > 0 then
          eOutputPath.Text := FileName
        else
          eOutputPath.Text := FileName + '.' +
                              LowerCase(FFileTypes.Names[FilterIndex - 1]);
      end;
    finally
      Free;
      FSaveButtonPressed := False;
      { The following code fixes a problem with TCommonDialog, where it
           sometimes brings another app to the front! }
      // find Recorder and ensure it is active.
      EnumWindows(@ActivateRecorder, 0);
      // find the form we are on and force it to the front
      lParent := Self;
      while Assigned(lParent) do begin
        if lParent is TForm then begin
          SetForegroundWindow(lParent.Handle);
          Break; // from loop
        end;
        lParent := lParent.Parent;
      end;
    end;
  end;
end;  // TdlgReportConfiguration.ShowSaveDialog 



{ TComboGrid }

{-==============================================================================
    TComboGrid
===============================================================================}
{-------------------------------------------------------------------------------
}
procedure TComboGrid.WMCommand(var Message: TWMCommand);
var
  lHandled: Boolean;
begin
  lHandled := False;
  // Test if InplaceEditor showing first.
  if Assigned(InplaceEditor) then
    if EditorMode and (Message.Ctl = InplaceEditor.Handle) then begin
      lHandled := True;
      inherited;
    end;
  // If not InplaceEditor, send on to whatever control wants it.
  if not lHandled and (Message.Ctl <> 0) then
    Message.Result := SendMessage(Message.Ctl, CN_COMMAND,
                                  TMessage(Message).WParam, TMessage(
                                      Message).LParam);
end;  // TComboGrid.WMCommand 

end.








