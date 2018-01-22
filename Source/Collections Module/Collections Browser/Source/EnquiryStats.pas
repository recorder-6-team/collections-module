{===============================================================================
  Unit:        EnquiryStats

  Defines:     TfrmEnquiryStats

  Description: Generates a csv file containing the number of enquires per
               department for a given date range.

  Model:       CollectionsCommonAndGeneral.mpb

  Created:     15/01/2004

  Last revision information:
    $Revision: 6 $
    $Date: 9/11/07 16:36 $
    $Author: Ericsalmon $

===============================================================================}
unit EnquiryStats;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, VagueDateEdit, VagueDate, ImageListButton, ADODB;

type
  {-----------------------------------------------------------------------------
    Generates a csv file containing the number of enquires per department for a given date
    range.
  }
  TfrmEnquiryStats = class(TForm)
    btnCancel: TImageListButton;
    btnFindFile: TButton;
    btnOK: TImageListButton;
    eDateFrom: TVagueDateEdit;
    eDateTo: TVagueDateEdit;
    eOutputPath: TEdit;
    gpMain: TGroupBox;
    lblDateFrom: TLabel;
    lblDateTo: TLabel;
    lblOutput: TLabel;
    procedure btnFindFileClick(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure eOutputPathChange(Sender: TObject);
  private
    FReportFile: TextFile;
    FRptHeaderFile: String;
    FRptRowFile: String;
    FStandardReportTemplatePath: String;
    procedure AddLineToReportOutput(const AText: string);
    function CheckPathsAreValid: Boolean;
    procedure GetReportInformation;
    function PopulateTemplate(ATemplate: string; ARecordset: _Recordset): String;
  public
    constructor Create(AComponent: TComponent); override;
  end;
  
//==============================================================================
implementation

uses
  GeneralData, ApplicationSettings, ResourceStrings;

{$R *.dfm}

{-==============================================================================
    TfrmEnquiryStats
===============================================================================}
{-------------------------------------------------------------------------------
  Creation code. 
}
constructor TfrmEnquiryStats.Create(AComponent: TComponent);
begin
  inherited;
  FStandardReportTemplatePath := AppSettings.StandardReportTemplatePath;
  if FStandardReportTemplatePath[length(FStandardReportTemplatePath)] <> '\' then
    FStandardReportTemplatePath := FStandardReportTemplatePath + '\';
end;  // TfrmEnquiryStats.Create 

{-------------------------------------------------------------------------------
  Adds single line to the report file. 
}
procedure TfrmEnquiryStats.AddLineToReportOutput(const AText: string);
begin
  WriteLn(FReportFile, AText);
end;  // TfrmEnquiryStats.AddLineToReportOutput 

{-------------------------------------------------------------------------------
  Locate a file to use for the output. 
}
procedure TfrmEnquiryStats.btnFindFileClick(Sender: TObject);
begin
  with TSaveDialog.Create(nil) do begin
    try
      Title := ResStr_SaveReportFile;
      Filter := 'CSV Files (*.csv)|*.csv|All Files (*.*)|*.*';
      DefaultExt := 'csv';
      if Execute then eOutputPath.Text := FileName;
    finally
      Free;
    end;
  end;
end;  // TfrmEnquiryStats.btnFindFileClick 

{-------------------------------------------------------------------------------
  Generate csv file. 
}
procedure TfrmEnquiryStats.btnOKClick(Sender: TObject);
var
  lRecordset: _Recordset;
  lHdrTemplateFile, lRowTemplateFile: TStringlist;
  lFromDateTime, lToDateTime: Double;
begin
  //Default dialog result not to close screen until output is successful
  ModalResult := mrNone;
  
  if not CheckPathsAreValid then Exit;;
  
  if IsVagueDate(Trim(eDateFrom.Text)) then
    lFromDateTime := eDateFrom.VagueDate.StartDate
  else if Trim(eDateFrom.Text) <> '' then begin
    MessageDlg('"' + eDateFrom.Text + '"' + ResStr_IsAnInvalidVagueDate, mtWarning, [mbOK], 0);
    Exit;
  end else
    lFromDateTime := -1;//Use earliest date possible
  
  if IsVagueDate(Trim(eDateTo.Text)) then
    lToDateTime := eDateTo.VagueDate.EndDate
  else if Trim(eDateTo.Text) <> '' then begin
    MessageDlg('"' + eDateTo.Text + '"' + ResStr_IsAnInvalidVagueDate, mtWarning, [mbOK], 0);
    Exit;
  end else
    lToDateTime := -1;//Use latest date possible
  
  lHdrTemplateFile := TStringList.Create;
  lRowTemplateFile := TStringList.Create;
  
  lRecordset := dmGeneral.GetRecordset('usp_EnquiryStats_Select',
      ['@VagueDateStart', lFromDateTime, '@VagueDateEnd', lToDateTime]);

  if FileExists(eOutputPath.Text) then
    if MessageDlg(Format(ResStr_ConfirmFileOverwrite, [eOutputPath.Text]),
                  mtConfirmation, [mbYes, mbNo], 0) <> mrYes then Exit;

  AssignFile(FReportFile, eOutputPath.Text);
  
  try
    Rewrite(FReportFile);
  except on E:EInOutError do
    begin
      MessageDlg('"' + eOutputPath.Text + '"' + ResStr_IsAnInvalidFile, mtWarning, [mbOK], 0);
      Exit;
    end;
  end;
  GetReportInformation;
  
  // Add the Report Header file
  lHdrTemplateFile.LoadFromFile(FRptHeaderFile);
  // <#Types> should be replaced with the name of the field containing the totals for each enquiry
  // types concatenated in a comma-separated string. 
  // The value of first record is comma-separated concatenation of all different enquiry types.
  // Recordset is therefore guaranteed to have at least one record in it.
  if lRecordset.Fields.Count > 1 then
    lHdrTemplateFile.Text := StringReplace(lHdrTemplateFile.Text,
                                           '<#Types>', VarToStr(lRecordset.Fields[1].Value),
                                           [rfReplaceAll, rfIgnoreCase]);
  AddLineToReportOutput(Trim(lHdrTemplateFile.Text)); //Trim to remove additional CRs
  lRecordset.MoveNext;

  //Populate csv data
  lRowTemplateFile.LoadFromFile(FRptRowFile);
  while not lRecordset.EOF do begin
    AddLineToReportOutput(Trim(PopulateTemplate(lRowTemplateFile.Text, lRecordset)));
    lRecordset.MoveNext;
  end;
  
  lRecordset.Close;
   // ensure file is always closed
  CloseFile(FReportFile);
  lHdrTemplateFile.Free;
  lRowTemplateFile.Free;
  
  MessageDlg(ResStr_EnquiryStatsOutputSuccessful + ExtractFileName(Trim(eOutputPath.Text)),
             mtInformation, [mbOK], 0);
  //Allow dialog to close as output was successful
  ModalResult := mrOk;
end;  // TfrmEnquiryStats.btnOKClick 

{-------------------------------------------------------------------------------
  Checks that all required file paths are valid. 
}
function TfrmEnquiryStats.CheckPathsAreValid: Boolean;
var
  lFilePath: String;
  lBSIndex: Integer;
begin
  lFilePath := Trim(eOutputPath.Text);
  for lBSIndex := Length(lFilePath) downto 1 do
    if lFilePath[lBSIndex] = '\' then break;
  
  if lBSIndex > 0 then lFilePath := Copy(lFilePath, 1, lBSIndex);
  
  if not DirectoryExists(FStandardReportTemplatePath) then begin
    MessageDlg(ResStr_StandardReportTemplatePathInvalid, mtError, [mbOK], 0);
    Result := False;
  end
  else if not DirectoryExists(lFilePath) then begin
    MessageDlg('"' + lFilePath + '"' + ResStr_IsAnInvalidPath, mtWarning, [mbOK], 0);
    Result := False;
  end
  else if (Length(ExtractFileName(Trim(eOutputPath.Text))) = 0) then begin
    MessageDlg(ResStr_PleaseSpecifyAFileName, mtInformation, [mbOK], 0);
    Result := False;
  end
  else
    Result := True;
end;  // TfrmEnquiryStats.CheckPathsAreValid 

{-------------------------------------------------------------------------------
  Sets state of OK button depending upon contents of eOutputPath. 
}
procedure TfrmEnquiryStats.eOutputPathChange(Sender: TObject);
begin
  btnOk.Enabled := (Length(Trim(eOutputPath.Text)) > 0);
end;  // TfrmEnquiryStats.eOutputPathChange 

{-------------------------------------------------------------------------------
  Sets names of report files. 
}
procedure TfrmEnquiryStats.GetReportInformation;
begin
  FRptHeaderFile := FStandardReportTemplatePath + 'Hdr_Enquiry_Stats.txt';
  FRptRowFile    := FStandardReportTemplatePath + 'Row_Enquiry_Stats.txt';
end;  // TfrmEnquiryStats.GetReportInformation 

{-------------------------------------------------------------------------------
  Populates input template with data. 
}
function TfrmEnquiryStats.PopulateTemplate(ATemplate: string; ARecordset: _Recordset): String;
var
  lIdx: Integer;
begin
  Result := ATemplate;
  // for each field we have, replace the tags with the supplied value
  for lIdx := 0 to ARecordset.Fields.Count-1 do
    Result := StringReplace(Result, '<#' + ARecordset.Fields[lIdx].Name + '>',
                            VarToStr(ARecordset.Fields[lIdx].Value),
                            [rfReplaceAll, rfIgnoreCase]);

  // Replace <#Types>'. This should match the field containing the totals for each enquiry
  // types concatenated in a comma-separated string.
  Result := StringReplace(Result, '<#Types>', VarToStr(ARecordset.Fields[1].Value),
                          [rfReplaceAll, rfIgnoreCase]);
end;  // TfrmEnquiryStats.PopulateTemplate

end.
