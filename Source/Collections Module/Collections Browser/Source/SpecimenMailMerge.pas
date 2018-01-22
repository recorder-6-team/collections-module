{===============================================================================
  Unit:        SpecimenMailMerge

  Defines:     TdlgSpecimenMailMerge

  Description: Generates a XML output file for the selected specimens.

  Model:       CollectionsCommonAndGeneral.mpb

  Created:     September 2004

  Last revision information:
    $Revision: 4 $
    $Date: 8/12/09 14:45 $
    $Author: Simonlewis $

===============================================================================}
unit SpecimenMailMerge;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ImageListButton, ADODB, InterfaceDataModule, ExtCtrls,
  FolderBrowser, ExceptionForm, Barcode;

type
  EMailMergeOutputError = class(TExceptionPath)
  end;
  
  TdlgSpecimenMailMerge = class(TForm)
    Barcode: TBarcode;
    Bevel1: TBevel;
    btnCancel: TImageListButton;
    btnGetOutputFolder: TImageListButton;
    btnGetStyleSheet: TButton;
    btnOK: TImageListButton;
    dlgFolder: TFolderBrowser;
    dlgOpen: TOpenDialog;
    eOutputFolder: TEdit;
    eReportFileName: TEdit;
    eStyleSheet: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    chkGenerateLabelRecords: TCheckBox;
    procedure btnGetOutputFolderClick(Sender: TObject);
    procedure btnGetStyleSheetClick(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure eStyleSheetExit(Sender: TObject);
    procedure TextChanged(Sender: TObject);
  private
    FKeyList: TStringList;
    function GenerateOutput: Boolean;
    function GetBarCodeFileName(const ARegNo: String): String;
    procedure UpdateButtonState;
    procedure CreateNewLabelForSpecimen(const AKey: String);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AddItemKey(const AKey: String);
  end;
  
//==============================================================================
implementation

uses
  GeneralData, ApplicationSettings, ResourceStrings, XMLIntf, XMLDoc, GeneralFunctions;

resourcestring
  ResStr_CreateFolderFailed =
      'Failed to create all the necessary folders for the output. ' +
      'Please ensure you have enough access rights to create the selected folder';

  ResStr_OverwriteFiles =
      'The selected folder already contains a file named ''%s'' and will be overwritten. ' +
      'Do you wish to proceed?';

const
  XML_HEADER = '<?xml version=''1.0''?><?xml-stylesheet type=''text/xsl'' href=''%s''?>';

  //----------------------------------------------------------------------------
  // SQL Required to create a temporary table of Field Collectors from the
  // Collections Browser that can be viewed by a separate stored proc.
  SQL_CREATE_TEMP_SPECIMEN_KEYS =
    'IF Object_Id(''tempdb..#TempSpecimenKeys'') IS NULL CREATE TABLE #TempSpecimenKeys ('+
    'Specimen_Unit_Key CHAR(16) COLLATE SQL_Latin1_General_CP1_CI_AS)';

  SQL_INSERT_TEMP_SPECIMEN_KEYS = 'INSERT INTO #TempSpecimenKeys Values (''%s'')';

  SQL_DROP_TEMP_SPECIMEN_KEYS =
    'IF Object_Id(''tempdb..#TempSpecimenKeys'') IS NOT NULL DROP TABLE #TempSpecimenKeys';

{$R *.dfm}

{-==============================================================================
    TdlgSpecimenMailMerge
===============================================================================}
{-------------------------------------------------------------------------------
}
constructor TdlgSpecimenMailMerge.Create(AOwner: TComponent);
begin
  inherited;
  
  FKeyList := TStringList.Create;
end;  // TdlgSpecimenMailMerge.Create 

{-------------------------------------------------------------------------------
}
destructor TdlgSpecimenMailMerge.Destroy;
begin
  FKeyList.Free;
  
  inherited;
end;  // TdlgSpecimenMailMerge.Destroy 

{-------------------------------------------------------------------------------
}
procedure TdlgSpecimenMailMerge.AddItemKey(const AKey: String);
begin
  FKeyList.Add(AKey);
end;  // TdlgSpecimenMailMerge.AddItemKey 

{-------------------------------------------------------------------------------
}
procedure TdlgSpecimenMailMerge.btnGetOutputFolderClick(Sender: TObject);
begin
  with dlgFolder do begin
    if eOutputFolder.Text = '' then
      Folder := ExtractFilePath(Application.ExeName)
    else
      Folder := eOutputFolder.Text;
  
    if Execute then
      eOutputFolder.Text := Folder;
  end;
  UpdateButtonState;
end;  // TdlgSpecimenMailMerge.btnGetOutputFolderClick 

{-------------------------------------------------------------------------------
}
procedure TdlgSpecimenMailMerge.btnGetStyleSheetClick(Sender: TObject);
begin
  with dlgOpen do begin
    if eStyleSheet.Text <> '' then
      FileName := eStyleSheet.Text;
  
    if Execute then
      eStyleSheet.Text := FileName;
  end;
  UpdateButtonState;
end;  // TdlgSpecimenMailMerge.btnGetStyleSheetClick 

{-------------------------------------------------------------------------------
}
procedure TdlgSpecimenMailMerge.btnOKClick(Sender: TObject);
begin
  if GenerateOutput then
    ModalResult := mrOk;
end;  // TdlgSpecimenMailMerge.btnOKClick 

{-------------------------------------------------------------------------------
}
procedure TdlgSpecimenMailMerge.eStyleSheetExit(Sender: TObject);
begin
  if (eStyleSheet.Text <> '') and (ExtractFileExt(eStyleSheet.Text) = '') then
    eStyleSheet.Text := eStyleSheet.Text + '.xsl';
end;  // TdlgSpecimenMailMerge.eStyleSheetExit 

{===============================================================================
Procedure Name: CreateNewLabelForSpecimen
       Purpose: Creates a new label for the specimen with the supplied
                collection unit key.
-------------------------------------------------------------------------------}
procedure TdlgSpecimenMailMerge.CreateNewLabelForSpecimen(const AKey: String);
var
  Params: Array of Variant;
begin
  SetLength(Params, 4);
  Params[0] := '@Collection_Unit_Key';
  Params[1] := AKey;
  Params[2] := '@SessionID';
  Params[3] := AppSettings.SessionID;

  dmGeneral.RunStoredProc('usp_SpecimensMailMerge_CreateSpecimenLabel', Params);
end;

{-------------------------------------------------------------------------------
}
function TdlgSpecimenMailMerge.GenerateOutput: Boolean;
var
  lFileName: String;
  lXMLDoc: IXMLDocument;
  lXMLNode: IXMLNode;
  i: Integer;
  lXML: String;
  lCursor: TCursor;
begin
  Result := False;
  // Create folders if required.
  if not DirectoryExists(eOutputFolder.Text) then
    if not ForceDirectories(eOutputFolder.Text) then
      raise EMailMergeOutputError.CreateNonCritical(ResStr_CreateFolderFailed);
  
  eOutputFolder.Text := IncludeTrailingPathDelimiter(eOutputFolder.Text);
  
  if ExtractFileExt(eReportFileName.Text) = '' then
    eReportFileName.Text := eReportFileName.Text + '.xml';
  
  lFileName := eOutputFolder.Text + eReportFileName.Text;
  
  // File exists in target folder, get user confirmation to continue.
  if FileExists(lFileName) then
    if MessageDlg(Format(ResStr_OverwriteFiles, [eReportFileName.Text]),
        mtConfirmation, [mbOk, mbCancel], 0) <> mrOk then
      Exit;

  lCursor := HourglassCursor;//crHourGlass;

  try
    // Create temp table for the stored proc
    dmGeneral.ExecuteSQL(SQL_CREATE_TEMP_SPECIMEN_KEYS);
    // Populate the temp table
    for i := 0 to FKeyList.Count - 1 do
    begin
      dmGeneral.ExecuteSQL(Format(SQL_INSERT_TEMP_SPECIMEN_KEYS, [FKeyList[i]]));

      if chkGenerateLabelRecords.Checked then
        CreateNewLabelForSpecimen(FKeyList[i]);
    end;

    lXMLDoc := TXMLDocument.Create(nil);
    // Run stored proc and retrieve data as XML
    lXML := dmGeneral.GetXMLData('specimens', 'usp_SpecimensMailMerge_Select', []);
    lXMLDoc.LoadFromXML(lXML);

    // Now, we should have a filled up XMLDoc now.
    // Add the reference to the stylesheet.
    lXMLNode := lXMLDoc.CreateNode('xml-stylesheet', ntProcessingInstr,
        Format('type="text/xsl" href="%s"', [eStyleSheet.Text]));
    lXMLDoc.ChildNodes.Insert(1, lXMLNode);

    // Get the correct Barcode type to use.
    with dmGeneral.GetRecordset('usp_Barcode_Get', []) do
      BarCode.Typ := Fields['Data'].Value;

    // Add date to XML.
    lXMLNode := lXMLDoc.ChildNodes.FindNode('specimens');
    lXMLNode.Attributes['Date'] := DateToStr(Date);
    // Need to find all nodes with a registration number and generate a bitmap
    // file of the associated barcode, where applicable.
    lXMLNode := lXMLNode.ChildNodes.First;
    while Assigned(lXMLNode) do begin
      lFileName := GetBarCodeFileName(VarToStr(lXMLNode.Attributes['RegNo']));
      // If there is a file to save, also update the node's attibute.
      if lFileName <> '' then
        lXMLNode.Attributes['Barcode'] := lFileName;
      lXMLNode := lXMLNode.NextSibling;
    end;

    lXMLDoc.SaveToFile(eOutputFolder.Text + eReportFileName.Text);

    ShellFile(eOutputFolder.Text + eReportFileName.Text);

    // Finish with temp table, get rid of it.
    dmGeneral.ExecuteSQL(SQL_DROP_TEMP_SPECIMEN_KEYS);

    Result := True;
  finally
    lXMLDoc := nil;
    DefaultCursor(lCursor);
  end;
end;  // TdlgSpecimenMailMerge.GenerateOutput

{-------------------------------------------------------------------------------
}
function TdlgSpecimenMailMerge.GetBarCodeFileName(const ARegNo: String): String;
var
  lBmp: TBitmap;
  lFileName: String;
begin
  Result := '';
  if ARegNo <> '' then begin
    lBmp := TBitmap.Create;
    try
      try
        lBmp.Height := 40;
        with Barcode do begin
          Height := 40;
          Top := 5;
          Left := 5;
        end;
        Barcode.Text := ARegNo;
        lBmp.Width := Barcode.Width + 10;
  
        Barcode.DrawBarcode(lBmp.Canvas);
        lBMp.Height := lBmp.Height + 5;
  
        lFileName := eOutputFolder.Text +
                     Copy(eReportFileName.Text, 1, Length(eReportFileName.Text) - 4) +
                     ARegNo + '.bmp';
        lBmp.SaveToFile(lFileName);
        Result := lFileName;
      except
        // Don't want to raise the exception about invalid barcode. Just hide it.
        on E:Exception do;
      end;
    finally
      lBmp.Free;
    end;
  end;
end;  // TdlgSpecimenMailMerge.GetBarCodeFileName 

{-------------------------------------------------------------------------------
}
procedure TdlgSpecimenMailMerge.TextChanged(Sender: TObject);
begin
  UpdateButtonState;
end;  // TdlgSpecimenMailMerge.TextChanged 

{-------------------------------------------------------------------------------
}
procedure TdlgSpecimenMailMerge.UpdateButtonState;
begin
  btnOk.Enabled := FileExists(eStyleSheet.Text) and
                   SameText(ExtractFileExt(eStyleSheet.Text), '.xsl') and
                   (eOutputFolder.Text <> '') and
                   (eReportFileName.Text <> '');
end;  // TdlgSpecimenMailMerge.UpdateButtonState 

end.



