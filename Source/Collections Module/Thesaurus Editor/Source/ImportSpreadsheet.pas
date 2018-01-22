{===============================================================================
  Unit:        ImportSpreadsheet

  Defines:     TdlgImport

  Description:

  Model:       ThesaurusEditor.mpb

  Created:     November 2003

  Last revision information:
    $Revision: 40 $
    $Date: 26/05/11 15:50 $
    $Author: Andrewkemp $

===============================================================================}

unit ImportSpreadsheet;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ImageListButton, ExtCtrls, Grids, DssStringGrid,
  InterfaceDataModule, DomainConceptGroupSelector, DB, ADODB, SMIBase,
  SMI2Cell, SMI2XLS, SMI2TXT, DataClasses, DataTypes, StrIntList, ExceptionForm,
  ComboListID, LuxIDComboBox, VagueDate, ApplicationSettings,
  ConceptGroupQualityChecker;

type
  EImportSpreadsheet = class(TExceptionPath)
  end;
  
  {-----------------------------------------------------------------------------
    Dialog allowing the user to select a spreadsheet (csv or Microsoft Excel
    format) to import into the Thesaurus as a list of concepts in a concept
    group
  }
  TdlgImportSpreadsheet = class(TForm)
    Bevel1: TBevel;
    btnCancel: TImageListButton;
    btnImport: TImageListButton;
    btnOpenFile: TButton;
    chkColumnTitles: TCheckBox;
    cmbFields: TComboBox;
    csvFile: TSMImportFromText;
    dlgOpen: TOpenDialog;
    eFileName: TEdit;
    excelFile: TSMImportFromXLS;
    fraImportDomainConceptGroupsSelector: TfraDomainConceptGroupsSelector;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    pnlCover: TPanel;
    sgMap: TDSSStringGrid;
    dsImport: TADOTable;
    chkConceptIntroducedVersion: TCheckBox;
    chkConceptExpiredVersion: TCheckBox;
    cmbConceptIntroducedVersion: TLuxIDComboBox;
    cmbConceptExpiredVersion: TLuxIDComboBox;
    fraSynonymDomainConceptGroupsSelector: TfraDomainConceptGroupsSelector;
    chkUsePrimarySynonymGroup: TCheckBox;
    procedure btnImportClick(Sender: TObject);
    procedure btnOpenFileClick(Sender: TObject);
    procedure chkColumnTitlesClick(Sender: TObject);
    procedure cmbFieldsKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure cmbFieldsSelect(Sender: TObject);
    procedure eFileNameChange(Sender: TObject);
    procedure fraDomainConceptGroupsSelectorcmbConceptGroupsClick(Sender: TObject);
    procedure fraDomainConceptGroupsSelectorcmbDomainsSelect(Sender: TObject);
    procedure sgMapCellSelectedCustom(Sender: TObject; ACol, ARow: Integer; WinControl: TWinControl);
    procedure ImportObjectAfterRecord(Sender: TObject; var Abort: Boolean);
    procedure cmbVersionPopulate(Sender: TObject);
    procedure chkConceptIntroducedVersionClick(Sender: TObject);
    procedure chkConceptExpiredVersionClick(Sender: TObject);
    procedure cmbConceptVersionChange(Sender: TObject);
    procedure chkUsePrimarySynonymGroupClick(Sender: TObject);
    procedure fraSynonymDomainConceptGroupsSelectorcmbConceptGroupsClick(
      Sender: TObject);
  private
    FDuplicateColumn: String;
    FMessagesExist: Boolean;
    FMessagesFailed: Boolean;
    FRejectsExist: Boolean;
    FRejectsFailed: Boolean;
    FSequenceList: TStringList;
    FConceptFromVersionDate: TVagueDate;
    FConceptToVersionDate: TVagueDate;
    procedure LogError(const Message: string);
    procedure LogMessage(const AMessage: string);
    procedure LogWarning(const Message: string);
    procedure UpdateConceptGroupVersions;
  protected
    FConceptKeys: array of TKeyString;
    FFileName: string;
    FImportData: _Recordset;
    FImportObject: TSMImportBaseComponent;
    FMessages: TextFile;
    FRejects: TextFile;
    FRowNumbers: TStringKeyedIntegerList;
    FTempPath: string;
    FLastProgressUpdate: TDateTime;
    function CheckMappings: Boolean; virtual;
    function CleanFieldName(const Value: string): string; virtual;
    function CleanValue(const Value: Variant): Variant; virtual;
    procedure CreateMappingsForImport; virtual;
    procedure CreateStructureForFieldNames(Sender: TObject; Columns: TSMIColumns); virtual;
    procedure CreateStructureForImport(Sender: TObject; Columns: TSMIColumns); virtual;
    procedure DisplayCompletionMessage; virtual;
    procedure DoImport; virtual;
    procedure DropImportTable; virtual;
    function FieldDescriptionToName(const Description: string): string; virtual;
    function GetConceptKey(const TermName: Variant): Variant;
    function GetMessagesFileName: string; virtual;
    function GetRejectsFileName: string; virtual;
    function GetTempPath: string; virtual;
    function GroupHasHierarchy: Boolean; virtual;
    function ImportRow: TKeyString; virtual;
    procedure ProcessFile(const FileName: string); virtual;
    procedure RejectRow; virtual;
    procedure ResolvePotentialSynonyms(const Timestamp: TSQLSvrTimestamp); virtual;
    function ResolveRelationship(const FieldName: string): Variant;
    function SaveMessages: Boolean; virtual;
    function SaveRejects: Boolean; virtual;
    procedure SetConceptKey(const Value: string); virtual;
    procedure SetFileName(const Value: string); virtual;
    procedure UpdateButtons; virtual;
    procedure UpdateMap; virtual;
    procedure ValidateFieldNames; virtual;
    procedure ViewFile(const FileName: string); virtual;
    property FileName: string read FFileName write SetFileName;
    property ImportObject: TSMImportBaseComponent read FImportObject;
    property MessagesFileName: string read GetMessagesFileName;
    property RejectsFileName: string read GetRejectsFileName;
    property TempPath: string read GetTempPath;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

//==============================================================================
implementation

{$R *.dfm}

uses
  ResourceStrings, GeneralData, ComObj, ApiUtils, FileUtils,
  ThesaurusEditorMain, BaseADODataModule, GeneralFunctions, EasyShell, AdoInt,
  LuxembourgConstants;

resourcestring
  ResStr_SpreadsheetLoading = 'Loading Spreadsheet...';
  ResStr_MappingColumns     = '"Columns in file","Fields in database"';

const
  TEMP_DIR = 'ThesaurusEditor';
  DEAD_CONCEPT_KEY = '** DEAD **';
  REJECTED_CONCEPT_KEY = '** FAIL **';
  SMI_TABLE_NAME = '#SMImport';
  SMI_TABLE_SQL = 'CREATE TABLE ' + SMI_TABLE_NAME + ' ('
      + ' author VARCHAR(100) NULL,'
      + ' child_of NVARCHAR(150) NULL,'
      + ' citation_date VARCHAR(100) NULL,'
      + ' rank VARCHAR(100) NULL,'
      + ' fact_#1_title VARCHAR(100) NULL,'
      + ' fact_#1_type NVARCHAR(150) NULL,'
      + ' fact_#1_description TEXT NULL,'
      + ' fact_#2_title VARCHAR(100) NULL,'
      + ' fact_#2_type NVARCHAR(150) NULL,'
      + ' fact_#2_description TEXT NULL,'
      + ' fact_#3_title VARCHAR(100) NULL,'
      + ' fact_#3_type NVARCHAR(150) NULL,'
      + ' fact_#3_description TEXT NULL,'
      + ' fact_#4_title VARCHAR(100) NULL,'
      + ' fact_#4_type NVARCHAR(150) NULL,'
      + ' fact_#4_description TEXT NULL,'
      + ' fact_#5_title VARCHAR(100) NULL,'
      + ' fact_#5_type NVARCHAR(150) NULL,'
      + ' fact_#5_description TEXT NULL,'
      + ' designation_#1_status NVARCHAR(150) NULL,'
      + ' designation_#1_start_date VARCHAR(100) NULL,'  // See ADDITIONAL_VAGUE_DATE_FIELDS below.
      + ' designation_#1_end_date VARCHAR(100) NULL,'
      + ' designation_#2_status NVARCHAR(150) NULL,'
      + ' designation_#2_start_date VARCHAR(100) NULL,'
      + ' designation_#2_end_date VARCHAR(100) NULL,'
      + ' list_code VARCHAR(50) NULL,'
      + ' name_type NVARCHAR(150) NULL,'
      + ' sort_code INT NULL,'
      + ' synonym_of NVARCHAR(150) NULL,'
      + ' language VARCHAR(50) NULL,'
      + ' language_key VARCHAR(4) NULL,'
      + ' term_name NVARCHAR(150) NULL,'
      + ' Concept_Key CHAR(16) NULL)';

  // The following is the number of vague date fields in the table above that have to be
  // "exploded" into their component parts before being pushed to the DB (_Start/_End/_Type).
  // Need 2 extra fields for each date field mapped to a vague date field. Should be clear enough.
  ADDITIONAL_VAGUE_DATE_FIELDS = 8;

{-==============================================================================
    TdlgImportSpreadsheet
===============================================================================}
{-------------------------------------------------------------------------------
}
constructor TdlgImportSpreadsheet.Create(AOwner: TComponent);
begin
  inherited;
  FRowNumbers := TStringKeyedIntegerList.Create(False);
  sgMap.Rows[0].CommaText := ResStr_MappingColumns;
  sgMap.ColumnsInfo[1].WinControl := cmbFields;
  sgMap.ColWidths[1] := sgMap.ClientWidth - sgMap.ColWidths[0] - 2;
  fraImportDomainConceptGroupsSelector.PopulateDomainCombo;
  fraSynonymDomainConceptGroupsSelector.PopulateDomainCombo;
  dsImport.TableName := SMI_TABLE_NAME;
  FSequenceList := TStringList.Create;
end;  // TdlgImportSpreadsheet.Create

{-------------------------------------------------------------------------------
}
destructor TdlgImportSpreadsheet.Destroy;
begin
  FSequenceList.Free;
  FRowNumbers.Free;
  inherited;
end;  // TdlgImportSpreadsheet.Destroy 

{-------------------------------------------------------------------------------
}
procedure TdlgImportSpreadsheet.btnImportClick(Sender: TObject);
var
  Timestamp: TSQLSvrTimestamp;
begin
  if CheckMappings then
  begin
    Screen.Cursor := crHourGlass;
    try
      Timestamp := dmGeneral.GetStoredProcOutputParam(
                          'usp_ConceptGroup_GetLatestTimestamp',
                          ['@concept_group_key',
                           fraImportDomainConceptGroupsSelector.ConceptGroupKey],
                          '@timestamp');                          
      DoImport;
    finally
      Screen.Cursor := crDefault;
      frmThesaurusEditor.SetProgress(0, 100);
    end;

    try
      ResolvePotentialSynonyms(Timestamp);
    finally
      ModalResult := mrOk;
      DisplayCompletionMessage;
    end;
  end;
end;  // TdlgImportSpreadsheet.btnImportClick 

{-------------------------------------------------------------------------------
}
procedure TdlgImportSpreadsheet.btnOpenFileClick(Sender: TObject);
begin
  if dlgOpen.Execute then eFileName.Text := dlgOpen.FileName;
end;  // TdlgImportSpreadsheet.btnOpenFileClick 

{-------------------------------------------------------------------------------
  Validate column mappings specified by the user.  Displays a message and
          returns False if they are unacceptable, otherwise True.
}
function TdlgImportSpreadsheet.CheckMappings: Boolean;
var
  row: Integer;
  fieldName: string;
  termNameSelected: Boolean;
  languageCount: Integer;
  childOfSelected: Boolean;

  function HasOtherMapping: Boolean;
  var
    i: Integer;
  begin
    Result := False;
    for i := 1 to sgMap.RowCount - 1 do
      if (i <> row) and (sgMap.Cells[1, i] = sgMap.Cells[1, row]) then
      begin
        Result := True;
        Break;
      end;
  end;

begin
  Result := False;
  termNameSelected := False;
  languageCount := 0;
  childOfSelected := False;

  for row := 1 to sgMap.RowCount - 1 do
  begin
    fieldName := FieldDescriptionToName(sgMap.Cells[1, row]);

    if (fieldName <> '') and HasOtherMapping then
    begin
      ShowMessage(Format(ResStr_DuplicateFieldMapping, [sgMap.Cells[1, row]]));
      Exit;
    end;

    if fieldName = 'term_name' then
      termNameSelected := True
    else if (fieldName = 'language') or (fieldName = 'language_key') then
      Inc(languageCount)
    else if fieldName = 'child_of' then
      childOfSelected := True;
  end;

  if not termNameSelected then
    ShowMessage(ResStr_NoTermMapping)
  else if languageCount <> 1 then
    ShowMessage(ResStr_BadLanguageMapping)
  else if childOfSelected and not GroupHasHierarchy then
    ShowMessage(ResStr_GroupHasNoHierarchy)
  else
    Result := True;
end;  // TdlgImportSpreadsheet.CheckMappings 

{-------------------------------------------------------------------------------
}
procedure TdlgImportSpreadsheet.chkColumnTitlesClick(Sender: TObject);
begin
  ProcessFile(FFileName);
end;  // TdlgImportSpreadsheet.chkColumnTitlesClick

{-------------------------------------------------------------------------------
  Value with leading and trailing spaces removed, and '""' replaced with '"'.
          If result would be empty then '<no name>'.
}
function TdlgImportSpreadsheet.CleanFieldName(const Value: string): string;
begin
  Result := CleanValue(Value);
  if Result = '' then Result := ResStr_NoColumnName;
end;  // TdlgImportSpreadsheet.CleanFieldName 

{-------------------------------------------------------------------------------
  Value with leading and trailing spaces removed, and '""' replaced with '"'. 
}
function TdlgImportSpreadsheet.CleanValue(const Value: Variant): Variant;
begin
  Result := StringReplace(Trim(VarToStr(Value)), '""', '"', [rfReplaceAll]);
  Result := StringReplace(Result, #39, #39#39, [rfReplaceAll]);
end;  // TdlgImportSpreadsheet.CleanValue 

{-------------------------------------------------------------------------------
  Clear selection when Delete is pressed. 
}
procedure TdlgImportSpreadsheet.cmbFieldsKeyDown(Sender: TObject; var Key: Word;
        Shift: TShiftState);
begin
  case Key of
    VK_DELETE, VK_BACK: begin
      Key := 0;
      cmbFields.ItemIndex := -1;
      cmbFieldsSelect(cmbFields);
    end;
  end;
end;  // TdlgImportSpreadsheet.cmbFieldsKeyDown 

{-------------------------------------------------------------------------------
}
procedure TdlgImportSpreadsheet.cmbFieldsSelect(Sender: TObject);
begin
  sgMap.Cells[sgMap.Col, sgMap.Row] := cmbFields.Text;
end;  // TdlgImportSpreadsheet.cmbFieldsSelect

{ ------------------------------------------------------------------------------
}
procedure TdlgImportSpreadsheet.cmbVersionPopulate(Sender: TObject);
var
  fillSequenceList: Boolean;
begin
  inherited;

  fillSequenceList := FSequenceList.Count = 0;

  with dmGeneral.GetRecordset(
      'usp_ConceptGroupVersions_Select_ForConceptGroup',
      ['@ParentKey', fraImportDomainConceptGroupsSelector.ConceptGroupKey]) do
  begin
    while not Eof do begin
      if Sender = cmbConceptIntroducedVersion then
        cmbConceptIntroducedVersion.Add(
            VarToStr(Fields['Item_Name'].Value),
            VarToStr(Fields['Key'].Value))
      else
        cmbConceptExpiredVersion.Add(
            VarToStr(Fields['Item_Name'].Value),
            VarToStr(Fields['Key'].Value));

      if fillSequenceList then
        FSequenceList.Add(Fields['Key'].Value + '=' + VarToStr(Fields['Sequence'].Value));

      MoveNext;
    end;
    Close;
  end;
end;  // TdlgImportSpreadsheet.cmbVersionPopulate

{-------------------------------------------------------------------------------
  Set up mappings in ImportObject as specified by the user in sgMap. 
}
procedure TdlgImportSpreadsheet.CreateMappingsForImport;
var
  i: Integer;
  fieldName: string;
begin
  ImportObject.Mappings.Clear;
  for i := 1 to sgMap.RowCount - 1 do
  begin
    fieldName := FieldDescriptionToName(sgMap.Cells[1, i]);
    if fieldName <> '' then
    begin
      if ImportObject is TSMImportFromText then
        // fields in CSV file are named 'Field1', 'Field2', ...
        ImportObject.Mappings.Add(Format('%s=Field%d', [fieldName, i]))
      else
        // fields in XLS file are named 'A', 'B', ...
        ImportObject.Mappings.Add( Format('%s=%s', [fieldName, Chr(Ord('A') + i - 1)]));
    end;
  end;
end;  // TdlgImportSpreadsheet.CreateMappingsForImport 

{-------------------------------------------------------------------------------
  Create structure in dsImport with field names taken from Columns. 
}
procedure TdlgImportSpreadsheet.CreateStructureForFieldNames(Sender: TObject; Columns: TSMIColumns);
var
  i, j: Integer;
  lColName, lSql: String;
begin
  dsImport.Close;

  // If first row supposed to have column names, check for duplicates, in case first
  // row actually doesn't have column names!
  if chkColumnTitles.Checked and (Columns.Count > 0) then
    for i := 0 to Columns.Count - 2 do begin
      lColName := CleanFieldName(Columns[i].FieldName);
      for j := i + 1 to Columns.Count - 1 do
        if SameText(lColName, CleanFieldName(Columns[j].FieldName)) then begin
          // SMImport seems to "swallow" exceptions, so have to use private var!
          FDuplicateColumn := lColName;
          Exit;
        end;
    end;

  lSql := 'CREATE TABLE ' + SMI_TABLE_NAME + ' (';
  for i := 0 to Columns.Count - 1 do
  begin
    if i > 0 then lSql := lSql + ', ';
    if chkColumnTitles.Checked then
      lSql := lSql + '"' + CleanFieldName(Columns[i].FieldName) + '" CHAR(1)'
    else
      lSQL := lSQL + '"' + Columns[i].Caption + '" CHAR(1)'
  end;
  lSql := lSql + ')';

  dmGeneral.ExecuteSql(lSql);

  dsImport.Connection := dmGeneral.Connection;
  dsImport.Open;
end;  // TdlgImportSpreadsheet.CreateStructureForFieldNames

{-------------------------------------------------------------------------------
  Create structure in dsImport that will be used to read the data from the
          spreadsheet.
}
procedure TdlgImportSpreadsheet.CreateStructureForImport(Sender: TObject; Columns: TSMIColumns);
begin
  dsImport.Close;

  dmGeneral.ExecuteSql(SMI_TABLE_SQL);

  dsImport.Connection := dmGeneral.Connection;
  dsImport.Open;
  
  CreateMappingsForImport;
end;  // TdlgImportSpreadsheet.CreateStructureForImport 

{-------------------------------------------------------------------------------
  Display message informing user that import has completed. Gives details of
          where to look for problems that occured.
}
procedure TdlgImportSpreadsheet.DisplayCompletionMessage;
var
  Message: string;
  SavedRejects, SavedMessages: Boolean;
begin
  Message := Format(ResStr_SpreadsheetImportComplete, [FileName]);
  SavedRejects := False;
  SavedMessages := False;

  if FRejectsExist then SavedRejects := SaveRejects;
  if SavedRejects then
      Message := Message + #10#10 + Format(
                                        ResStr_SpreadsheetImportRejects,
                                        [RejectsFileName]);
  
  if FMessagesExist then SavedMessages := SaveMessages;
  if SavedMessages then
      Message := Message + #10#10 + Format(
                                        ResStr_SpreadsheetImportMessages,
                                        [MessagesFileName]);
  
  if not (SavedRejects or SavedMessages) then
    ShowMessage(Message)
  else
  begin
    if (SavedRejects and SavedMessages) then
      Message := Message + #10#10 + ResStr_ViewTheseFiles
    else
      Message := Message + #10#10 + ResStr_ViewThisFile;
  
    if MessageDlg(Message, mtWarning, [mbYes, mbNo], 0) = mrYes then
    begin
      if SavedRejects then ViewFile(RejectsFileName);
      if SavedMessages then ViewFile(MessagesFileName);
    end;
  end;
end;  // TdlgImportSpreadsheet.DisplayCompletionMessage 

{-------------------------------------------------------------------------------
  Perform the spreadsheet import according to the user's specification.
}
procedure TdlgImportSpreadsheet.DoImport;
var
  lTime: TDateTime;
  lRecordCount: Integer;
begin
  FRowNumbers.Clear;
  FMessagesExist := False;
  FMessagesFailed := False;
  FRejectsExist := False;
  FRejectsFailed := False;

  if chkColumnTitles.Checked then
    ImportObject.RowFirst := 2
  else
    ImportObject.RowFirst := 1;

  ImportObject.RowLast := High(Integer);
  ImportObject.Mappings.Clear;
  ImportObject.OnCreateStructure := CreateStructureForImport;
  frmThesaurusEditor.SetProgress(0, 100);
  FLastProgressUpdate := Now;
  frmThesaurusEditor.SetStatus(ResStr_SpreadsheetLoading);
  try
    try
      ImportObject.Execute;
    except
      on E:Exception do
      begin
        // Abort if error opening file, display non-critical message
        frmThesaurusEditor.SetStatus('');
        frmThesaurusEditor.SetProgress(0, 100);
        raise EImportSpreadsheet.CreateNonCritical(E.Message);
      end;
    end;
                       
    frmThesaurusEditor.SetStatus(ResStr_SpreadsheetImporting);
    try
      FImportData := dmGeneral.ExecuteSql(
          'SELECT * FROM ' + SMI_TABLE_NAME,
          True);
      lRecordCount := FImportData.RecordCount;
      SetLength(FConceptKeys, lRecordCount);

      frmThesaurusEditor.SetProgress(0, lRecordCount);

      // Record the time so we can redraw everything every 5 seconds
      lTime := Now;

      // Get dates for concept history. No need to do it more than once.
      if chkConceptIntroducedVersion.Checked and
         (cmbConceptIntroducedVersion.CurrentStrId <> '') then
        with dmGeneral.GetRecordset(
            'usp_ConceptGroupVersion_Select',
            ['@Key', cmbConceptIntroducedVersion.CurrentStrID]) do
          if not Eof then begin
            if not VarIsNull(Fields['From_Vague_Date_Start'].Value) then
              FConceptFromVersionDate.StartDate    := Fields['From_Vague_Date_Start'].Value;
            if not VarIsNull(Fields['From_Vague_Date_End'].Value) then
              FConceptFromVersionDate.EndDate      := Fields['From_Vague_Date_End'].Value;
            FConceptFromVersionDate.DateTypeString := Fields['From_Vague_Date_Type'].Value;
            Close;
          end;

      if chkConceptExpiredVersion.Checked and
         (cmbConceptExpiredVersion.CurrentStrId <> '') then
        with dmGeneral.GetRecordset(
            'usp_ConceptGroupVersion_Select',
            ['@Key', cmbConceptExpiredVersion.CurrentStrID]) do
          if not Eof then begin
            if not VarIsNull(Fields['From_Vague_Date_Start'].Value) then
              FConceptToVersionDate.StartDate    := Fields['From_Vague_Date_Start'].Value;
            if not VarIsNull(Fields['From_Vague_Date_End'].Value) then
              FConceptToVersionDate.EndDate      := Fields['From_Vague_Date_End'].Value;
            FConceptToVersionDate.DateTypeString := Fields['From_Vague_Date_Type'].Value;
            Close;
          end;

      with FImportData do
        while not Eof do      
        begin
          ImportRow;
          frmThesaurusEditor.SetProgress(AbsolutePosition, lRecordCount);
          MoveNext;
          // if more than 5 seconds since last processmessages
          if Now > lTime + 1 / (24 * 60 * 12) then 
          begin
            Application.ProcessMessages;
            lTime := Now;
          end;
        end;
    finally
      frmThesaurusEditor.SetStatus('');
      FImportData := nil;
      SetLength(FConceptKeys, 0);
    end;
  finally
    DropImportTable;
  end;
end;  // TdlgImportSpreadsheet.DoImport 

{-------------------------------------------------------------------------------
}
procedure TdlgImportSpreadsheet.DropImportTable;
var
  lSql: String;
begin
  dsImport.Close;
  lSql := 'IF object_id(''tempdb..' + SMI_TABLE_NAME + ''') IS NOT NULL'
      + ' DROP TABLE ' + SMI_TABLE_NAME;
  dmGeneral.ExecuteSql(lSql);  
end;

{-------------------------------------------------------------------------------
}
procedure TdlgImportSpreadsheet.eFileNameChange(Sender: TObject);
begin
  FileName := eFileName.Text;
end;  // TdlgImportSpreadsheet.eFileNameChange 

{-------------------------------------------------------------------------------
  Convert a field description from cmbFields to a field name in dsImport. 
}
function TdlgImportSpreadsheet.FieldDescriptionToName(const Description: String): String;
begin
  Result := LowerCase(StringReplace(Description, ' ', '_', [rfReplaceAll]));
end;  // TdlgImportSpreadsheet.FieldDescriptionToName

{-------------------------------------------------------------------------------
}
procedure TdlgImportSpreadsheet.fraDomainConceptGroupsSelectorcmbConceptGroupsClick(
    Sender: TObject);
begin
  UpdateConceptGroupVersions;
  UpdateButtons;
end;  // TdlgImportSpreadsheet.fraDomainConceptGroupsSelectorcmbConceptGroupsClick

{-------------------------------------------------------------------------------
}
procedure TdlgImportSpreadsheet.fraDomainConceptGroupsSelectorcmbDomainsSelect(Sender: TObject);
begin
  fraImportDomainConceptGroupsSelector.cmbDomainsSelect(Sender);
  UpdateButtons;
end;  // TdlgImportSpreadsheet.fraDomainConceptGroupsSelectorcmbDomainsSelect 

{-------------------------------------------------------------------------------
  Key of the concept in the spreadsheet corresponding to the named term, or
          Null if there is no such concept. The concept is immediately imported
          if necessary.  May also return DEAD_CONCEPT_KEY or
          REJECTED_CONCEPT_KEY.
}
function TdlgImportSpreadsheet.GetConceptKey(const TermName: Variant): Variant;
var
  Bookmark: Variant;
begin
  Bookmark := FImportData.Bookmark;
  try
    FImportData.MoveFirst;
    FImportData.Find(
        'term_name = ''' + CleanValue(TermName) + '''',  // Make sure apostrophes don't break it!!!
        0,
        adSearchForward,
        EmptyParam);
    if FImportData.Bof or FImportData.Eof then
      Result := Null
    else if FConceptKeys[FImportData.AbsolutePosition - 1] <> '' then
      Result := FConceptKeys[FImportData.AbsolutePosition - 1]
    else
      Result := ImportRow;
  finally
    FImportData.Bookmark := Bookmark;
  end;
end;  // TdlgImportSpreadsheet.GetConceptKey 

{-------------------------------------------------------------------------------
  Fully qualified name of file used to log warnings/errors. 
}
function TdlgImportSpreadsheet.GetMessagesFileName: string;
begin
  Result := TempPath + ChangeFileExt(ExtractFileName(FileName), '.log');
end;  // TdlgImportSpreadsheet.GetMessagesFileName

{-------------------------------------------------------------------------------
  Fully qualified name of file used to log rejected rows. 
}
function TdlgImportSpreadsheet.GetRejectsFileName: string;
begin
  Result := TempPath
      + StringReplace(ExtractFileName(FileName), '.', '_', [rfReplaceAll])
      + '_rejects.csv'
end;  // TdlgImportSpreadsheet.GetRejectsFileName 

{-------------------------------------------------------------------------------
  Path for application temporary files. 
}
function TdlgImportSpreadsheet.GetTempPath: string;
begin
  if FTempPath = '' then
  begin
    FTempPath := ExpandLongPathName(gfnGetTempPath + TEMP_DIR + '\');
    if not DirectoryExists(FTempPath) then CreateDir(FTempPath);
  end;
  Result := FTempPath;
end;  // TdlgImportSpreadsheet.GetTempPath 

{-------------------------------------------------------------------------------
  Does the selected concept group have a hierarchical relationship?
}
function TdlgImportSpreadsheet.GroupHasHierarchy: Boolean;
begin
  Result := (dmGeneral.GetStoredProcOutputParam(
                    'usp_HierarchyRelationTypeKey_Get_ForConceptGroup',
                    ['@Key', fraImportDomainConceptGroupsSelector.ConceptGroupKey],
                    '@HierarchyRelationTypeKey')
                    <> Null);
end;  // TdlgImportSpreadsheet.GroupHasHierarchy 

{-------------------------------------------------------------------------------
  Import the current row from FImportData as a new Concept, unless it has already
          been processed. The new concept key is written back to the current
          row of the dataset. Returns the new concept key, or possibly
          DEAD_CONCEPT_KEY or REJECTED_CONCEPT_KEY.
}
function TdlgImportSpreadsheet.ImportRow: TKeyString;
var
  params: Array of Variant;
  idxField, idxParam: Integer;
  fieldName: String;
  vagueDateValue: TVagueDate;
begin
  if FConceptKeys[FImportData.AbsolutePosition - 1] <> '' then
    Result := FConceptKeys[FImportData.AbsolutePosition - 1]
  else
  begin
    SetConceptKey(DEAD_CONCEPT_KEY);  // protect against cyclic relationships

    SetLength(params, 2 * (FImportData.Fields.Count + ADDITIONAL_VAGUE_DATE_FIELDS));
    params[0] := '@concept_group_key';
    params[1] := fraImportDomainConceptGroupsSelector.ConceptGroupKey;

    idxParam := 0;
    for idxField := 0 to FImportData.Fields.Count - 2 do // <= -2 coz we don't need @Concept_Key in params.
    begin
      fieldName := FImportData.Fields[idxField].Name;

      // Handle designation dates differently to other fields.
      if (Pos('designation_#', fieldName) > 0) and (Pos('_date', fieldName) > 0) then
      begin
        params[2 + 2 * idxParam] := '@' + fieldName + '_start';
        params[4 + 2 * idxParam] := '@' + fieldName + '_end';
        params[6 + 2 * idxParam] := '@' + fieldName + '_type';
        // If not status, ignore the date, otherwise, break it down.
        if not VarIsNull(FImportData.Fields[Copy(fieldName, 1, 15) + 'status'].Value) and
           not VarIsNull(FImportData.Fields[idxField].Value) then
        begin
          vagueDateValue := StringToVagueDate(FImportData.Fields[idxField].Value);
          params[3 + 2 * idxParam] := vagueDateValue.StartDate;
          params[5 + 2 * idxParam] := vagueDateValue.EndDate;
          params[7 + 2 * idxParam] := vagueDateValue.DateTypeString;
        end;
        Inc(idxParam, 2);
      end else begin
      // Standard/normal field
        params[2 + 2 * idxParam] := '@' + fieldName;

        if (fieldName = 'synonym_of') or (fieldName = 'child_of') then
          params[3 + 2 * idxParam] := ResolveRelationship(fieldName)
        else
          params[3 + 2 * idxParam] := VarToStr(FImportData.Fields[idxField].Value);
      end;
      Inc(idxParam);
    end;

    try
      Result := dmGeneral.RunInsertStoredProc(
                        'Concept',
                        'usp_Concept_InsertSpreadsheetRow',
                        params,
                        '@concept_key');

      // If concept versions have been selected, add a Concept_History record.
      if not SameText(Result, DEAD_CONCEPT_KEY) and
         ((chkConceptIntroducedVersion.Checked and (cmbConceptIntroducedVersion.CurrentStrId <> '')) or
          (chkConceptExpiredVersion.Checked and (cmbConceptExpiredVersion.CurrentStrId <> ''))) then
      begin
        dmGeneral.RunInsertStoredProc(
            TN_CONCEPT_HISTORY,
            'usp_ConceptHistory_Insert',
            ['@ConceptKey', Result,
             '@ConceptGroupVersionFromKey', IIf(chkConceptIntroducedVersion.Checked,
                                                cmbConceptIntroducedVersion.CurrentStrID,
                                                ''),
             '@ConceptGroupVersionToKey', IIf(chkConceptExpiredVersion.Checked,
                                              cmbConceptExpiredVersion.CurrentStrID,
                                              ''),
             '@FromVagueDateStart', FConceptFromVersionDate.StartDate,
             '@FromVagueDateEnd', FConceptFromVersionDate.EndDate,
             '@FromVagueDateType', FConceptFromVersionDate.DateTypeString,
             '@ToVagueDateStart', FConceptToVersionDate.StartDate,
             '@ToVagueDateEnd', FConceptToVersionDate.EndDate,
             '@ToVagueDateType', FConceptToVersionDate.DateTypeString],
            '@Key');
      end;
      FRowNumbers.Add(Result, FImportData.AbsolutePosition);
    except
      on E: EOleException do begin
        LogError(E.Message);
        Result := REJECTED_CONCEPT_KEY;
        RejectRow;
      end;
    end;

    SetConceptKey(Result);
  end;
end;  // TdlgImportSpreadsheet.ImportRow 

{-------------------------------------------------------------------------------
  Write an error log entry to FMessages for the current row of FImportData.
}
procedure TdlgImportSpreadsheet.LogError(const Message: string);
begin
  LogMessage(Format(
      ResStr_SpreadsheetImportError,
      [FImportData.Fields['Term_Name'].Value, Message]));
end;  // TdlgImportSpreadsheet.LogError 

{-------------------------------------------------------------------------------
  Log any generic message. 
}
procedure TdlgImportSpreadsheet.LogMessage(const AMessage: string);
begin
  if FMessagesFailed then Exit;
  try
    // Create log file if not yet present
    if not FMessagesExist then begin
      FMessagesExist := True;
      AssignFile(FMessages, MessagesFileName);
      Rewrite(FMessages);
      WriteLn(
          FMessages, 
          Format(ResStr_SpreadsheetImportLogHeader, [FFileName]));
      WriteLn(FMessages, DateTimeToStr(Now));
    end;
    WriteLn(FMessages, AMessage);
  except
    FMessagesFailed := True;
  end;
end;  // TdlgImportSpreadsheet.LogMessage 

{-------------------------------------------------------------------------------
  Write a warning log entry to FMessages for the current row of FImportData.
}
procedure TdlgImportSpreadsheet.LogWarning(const Message: string);
begin
  LogMessage(Format(
      ResStr_SpreadsheetImportWarning,
      [FImportData.Fields['Term_Name'].Value, Message]));
end;  // TdlgImportSpreadsheet.LogWarning 

{-------------------------------------------------------------------------------
  Load field names from the specified file into sgMap. 
}
procedure TdlgImportSpreadsheet.ProcessFile(const FileName: string);
var
  Extension: string;
begin
  FDuplicateColumn := '';
  FImportObject := nil;

  if FileExists(FileName) then
  begin
    Extension := LowerCase(ExtractFileExt(FileName));
    if Extension = '.xls' then
      FImportObject := excelFile
    else if Extension = '.csv' then
      FImportObject := csvFile;
  
    if not Assigned(ImportObject) then
      ShowMessage(ResStr_InvalidSpreadsheetFileType)
    else
    begin
      { load minimum of data to get structure of import file }
      ImportObject.SourceFileName := FileName;
      ImportObject.RowFirst := 2;
      ImportObject.RowLast := 2;
      ImportObject.OnCreateStructure := CreateStructureForFieldNames;
      frmThesaurusEditor.SetProgress(0, 1);
      FLastProgressUpdate := Now;
      frmThesaurusEditor.SetStatus(ResStr_SpreadsheetLoading);
      Screen.Cursor := crHourglass;
      try
        try
          ImportObject.Execute;
        except
          // Relegate file open errors to non-critical
          on E:Exception do begin
            FImportObject := nil;
            eFileName.Text := '';
            if E is EFOpenError then
              raise EImportSpreadsheet.CreateNonCritical(E.Message, E)
            else
              raise;
          end;
        end; // try
      finally
        frmThesaurusEditor.SetProgress(0, 100);
        frmThesaurusEditor.SetStatus('');
        DropImportTable;
        Screen.Cursor := crDefault;
      end;
      if FDuplicateColumn <> '' then begin
        FImportObject := nil;
        UpdateMap;  // This clears the grid and put the "cover" back on.
        raise EImportSpreadSheet.CreateNonCritical(
                  Format(ResStr_DuplicateColumnNames, [FDuplicateColumn]));
      end else
        ValidateFieldNames;
    end;
  end;
  UpdateMap;
end;  // TdlgImportSpreadsheet.ProcessFile 

{-------------------------------------------------------------------------------
  Write a line to FRejects having comma separated fields containing the
          spreadsheet data corresponding to the current row of FImportData.
}
procedure TdlgImportSpreadsheet.RejectRow;
var
  Header: string;
  Reject: string;
  I: Integer;
  FieldName: string;
begin
  if FRejectsFailed then Exit;
  
  Header := '';
  Reject := '';
  for I := 1 to sgMap.RowCount - 1 do
  begin
    FieldName := FieldDescriptionToName(sgMap.Cells[1, I]);
    if FieldName <> '' then
    begin
      if not FRejectsExist then
      begin
        if Header <> '' then Header := Header + ',';
        Header := Header + FieldName;
      end;
      if Reject <> '' then Reject := Reject + ',';
      Reject := Reject 
          + '"' + VarToStr(FImportData.Fields[FieldName].Value) + '"';
    end;
  end;
  try
    if not FRejectsExist then begin
      FRejectsExist := True;
      AssignFile(FRejects, RejectsFileName);
      Rewrite(FRejects);
      if Header<>'' then
        WriteLn(FRejects, Header);
    end;
    WriteLn(FRejects, Reject);
  except
    FRejectsFailed := True;
  end;
end;  // TdlgImportSpreadsheet.RejectRow 

{-------------------------------------------------------------------------------
  Prompt the user to deal with potential synonyms in imported data (concepts
          with timestamp greater then the given value).
}
procedure TdlgImportSpreadsheet.ResolvePotentialSynonyms(const Timestamp: TSQLSvrTimestamp);
var
  lNumberOfPotSynonyms: Integer;
begin
  lNumberOfPotSynonyms := dmGeneral.GetStoredProcOutputParam(
      'usp_PotentialSynonyms_Count',
      ['@ConceptGroupKey', fraImportDomainConceptGroupsSelector.ConceptGroupKey],
      '@RowCount');

  if lNumberOfPotSynonyms > 0 then
    TfrmConceptGroupQualityChecker.Create(
      Owner,
      true,
      fraImportDomainConceptGroupsSelector.ConceptGroupKey,
      '',
      fraSynonymDomainConceptGroupsSelector.ConceptGroupKey,
      0,  //ensures ROWCOUNT = 0, so there is no upper limit on results returned
      Timestamp,
      true,
      AppSettings.SessionID);
end;  // TdlgImportSpreadsheet.ResolvePotentialSynonyms 

{-------------------------------------------------------------------------------
  Resolves the named relationship for the current record in FImportData.  If 
          this is not possible then returns Null and logs an error message.
}
function TdlgImportSpreadsheet.ResolveRelationship(const FieldName: string): Variant;
begin
  if Trim(VarToStr(FImportData.Fields[FieldName].Value))='' then
    Result := Null
  else
  begin
    Result := GetConceptKey(FImportData.Fields[FieldName].Value);
    if Result = Null then
      LogWarning(Format(ResStr_BrokenRelationship, [FieldName]))
    else if Result = REJECTED_CONCEPT_KEY then
    begin
      LogWarning(Format(ResStr_RejectedParent, [FieldName]));
      Result := Null;
    end
    else if Result = DEAD_CONCEPT_KEY then
    begin
      LogWarning(Format(ResStr_CyclicRelationship, [FieldName]));
      Result := Null;
    end;
  end;
end;  // TdlgImportSpreadsheet.ResolveRelationship 

{-------------------------------------------------------------------------------
  Save the contents of FMessages to a file.  True if and only if successful. 
}
function TdlgImportSpreadsheet.SaveMessages: Boolean;
begin
  Result := not FMessagesFailed;

  if Result then
    try
      Flush(FMessages);
      CloseFile(FMessages);
    except
      Result := False;
    end;

  if not Result then
    ShowMessage(Format(ResStr_FailedToSaveLog, [MessagesFileName]));
end;  // TdlgImportSpreadsheet.SaveMessages 

{-------------------------------------------------------------------------------
  Flush the contents of FRejects to a file.  True if and only if successful. 
}
function TdlgImportSpreadsheet.SaveRejects: Boolean;
begin
  Result := not FRejectsFailed;

  if Result then
    try
      Flush(FRejects);
      CloseFile(FRejects);
    except
      Result := False;
    end;

  if not Result then
    ShowMessage(Format(ResStr_FailedToSaveRejects, [RejectsFileName]));
end;  // TdlgImportSpreadsheet.SaveRejects 

{-------------------------------------------------------------------------------
  Set concept key for the current row of FImportData. 
}
procedure TdlgImportSpreadsheet.SetConceptKey(const Value: string);
begin
  FConceptKeys[FImportData.AbsolutePosition - 1] := Value;
end;  // TdlgImportSpreadsheet.SetConceptKey

{-------------------------------------------------------------------------------
}
procedure TdlgImportSpreadsheet.SetFileName(const Value: string);
begin
  FFileName := Value;
  ProcessFile(Value);
end;  // TdlgImportSpreadsheet.SetFileName 

{-------------------------------------------------------------------------------
}
procedure TdlgImportSpreadsheet.sgMapCellSelectedCustom(Sender: TObject; ACol,
        ARow: Integer; WinControl: TWinControl);
begin
  cmbFields.ItemIndex := cmbFields.Items.IndexOf(sgMap.Cells[ACol, ARow]);
end;  // TdlgImportSpreadsheet.sgMapCellSelectedCustom

{-------------------------------------------------------------------------------
  Update enabled state of import button. 
}
procedure TdlgImportSpreadsheet.UpdateButtons;
begin
  btnImport.Enabled :=
      Assigned(ImportObject)
      and (fraImportDomainConceptGroupsSelector.ConceptGroupKey <> '')
      and (not chkConceptIntroducedVersion.Checked or (cmbConceptIntroducedVersion.ItemIndex > -1))
      and (not chkConceptExpiredVersion.Checked or (cmbConceptExpiredVersion.ItemIndex > -1))
      and (not chkUsePrimarySynonymGroup.Checked
          or (fraSynonymDomainConceptGroupsSelector.ConceptGroupKey <> ''));
end;  // TdlgImportSpreadsheet.UpdateButtons

{ ------------------------------------------------------------------------------
}
procedure TdlgImportSpreadSheet.UpdateConceptGroupVersions;
begin
  cmbConceptIntroducedVersion.Clear;
  cmbConceptExpiredVersion.Clear;
  FSequenceList.Clear;
end;  // TdlgImportSpreadSheet.UpdateConceptGroupVersions

{-------------------------------------------------------------------------------
  Update mapping grid according to selected file and options. 
}
procedure TdlgImportSpreadsheet.UpdateMap;
var
  I: Integer;
begin
  sgMap.Enabled := Assigned(ImportObject);
  sgMap.ReadOnly := not Assigned(ImportObject);
  pnlCover.Visible := not Assigned(ImportObject);
  
  if Assigned(ImportObject) then
  begin
    if dsImport.FieldDefs.Count = 0 then
      sgMap.RowCount := 2  // Want to keep fixed row!!!!!
    else
      sgMap.RowCount := dsImport.FieldDefs.Count + 1;
    for I := 0 to dsImport.FieldDefs.Count - 1 do
    begin
      if chkColumnTitles.Checked then
        sgMap.Cells[0, I + 1] := dsImport.FieldDefs[I].Name
      else
        sgMap.Cells[0, I + 1] := Format(ResStr_NumberedColumn, [I + 1]);
    end;
    sgMap.SetFocus;
  end;
  
  UpdateButtons;
end;  // TdlgImportSpreadsheet.UpdateMap 

{-------------------------------------------------------------------------------
  If dataset defined in dsImport is unacceptable, display a message and make
          ImportObject unassigned.
}
procedure TdlgImportSpreadsheet.ValidateFieldNames;
var
  I, J: Integer;
  Message: string;
begin
  Message := '';
  
  if dsImport.FieldDefs.Count = 0 then
    Message := ResStr_EmptySpreadsheetFile
  else
    for I := 0 to dsImport.FieldDefs.Count - 1 do
      for J := 1 to Length(dsImport.FieldDefs[I].Name) do
      begin
        if Ord(dsImport.FieldDefs[I].Name[J]) < 32 then
        begin
          Message := ResStr_NotASpreadsheetFile;
          Break;
        end;
      end;

  if Message <> '' then
  begin
    ShowMessage(Message);
    FImportObject := nil;
  end;
end;  // TdlgImportSpreadsheet.ValidateFieldNames

{-------------------------------------------------------------------------------
  Launch the specified file in Notepad.
}
procedure TdlgImportSpreadsheet.ViewFile(const FileName: string);
var
  StartupInfo: TStartupInfo;
  ProcessInfo: TProcessInformation;
begin
  FillChar(StartupInfo, SizeOf(StartupInfo), #0);
  StartupInfo.cb := SizeOf(StartupInfo);
  Win32Check(CreateProcess(
          nil,
          PChar('Notepad.exe "' + FileName + '"'),
          nil,
          nil,
          False,
          0,
          nil,
          nil,
          StartupInfo,
          ProcessInfo));
end;  // TdlgImportSpreadsheet.ViewFile

{ ------------------------------------------------------------------------------
}
procedure TdlgImportSpreadsheet.ImportObjectAfterRecord(Sender: TObject; var Abort: Boolean);
begin
  frmThesaurusEditor.SetProgress(
      ImportObject.Statistic.TotalImported,
      ImportObject.Statistic.TotalCount);
  if Now > FLastProgressUpdate + 1 / (24 * 60 * 12) then
  begin
    Application.ProcessMessages;
    FLastProgressUpdate := Now;
  end;
end;

{ ------------------------------------------------------------------------------
}
procedure TdlgImportSpreadsheet.chkConceptIntroducedVersionClick(Sender: TObject);
begin
  cmbConceptIntroducedVersion.Enabled := chkConceptIntroducedVersion.Checked;
  UpdateButtons;
end;

{ ------------------------------------------------------------------------------
}
procedure TdlgImportSpreadsheet.chkConceptExpiredVersionClick(Sender: TObject);
begin
  cmbConceptExpiredVersion.Enabled := chkConceptExpiredVersion.Checked;
  UpdateButtons;
end;

{ ------------------------------------------------------------------------------
  On selection of an item, cmbConceptIntroducedVersion is cleared if it refers
  to a version after the selected version in cmbConceptExpiredVersion
  (i.e. if the Sequence field is higher in the concept group version).
}
procedure TdlgImportSpreadsheet.cmbConceptVersionChange(Sender: TObject);
begin
  if (cmbConceptIntroducedVersion.CurrentStrID <> '') and
     (cmbConceptExpiredVersion.CurrentStrID <> '') then
    if StrToInt(FSequenceList.Values[cmbConceptIntroducedVersion.CurrentStrID]) >
       StrToInt(FSequenceList.Values[cmbConceptExpiredVersion.CurrentStrID]) then
      if Sender = cmbConceptIntroducedVersion then
        cmbConceptExpiredVersion.ItemIndex := -1
      else
        cmbConceptIntroducedVersion.ItemIndex := -1;
  UpdateButtons;
end;

{ ------------------------------------------------------------------------------
}
procedure TdlgImportSpreadsheet.chkUsePrimarySynonymGroupClick(
  Sender: TObject);
var
  Enable: Boolean;
begin
  Enable := chkUsePrimarySynonymGroup.Checked;
  fraSynonymDomainConceptGroupsSelector.Enabled := Enable;
  if not Enable then fraSynonymDomainConceptGroupsSelector.Clear;
  UpdateButtons;
end;

{ ------------------------------------------------------------------------------
}
procedure TdlgImportSpreadsheet.fraSynonymDomainConceptGroupsSelectorcmbConceptGroupsClick(
  Sender: TObject);
begin
  UpdateButtons;
end;

end.

