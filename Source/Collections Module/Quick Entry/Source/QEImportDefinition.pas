{===============================================================================
  Unit:        QEImportDefinition.pas

  Defines:     TQEImportDefinition

  Description: Specification of a Quick Entry import job.

  Model:       -

  Created:     September 2004

  Last revision information:
    $Revision: 5 $
    $Date: 4/10/04 16:21 $
    $Author: Ericsalmon $

===============================================================================}
unit QEImportDefinition;

interface

uses
  Classes, Windows, SMIBase, QuickEntryFrame;

resourcestring
  ResStr_UnknownQESession = 'Unknown quick entry session (%d)';
  ResStr_UnknownImportType = 'Unknown import type';

const
  QE_IMPORT_TABLE_NAME = '#SMImport';
  QE_IMPORT_ID_FIELD = '__REC_NO__';

type
  TQEImportType = (
      itUnknown,
      itCsv, 
      itExcel, 
      itText, 
      itDBase, 
      itLotus, 
      itParadox, 
      itQuattro, 
      itAdo);

const
  itSpreadsheet = [itText, itCsv, itExcel, itLotus, itQuattro];
  itDatabase = [itDBase, itParadox, itAdo];

type      
  TSMIClass = class of TSMImportBaseComponent;
  
  TQEImportDefinition = class
  private
    FColWidths: array of Integer;
    FDataSource: String;
    FDateDelimiter: Char;
    FDateFormat: String;
    FDelimiter: Char;
    FImportType: TQEImportType;
    FFirstRow: Integer;
    FFixedColumns: Boolean;
    FImportFieldCount: Integer;
    FImportFieldNames: TStringList;
    FImportFieldNamesFromData: Boolean;
    FImportValues: array of Variant;
    FLastRow: Integer;
    FHasImportData: Boolean;
    FOwner: TComponent;
    FParentHandle: HWND;
    FRecordSeparator: TSMIRecordSeparator;
    FSessionFields: TStringList;
    FSessionFrame: TfraQuickEntry;
    FTableName: String;
    FTextQualifier: Char;
  protected
    procedure BuildFixedWidthFieldMappings(Mappings: TStrings); virtual;
    function CreateImportObject: TSMImportBaseComponent; virtual;
    procedure DoImportValues; virtual;
    function GetColWidths(Index: Integer): Integer; virtual;
    function GetImportFieldNames(Index: Integer): String; virtual;
    function GetSessionFieldCount: Integer; virtual;
    function GetSessionFieldLabels(Index: Integer): String; virtual;
    function ImportClass: TSMIClass; virtual;
    procedure LoadSessionFields; virtual;
    procedure SMImportBeforeRecordEvent(Sender: TObject; const Fields: string;
        var Values: Variant; var Accept: Boolean);
    procedure SMImportAfterRecordEvent(Sender: TObject; var Abort: Boolean);
    procedure SMImportCreateStructure(Sender: TObject; Columns: TSMIColumns);
  public
    constructor Create(Owner: TComponent; ParentHandle: HWND; SessionFrame:
        TfraQuickEntry);
    destructor Destroy; override;
    procedure AddColumnMapping(SessionFieldIndex, ImportFieldIndex: Integer);
        virtual;
    procedure AddFixedWidthColumn(Width: Integer); virtual;
    procedure ClearColumnMappings; virtual;
    procedure ClearFixedWidthColumns; virtual;    
    procedure CloseWindow; virtual;
    procedure DropImportTable; virtual;
    procedure ImportValues(Values: array of Variant); virtual;
    procedure LoadImportData; virtual;
    property ColWidths[Index: Integer]: Integer read GetColWidths;
    property DataSource: String read FDataSource write FDataSource;
    property DateDelimiter: Char read FDateDelimiter write FDateDelimiter;
    property DateFormat: String read FDateFormat write FDateFormat;
    property Delimiter: Char read FDelimiter write FDelimiter;
    property FirstRow: Integer read FFirstRow write FFirstRow;
    property FixedColumns: Boolean read FFixedColumns write FFixedColumns;
    property HasImportData: Boolean read FHasImportData;
    property ImportFieldCount: Integer read FImportFieldCount;
    property ImportFieldNames[Index: Integer]: String read GetImportFieldNames;
    property ImportFieldNamesFromData: Boolean read FImportFieldNamesFromData
        write FImportFieldNamesFromData;
    property ImportType: TQEImportType read FImportType write FImportType;
    property LastRow: Integer read FLastRow write FLastRow;
    property Owner: TComponent read FOwner;
    property ParentHandle: HWND read FParentHandle;
    property RecordSeparator: TSMIRecordSeparator read FRecordSeparator write
        FRecordSeparator;
    property SessionFieldCount: Integer read GetSessionFieldCount;
    property SessionFieldLabels[Index: Integer]: String read
        GetSessionFieldLabels;
    property TableName: String read FTableName write FTableName;
    property TextQualifier: Char read FTextQualifier write FTextQualifier;
  end;

//==============================================================================
implementation

uses
  GeneralData, Messages, SysUtils, Math, Variants, ADODB, Sql, SMI2TXT,
  SMI2XLS, SMI2WKS, SMI2WQ, SMI2DBF, SMI2DB, SMI2ADO, SMCells, Recorder2000_TLB;

const
  IMPORT_FIELD_NAME = 'Field_%d';

{-==============================================================================
    TQEImportDefinition
===============================================================================}
{-------------------------------------------------------------------------------
}
constructor TQEImportDefinition.Create(Owner: TComponent; ParentHandle: HWND;
    SessionFrame: TfraQuickEntry);
begin 
  FOwner := Owner;
  FParentHandle := ParentHandle;
  FSessionFrame := SessionFrame;
  SetLength(FColWidths, 0);
  FImportFieldNames := TStringList.Create;
  FSessionFields := TStringList.Create;
  LoadSessionFields;
end;  // TQEImportDefinition.Create 

{-------------------------------------------------------------------------------
}
destructor TQEImportDefinition.Destroy;
begin
  FImportFieldNames.Free;
  FSessionFields.Free;
  DropImportTable;
  inherited;
end;  // TQEImportDefinition.Destroy 

{-------------------------------------------------------------------------------
}
procedure TQEImportDefinition.AddColumnMapping(SessionFieldIndex,
    ImportFieldIndex: Integer);
begin
  FSessionFields.ValueFromIndex[SessionFieldIndex] := 
      IntToStr(ImportFieldIndex);
end;  // TQEImportDefinition.AddColumnMapping 

{-------------------------------------------------------------------------------
}
procedure TQEImportDefinition.AddFixedWidthColumn(Width: Integer);
begin
  SetLength(FColWidths, ImportFieldCount + 1);
  FColWidths[ImportFieldCount] := Width;
  Inc(FImportFieldCount);
end;  // TQEImportDefinition.AddFixedWidthColumn 

{-------------------------------------------------------------------------------
}
procedure TQEImportDefinition.ClearColumnMappings;
var
  I: Integer;
begin
  for I := 0 to FSessionFields.Count - 1 do
    FSessionFields.ValueFromIndex[I] := '-1';
end;  // TQEImportDefinition.ClearColumnMappings 

{-------------------------------------------------------------------------------
}
procedure TQEImportDefinition.ClearFixedWidthColumns;
begin
  FImportFieldCount := ImportFieldCount - Length(FColWidths);
  SetLength(FColWidths, 0);
end;  // TQEImportDefinition.ClearFixedWidthColumns

{-------------------------------------------------------------------------------
}
procedure TQEImportDefinition.CloseWindow;
begin
  SendMessage(ParentHandle, WM_CLOSE, 0, 0);
end;  // TQEImportDefinition.CloseWindow 

{-------------------------------------------------------------------------------
}
procedure TQEImportDefinition.BuildFixedWidthFieldMappings(Mappings: TStrings);
var
  I: Integer;
  lStart: Integer;
  lEnd: Integer;
  lSql: String;
begin
  Mappings.Clear;
  lSql := 'CREATE TABLE ' + QE_IMPORT_TABLE_NAME + ' (';
  
  lStart := 1;
  for I := 0 to ImportFieldCount - 1 do
  begin
    lEnd := lStart + ColWidths[I] - 1;
    Mappings.Add(
        Format(IMPORT_FIELD_NAME, [I]) 
        + '=Text' + IntToStr(lStart) + '-' + IntToStr(lEnd));
    lSql := lSql + Format(IMPORT_FIELD_NAME, [I]) + ' TEXT NULL, ';    
    lStart := lEnd + 1;
  end;
  
  lSql := lSql + QE_IMPORT_ID_FIELD + ' INT IDENTITY PRIMARY KEY)';
  dmGeneral.ExecuteSql(lSql);  
end;  // TQEImportDefinition.BuildFixedWidthFieldMappings 

{-------------------------------------------------------------------------------
}
function TQEImportDefinition.CreateImportObject: TSMImportBaseComponent;

  procedure SetRequiredRows;
  begin
    if ImportType in itDatabase then
      // we have real column names, so we don't need the first row of data
      // unless the user has specified that it should be imported
      Result.RowFirst := IfThen(FirstRow = 0, 1, FirstRow)
    else if (ImportType in [itCsv, itText]) and not FixedColumns then
      // never import the first row explicitly -- we must let SMImport 
      // interpret it as column headings otherwise it will not recognise
      // the columns
      Result.RowFirst := IfThen(FirstRow < 2, 2, FirstRow)
    else
      // we always need the first row of data in case the user wants to use
      // it for column headings
      Result.RowFirst := 1;
      
    if LastRow <> 0 then
      Result.RowLast := LastRow;  
  end;

  procedure DefineFixedColumns;
  begin
    Result.RowFieldNames := 0;
    Result.FieldDelimiter := fdNone;
    Result.TextQualifier := tqNone;
    Result.TrimSpaces := tsTrimRight;
    BuildFixedWidthFieldMappings(Result.Mappings);
    Result.DataSet.Open;
  end;

  procedure SetDelimiters;
  begin
    case Delimiter of
      #9:  Result.FieldDelimiter := fdTab;
      ';': Result.FieldDelimiter := fdSemicolon;
      ',': Result.FieldDelimiter := fdComma;
      ' ': Result.FieldDelimiter := fdSpace;
    else
      Result.FieldDelimiter := fdCustom;
      Result.FieldDelimiterCustom := Delimiter;
    end;
    case TextQualifier of
      ' ':  Result.TextQualifier := tqNone;
      '"':  Result.TextQualifier := tqQuot;
      '''': Result.TextQualifier := tqApos;
    else
      Result.TextQualifier := tqCustom;
      Result.TextQualifierCustom := TextQualifier;
    end;
  end;

begin
  Result := ImportClass.Create(nil);
  try  
    Result.DataSet := TADOTable.Create(Result);  
    with Result.DataSet as TADOTable do
    begin
      Connection := dmGeneral.Connection;
      TableName := QE_IMPORT_TABLE_NAME;
    end;

    with Result do
    begin
      SourceFileName := DataSource;
      AnimatedStatus := False;
      AbortOnProblem := False;
      Options := [soExtendedStatistic, soSkipEmptyRow, soWaitCursor];
      if not FixedColumns then
        OnCreateStructure := SMImportCreateStructure;
      OnBeforeRecordEvent := SMImportBeforeRecordEvent;
      OnAfterRecordEvent := SMImportAfterRecordEvent;
    end;

    SetRequiredRows;

    if ImportType = itADO then 
      TSMImportFromADO(Result).Sql := 'SELECT * FROM [' + TableName + ']'
    else if ImportType in [itCsv, itText] then
    begin
      Result.RecordSeparator := RecordSeparator;

      Result.Fixed := FixedColumns;
      if FixedColumns then
        DefineFixedColumns
      else
        SetDelimiters;
    end;
  except
    Result.DataSet.Free;
    Result.Free;
    raise;
  end;
end;

{+------------------------------------------------------------------------------
  Add the record described by FImportValues (see ImportValues method) to the
  quick entry session.
}
procedure TQEImportDefinition.DoImportValues;
begin
  try
    FSessionFrame.AddImportedRow(FImportValues);
  finally
    SetLength(FImportValues, 0);
  end;
end;  // TQEImportDefinition.DoImportValues 

{-------------------------------------------------------------------------------
}
procedure TQEImportDefinition.DropImportTable;
begin
  dmGeneral.ExecuteSql(
    'IF NOT object_id(''tempdb..' + QE_IMPORT_TABLE_NAME + ''') IS NULL'
    + ' DROP TABLE ' + QE_IMPORT_TABLE_NAME);    
end;  // TQEImportDefinition.DropImportTable 

{-------------------------------------------------------------------------------
}
function TQEImportDefinition.GetColWidths(Index: Integer): Integer;
begin
  Result := FColWidths[Index];
end;  // TQEImportDefinition.GetColWidths

{-------------------------------------------------------------------------------
}
function TQEImportDefinition.GetImportFieldNames(Index: Integer): String;
begin
  Result := FImportFieldNames[Index];
end;  // TQEImportDefinition.GetImportFieldNames 

{-------------------------------------------------------------------------------
}
function TQEImportDefinition.GetSessionFieldCount: Integer;
begin
  Result := FSessionFields.Count;
end;  // TQEImportDefinition.GetSessionFieldCount: 

{-------------------------------------------------------------------------------
}
function TQEImportDefinition.GetSessionFieldLabels(Index: Integer): String;
begin
  Result := FSessionFields.Names[Index];
end;  // TQEImportDefinition.GetSessionFieldLabels 

{-------------------------------------------------------------------------------
}
function TQEImportDefinition.ImportClass: TSMIClass;
begin
  case ImportType of
    itCsv, itText:  Result := TSMImportFromText;
    itExcel:        Result := TSMImportFromXLS;
    itLotus:        Result := TSMImportFromWKS;
    itQuattro:      Result := TSMImportFromQuattro;
    itDBase:        Result := TSMImportFromDBF;
    itParadox:      Result := TSMImportFromParadox;
    itADO:          Result := TSMImportFromADO;
  else
    raise Exception.Create(ResStr_UnknownImportType);
  end;
end;  // TQEImportDefinition.ImportClass 

{+------------------------------------------------------------------------------
  Add a row of imported data to the quick entry session.

  Values contains the complete list of values read from the import source
  (including columns that were not mapped to session fields).
}
procedure TQEImportDefinition.ImportValues(Values: array of Variant);
var
  I: Integer;
  lImportField: Integer;
begin
  SetLength(FImportValues, 0);
  
  for I := 0 to SessionFieldCount - 1 do
    if FSessionFields.ValueFromIndex[I] <> '' then
    begin
      lImportField := StrToInt(FSessionFields.ValueFromIndex[I]);
      if lImportField <> -1 then
      begin
        SetLength(FImportValues, Length(FImportValues) + 2);
        FImportValues[Length(FImportValues) - 2] := I;
        FImportValues[Length(FImportValues) - 1] := Values[lImportField];
      end;
    end;

  TThread.Synchronize(nil, DoImportValues);    
end;  // TQEImportDefinition.ImportValues 

{-------------------------------------------------------------------------------
}
procedure TQEImportDefinition.LoadImportData;
var
  lRecorder: IRecorderMainForm;
  lSMImport: TSMImportBaseComponent;
  lOldDateFormat: String;
  lOldDateSep: Char;
begin
  FHasImportData := False;

  lRecorder := dmGeneral.Recorder.RecorderMainForm;
  lRecorder.StartProgressBar;
  lOldDateFormat := ShortDateFormat;
  lOldDateSep := DateSeparator;
  try
    if DateFormat <> '' then ShortDateFormat := DateFormat;
    if DateDelimiter <> '' then DateSeparator := DateDelimiter;

    lSMImport := CreateImportObject;
    try
      lSMImport.Execute;
      FHasImportData := lSMImport.DataSet.Active;
    finally
      lSMImport.DataSet.Free;
      lSMImport.Free;
    end;
  finally
    ShortDateFormat := lOldDateFormat;
    DateSeparator := lOldDateSep;
    lRecorder.StopProgressBar;
  end;
end;  // TQEImportDefinition.LoadImportData 

{+------------------------------------------------------------------------------
  Load field names from the Quick Entry session.
}
procedure TQEImportDefinition.LoadSessionFields;
var
  I: Integer;  
begin
  FSessionFields.Clear;
  
  with FSessionFrame.sgQuickEntry do
    for I := 2 to ColCount - 1 do  // 2 => skip 'Validated'
      FSessionFields.Add(Cells[I, 0] + '=-1');
end;  // TQEImportDefinition.LoadSessionFields 

{+------------------------------------------------------------------------------
}
procedure TQEImportDefinition.SMImportAfterRecordEvent(Sender: TObject; var
    Abort: Boolean);
var
  lProgress: Integer;
begin
  with (Sender as TSMImportBaseComponent).Statistic do
    lProgress := 100 * TotalImported div TotalCount;

  with dmGeneral.Recorder.RecorderMainForm do
    if Progress <> lProgress then Progress := lProgress;
end;  // TQEImportDefinition.SMImportAfterRecordEvent 

{+------------------------------------------------------------------------------
}
procedure TQEImportDefinition.SMImportBeforeRecordEvent(Sender: TObject; const
    Fields: string; var Values: Variant; var Accept: Boolean);
var
  I: Integer;
  lNames: Boolean;
  lValues: String;
begin
  lNames := FImportFieldNames.Count = 0;
  
  lValues := '';
  for I := 0 to VarArrayHighBound(Values, 1) do
  begin
    if lNames then FImportFieldNames.Add(VarToStr(Values[I][2]));
    if I > 0 then lValues := lValues + ', ';
    lValues := lValues + TransactSqlLiteral(VarToStr(Values[I][2]));
  end;

  dmGeneral.ExecuteSql(
      'INSERT INTO ' + QE_IMPORT_TABLE_NAME
      + ' VALUES (' + lValues + ')');
  
  Accept := False;
end;  // TQEImportDefinition.SMImportBeforeRecordEvent 

{+------------------------------------------------------------------------------
}
procedure TQEImportDefinition.SMImportCreateStructure(Sender: TObject; Columns:
    TSMIColumns);
var
  I: Integer;
  lSmi: TSMImportBaseComponent;
  lSql: String;
  lFieldName: String;
  lFieldType: String;

  { Insert the first row.  Only required for delimited text and CSV types, due 
    to random SMImport crapness -- these types won't detect the columns in the
    file if RowFirst = 1, so we must start the import proper from row 2.
  }
  procedure CopyFieldNamesToRow;
  var
    I: Integer;
  begin
    with lSmi.DataSet do 
    begin
      Insert;
      for I := 0 to Fields.Count - 2 do   // -2 => miss out ID field
        Fields[i].Value := Columns[I].FieldName;
      Post;
    end;
  end;
  
begin
  DropImportTable;
  FImportFieldNames.Clear;
  
  lSmi := Sender as TSMImportBaseComponent;  
  lSql := 'CREATE TABLE ' + QE_IMPORT_TABLE_NAME + ' (';
  FImportFieldCount := Columns.Count;
  
  for I := 0 to Columns.Count - 1 do 
  begin
    lFieldName := Format(IMPORT_FIELD_NAME, [I]);
    case Columns[I].DataType of
      itString:  
          lFieldType := 'TEXT';
      itInteger: 
          lFieldType := 'INT';
      itFloat:   
          lFieldType := 'FLOAT';
      itDateTime, itDate, itTime: 
          lFieldType := 'DATETIME'
    else
      lFieldType := 'BIT';
    end;
    lSql := lSql + lFieldName + ' ' + lFieldType + ' NULL, ';

    case ImportType of
      itDBase, itParadox, itAdo:
          lSmi.Mappings.Add(lFieldName + '="' + Columns[I].FieldName + '"');
      itExcel, itLotus, itQuattro:
          lSmi.Mappings.Add(lFieldName + '=' + GetSpreadSheetColByID(I + 1));
    else
      lSmi.Mappings.Add(lFieldName + '=' + Columns[I].Caption);
    end;

    if (ImportType in [itCsv, itText]) or (ImportType in itDatabase) then
      FImportFieldNames.Add(Columns[I].FieldName);
  end;
    
  lSql := lSql + QE_IMPORT_ID_FIELD + ' INT IDENTITY PRIMARY KEY)';
  dmGeneral.ExecuteSql(lSql);  

  lSmi.DataSet.Open;

  if ImportType in [itText, itCsv] then
    CopyFieldNamesToRow;
end;  // TQEImportDefinition.SMImportCreateStructure 

end.
