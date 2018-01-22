//==============================================================================
//  Unit:        DatabaseOutput
//
//  Implements:  TDatabaseOutput
//
//  Description: Provides functionality to export data to an Access database.
//
//  Author:      Eric Salmon
//  Created:     29 May 2002
//
//  Last Revision Details:
//    $Revision: 48 $
//    $Date: 1/10/09 10:07 $
//    $Author: Simonlewis $
//
//  $History: DatabaseOutput.pas $
//  
//  *****************  Version 48  *****************
//  User: Simonlewis   Date: 1/10/09    Time: 10:07
//  Updated in $/JNCC/Components
//  Changed the DoExport routine from a procedure to a function; it now
//  returns the file name of the export file, which is used in FilterResult
//  to save metadata.
//  
//  *****************  Version 47  *****************
//  User: Qingsun      Date: 17/12/08   Time: 12:50
//  Updated in $/JNCC/Components
//  CCN 298
//  
//  *****************  Version 46  *****************
//  User: Johnvanbreda Date: 19/06/08   Time: 16:03
//  Updated in $/JNCC/Components
//  VI17020
//  Fixed export of confidential to zip
//  
//  *****************  Version 45  *****************
//  User: Qingsun      Date: 30.05.08   Time: 17:26
//  Updated in $/JNCC/Components
//  
//  *****************  Version 44  *****************
//  User: Johnvanbreda Date: 27/05/08   Time: 10:31
//  Updated in $/JNCC/Components
//  VI17020
//  Fixed export filter output file occurrence filtration
//  
//  *****************  Version 43  *****************
//  User: Johnvanbreda Date: 26/05/08   Time: 11:56
//  Updated in $/JNCC/Components
//  VI17136
//  Added intelligence about which joins to follow, for performance
//  improvements
//  
//  *****************  Version 42  *****************
//  User: Johndurman   Date: 21/05/08   Time: 11:22
//  Updated in $/JNCC/Components
//  VI 17136 - CCN257 - Improve performance of exporting
//  
//  *****************  Version 41  *****************
//  User: Ericsalmon   Date: 24/04/08   Time: 11:06
//  Updated in $/JNCC/Components
//  Fix for code using old Execute form.
//  
//  *****************  Version 40  *****************
//  User: Ericsalmon   Date: 31/03/08   Time: 16:58
//  Updated in $/JNCC/Components
//  VI16694. Collation problem with temp tables.
//
//  *****************  Version 39  *****************
//  User: Ericsalmon   Date: 31/03/08   Time: 12:43
//  Updated in $/JNCC/Components
//  VI16624. Fixed export for confidential occurrences.
//  
//  *****************  Version 38  *****************
//  User: Ericsalmon   Date: 21/03/08   Time: 9:47
//  Updated in $/JNCC/Components
//  VI16628 / CCN233. Revalidation before export.
//  
//  *****************  Version 37  *****************
//  User: Johnvanbreda Date: 11/01/08   Time: 14:39
//  Updated in $/JNCC/Components
//  CCN246  Zero abundance records are exported.
//  
//  *****************  Version 36  *****************
//  User: Rickyshrestha Date: 3/01/08    Time: 11:56
//  Updated in $/JNCC/Components
//  Changed some hardocded strings to resourestring
//  
//  *****************  Version 35  *****************
//  User: Rickyshrestha Date: 28/12/07   Time: 13:34
//  Updated in $/JNCC/Components
//  Changed some hardcoded strings to resourcestring
//  
//  *****************  Version 34  *****************
//  User: Johnvanbreda Date: 8/08/06    Time: 15:17
//  Updated in $/JNCC/Components
//  IR12410
//  Fixed export to scheme attachment location
//  
//  *****************  Version 33  *****************
//  User: Johnvanbreda Date: 14/06/06   Time: 14:35
//  Updated in $/JNCC/Components
//  IR11240
//  Export fix for Thesaurus
//  
//  *****************  Version 32  *****************
//  User: Johnvanbreda Date: 3/03/06    Time: 8:35
//  Updated in $/JNCC/Components
//  Fixed so its collation sequence independent.
//  
//  *****************  Version 31  *****************
//  User: Johnvanbreda Date: 22/02/06   Time: 13:39
//  Updated in $/JNCC/Components
//  IR11060
//  Fixed cancellation of zipped access export.
//  
//  *****************  Version 30  *****************
//  User: Johnvanbreda Date: 3/01/06    Time: 14:14
//  Updated in $/JNCC/Components
//  IR10689
//  Fixed export filters for large surveys
//  
//  *****************  Version 29  *****************
//  User: Ericsalmon   Date: 1/12/05    Time: 14:47
//  Updated in $/JNCC/Components
//  VI 10531. Fixed. Source_Join already had records, so when trying to add
//  all related records, the existing ones would cause a key violation.
//  
//  *****************  Version 28  *****************
//  User: Johnvanbreda Date: 9/08/05    Time: 9:47
//  Updated in $/JNCC/Components
//  IR9587
//  Fixed collections export
//  
//  *****************  Version 27  *****************
//  User: Johnvanbreda Date: 5/08/05    Time: 16:18
//  Updated in $/JNCC/Components
//  IR9587
//  Performance - major rewrite
//  
//  *****************  Version 26  *****************
//  User: Ericsalmon   Date: 10/02/05   Time: 17:09
//  Updated in $/JNCC/Components
//  ID 7833. Removed as much dependencies as possible, so that it can be
//  used in other projects.
//
//  *****************  Version 25  *****************
//  User: Johnvanbreda Date: 14/10/04   Time: 12:58
//  Updated in $/JNCC/Components
//  IR7199
//  Fixes to allow import of collections data
//
//  Copyright © Dorset Software Services Ltd, 2002
//
//==============================================================================

{$I DelphiVersions.Inc}

unit DatabaseOutput;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, Db,
  ComCtrls, ExceptionForm, DataClasses, JNCCDatasets, TaskProgress,
  GeneralFunctions, DatabaseUtilities, VCLZip, VCLUnzip,
  Relationships_ADO, ADODB, DatabaseAccessADO, ADOX_TLB, Constants, SQLConstants
  {$IFDEF DELPHI7UP}, Variants{$ENDIF};

const
  I_MAX_KEYS   = 1000;
  I_MAX_2_KEYS = 1;

type
  EDatabaseOutput = class(TExceptionPath);

  TDatabaseOutput = class
  private
    { Private declarations }
    FTablesUsed: TStringlist;
    FTablesPopulated: TStringlist;
    FRequiredFields: TStringList;
    FTotalRows: integer;
    FDatabaseModule: TdmDatabase;
    FDatabaseName: string;
    FSetStatus        : TSetStatusEvent;
    FSetProgress      : TSetProgressEvent;
    FProgressBar      : TTaskProgressBar;
    FZipTool          : TVCLZip;
    FCancelled: boolean;
    FUserAccessLevel: TUserAccessLevel;
    FExportConfidentialOccurrences: boolean;
    FCanExportSystemSupplied: boolean;
    FInvalidKeys: TKeyList;
    FProcessedJoins: TStringList;
    FUsingExportFilter: Boolean;
    function GetRequiredFieldsForTable(const ATableName: string;
         const AAlias: string=''): string;
    procedure ProcessTable(const ATableName, ALastTableName: string);
    procedure CreateTable(const ATableName: string);
    procedure DoDataEntryNames;
    function DoExport: String;
    procedure SetCancelled(const Value: boolean);
    procedure SetStatus(const AMessage: String);
    procedure SetProgress(const AProgress: Integer);
    function GetCommaSepItems(const AStringList: TStringList;
      const AStartPos: Integer; const AAmount: Integer = I_MAX_KEYS): String;
    procedure DoZip;
    procedure ZipTotalPercentDone(Sender: TObject; Percent: Integer);
    procedure GetObservations;
    procedure CopyJoinedRecords(const ATable, AJoinTable, AField,
      AJoinField: string; AForceProcess: boolean; AProcessNextTable: boolean=true);
    function GetFilterForTable(const tableName, alias: String): String;
    procedure ProcessMetadataTable;
    procedure ProcessSourceJoinTable;
    function GetUserAccessLevel: Integer;
    procedure SetUserAccessLevel(const Value: Integer);
    procedure CleanupTempTables;
    procedure InitialiseFields(ADatabaseModule: TdmDatabase;
            const ADBName: String; const ASetStatusEvent: TSetStatusEvent);
    procedure SetUsingExportFilter(const Value: Boolean);
    procedure SetExportConfidentialOccurrences(const Value: Boolean);
  public
    { Public declarations }
    constructor Create(ADatabaseModule: TdmDatabase; const ADBName: String;
                  const ASetStatusEvent: TSetStatusEvent; const AProgressBar: TTaskProgressBar); overload;
    constructor Create(ADatabaseModule: TdmDatabase; const ADBName: String;
                  const ASetStatusEvent: TSetStatusEvent; const ASetProgressEvent: TSetProgressEvent); overload;
    destructor Destroy; override;
    procedure Execute(AKeyList: TKeyList; AWantObservations: Boolean); overload;
    procedure Execute(AKeyList, AInvalidKeys: TKeyList; AWantObservations: Boolean); overload;
    property Cancelled: boolean read FCancelled write SetCancelled;
    property UserAccessLevel: Integer read GetUserAccessLevel write SetUserAccessLevel;
    property ExportConfidentialOccurrences: Boolean read FExportConfidentialOccurrences
        write SetExportConfidentialOccurrences;
    property CanExportSystemSupplied: Boolean read FCanExportSystemSupplied
        write FCanExportSystemSupplied;
    property UsingExportFilter: Boolean read FUsingExportFilter write
        SetUsingExportFilter;
  end;

//==============================================================================
implementation

uses ADOInt, ComObj;

resourcestring
  ResStr_GatheringInformationForExport = 'Gathering information for export - ';
  ResStr_ProcessingMetadata = 'Processing metadata...';
  ResStr_ProcessingDataEntryNames =  'Processing data entry names...';
  ResStr_ProcessingSources = 'Processing sources...';
  ResStr_Cancelled = 'Export was cancelled.';
  ResStr_ExportingData =  'Exporting data (Tables processed %d of %d - %s)';
  ResStr_ExportDBCompressed = 'Export database compressed.';
  ResStr_CompressExpDB =  'Compressing export database...';

const
  SQL_REQUIRED_FKS =
      'SELECT DISTINCT Detail_Field AS Field '+
      'FROM Database_Relationship '+
      'WHERE Detail_Table=''%s'' '+
      'UNION '+
      'SELECT DISTINCT Master_Field AS Field '+
      'FROM Database_Relationship '+
      'WHERE Master_Table=''%s'' '+
      'UNION '+
      'SELECT DISTINCT name COLLATE database_default FROM syscolumns '+
      'WHERE id=OBJECT_ID(''%s'') AND name IN ('+
      '''entered_by'', ''changed_by'', ''system_supplied_data'', ''checked'', '+
      '''confidential'', ''verified'')';

  FILTERS: array[0..2] of string = ('checked', 'confidential', 'verified');

  SQL_LINKED_TABLES =
      'SELECT Master_Table, Master_Field, Detail_Table, Detail_Field, '+
      'Follow_Up, Follow_Down, One_To_One '+
      'FROM Database_Relationship '+
      'WHERE (Master_Table=''%s'' AND Follow_Down=1) '+
      'OR (Detail_table=''%s'' AND Follow_Up=1)';

  SQL_INSERT_TABLE =
      'INSERT INTO [#%s] ' +
      'SELECT DISTINCT %s ' +
      'FROM [#%s] T1 ' +
      'INNER JOIN [%s] T2 ON T2.%s=T1.%s ' +
      'LEFT JOIN #%s E ON E.%s=T1.%s ' +
      'WHERE E.%s IS NULL ' +
      'AND T2.%s NOT IN (SELECT ItemKey COLLATE Database_Default FROM #InvalidKeys WHERE TableName = ''%s'')';

  SQL_CREATE_TABLE =
      'SELECT TOP 0 %s '+
      'INTO #%s ' +
      'FROM [%s]';

  SQL_ADD_PK =
      'ALTER TABLE #%s ADD CONSTRAINT [Pk_%s] PRIMARY KEY (%s)';

  SQL_INSERT_NAMES =
      'INSERT INTO [#NAME] (Name_Key, System_Supplied_Data) '+
      'SELECT DISTINCT T1.Name_Key, 0 '+
      'FROM [#NAMETEMP] T1 '+
      'INNER JOIN [#NAME] T2 ON T2.Name_Key=T1.Name_Key '+
      'LEFT JOIN #NAME E ON E.Name_Key=T1.Name_Key '+
      'WHERE E.Name_Key IS NULL';

  SQL_CREATE_NAMETEMP =
      'CREATE TABLE #NameTemp (Name_Key CHAR(16) COLLATE SQL_Latin1_General_CP1_CI_AS)';
  SQL_DROP_NAMETEMP = 'DROP TABLE #NameTemp';

  EXT_ZIP = '.zip';
  EXT_MDB = '.mdb';

function TDatabaseOutput.GetRequiredFieldsForTable(
  const ATableName: string; const AAlias: string=''): string;
var
  lPK: TPrimaryKey;
  lFields: TStringList;
begin
  lFields := TStringList.Create;
  lFields.Sorted := True;
  lFields.Duplicates := dupIgnore;
  try
    // Use cached list of fields if available
    Result := FRequiredFields.Values[ATableName];
    if Result = '' then begin
      lPK := FDatabaseModule.SplitPrimaryKey(ATableName);
      lFields.Add('[' + Uppercase(lPk.Key1) + ']');
      if lPk.Key2<>'' then
        lFields.Add('[' + Uppercase(lPk.Key2) + ']');
      // Not in cache so fetch fields from db
      with FDatabaseModule.ExecuteSQL(Format(SQL_REQUIRED_FKS,
          [ATableName, ATableName, ATableName, ATableName]), True) do
        while not EOF do begin
          lFields.Add('[' + Uppercase(Fields['Field'].Value) + ']');
          MoveNext;
        end;
      Result := lFields.CommaText;
      FRequiredFields.Add(ATableName + '=' + Result);
    end;
    // if the field list requires an alias, insert it
    if AAlias<>'' then
      Result := AAlias + '.' + StringReplace(Result, ',[', ',[' + AAlias + '].[', [rfReplaceAll]);
  finally
    lFields.Free;
  end;
end;

procedure TDatabaseOutput.DoDataEntryNames;
var
  lSql: string;
  i: integer;

    procedure AddUnion(const ASql: string);
    begin
      if lSql = '' then
        lSql := ASql
      else
        lSql := lSql + ' UNION ' + ASql;
    end;

begin
  lSql := '';
  SetStatus(ResStr_ProcessingDataEntryNames);
  for i := 0 to FTablesPopulated.Count-1 do begin
    if Pos('[ENTERED_BY]', GetRequiredFieldsForTable(FTablesUsed[i]))>0 then
      AddUnion('SELECT DISTINCT Entered_By AS Name_Key FROM ' + FTablesUsed[i]);
    if Pos('[CHANGED_BY]', GetRequiredFieldsForTable(FTablesUsed[i]))>0 then
      AddUnion('SELECT DISTINCT Changed_By AS Name_Key FROM ' + FTablesUsed[i]);
    SetProgress(i * 100 div FTablesPopulated.Count);
  end;
  if lSql>'' then begin
    FDatabaseModule.ExecuteSQL(SQL_CREATE_NAMETEMP);
    try
      FDatabaseModule.ExecuteSQL('INSERT INTO #NameTemp (Name_Key) ' + lSql);
      CreateTable('NAME');
      FDatabaseModule.ExecuteSQL(SQL_INSERT_NAMES);
    finally
      FDatabaseModule.ExecuteSQL(SQL_DROP_NAMETEMP);
    end;
  end;
end;

{-------------------------------------------------------------------------------
}
function TDatabaseOutput.GetFilterForTable(const tableName, alias: String): String;
var
  tableFields: String;

  procedure AddFilter(const filterPart: String);
  begin
    if alias <> '' then
      Result := Result + ' AND ' + alias + '.' + filterPart
    else
      Result := Result + ' AND ' + filterPart;
  end;

begin
  Result := '';
  tableFields := GetRequiredFieldsForTable(tableName);
  if (not FCanExportSystemSupplied) and
     (Pos('[SYSTEM_SUPPLIED_DATA]', tableFields) > 0) then
    AddFilter('System_Supplied_Data=0');

  if (not UsingExportFilter) and (Pos('[CHECKED]', tableFields) > 0) then
    AddFilter('Checked=1');

  if (not UsingExportFilter) and (Pos('[VERIFIED]', tableFields) > 0) then
    AddFilter('Verified<>1');
  //that is why you can not see confidential
  if (not ExportConfidentialOccurrences) and (Pos('[CONFIDENTIAL]', tableFields) > 0) then
    AddFilter('Confidential=0');
end;

{-------------------------------------------------------------------------------
  Copy the records identified by the foreign key in a table into the temp
     version of the joined table.
}
procedure TDatabaseOutput.CopyJoinedRecords(const ATable, AJoinTable, AField, AJoinField: string;
     AForceProcess: boolean; AProcessNextTable: boolean=true);
var
  lFilter: string;
  lRowsAffected: integer;
  lPK: TPrimaryKey;
begin
  // Don't follow joins that have already been processed in the other direction
  // as this wastes time, apart from one to one relationships (e.g. NAME - INDIVIDUAL)
  if AForceProcess or (FProcessedJoins.IndexOf(AJoinTable + ',' + ATable)=-1) then begin
    // Remember the join - this is the other way round to the test above so that
    // we don't bounce backwards along the same join later
    if FProcessedJoins.IndexOf(AJoinTable + ',' + ATable)=-1 then
      FProcessedJoins.Add(ATable + ',' + AJoinTable);
    CreateTable(AJoinTable);
    lFilter := GetFilterForTable(AJoinTable, 'T2');
    // Insert into the detail table rows linked to the master table

    lPK := FDatabaseModule.SplitPrimaryKey(AJoinTable);

    lRowsAffected := FDatabaseModule.ExecuteSQLGetRowsAffected(Format(SQL_INSERT_TABLE, [
        AJoinTable,
        GetRequiredFieldsForTable(AJoinTable, 'T2'),
        ATable,
        AJoinTable,
        AJoinField,
        AField,
        AJoinTable,
        AJoinField,
        AField,
        AJoinField,
        lPK.Key1,
        AJoinTable
        ]) + lFilter);
    Inc(FTotalRows, lRowsAffected);
    if lRowsAffected > 0 then begin
      FTablesPopulated.Add(AJoinTable);
      if AProcessNextTable then
        ProcessTable(AJoinTable, ATable);
    end;
  end;
end;

{-------------------------------------------------------------------------------
}
procedure TDatabaseOutput.ProcessTable(const ATableName, ALastTableName: string);
begin
  SetStatus(ResStr_GatheringInformationForExport + ReadableFormat(ATableName));
  if FCancelled then
    raise EDatabaseOutput.CreateNonCritical(ResStr_Cancelled);
  with FDatabaseModule.ExecuteSQL(Format(SQL_LINKED_TABLES,
      [ATableName, ATableName]), True) do
    while not EOF do begin
      if (CompareText(Fields['Master_Table'].Value, ATableName) = 0) and
          (Fields['Follow_Down'].Value) and
          (Fields['Master_Table'].Value <> ALastTableName) then
          CopyJoinedRecords(
            Fields['Master_Table'].Value,
            Fields['Detail_Table'].Value,
            Fields['Master_Field'].Value,
            Fields['Detail_Field'].Value,
            Fields['One_To_One'].Value)
      else
      if (CompareText(Fields['Detail_Table'].Value, ATableName) = 0) and
          (Fields['Follow_Up'].Value) and
          (Fields['Detail_Table'].Value <> ALastTableName) then
          CopyJoinedRecords(
            Fields['Detail_Table'].Value,
            Fields['Master_Table'].Value,
            Fields['Detail_Field'].Value,
            Fields['Master_Field'].Value,
            true);// always follow from detail to master
      //CCN298  handlling the hierachal recursive relationships here is for 'Location'
      if  (CompareText(Fields['Master_Table'].Value, ATableName) = 0) and
          (CompareText(Fields['Detail_Table'].Value, ATableName) = 0) and
          (CompareText(Fields['Master_Table'].Value, ALastTableName) = 0) and
          (CompareText(Fields['Detail_Table'].Value, ALastTableName) = 0) and
          (Fields['Follow_Down'].Value) then
        CopyJoinedRecords(
            Fields['Master_Table'].Value,
            Fields['Detail_Table'].Value,
            Fields['Master_Field'].Value,
            Fields['Detail_Field'].Value,
            true) ;
      MoveNext;
    end;
end;

procedure TDatabaseOutput.CreateTable(const ATableName: string);
var
  lPk: TPrimaryKey;
  lPkFields: string;
begin
  lPk := FDatabaseModule.SplitPrimaryKey(ATableName);
  lPkFields := lPk.Key1;
  if lPk.Key2<>'' then
    lPkFields := lPkFields + ', ' + lPk.Key2;
  if FTablesUsed.IndexOf(ATableName)=-1 then begin
    FDatabaseModule.ExecuteSQL(FORMAT(SQL_CREATE_TABLE, [
        GetRequiredFieldsForTable(ATableName),
        ATableName, ATableName]));
    FDatabaseModule.ExecuteSQL(FORMAT(SQL_ADD_PK, [
        ATableName, ATableName, lPkFields]));
    FTablesUsed.Add(ATableName);
  end;
end;

{-------------------------------------------------------------------------------
  Constructor for use when running under the main thread.
}
constructor TDatabaseOutput.Create(ADatabaseModule: TdmDatabase;
  const ADBName: String; const ASetStatusEvent: TSetStatusEvent;
  const AProgressBar: TTaskProgressBar);
begin
  InitialiseFields(ADatabaseModule, ADBName, ASetStatusEvent);
  FProgressBar := AProgressBar;
end;

{-------------------------------------------------------------------------------
  Constructor for use when running under a separate thread.
}
constructor TDatabaseOutput.Create(ADatabaseModule: TdmDatabase;
            const ADBName: String; const ASetStatusEvent: TSetStatusEvent;
            const ASetProgressEvent: TSetProgressEvent);
begin
  InitialiseFields(ADatabaseModule, ADBName, ASetStatusEvent);
  FSetProgress := ASetProgressEvent;
end;

{-------------------------------------------------------------------------------
  Initialise class fields
}
procedure TDatabaseOutput.InitialiseFields(ADatabaseModule: TdmDatabase;
            const ADBName: String; const ASetStatusEvent: TSetStatusEvent);
var
  lFileExt: String;
begin
  lFileExt := ExtractFileExt(ADBName);
  if not SameText(lFileExt, EXT_ZIP) then
    FDatabaseName := ADBName + EXT_MDB
  else
    FDatabaseName := Copy(ADBName, 1, Length(ADBName) - Length(lFileExt)) + EXT_MDB;

  FDatabaseModule := ADatabaseModule;
  FSetStatus      := ASetStatusEvent;

  FTablesUsed := TStringList.Create;
  FTablesUsed.Sorted := True;
  FTablesPopulated := TStringList.Create;
  FTablesPopulated.Sorted := True;
  FRequiredFields := TStringList.Create;
  FRequiredFields.Sorted := True;
  FCancelled := False;
  FCanExportSystemSupplied := False;
  FExportConfidentialOccurrences := false;
  FProcessedJoins := TStringList.Create;
  FProcessedJoins.Sorted := true;
  FUsingExportFilter := false; // default
end;    // TDatabaseOutput.InitialiseFields

{-------------------------------------------------------------------------------
}
procedure TDatabaseOutput.Execute(AKeyList: TKeyList; AWantObservations: Boolean);
begin
  Execute(AKeyList, nil, AWantObservations);
end;

{-------------------------------------------------------------------------------
}
procedure TDatabaseOutput.Execute(AKeyList, AInvalidKeys: TKeyList; AWantObservations: Boolean);
var
  lIdx: integer;
  lPk : TPrimaryKey;

    // Create a table in the set of temp export tables then populate it
    procedure InsertStartingTables;
    var
      i: integer;
      lFields: string;
    begin
      for i := 0 to FTablesPopulated.Count-1 do begin
        lFields := GetRequiredFieldsForTable(FTablesPopulated[i]);
        lPk := FDatabaseModule.SplitPrimaryKey(FTablesPopulated[i]);
        // Have got all the keys for this table, so insert into temp table and start process
        FDatabaseModule.ExecuteSQL('SELECT ' + lFields +
            ' INTO #' + FTablesPopulated[i] +
            ' FROM ' + FTablesPopulated[i] +
            ' WHERE ' + lPk.Key1 + ' IN (SELECT ItemKey COLLATE Database_Default FROM #ExportKeys'+
            ' WHERE TableName=''' + FTablesPopulated[i] + ''')'
            + GetFilterForTable(FTablesPopulated[i], ''));
      end;
    end;

    // Go through each source temp table and find all related data also required
    procedure ProcessSourceTempTables;
    var
      i: integer;
      lTablesInSourceList: TStringList;
    begin
      lTablesInSourceList := TStringlist.Create;
      try
        // take a snapshot of the initial list of tables
        lTablesInSourceList.Assign(FTablesPopulated);
        for i := 0 to lTablesInSourceList.Count-1 do
          ProcessTable(lTablesInSourceList[i], '');
      finally
        lTablesInSourceList.Free;
      end; //try
    end;

begin
  FInvalidKeys := AInvalidKeys;
  FTotalRows := 0;
  FDatabaseModule.ExecuteSQL('CREATE TABLE #ExportKeys (ItemKey CHAR(16), TableName VARCHAR(100))');
  FDatabaseModule.ExecuteSQL('CREATE TABLE #InvalidKeys (ItemKey CHAR(16), TableName VARCHAR(100))');
  try
    // Need to populate the invalid keys first.
    if Assigned(FInvalidKeys) then
      with FInvalidKeys do
        for lIdx := 0 to Header.ItemCount - 1 do
          FDatabaseModule.ExecuteSQL(
                    'INSERT INTO #InvalidKeys VALUES ('''
                    + Items[lIdx].KeyField1 + ''', '''
                    + ItemTable[lIdx] + ''')');

    try
      with AKeyList do
        for lIdx := 0 to Header.ItemCount - 1 do begin
          FDatabaseModule.ExecuteSQL(
              'INSERT INTO #ExportKeys VALUES ('''
              + Items[lIdx].KeyField1 + ''', '''
              + ItemTable[lIdx] + ''')');
          FTablesPopulated.Add(ItemTable[lIdx]);
          FTablesUsed.Add(ItemTable[lIdx]);
        end;
      InsertStartingTables;
      // Do we want observations for any locations or names in the list?
      if AWantObservations then GetObservations;
      ProcessSourceTempTables;
    finally
      FDatabaseModule.ExecuteSQL('DROP TABLE #ExportKeys');
    end;

    // Obtain names that entered or changed records
    DoDataEntryNames;
    // Collections specific data export
    ProcessMetadataTable;
    ProcessSourceJoinTable;
    // Now that we have all the tables and keys, create the database and export
    DoExport;
      // Compact the database before zipping it
    if not Cancelled then FDatabaseModule.CompactAccessDatabase(FDatabaseName, FSetStatus);
    // Zip the database
    if not Cancelled then
      DoZip;
  finally
    CleanupTempTables;
  end;
end;

{-------------------------------------------------------------------------------
}
destructor TDatabaseOutput.Destroy;
begin
  FTablesUsed.Free;
  FTablesPopulated.Free;
  FRequiredFields.Free;
  FProcessedJoins.Free;
  inherited;
end;

{-------------------------------------------------------------------------------
}
procedure TDatabaseOutput.CleanupTempTables;
var
  i: integer;
begin
  FDatabaseModule.ExecuteSQL('DROP TABLE #InvalidKeys');
  for i := 0 to FTablesUsed.Count-1 do
    FDatabaseModule.ExecuteSQL('DROP TABLE #' + FTablesUsed[i]);
end;


//==============================================================================
// Create the export Access database and export each table in turn. There are
// no referential integrity constraints in the target database, so the tables
// can be exported in whatever order.
function TDatabaseOutput.DoExport: String;
var liIdx        : Integer;
    lconnExportDB: TADOConnection;
    lQuery     : TADOQuery;
    lRecordsDone : Integer;

  //----------------------------------------------------------------------------
  procedure UpdateProgress;
  begin
    Inc(lRecordsDone, lQuery.RowsAffected);
    if FTotalRows>0 then
      SetProgress(lRecordsDone * 100 div FTotalRows);
  end;

  //----------------------------------------------------------------------------
  procedure RunInsert(const ATableName: String);
  var lPk: TPrimaryKey;
      lstSQL: String;
      lstKeyFields: string;
      lslKeys, lslKeys2 : TStringList;
      lMaxKeys : integer;
      lBlock : integer;
  begin
    lPk := FDatabaseModule.SplitPrimaryKey(ATableName);
    lstSQL := 'INSERT INTO [' + ATableName + '] ' +
              'SELECT * FROM [SQL_' + ATableName + '] ' +
              'WHERE [' + lPk.Key1 + '] IN (%s) ';
    lslKeys := TStringList.Create;
    if lPk.Key2 = '' then begin
      lMaxKeys := I_MAX_KEYS;
      lstKeyFields := lPk.Key1;
      lslKeys2 := nil;
    end else begin
      lMaxKeys := I_MAX_2_KEYS;
      lstSQL := lstSQL + ' AND [' + lPk.Key2 + '] IN (%s) ';
      lslKeys2 := TStringlist.Create;
      lstKeyFields := lPk.Key1 + '], [' + lPk.Key2;
    end;

    try
      with FDatabaseModule.ExecuteSQL('SELECT [' + lstKeyFields + '] FROM #' + ATableName, True) do
        while not EOF do begin
          lslKeys.Add('''' + Fields[lPk.Key1].Value + '''');
          if lPk.Key2 <> '' then begin
            if Fields[lPk.Key2].Type_ = 3 then // adInteger
              lslKeys2.Add(IntToStr(Fields[lPk.Key2].Value))
            else
              lslKeys2.Add('''' + Fields[lPk.Key2].Value + '''');
          end;
          MoveNext;
        end; // while

      if lslKeys.Count = 0 then Exit;

      if lslKeys.Count <= lMaxKeys then begin
        if lPk.Key2 = '' then
          lQuery.SQL.Text := Format(lstSQL, [lslKeys.CommaText])
        else
          lQuery.SQL.Text := Format(lstSQL, [lslKeys.CommaText, lslKeys2.CommaText]);
        lQuery.ExecSQL;
        UpdateProgress;
      end else begin
        // Query too big for Access so process in chunks.
        for lBlock := 0 to lslKeys.Count div lMaxKeys - 1 do begin
          if lPk.Key2 = '' then
            lQuery.SQL.Text := Format(lstSQL,
                [GetCommaSepItems(lslKeys, lBlock * lMaxKeys, lMaxKeys)])
          else
            lQuery.SQL.Text := Format(lstSQL,
                [GetCommaSepItems(lslKeys, lBlock * lMaxKeys, lMaxKeys),
                 GetCommaSepItems(lslKeys2, lBlock * lMaxKeys, lMaxKeys)]);
          lQuery.ExecSQL;
          UpdateProgress;
        end;
        // do last incomplete block of keys
        if (lslKeys.Count mod lMaxKeys) > 0 then begin
          if lPk.Key2 = '' then
            lQuery.SQL.Text := Format(lstSQL,
                [GetCommaSepItems(lslKeys, lMaxKeys * (lslKeys.Count div lMaxKeys), lMaxKeys)])
          else
            lQuery.SQL.Text := Format(lstSQL,
                [GetCommaSepItems(lslKeys, lMaxKeys * (lslKeys.Count div lMaxKeys), lMaxKeys),
                 GetCommaSepItems(lslKeys, lMaxKeys * (lslKeys2.Count div lMaxKeys), lMaxKeys)]);
          lQuery.ExecSQL;
          UpdateProgress;
        end;
      end;
    finally
      lslKeys.Free;
    end; // try
  end;  // RunInsert

  //----------------------------------------------------------------------------
begin
  Result := '';
  lQuery := TADOQuery.Create(nil);
  if Cancelled then Exit;
  SetProgress(0);
  lRecordsDone := 0;
  try
    lconnExportDB := FDatabaseModule.CreateImportExportDatabase(FDatabaseName);
    if Assigned(lconnExportDB) then
    try
      lconnExportDB.Open;
      // Point the query towards Access database
      lQuery.Connection := lconnExportDB;

      with FTablesPopulated do for liIdx := 0 to Count - 1 do begin
        if Cancelled then Exit;
        // Update Status bar.
        SetStatus(Format(ResStr_ExportingData,
                         [liIdx + 1, Count,
                         ReadableFormat(Strings[liIdx])]));
        FDatabaseModule.CreateImportExportTable(lconnExportDB, Strings[liIdx]);
        FDatabaseModule.CreateLinkedTable(lconnExportDB, Strings[liIdx], 'SQL_' + Strings[liIdx]);
        try
          RunInsert(Strings[liIdx]);
          // Return the name that the exported zip file will have.
          Result := FDatabaseName;
        finally
          FDatabaseModule.RemoveLinkedTable(lconnExportDB, 'SQL_' + Strings[liIdx]);
        end;
      end;
      lconnExportDB.Close;
    finally
      // Make sure the database is closed, otherwise, zipping won't work
      lconnExportDB.Free;
      lQuery.Free;
    end;
  except
    raise;
  end;
end;  // DoExport



{-------------------------------------------------------------------------------
}
procedure TDatabaseOutput.SetCancelled(const Value: boolean);
begin
  FCancelled := Value;
end;


{-------------------------------------------------------------------------------
  Callback function to update status on main application form
}
procedure TDatabaseOutput.SetStatus(const AMessage: String);
begin
  if Assigned(FSetStatus) then FSetStatus(AMessage);
end;  // SetStatus

{-------------------------------------------------------------------------------
  Callback function to update status on main application form
}
procedure TDatabaseOutput.SetProgress(const AProgress: Integer);
begin
  if Assigned(FSetProgress) then FSetProgress(AProgress);
  if Assigned(FProgressBar) then FProgressBar.TaskPosition := AProgress;
end;  // SetProgress


{-------------------------------------------------------------------------------
  Return a chunk of comma separated strings from a string list.
}
function TDatabaseOutput.GetCommaSepItems(const AStringList: TStringList;
  const AStartPos: Integer; const AAmount: Integer = I_MAX_KEYS): String;
var i, lMaxAllowed: Integer;
begin
  // CommaText property returns the complete stringlist in a string.
  // We only want a few items comma separated, so we do it ourselves.
  Result := '';

  // Work out the maximum allowed, so we don't have to test inside the loop
  lMaxAllowed := AAmount - 1;
  if AStartPos + lMaxAllowed > AStringList.Count - 1 then
    lMaxAllowed := AStringList.Count - AStartPos - 1;

  for i := 0 to lMaxAllowed do  // The -1 has been included already
    Result := Result + ',' + AStringList[i + AStartPos];

  // Remove the first comma from the string, or SQL won't like it.
  if Result <> '' then Result[1] := ' ';
end;  // GetCommaSepItems

{-------------------------------------------------------------------------------
  Compress the export db
}
procedure TDatabaseOutput.DoZip;
begin
  if Cancelled then Exit;
  SetStatus(ResStr_CompressExpDB);
  FZipTool := TVCLZip.Create(nil);
  try
    // Setup component ready for zipping the database
    with FZipTool do begin
      // Setup the progress bar feedback
      OnTotalPercentDone := ZipTotalPercentDone;
      // Store user's date format, since we are going to force ISO dates for the file name
      ZipName := ExtractFilePath(FDatabaseName) + ExtractWithoutExt(FDatabaseName) + EXT_ZIP;

      Recurse    := False;
      StorePaths := False;
      PackLevel  := 9;
      DoProcessMessages := true;
      // Add file name to list of files to zip (only one)
      FilesList.Add(FDatabaseName);
    end;
    try
      if FileExists(FZipTool.ZipName) then DeleteFile(FZipTool.ZipName);
      FZipTool.Zip;
      SetStatus(ResStr_ExportDBCompressed);
      // Get rid of export Access db, it's available in zip now
      DeleteFile(FDatabaseName);
    except
      on EUserCanceled do;
    end;
  finally
    FZipTool.Free;
  end; // try
end;  // DoZip

{-------------------------------------------------------------------------------
  Callback for zip progress
}
procedure TDatabaseOutput.ZipTotalPercentDone(Sender: TObject; Percent:
    LongInt);
begin
  SetProgress(Percent);
end;  // ZipTotalPercentDone


{-------------------------------------------------------------------------------
  If requested, populate the list of observations associated with the selected
     data so that they are exported too.
}
procedure TDatabaseOutput.GetObservations;
begin
  // Include events or samples for any locations
  if FTablesUsed.IndexOf('LOCATION')<>-1 then begin
    CopyJoinedRecords('Location', 'Survey_Event', 'Location_Key', 'Location_Key', True, False);
    CopyJoinedRecords('Location', 'Sample', 'Location_Key', 'Location_Key', True, False);
  end;
  // Include surveys run by or events for a name, individual or organisation
  if FTablesUsed.IndexOf('NAME')<>-1 then begin
    CopyJoinedRecords('Name', 'Survey', 'Name_Key', 'Run_By', True, False);
    CopyJoinedRecords('Name', 'Survey_Event_Recorder', 'Name_Key', 'Name_Key', True, False);
  end;
  if FTablesUsed.IndexOf('Individual')<>-1 then begin
    CopyJoinedRecords('Individual', 'Survey', 'Name_Key', 'Run_By', True, False);
    CopyJoinedRecords('Individual', 'Survey_Event_Recorder', 'Name_Key', 'Name_Key', True, False);
  end;
  // Organisations cannot be survey event recorder
  if FTablesUsed.IndexOf('Organisation')<>-1 then
    CopyJoinedRecords('Organisation', 'Survey', 'Name_Key', 'Run_By', True, False);
  // Use the survey event recorder field to establish the samples required
  if (FTablesUsed.IndexOf('Name')<>-1) or (FTablesUsed.IndexOf('Individual')<>-1) then begin
    CopyJoinedRecords('Survey_Event_Recorder', 'Sample_Recorder',
        'Se_Recorder_Key', 'Se_Recorder_Key', True, False);
    CopyJoinedRecords('Sample_Recorder', 'Sample',
        'Sample_Key', 'Sample_Key', True, False);
  end;
end;

{-------------------------------------------------------------------------------
  Identify Metadata records for the output dataset, if the Collections Module is
      installed.
}
procedure TDatabaseOutput.ProcessMetadataTable;
var i     : integer;
    lSQL  : String;
    lPk   : TPrimaryKey;
begin
  SetStatus(ResStr_ProcessingMetadata);
  try
    FDatabaseModule.ExecuteSQL('SELECT TOP 0 * FROM Metadata');
  except
    // We get an exception if the table can't be found. That means no Collections module.
    // So we're finished here.
    on E:EOleException do
      Exit;
  end;
  CreateTable('METADATA');
  CreateTable('METADATA_TYPE');
  for i := 0 to FTablesPopulated.Count-1 do begin
    lPk := FDatabaseModule.SplitPrimaryKey(FTablesPopulated[i]);
    if lPk.Key2='' then begin
      // Need to filter on TableName AND Record_Key's
      lSQL := 'INSERT INTO #Metadata ' +
              'SELECT DISTINCT M.Metadata_Key, M.Metadata_Type_Key, M.System_Supplied_Data ' +
              'FROM Metadata_Type MT ' +
              'INNER JOIN Metadata M ON M.Metadata_Type_Key = MT.Metadata_Type_Key ' +
              'WHERE MT.Table_Name = ''' + FTablesPopulated[i] + ''' ' +
              'AND M.Record_Key IN (SELECT ' + lPk.Key1 +
              ' FROM #' + FTablesPopulated[i] + ')';
      if not FCanExportSystemSupplied then
        lSql := lSql + ' AND M.System_Supplied_Data=0';
      try
        if FDatabaseModule.ExecuteSQLGetRowsAffected(lSQL)>0 then
          FTablesPopulated.Add('METADATA');
      except on E:Exception do
        ShowMessage(E.Message);
      end;
    end;
  end;
  if FTablesPopulated.IndexOf('METADATA')<>-1 then
    CopyJoinedRecords('Metadata', 'Metadata_Type', 'Metadata_Type_Key',
          'Metadata_Type_Key', True, False);
end;  // ProcessMetadataTable

{-------------------------------------------------------------------------------
}
procedure TDatabaseOutput.ProcessSourceJoinTable;
var i     : integer;
    lSQL  : String;
    lPk   : TPrimaryKey;
begin
  SetStatus(ResStr_ProcessingSources);
  try
    FDatabaseModule.ExecuteSQL('SELECT TOP 0 * FROM Source_Join');
  except
    // We get an exception if the table can't be found. That means no Collections module.
    // So we're finished here.
    on E:EOleException do
      Exit;
  end;
  CreateTable('SOURCE_JOIN');
  CreateTable('SOURCE');
  for i := 0 to FTablesPopulated.Count-1 do begin
    lPk := FDatabaseModule.SplitPrimaryKey(FTablesPopulated[i]);
    if lPk.Key2='' then begin
      // Need to filter on TableName AND Record_Key's
      lSQL := 'INSERT INTO #Source_Join (Source_Join_Key, Source_Key, System_Supplied_Data) ' +
              'SELECT DISTINCT SJ.Source_Join_Key, SJ.Source_Key, SJ.System_Supplied_Data ' +
              'FROM Source_Join SJ ' +
              // Some records may already be there through ProcessTable, so skip'em.
              'LEFT JOIN #Source_Join TSJ ON TSJ.Source_Join_Key = SJ.Source_Join_Key ' +
              'WHERE SJ.Table_Name = ''' + FTablesPopulated[i] + ''' ' +
              'AND TSJ.Source_Join_Key IS NULL ' +
              'AND SJ.Record_Key IN (SELECT ' + lPk.Key1 +
              ' FROM #' + FTablesPopulated[i] + ')';
      if not FCanExportSystemSupplied then
        lSql := lSql + ' AND SJ.System_Supplied_Data=0 ';
      try
        if FDatabaseModule.ExecuteSQLGetRowsAffected(lSQL)>0 then
          FTablesPopulated.Add('SOURCE_JOIN');
      except on E:Exception do
        ShowMessage(E.Message);
      end;
    end;
  end;
  if FTablesPopulated.IndexOf('SOURCE_JOIN')<>-1 then
    CopyJoinedRecords('Source_Join', 'Source', 'Source_Key', 'Source_Key', True, True);
end;

{-------------------------------------------------------------------------------
}
function TDatabaseOutput.GetUserAccessLevel: Integer;
begin
  Result := Integer(FUserAccessLevel);
end; // TDatabaseOutput.GetUserAccessLevel

{-------------------------------------------------------------------------------
}
procedure TDatabaseOutput.SetUserAccessLevel(const Value: Integer);
begin
 FUserAccessLevel := TUserAccessLevel(Value);
end;  // TDatabaseOutput.SetUserAccessLevel

{-------------------------------------------------------------------------------
  Accessor - are we using an export filter? (which changes the filter applied
     for occurrence records)
}
procedure TDatabaseOutput.SetUsingExportFilter(const Value: Boolean);
begin
  FUsingExportFilter := Value;
end;

{-------------------------------------------------------------------------------
  Accessor
}
procedure TDatabaseOutput.SetExportConfidentialOccurrences(
  const Value: Boolean);
begin
  FExportConfidentialOccurrences := Value;
end;

end.

