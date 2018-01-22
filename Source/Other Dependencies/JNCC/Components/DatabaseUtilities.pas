//==============================================================================
//  Unit:        DatabaseUtilities
//
//  Implements:  Database management functions
//
//  Description: Functions include managament of Taxon Group Indices
//               Adding relationships using DAO
//
//  Author:      John van Breda
//  Created:     05/03/2002
//
//  Changes:     Eric Salmon - 18 March 2002
//               Check for existing Taxon Group Index database, and if found,
//               don't try to create it again.
//               Progress bar while generating group indexes.
//
//               Eric Salmon - 03/04/2002
//               Adapted to work with D5/ADO. Assumption is no separate database
//               for Taxon Group Index table when using SQL Server, as it can
//               handle large amount of data.
//
//  Last Revision Details:
//    $Revision: 26 $
//    $Date: 28/12/07 13:40 $
//    $Author: Rickyshrestha $
//
//  $History: DatabaseUtilities.pas $
//  
//  *****************  Version 26  *****************
//  User: Rickyshrestha Date: 28/12/07   Time: 13:40
//  Updated in $/JNCC/Components
//  Changed some hardcoded strings to resourcestring
//  
//  *****************  Version 25  *****************
//  User: Johnvanbreda Date: 21/03/06   Time: 16:28
//  Updated in $/JNCC/Components
//  CCN135
//  Has_Children populated in Index_Taxon_Name
//  
//  *****************  Version 24  *****************
//  User: Johnvanbreda Date: 25/01/05   Time: 14:01
//  Updated in $/JNCC/Components
//  IR8391
//  Implemented SQL supplied by Hannah for ITN.Preferred_List field update
//  
//  *****************  Version 23  *****************
//  User: Johnvanbreda Date: 21/01/05   Time: 16:02
//  Updated in $/JNCC/Components
//  Removed recommended sort order updated for Index Taxon Name as now in
//  usp_IndexTaxonName_ApplyNameServer.
//  
//  *****************  Version 22  *****************
//  User: Johnvanbreda Date: 20/12/04   Time: 9:37
//  Updated in $/JNCC/Components
//  IR7527 (CCN121) - Sort Order populated
//  
//  *****************  Version 21  *****************
//  User: Johnvanbreda Date: 25/11/04   Time: 16:25
//  Updated in $/JNCC/Components
//  CCN121
//  Updating of Recommended_Taxon_lisT_item_Key
//  
//  *****************  Version 20  *****************
//  User: Ericsalmon   Date: 25/02/03   Time: 16:07
//  Updated in $/JNCC/Components
//  Cleanup. Remove compiler hint.
//  
//  *****************  Version 19  *****************
//  User: Johnvanbreda Date: 25/02/03   Time: 10:43
//  Updated in $/JNCC/Components
//  ID513
//  Taxon group index rebuild fix + progress bar work
//  
//  *****************  Version 18  *****************
//  User: Ericsalmon   Date: 20/02/03   Time: 10:00
//  Updated in $/JNCC/Components
//  Update. R2K_Administrator role allowed to use TRUNCATE TABLE, removing
//  the need for stored proc.
//  
//  *****************  Version 17  *****************
//  User: Ericsalmon   Date: 17/02/03   Time: 10:37
//  Updated in $/JNCC/Components
//  Improved index taxon name/synonym rebuild.
//  
//  *****************  Version 16  *****************
//  User: Ericsalmon   Date: 12/02/03   Time: 18:55
//  Updated in $/JNCC/Components
//  IR 482 fixed and improved.
//  
//  *****************  Version 15  *****************
//  User: Ericsalmon   Date: 10/02/03   Time: 12:09
//  Updated in $/JNCC/Components
//  Fixes and improvements.
//  
//  *****************  Version 14  *****************
//  User: Pollyshaw    Date: 14/01/03   Time: 8:59
//  Updated in $/JNCC/Components
//  Fixed problem with rebuilding taxon synonyms
//  
//  *****************  Version 13  *****************
//  User: Ericsalmon   Date: 4/12/02    Time: 18:22
//  Updated in $/JNCC/Components
//  Removed USE_TITAN compiler directive and D4-specific code.
//  
//  *****************  Version 12  *****************
//  User: Johnvanbreda Date: 3/12/02    Time: 14:29
//  Updated in $/JNCC/Components
//  Index taxon synonyms fix
//  
//  *****************  Version 11  *****************
//  User: Johnvanbreda Date: 27/11/02   Time: 15:28
//  Updated in $/JNCC/Components
//  hourglasses
//  
//  *****************  Version 10  *****************
//  User: Johnvanbreda Date: 27/11/02   Time: 15:12
//  Updated in $/JNCC/Components
//  Rebuild index progress bars
//
//  *****************  Version 9  *****************
//  User: Ericsalmon   Date: 21/11/02   Time: 16:03
//  Updated in $/JNCC/Components
//  Changed table name TAXON_GROUP_INDEX to INDEX_TAXON_GROUP
//  
//  *****************  Version 8  *****************
//  User: Ericsalmon   Date: 20/11/02   Time: 15:38
//  Updated in $/JNCC/Components
//  CCN83 - Queries to rebuild Index_Taxon_Name and Index_Taxon_Synonym
//  tables.
//
//  *****************  Version 7  *****************
//  User: Ericsalmon   Date: 24/06/02   Time: 16:33
//  Updated in $/JNCC/Components
//  Removed CreateTaxonGroupIndex, as it is not needed any longer.
//
//  Copyright � Dorset Software Services Ltd, 2001
//
//==============================================================================

unit DatabaseUtilities;

interface

uses
  Sysutils, Classes, StdCtrls, Forms, ADODB, GeneralFunctions, Controls;

type
  TSetStatusEvent = procedure(const iStatusString: String; iPrcMsgs: Boolean = true) of object;
  TSetProgressEvent = procedure(const iProgress: Integer; iPrcMsgs: Boolean = true) of object;

procedure PopulateTaxonGroupIndex(iGeneralQuery: TADOQuery;
  iSetStatus: TSetStatusEvent; iSetProgress: TSetProgressEvent);

procedure ClearSystemTaxonGroupIndex(iGeneralQuery: TADOQuery);

procedure RebuildIndexTaxonName(iGeneralQuery: TADOQuery;
  iSetStatus: TSetStatusEvent; iSetProgress: TSetProgressEvent);

procedure RebuildIndexTaxonSynonym(iGeneralQuery: TADOQuery;
  iSetStatus: TSetStatusEvent; iSetProgress: TSetProgressEvent);

//==============================================================================
implementation

const
  SQLBoolStr: array[Boolean] of String = ('0', '1');

  SQL_APPLY_NAMESERVER =
      'EXEC usp_IndexTaxonName_ApplyNameServer';

resourcestring
  ResStr_ClearingOldNameIndex = 'Clearing old name index...';
  ResStr_PopRecommendedNames = 'Populating recommended names from NameServer...';
  ResStr_AddingTaxonName = 'Adding taxon names to index...';
  ResStr_AddingTaxonUserName = 'Adding taxon user names to index...';
  ResStr_ClearingOldSynonymInd = 'Clearing old synonym index...';
  ResStr_AddinTaxonSynonym =  'Adding taxon synonyms to index...';


//==============================================================================
{ Fill the taxon group index.  This creates a table with a join between every
    taxon list item, and every item beneath it in the hierarchy,
    note - use the LEVEL field to temporarily indicate which
    items we have searched for children on.  This saves the iterative process
    from requerying everything every time, so its much faster.
    If the label is supplied, then some progress information is passed to it.
    iSetStatus is a callback method for progress reporting }
procedure PopulateTaxonGroupIndex(iGeneralQuery: TADOQuery;
  iSetStatus: TSetStatusEvent; iSetProgress: TSetProgressEvent);
var
  lLevelCount, lProgress    : Integer;
  lCursor                   : TCursor;
  lFirstLevelRows           : integer;
const
  SQL_FIRST_LEVEL=
    'INSERT INTO Index_Taxon_Group '+
    '    (Taxon_List_Item_Key, Contained_List_Item_Key, System_Supplied_Data, Item_Level) '+
    'SELECT Taxon_List_Item_Key, Taxon_List_Item_Key, 1, 0 FROM Taxon_List_Item';
  SQL_OTHER_LEVEL=
    'INSERT INTO Index_Taxon_Group '+
    '      (Taxon_List_Item_Key, Contained_List_Item_Key, System_Supplied_Data, Item_Level) '+
    'SELECT ITG.Taxon_List_Item_Key, TLI.Taxon_List_Item_KEy as Contained_List_ITem_Key, 1, %s '+
    '  FROM Index_Taxon_Group ITG '+
    '        INNER JOIN Taxon_List_Item TLI ON ITG.Contained_List_Item_Key = TLI.Parent '+
    '        LEFT JOIN Index_Taxon_Group Existing on (Existing.Taxon_List_Item_Key = ITG.Taxon_List_Item_Key '+
    '              And Existing.Contained_List_Item_Key=TLI.Taxon_List_Item_Key) '+
    '  WHERE ITG.Item_Level=%s '+
    '      AND Existing.Taxon_List_Item_Key is null';
resourcestring
  ResStr_ProgressLabel = 'Preparing taxon group index (Level %s)...';
begin
  lCursor := HourglassCursor;
  try
    lProgress := 0;
    if Assigned(iSetStatus) then
      iSetStatus(Format(ResStr_ProgressLabel, ['1']));
    if Assigned(iSetProgress) then
      iSetProgress(0);
    with iGeneralQuery do begin
      Close; // just in case
      // First create 1-1 joins so that groups include themselves
      SQL.Text := SQL_FIRST_LEVEL;
      ExecSQL;
      lFirstLevelRows := Round(RowsAffected*0.6); // for progress estimation
      if assigned(iSetProgress) then begin
        lProgress:=20;
        iSetProgress(lProgress); // an arbitrary value for the first level
      end;
      lLevelCount := 0;
      while RowsAffected>0 do begin
        SQL.Text := Format(SQL_OTHER_LEVEL, [IntToStr(lLevelCount+1), IntToStr(lLevelCount)]);
        if Assigned(iSetStatus) then
          iSetStatus(Format(ResStr_ProgressLabel, [IntToStr(lLevelCount+2)]));
        ExecSQL;
        if Assigned(iSetProgress) then begin
          if 20 + 80 * ((lFirstLevelRows-RowsAffected)/lFirstLevelRows) > lProgress then begin
            lProgress:=Round(20 + 80 * ((lFirstLevelRows-RowsAffected)/lFirstLevelRows));
            iSetProgress(lProgress);
          end;
        end;
        Inc(lLevelCount);
      end; // while
    end; // with iGeneralQuery
  finally
    DefaultCursor(lCursor);
  end; // try
end;  // PopulateTaxonGroupIndex

//==============================================================================
{ Procedure to clear the taxon group index before it is refilled.  Only system
     supplied items are removed (i.e. not arbitrary groups }
procedure ClearSystemTaxonGroupIndex(iGeneralQuery: TADOQuery);
begin
  with iGeneralQuery do begin
    Close; // just in case
    SQL.Text := 'DELETE FROM Index_Taxon_Group WHERE System_Supplied_Data = 1';
    ExecSQL;
  end;
end;  // ClearSystemTaxonGroupIndex

//==============================================================================
{ Description : Private function that returns an list of keys that define the
     boundaries of groups of taxon list item records, i.e. the first key returned
     is the key that marks the first 5% of records etc...
  Updated for speed. A bit more complex, but more efficient.
  Created : 27/11/2002 }
function GetTaxonListItemSegments(iGeneralQuery: TADOQuery; ASegmentCount: Integer): TStringList;
var
  lTLIStep, lSegmentIndex : integer;
begin
  Result := TStringList.Create;
  with iGeneralQuery do begin
    SQl.Text := 'SELECT Count(*) FROM Taxon_List_Item';
    Open;
    lTLIStep := Fields[0].AsInteger;
    Close;
    if lTLIStep > ASegmentCount then lTLIStep := lTLIStep div ASegmentCount
                                else ASegmentCount := 1;
    // Create temp table to help gathering keys
    SQL.Text := 'CREATE TABLE "#Temp" (TLI_Number int IDENTITY(1,1), Taxon_List_Item_Key char(16))';
    ExecSQL;
    // Shove TLI_Keys in temp table
    SQL.Text := 'INSERT INTO "#Temp" SELECT Taxon_List_Item_Key FROM Taxon_List_Item ORDER BY Taxon_List_Item_Key';
    ExecSQL;
    // Use the Identity field to get the TLIKeys we're interested in directly, no SLOOOWWWW MoveBy
    for lSegmentIndex := 1 to ASegmentCount do begin
      SQL.Text := 'SELECT Taxon_List_Item_Key FROM "#Temp" WHERE TLI_Number = ' + IntToStr(lTLIStep * lSegmentIndex);
      Open;
      Result.Add(FieldByName('Taxon_List_Item_Key').AsString);
      Close;
    end;
    // Remove temp table, now that we finished with it
    SQL.Text := 'DROP TABLE "#Temp"';
    ExecSQL;
  end;
end;

{-------------------------------------------------------------------------------
  Code to populate Recommended_Taxon_List_Item_Key in Index_Taxon_Name
}
procedure PopulateRecommendedNames(iGeneralQuery: TADOQuery;
  iSetStatus: TSetStatusEvent; iSetProgress: TSetProgressEvent);
begin
  iSetProgress(0);
  iSetStatus(ResStr_PopRecommendedNames);
  try
    with iGeneralQuery do begin
      if Active then Close;
      SQL.Clear;
      SQL.Text := SQL_APPLY_NAMESERVER;
      ExecSQL;
    end;
  finally
    iSetStatus('');
  end; // try
end;

//==============================================================================
procedure RebuildIndexTaxonName(iGeneralQuery: TADOQuery;
  iSetStatus: TSetStatusEvent; iSetProgress: TSetProgressEvent);
var lSegmentKeys: TStringList;
    i           : Integer;
    lProgress   : Single;
    lCursor     : TCursor;
begin
  lCursor := HourglassCursor;
  try
    with iGeneralQuery do begin
      if Active then Close;
      iSetProgress(0);
      iSetStatus(ResStr_ClearingOldNameIndex);
      // Use faster TRUNCATE TABLE to avoid log of every single row being deleted
      // Only R2K_Administrator roles are allowed to use TRUNCATE TABLE
      SQL.Text := 'TRUNCATE TABLE Index_Taxon_Name';
      ExecSQL;
      iSetProgress(2);
      iSetStatus(ResStr_AddingTaxonName);
      lSegmentKeys := GetTaxonListItemSegments(iGeneralQuery, 20);
      // Work out the amount by which to increase the progress bar.
      lProgress := (87 / lSegmentKeys.Count);
      try
        // Populate the table with names and common names
        SQL.Clear;
        SQL.Text :=
                'INSERT INTO Index_Taxon_Name (Taxon_List_Item_Key, Taxon_List_Version_Key, ' +
                '  Actual_Name, Actual_Name_Italic, Common_Name, Common_Name_Italic, ' +
                '  Preferred_Name, Preferred_Name_Italic, Abbreviation, Authority, System_Supplied_Data)';
        SQL.Add('SELECT TLI.Taxon_List_Item_Key, TLI.Taxon_List_Version_Key, ' +
                '  T.Item_Name, CASE WHEN TR3.List_Font_Italic = 1 AND T.Language = ''La'' THEN 1 ELSE 0 END, ' +
                '  T2.Item_Name, CASE WHEN TR3.List_Font_Italic = 1 AND T2.Language = ''La'' THEN 1 ELSE 0 END, ' +
                '  T3.Item_Name, CASE WHEN TR3.List_Font_Italic = 1 AND T3.Language = ''La'' THEN 1 ELSE 0 END, ' +
                '  T.Abbreviation, T.Authority, 1');
        SQL.Add('FROM Taxon_List_Item AS TLI ' +
                'LEFT JOIN Taxon_version AS TV ON TV.Taxon_Version_Key = TLI.Taxon_Version_Key ' +
                'LEFT JOIN Taxon AS T ON T.Taxon_Key = TV.Taxon_Key ' +
                'LEFT JOIN Taxon_Common_Name AS TCN ON TCN.Taxon_List_Item_Key = TLI.Taxon_List_Item_Key ' +
                'LEFT JOIN Taxon_Version AS TV2 ON TV2.Taxon_Version_Key = TCN.Taxon_Version_Key ' +
                'LEFT JOIN Taxon AS T2 ON T2.Taxon_Key = TV2.Taxon_Key ');
        SQL.Add('LEFT JOIN Taxon_List_Item AS TLI3 ON TLI3.Taxon_List_Item_Key = TLI.Preferred_Name ' +
                'LEFT JOIN Taxon_Rank AS TR3 ON TR3.Taxon_Rank_Key = TLI3.Taxon_Rank_Key ' +
                'LEFT JOIN Taxon_Version AS TV3 ON TV3.Taxon_Version_Key = TLI3.Taxon_Version_Key ' +
                'LEFT JOIN Taxon AS T3 ON T3.Taxon_Key = TV3.Taxon_Key ' +
                'WHERE TLI.Taxon_List_Version_To IS NULL');
        SQL.Add('');
        for i := 0 to lSegmentKeys.Count do begin
          if i = 0 then
            SQL[4] := 'AND TLI.Taxon_List_Item_Key < ''' + lSegmentKeys[0] + ''' '
          else if i = lSegmentKeys.Count then
            SQL[4] := 'AND TLI.Taxon_List_Item_Key >= ''' + lSegmentKeys[i - 1] + ''' '
          else
            SQL[4] := 'AND TLI.Taxon_List_Item_Key >= ''' + lSegmentKeys[i - 1] + ''' '+
                      'AND TLI.Taxon_List_Item_Key < ''' + lSegmentKeys[i] + ''' ';
          ExecSQL;
          iSetProgress(2 + Trunc(i * lProgress));  // up to 89%
        end;
      finally
        lSegmentKeys.Free;
      end; // try
      // Populate the table with user names
      iSetStatus(ResStr_AddingTaxonUserName);
      SQL.Clear;
      SQL.Text :=
              'INSERT INTO Index_Taxon_Name ( Taxon_List_Item_Key, Taxon_List_Version_Key, ' +
              '   Actual_Name, Actual_Name_Italic, Common_Name, Common_Name_Italic, ' +
              '   Preferred_Name, Preferred_Name_Italic, System_Supplied_Data)';
      SQL.Add('SELECT TLI.Taxon_List_Item_Key, TLI.Taxon_List_Version_Key, ' +
              '  TUN.Item_Name, CASE WHEN TR3.List_Font_Italic = 1 AND TUN.Language = ''La'' THEN 1 ELSE 0 END, ' +
              '  T2.Item_Name, CASE WHEN TR3.List_Font_Italic = 1 AND T2.Language = ''La'' THEN 1 ELSE 0 END, ' +
              '  T3.Item_Name, CASE WHEN TR3.List_Font_Italic = 1 AND T3.Language = ''La'' THEN 1 ELSE 0 END, ' +
              '  0');
      SQL.Add('FROM Taxon_User_Name AS TUN ' +
              'LEFT JOIN Taxon_List_Item AS TLI ON TLI.Taxon_List_Item_Key = TUN.Taxon_List_Item_Key ' +
              'LEFT JOIN Taxon_version AS TV ON TV.Taxon_Version_Key = TLI.Taxon_Version_Key ' +
              'LEFT JOIN Taxon_Common_Name AS TCN ON TCN.Taxon_List_Item_Key = TLI.Taxon_List_Item_Key ' +
              'LEFT JOIN Taxon_Version AS TV2 ON TV2.Taxon_Version_Key = TCN.Taxon_Version_Key ' +
              'LEFT JOIN Taxon AS T2 ON T2.Taxon_Key = TV2.Taxon_Key ');
      SQL.Add('LEFT JOIN Taxon_List_Item AS TLI3 ON TLI3.Taxon_List_Item_Key = TLI.Preferred_Name ' +
              'LEFT JOIN Taxon_Rank AS TR3 ON TR3.Taxon_Rank_Key = TLI3.Taxon_Rank_Key ' +
              'LEFT JOIN Taxon_Version AS TV3 ON TV3.Taxon_Version_Key = TLI3.Taxon_Version_Key ' +
              'LEFT JOIN Taxon AS T3 ON T3.Taxon_Key = TV3.Taxon_Key ' +
              'WHERE TLI.Taxon_List_Version_To IS NULL');
      ExecSQL;
      iSetProgress(90);
      SQL.Clear;
      SQL.Text :=
              'UPDATE Index_Taxon_Name ' +
              'SET Common_Name = TUN.Item_Name, ' +
              'Common_Name_Italic = CASE WHEN TR.List_Font_Italic=1 AND TUN.Language=''La'' THEN 1 ELSE 0 END ' +
              'FROM Index_Taxon_Name ITN ' +
              'INNER JOIN Taxon_User_Name TUN ON TUN.Taxon_List_Item_Key = ITN.Taxon_List_Item_Key ' +
              'INNER JOIN Taxon_List_Item TLI ON TLI.Taxon_List_Item_Key = ITN.Taxon_List_Item_Key ' +
              'INNER JOIN Taxon_Rank TR ON TR.Taxon_Rank_Key = TLI.Taxon_Rank_Key ' +
              'WHERE TUN.Preferred = 1';
      ExecSQL;
      iSetProgress(92);
      SQL.Clear;
      SQL.Text :=
              'UPDATE ITN '+
              'SET ITN.Preferred_List=TL.Preferred, ITN.Allow_Data_Entry=TLT.Allow_Data_Entry '+
              'FROM Index_Taxon_Name ITN '+
              'INNER JOIN Taxon_List_Version TLV ON TLV.Taxon_List_Version_Key = ITN.Taxon_List_Version_Key '+
              'INNER JOIN Taxon_List TL ON TL.Taxon_List_Key=TLV.Taxon_List_Key '+
              'INNER JOIN Taxon_List_Type TLT ON TL.Taxon_List_Type_Key=TLT.Taxon_List_Type_Key';
      ExecSQL;
      iSetProgress(94);
      SQL.Clear;
      SQL.Text :=
              'UPDATE ITN '+
              'SET ITN.Preferred_List=1 '+
              'FROM TAXON_LIST_ITEM TLI '+
              'INNER JOIN INDEX_TAXON_NAME ITN ON ITN.TAXON_LIST_ITEM_KEY=TLI.TAXON_LIST_ITEM_KEY '+
              'INNER JOIN TAXON_VERSION TV ON TLI.TAXON_VERSION_KEY = TV.TAXON_VERSION_KEY '+
              'INNER JOIN TAXON_GROUP TG ON TV.OUTPUT_GROUP_KEY = TG.TAXON_GROUP_KEY '+
              'INNER JOIN TAXON_LIST_VERSION TLV'+
              '      ON TLI.TAXON_LIST_VERSION_KEY = TLV.TAXON_LIST_VERSION_KEY  '+
              '      AND TG.USE_TAXON_LIST_KEY = TLV.TAXON_LIST_KEY';
      ExecSQL;
      iSetProgress(96);
      SQL.Clear;
      SQL.Text :=
              'UPDATE ITN '+
              'SET Has_Children=1 '+
              'FROM Index_Taxon_Name ITN '+
              'INNER JOIN Taxon_List_Item TLIChild '+
              ' 	ON TLIChild.Parent=ITN.Taxon_List_Item_Key';
      ExecSQL;
      iSetProgress(100);
      PopulateRecommendedNames(iGeneralQuery, iSetStatus, iSetProgress);
    end;
  finally
    DefaultCursor(lCursor);
    iSetProgress(0);
    iSetStatus('');
  end; // try
end;

//==============================================================================
procedure RebuildIndexTaxonSynonym(iGeneralQuery: TADOQuery;
  iSetStatus: TSetStatusEvent; iSetProgress: TSetProgressEvent);
var lSegmentKeys: TStringList;
    i           : Integer;
    lProgress   : Single;
    lCursor     : TCursor;
begin
  lCursor := HourglassCursor;
  try
    with iGeneralQuery do begin
      if Active then Close;
      iSetStatus(ResStr_ClearingOldSynonymInd);
      iSetProgress(0);
      // Only R2K_Administrator roles are allowed to use TRUNCATE TABLE
      SQL.Text := 'TRUNCATE TABLE Index_Taxon_Synonym';
      ExecSQL;
      iSetProgress(2);
      // Populate the table with names and common names
      iSetStatus(ResStr_AddinTaxonSynonym);
      lSegmentKeys := GetTaxonListItemSegments(iGeneralQuery, 40);
      // Work out the amount by which to increase the progress bar.
      lProgress := (98 / lSegmentKeys.Count);
      SQL.Clear;
      SQL.Text :=
              'INSERT INTO Index_Taxon_Synonym ' +
              'SELECT DISTINCT TLI.Taxon_List_Item_Key AS Taxon_List_Item_Key, TLI3.Taxon_List_Item_Key AS Synonym_List_Item_Key ';
      SQL.Add('FROM (Taxon_List_Item AS TLI ' +
              'INNER JOIN Taxon_List_Item AS TLI2 ON TLI2.Taxon_Version_Key = TLI.Taxon_Version_Key) ' +
              'INNER JOIN Taxon_List_Item AS TLI3 ON TLI3.Preferred_Name = TLI2.Preferred_Name ');
      SQL.Add('');
      for i := 0 to lSegmentKeys.Count do begin
        if i = 0 then
          SQL[2] := 'WHERE TLI.Taxon_List_Item_Key < ''' + lSegmentKeys[0] + ''''
        else if i = lSegmentKeys.Count then
          SQL[2] := 'WHERE TLI.Taxon_List_Item_Key >= ''' + lSegmentKeys[i - 1] + ''''
        else
          SQL[2] := 'WHERE TLI.Taxon_List_Item_Key >= ''' + lSegmentKeys[i - 1] + '''' +
                    ' AND TLI.Taxon_List_Item_Key < ''' + lSegmentKeys[i] + '''';
        ExecSQL;
        iSetProgress(2 + Trunc(i * lProgress));  // up to 100%
      end;
    end;
  finally
    iSetProgress(100);
    DefaultCursor(lCursor);
  end; // try
end;

//==============================================================================
end.
