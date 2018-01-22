{===============================================================================

  Copyright © Dorset Software Services Ltd, 1996

  Component:
    TDBHintGrid - Eric Salmon 31/03/2000

  Updates:

  Packages:
    DBInHouse5, Delphi 5 package for in house components.

  Description:
    DBGrid with the ability to show the whole record under the mouse pointer in
    a hint window. The call has to be made from the OnShowHint event handler
    of the application, using DoHint and the HintInfo.CursorPos and either True
    if you want all fields displayed on one single line, or False if you want
    to have one field per line.

  Additional information:

===============================================================================}

unit DBHintGrid;

interface

uses
  Windows, Classes, Controls, Forms, Grids, DBGrids, Sysutils, db;

type
  TDBHintGrid = class(TDBGrid)
  private
    FLastHintRow:integer;
  public
    function DoHint(const APoint:TPoint; const LinearHint:boolean;
                            const iShowHeadInfo : boolean):string;
  end;  // TDBHintGrid

//==============================================================================
implementation



//==============================================================================
function TDBHintGrid.DoHint(const APoint:TPoint;
                            const LinearHint:boolean;
                            const iShowHeadInfo : boolean):string;
var
  lGridCoord:TGridCoord;
  lOldActive:integer;
  lCol:integer;
  lstTempRes : String;
  lstLastString : string;
begin
  lGridCoord:=MouseCoord(APoint.X, APoint.Y);
  Result:='';
  FLastHintRow:=lGridCoord.Y;
  // Save current record position
  lOldActive:=DataLink.ActiveRecord;
  if DataLink.DataSet.IsEmpty = false then
  try
    // If there are some columns displayed, build the hint, otherwise do nothing
    if Columns.Count > 0 then
    begin
      if (lGridCoord.Y=0) and (dgTitles	in Options) and (iShowHeadInfo = true) then
      begin
        // Display column headers, or field names
        Result:=Columns[0].Field.DisplayName;
        if LinearHint then
          for lCol:=1 to Columns.Count-1 do
            Result := Result + ' - ' + Columns[lCol].Field.DisplayName
        else
          for lCol:=1 to Columns.Count-1 do
            // Check for a datetime field
            Result := Result + #13 + Columns[lCol].Field.DisplayName;
      end //if (lGridCoord.Y=0) and (dgTitles	in Options) and (iShowHeadInfo = true)
      else
      if (lGridCoord.Y=0) and (dgTitles	in Options) and (iShowHeadInfo = false) then
        result := ''
      else
      begin
        // Take column headers into account if they are present and change to
        // record under the mouse
        if dgTitles in Options then
          DataLink.ActiveRecord:=lGridCoord.Y-1
        else
          DataLink.ActiveRecord:=lGridCoord.Y;
        if Datalink.ActiveRecord >= 0 then
        begin
          if columns[0].Visible = true then
          begin
            if Columns[0].Field.DataType = ftDateTime then
              Result := DateToStr(trunc(Columns[0].Field.AsDateTime))
            else
              Result:=Columns[0].Field.DisplayText;
            lstLastString := Result;
          end;
          // if linear
          if LinearHint then
            for lCol := 1 to Columns.Count-1 do
              Result := Result + ' - ' + Columns[lCol].Field.DisplayText
          else
            // if non-linear
            for lCol := 1 to Columns.Count-1 do
              if columns[lCol].Visible = true then
              begin
                lstTempRes := Columns[lCol].Field.DisplayText;
                // there anything to report?
                if Trim(lstTempRes) <> '' then
                begin
                  // If it is a datetime, remove the time
                  if Columns[lCol].Field.DataType = ftDateTime then
                    lstTempRes := DateToStr(trunc(Columns[lCol].Field.AsDateTime));
                  if lstLastString = '' then
                    Result := Result +  lstTempRes
                  else
                    Result := Result + #13 + lstTempRes;
                  lstLastString := lstTempRes;
                end; // if Trim(lstTempRes) <> ''
              end; // if columns[lCol].Visible = true
        end; // if Datalink.ActiveRecord >= 0
      end;
    end; // if Columns.Count > 0
  finally
    // Return to current record
    DataLink.ActiveRecord := lOldActive;
  end;
end;  // DoHint

//==============================================================================
end.
