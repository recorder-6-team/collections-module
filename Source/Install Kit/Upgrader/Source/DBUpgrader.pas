{===============================================================================
  Unit:           DBUpgrader

  Defines:

  Description:

  Created:

  Last revision information:
    $Revision: 5 $
    $Date: 1/11/05 9:25 $
    $Author: Johnvanbreda $

===============================================================================}
unit DBUpgrader;

interface

uses
  SysUtils, Windows, Messages, Classes, Forms, ComCtrls, ADODb, Settings, ExceptionForm;

type
  EDBUpgraderError = class (TExceptionPath)
  end;
  
  TDBUpgrader = class (TObject)
  private
    FConnection: TADOConnection;
    FQuery: TADOQuery;
    FRunList: TStringList;
    FSettings: TSettings;
    procedure Connect;
    function GetCurrentScriptNumber: string;
    procedure PrepareRunList;
    procedure RunQuery;
    procedure RunScript(const AScriptFile: String);
  public
    constructor Create(ASettings: TSettings);
    destructor Destroy; override;
    procedure RunScripts(AProgressBar: TProgressBar);
    procedure UpdateDBSeq(const ALastScript: String);
  end;
  
//==============================================================================
implementation

uses
  GeneralFunctions;

resourcestring
  SDBUpgrader_ScriptError = 'Error occurred during processing of script ';

{-==============================================================================
    TDBUpgrader
===============================================================================}
{-------------------------------------------------------------------------------
  Initialises member fields.
  Pass True in AConnectionString to use integrated security, or false if the sa username and 
      password are provided. 
}
constructor TDBUpgrader.Create(ASettings: TSettings);
begin
  inherited Create;
  
  FRunList := TStringList.Create;
  FSettings := ASettings;
  FConnection := TADOConnection.Create(nil);
  FQuery := TADOQuery.Create(nil);
  FQuery.Connection := FConnection;
  FQuery.ParamCheck := False;
  FQuery.CommandTimeout := 0;
end;  // TDBUpgrader.Create 

{-------------------------------------------------------------------------------
  Destroys any member fields. 
}
destructor TDBUpgrader.Destroy;
begin
  FRunList.Free;
  FQuery.Free;
  FConnection.Free;
  
  inherited Destroy;
end;  // TDBUpgrader.Destroy 

{-------------------------------------------------------------------------------
  Sets the connection login to the current settings and connects to the database.
  Raises an exception if the login fails. 
}
procedure TDBUpgrader.Connect;
begin
  if not FConnection.Connected then begin
    FConnection.ConnectionString := FSettings.GetConnectionString;
    // connect or raise an exception
    FConnection.Connected := True;
  end;
end;  // TDBUpgrader.Connect 

{-------------------------------------------------------------------------------
  Returns the number of the last run script on the database.  This is an NBN 8 character key.
}
function TDBUpgrader.GetCurrentScriptNumber: string;
begin
  with FQuery do
    try
      SQL.Text := 'SELECT Data FROM Setting WHERE Name=''CM DB Seq''';
      Open;
      if RecordCount = 0 then
        Result := ''
      else
        Result := FieldByName('Data').AsString;
    finally
      Close;
    end;
end;  // TDBUpgrader.GetCurrentScriptNumber

{-------------------------------------------------------------------------------
  Prepares the list of scripts to run (FRunList).  This is all scripts whose keys are higher 
      than the value currently recorded against the database Settings table. 
}
procedure TDBUpgrader.PrepareRunList;
var
  lCurrentScript: string;
  lSearchRec: TSearchRec;
begin
  FRunList.Clear;
  lCurrentScript := GetCurrentScriptNumber;
  if FindFirst(ExtractFilePath(Application.Exename) + 'Scripts\*.sql',
               faAnyFile, lSearchRec) = 0 then
    repeat
      if ExtractWithoutExt(lSearchRec.Name) > lCurrentScript then
        FRunList.Add(ExtractWithoutExt(lSearchRec.Name));
    until FindNext(lSearchRec) <> 0;
end;  // TDBUpgrader.PrepareRunList 

{-------------------------------------------------------------------------------
  Runs the query already loaded into FQuery.
  Clears the SQL ready for the next query.
  If the query starts with a comment --try, then any errors are ignored. 
}
procedure TDBUpgrader.RunQuery;
begin
  try
    FQuery.ExecSQL;
  except
    on E:Exception do
      // batches with --try on first line are allowed to fail in our
      if CompareText(FQuery.SQL[0], '--try') <> 0 then begin
        FQuery.SQL.SaveToFile(IncludeTrailingPathDelimiter(GetWindowsTempDir) +
                              'Failed Query.sql');
        raise;
      end;
  end; // try
  FQuery.SQL.Clear;
end;  // TDBUpgrader.RunQuery 

{-------------------------------------------------------------------------------
  Loads the SQL file identified by AScriptFile (without extension) from the \Scripts folder 
      and executes it. 
}
procedure TDBUpgrader.RunScript(const AScriptFile: String);
var
  lScript: TStringList;
  lLineIdx: Integer;
begin
  lScript := TStringList.Create;
  try
    lScript.LoadFromFile(ExtractFilePath(Application.ExeName) + 'Scripts\' +
                                         AScriptFile + '.sql');
    FQuery.SQL.Clear;
    for lLineIdx := 0 to lScript.Count - 1 do
      if CompareText(Trim(lScript[lLineIdx]), 'GO') = 0 then begin
        RunQuery;
      end else
        FQuery.SQL.Add(lScript[lLineIdx]);
    // if there was a query at the end with no GO, also run it
    if FQuery.SQL.Count > 0 then
      RunQuery;
  finally
    lScript.Free;
  end;
end;  // TDBUpgrader.RunScript 

{-------------------------------------------------------------------------------
  Run each of the scripts identified in FRunList in turn. 
}
procedure TDBUpgrader.RunScripts(AProgressBar: TProgressBar);
var
  i: Integer;
begin
  Connect;
  FQuery.SQL.Text := 'SET ARITHABORT ON';
  FQuery.ExecSQL;
  PrepareRunList;
  if FRunList.Count > 0 then begin
    for i := 0 to FRunList.Count - 1 do begin
      try
        // each script should default to ansi_nulls, as this allows indexed views
        FQuery.SQL.Text := 'SET ANSI_NULLS ON';
        FQuery.ExecSQL;
        RunScript(FRunList[i]);
      except
        on E:Exception do begin
          E.Message := 'Error in script ' + FRunList[i] + '.'#13#10 + E.Message;
          raise;
        end;
      end;
      AProgressBar.Position := (i + 1) * 100 div FRunList.Count;
    end;
    UpdateDBSeq(FRunList[FRunList.Count - 1]);
  end;
end;  // TDBUpgrader.RunScripts

{-------------------------------------------------------------------------------
  Updates the DB Seq setting in the Settings table to reflect the last script run.
}
procedure TDBUpgrader.UpdateDBSeq(const ALastScript: String);
const
  SQL_SEQ_UPDATE =
                 'IF EXISTS (SELECT 1 FROM SETTING WHERE [Name]=''CM DB Seq'') '+
                 '  UPDATE Setting SET Data=''%s'' WHERE Name=''CM DB Seq'' ' +
                 'ELSE '+
                 '  INSERT INTO Setting VALUES (''CM DB Seq'', ''%s'')';
begin
  FQuery.SQL.Text := Format(SQL_SEQ_UPDATE, [ALastScript, ALastScript]);
  FQuery.ExecSQL;
end;  // TDBUpgrader.UpdateDBSeq 

end.
