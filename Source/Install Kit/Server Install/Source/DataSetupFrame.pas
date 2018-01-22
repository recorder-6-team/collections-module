{===============================================================================
  Unit:        DataSetupFrame

  Defines:     TfraDataSetup

  Description: Setup the data in the database according to selected options in
               the wizard.

  Created:     September 2003

  Last revision information:
    $Revision: 7 $
    $Date: 14/05/07 16:42 $
    $Author: Johnvanbreda $

===============================================================================}

unit DataSetupFrame;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, BaseStepFrame, StdCtrls, ComCtrls;

resourcestring
  ResStr_SiteID='Site ID Required';
  ResStr_InputSiteID='Please input the Site ID for this database:';
  ResStr_SiteIDAlphaNumeric='The Site ID must consist of numbers and letters only. It is '+
      'provided with your Recorder license.';
  ResStr_SiteID8chars='The Site ID must be 8 characters long. It is '+
      'provided with your Recorder license.';

type
  TfraDataSetup = class(TfraBaseStep)
    Label3: TLabel;
    Label1: TLabel;
    pbSetup: TProgressBar;
  private
    function RequestInputSiteID: string;
  protected
    function GetNext: TStepFrameClass; override;
  public
    function Execute: Boolean; override;
  end;

//==============================================================================
implementation

{$R *.dfm}

uses
  ADODB, GeneralData, Settings, BaseADODataModule, CompleteFrame, GeneralFunctions;

{-==============================================================================
    TfraDataSetup
===============================================================================}
{-------------------------------------------------------------------------------
}
function TfraDataSetup.Execute: Boolean;
var lQuery: TADOCommand;
    i: Integer;
    lNameKey: String;
    lIniFile: TStringList;
    lSiteID: string;

  function RunCheck(const ACommand: String): Boolean;
  begin
    lQuery.CommandText := ACommand;
    Result := not lQuery.Execute.Eof;
  end;

  procedure RunCommand(const ACommand: String);
  begin
    lQuery.CommandText := ACommand;
    lQuery.Execute;
  end;

begin
  pbSetup.Max := Settings.Languages.Count + 5;
  lQuery := TADOCommand.Create(nil);
  try
    lQuery.Connection := dmGeneral.Connection;
    lQuery.CommandType := cmdText;
    // In case this install is run before the first run of Rec6, SiteID won't be ready!
    lQuery.CommandText := 'SELECT Data FROM Setting WHERE [Name] = ''SiteID''';
    with lQuery.Execute do begin
      if VarIsNull(Fields['Data'].Value) then begin
        lIniFile := TStringList.Create;
        try
          if FileExists(Settings.InstallationPath + 'InstallSettings.ini') then begin
            lIniFile.LoadFromFile(Settings.InstallationPath + 'InstallSettings.ini');
            lSiteId := lIniFile.Values['SiteID'];
          end
          else
            lSiteID := RequestInputSiteID;
          RunCommand('UPDATE Setting SET Data = ''' + lSiteID +
                     ''' WHERE [Name] = ''SiteID''');
        finally
          lIniFile.Free;
        end;
      end;
      Close;
    end;

    // Generate a new key first.
    lNameKey := dmGeneral.GetStoredProcOutputParam('spNextKey',
                                                   ['@TableName', 'Name',
                                                    '@Key', ''], '@Key');
    pbSetup.Position := pbSetup.Position + 1;

    // Insert or update name and organisation
    if RunCheck(Format('SELECT * FROM [Name] WHERE Name_Key = ''%s''', [lNameKey])) then
      RunCommand(Format('UPDATE [Name] SET Organisation = 1, Entered_By = ''NBNSYS0000000004''' +
                        'WHERE Name_Key = ''%s''', [lNameKey]))
    else
      RunCommand(Format('INSERT INTO [Name](Name_Key, Organisation, Entered_By) ' +
                        'VALUES(''%s'', 1, ''NBNSYS0000000004'')', [lNameKey]));
    pbSetup.Position := pbSetup.Position + 1;

    if RunCheck(Format('SELECT * FROM Organisation WHERE Name_Key = ''%s''', [lNameKey])) then
      RunCommand(Format('UPDATE Organisation SET Full_Name = ''%s'', Acronym = ''%s'', ' +
                        'Entered_By = ''NBNSYS0000000004'' WHERE Name_Key = ''%s'')',
                        [QuotedStr(Settings.OrgFullName), QuotedStr(Settings.OrgAcronym), lNameKey]))
    else
      RunCommand(Format('INSERT INTO Organisation(Name_Key, Full_Name, Acronym, Entered_By) ' +
                        'VALUES(''%s'', %s, %s, ''NBNSYS0000000004'')',
                        [lNameKey, QuotedStr(Settings.OrgFullName), QuotedStr(Settings.OrgAcronym)]));
    pbSetup.Position := pbSetup.Position + 1;
    if RunCheck('SELECT * FROM Setting WHERE [Name] = ''HoldingOrg''') then
      RunCommand(Format('UPDATE Setting SET [Data] = ''%s'' WHERE [Name] = ''HoldingOrg''',
                        [lNameKey]))
    else
      RunCommand(Format('INSERT INTO Setting([Name], [Data]) VALUES (''HoldingOrg'', ''%s'')',
                        [lNameKey]));

    // Barcode
    if RunCheck('SELECT * FROM Setting WHERE [Name] = ''Barcode''') then
      RunCommand(Format('UPDATE Setting SET [Data] = ''%d'' WHERE [Name] = ''Barcode''',
                        [Settings.BarCodeFormat]))
    else
      RunCommand(Format('INSERT INTO Setting([Name], [Data]) VALUES (''Barcode'', ''%d'')',
                        [Settings.BarCodeFormat]));

    // ShortDateFormat
    if RunCheck('SELECT * FROM Setting WHERE [Name] = ''ShortDates''') then
      RunCommand(Format('UPDATE Setting SET [Data] = ''%s'' WHERE [Name] = ''ShortDates''',
                        [ShortDateFormat]))
    else
      RunCommand(Format('INSERT INTO Setting([Name], [Data]) VALUES (''ShortDates'', ''%s'')',
                        [ShortDateFormat]));
    pbSetup.Position := pbSetup.Position + 1;

    // Languages
    for i := 0 to Settings.Languages.Count - 1 do begin
      RunCommand(Format('UPDATE Language SET Priority = %d WHERE Item_Name = ''%s''',
                        [i + 1, Settings.Languages[i]]));
      pbSetup.Position := pbSetup.Position + 1;
    end;

    // Conservation Job seed
    if Settings.JobSeed <> '' then
      RunCommand('DBCC CHECKIDENT(''Conservation_Job'', RESEED, ' + Settings.JobSeed + ')');
    pbSetup.Position := pbSetup.Position + 1;
    Sleep(500);
  finally
    lQuery.Free;
  end;
  Result := True;
  ChangedContent;
end;  // TfraDataSetup.Execute

{-------------------------------------------------------------------------------
}
function TfraDataSetup.GetNext: TStepFrameClass;
begin
  Result := TfraComplete;
end;  // TfraDataSetup.GetNext

{-------------------------------------------------------------------------------
  Site ID not available in DB yet, so request user input
}
function TfraDataSetup.RequestInputSiteID: string;
    function IsValid(const ASiteID: string): boolean;
    var
      i: integer;
    begin
      Result := true;
      if Length(ASiteID)<>8 then begin
        ShowInformation(ResStr_SiteID8Chars);
        Result := False;
      end
      else
        for i := 1 to 8 do
          if not (Uppercase(ASiteID)[i] in ['A'..'Z', '0'..'9']) then begin
            ShowInformation(ResStr_SiteIDAlphaNumeric);
            Result := False;
          end;
    end;

begin
  Result := '';
  repeat
    Result := InputBox(ResStr_SiteID, ResStr_InputSiteID, '');
  until IsValid(Result);
  Result := Uppercase(Result);
end;

end.
