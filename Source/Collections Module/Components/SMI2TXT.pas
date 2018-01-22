{ SMImport suite
  TSMImportFromText component.

  Copyright (C) 2000-2003, written by Shkolnik Mike, Scalabium Software
  E-Mail:  mshkolnik@scalabium.com
           mshkolnik@yahoo.com
  WEB: http://www.scalabium.com
       http://www.geocities.com/mshkolnik
  tel: 380-/44/-552-10-29
}
unit SMI2TXT;

interface
{$I SMI.inc}

uses
  Classes, DB, SMIBase;

type
  TSMImportFromText = class(TSMImportBaseComponent)
  private
    { Private declarations }
    intLenText, intLenField: Integer;

    lstFileRows: TStrings;
    lst: TList;
    intCurRow: Integer;
    strFromFile: string;
    strCaptions: string;
  protected
    { Protected declarations }
    procedure BeforeImport; override;
    procedure AfterImport; override;
    function LoadRecord: Boolean; override;

    {data navigation}
    procedure GoFirst; override;
    procedure GoNext; override;
    function NeedMore: Boolean; override;
    function GetTotalCount: Integer; override;
    function GetFieldValue(v: Variant): Variant; override;

    function NotDefined(var strProperty: string): Boolean; override;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;

    class function Extension: string; override;
    class function FillFileFilters: string; override;

    procedure DetectAutoMapping; override;
    procedure Columns2Mapping; override;
  published
    { Published declarations }
    property SourceFileName;
    property Fixed;

    property RowFirst;
    property RowLast;
    property RowFieldNames;

    property FieldDelimiter;
    property FieldDelimiterCustom;

    property TextQualifier;
    property TextQualifierCustom;

    property RecordSeparator;
    property RecordSeparatorCustom;
  end;

procedure LoadTextFile(IsLimited, SkipEmpty: Boolean; var Captions: string; lst: TStrings; const FileName, RecordSeparator, TextQualifier: string; intFirstRow, intLastRow, intFieldNameRow: Integer);
procedure FillFields(DS: TDataset; IsFixed: Boolean; Separator, Delimiter: Char; const strLine: string; var lst: TList);

implementation
uses SysUtils, ImpCnst, SMIStat
     {$IFDEF SMForDelphi6} , Variants {$ENDIF};

procedure LoadTextFile(IsLimited, SkipEmpty: Boolean; var Captions: string; lst: TStrings; const FileName, RecordSeparator, TextQualifier: string; intFirstRow, intLastRow, intFieldNameRow: Integer);

  function IsRecordSeparator(ch: Char; IsBody: Boolean): Boolean;
  var
    i: Integer;
  begin
    Result := False;
    if not IsBody then
      for i := 1 to Length(RecordSeparator) do
        if ch = RecordSeparator[i] then //#10, #13
        begin
          Result := True;
          break
        end
  end;

  procedure SetTextStr(const Value: string; intLength: Integer);
  var
    P, Start: PChar;
    S: string;
    i, intProcessed, intCurLine, intLast, intLenRecordSeparator: Integer;
    IsBody: Boolean;
  begin
    intLast := intLastRow;
    if IsLimited and (intLast-intFirstRow > 20) then
      intLast := intFirstRow+20-1;

    P := Pointer(Value);
    intLenRecordSeparator := Length(RecordSeparator);
    intProcessed := 0;
    intCurLine := 1;
    if P <> nil then
//      while P^ <> #0 do
      while intProcessed < intLength do
      begin
        IsBody := (P^ = TextQualifier);
        Start := P;
//        while not (P^ = #0) and not IsRecordSeparator(P^) do
        while (intProcessed <> intLength) and not IsRecordSeparator(P^, IsBody) do
        begin
          Inc(P);
          Inc(intProcessed);
          if (P^ = TextQualifier) then
            IsBody := not IsBody;
        end;
        i := P - Start;
        if (i = 0) then
          S := ''
        else
          SetString(S, Start, i);
        if (not SkipEmpty) or (SkipEmpty and (Trim(S) <> '')) then
        begin
          if (intCurLine = intFieldNameRow{intFirstRow}) then
            Captions := S;
          if (intCurLine >= intFirstRow) and (intCurLine <= {intFirstRow + }intLast) then
            lst.Add(S);
        end;
        Inc(intCurLine);
        i := 1;
        while not IsBody and
              (i <= intLenRecordSeparator) and
              {(P^ = RecordSeparator[i])} IsRecordSeparator(P^, IsBody) do
        begin
          Inc(P);
          Inc(i);
          Inc(intProcessed);
          if (P^ = TextQualifier) then
            IsBody := not IsBody;
        end;
      end;
  end;

var
  intSize: Integer;
  S: string;
begin
  Captions := '';
  try
    lst.BeginUpdate;
    lst.Clear;
    with TFileStream.Create(FileName, fmOpenRead{ or fmShareDenyWrite}) do
      try
        intSize := Size - Position;

        {limit to first 100Kb for 99.99% of text files which must be loaded
        PS: attempt don't load large files for wizard preview - just load first block in 100Kb
        where will be placed most 20 records for preview
        but if FirstRow is large, data could be not read from file in this case}
        if IsLimited and (intSize > 102400) and (intFirstRow < 10) then
          intSize := 102400;

        SetString(S, nil, intSize);
        Read(Pointer(S)^, intSize);
        SetTextStr(S, intSize);
        while (intSize > 0) and (S[intSize] in [#26, #27]) do
        begin
          Dec(intSize);
          SetLength(S, intSize)
        end;
//        if (intSize > 0) and (S[intSize] = #27) then
//          SetLength(S, intSize-1)
      finally
        Free;
      end;
  finally
    lst.EndUpdate;
  end;
end;

procedure FillFields(DS: TDataset; IsFixed: Boolean; Separator, Delimiter: Char; const strLine: string; var lst: TList);

  function GetFieldLength(fld: TField): Integer;
  begin
    Result := 0;
    if Assigned(fld) then
      Result := fld.DisplayWidth;
  end;

var
  i, j, k, intCurPos, intPrevPos, intLength: Integer;
  IsFieldBody: Boolean;
  P: PChar;
begin
  lst.Clear;
  if (not Assigned(DS) and IsFixed) then exit;

  intLength := Length(strLine);
  intPrevPos := 1;
  if IsFixed then
  begin
    for i := 0 to DS.FieldCount-1 do
    begin
      intCurPos := intPrevPos + GetFieldLength(DS.Fields[i]);
      if intCurPos > intLength then
      begin
        intCurPos := intLength;
        k := intCurPos;
      end
      else
      begin
        if (Delimiter = #0) or
           (strLine[intCurPos] in [Delimiter, Separator]) then
          k := intCurPos-1
        else
          k := intCurPos;
      end;
      lst.Add(TObject(k));
      intPrevPos := intCurPos;
      if (Delimiter <> #0) then
        Inc(intPrevPos);
      if (intPrevPos <= intLength) and
         (strLine[intPrevPos] = Separator) then
        Inc(intPrevPos);
      if (intPrevPos >= intLength) then break;
    end;
  end
  else
  begin
    IsFieldBody := False;
    P := Pointer(strLine);
    i := 1;
    if P <> nil then
    begin
      while (i <= intLength) do
      begin
        if (Delimiter = #0) then
          IsFieldBody := False
        else
          if (Delimiter <> #0) and (P^ = Delimiter) then
            IsFieldBody := not IsFieldBody;
        if ((P^ = Separator) and (not IsFieldBody)) or
           (i = intLength) then
        begin
          if (i = intLength) {}and
             ((strLine[i] <> Separator) {} or (Separator = #0)) then
            intCurPos := i
          else
            intCurPos := i-1;

          if ((Delimiter <> #0) and (strLine[intPrevPos] = Delimiter)) or
             (strLine[intPrevPos] = Separator) then
            j := intPrevPos+1
          else
            j := intPrevPos;
          if ((Delimiter <> #0) and (strLine[intCurPos] = Delimiter)) then
            k := intCurPos-1
          else
            k := intCurPos;

          lst.Add(TObject(10000*j + k));
          intPrevPos := intCurPos+1;
          if (intPrevPos <= intLength) and
             (strLine[intPrevPos] = Separator) then
            Inc(intPrevPos);

        end;

        Inc(i);
        Inc(P);
      end;
    end;

    if (intLength > 0) and (strLine[intLength] = Separator) then
      lst.Add(TObject(10000*(intLength+1) + intLength+1));
  end;
end;

{ TSMImportFromText }
constructor TSMImportFromText.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  TableType := teText;
end;

class function TSMImportFromText.Extension: string;
begin
  Result := '.TXT';
end;

class function TSMImportFromText.FillFileFilters: string;
begin
  Result := strFormat3 + '|*.TXT|' +
            strFormat3A + '|*.CSV|' +
            strFormat3B + '|*.DAT|' +
            strFormat3C + '|*.PRN|' +
            strFormat3D + '|*.TAB|' +
            strFormat3E + '|*.ASC|' +
            inherited FillFileFilters
end;

function TSMImportFromText.NotDefined(var strProperty: string): Boolean;
begin
  Result := inherited NotDefined(strProperty);

  if not Result and (SourceFileName = '') then
  begin
    Result := True;
    strProperty := 'SourceFileName'
  end;
end;

procedure TSMImportFromText.Columns2Mapping;
var
  i, j: Integer;
begin
  for i := 0 to Columns.Count-1 do
  begin
    for j := 0 to Dataset.FieldCount-1 do
    begin
      if (CompareText(Columns[i].FieldName, Dataset.Fields[j].FieldName) = 0) or
         (UseDisplayNames and (CompareText(Columns[i].FieldName, Dataset.Fields[j].DisplayLabel) = 0)) then
      begin
        Mappings.Add(Dataset.Fields[j].FieldName + '=' + Columns[i].Caption);
        break;
      end;
    end;
  end;
end;

procedure TSMImportFromText.DetectAutoMapping;
var
  j, intStart, intEnd: Integer;
  s: string;
  lstPositions: TList;
begin
  if RowFirst > 1 then
  begin
    lstPositions := TList.Create;
    FillFields(DataSet, Fixed, FieldDelimiterCustom, TextQualifierCustom, strCaptions, lstPositions);
    intStart := 0;

    for j := 0 to lstPositions.Count-1 do
    begin
      intEnd := LongInt(lstPositions[j]);
      if Fixed then
      begin
        s := Copy(strCaptions, intStart, intEnd-intStart+1);
        intStart := intEnd
      end
      else
      begin
        intStart := (intEnd div 10000);
        intEnd := (intEnd mod 10000);

        s := Copy(strCaptions, intStart, intEnd-intStart+1);
      end;

      with Columns.Add do
      begin
        FieldName := s;
        DataType := itString;

        if Fixed then
        begin
          Caption := strTextFieldName + IntToStr(intStart) + '-' + IntToStr(intEnd);
          Size := intEnd-intStart+1
        end
        else
          Caption := strField + IntToStr(j+1);
      end;
    end;
    lstPositions.Free;
  end
  else
  begin
    inherited;

    intStart := 1;
    for j := 0 to Columns.Count-1 do
      if Fixed then
      begin
        intEnd := intStart + Columns[j].Size;
        Columns[j].FieldName := strTextFieldName + IntToStr(intStart) + '-' + IntToStr(intEnd);
        intStart := intEnd+1
      end
      else
        Columns[j].FieldName := strField + IntToStr(j+1)
  end
end;

procedure TSMImportFromText.BeforeImport;
begin
  {calculate length of Text/Field text
  PS: could be changed in resource file}
  intLenText := Length(strTextFieldName);
  intLenField := Length(strField);

  {load a text file}
  lstFileRows := TStringList.Create;
  if not Fixed then
    lst := TList.Create;

  LoadTextFile(False, (soSkipEmptyRow in Options), strCaptions, lstFileRows, SourceFileName, RecordSeparatorCustom, TextQualifierCustom, RowFirst, RowLast, RowFieldNames);
end;

procedure TSMImportFromText.AfterImport;
begin
  if not Fixed then
    lst.Free;
  lstFileRows.Free
end;

procedure TSMImportFromText.GoFirst;
begin
  intCurRow := 0;
end;

procedure TSMImportFromText.GoNext;
begin
  Inc(intCurRow);
end;

function TSMImportFromText.NeedMore: Boolean;
begin
  Result := (intCurRow < lstFileRows.Count) and (intCurRow <= RowLast - RowFirst);
end;

function TSMImportFromText.GetTotalCount: Integer;
begin
  Result := lstFileRows.Count
end;

function TSMImportFromText.LoadRecord: Boolean;
begin
  strFromFile := lstFileRows[intCurRow];

  {we must define the KeyFields for Locate method
   and array with start/end of text mapping}
  if not Fixed then
    FillFields(DataSet, False, FieldDelimiterCustom, TextQualifierCustom, strFromFile, lst);

  Result := inherited LoadRecord
end;

function TSMImportFromText.GetFieldValue(v: Variant): Variant;
var
  s: string;
  i, intStart, intEnd: Integer;
begin
  s := VarToStr(inherited GetFieldValue(v));
  if (s = strTextFieldName) then
  begin
    Result := strFromFile; {return current row from text file}
    exit;
  end;

  intStart := 0;
  intEnd := 0;
  if Fixed and
     (Copy(s, 1, intLenText) = strTextFieldName) then
  begin
    {search a TextXX-YY}
    i := Pos('-', s);
    intStart := StrToIntDef(Copy(s, intLenText+1, i-intLenText-1), 0);
    intEnd := StrToIntDef(Copy(s, i+1, Length(s)-i), 0);
  end
  else
  if (Copy(s, 1, intLenField) = strField) then
  begin
    {search a FieldXX}
    i := StrToIntDef(Copy(s, intLenField+1, Length(s)-intLenField+1), -1);
    if (i > 0) and (i <= lst.Count) then
    begin
      i := LongInt(lst[i-1]);
      intStart := i div 10000;
      intEnd := (i mod 10000);
    end
    else
      if (Copy(s, 1, intLenField) = strField) then
        s := ''
  end;
  if (intStart > 0) then
  begin
    if (intEnd > 0) then
      Result := Copy(strFromFile, intStart, intEnd-intStart+1)
    else
      Result := ''
  end
  else
    Result := s;
  i := Length(Result);
  if (i > 0) then
  begin
    if (Copy(Result, i, 1) = FieldDelimiterCustom) then
      Result := Copy(Result, 1, i-1)
  end;
//    if Result = FieldDelimiterCustom then
//      Result := '';
end;
                
end.

