{ ------------------------------------------------------------------------------
// Unit:          StrIntList
//
//                Copyright © Dorset Software Services Ltd, 1998
//
// Description:   String-keyed list of integers
//
// Author:        AJWK
//
// Created:       1999-10-06
//
// Changes:
//
// To Do:         
// ----------------------------------------------------------------------------}
unit strintlist;

interface

uses
  KeyList;

type
  TStringKeyedIntegerList = class(TCustomKeyedList)
  protected
    FCaseSensitive: Boolean;
    
    function CompareIndirectedKeys(pKey1, pKey2: Pointer): Integer; override;
    procedure FreeIndirectedPair(pKey, pValue: Pointer); override;

    function GetKey(i: Integer): String;
    function GetValue(i: Integer): Integer;
    function GetFoundValue: Integer;

  public
    constructor Create(casesensitive: Boolean);

    procedure Add(const key: String; value: Integer);

    function Find(const key: String): Boolean;

    property CaseSensitive: Boolean read FCaseSensitive;
    property Key[i: Integer]: String read GetKey;
    property Value[i: Integer]: Integer read GetValue;
    property FoundValue: Integer read GetFoundValue;

  end;  {class TStringKeyedIntegerList}




implementation

uses
  SysUtils;


{ ------------------------------------------------------------------------------
// TStringKeyedIntegerList.Create
// ----------------------------------------------------------------------------}
constructor TStringKeyedIntegerList.Create(casesensitive: Boolean);
begin
  inherited Create;
  FCaseSensitive := casesensitive;
end;  {TStringKeyedIntegerList.Create}


{ ------------------------------------------------------------------------------
// TStringKeyedIntegerList.Add
// ----------------------------------------------------------------------------}
procedure TStringKeyedIntegerList.Add(const key: String; value: Integer);
var
  pKey:    ^String;
  pValue:  ^Integer;

begin
  New(pKey);
  try
    pKey^ := key;
    New(pValue);
    try
      pValue^ := value;
      AppendIndirectedPair(pKey, pValue);
    except
      Dispose(pValue);
      raise;
    end;  {try .. except}
  except
    Dispose(pKey);
    raise;
  end;  {try .. except}
end;  {TStringKeyedIntegerList.Add}


{ ------------------------------------------------------------------------------
// TStringKeyedIntegerList.Find
// ----------------------------------------------------------------------------}
function TStringKeyedIntegerList.Find(const key: String): Boolean;
begin
  Result := FindIndirectedKey(@key);
end;  {TStringKeyedIntegerList.Find}


{ ------------------------------------------------------------------------------
// TStringKeyedIntegerList.CompareIndirectedKeys
// ----------------------------------------------------------------------------}
function TStringKeyedIntegerList.CompareIndirectedKeys(
    pKey1, pKey2: Pointer): Integer;
var
  pStr1, pStr2:  ^String;

begin
  pStr1 := pKey1;
  pStr2 := pKey2;
  if CaseSensitive then
    Result := CompareStr(pStr1^, pStr2^)
  else
    Result := CompareText(pStr1^, pStr2^);
end;  {TStringKeyedIntegerList.CompareIndirectedKeys}


{ ------------------------------------------------------------------------------
// TStringKeyedIntegerList.FreeIndirectedPair
// ----------------------------------------------------------------------------}
procedure TStringKeyedIntegerList.FreeIndirectedPair(pKey, pValue: Pointer);
var
  pStr:  ^String;
  pInt:  ^Integer;

begin
  pStr := pKey;
  Dispose(pStr);
  pInt := pValue;
  Dispose(pInt);
end;  {TStringKeyedIntegerList.FreeIndirectedPair}


{ ------------------------------------------------------------------------------
// TStringKeyedIntegerList.GetKey
// ----------------------------------------------------------------------------}
function TStringKeyedIntegerList.GetKey(i: Integer): String;
var
  pKey:  ^String;

begin
  pKey := IndirectedKey[i];
  Result := pKey^;
end;  {TStringKeyedIntegerList.GetKey}


{ ------------------------------------------------------------------------------
// TStringKeyedIntegerList.GetValue
// ----------------------------------------------------------------------------}
function TStringKeyedIntegerList.GetValue(i: Integer): Integer;
var
  pValue:  ^Integer;

begin
  pValue := IndirectedValue[i];
  Result := pValue^;
end;  {TStringKeyedIntegerList.GetValue}


{ ------------------------------------------------------------------------------
// TStringKeyedIntegerList.GetFoundValue
// ----------------------------------------------------------------------------}
function TStringKeyedIntegerList.GetFoundValue: Integer;
var
  pValue:  ^Integer;

begin
  pValue := IndirectedFoundValue;
  Result := pValue^;
end;  {TStringKeyedIntegerList.GetFoundValue}


end.
