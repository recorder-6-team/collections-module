{===============================================================================
  Unit:        PreviousResultsCriterion

  Defines:     TPreviousResultsCriterion

  Description: Defines the search criteria which take the results from an
               existing specimen search. 

===============================================================================}

unit PreviousResultsCriterion;

interface

uses
  Classes, Contnrs, ICriterionUnit, SysUtils, ResourceStrings, LuxembourgConstants;

type
  TPreviousResultsCriterion = class(TInterfacedObject, ICriterion)
  private
    FKeys: string;
  public
    constructor Create(Keys: string);
    property Keys : string read FKeys write FKeys;
    function GetJoinTables: TStrings;
    function GetJoinLogic(Table: TJoinTable): string;
    function GetSqlWhereExpression: string;
end;

implementation

constructor TPreviousResultsCriterion.Create(Keys: string);
begin
  FKeys := Keys;
end;

function TPreviousResultsCriterion.GetJoinTables: TStrings;
begin
  Result := nil;
end;

function TPreviousResultsCriterion.GetJoinLogic(Table: TJoinTable): string;
begin
  Result := '';
end;

function TPreviousResultsCriterion.GetSqlWhereExpression: string;
begin
  Result := 'SpecUnit.Collection_Unit_Key IN (' + FKeys + ')';
end;

end.
