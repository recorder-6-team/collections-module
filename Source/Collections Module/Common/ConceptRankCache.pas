{===============================================================================
  Unit:           ConceptRankCache

  Defines:        TConceptRankCache

  Description:    A class that maintains a cache of concept rank details

  Model:          ThesaurusNavigator.mpb

  Created:        August 2003

  Last revision information:
    $Revision: 2 $
    $Date: 26/01/04 8:58 $
    $Author: Johnvanbreda $

===============================================================================}
unit ConceptRankCache;

interface

uses
  SysUtils, Windows, Messages, Classes, Graphics, Controls, Forms, Dialogs,
  InterfaceDataModule, GeneralData, Variants, ADODb;

type
  TConceptRankCache = class (TObject)
  private
    FRankList: TStringList;
    function AddRankToCache(const ARankKey: string): TRank;
    procedure DoAdditionToCache(const ARankKey: string; ARank: TRank);
  public
    constructor Create;
    destructor Destroy; override;
    function Exists(const ARankKey: string): Boolean;
    function GetRank(const ARankKey: string): TRank;
  end;
  

implementation

{-==============================================================================
    TConceptRankCache
===============================================================================}
{-------------------------------------------------------------------------------
  Default initialisation 
}
constructor TConceptRankCache.Create;
begin
  inherited Create;
  
  FRankList := TStringList.Create;
end;  // TConceptRankCache.Create 

{-------------------------------------------------------------------------------
  Cleanup 
}
destructor TConceptRankCache.Destroy;
var
  lIdx: Integer;
begin
  // and ranks
  for lIdx := 0 to FRankList.Count-1 do
    TObject(FRankList.Objects[lIdx]).Free;
  FRankList.Free;
  
  inherited Destroy;
end;  // TConceptRankCache.Destroy 

{-------------------------------------------------------------------------------
  Loads a rank's detail from the database and stores it in the internal cache.  
      Returns nil if rank not found 
}
function TConceptRankCache.AddRankToCache(const ARankKey: string): TRank;
begin
  with dmGeneral.GetRecordset('usp_ConceptRank_Select', [
    '@ConceptRankKey', ARankKey]) do
  if not EOF then begin
    Result := TRank.Create;
    Result.Abbreviation := Fields['Abbreviation'].Value;
    Result.Colour := Fields['Color_R'].Value shl 16
      + Fields['Color_G'].Value shl 8
      + Fields['Color_B'].Value;
    Result.RankKey := ARankKey;
    if not VarIsNull(Fields['Sort_Order'].Value) then
      Result.SortOrder := Fields['Sort_Order'].Value
    else
      Result.SortOrder := -1; // rank order not specified, so position at top.
    DoAdditionToCache(ARankKey, Result);
  end
  else
    Result := nil;
end;  // TConceptRankCache.AddRankToCache 

{-------------------------------------------------------------------------------
  Inserts the rank into the cache in the correct place, according to the sort 
      order. 
}
procedure TConceptRankCache.DoAdditionToCache(const ARankKey: string; ARank: 
    TRank);
var
  lIdx: Integer;
  lInserted: Boolean;
begin
  // insert into cache, in correct position according to sort order.
  lInserted := False;
  for lIdx := 0 to FRankList.Count-1 do
    if TRank(FRankList.Objects[lIdx]).SortOrder<ARank.SortOrder then begin
      FRankList.InsertObject(lIdx, ARankKey, ARank);
      lInserted := True;
      Break; // from loop
    end;
  // Insert at end of list
  if not lInserted then
    FRankList.AddObject(ARankKey, ARank);
end;  // TConceptRankCache.DoAdditionToCache 

{-------------------------------------------------------------------------------
  Returns true if the rank is already loaded in the cache. 
}
function TConceptRankCache.Exists(const ARankKey: string): Boolean;
begin
  Result := FRankList.IndexOf(ARankKey)>-1;
end;  // TConceptRankCache.Exists 

{-------------------------------------------------------------------------------
  Returns a rank for the supplied key.  Loads it into the cache if required. 
}
function TConceptRankCache.GetRank(const ARankKey: string): TRank;
var
  lIdx: Integer;
begin
  lIdx := FRankList.IndexOf(ARankKey);
  if lIdx>-1 then
    Result := TRank(FRankList.Objects[lIdx])
  else
    Result := AddRankToCache(ARankKey);
end;  // TConceptRankCache.GetRank 


end.
