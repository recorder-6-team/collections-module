{===============================================================================
  Unit:        DragAndDropCriterion

  Defines:     TFinderType
               TDragAndDropCriterion

  Description: Defines the search criteria which can be dragged from another
                panel into the specimen finder.

===============================================================================}

unit DragAndDropCriterion;

interface

uses
  Classes, Contnrs, ICriterionUnit, SysUtils, ResourceStrings, LuxembourgConstants;

type
  TFinderType = (ftCollection, ftConcept, ftLocation, ftName, ftSurvey,
      ftTaxonListItem, ftStore, ftMovement);

  TDragAndDropCriterion = class(TInterfacedObject, ICriterion)
  private
    FSearchType: TFinderType;
    FTableName : string;
    FSearchValue: string;
    FJoinTables: TStrings;
    procedure InitialiseJoinTables;
  public
    property SearchType : TFinderType read FSearchType;
    property SearchValue : string read FSearchValue write FSearchValue;
    property TableName : string read FTableName write FTableName;
    constructor Create(SearchTable: string; SearchValue: string);
    destructor Destroy; override;
    function FinderType(const tableName: String): TFinderType;
    function GetJoinTables: TStrings;
    function GetSqlWhereExpression : string;
    function GetJoinLogic(Table: TJoinTable): string;
  end;

  EInvalidFinderType = class(Exception)
end;

implementation

{-------------------------------------------------------------------------------
}
constructor TDragAndDropCriterion.Create(SearchTable: string; SearchValue: string);
begin
  inherited Create;
  FTableName := SearchTable;
  FSearchType := FinderType(SearchTable);
  FSearchValue := SearchValue;
  FJoinTables := TStringList.Create;
  InitialiseJoinTables;
end;

{-------------------------------------------------------------------------------
}
destructor TDragAndDropCriterion.Destroy;
begin
  FJoinTables.Free;
  inherited Destroy;
end;

{-------------------------------------------------------------------------------
  Returns the sql WHERE clause for each criterion.
}
function TDragAndDropCriterion.GetSqlWhereExpression: string;
begin
  case FSearchType of
    ftCollection:
      Result := 'SpecUnit.Parent_Collection_Collection_Unit_Key IN (''' + FSearchValue + ''')';
    ftTaxonListItem:
      Result := 'IndTaxSynGro.Taxon_List_Item_Key IN (''' + FSearchValue + ''')';
    ftName:
      Result := 'Coll.Assembler_Name_Key IN (''' + FSearchValue
                + ''') OR Sur.Run_By IN (''' + FSearchValue
                + ''') OR SurEvRec.Name_Key IN (''' + FSearchValue
                + ''') OR TaxDet.Determiner IN (''' + FSearchValue
                + ''') OR Det.Determiner_Name_Key IN (''' + FSearchValue
                + ''')';
    ftLocation:
      Result := 'Samp.Location_Key IN (''' + FSearchValue + ''') AND SpecFieldData.Gathering_Event = 1';
    ftSurvey:
      Result := 'Sur.Survey_Key IN (''' + FSearchValue + ''')';
    ftStore:
      Result := 'CollUnit.Current_Container_Collection_Unit_Key IN (''' + FSearchValue + ''')';
    ftMovement:
      Result := 'Mov.Movement_Key IN (''' + FSearchValue + ''')';
    ftConcept:
      Result := 'HierSyn%0:s.Concept_Key IS NOT NULL OR ' +
                'GeoSyn%0:s.Concept_Key IS NOT NULL';
  end;
end;

{-------------------------------------------------------------------------------
}
procedure TDragAndDropCriterion.InitialiseJoinTables;
begin
  case FSearchType of
    //ftCollection:
    //begin
    //  tableSet := [];
    //end;
    ftTaxonListItem:
    begin
      FJoinTables.Add(IntToStr(Ord(jtTaxonDetermination)));
      FJoinTables.Add(IntToStr(Ord(jtIndexTaxonSynonym)));
      FJoinTables.Add(IntToStr(Ord(jtIndexTaxonInGroup)));
      FJoinTables.Add(IntToStr(Ord(jtIndexTaxonSynonymInGroup)));
    end;
    ftName:
    begin
      FJoinTables.Add(IntToStr(Ord(jtCollectionByParentKey)));
      FJoinTables.Add(IntToStr(Ord(jtSpecimenFieldData)));
      FJoinTables.Add(IntToStr(Ord(jtOccurrence)));
      FJoinTables.Add(IntToStr(Ord(jtTaxonOccurrence)));
      FJoinTables.Add(IntToStr(Ord(jtSample)));
      FJoinTables.Add(IntToStr(Ord(jtSampleRecorder)));
      FJoinTables.Add(IntToStr(Ord(jtSurveyEvent)));
      FJoinTables.Add(IntToStr(Ord(jtSurvey)));
      FJoinTables.Add(IntToStr(Ord(jtDetermination)));
      FJoinTables.Add(IntToStr(Ord(jtTaxonDetermination)));
    end;
    ftLocation:
    begin
      FJoinTables.Add(IntToStr(Ord(jtSpecimenFieldData)));
      FJoinTables.Add(IntToStr(Ord(jtOccurrence)));
      FJoinTables.Add(IntToStr(Ord(jtTaxonOccurrence)));
      FJoinTables.Add(IntToStr(Ord(jtSample)));
    end;
    ftSurvey:
    begin
      FJoinTables.Add(IntToStr(Ord(jtSpecimenFieldData)));
      FJoinTables.Add(IntToStr(Ord(jtOccurrence)));
      FJoinTables.Add(IntToStr(Ord(jtTaxonOccurrence)));
      FJoinTables.Add(IntToStr(Ord(jtSample)));
      FJoinTables.Add(IntToStr(Ord(jtSurveyEvent)));
      FJoinTables.Add(IntToStr(Ord(jtSurvey)));
    end;
    ftStore:
    begin
      FJoinTables.Add(IntToStr(Ord(jtCollectionUnit)));
    end;
    ftMovement:
    begin
      FJoinTables.Add(IntToStr(Ord(jtCollectionUnit)));
      FJoinTables.Add(IntToStr(Ord(jtMovementCollectionUnit)));
      FJoinTables.Add(IntToStr(Ord(jtMovementDirection)));
      FJoinTables.Add(IntToStr(Ord(jtMovement)));
      FJoinTables.Add(IntToStr(Ord(jtCollectionUnitNumberPreferred)));
      FJoinTables.Add(IntToStr(Ord(jtDeterminationPreferred)));
      FJoinTables.Add(IntToStr(Ord(jtConceptTermPreferred)));
      FJoinTables.Add(IntToStr(Ord(jtTaxonDeterminationPreferredKey)));
      FJoinTables.Add(IntToStr(Ord(jtIndexTaxonName)));
    end;
    ftConcept:
    begin
      FJoinTables.Add(IntToStr(Ord(jtSpecimenFieldData)));
      FJoinTables.Add(IntToStr(Ord(jtOccurrence)));
      FJoinTables.Add(IntToStr(Ord(jtTaxonOccurrence)));
      FJoinTables.Add(IntToStr(Ord(jtSample)));
      FJoinTables.Add(IntToStr(Ord(jtSurveyEventGeoArea)));
      FJoinTables.Add(IntToStr(Ord(jtDetermination)));
      FJoinTables.Add(IntToStr(Ord(jtConceptByDetermination)));
      FJoinTables.Add(IntToStr(Ord(jtHierarchicalSynonym)));
      FJoinTables.Add(IntToStr(Ord(jtSurveyEventGeoAreaSynonym)));
    end;
  end;
end;

{-------------------------------------------------------------------------------
  Returns the set of join tables required by each criterion.
}
function TDragAndDropCriterion.GetJoinTables: TStrings;
begin
  result := FJoinTables;
end;

{-------------------------------------------------------------------------------
  Returns any additional logic that is required in the join clause for the
  specified table.
}
function TDragAndDropCriterion.GetJoinLogic(Table: TJoinTable): string;
begin
  case Table of
    jtHierarchicalSynonym:
    begin
      case FSearchType of
        ftConcept:
        begin
          Result := 'HierSyn%0:s.Concept_Key IN (''' + FSearchValue + ''')';
        end;
      else
        Result := '';
      end
    end;
    jtSurveyEventGeoAreaSynonym:
    begin
      case FSearchType of
        ftConcept:
        begin
          Result := 'GeoSyn%0:s.Parent_Concept_Key IN (''' + FSearchValue + ''')';
        end;
      else
        Result := '';
      end;
    end;
  else
    Result := '';
  end;
end;

{-------------------------------------------------------------------------------
  Returns the finder type given a table name.
}
function TDragAndDropCriterion.FinderType(const tableName: String): TFinderType;
begin
  if SameText(tableName, TN_COLLECTION)      then Result := ftCollection    else
  if SameText(tableName, TN_CONCEPT)         then Result := ftConcept       else
  if SameText(tableName, TN_TAXON_LIST_ITEM) then Result := ftTaxonListItem else
  if SameText(tableName, TN_LOCATION)        then Result := ftLocation      else
  if SameText(tableName, TN_NAME)            then Result := ftName          else
  if SameText(tableName, TN_SURVEY)          then Result := ftSurvey        else
  if SameText(tableName, TN_STORE)           then Result := ftStore         else
  if SameText(tableName, TN_MOVEMENT)        then Result := ftMovement
  else
    raise EInvalidFinderType.Create(ResStr_FinderTypeNotFoundFor + tableName);
end;  // TDragAndDropCriterion.ReturnFinderType

end.
