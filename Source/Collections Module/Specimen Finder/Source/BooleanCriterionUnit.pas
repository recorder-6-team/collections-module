{===============================================================================
  Unit:        BooleanCriterionUnit

  Defines:     TBooleanCriterion

  Description: Defines the search criteria which are created to filter results
                in specimen finder.

===============================================================================}

unit BooleanCriterionUnit;

interface
uses
  Classes, Contnrs, ICriterionUnit, Forms, Dialogs, TypInfo, BooleanProperty,
  SysUtils, VagueDate;

type

  TBooleanCriterion = class(TInterfacedObject, ICriterion)
  private
    FPropertyType : TBooleanProperty;
    FOperator : TLogicalOperator;
    FOperand1 : string;
    FOperand2 : string;
    FHasProperty : Boolean;
    FJoinTables: TStrings;
    procedure InitialiseJoinTables;
    function GetWhereCondition : string;
    function GetWhereConditionForVagueDate : string;
    function GetHasMultimediaClause(const tableName: string): string;
    function GetOperand1: string;
  public
    property PropertyType : TBooleanProperty read FPropertyType;
    property Operator : TLogicalOperator read FOperator write FOperator;
    property Operand1 : string read GetOperand1;
    property Operand2 : string read FOperand2 write FOperand2;
    property HasProperty : Boolean read FHasProperty write FHasProperty;
    constructor Create(
                  propertyType: TBooleanProperty;
                  operator: TLogicalOperator;
                  operand1: string;
                  operand2: string;
                  hasProperty: Boolean);
    destructor Destroy; override;
    function GetJoinTables : TStrings;
    function GetJoinLogic(Table: TJoinTable): string;
    function GetSqlWhereExpression : string;
  end;


implementation

uses DateUtils, StrUtils;

const
  FORMATTED_NAME =  'CASE WHEN %0:s.Forename IS NULL THEN ' +
	                  ' 		CASE WHEN %0:s.Initials IS NULL THEN ' +
	                  '				CASE WHEN %0:s.Title IS NULL THEN %0:s.Surname ' +
	                  '				ELSE %0:s.Title + '' '' + %0:s.Surname END ' +
	                  '			ELSE %0:s.Initials + '' '' + %0:s.Surname END ' +
	                  '		ELSE %0:s.Forename + '' '' + %0:s.Surname END ';

{-----------------------------------------------------------------------------
}
constructor TBooleanCriterion.Create(
                propertyType: TBooleanProperty;
                operator: TLogicalOperator;
                operand1: string;
                operand2: string;
                hasProperty: Boolean);
begin
  inherited Create;
  FPropertyType := propertyType;
  FOperator := operator;
  FOperand1 := operand1;
  FOperand2 := operand2;
  FHasProperty := hasProperty;
  FJoinTables := TStringList.Create;
  InitialiseJoinTables;
end;

{-------------------------------------------------------------------------------
}
destructor TBooleanCriterion.Destroy;
begin
  FJoinTables.Free;
  inherited Destroy;
end;

{-------------------------------------------------------------------------------
}
procedure TBooleanCriterion.InitialiseJoinTables;
begin
  case PropertyType.PropertyEnum of
    bpeCollectionName:
      FJoinTables.Add(IntToStr(Ord(jtCollectionByParentKey)));
    bpeValue:
      FJoinTables.Add(IntToStr(Ord(jtCollectionUnitData)));
    bpeAppliesTo:
      FJoinTables.Add(IntToStr(Ord(jtCollectionUnitData)));
    bpeCollectionHasMultimedia:
    begin
      FJoinTables.Add(IntToStr(Ord(jtCollectionByParentKey)));
      FJoinTables.Add(IntToStr(Ord(jtSourceFileCollection)));
    end;
    bpeSpecimenHasMultimedia:
      FJoinTables.Add(IntToStr(Ord(jtSourceFileSpecimenUnit)));
    bpeCollectionTopic:
      FJoinTables.Add(IntToStr(Ord(jtCollectionByParentKey)));
    bpePreferredNumber:
      FJoinTables.Add(IntToStr(Ord(jtCollectionUnitNumberPreferred)));
    bpePreferredAccessionNumber:
    begin
      FJoinTables.Add(IntToStr(Ord(jtMovementCollectionUnit)));
      FJoinTables.Add(IntToStr(Ord(jtMovementDirection)));
      FJoinTables.Add(IntToStr(Ord(jtMovement)));
    end;
    bpeAccessionNumber:
    begin
      FJoinTables.Add(IntToStr(Ord(jtCollectionByParentKey)));
      FJoinTables.Add(IntToStr(Ord(jtMovementCollectionUnitByCollection)));
      FJoinTables.Add(IntToStr(Ord(jtMovementDirectionByCollection)));
      FJoinTables.Add(IntToStr(Ord(jtMovementByCollection)));
    end;
    bpeSpecimenGatheringDate:
    begin
      FJoinTables.Add(IntToStr(Ord(jtSpecimenFieldData)));
      FJoinTables.Add(IntToStr(Ord(jtOccurrence)));
      FJoinTables.Add(IntToStr(Ord(jtTaxonOccurrence)));
      FJoinTables.Add(IntToStr(Ord(jtSample)));
    end;
    bpeAnyNumber:
      FJoinTables.Add(IntToStr(Ord(jtCollectionUnitNumber)));
    bpeSpecimenGatheringLocation:
    begin
      FJoinTables.Add(IntToStr(Ord(jtSpecimenFieldData)));
      FJoinTables.Add(IntToStr(Ord(jtOccurrence)));
      FJoinTables.Add(IntToStr(Ord(jtTaxonOccurrence)));
      FJoinTables.Add(IntToStr(Ord(jtSample)));
      FJoinTables.Add(IntToStr(Ord(jtLocationName)));
    end;
    bpeParameter:
    begin
      FJoinTables.Add(IntToStr(Ord(jtCollectionUnitData)));
      FJoinTables.Add(IntToStr(Ord(jtConceptByParameter)));
      FJoinTables.Add(IntToStr(Ord(jtSynonymByParameter)));
      FJoinTables.Add(IntToStr(Ord(jtSearchTermByParameter)));
    end;
    bpeSpecimenType:
    begin
      FJoinTables.Add(IntToStr(Ord(jtConceptByType)));
      FJoinTables.Add(IntToStr(Ord(jtSynonymByType)));
      FJoinTables.Add(IntToStr(Ord(jtSearchTermByType)));
    end;
    bpeObservationLocation:
    begin
      FJoinTables.Add(IntToStr(Ord(jtSpecimenFieldData)));
      FJoinTables.Add(IntToStr(Ord(jtOccurrence)));
      FJoinTables.Add(IntToStr(Ord(jtTaxonOccurrence)));
      FJoinTables.Add(IntToStr(Ord(jtSample)));
      FJoinTables.Add(IntToStr(Ord(jtPlainMetadata)));
    end;
    bpeSpecimenMetadata:
    begin
      FJoinTables.Values[IntToStr(Ord(jtMetadata))]
        := FPropertyType.MetadataName;
      FJoinTables.Values[IntToStr(Ord(jtMetadataType))]
        := FPropertyType.MetadataName;
    end;
    bpeCollectionMetadata:
    begin
      FJoinTables.Add(IntToStr(Ord(jtCollectionByParentKey)));
      FJoinTables.Values[IntToStr(Ord(jtMetadataByCollection))]
        := FPropertyType.MetadataName;
      FJoinTables.Values[IntToStr(Ord(jtMetadataTypeByCollection))]
        := FPropertyType.MetadataName;
    end;
    bpePreferredDetermination:
    begin
      FJoinTables.Add(IntToStr(Ord(jtDeterminationPreferred)));
      FJoinTables.Add(IntToStr(Ord(jtConceptByDeterminationPreferred)));
      FJoinTables.Add(IntToStr(Ord(jtSynonymByDeterminationPreferred)));
      FJoinTables.Add(IntToStr(Ord(jtSearchTermByDeterminationPreferred)));
      FJoinTables.Add(IntToStr(Ord(jtTaxonDeterminationPreferredKey)));
      FJoinTables.Add(IntToStr(Ord(jtIndexTaxonSynonymPreferred)));
      FJoinTables.Add(IntToStr(Ord(jtTaxonPreferred)));
    end;
    bpeAnyDetermination:
    begin
      FJoinTables.Add(IntToStr(Ord(jtDetermination)));
      FJoinTables.Add(IntToStr(Ord(jtConceptByDetermination)));
      FJoinTables.Add(IntToStr(Ord(jtSynonymByDetermination)));
      FJoinTables.Add(IntToStr(Ord(jtSearchTermByDetermination)));
      FJoinTables.Add(IntToStr(Ord(jtTaxonDetermination)));
      FJoinTables.Add(IntToStr(Ord(jtIndexTaxonSynonym)));
      FJoinTables.Add(IntToStr(Ord(jtTaxon)));
    end;
    bpeNomenclatureStatus:
    begin
      FJoinTables.Add(IntToStr(Ord(jtDetermination)));
      FJoinTables.Add(IntToStr(Ord(jtConceptNomenclatureStatus)));
    end;
    bpeHasSpatialReference:
    begin
      FJoinTables.Add(IntToStr(Ord(jtHasSpatialReference)));
    end;
    bpeAssembledBy:
    begin
      FJoinTables.Add(IntToStr(Ord(jtCollectionByParentKey)));
      FJoinTables.Add(IntToStr(Ord(jtIndividual)));
      FJoinTables.Add(IntToStr(Ord(jtOrganisation)));
    end;
    bpeDeterminationInGroup:
    begin
      FJoinTables.Add(IntToStr(Ord(jtDetermination)));
      FJoinTables.Add(IntToStr(Ord(jtConceptByDetermination)));
      FJoinTables.Add(IntToStr(Ord(jtHierarchicalSynonym)));
      FJoinTables.Add(IntToStr(Ord(jtTaxonDetermination)));
      FJoinTables.Add(IntToStr(Ord(jtIndexTaxonSynonym)));
      FJoinTables.Add(IntToStr(Ord(jtIndexTaxonInGroup)));
      FJoinTables.Add(IntToStr(Ord(jtIndexTaxonSynonymInGroup)));
      FJoinTables.Add(IntToStr(Ord(jtTaxonInGroup)));
    end;
    bpeDeterminer:
    begin
      FJoinTables.Add(IntToStr(Ord(jtDetermination)));
      FJoinTables.Add(IntToStr(Ord(jtTaxonDetermination)));
      FJoinTables.Add(IntToStr(Ord(jtIndividualByDetermination)));
      FJoinTables.Add(IntToStr(Ord(jtIndividualByTaxonDetermination)));
    end;
    bpeGeoArea:
    begin
      FJoinTables.Add(IntToStr(Ord(jtSpecimenFieldData)));
      FJoinTables.Add(IntToStr(Ord(jtOccurrence)));
      FJoinTables.Add(IntToStr(Ord(jtTaxonOccurrence)));
      FJoinTables.Add(IntToStr(Ord(jtSample)));
      FJoinTables.Add(IntToStr(Ord(jtSurveyEventGeoArea)));
      FJoinTables.Add(IntToStr(Ord(jtSurveyEventGeoAreaSynonym)));
    end;
  end;
end;

{-------------------------------------------------------------------------------
  Returns a set of join tables depending on the type of filtering property
}
function TBooleanCriterion.GetJoinTables : TStrings;
begin
  result := FJoinTables;
end;

{-------------------------------------------------------------------------------
  Returns any additional logic that is required in the join clause for the
  specified table.
}
function TBooleanCriterion.GetJoinLogic(Table: TJoinTable): string;
begin
  case Table of
    jtHierarchicalSynonym:
    begin
      case PropertyType.PropertyEnum of
        bpeDeterminationInGroup:
          Result := Format(
              GetWhereCondition,
              ['ISNULL(HierSyn%0:s.Plaintext, '''')']) +
              ' AND SpecUnit.Life_Sciences = 0';
      else
        Result := '';
      end;
    end;
    jtSurveyEventGeoAreaSynonym:
    begin
      case PropertyType.PropertyEnum of
        bpeGeoArea:
        begin
          Result := Format(
              GetWhereCondition,
              ['ISNULL(GeoSyn%0:s.Published_Term, '''')'])
        end;
      else
        Result := '';
      end;
    end;
  else
    Result := '';
  end;
end;

{-----------------------------------------------------------------------------
  Returns the filtering where clause for a particular criterion.
}
function TBooleanCriterion.GetSqlWhereExpression : string;

  function BooleanPropertyExpression(const BooleanProperty: string): string;
  begin
    Result := Format(
        '%0:s %1:s %2:s',
        [BooleanProperty,
         IfThen(Operator = loEqualTo, '=', '<>'),
         IfThen(HasProperty, '1', '0')]);
  end;

begin
  case PropertyType.PropertyEnum of
    bpeCollectionName:
      Result := Format(GetWhereCondition, ['ISNULL(Coll.Item_Name, '''')']);
    bpeValue:
      Result := Format(GetWhereCondition, ['ISNULL(CollUnitData.Lower_Value, '''')']);
    bpeAppliesTo:
      Result := Format(GetWhereCondition, ['ISNULL(CollUnitData.Applies_To, '''')']);
    bpeCollectionHasMultimedia:
      Result := GetHasMultimediaClause('Collection');
    bpeSpecimenHasMultimedia:
      Result := GetHasMultimediaClause('Specimen_Unit');
    bpeCollectionTopic:
      Result := Format(GetWhereCondition, ['ISNULL(Coll.Topic, '''')']);
    bpePreferredNumber:
      Result := Format(GetWhereCondition, ['ISNULL(CollUnitNumPref.Number, '''')']);
    bpePreferredAccessionNumber:
      Result := Format(GetWhereCondition, ['ISNULL(Mov.Number, '''')']) +
                  ' AND (((Mov.Movement_Type = 0 OR Mov.Movement_Type = 1)' +
                  ' AND MovDir.Outbound = 0) OR MovDir.Outbound IS NULL)';
    bpeAccessionNumber:
      Result := Format(GetWhereCondition, ['ISNULL(MovColl.Number, '''')']) +
                  ' AND (((MovColl.Movement_Type = 0 OR MovColl.Movement_Type = 1) AND MovDirColl.Outbound = 0) OR MovDirColl.Outbound IS NULL)';
    bpeSpecimenGatheringDate:
      Result := Format(GetWhereConditionForVagueDate,
                  ['Samp.VAGUE_DATE_START', 'Samp.VAGUE_DATE_END', 'Samp.VAGUE_DATE_TYPE']) +
                  ' AND SpecFieldData.Gathering_Event = 1 ';
    bpeAnyNumber:
      Result := Format(GetWhereCondition, ['ISNULL(CollUnitNum.Number, '''')']);
    bpeSpecimenGatheringLocation:
      Result := '((' + Format(GetWhereCondition, ['ISNULL(LocNam.Item_Name, '''')'])
                  + ') OR (' +
                  Format(GetWhereCondition, ['ISNULL(Samp.Location_Name, '''')'])
                  + ')) AND SpecFieldData.Gathering_Event = 1 ';
    bpeParameter:
      Result := Format(GetWhereCondition, ['ISNULL(SearchTermByPar.Plaintext, '''')']);
    bpeSpecimenType:
      Result := Format(GetWhereCondition, ['ISNULL(SearchTermByType.Plaintext, '''')']);
    bpeObservationLocation:
      Result := '(' + Format(GetWhereCondition,
                      ['CAST(ISNULL(Met.Text, '''') as VARCHAR(8000))']) +
                ' AND ISNULL(Met.Metadata_Type_Key, '''') = ''SYSTEM0000000008'')' +
                ' OR (' + Format(GetWhereCondition,
                                  ['ISNULL(Samp.Location_Name, '''')']) + ')';
    bpeSpecimenMetadata:
      Result := '(Met' + StringReplace(FPropertyType.MetadataName, ' ', '', [rfReplaceAll]) + '.Text IS NOT NULL AND ' +
                   Format(GetWhereCondition,
                    ['CAST(ISNULL(Met' + StringReplace(FPropertyType.MetadataName, ' ', '', [rfReplaceAll])
                       + '.Text, '''') as VARCHAR(8000))']) + ' AND MetType' +
                 StringReplace(FPropertyType.MetadataName, ' ', '', [rfReplaceAll]) + '.Table_Name IS NOT NULL AND ' +
                 'MetType' + StringReplace(FPropertyType.MetadataName, ' ', '', [rfReplaceAll]) + '.Item_Name ' +
                 'IS NOT NULL) OR (Met' + StringReplace(FPropertyType.MetadataName, ' ', '', [rfReplaceAll]) +
                 '.Text IS NULL AND ' +
                 Format(GetWhereCondition, ['''''']) + ')';
    bpeCollectionMetadata:
      Result := '(MetColl' + StringReplace(FPropertyType.MetadataName, ' ', '', [rfReplaceAll]) + '.Text IS NOT NULL AND ' +
                   Format(GetWhereCondition,
                    ['CAST(ISNULL(MetColl' + StringReplace(FPropertyType.MetadataName, ' ', '', [rfReplaceAll])
                       + '.Text, '''') as VARCHAR(8000))']) + ' AND MetTypeColl' +
                 StringReplace(FPropertyType.MetadataName, ' ', '', [rfReplaceAll]) + '.Table_Name IS NOT NULL AND ' +
                 'MetTypeColl' + StringReplace(FPropertyType.MetadataName, ' ', '', [rfReplaceAll]) + '.Item_Name ' +
                 'IS NOT NULL) OR (MetColl' + StringReplace(FPropertyType.MetadataName, ' ', '', [rfReplaceAll]) +
                 '.Text IS NULL AND ' +
                 Format(GetWhereCondition, ['''''']) + ')';
    bpePreferredDetermination:
      Result := '(' + Format(GetWhereCondition, ['ISNULL(SearchTermByDetPref.Plaintext, '''')']) +
                ' AND SpecUnit.Life_Sciences = 0) OR (' +
                 Format(GetWhereCondition, ['ISNULL(TaxPref.Item_Name, '''')']) + ' AND SpecUnit.Life_Sciences = 1) ';
    bpeAnyDetermination:
      Result := '(' + Format(GetWhereCondition, ['ISNULL(SearchTermByDet.Plaintext, '''')']) +
                ' AND SpecUnit.Life_Sciences = 0) OR (' +
                 Format(GetWhereCondition, ['ISNULL(Tax.Item_Name, '''')']) + ' AND SpecUnit.Life_Sciences = 1) ';
    bpeNomenclatureStatus:
      Result := Format(GetWhereCondition, ['ISNULL(ConNom.Plaintext, '''')']);
    bpeHasSpatialReference:
    begin
      Result := 'SpecHasSpatialRef.Spatial_Ref_Count = 0';
      if (((Operator = loEqualTo) and HasProperty) or
      ((Operator = loNotEqualTo) and (not HasProperty))) then
        Result := 'NOT (' + Result + ')';
    end;
    bpeAssembledBy:
      Result := Format(GetWhereCondition, ['Org.Full_Name']) +' OR ' +
        Format(GetWhereCondition,
        ['dbo.FormatIndividual(Ind.Title, Ind.Initials, Ind.Forename, Ind.Surname)']);
    bpeDeterminationInGroup:
      Result := '(' + Format(GetWhereCondition,
                   ['ISNULL(TaxInGro.Item_Name, '''')']) +
                ' AND SpecUnit.Life_Sciences = 1) OR  (' +
                'HierSyn%0:s.Concept_Key IS NOT NULL)';
    bpeDeterminer:
      Result := '((' + Format(
                        GetWhereCondition,
                        [Format(FORMATTED_NAME, ['IndDet'])]) +
                ' and Det.Determination_Key IS NOT NULL) OR (' +
                Format(
                  GetWhereCondition,
                  [Format(FORMATTED_NAME, ['IndTaxDet'])]) +
                ' and TaxDet.Taxon_Determination_Key IS NOT NULL))';
    bpeGeoArea:
      Result := 'GeoSyn%0:s.Concept_Key IS NOT NULL';
    bpeSpecimenIsChecked:
      Result := BooleanPropertyExpression('SpecUnit.Checked');
    bpeSpecimenIsConfidential:
      Result := BooleanPropertyExpression('SpecUnit.Confidential');
    bpeSpecimenIsDangerous:
      Result := BooleanPropertyExpression('SpecUnit.Dangerous');
    bpeSpecimenIsForInternalUse:
      Result := BooleanPropertyExpression('SpecUnit.Internal_Use');
    bpeSpecimenIsForWebUse:
      Result := BooleanPropertyExpression('SpecUnit.Publish_To_Web');
  end;
end;

{-----------------------------------------------------------------------------
  Produces the part of the where clause which compares the values of the results
  to those entered by the user.
}
function TBooleanCriterion.GetWhereCondition : string;
var
  value1 : string;
  value2 : string;

  { Escapes the given value for use in a SQL string literal within a Delphi
    format string }
  function StringEscape(const Value: string): string;
  begin
    Result := AnsiReplaceStr(AnsiReplaceStr(Value, '''', ''''''), '%', '%%');
  end;

  { Escapes the given value for use in a SQL LIKE pattern }
  function LikeEscape(const Value: string): string;
  begin
    Result :=
        AnsiReplaceStr(
            AnsiReplaceStr(
                AnsiReplaceStr(Value, '[', '[[]'), '%', '[%]'), '_', '[_]');
  end;
begin
  if PropertyType.PropertyType = ptVagueDate then
    Result := GetWhereConditionForVagueDate
  else
  begin
    value2 := '';
    case PropertyType.PropertyType of
    ptBoolean:
      begin
        if HasProperty then value1 := '1'
        else value1 := '0';
      end;
      ptString, ptMeasurementValue:
      begin
        value1 := '''' + StringEscape(Operand1) + '''';
        value2 := '''' + StringEscape(Operand2) + '''';
      end;
    end;

    case Operator of
    loEqualTo:
      Result := '%s = ' + value1;
    loNotEqualTo:
      Result := '%s <> ' + value1;
    loGreaterThan:
      Result := '%s > ' + value1;
    loLessThan:
      Result := '%s < ' + value1;
    loBetween:
      Result := '%0:s BETWEEN ' + value1 + ' AND ' + value2;
    loStartsWith:
      Result := '%s LIKE ''' + StringEscape(LikeEscape(Operand1)) + '%%''';
    loContains:
      Result := '%s LIKE ''%%' + StringEscape(LikeEscape(Operand1)) + '%%''';
    end;
  end;
end;



{-----------------------------------------------------------------------------
  Specific WHERE clause generation for filtering criterion of vague date type
}
function TBooleanCriterion.GetWhereConditionForVagueDate : string;
var
  vagueDate1 : TVagueDate;
  vagueDate2 : TVagueDate;
begin
   vagueDate1 := StringToVagueDate(Operand1);
   case Operator of
     loBetween:
     begin
       vagueDate2 := StringToVagueDate(Operand2);
       Result := '%s >= ' + IntToStr(Trunc(vagueDate1.StartDate)) +
                  ' AND %s <= ' + IntToStr(Trunc(vagueDate2.EndDate));
     end;
     loEqualTo:
     begin
       Result := '%s = ' + IntToStr(Trunc(vagueDate1.StartDate)) +
                  ' AND %s = ' + IntToStr(Trunc(vagueDate1.EndDate)) +
                  ' AND %s = ''' + vagueDate1.DateTypeString + '''';
     end;
     loNotEqualTo:
     begin
       Result := '%s < ' + IntToStr(Trunc(vagueDate1.StartDate)) +
                 ' OR %s > ' + IntToStr(Trunc(vagueDate1.EndDate)) +
                 ' OR %s <> ''' + vagueDate1.DateTypeString + '''';
     end;
     loGreaterThan:
       Result := '%1:s > ' + IntToStr(Trunc(vagueDate1.EndDate));
     loLessThan:
       Result := '%s < ' + IntToStr(Trunc(vagueDate1.StartDate));
  end;
end;

function TBooleanCriterion.GetHasMultimediaClause(const tableName: string): string;
begin
   Result := '((SFS.File_Name IS NULL) OR (SFS.File_Name IS NOT NULL ' +
              'AND SJS.Table_Name IS NOT NULL AND SJS.Table_Name <> ''' +
              tableName + '''))';
   if tableName = 'Collection' then
   begin
     Result := StringReplace(Result, 'SFS', 'SFC', [rfReplaceAll]);
     Result := StringReplace(Result, 'SJS', 'SJC', [rfReplaceAll]);
   end;
   if (((Operator = loEqualTo) and HasProperty) or
      ((Operator = loNotEqualTo) and (not HasProperty))) then
      Result := 'NOT (' + Result + ')';
end;

{-------------------------------------------------------------------------------
}
function TBooleanCriterion.GetOperand1: string;
begin
  if Assigned(Self) then
    result := FOperand1
  else
    result := '';
end;

end.

