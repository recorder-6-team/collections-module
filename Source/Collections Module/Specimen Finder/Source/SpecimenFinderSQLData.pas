{===============================================================================
  Unit:        SpecimenFinderSQLData

  Defines:     TFinderSQLGenerator
               TFinderSQLItem

  Description:

  Model:       SpecimenFinder.mpb
  
===============================================================================}

unit SpecimenFinderSQLData;

interface

uses
  Classes, Contnrs, ICriterionUnit, DragAndDropCriterion, BooleanCriterionUnit;

type
  {-----------------------------------------------------------------------------
    Class that generates the SQL required for the Specimen Finder output.
  }
  TFinderSQLGenerator = class(TObject)
  private
    //FItemList: TStringList;
    function ApplyAppSettings(const ASql: string): string;
    function GetSqlWhereClause(criteria: array of ICriterion;
                                 includeAll: Boolean) : string;
    function GetLeftJoin(const tableOrdinal: string;
                         const tableMetadataOrdinal: string;
                         const tableKey: string;
                         const criteria: array of ICriterion;
                         includeAll: Boolean): string;
    function GetJoinLogicForTable(
                table: TJoinTable;
                const criteria: array of ICriterion;
                includeAll: Boolean): string;
    function GetSqlJoinClause(
                const criteria: array of ICriterion;
                includeAll: Boolean): string;
  public
    constructor Create;
    destructor Destroy; override;
    function GenerateSql(
            criteria: array of ICriterion;
            IncludeAll: Boolean;
            out PreSql: string) : string;
  end;

//==============================================================================
implementation

uses
  SysUtils, StrUtils, ApplicationSettings, LuxembourgConstants;

const
   SPECIMEN_SQL =
      ' SELECT DISTINCT'#13#10 +
      '    SpecUnit.Collection_Unit_Key AS Item_Key,'#13#10 +
      '    CASE'#13#10 +
      '      WHEN C.Concept_Key IS NOT NULL THEN C.Concept_Key'#13#10 +
      '      WHEN ITN.Taxon_List_Item_Key IS NOT NULL THEN ITN.Taxon_List_Item_Key'#13#10 +
      '    END AS Det_Item_Key,'#13#10 +
      '    CASE'#13#10 +
      '      WHEN SpecUnit.Life_Sciences = 0 THEN CP.Published_Term'#13#10 +
      '      ELSE'#13#10 +
      '      CASE ITN.Actual_Name_Italic'#13#10 +
      '		      WHEN 0 THEN'#13#10 +
      '			    CASE'#13#10 +
      '           WHEN ITN.Authority IS NULL THEN'#13#10 +
      '				      ITN.Actual_Name'#13#10 +
      '			      ELSE'#13#10 +
      '				      ITN.Actual_Name + '' '' + ITN.Authority'#13#10 +
      '			      END'#13#10 +
      '		      ELSE'#13#10 +
      '			    CASE'#13#10 +
      '           WHEN ITN.Authority IS NULL THEN'#13#10 +
      '				      ''<i>'' + ITN.Actual_Name + ''</i>'''#13#10 +
      '			      ELSE'#13#10 +
      '				      ''<i>'' + ITN.Actual_Name + ''</i> '' + ITN.Authority'#13#10 +
      '			    END'#13#10 +
      '		   END'#13#10 +
      '    END AS Item_Name,'#13#10 +
      '    SpecUnit.Life_Sciences,'#13#10 +
      '    CUN.Number,'#13#10 +
      '    CU.Current_Location_Code,'#13#10 +
      '    SpecUnit.Specimen_Type_Concept_Key,'#13#10 +
      '    STCP.Published_Term AS Specimen_Type,'#13#10 +
      '    C.List_Code,'#13#10 +
      '    DM.Item_Name as Domain_Name'#13#10 +
      ' FROM Specimen_Unit SpecUnit'#13#10 +
      ' INNER JOIN Collection_Unit CU ON SpecUnit.Collection_Unit_Key = CU.Collection_Unit_Key'#13#10 +
      '    AND ((CU.Domain_Mask & @UserDomainMask > 0) OR (CU.Domain_Mask = 0) OR (CU.Entered_Session_ID = @SessionID) OR (CU.Changed_Session_ID = @SessionID))'#13#10 +
      ' LEFT JOIN Collection_Unit_Number CUN ON SpecUnit.Collection_Unit_key = CUN.Collection_Unit_Key'#13#10 +
      '    AND CUN.Preferred = 1'#13#10 +
      ' LEFT JOIN Determination D ON SpecUnit.Preferred_Determination_Key = D.Determination_Key'#13#10 +
      ' LEFT JOIN Concept C ON D.Concept_Key = C.Concept_Key'#13#10 +
      ' LEFT JOIN Concept CP ON CP.Meaning_Key = C.Meaning_Key AND CP.Concept_Group_Key = C.Concept_Group_Key AND CP.List_Preferred = 1'#13#10 +
      ' LEFT JOIN Taxon_Determination TD ON SpecUnit.Preferred_Taxon_Determination_Key = TD.Taxon_Determination_Key'#13#10 +
      ' LEFT JOIN Index_Taxon_Name ITN ON TD.Taxon_List_Item_Key = ITN.Taxon_List_Item_Key'#13#10 +
      ' INNER JOIN %s ON %s.Collection_Unit_Key=SpecUnit.Collection_Unit_Key '#13#10 +
      ' LEFT JOIN Concept CT ON CT.Meaning_Key = C.Meaning_Key AND CT.Term_Key = C.Term_Key'#13#10 +
      ' AND CT.Preferred = 1'#13#10 +
      ' LEFT JOIN Concept_Group CG ON C.Concept_Group_Key = CG.Concept_Group_Key'#13#10 +
      ' LEFT JOIN Local_Domain LD ON CG.Local_Domain_Key = LD.Local_Domain_Key'#13#10 +
      ' LEFT JOIN Domain DM ON LD.Domain_Key = DM.Domain_Key'#13#10 +
      ' INNER JOIN Concept STC ON SpecUnit.Specimen_Type_Concept_Key = STC.Concept_Key'#13#10 +
      ' LEFT JOIN Concept STCP ON STCP.Meaning_Key = STC.Meaning_Key'#13#10 +
      ' AND STCP.Concept_Group_Key = STC.Concept_Group_Key AND STCP.List_Preferred = 1'#13#10;

   FILTER_TABLE_SQL =
      'if object_id(''tempdb..#Filter'') is not null drop table #Filter'#13#10 +
      'CREATE TABLE #Filter (' +
      '	RecID INT IDENTITY(1,1) PRIMARY KEY,'#13#10 +
      '	Collection_Unit_Key CHAR(16) COLLATE database_default NOT NULL,'#13#10 +
      '	Specimen_Type_Concept_Key CHAR(16) COLLATE database_default NOT NULL,'#13#10 +
      '	Life_Sciences BIT NOT NULL,'#13#10 +
      '	Preferred_Determination_Key CHAR(16) COLLATE database_default NULL,'#13#10 +
      '	Preferred_Taxon_Determination_Key CHAR(16) COLLATE database_default NULL)'#13#10 +
      ' INSERT INTO #Filter(Collection_Unit_Key, Life_Sciences, Specimen_Type_Concept_Key, Preferred_Determination_Key, Preferred_Taxon_Determination_Key)'#13#10 +
      ' SELECT'#13#10 +
      '	SpecUnit.Collection_Unit_Key,'#13#10 +
      '	SpecUnit.Life_Sciences,'#13#10 +
      '	SpecUnit.Specimen_Type_Concept_Key,'#13#10 +
      '	SpecUnit.Preferred_Determination_Key,'#13#10 +
      '	SpecUnit.Preferred_Taxon_Determination_Key'#13#10 +
      'FROM Specimen_Unit SpecUnit'#13#10;

{-==============================================================================
    TFinderSQLGenerator
===============================================================================}
{-------------------------------------------------------------------------------
}
constructor TFinderSQLGenerator.Create;
begin
  inherited Create;
end;  // TFinderSQLGenerator.Create

{-------------------------------------------------------------------------------
}
destructor TFinderSQLGenerator.Destroy;
begin
  inherited Destroy;
end;  // TFinderSQLGenerator.Destroy

{-------------------------------------------------------------------------------
  Applies settings such as user domain mask, show common names to the result
     SQL
}
function TFinderSQLGenerator.ApplyAppSettings(const ASql: string): string;
begin
  Result := ASql;
  Result := StringReplace(Result, '@UserDomainMask', IntToStr(AppSettings.DomainMask),
      [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '@SessionID', '''' + AppSettings.SessionID + '''',
      [rfReplaceAll, rfIgnoreCase]);
  if AppSettings.DisplayCommonNames then
    Result := StringReplace(Result, '@ShowCommonNames', '1', [rfReplaceAll, rfIgnoreCase])
  else
    Result := StringReplace(Result, '@ShowCommonNames', '0', [rfReplaceAll, rfIgnoreCase]);
end;

{-------------------------------------------------------------------------------
  Generates sql for returning
}
function TFinderSQLGenerator.GenerateSql(
            criteria: array of ICriterion;
            IncludeAll: Boolean;
            out PreSql: string) : string;

begin
   PreSql := FILTER_TABLE_SQL;
   PreSql := PreSql + GetSqlJoinClause(criteria, IncludeAll);
   PreSql := PreSql + GetSqlWhereClause(criteria, IncludeAll);
   Result := Format(SPECIMEN_SQL, ['#Filter', '#Filter']);
   Result := ApplyAppSettings(Result);
   Result := Result + ' DROP TABLE #Filter';


end;

{-----------------------------------------------------------------------------
  Concatenates the join clauses required by the search criteria in lbFinder
}
function TFinderSQLGenerator.GetSqlJoinClause(
            const criteria: array of ICriterion;
            includeAll: Boolean): string;
var
  lCurrentCriterion: ICriterion;
  lCurrentTables: TStrings;
  lTables: TStrings;
  lTable: String;
  lJoinClause: string;
  i, j: Integer;
begin
  lTables := TStringList.Create;
  for i := 0 to Length(criteria) - 1 do
  begin
    lCurrentCriterion := criteria[i];
    lCurrentTables := lCurrentCriterion.GetJoinTables;
    if not (lCurrentTables = nil) then
    begin
      for j := 0 to lCurrentTables.Count - 1 do
      begin
        lTable := lCurrentTables[j];
        if Length(lCurrentTables.Names[j]) = 0 then
        begin
          if includeAll and (TJoinTable(StrToInt(lTable)) in [
            jtHierarchicalSynonym,
            jtSurveyEventGeoAreaSynonym]) then
          begin
            lTable := lTable + '=' + IntToStr(i);
          end;
        end;
        if lTables.IndexOf(lTable) = -1 then lTables.Add(lTable);
      end;
    end;
  end;

  lJoinClause := '';
  for i := 0 to lTables.Count - 1 do
  begin
    lJoinClause := lJoinClause + GetLeftJoin(lTables.Strings[i],
                                              lTables.Names[i],
                                              lTables.ValueFromIndex[i],
                                              criteria,
                                              includeAll);
  end;
  Result := lJoinClause;
end;

{-------------------------------------------------------------------------------
  Contains the join clause required by the select query for each type of
  join table
}
function TFinderSQLGenerator.GetLeftJoin(
  const tableOrdinal: string;
  const tableMetadataOrdinal: string;
  const tableKey: string;
  const criteria: array of ICriterion;
  includeAll: Boolean): string;
var
  table: TJoinTable;
  criterion: ICriterion;
  additionalLogic: string;
begin
  if Length(tableMetadataOrdinal) = 0 then
    table := TJoinTable(StrToInt(tableOrdinal))
  else
    table := TJoinTable(StrToInt(tableMetadataOrdinal));
  case table of
  jtCollectionByParentKey:
    Result := 'LEFT JOIN Collection Coll ON SpecUnit.Parent_Collection_Collection_Unit_Key = Coll.Collection_Unit_Key'#13#10;
  jtSpecimenFieldData:
    Result := 'LEFT JOIN Specimen_Field_Data SpecFieldData ON SpecUnit.Collection_Unit_Key = SpecFieldData.Collection_Unit_Key'#13#10;
  jtOccurrence:
    Result := 'LEFT JOIN Occurrence Occ ON Occ.Occurrence_Key = SpecFieldData.Occurrence_Key'#13#10;
  jtTaxonOccurrence:
    Result := 'LEFT JOIN Taxon_Occurrence TaxOcc ON TaxOcc.Taxon_Occurrence_Key = SpecFieldData.Taxon_Occurrence_Key'#13#10;
  jtSample:
    Result := 'LEFT JOIN Sample Samp ON Samp.Sample_Key = TaxOcc.Sample_Key OR Samp.Sample_Key = Occ.Sample_Key'#13#10;
  jtSampleRecorder:
    Result := 'LEFT JOIN (Sample_Recorder SampRec INNER JOIN Survey_Event_Recorder SurEvRec ON SurEvRec.SE_Recorder_Key = SampRec.SE_Recorder_Key) ON SampRec.Sample_Key = Samp.Sample_Key'#13#10;
  jtSurveyEvent:
    Result := 'LEFT JOIN Survey_Event SurEv ON SurEv.Survey_Event_Key = Samp.Survey_Event_Key'#13#10;
  jtSurveyEventGeoArea:
    Result := 'LEFT JOIN Survey_Event_Geo_Area SurEvGeoArea ON SurEvGeoArea.Survey_Event_Key = Samp.Survey_Event_Key'#13#10;
  jtSurvey:
    Result := 'LEFT JOIN Survey Sur ON SurEv.Survey_Key = Sur.Survey_Key'#13#10;
  jtTaxonDetermination:
    Result := 'LEFT JOIN Taxon_Determination TaxDet ON SpecUnit.Collection_Unit_Key = TaxDet.Specimen_Collection_Unit_Key'#13#10;
  jtIndexTaxonSynonym:
    Result := 'LEFT JOIN Index_Taxon_Synonym IndTaxSyn ON IndTaxSyn.Synonym_List_Item_Key=TaxDet.Taxon_List_Item_Key'#13#10;
  jtTaxon:
    Result := 'LEFT JOIN Taxon Tax ON IndTaxSyn.Taxon_List_Item_Key = Tax.Taxon_Key'#13#10;
  jtIndexTaxonInGroup:
    Result := 'LEFT JOIN Index_Taxon_Group IndTaxGro ON IndTaxGro.Contained_List_Item_Key=IndTaxSyn.Taxon_List_Item_Key'#13#10;
  jtIndexTaxonSynonymInGroup:
    Result := 'LEFT JOIN Index_Taxon_Synonym IndTaxSynGro ON IndTaxSynGro.Synonym_List_Item_Key=IndTaxGro.Taxon_List_Item_Key'#13#10;
  jtTaxonInGroup:
    Result := 'LEFT JOIN Taxon TaxInGro ON IndTaxSynGro.Taxon_List_Item_Key = TaxInGro.Taxon_Key'#13#10;
  jtMovementCollectionUnit:
    Result := 'LEFT JOIN Movement_Collection_Unit MovCollUnit ON SpecUnit.Collection_Unit_Key = MovCollUnit.Collection_Unit_Key'#13#10;
  jtMovementCollectionUnitByCollection:
    Result := 'LEFT JOIN Movement_Collection_Unit MovCollUnitColl ON Coll.Collection_Unit_Key = MovCollUnitColl.Collection_Unit_Key'#13#10;
  jtMovementDirection:
    Result := 'LEFT JOIN Movement_Direction MovDir  ON  MovCollUnit.Movement_Direction_Key = MovDir.Movement_Direction_Key'#13#10;
  jtMovement:
    Result := 'LEFT JOIN Movement Mov ON MovDir.Movement_Key = Mov.Movement_Key'#13#10;
  jtMovementDirectionByCollection:
    Result := 'LEFT JOIN Movement_Direction MovDirColl  ON  MovCollUnitColl.Movement_Direction_Key = MovDirColl.Movement_Direction_Key AND MovDirColl.Outbound = 0';
  jtMovementByCollection:
    Result := 'LEFT JOIN Movement MovColl ON MovDirColl.Movement_Key = MovColl.Movement_Key'#13#10;
  jtCollectionUnitNumber:
    Result := 'LEFT JOIN Collection_Unit_Number CollUnitNum ON  SpecUnit.Collection_Unit_Key = CollUnitNum.Collection_Unit_Key'#13#10;
  jtCollectionUnitNumberPreferred:
    Result := 'LEFT JOIN Collection_Unit_Number CollUnitNumPref ON  SpecUnit.Collection_Unit_Key = CollUnitNumPref.Collection_Unit_Key AND CollUnitNumPref.Preferred = 1'#13#10;
  jtDeterminationPreferred:
    Result := 'LEFT JOIN Determination DetPref ON  SpecUnit.Preferred_Determination_Key = DetPref.Determination_Key'#13#10;
  jtConceptTermPreferred:
    Result := 'LEFT JOIN VW_ConceptTermPreferred ConTer ON DetPref.Concept_Key = ConTer.Concept_Key'#13#10;
  jtTaxonDeterminationPreferredKey:
    Result := 'LEFT JOIN Taxon_Determination TaxDetPref  ON  SpecUnit.Preferred_Taxon_Determination_Key = TaxDetPref.Taxon_Determination_Key'#13#10;
  jtIndexTaxonName:
    Result := 'LEFT JOIN Index_Taxon_Name IndTaxName ON  TaxDetPref.Taxon_List_Item_Key = IndTaxName.Taxon_List_Item_Key'#13#10;
  jtDetermination:
    Result := 'LEFT JOIN Determination Det ON Det.Specimen_Collection_Unit_Key = SpecUnit.Collection_Unit_Key'#13#10;
  jtCollectionUnit:
    Result := 'LEFT JOIN Collection_Unit CollUnit ON SpecUnit.Collection_Unit_Key = CollUnit.Collection_Unit_Key'#13#10;
  jtCollectionUnitData:
    Result := 'LEFT JOIN Collection_Unit_Data CollUnitData ON SpecUnit.Collection_Unit_Key = CollUnitData.Collection_Unit_Key'#13#10;
  jtSourceFileSpecimenUnit:
    Result :=   'LEFT JOIN Source_Join SJS ON SJS.Record_Key = SpecUnit.Collection_Unit_Key ' +
                'LEFT JOIN Source_File SFS ON SJS.Source_Key = SFS.Source_Key'#13#10;
  jtSourceFileCollection:
    Result :=   'LEFT JOIN Source_Join SJC ON SJC.Record_Key = Coll.Collection_Unit_Key ' +
                'LEFT JOIN Source_File SFC ON SJC.Source_Key = SFC.Source_Key'#13#10;
  jtHasSpatialReference:
    Result := 'LEFT JOIN (' +
              ' SELECT ' +
              '   SFD.Collection_Unit_Key, ' +
              '   COUNT(S.Spatial_Ref) as Spatial_Ref_Count ' +
              ' FROM Specimen_Field_Data SFD ' +
              ' LEFT JOIN Occurrence O ON O.Occurrence_Key = SFD.Occurrence_Key ' +
              ' LEFT JOIN Taxon_Occurrence TaxO ON TaxO.Taxon_Occurrence_Key = SFD.Taxon_Occurrence_Key ' +
              ' LEFT JOIN Sample S ON S.Sample_Key = TaxO.Sample_Key OR S.Sample_Key = O.Sample_Key ' +
              ' GROUP BY SFD.Collection_Unit_Key ' +
              ') as SpecHasSpatialRef ON  SpecHasSpatialRef.Collection_Unit_Key = SpecUnit.Collection_Unit_Key'#13#10;
  jtLocationName:
    Result := 'LEFT JOIN Location_Name LocNam ON LocNam.Location_Key = Samp.Location_Key'#13#10;
  jtConceptByParameter:
    Result := 'LEFT JOIN Concept ConByPar ON CollUnitData.Parameter_Concept_Key = ConByPar.Concept_Key'#13#10;
  jtSynonymByParameter:
    Result := 'LEFT JOIN Concept SynByPar ON ConByPar.Meaning_Key = SynByPar.Meaning_Key'#13#10;
  jtSearchTermByParameter:
    Result := 'LEFT JOIN Search_Term SearchTermByPar ON SynByPar.Concept_Key = SearchTermByPar.Concept_Key'#13#10;
  jtConceptByType:
    Result := 'LEFT JOIN Concept ConByType ON SpecUnit.Specimen_Type_Concept_Key = ConByType.Concept_Key'#13#10;
  jtSynonymByType:
    Result := 'LEFT JOIN Concept SynByType ON ConByType.Meaning_Key = SynByType.Meaning_Key'#13#10;
  jtSearchTermByType:
    Result := 'LEFT JOIN Search_Term SearchTermByType ON SynByType.Concept_Key = SearchTermByType.Concept_Key'#13#10;
  jtPlainMetadata:
    Result := 'LEFT JOIN Metadata Met ON Met.Record_Key = SpecUnit.Collection_Unit_Key'#13#10;
  jtMetadata:
    Result := 'LEFT JOIN Metadata Met' + StringReplace(tableKey, ' ', '', [rfReplaceAll]) + ' ON Met' + StringReplace(tableKey, ' ', '', [rfReplaceAll]) +
                '.Record_Key = SpecUnit.Collection_Unit_Key'#13#10;
  jtMetadataByCollection:
    Result := 'LEFT JOIN Metadata MetColl' + StringReplace(tableKey, ' ', '', [rfReplaceAll]) + ' ON MetColl' + StringReplace(tableKey, ' ', '', [rfReplaceAll]) +
                '.Record_Key = Coll.Collection_Unit_Key'#13#10;
  jtMetadataType:
    Result := 'LEFT JOIN Metadata_Type MetType' + StringReplace(tableKey, ' ', '', [rfReplaceAll]) +
              ' ON Met' + StringReplace(tableKey, ' ', '', [rfReplaceAll]) + '.Metadata_Type_Key = MetType' +
              StringReplace(tableKey, ' ', '', [rfReplaceAll]) + '.Metadata_Type_Key AND ' +
              'MetType' + StringReplace(tableKey, ' ', '', [rfReplaceAll]) + '.Table_Name = ''Specimen'' ' +
              'AND MetType' + StringReplace(tableKey, ' ', '', [rfReplaceAll]) + '.Item_Name = ''' +
              tableKey + ''''#13#10;
  jtMetadataTypeByCollection:
    Result := 'LEFT JOIN Metadata_Type MetTypeColl' + StringReplace(tableKey, ' ', '', [rfReplaceAll]) +
              ' ON MetColl' + StringReplace(tableKey, ' ', '', [rfReplaceAll]) + '.Metadata_Type_Key = MetTypeColl' +
              StringReplace(tableKey, ' ', '', [rfReplaceAll]) + '.Metadata_Type_Key  AND ' +
              'MetTypeColl' + StringReplace(tableKey, ' ', '', [rfReplaceAll]) + '.Table_Name = ''Collection'' ' +
              'AND MetTypeColl' + StringReplace(tableKey, ' ', '', [rfReplaceAll]) + '.Item_Name = ''' +
              tableKey + ''''#13#10;
  jtConceptByDetermination:
    Result := 'LEFT JOIN Concept ConByDet ON Det.Concept_Key = ConByDet.Concept_Key'#13#10;
  jtSynonymByDetermination:
    Result := 'LEFT JOIN Concept SynByDet ON ' +
                'ConByDet.Meaning_Key = SynByDet.Meaning_Key'#13#10;
  jtSearchTermByDetermination:
    Result := 'LEFT JOIN Search_Term SearchTermByDet ON ' +
                'SynByDet.Concept_Key = SearchTermByDet.Concept_Key'#13#10;
  jtIndexTaxonSynonymPreferred:
    Result := 'LEFT JOIN Index_Taxon_Synonym IndTaxSynPref ON ' +
                'IndTaxSynPref.Synonym_List_Item_Key=TaxDetPRef.Taxon_List_Item_Key'#13#10;
  jtTaxonPreferred:
    Result := 'LEFT JOIN Taxon TaxPref ON ' +
                'IndTaxSynPref.Taxon_List_Item_Key = TaxPref.Taxon_Key'#13#10;
  jtConceptByDeterminationPreferred:
    Result := 'LEFT JOIN Concept ConByDetPref ON ' +
                'DetPref.Concept_Key = ConByDetPref.Concept_Key'#13#10;
  jtSynonymByDeterminationPreferred:
    Result := 'LEFT JOIN Concept SynByDetPref ' +
                'ON ConByDetPref.Meaning_Key = SynByDetPref.Meaning_Key'#13#10;
  jtSearchTermByDeterminationPreferred:
    Result := 'LEFT JOIN Search_Term SearchTermByDetPref ON ' +
                'SynByDetPref.Concept_Key = SearchTermByDetPref.Concept_Key'#13#10;
  jtConceptNomenclatureStatus:
    Result := 'LEFT JOIN VW_ConceptTerm ConNom ON ' +
                'ConNom.Concept_Key = Det.Nomenclatural_Status_Concept_Key'#13#10;
  jtIndividual:
    Result := 'LEFT JOIN Individual Ind ON Ind.Name_Key = Coll.Assembler_Name_Key'#13#10;
  jtOrganisation:
    Result := 'LEFT JOIN Organisation Org ON Org.Name_Key = Coll.Assembler_Name_Key'#13#10;

  jtHierarchicalSynonym:
  begin
    Result := Format(
        'LEFT JOIN'#13#10 +
        '('#13#10 +
        '  	SELECT Concept_Key, Meaning_Key, PlainText'#13#10 +
        '  	FROM  	vw_ConceptTerm'#13#10 +
        '   UNION ALL'#13#10 +
        '  	SELECT DISTINCT CSearch.Concept_Key, CChild.Meaning_Key, CSearch.PlainText'#13#10 +
        '		FROM 		vw_ConceptTerm CSearch'#13#10 +
        '		INNER JOIN 	Concept_Group CG ON CG.Concept_Group_Key = CSearch.Concept_Group_Key'#13#10 +
        '		INNER JOIN 	Local_Domain LD ON LD.Local_Domain_Key = CG.Local_Domain_Key'#13#10 +
        '		INNER JOIN 	Domain D ON D.Domain_Key = LD.Domain_Key AND D.Has_Occurrences = 1'#13#10 +
        '		INNER JOIN 	Concept CSynSearch ON CSynSearch.Meaning_Key = CSearch.Meaning_Key'#13#10 +
        '		INNER JOIN 	Concept_Lineage CL ON CL.Concept_Key = CSynSearch.Concept_Key'#13#10 +
        '		INNER JOIN 	Concept_Lineage CLChild ON CLChild.Lineage LIKE CL.Lineage + ''\%%'''#13#10 +
        '		INNER JOIN 	Concept CChild ON CChild.Concept_Key = CLChild.Concept_Key AND (CChild.Concept_Group_Key = CSynSearch.Concept_Group_Key)'#13#10 +
        ') as HierSyn%0:s'#13#10 +
        'ON ConByDet.Meaning_Key = HierSyn%0:s.Meaning_Key'#13#10,
        [IfThen(includeAll, tableKey, '')]);
    if includeAll then begin
      criterion := criteria[StrToInt(tableKey)];
      Result := Result + ' AND ' + StringReplace(
          criterion.GetJoinLogic(table),
          'Syn%0:s',
          'Syn' + tableKey,
          [rfReplaceAll]) + #13#10;
    end;
  end;
  jtIndividualByDetermination:
    Result := 'LEFT JOIN Individual IndDet ON IndDet.Name_Key = Det.Determiner_Name_Key'#13#10;
  jtIndividualByTaxonDetermination:
    Result := 'LEFT JOIN Individual IndTaxDet ON IndTaxDet.Name_Key = TaxDet.Determiner'#13#10;
  jtSurveyEventGeoAreaSynonym:
  begin
    Result := Format(
        'LEFT JOIN'#13#10 +
        '('#13#10 +
        '  SELECT'#13#10 +
        '    c.Concept_Key AS Parent_Concept_Key,'#13#10 +
        '    c.Published_Term,'#13#10 +
        '    s.Concept_Key'#13#10 +
        '  FROM dbo.Concept AS c'#13#10 +
        '  INNER JOIN dbo.Concept AS s'#13#10 +
        '  ON s.Meaning_Key = c.Meaning_Key'#13#10 +
        '  UNION'#13#10 +
        '  SELECT'#13#10 +
        '    p.Concept_Key AS Parent_Concept_Key,'#13#10 +
        '    p.Published_Term,'#13#10 +
        '    cs.Concept_Key'#13#10 +
        '  FROM dbo.Concept AS p'#13#10 +
        '  INNER JOIN dbo.Concept AS ps'#13#10 +
        '  ON ps.Meaning_Key = p.Meaning_Key'#13#10 +
        '  INNER JOIN dbo.Concept_Lineage AS pl'#13#10 +
        '  ON pl.Concept_Key = ps.Concept_Key'#13#10 +
        '  INNER JOIN dbo.Concept_Lineage AS cl'#13#10 +
        '  ON cl.Lineage LIKE pl.Lineage + ''\%%'''#13#10 +
        '  INNER JOIN dbo.Concept AS c'#13#10 +
        '  ON c.Concept_Key = cl.Concept_Key'#13#10 +
        '  AND c.Concept_Group_Key = ps.Concept_Group_Key'#13#10 +
        '  INNER JOIN dbo.Concept AS cs'#13#10 +
        '  ON cs.Meaning_Key = c.Meaning_Key'#13#10 +
        ') AS GeoSyn%0:s ON SurEvGeoArea.Concept_Key = GeoSyn%0:s.Concept_Key'#13#10,
        [IfThen(includeAll, tableKey, '')]);
    if includeAll then begin
      criterion := criteria[StrToInt(tableKey)];
      Result := Result + ' AND ' + StringReplace(
          criterion.GetJoinLogic(table),
          'Syn%0:s',
          'Syn' + tableKey,
          [rfReplaceAll]) + #13#10;
    end;
  end;
  else
    Result := '';
  end;

  if (Result <> '') and (not includeAll) then
  begin
    additionalLogic := GetJoinLogicForTable(table, criteria, includeAll);
    if additionalLogic <> '' then
    begin
      Result := Result + 'AND (' +
          StringReplace(additionalLogic, 'Syn%0:s', 'Syn', [rfReplaceAll]) +
      ')'#13#10;
    end;
  end;
end;

{-------------------------------------------------------------------------------
  Returns any additional logic that is required in the join clause for the
  specified table.
}
function TFinderSQLGenerator.GetJoinLogicForTable(
  table: TJoinTable;
  const criteria: array of ICriterion;
  includeAll: Boolean): string;
var
  I: Integer;
  Logic: string;
begin
  Result := '';
  for I := 0 to Length(criteria) - 1 do
  begin
    Logic := criteria[I].GetJoinLogic(table);
    if Logic <> '' then
    begin
      if Result <> '' then
      begin
        if includeAll then
          Result := Result + ' AND '
        else
          Result := Result + ' OR ';
      end;
      Result := Result + '(' + Logic + ')';
    end;
  end;
end;

{-------------------------------------------------------------------------------
  Returns the WHERE clause required by the select query.
}
function TFinderSQLGenerator.GetSqlWhereClause(
            criteria: array of ICriterion;
            includeAll: Boolean) : string;
var
  i: integer;
  lWhereClause: string;
  lLogicalOperator : string;
begin
  if includeAll then
  begin
    lLogicalOperator := ' AND ';
  end else
    lLogicalOperator := ' OR ';

  lWhereClause := ' WHERE ';
  for i := 0 to Length(criteria) - 1 do
  begin
    if i > 0 then
    begin
      lWhereClause := lWhereClause + lLogicalOperator;
    end;
    lWhereClause := lWhereClause + '(' + StringReplace(
        criteria[i].GetSqlWhereExpression,
        'Syn%0:s',
        'Syn' + IfThen(includeAll, IntToStr(i), ''),
        [rfReplaceAll]) +
    ')'#13#10;
  end;
  Result := lWhereClause;
end;

end.
