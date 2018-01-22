{===============================================================================
  Unit:        ICriterionUnit

  Defines:     ICriterion
               TJoinTable

  Description: Interface used by all types of specimen filtering criteria.
               Contains a list of tables to be joined when generated the
               search SQL.

===============================================================================}

unit ICriterionUnit;

interface

uses
  Classes;

type

  TJoinTable = (jtCollectionByParentKey, jtSpecimenFieldData, jtOccurrence,
                jtTaxonOccurrence, jtSample, jtSampleRecorder, jtSurveyEvent,
                jtSurveyEventGeoArea,
                jtSurvey, jtTaxonDetermination, jtIndexTaxonSynonym, jtTaxon,
                jtIndexTaxonInGroup, jtIndexTaxonSynonymInGroup, jtTaxonInGroup,
                jtMovementCollectionUnit,
                jtMovementCollectionUnitByCollection,
                jtMovementDirection, jtMovement, jtCollectionUnitNumber,
                jtCollectionUnitNumberPreferred,
                jtDeterminationPreferred, jtTaxonDeterminationPreferredKey,
                jtIndexTaxonName, jtConceptTermPreferred, jtDetermination,
                jtCollectionUnit, jtCollectionUnitData, jtSourceFileCollection,
                jtSourceFileSpecimenUnit, jtLocationName, jtConceptByParameter,
                jtSynonymByParameter, jtSearchTermByParameter,
                jtConceptByDetermination, jtSynonymByDetermination,
                jtSearchTermByDetermination,
                jtConceptByType, jtSynonymByType, jtSearchTermByType,
                jtPlainMetadata, jtPlainMetadataType,
                jtMetadata, jtMetadataByCollection, jtMetadataType,
                jtMetadataTypeByCollection,
                jtConceptByDeterminationPreferred,
                jtSynonymByDeterminationPreferred,
                jtIndexTaxonSynonymPreferred,
                jtTaxonPreferred, jtSearchTermByDeterminationPreferred,
                jtConceptNomenclatureStatus, jtHasSpatialReference,
                jtMovementDirectionByCollection, jtMovementByCollection,
                jtPreferredDeterminationName,
                jtIndividual, jtOrganisation,
                jtHierarchicalSynonym, jtHierarchicalConcept,
                jtIndividualByDetermination, jtIndividualByTaxonDetermination,
                jtSurveyEventGeoAreaSynonym);

  //TTableSet = set of TJoinTable;

  ICriterion = interface(IInterface)

    function GetJoinTables : TStrings;
    function GetJoinLogic(Table: TJoinTable): string;
    function GetSqlWhereExpression : string;
    property JoinTables : TStrings read GetJoinTables;
  end;

implementation

end.
