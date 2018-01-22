{===============================================================================
  Unit:         BooleanProperty

  Defines:      TBooleanPropertyEnum
                TBooleanPropertyCategory
                TLogicalOperator
                TOperatorSet
                TPropertyType
                TBooleanProperty
                TOperatorPair
                TCategoryPair

  Description:  Defines the types of filtering properties that can be created
                in specimen finder. These include information about which type
                of filtering property it is (string, boolean, etc.), which
                logical operators are available to it, and which category it
                belongs to (Collection, Specimen or Specimen Measurement).

===============================================================================}

unit BooleanProperty;

interface

uses
  Classes, Contnrs, TypInfo, SysUtils, GeneralData, ADODB, DataClasses;

type
  TBooleanPropertyEnum = (bpeAccessionNumber, bpeAssembledBy, bpeCollectionName,
                          bpeCollectionHasMultimedia, bpeAnyNumber,
                          bpeCollectionTopic, bpeCollectionMetadata,
                          bpePreferredNumber, bpePreferredAccessionNumber,
                          bpePreferredDetermination, bpeAnyDetermination,
                          bpeDeterminationInGroup, bpeDeterminer, bpeSpecimenType,
                          bpeSpecimenGatheringLocation, bpeSpecimenGatheringDate,
                          bpeValue, bpeAppliesTo, bpeParameter,
                          bpeSpecimenHasMultimedia, bpeObservationLocation,
                          bpeSpecimenMetadata, bpeNomenclatureStatus,
                          bpeHasSpatialReference, bpeGeoArea,
                          bpeSpecimenIsChecked, bpeSpecimenIsConfidential,
                          bpeSpecimenIsDangerous, bpeSpecimenIsForInternalUse,
                          bpeSpecimenIsForWebUse);

  TBooleanPropertyCategory = (bpcCollections,
                              bpcSpecimens,
                              bpcSpecMeasurements);

  TLogicalOperator = (loEqualTo, loNotEqualTo, loGreaterThan, loLessThan,
                        loBetween, loStartsWith, loContains);

  TOperatorSet = set of TLogicalOperator;

  TPropertyType = (ptBoolean, ptVagueDate, ptString, ptMeasurementValue);

  TBooleanProperty = class(TObject)
    private
      FPropertyEnum: TBooleanPropertyEnum;
      FPropertyName: string;
      FCategory: TBooleanPropertyCategory;
      FPropertyType: TPropertyType;
      FApplicableOperators: TOperatorSet;
      FHasMetadata : Boolean;
      FMetadata : TStringList;
      FIsMetadata : Boolean;
      FMetadataName : string;
      procedure SetNameCategoryAndType;
      procedure SetApplicableOperators;
      procedure SetProperties(
                  propertyName: string;
                  category: TBooleanPropertyCategory;
                  propertyType: TPropertyType);

    public
      constructor Create(propertyEnum: TBooleanPropertyEnum); overload;
      constructor Create; overload;
      destructor Destroy; override;
      property PropertyEnum: TBooleanPropertyEnum read FPropertyEnum;
      property PropertyName: string read FPropertyName;
      property PropertyType: TPropertyType read FPropertyType;
      property Category: TBooleanPropertyCategory read FCategory;
      property ApplicableOperators: TOperatorSet read FApplicableOperators;
      property HasMetadata : Boolean read FHasMetadata write FHasMetadata;
      property Metadata : TStringList read FMetadata;
      property IsMetadata : Boolean read FIsMetadata write FIsMetadata;
      property MetadataName : string read FMetadataName write FMetadataName;
      procedure SetMetadata;
  end;

  TOperatorPair = class(TObject)
    private
      FOperatorEnum: TLogicalOperator;
      FOperatorName: string;
    public
      constructor Create(operatorEnum: TLogicalOperator);
      property OperatorEnum: TLogicalOperator read FOperatorEnum;
      property OperatorName: string read FOperatorName;
  end;

  TCategoryPair = class(TObject)
    private
      FCategoryEnum: TBooleanPropertyCategory;
      FCategoryName: string;
    public
      constructor Create(categoryEnum: TBooleanPropertyCategory);
      property CategoryEnum: TBooleanPropertyCategory read FCategoryEnum;
      property CategoryName: string read FCategoryName;


end;

implementation

uses ADOInt;
{-------------------------------------------------------------------------------
}
constructor TBooleanProperty.Create(propertyEnum: TBooleanPropertyEnum);
begin
  inherited Create;
  FPropertyEnum := propertyEnum;
  SetNameCategoryAndType;
  SetApplicableOperators;
  FHasMetadata := False;
  FIsMetadata := False;
end;

constructor TBooleanProperty.Create;
begin
  inherited Create;
end;

{-------------------------------------------------------------------------------
}
destructor TBooleanProperty.Destroy;
begin
  FMetadata.Free;
  inherited Destroy;
end;

{-------------------------------------------------------------------------------
}
procedure TBooleanProperty.SetNameCategoryAndType;
begin
  case FPropertyEnum of
    bpeAccessionNumber:
      SetProperties('Accession Number', bpcCollections, ptString);
    bpeAssembledBy:
      SetProperties('Assembled By', bpcCollections, ptString);
    bpeCollectionName:
      SetProperties('Collection Name', bpcCollections, ptString);
    bpePreferredNumber:
      SetProperties('Preferred Number', bpcSpecimens, ptString);
    bpeCollectionHasMultimedia:
      SetProperties('Has Multimedia', bpcCollections, ptBoolean);
    bpeCollectionTopic:
      SetProperties('Topic', bpcCollections, ptString);
    bpeCollectionMetadata:
      SetProperties('Metadata', bpcCollections, ptString);
    bpePreferredAccessionNumber:
      SetProperties('Preferred Accession Number', bpcSpecimens, ptString);
    bpeAnyNumber:
      SetProperties('Any Number', bpcSpecimens, ptString);
    bpePreferredDetermination:
      SetProperties('Preferred Determination', bpcSpecimens, ptString);
    bpeAnyDetermination:
      SetProperties('Any Determination', bpcSpecimens, ptString);
    bpeDeterminationInGroup:
      SetProperties('Determination In Group', bpcSpecimens, ptString);
    bpeSpecimenType:
      SetProperties('Specimen Type', bpcSpecimens, ptString);
    bpeSpecimenGatheringLocation:
      SetProperties('Gathering Location', bpcSpecimens, ptString);
    bpeSpecimenGatheringDate:
      SetProperties('Gathering Date', bpcSpecimens, ptVagueDate);
    bpeValue:
      SetProperties('Value', bpcSpecMeasurements, ptMeasurementValue);
    bpeAppliesTo:
      SetProperties('Applies To', bpcSpecMeasurements, ptString);
    bpeParameter:
      SetProperties('Parameter', bpcSpecMeasurements, ptString);
    bpeSpecimenHasMultimedia:
      SetProperties('Has Multimedia', bpcSpecimens, ptBoolean);
    bpeObservationLocation:
      SetProperties('Observation Location', bpcSpecimens, ptString);
    bpeSpecimenMetadata:
      SetProperties('Metadata', bpcSpecimens, ptString);
    bpeNomenclatureStatus:
      SetProperties('Nomenclature Status', bpcSpecimens, ptString);
    bpeHasSpatialReference:
      SetProperties('Has Spatial Reference', bpcSpecimens, ptBoolean);
    bpeDeterminer:
      SetProperties('Determiner', bpcSpecimens, ptString);
    bpeGeoArea:
      SetProperties('Geographic Area', bpcSpecimens, ptString);
    bpeSpecimenIsChecked:
      SetProperties('Checked', bpcSpecimens, ptBoolean);
    bpeSpecimenIsConfidential:
      SetProperties('Confidential', bpcSpecimens, ptBoolean);
    bpeSpecimenIsDangerous:
      SetProperties('Dangerous', bpcSpecimens, ptBoolean);
    bpeSpecimenIsForInternalUse:
      SetProperties('Internal Use', bpcSpecimens, ptBoolean);
    bpeSpecimenIsForWebUse:
      SetProperties('Web Use', bpcSpecimens, ptBoolean);
  end;
end;

{-------------------------------------------------------------------------------
  Sets the name of the filter property (to be displayed in the drop-down list)
  as well as the value of the filter property and category types.
}
  procedure TBooleanProperty.SetProperties(
              propertyName: string;
              category: TBooleanPropertyCategory;
              propertyType: TPropertyType);
  begin
     FPropertyName := propertyName;
     FCategory := category;
     FPropertyType := propertyType;

  end;

{-------------------------------------------------------------------------------
  Defines the set of logical operators available, depending on the filtering
  property.
}
  procedure TBooleanProperty.SetApplicableOperators;
  begin
     case FPropertyType of
     ptBoolean:
        FApplicableOperators := [loEqualTo, loNotEqualTo];
     ptVagueDate:
        FApplicableOperators := [loEqualTo, loNotEqualTo, loGreaterThan,
                          loLessThan, loBetween];
      ptString:
        FApplicableOperators := [loEqualTo, loNotEqualTo, loStartsWith, loContains];
      ptMeasurementValue:
        FApplicableOperators := [loEqualTo, loNotEqualTo, loGreaterThan,
                          loLessThan, loBetween, loStartsWith, loContains];
    end;

  end;

{-------------------------------------------------------------------------------

}
procedure TBooleanProperty.SetMetadata;
var
  lTableName: string;
  lItemName : string;
begin
  if HasMetadata then
  begin
    FMetadata.Free;
    FMetadata := TStringList.Create;
    case FPropertyEnum of
      bpeSpecimenMetadata:
        lTableName := 'Specimen';
      bpeCollectionMetadata:
        lTableName := 'Collection';
    end;
    with dmGeneral.GetRecordset('dbo.usp_MetadataType_Select',
                          ['@TableName', lTableName]) do
    begin
      while not EOF do begin
        lItemName := Fields['Item_Name'].Value;
        FMetadata.Add(lItemName);
        MoveNext;
      end;
      Close;
    end;
  end;
end;

{-------------------------------------------------------------------------------
  Upon creation, sets the text to be displayed in the drop-down list of logical
  operators.
}
constructor TOperatorPair.Create(operatorEnum: TLogicalOperator);
begin
   FOperatorEnum := operatorEnum;
   case FOperatorEnum of
     loEqualTo:
       FOperatorName := 'is equal to';
     loNotEqualTo:
       FOperatorName := 'is not equal to';
     loGreaterThan:
       FOperatorName := 'is greater than';
     loLessThan:
       FOperatorName := 'is less than';
     loBetween:
       FOperatorName := 'is between';
     loStartsWith:
       FOperatorName := 'starts with';
     loContains:
       FOperatorName := 'contains';
  end;
end;

{-------------------------------------------------------------------------------
  Upon creation, sets the text to be displayed for a particular category in the
  drop-down list of filter properties.
}
constructor TCategoryPair.Create(categoryEnum: TBooleanPropertyCategory);
begin
  FCategoryEnum := categoryEnum;
    case FCategoryEnum of
      bpcCollections:
        FCategoryName := 'Collection';
      bpcSpecimens:
        FCategoryName := 'Specimen';
      bpcSpecMeasurements:
       FCategoryName := 'Specimen Measurements';
    end;
  end;
end.
