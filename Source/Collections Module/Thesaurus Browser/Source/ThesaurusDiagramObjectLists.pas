{===============================================================================
  Unit:        ThesaurusDiagramObjectLists

  Defines:     TDiagramConceptLists
               TDiagramRelationshipList

  Description: Stringly typed lists of the objects that appear on a diagram

  Created:     Nov 2003

  Model:       Thesaurus.mpb

  Last revision information:
    $Revision: 6 $
    $Date: 3/09/04 16:25 $
    $Author: Johnvanbreda $

===============================================================================}
unit ThesaurusDiagramObjectLists;

interface

uses
  SysUtils, Windows, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, DiagramObjects, ThesaurusDiagramObjects, XMLDoc, XMLIntf,
  DiagramXMLConstants;
  
resourcestring
  ResStr_LoadingConcepts = 'Loading concepts...'; 

type
  TBaseDiagramObjectList = class (TList)
  private
    FDiagram: TObject;
    class function GetXMLElementName: string; virtual; abstract;
  public
    constructor Create(ADiagram: TObject); reintroduce;
    procedure ReadXML(AXMLNode: IXMLNode); virtual; abstract;
    procedure WriteXML(AXMLNode: IXMLNode);
  end;
  
  {-----------------------------------------------------------------------------
    Strongly typed list of relationships on the diagram.  The class is a 
    strongly typed derivative of TList.
  }
  TDiagramRelationshipList = class (TBaseDiagramObjectList)
  private
    FRelationTypeKey: string;
    function GetItems(AIndex: Integer): TDiagramRelationship;
    class function GetXMLElementName: string; override;
    procedure SetItems(AIndex: Integer; AItem: TDiagramRelationship);
  public
    destructor Destroy; override;
    function Add(AItem: TDiagramRelationship): Integer;
    procedure Clear; override;
    function Extract(AItem: TDiagramRelationship): TDiagramRelationship;
    function FindRelationship(AFromConceptID, AToConceptID: integer; ATypeKey, 
        ATypeName: string; out AReverse: boolean): TDiagramRelationship;
    function First: TDiagramRelationship;
    procedure FreeAndDelete(AIndex: integer);
    function IndexOf(AItem: TDiagramRelationship): Integer;
    procedure Insert(AIndex: Integer; AItem: TDiagramRelationship);
    function Last: TDiagramRelationship;
    procedure ReadXML(AXMLNode: IXMLNode); override;
    function Remove(AItem: TDiagramRelationship): Integer;
    property Items[AIndex: Integer]: TDiagramRelationship read GetItems write 
        SetItems; default;
    property RelationTypeKey: string read FRelationTypeKey;
  end;
  
  {-----------------------------------------------------------------------------
    Strongly typed list of concepts on the diagram.  The class is a strongly 
    typed derivative of TList.
  }
  TDiagramConceptList = class (TBaseDiagramObjectList)
  private
    function GetItems(AIndex: Integer): TDiagramConcept;
    class function GetXMLElementName: string; override;
    procedure SetItems(AIndex: Integer; AItem: TDiagramConcept);
  public
    destructor Destroy; override;
    function Add(AItem: TDiagramConcept): Integer;
    procedure Clear; override;
    function Extract(AItem: TDiagramConcept): TDiagramConcept;
    function First: TDiagramConcept;
    procedure FreeAndDelete(AIndex: integer);
    function IndexOf(AItem: TDiagramConcept): Integer;
    procedure Insert(AIndex: Integer; AItem: TDiagramConcept);
    function Last: TDiagramConcept;
    procedure ReadXML(AXMLNode: IXMLNode); override;
    function Remove(AItem: TDiagramConcept): Integer;
    property Items[AIndex: Integer]: TDiagramConcept read GetItems write 
        SetItems; default;
  end;
  
  TRelationType=(rtConcept, rtMeaning, rtTermVersion);

  {-----------------------------------------------------------------------------
    Base class for diagram classes that manage details for a category of 
    objects.  This includes a concept group or a relation type.
  }
  TBaseDiagramObjectCategory = class (TBaseDiagramPersistantObject)
  protected
    FCaption: string;
  public
    property Caption: string read FCaption;
  end;
  
  {-----------------------------------------------------------------------------
    List of concept groups present on the diagram.
  }
  TDiagramConceptGroup = class (TBaseDiagramObjectCategory)
  private
    FConceptGroupKey: string;
    FDisplayProperties: TConceptDisplayProperties;
    procedure SetConceptGroupKey(const Value: string);
  public
    constructor Create; reintroduce; overload;
    destructor Destroy; override;
    procedure ReadXML(AXMLNode: IXMLNode); override;
    procedure WriteXML(AXMLNode: IXMLNode); override;
    property ConceptGroupKey: string read FConceptGroupKey write 
        SetConceptGroupKey;
    property DisplayProperties: TConceptDisplayProperties read 
        FDisplayProperties;
  end;
  
  {-----------------------------------------------------------------------------
    List of relationship types present on the diagram.
  }
  TDiagramRelationType = class (TBaseDiagramObjectCategory)
  private
    FDisplayProperties: TRelationshipDisplayProperties;
    FRelationType: TRelationType;
    FRelationTypeKey: string;
    procedure SetRelationType(Value: TRelationType);
    procedure SetRelationTypeKey(const Value: string);
  public
    constructor Create(const AKey, ACaption: string; ADiagramProperties: 
        TRelationshipDisplayProperties); reintroduce; overload;
    destructor Destroy; override;
    procedure ReadXML(AXMLNode: IXMLNode); override;
    class function RelationTypeStringToEnumeration(const AType: string): 
        TRelationType;
    procedure WriteXML(AXMLNode: IXMLNode); override;
    property DisplayProperties: TRelationshipDisplayProperties read 
        FDisplayProperties;
    property RelationType: TRelationType read FRelationType write 
        SetRelationType;
    property RelationTypeKey: string read FRelationTypeKey write 
        SetRelationTypeKey;
  end;
  
  {-----------------------------------------------------------------------------
    List of the relationship types in the diagram and their display properties. 
    The class is a strongly typed derivative of TList.
  }
  TDiagramRelationTypeList = class (TBaseDiagramObjectList)
  private
    function GetItems(AIndex: Integer): TDiagramRelationType;
    class function GetXMLElementName: string; override;
    procedure SetItems(AIndex: Integer; AItem: TDiagramRelationType);
  public
    destructor Destroy; override;
    function Add(AItem: TDiagramRelationType): Integer;
    procedure Clear; override;
    function Extract(AItem: TDiagramRelationType): TDiagramRelationType;
    function FindRelationType(const ATypeKey, ACaption: string): 
        TDiagramRelationType;
    function First: TDiagramRelationType;
    function IndexOf(AItem: TDiagramRelationType): Integer;
    procedure Insert(AIndex: Integer; AItem: TDiagramRelationType);
    function Last: TDiagramRelationType;
    procedure ReadXML(AXMLNode: IXMLNode); override;
    function Remove(AItem: TDiagramRelationType): Integer;
    property Items[AIndex: Integer]: TDiagramRelationType read GetItems write 
        SetItems; default;
  end;
  
  {-----------------------------------------------------------------------------
    List of the concept groups in the diagram and their display properties.  
    The class is a strongly typed derivative of TList.
  }
  TDiagramConceptGroupList = class (TBaseDiagramObjectList)
  private
    function GetItems(AIndex: Integer): TDiagramConceptGroup;
    class function GetXMLElementName: string; override;
    procedure SetItems(AIndex: Integer; AItem: TDiagramConceptGroup);
  public
    destructor Destroy; override;
    function Add(AItem: TDiagramConceptGroup): Integer;
    procedure Clear; override;
    function Extract(AItem: TDiagramConceptGroup): TDiagramConceptGroup;
    function First: TDiagramConceptGroup;
    function IndexOf(AItem: TDiagramConceptGroup): Integer;
    procedure Insert(AIndex: Integer; AItem: TDiagramConceptGroup);
    function Last: TDiagramConceptGroup;
    procedure ReadXML(AXMLNode: IXMLNode); reintroduce; overload; override;
    function Remove(AItem: TDiagramConceptGroup): Integer;
    property Items[AIndex: Integer]: TDiagramConceptGroup read GetItems write 
        SetItems; default;
  end;
  

implementation

uses
  GeneralData, ThesaurusDiagram, ResourceStrings;

{-==============================================================================
    TBaseDiagramObjectList
===============================================================================}
{-------------------------------------------------------------------------------
  Object initialisation 
}
constructor TBaseDiagramObjectList.Create(ADiagram: TObject);
begin
  inherited Create;
  
  if not (ADiagram is TThesaurusDiagram) then
    raise EThesaurusDiagramObjectsException.Create(Format(
        ResStr_InvalidMethodCall,
        ['TBaseDiagramObjectList.Create']));
  FDiagram := ADiagram;
end;  // TBaseDiagramObjectList.Create 

{-------------------------------------------------------------------------------
  Write the object and its contents into the XML document. 
}
procedure TBaseDiagramObjectList.WriteXML(AXMLNode: IXMLNode);
var
  lIdx: Integer;
  lXMLNode: IXMLNode;
begin
  lXMLNode := AXMLNode.AddChild(GetXMLElementName);
  for lIdx := 0 to Count-1 do
    TBaseDiagramPersistantObject(Items[lIdx]).WriteXML(lXMLNode);
end;  // TBaseDiagramObjectList.WriteXML 

{-==============================================================================
    TDiagramRelationshipList
===============================================================================}
{-------------------------------------------------------------------------------
  Clean up owned objects 
}
destructor TDiagramRelationshipList.Destroy;
begin
  Clear;
  inherited Destroy;
end;  // TDiagramRelationshipList.Destroy 

{-------------------------------------------------------------------------------
  Add a relationship to the list. 
}
function TDiagramRelationshipList.Add(AItem: TDiagramRelationship): Integer;
begin
  Result := inherited Add(Pointer(AItem));
end;  // TDiagramRelationshipList.Add 

{-------------------------------------------------------------------------------
  Clear the list, free owned objects. 
}
procedure TDiagramRelationshipList.Clear;
var
  I: Integer;
begin
  for I := Count - 1 downto 0 do begin
      Items[I].Free;
      Delete(I);
  end;
  
  inherited Clear;
end;  // TDiagramRelationshipList.Clear 

{-------------------------------------------------------------------------------
  Strongly typed version of TList method. 
}
function TDiagramRelationshipList.Extract(AItem: TDiagramRelationship): 
    TDiagramRelationship;
begin
  Result := TDiagramRelationship(inherited Extract(AItem));
end;  // TDiagramRelationshipList.Extract 

{-------------------------------------------------------------------------------
  Locates a relationship between 2 concepts.  Returns lReverse=true if the relationship is in the opposite direction.  If the relationship doesn't exist, then it is created and attached to the supplied type, unless ATypeKey is not supplied in which case nil is returned. 
}
function TDiagramRelationshipList.FindRelationship(AFromConceptID, 
    AToConceptID: integer; ATypeKey, ATypeName: string; out AReverse: boolean): 
    TDiagramRelationship;
var
  lIdx: Integer;
begin
  Result := nil;
  // Loop to find the relationship
  for lIdx := 0 to Count-1 do
    if (AFromConceptID = Items[lIdx].FromConcept.ObjectId) and
        (AToConceptID = Items[lIdx].ToConcept.ObjectId) then begin
      Result := Items[lIdx];
      AReverse := False;
    end
    else if (AFromConceptID = Items[lIdx].ToConcept.ObjectId) and
        (AToConceptID = Items[lIdx].FromConcept.ObjectId) then begin
      Result := Items[lIdx];
      AReverse := True;
    end;
  if not Assigned(Result) and (ATypeKey <> '') then begin
    with TThesaurusDiagram(FDiagram) do begin
      Result := TDiagramRelationship.Create(FDiagram,
          FindConceptByID(AFromConceptID), FindConceptByID(AToConceptID));
      AssignToRelationType(Result, ATypeKey, ATypeName);
    end; // with
    TThesaurusDiagram(FDiagram).ReadSettings(Result);
    Add(Result);
  end;
end;  // TDiagramRelationshipList.FindRelationship 

{-------------------------------------------------------------------------------
  Strongly typed version of TList method. 
}
function TDiagramRelationshipList.First: TDiagramRelationship;
begin
  Result := TDiagramRelationship(inherited First);
end;  // TDiagramRelationshipList.First 

{-------------------------------------------------------------------------------
  Removes a relationship from the list and frees it 
}
procedure TDiagramRelationshipList.FreeAndDelete(AIndex: integer);
begin
  Items[AIndex].Free;
  Delete(AIndex);
end;  // TDiagramRelationshipList.FreeAndDelete 

{-------------------------------------------------------------------------------
}
function TDiagramRelationshipList.GetItems(AIndex: Integer): 
    TDiagramRelationship;
begin
  Result := TDiagramRelationship(inherited Items[AIndex]);
end;  // TDiagramRelationshipList.GetItems 

{-------------------------------------------------------------------------------
}
class function TDiagramRelationshipList.GetXMLElementName: string;
begin
  Result := EL_RELATIONSHIPS;
end;  // TDiagramRelationshipList.GetXMLElementName 

{-------------------------------------------------------------------------------
  Strongly typed version of TList method. 
}
function TDiagramRelationshipList.IndexOf(AItem: TDiagramRelationship): Integer;
begin
  Result := inherited IndexOf(Pointer(AItem));
end;  // TDiagramRelationshipList.IndexOf 

{-------------------------------------------------------------------------------
  Strongly typed version of TList method. 
}
procedure TDiagramRelationshipList.Insert(AIndex: Integer; AItem: 
    TDiagramRelationship);
begin
  inherited Insert(AIndex, Pointer(AItem));
end;  // TDiagramRelationshipList.Insert 

{-------------------------------------------------------------------------------
  Strongly typed version of TList method. 
}
function TDiagramRelationshipList.Last: TDiagramRelationship;
begin
  Result := TDiagramRelationship(inherited Last);
end;  // TDiagramRelationshipList.Last 

{-------------------------------------------------------------------------------
  Reading a relationship list is not implemented, the relationships are 
      automatically loaded as the concepts load. 
}
procedure TDiagramRelationshipList.ReadXML(AXMLNode: IXMLNode);
begin
  // No implementation
end;  // TDiagramRelationshipList.ReadXML 

{-------------------------------------------------------------------------------
  Strongly typed version of TList method. 
}
function TDiagramRelationshipList.Remove(AItem: TDiagramRelationship): Integer;
begin
  Result := inherited Remove(Pointer(AItem));
end;  // TDiagramRelationshipList.Remove 

{-------------------------------------------------------------------------------
}
procedure TDiagramRelationshipList.SetItems(AIndex: Integer; AItem: 
    TDiagramRelationship);
begin
  inherited Items[AIndex] := Pointer(AItem);
end;  // TDiagramRelationshipList.SetItems 

{-==============================================================================
    TDiagramConceptList
===============================================================================}
{-------------------------------------------------------------------------------
}
destructor TDiagramConceptList.Destroy;
begin
  Clear;
  inherited Destroy;
end;  // TDiagramConceptList.Destroy 

{-------------------------------------------------------------------------------
}
function TDiagramConceptList.Add(AItem: TDiagramConcept): Integer;
begin
  Result := inherited Add(Pointer(AItem));
end;  // TDiagramConceptList.Add 

{-------------------------------------------------------------------------------
}
procedure TDiagramConceptList.Clear;
var
  I: Integer;
begin
  for I := Count - 1 downto 0 do begin
      Items[I].Free;
      Delete(I);
  end;
  
  inherited Clear;
end;  // TDiagramConceptList.Clear 

{-------------------------------------------------------------------------------
}
function TDiagramConceptList.Extract(AItem: TDiagramConcept): TDiagramConcept;
begin
  Result := TDiagramConcept(inherited Extract(AItem));
end;  // TDiagramConceptList.Extract 

{-------------------------------------------------------------------------------
}
function TDiagramConceptList.First: TDiagramConcept;
begin
  Result := TDiagramConcept(inherited First);
end;  // TDiagramConceptList.First 

{-------------------------------------------------------------------------------
  Removes a concept from the list and frees it 
}
procedure TDiagramConceptList.FreeAndDelete(AIndex: integer);
begin
  Items[AIndex].Free;
  Delete(AIndex);
end;  // TDiagramConceptList.FreeAndDelete 

{-------------------------------------------------------------------------------
}
function TDiagramConceptList.GetItems(AIndex: Integer): TDiagramConcept;
begin
  Result := TDiagramConcept(inherited Items[AIndex]);
end;  // TDiagramConceptList.GetItems 

{-------------------------------------------------------------------------------
}
class function TDiagramConceptList.GetXMLElementName: string;
begin
  Result := EL_CONCEPTS;
end;  // TDiagramConceptList.GetXMLElementName 

{-------------------------------------------------------------------------------
}
function TDiagramConceptList.IndexOf(AItem: TDiagramConcept): Integer;
begin
  Result := inherited IndexOf(Pointer(AItem));
end;  // TDiagramConceptList.IndexOf 

{-------------------------------------------------------------------------------
}
procedure TDiagramConceptList.Insert(AIndex: Integer; AItem: TDiagramConcept);
begin
  inherited Insert(AIndex, Pointer(AItem));
end;  // TDiagramConceptList.Insert 

{-------------------------------------------------------------------------------
}
function TDiagramConceptList.Last: TDiagramConcept;
begin
  Result := TDiagramConcept(inherited Last);
end;  // TDiagramConceptList.Last 

{-------------------------------------------------------------------------------
  Read object state from XML.  Contents of node must be a list of concepts. 
}
procedure TDiagramConceptList.ReadXML(AXMLNode: IXMLNode);
var
  lIdx: Integer;
begin
  dmGeneral.Recorder.RecorderMainForm.StatusText := ResStr_LoadingConcepts;
  try
    for lIdx := 0 to AXMLNode.ChildNodes.Count-1 do begin
      if AXMLNode.ChildNodes[lIdx].NodeName=EL_CONCEPT then begin
        // Note that constructing a concept automatically adds it to the list.
        with TDiagramConcept.Create(FDiagram) do
          ReadXML(AXMLNode.ChildNodes[lIdx]);
      end;
      dmGeneral.Recorder.RecorderMainForm.Progress := lIdx * 100 div 
          AXMLNode.ChildNodes.Count;
    end;
  finally
    dmGeneral.Recorder.RecorderMainForm.Progress := 0;
  end;
end;  // TDiagramConceptList.ReadXML 

{-------------------------------------------------------------------------------
}
function TDiagramConceptList.Remove(AItem: TDiagramConcept): Integer;
begin
  Result := inherited Remove(Pointer(AItem));
end;  // TDiagramConceptList.Remove 

{-------------------------------------------------------------------------------
}
procedure TDiagramConceptList.SetItems(AIndex: Integer; AItem: TDiagramConcept);
begin
  inherited Items[AIndex] := Pointer(AItem);
end;  // TDiagramConceptList.SetItems 

{-==============================================================================
    TDiagramConceptGroup
===============================================================================}
{-------------------------------------------------------------------------------
}
constructor TDiagramConceptGroup.Create;
begin
  inherited Create;
  
  FDisplayProperties := TConceptDisplayProperties.Create;
end;  // TDiagramConceptGroup.Create 

{-------------------------------------------------------------------------------
}
destructor TDiagramConceptGroup.Destroy;
begin
  FDisplayProperties.Free;
  
  inherited Destroy;
end;  // TDiagramConceptGroup.Destroy 

{-------------------------------------------------------------------------------
}
procedure TDiagramConceptGroup.ReadXML(AXMLNode: IXMLNode);
begin
  ConceptGroupKey := AXMLNode.Attributes[AT_CONCEPTGROUPKEY];
  FDisplayProperties.ReadXML(AXMLNode.ChildNodes[EL_CONCEPTDISPLAY]);
end;  // TDiagramConceptGroup.ReadXML 

{-------------------------------------------------------------------------------
}
procedure TDiagramConceptGroup.SetConceptGroupKey(const Value: string);
begin
  if FConceptGroupKey <> Value then
  begin
  
  
    FConceptGroupKey := Value;
  
    // Find out the caption
    with dmGeneral.GetRecordset('usp_ConceptGroup_Select', ['@Key',
        FConceptGroupKey]) do
      if not EOF then
        FCaption := Fields['Item_Name'].Value;
  
  end;
end;  // TDiagramConceptGroup.SetConceptGroupKey 

{-------------------------------------------------------------------------------
  Generate the XML required to persist this information to a file. 
}
procedure TDiagramConceptGroup.WriteXML(AXMLNode: IXMLNode);
var
  lNode: IXMLNode;
begin
  lNode := AXMLNode.AddChild(EL_CONCEPTGROUP);
  lNode.Attributes[AT_CONCEPTGROUPKEY] := FConceptGroupKey;
  if FDisplayProperties.HasOverride then
    FDisplayProperties.WriteXML(lNode);
end;  // TDiagramConceptGroup.WriteXML 

{-==============================================================================
    TDiagramRelationType
===============================================================================}
{-------------------------------------------------------------------------------
  Initialise object.  Takes a relation type key, and a pointer to the diagram's 
      default relationship display properties. 
}
constructor TDiagramRelationType.Create(const AKey, ACaption: string; 
    ADiagramProperties: TRelationshipDisplayProperties);
begin
  inherited Create;
  
  FRelationTypeKey := AKey;
  FCaption := ACaption;
  FDisplayProperties := TRelationshipDisplayProperties.Create;
  FDisplayProperties.Parent := ADiagramProperties;
end;  // TDiagramRelationType.Create 

{-------------------------------------------------------------------------------
}
destructor TDiagramRelationType.Destroy;
begin
  FDisplayProperties.Free;
  
  inherited Destroy;
end;  // TDiagramRelationType.Destroy 

{-------------------------------------------------------------------------------
  Reads the display properties (including those for contained relationships) 
      from the XML. 
}
procedure TDiagramRelationType.ReadXML(AXMLNode: IXMLNode);
begin
  FDisplayProperties.ReadXML(AXMLNode.ChildNodes[EL_RELATIONSHIPDISPLAY]);
end;  // TDiagramRelationType.ReadXML 

{-------------------------------------------------------------------------------
  Converts a relation type as a string ('concept', 'meaning' or 'termversion') 
      to the enumeration. 
}
class function TDiagramRelationType.RelationTypeStringToEnumeration(const 
    AType: string): TRelationType;
begin
  if CompareText(AType, 'concept')=0 then
    Result := rtConcept
  else if CompareText(AType, 'meaning')=0 then
    Result := rtMeaning
  else
    Result := rtTermVersion;
end;  // TDiagramRelationType.RelationTypeStringToEnumeration 

{-------------------------------------------------------------------------------
  Accessor method. 
}
procedure TDiagramRelationType.SetRelationType(Value: TRelationType);
begin
  FRelationType := Value;
end;  // TDiagramRelationType.SetRelationType 

{-------------------------------------------------------------------------------
}
procedure TDiagramRelationType.SetRelationTypeKey(const Value: string);
begin
  FRelationTypeKey := Value;
end;  // TDiagramRelationType.SetRelationTypeKey 

{-------------------------------------------------------------------------------
}
procedure TDiagramRelationType.WriteXML(AXMLNode: IXMLNode);
var
  lNode: IXMLNode;
begin
  lNode := AXMLNode.AddChild(EL_RELATIONTYPE);
  lNode.Attributes[AT_RELATIONTYPEKEY] := FRelationTypeKey;
  lNode.Attributes[AT_NAME] := FCaption;
  if FDisplayProperties.HasOverride then
    FDisplayProperties.WriteXML(lNode);
end;  // TDiagramRelationType.WriteXML 

{-==============================================================================
    TDiagramRelationTypeList
===============================================================================}
{-------------------------------------------------------------------------------
}
destructor TDiagramRelationTypeList.Destroy;
begin
  Clear;
  inherited Destroy;
end;  // TDiagramRelationTypeList.Destroy 

{-------------------------------------------------------------------------------
}
function TDiagramRelationTypeList.Add(AItem: TDiagramRelationType): Integer;
begin
  Result := inherited Add(Pointer(AItem));
end;  // TDiagramRelationTypeList.Add 

{-------------------------------------------------------------------------------
}
procedure TDiagramRelationTypeList.Clear;
var
  I: Integer;
begin
  for I := Count - 1 downto 0 do begin
      Items[I].Free;
      Delete(I);
  end;
  
  inherited Clear;
end;  // TDiagramRelationTypeList.Clear 

{-------------------------------------------------------------------------------
}
function TDiagramRelationTypeList.Extract(AItem: TDiagramRelationType): 
    TDiagramRelationType;
begin
  Result := TDiagramRelationType(inherited Extract(AItem));
end;  // TDiagramRelationTypeList.Extract 

{-------------------------------------------------------------------------------
  Locates a relationship type by key.  If none found, then creates one. 
}
function TDiagramRelationTypeList.FindRelationType(const ATypeKey, ACaption: 
    string): TDiagramRelationType;
var
  lIdx: Integer;
begin
  Result := nil;
  for lIdx := 0 to Count-1 do
    if Items[lIdx].RelationTypeKey = ATypeKey then begin
      Result := Items[lIdx];
      Break;
    end;
  if not Assigned(Result) then begin
    Result := TDiagramRelationType.Create(ATypeKey, ACaption, TThesaurusDiagram(
            FDiagram).ThesaurusDiagramDisplayProperties.RelationshipDisplayProperties);
    Result.DisplayProperties.ParentLabelSettings := True;
    Result.DisplayProperties.ParentFont := True;
    Result.DisplayProperties.ParentPen := True;
    TThesaurusDiagram(FDiagram).ReadSettings(Result);
    Add(Result);
  end;
end;  // TDiagramRelationTypeList.FindRelationType 

{-------------------------------------------------------------------------------
}
function TDiagramRelationTypeList.First: TDiagramRelationType;
begin
  Result := TDiagramRelationType(inherited First);
end;  // TDiagramRelationTypeList.First 

{-------------------------------------------------------------------------------
}
function TDiagramRelationTypeList.GetItems(AIndex: Integer): 
    TDiagramRelationType;
begin
  Result := TDiagramRelationType(inherited Items[AIndex]);
end;  // TDiagramRelationTypeList.GetItems 

{-------------------------------------------------------------------------------
}
class function TDiagramRelationTypeList.GetXMLElementName: string;
begin
  Result := EL_RELATIONTYPES;
end;  // TDiagramRelationTypeList.GetXMLElementName 

{-------------------------------------------------------------------------------
}
function TDiagramRelationTypeList.IndexOf(AItem: TDiagramRelationType): Integer;
begin
  Result := inherited IndexOf(Pointer(AItem));
end;  // TDiagramRelationTypeList.IndexOf 

{-------------------------------------------------------------------------------
}
procedure TDiagramRelationTypeList.Insert(AIndex: Integer; AItem: 
    TDiagramRelationType);
begin
  inherited Insert(AIndex, Pointer(AItem));
end;  // TDiagramRelationTypeList.Insert 

{-------------------------------------------------------------------------------
}
function TDiagramRelationTypeList.Last: TDiagramRelationType;
begin
  Result := TDiagramRelationType(inherited Last);
end;  // TDiagramRelationTypeList.Last 

{-------------------------------------------------------------------------------
  Read the relationship type list from the XML. 
}
procedure TDiagramRelationTypeList.ReadXML(AXMLNode: IXMLNode);
var
  lIdx: Integer;
  lRelationType: TDiagramRelationType;
begin
  for lIdx := 0 to AXMLNode.ChildNodes.Count-1 do
    if AXMLNode.ChildNodes[lIdx].NodeName = EL_RELATIONTYPE then begin
      lRelationType := FindRelationType(
          AXMLNode.ChildNodes[lIdx].Attributes[AT_RELATIONTYPEKEY],
          AXMLNode.ChildNodes[lIdx].Attributes[AT_NAME]);
      lRelationType.ReadXML(AXMLNode.ChildNodes[lIdx]);
  end;
end;  // TDiagramRelationTypeList.ReadXML 

{-------------------------------------------------------------------------------
}
function TDiagramRelationTypeList.Remove(AItem: TDiagramRelationType): Integer;
begin
  Result := inherited Remove(Pointer(AItem));
end;  // TDiagramRelationTypeList.Remove 

{-------------------------------------------------------------------------------
}
procedure TDiagramRelationTypeList.SetItems(AIndex: Integer; AItem: 
    TDiagramRelationType);
begin
  inherited Items[AIndex] := Pointer(AItem);
end;  // TDiagramRelationTypeList.SetItems 

{-==============================================================================
    TDiagramConceptGroupList
===============================================================================}
{-------------------------------------------------------------------------------
}
destructor TDiagramConceptGroupList.Destroy;
begin
  Clear;
  inherited Destroy;
end;  // TDiagramConceptGroupList.Destroy 

{-------------------------------------------------------------------------------
}
function TDiagramConceptGroupList.Add(AItem: TDiagramConceptGroup): Integer;
begin
  Result := inherited Add(Pointer(AItem));
end;  // TDiagramConceptGroupList.Add 

{-------------------------------------------------------------------------------
}
procedure TDiagramConceptGroupList.Clear;
var
  I: Integer;
begin
  for I := Count - 1 downto 0 do begin
      Items[I].Free;
      Delete(I);
  end;
  
  inherited Clear;
end;  // TDiagramConceptGroupList.Clear 

{-------------------------------------------------------------------------------
}
function TDiagramConceptGroupList.Extract(AItem: TDiagramConceptGroup): 
    TDiagramConceptGroup;
begin
  Result := TDiagramConceptGroup(inherited Extract(AItem));
end;  // TDiagramConceptGroupList.Extract 

{-------------------------------------------------------------------------------
}
function TDiagramConceptGroupList.First: TDiagramConceptGroup;
begin
  Result := TDiagramConceptGroup(inherited First);
end;  // TDiagramConceptGroupList.First 

{-------------------------------------------------------------------------------
}
function TDiagramConceptGroupList.GetItems(AIndex: Integer): 
    TDiagramConceptGroup;
begin
  Result := TDiagramConceptGroup(inherited Items[AIndex]);
end;  // TDiagramConceptGroupList.GetItems 

{-------------------------------------------------------------------------------
}
class function TDiagramConceptGroupList.GetXMLElementName: string;
begin
  Result := EL_CONCEPTGROUPS;
end;  // TDiagramConceptGroupList.GetXMLElementName 

{-------------------------------------------------------------------------------
}
function TDiagramConceptGroupList.IndexOf(AItem: TDiagramConceptGroup): Integer;
begin
  Result := inherited IndexOf(Pointer(AItem));
end;  // TDiagramConceptGroupList.IndexOf 

{-------------------------------------------------------------------------------
}
procedure TDiagramConceptGroupList.Insert(AIndex: Integer; AItem: 
    TDiagramConceptGroup);
begin
  inherited Insert(AIndex, Pointer(AItem));
end;  // TDiagramConceptGroupList.Insert 

{-------------------------------------------------------------------------------
}
function TDiagramConceptGroupList.Last: TDiagramConceptGroup;
begin
  Result := TDiagramConceptGroup(inherited Last);
end;  // TDiagramConceptGroupList.Last 

{-------------------------------------------------------------------------------
}
procedure TDiagramConceptGroupList.ReadXML(AXMLNode: IXMLNode);
var
  lIdx: Integer;
  lNewGroup: TDiagramConceptGroup;
begin
  for lIdx := 0 to AXMLNode.ChildNodes.Count-1 do
    if AXMLNode.ChildNodes[lIdx].NodeName=EL_CONCEPTGROUP then begin
      lNewGroup := TDiagramConceptGroup.Create;
      lNewGroup.ReadXML(AXMLNode.ChildNodes[lIdx]);
      lNewGroup.DisplayProperties.Parent := TThesaurusDiagram(FDiagram).
          ThesaurusDiagramDisplayProperties.ConceptDisplayProperties;
      Add(lNewGroup);
    end; // if
end;  // TDiagramConceptGroupList.ReadXML 

{-------------------------------------------------------------------------------
}
function TDiagramConceptGroupList.Remove(AItem: TDiagramConceptGroup): Integer;
begin
  Result := inherited Remove(Pointer(AItem));
end;  // TDiagramConceptGroupList.Remove 

{-------------------------------------------------------------------------------
}
procedure TDiagramConceptGroupList.SetItems(AIndex: Integer; AItem: 
    TDiagramConceptGroup);
begin
  inherited Items[AIndex] := Pointer(AItem);
end;  // TDiagramConceptGroupList.SetItems 


end.
