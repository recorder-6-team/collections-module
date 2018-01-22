{===============================================================================
  Unit:        ConceptDetailsNodes

  Defines:

  Description: Fly node classes for the tree that details a single concept in
               the Concept Organiser concept details screen.

  Created:     Dec 2003

  Last revision information:
    $Revision: 25 $
    $Date: 17/11/11 16:37 $
    $Author: Jamesbichard $

===============================================================================}
unit ConceptDetailsNodes;

interface

uses
  Sysutils, Classes, RapTree, TreeColl, DataTypes, BaseDetailFrameUnit,
  DataClasses, Variants, AdoDB, ExceptionForm;

const
  PROP_THISTERMVERSION = 'ThisTermVersion';

type
  EConceptDetailsNodes = class(TExceptionPath);
  
  {-----------------------------------------------------------------------------
    Base class for nodes in the Concept Details hierarchy.
  }
  TBaseConceptDetailsNode = class(TFlyNode, IAdditionalProperties)
  private
    function GetInternalImageIndex: Integer; virtual; abstract;
    function GetProperty(const AName: string): Variant;
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  protected
    function GetDetailsFrameClass: TBaseDetailFrameClass; virtual;
    function GetKey: string; virtual; abstract;
    function GetTableName: string; virtual;
    function GetTopLevelNode: TFlyNode; virtual;
    function InternalGetProperty(const AName: String): Variant; virtual;
    procedure SetKey(const Value: string); virtual; abstract;
  public
    constructor Create(AOwner : TTreeCollection); override;
    function CanAdd: Boolean; virtual;
    function CanDelete: Boolean; virtual; abstract;
    function CanEdit: Boolean; virtual; abstract;
    class function ClassTableName: string; virtual; abstract;
    function GetProcParams: TVariantArray; virtual;
    property DetailsFrameClass: TBaseDetailFrameClass read GetDetailsFrameClass;
    property InternalImageIndex: Integer read GetInternalImageIndex;
    property Key: string read GetKey write SetKey;
    property TableName: string read GetTableName;
    property TopLevelNode: TFlyNode read GetTopLevelNode;
  end;
  
  {-----------------------------------------------------------------------------
    Base class for structural nodes in the Concept Details hierarchy.
  }
  TConceptDetailsStructuralNode = class(TBaseConceptDetailsNode)
  protected
    function GetChildNodeType: TFlyNodeClass; virtual; abstract;
    function GetChildrenStoredProc: string; virtual; abstract;
  public
    function CanDelete: Boolean; override;
    function CanEdit: Boolean; override;
    property ChildNodeType: TFlyNodeClass read GetChildNodeType;
    property ChildrenStoredProc: string read GetChildrenStoredProc;
  end;
  
  TConceptDetailsTopNode = class(TConceptDetailsStructuralNode)
  private
    FConceptGroupKey: TKeyString;
    FConceptGroupVersionKey: TKeyString;
    FKey: string;
    FMeaningKey: TKeyString;
    FTermVersionKey: TKeyString;
    function GetInternalImageIndex: Integer; override;
    procedure SetMeaningKey(Value: TKeyString);
    procedure SetTermVersionKey(Value: TKeyString);
  protected
    function GetChildNodeType: TFlyNodeClass; override;
    function GetChildrenStoredProc: string; override;
    function GetDetailsFrameClass: TBaseDetailFrameClass; override;
    function GetKey: string; override;
    function InternalGetProperty(const AName: String): Variant; override;
    procedure SetKey(const Value: string); override;
  public
    function CanAdd: Boolean; override;
    function CanEdit: Boolean; override;
    class function ClassTableName: string; override;
    property ConceptGroupKey: TKeyString read FConceptGroupKey write
            FConceptGroupKey;
    property ConceptGroupVersionKey: TKeyString read FConceptGroupVersionKey
            write FConceptGroupVersionKey;
    property MeaningKey: TKeyString read FMeaningKey write SetMeaningKey;
    property TermVersionKey: TKeyString read FTermVersionKey write
            SetTermVersionKey;
  end;
  
  TConceptDetailsFolderNode = class(TConceptDetailsStructuralNode)
  protected
    function GetKey: string; override;
    procedure SetKey(const Value: string); override;
  end;
  
  TSynonymyListNode = class(TConceptDetailsFolderNode)
  end;
  
  TListSynonymyListNode = class(TSynonymyListNode)
  private
    function GetInternalImageIndex: Integer; override;
  protected
    function GetChildNodeType: TFlyNodeClass; override;
    function GetChildrenStoredProc: string; override;
  public
    function CanAdd: Boolean; override;
  end;

  TAllSynonymyListNode = class(TSynonymyListNode)
  private
    function GetInternalImageIndex: Integer; override;
  protected
    function GetChildNodeType: TFlyNodeClass; override;
    function GetChildrenStoredProc: string; override;
  public
    function CanAdd: Boolean; override;
  end;
  
  TPotentialSynonymsListNode = class(TSynonymyListNode)
  private
    function GetInternalImageIndex: Integer; override;
  protected
    function GetChildNodeType: TFlyNodeClass; override;
    function GetChildrenStoredProc: string; override;
  public
    function CanAdd: Boolean; override;
  end;

  THomonymsListNode = class(TSynonymyListNode)
  private
    function GetInternalImageIndex: Integer; override;
  protected
    function GetChildNodeType: TFlyNodeClass; override;
    function GetChildrenStoredProc: string; override;
  public
    function CanAdd: Boolean; override;
  end;
  
  TConceptHistoryListNode = class(TConceptDetailsFolderNode)
  private
    function GetInternalImageIndex: Integer; override;
  protected
    function GetChildNodeType: TFlyNodeClass; override;
    function GetChildrenStoredProc: string; override;
  end;
  
  TDesignationsListNode = class(TConceptDetailsFolderNode)
  private
    function GetInternalImageIndex: Integer; override;
  protected
    function GetChildNodeType: TFlyNodeClass; override;
    function GetChildrenStoredProc: string; override;
  end;
  
  TFactsListNode = class(TConceptDetailsFolderNode)
  private
    function GetInternalImageIndex: Integer; override;
  protected
    function GetChildNodeType: TFlyNodeClass; override;
    function GetChildrenStoredProc: string; override;
  end;
  
  TParentsListNode = class(TConceptDetailsFolderNode)
  private
    function GetInternalImageIndex: Integer; override;
  protected
    function GetChildNodeType: TFlyNodeClass; override;
    function GetChildrenStoredProc: string; override;
  public
    function CanAdd: Boolean; override;
  end;
  
  TRelationshipsListNode = class(TConceptDetailsFolderNode)
  private
    function GetInternalImageIndex: Integer; override;
  protected
    function GetChildNodeType: TFlyNodeClass; override;
    function GetChildrenStoredProc: string; override;
  end;
  
  TTermVersionsListNode = class(TConceptDetailsFolderNode)
  private
    function GetInternalImageIndex: Integer; override;
  protected
    function GetChildNodeType: TFlyNodeClass; override;
    function GetChildrenStoredProc: string; override;
  public
    function CanAdd: Boolean; override;
    function GetProcParams: TVariantArray; override;
  end;
  
  TConceptDetailsItemNode = class(TBaseConceptDetailsNode,
          IAdditionalProperties)
  private
    FKey: string;
  protected
    function GetDeleteStoredProc: string; virtual; abstract;
    function GetKey: string; override;
    procedure SetKey(const Value: string); override;
  public
    function CanDelete: Boolean; override;
    function CanEdit: Boolean; override;
    procedure Initialise(ARecordset: _Recordset); virtual;
    property DeleteStoredProc: string read GetDeleteStoredProc;
  end;
  
  {-----------------------------------------------------------------------------
    Node type for a designation attached to a concept.
  }
  TDesignationNode = class(TConceptDetailsItemNode)
  private
    function GetInternalImageIndex: Integer; override;
  protected
    function GetDeleteStoredProc: string; override;
    function GetDetailsFrameClass: TBaseDetailFrameClass; override;
    function InternalGetProperty(const AName: String): Variant; override;
  public
    class function ClassTableName: string; override;
  end;

  TConceptHistoryNode = class(TConceptDetailsItemNode)
  private
    function GetInternalImageIndex: Integer; override;
  protected
    function GetDeleteStoredProc: string; override;
    function GetDetailsFrameClass: TBaseDetailFrameClass; override;
  public
    class function ClassTableName: string; override;
  end;

  TFactNode = class(TConceptDetailsItemNode)
  private
    function GetInternalImageIndex: Integer; override;
  protected
    function GetDeleteStoredProc: string; override;
    function GetDetailsFrameClass: TBaseDetailFrameClass; override;
    function InternalGetProperty(const AName: String): Variant; override;
  public
    class function ClassTableName: string; override;
  end;
  
  TRelationshipNode = class(TConceptDetailsItemNode)
  private
    FAppliesTo: TAppliesTo;
    function GetInternalImageIndex: Integer; override;
  protected
    function GetDeleteStoredProc: string; override;
    function GetDetailsFrameClass: TBaseDetailFrameClass; override;
    function GetTableName: string; override;
    function InternalGetProperty(const AName: String): Variant; override;
  public
    procedure Initialise(ARecordset: _Recordset); override;
    property AppliesTo: TAppliesTo read FAppliesTo write FAppliesTo;
  end;
  
  TSynonymNode = class(TConceptDetailsItemNode)
  private
    function GetInternalImageIndex: Integer; override;
  protected
    function GetDeleteStoredProc: string; override;
    function GetDetailsFrameClass: TBaseDetailFrameClass; override;
    function InternalGetProperty(const AName: String): Variant; override;
  public
    function CanAdd: Boolean; override;
    function CanEdit: Boolean; override;
    class function ClassTableName: string; override;
  end;
  
  {-----------------------------------------------------------------------------
    Can delete only the 'This Term Version' node.
  }
  TTermVersionNode = class(TConceptDetailsItemNode)
  private
    FIsThisVersion: Boolean;
    function GetInternalImageIndex: Integer; override;
  protected
    function GetDeleteStoredProc: string; override;
    function GetDetailsFrameClass: TBaseDetailFrameClass; override;
    function InternalGetProperty(const AName: String): Variant; override;
  public
    function CanAdd: Boolean; override;
    function CanDelete: Boolean; override;
    function CanEdit: Boolean; override;
    class function ClassTableName: string; override;
    procedure Initialise(ARecordset: _Recordset); override;
    property IsThisVersion: Boolean read FIsThisVersion write FIsThisVersion;
  end;
  
  TParentNode = class(TConceptDetailsItemNode)
  private
    function GetInternalImageIndex: Integer; override;
  protected
    function GetDeleteStoredProc: string; override;
    function GetDetailsFrameClass: TBaseDetailFrameClass; override;
    function InternalGetProperty(const AName: String): Variant; override;
  public
    constructor Create(AOwner : TTreeCollection); override;
    function CanAdd: Boolean; override;
    function CanEdit: Boolean; override;
    class function ClassTableName: string; override;
  end;
  
  TListSynonymNode = class(TSynonymNode, IAdditionalProperties)
  public
    function CanEdit: Boolean; override;
  end;
  
  TAllSynonymNode = class(TSynonymNode, IAdditionalProperties)
  private
    FConceptGroupKey: String;
  public
    constructor Create(AOwner : TTreeCollection); override;
    procedure Initialise(ARecordset: _Recordset); override;
    property ConceptGroupKey: String read FConceptGroupKey;
  end;
  
  TPotentialSynonymNode = class(TSynonymNode, IAdditionalProperties)
  public
    constructor Create(AOwner : TTreeCollection); override;
    function CanDelete: Boolean; override;
  end;

  THomonymNode = class(TSynonymNode, IAdditionalProperties)
  public
    function GetDeleteStoredProc: String; override;
  end;

//==============================================================================
implementation

uses
  LuxembourgConstants, Relationship, Fact, Designation, TermVersion,
  ConceptHistory, Synonym, Concept, ResourceStrings;

{-==============================================================================
    TBaseConceptDetailsNode
===============================================================================}
{-------------------------------------------------------------------------------
}
constructor TBaseConceptDetailsNode.Create(AOwner : TTreeCollection);
begin
  inherited Create(AOwner);

  ImageIndex := InternalImageIndex;
  SelectedIndex := InternalImageIndex;
end;  // TBaseConceptDetailsNode.Create 

{-------------------------------------------------------------------------------
}
function TBaseConceptDetailsNode.CanAdd: Boolean;
begin
  Result := True;
end;  // TBaseConceptDetailsNode.CanAdd 

{-------------------------------------------------------------------------------
  Virtual - derived classes implement this to specify the frame that is loaded
          when they are selected.  Not abstract so default behaviour can return
          no frame.
}
function TBaseConceptDetailsNode.GetDetailsFrameClass: TBaseDetailFrameClass;
begin
  Result := nil;
end;  // TBaseConceptDetailsNode.GetDetailsFrameClass 

{-------------------------------------------------------------------------------
}
function TBaseConceptDetailsNode.GetProcParams: TVariantArray;
begin
  Result := VarArrayOf(['@Key', Key]);
end;  // TBaseConceptDetailsNode.GetProcParams 

{-------------------------------------------------------------------------------
  As TBaseConceptDetailsNode implements IAdditionalProperties, the GetProperty
          method is required. It is used to get the Key and ParentKey (
          ParentKey is null for this type of node).
}
function TBaseConceptDetailsNode.GetProperty(const AName: string): Variant;
begin
  Result := InternalGetProperty(AName);
end;  // TBaseConceptDetailsNode.GetProperty 

{-------------------------------------------------------------------------------
}
function TBaseConceptDetailsNode.GetTableName: string;
begin
  Result := ClassTableName;
end;  // TBaseConceptDetailsNode.GetTableName 

{-------------------------------------------------------------------------------
}
function TBaseConceptDetailsNode.GetTopLevelNode: TFlyNode;
var
  lCurrentNode: TFlyNode;
begin
  lCurrentNode := self;
  
  while Assigned(lCurrentNode.Parent) do
    lCurrentNode := lCurrentNode.Parent;
  Result := lCurrentNode;
end;  // TBaseConceptDetailsNode.GetTopLevelNode 

{-------------------------------------------------------------------------------
}
function TBaseConceptDetailsNode.InternalGetProperty(const AName: String):
        Variant;
begin
  if AName = PROP_KEY then
    Result := Key
  else if AName = PROP_TABLE_NAME then
    Result := TableName
  else if AName = PROP_PARENT_KEY then
    if Assigned(TopLevelNode) then
      if TopLevelNode is TBaseConceptDetailsNode then
        Result := TBaseConceptDetailsNode(TopLevelNode).Key
  else
    Result := Unassigned;
end;  // TBaseConceptDetailsNode.InternalGetProperty 

{-------------------------------------------------------------------------------
}
function TBaseConceptDetailsNode.QueryInterface(const IID: TGUID; out Obj):
        HResult;
begin
  if GetInterface(IID, Obj) then
    Result := 0
  else
    Result := E_NOINTERFACE;
end;  // TBaseConceptDetailsNode.QueryInterface 

{-------------------------------------------------------------------------------
}
function TBaseConceptDetailsNode._AddRef: Integer;
begin
  Result := -1; //Don't worry about reference counting
end;  // TBaseConceptDetailsNode._AddRef 

{-------------------------------------------------------------------------------
}
function TBaseConceptDetailsNode._Release: Integer;
begin
  Result := -1; //Don't worry about reference counting
end;  // TBaseConceptDetailsNode._Release 


{-==============================================================================
    TConceptDetailsTopNode
===============================================================================}
{-------------------------------------------------------------------------------
}
function TConceptDetailsTopNode.CanAdd: Boolean;
begin
  Result := False;
end;  // TConceptDetailsTopNode.CanAdd 

{-------------------------------------------------------------------------------
}
function TConceptDetailsTopNode.CanEdit: Boolean;
begin
  Result := True;
end;  // TConceptDetailsTopNode.CanEdit 

{-------------------------------------------------------------------------------
}
class function TConceptDetailsTopNode.ClassTableName: string;
begin
  Result := TN_CONCEPT;
end;  // TConceptDetailsTopNode.ClassTableName 

{-------------------------------------------------------------------------------
  GetChildNodeType returns nil as this only ever contains pre-determined
          structural nodes.
}
function TConceptDetailsTopNode.GetChildNodeType: TFlyNodeClass;
begin
  Result := nil;
end;  // TConceptDetailsTopNode.GetChildNodeType 

{-------------------------------------------------------------------------------
  No stored procedure required to obtain children. 
}
function TConceptDetailsTopNode.GetChildrenStoredProc: string;
begin
  Result := '';
end;  // TConceptDetailsTopNode.GetChildrenStoredProc 

{-------------------------------------------------------------------------------
  Identify the Designations screen as the details frame. 
}
function TConceptDetailsTopNode.GetDetailsFrameClass: TBaseDetailFrameClass;
begin
  Result := TfraConcept;
end;  // TConceptDetailsTopNode.GetDetailsFrameClass 

{-------------------------------------------------------------------------------
}
function TConceptDetailsTopNode.GetInternalImageIndex: Integer;
begin
  Result := 0;
end;  // TConceptDetailsTopNode.GetInternalImageIndex 

{-------------------------------------------------------------------------------
  Accessor method override. 
}
function TConceptDetailsTopNode.GetKey: string;
begin
  Result := FKey;
end;  // TConceptDetailsTopNode.GetKey 

{-------------------------------------------------------------------------------
}
function TConceptDetailsTopNode.InternalGetProperty(const AName: String):
        Variant;
begin
  Result := inherited InternalGetProperty(AName);
  
  if Result = Unassigned then begin
    if AName = PROP_CONCEPT_GROUP_KEY then
      Result := FConceptGroupKey
    else if AName = PROP_CONCEPT_GROUP_VERSION_KEY then
      Result := FConceptGroupVersionKey;
  end;
end;  // TConceptDetailsTopNode.InternalGetProperty 

{-------------------------------------------------------------------------------
  Accessor method override 
}
procedure TConceptDetailsTopNode.SetKey(const Value: string);
begin
  FKey := Value;
end;  // TConceptDetailsTopNode.SetKey 

{-------------------------------------------------------------------------------
}
procedure TConceptDetailsTopNode.SetMeaningKey(Value: TKeyString);
begin
  FMeaningKey := Value;
end;  // TConceptDetailsTopNode.SetMeaningKey 

{-------------------------------------------------------------------------------
}
procedure TConceptDetailsTopNode.SetTermVersionKey(Value: TKeyString);
begin
  FTermVersionKey := Value;
end;  // TConceptDetailsTopNode.SetTermVersionKey 

{-==============================================================================
    TConceptDetailsFolderNode
===============================================================================}
{-------------------------------------------------------------------------------
  Accessor method override.  Folder nodes use the top level node to obtain
          their key values.
}
function TConceptDetailsFolderNode.GetKey: string;
begin
  if not (Parent is TConceptDetailsTopNode) then
    raise EConceptDetailsNodes.Create(Format(ResStr_InvalidMethodCall,
      ['TConceptDetailsFolderNode.GetKey']));
  Result := TConceptDetailsTopNode(Parent).Key;
end;  // TConceptDetailsFolderNode.GetKey 

{-------------------------------------------------------------------------------
  Accessor override.  The key should never be set on a folder node as it is
          obtained from the top node.
}
procedure TConceptDetailsFolderNode.SetKey(const Value: string);
begin  
  raise EConceptDetailsNodes.Create(Format(ResStr_InvalidMethodCall,
      ['TConceptDetailsFolderNode.SetKey']));
end;  // TConceptDetailsFolderNode.SetKey



{-==============================================================================
    TListSynonymyListNode
===============================================================================}
{-------------------------------------------------------------------------------
}
function TListSynonymyListNode.CanAdd: Boolean;
begin
  Result := False;
end;  // TListSynonymyListNode.CanAdd 

{-------------------------------------------------------------------------------
}
function TListSynonymyListNode.GetChildNodeType: TFlyNodeClass;
begin
  Result := TListSynonymNode;
end;  // TListSynonymyListNode.GetChildNodeType 

{-------------------------------------------------------------------------------
}
function TListSynonymyListNode.GetChildrenStoredProc: string;
begin
  Result := 'usp_ListSynonyms_Select_ForConcept';
end;  // TListSynonymyListNode.GetChildrenStoredProc 

{-------------------------------------------------------------------------------
}
function TListSynonymyListNode.GetInternalImageIndex: Integer;
begin
  Result := 8;
end;  // TListSynonymyListNode.GetInternalImageIndex 


{-==============================================================================
    TAllSynonymyListNode
===============================================================================}
{-------------------------------------------------------------------------------
}
function TAllSynonymyListNode.CanAdd: Boolean;
begin
  Result := False;
end;  // TAllSynonymyListNode.CanAdd 

{-------------------------------------------------------------------------------
}
function TAllSynonymyListNode.GetChildNodeType: TFlyNodeClass;
begin
  Result := TAllSynonymNode;
end;  // TAllSynonymyListNode.GetChildNodeType 

{-------------------------------------------------------------------------------
}
function TAllSynonymyListNode.GetChildrenStoredProc: string;
begin
  Result := 'usp_AllSynonyms_Select_ForConcept';
end;  // TAllSynonymyListNode.GetChildrenStoredProc 

{-------------------------------------------------------------------------------
}
function TAllSynonymyListNode.GetInternalImageIndex: Integer;
begin
  Result := 8;
end;  // TAllSynonymyListNode.GetInternalImageIndex 

{-==============================================================================
    TPotentialSynonymsListNode
===============================================================================}
{-------------------------------------------------------------------------------
}
function TPotentialSynonymsListNode.CanAdd: Boolean;
begin
  Result := False;
end;  // TPotentialSynonymsListNode.CanAdd

{-------------------------------------------------------------------------------
}
function TPotentialSynonymsListNode.GetChildNodeType: TFlyNodeClass;
begin
  Result := TPotentialSynonymNode;
end;  // TPotentialSynonymsListNode.GetChildNodeType

{-------------------------------------------------------------------------------
}
function TPotentialSynonymsListNode.GetChildrenStoredProc: string;
begin
  Result := 'usp_PotentialSynonyms_Select_ForConcept';
end;  // TPotentialSynonymsListNode.GetChildrenStoredProc

{-------------------------------------------------------------------------------
}
function TPotentialSynonymsListNode.GetInternalImageIndex: Integer;
begin
  Result := 11;
end;  // TPotentialSynonymsListNode.GetInternalImageIndex

{-==============================================================================
    THomonymsListNode
===============================================================================}
{-------------------------------------------------------------------------------
}
function THomonymsListNode.CanAdd: Boolean;
begin
  Result := False;
end;  // TPotentialSynonymsListNode.CanAdd

{-------------------------------------------------------------------------------
}
function THomonymsListNode.GetChildNodeType: TFlyNodeClass;
begin
  Result := THomonymNode;
end;  // TPotentialSynonymsListNode.GetChildNodeType

{-------------------------------------------------------------------------------
}
function THomonymsListNode.GetChildrenStoredProc: string;
begin
  Result := 'usp_Concept_Select_Homonyms';
end;  // TPotentialSynonymsListNode.GetChildrenStoredProc

{-------------------------------------------------------------------------------
}
function THomonymsListNode.GetInternalImageIndex: Integer;
begin
  Result := 11;
end;  // TPotentialSynonymsListNode.GetInternalImageIndex

{-==============================================================================
    TConceptHistoryListNode
===============================================================================}
{-------------------------------------------------------------------------------
}
function TConceptHistoryListNode.GetChildNodeType: TFlyNodeClass;
begin
  Result := TConceptHistoryNode;
end;  // TConceptHistoryListNode.GetChildNodeType 

{-------------------------------------------------------------------------------
}
function TConceptHistoryListNode.GetChildrenStoredProc: string;
begin
  Result := 'usp_ConceptHistory_Select_ForConcept';
end;  // TConceptHistoryListNode.GetChildrenStoredProc 

{-------------------------------------------------------------------------------
}
function TConceptHistoryListNode.GetInternalImageIndex: Integer;
begin
  Result := 10;
end;  // TConceptHistoryListNode.GetInternalImageIndex 

{-==============================================================================
    TDesignationsListNode
===============================================================================}
{-------------------------------------------------------------------------------
}
function TDesignationsListNode.GetChildNodeType: TFlyNodeClass;
begin
  Result := TDesignationNode;
end;  // TDesignationsListNode.GetChildNodeType 

{-------------------------------------------------------------------------------
}
function TDesignationsListNode.GetChildrenStoredProc: string;
begin
  Result := 'usp_ConceptDesignations_Select_ForConcept';
end;  // TDesignationsListNode.GetChildrenStoredProc 

{-------------------------------------------------------------------------------
}
function TDesignationsListNode.GetInternalImageIndex: Integer;
begin
  Result := 5;
end;  // TDesignationsListNode.GetInternalImageIndex 

{-==============================================================================
    TFactsListNode
===============================================================================}
{-------------------------------------------------------------------------------
}
function TFactsListNode.GetChildNodeType: TFlyNodeClass;
begin
  Result := TFactNode;
end;  // TFactsListNode.GetChildNodeType 

{-------------------------------------------------------------------------------
}
function TFactsListNode.GetChildrenStoredProc: string;
begin
  Result := 'usp_ThesaurusFacts_Select_ForConcept';
end;  // TFactsListNode.GetChildrenStoredProc 

{-------------------------------------------------------------------------------
}
function TFactsListNode.GetInternalImageIndex: Integer;
begin
  Result := 3;
end;  // TFactsListNode.GetInternalImageIndex 


{-==============================================================================
    TParentsListNode
===============================================================================}
{-------------------------------------------------------------------------------
}
function TParentsListNode.CanAdd: Boolean;
begin
  Result := True;
end;  // TParentsListNode.CanAdd 

{-------------------------------------------------------------------------------
}
function TParentsListNode.GetChildNodeType: TFlyNodeClass;
begin
  Result := TParentNode;
end;  // TParentsListNode.GetChildNodeType 

{-------------------------------------------------------------------------------
}
function TParentsListNode.GetChildrenStoredProc: string;
begin
  Result := 'usp_Parents_Select_ForConcept';
end;  // TParentsListNode.GetChildrenStoredProc 

{-------------------------------------------------------------------------------
}
function TParentsListNode.GetInternalImageIndex: Integer;
begin
  Result := 12;
end;  // TParentsListNode.GetInternalImageIndex 

{-==============================================================================
    TTermVersionsListNode
===============================================================================}
{-------------------------------------------------------------------------------
}
function TTermVersionsListNode.CanAdd: Boolean;
begin
  Result := False;
end;  // TTermVersionsListNode.CanAdd 

{-------------------------------------------------------------------------------
}
function TTermVersionsListNode.GetChildNodeType: TFlyNodeClass;
begin
  Result := TTermVersionNode;
end;  // TTermVersionsListNode.GetChildNodeType 

{-------------------------------------------------------------------------------
}
function TTermVersionsListNode.GetChildrenStoredProc: string;
begin
  Result := 'usp_TermVersions_Select_ForConcept';
end;  // TTermVersionsListNode.GetChildrenStoredProc 

{-------------------------------------------------------------------------------
}
function TTermVersionsListNode.GetInternalImageIndex: Integer;
begin
  Result := 13;
end;  // TTermVersionsListNode.GetInternalImageIndex 

{-------------------------------------------------------------------------------
}
function TTermVersionsListNode.GetProcParams: TVariantArray;
begin
  Result := VarArrayOf([
      '@Key', Key,
      '@ThisTermVersionLabel', ResStr_ThisTermVersion,
      '@VersionLabel', ResStr_Version]);
end;  // TTermVersionsListNode.GetProcParams 

{-==============================================================================
    TConceptDetailsItemNode
===============================================================================}
{-------------------------------------------------------------------------------
}
function TConceptDetailsItemNode.CanDelete: Boolean;
begin
  Result := True;
end;  // TConceptDetailsItemNode.CanDelete 

{-------------------------------------------------------------------------------
}
function TConceptDetailsItemNode.CanEdit: Boolean;
begin
  Result := True;
end;  // TConceptDetailsItemNode.CanEdit 

{-------------------------------------------------------------------------------
  Accessor override 
}
function TConceptDetailsItemNode.GetKey: string;
begin
  Result := FKey;
end;  // TConceptDetailsItemNode.GetKey 

{-------------------------------------------------------------------------------
}
procedure TConceptDetailsItemNode.Initialise(ARecordset: _Recordset);
begin
  Caption  := ARecordset.Fields['Item_Name'].Value;
  Key      := VarToStr(ARecordset.Fields['Item_Key'].Value);
end;  // TConceptDetailsItemNode.Initialise 

{-------------------------------------------------------------------------------
  Accessor override 
}
procedure TConceptDetailsItemNode.SetKey(const Value: string);
begin
  FKey := Value;
end;  // TConceptDetailsItemNode.SetKey 

{-==============================================================================
    TDesignationNode
===============================================================================}
{-------------------------------------------------------------------------------
}
class function TDesignationNode.ClassTableName: string;
begin
  Result := TN_CONCEPT_DESIGNATION;
end;  // TDesignationNode.ClassTableName 

{-------------------------------------------------------------------------------
}
function TDesignationNode.GetDeleteStoredProc: string;
begin
  Result := 'usp_Designation_Delete';
end;  // TDesignationNode.GetDeleteStoredProc 

{-------------------------------------------------------------------------------
}
function TDesignationNode.GetDetailsFrameClass: TBaseDetailFrameClass;
begin
  Result := TfraDesignation;
end;  // TDesignationNode.GetDetailsFrameClass

{-------------------------------------------------------------------------------
  Return the image index for the node type.
}
function TDesignationNode.GetInternalImageIndex: Integer;
begin
  Result := 6;
end;  // TDesignationNode.GetInternalImageIndex

{-------------------------------------------------------------------------------
}
function TDesignationNode.InternalGetProperty(const AName: String): Variant;
begin
  Result := inherited InternalGetProperty(AName);
  
  if Result = Unassigned then begin
    if AName = PROP_NODE_CLASS then
      Result := Self.Classname
    else if AName = PROP_CONCEPT_GROUP_KEY then
      if TopLevelNode is TConceptDetailsTopNode then
        Result := TConceptDetailsTopNode(TopLevelNode).ConceptGroupKey
  end;
end;  // TDesignationNode.InternalGetProperty

{-==============================================================================
    TConceptHistoryNode
===============================================================================}
{-------------------------------------------------------------------------------
}
class function TConceptHistoryNode.ClassTableName: string;
begin
  Result := TN_CONCEPT_HISTORY;
end;  // TConceptHistoryNode.ClassTableName 

{-------------------------------------------------------------------------------
}
function TConceptHistoryNode.GetDeleteStoredProc: string;
begin
  Result := 'usp_ConceptHistory_Delete';
end;  // TConceptHistoryNode.GetDeleteStoredProc 

{-------------------------------------------------------------------------------
  Retrieve concept details when selected. 
}
function TConceptHistoryNode.GetDetailsFrameClass: TBaseDetailFrameClass;
begin
  Result := TfraConceptHistory;
end;  // TConceptHistoryNode.GetDetailsFrameClass 

{-------------------------------------------------------------------------------
  Return the image index for the node type. 
}
function TConceptHistoryNode.GetInternalImageIndex: Integer;
begin
  Result := 10;
end;  // TConceptHistoryNode.GetInternalImageIndex 

{-==============================================================================
    TFactNode
===============================================================================}
{-------------------------------------------------------------------------------
}
class function TFactNode.ClassTableName: string;
begin
  Result := TN_THESAURUS_FACT;
end;  // TFactNode.ClassTableName 

{-------------------------------------------------------------------------------
}
function TFactNode.GetDeleteStoredProc: string;
begin
  Result := 'usp_ThesaurusFact_Delete';
end;  // TFactNode.GetDeleteStoredProc 

{-------------------------------------------------------------------------------
}
function TFactNode.GetDetailsFrameClass: TBaseDetailFrameClass;
begin
  Result := TfraFact;
end;  // TFactNode.GetDetailsFrameClass 

{-------------------------------------------------------------------------------
  Return the image index for the node type. 
}
function TFactNode.GetInternalImageIndex: Integer;
begin
  Result := 4;
end;  // TFactNode.GetInternalImageIndex 

{-------------------------------------------------------------------------------
}
function TFactNode.InternalGetProperty(const AName: String): Variant;
var
  lTopLevelNode: TFlyNode;
begin
  Result := inherited InternalGetProperty(AName);

  if Result = Unassigned then begin
    lTopLevelNode := TopLevelNode;

    if Assigned(lTopLevelNode) then
      if lTopLevelNode is TConceptDetailsTopNode then
        with TConceptDetailsTopNode(lTopLevelNode) do begin
          if AName = PROP_MEANING_KEY then
            Result := MeaningKey
          else if AName = PROP_TERM_VERSION_KEY then
            Result := TermVersionKey
          else if AName = PROP_CONCEPT_KEY then
            Result := Key
        end // with TConceptDetailsTopNode(lTopLevelNode)
  end;
end;  // TFactNode.InternalGetProperty

{-==============================================================================
    TRelationshipNode
===============================================================================}
{-------------------------------------------------------------------------------
}
function TRelationshipNode.GetDeleteStoredProc: string;
begin
  case FAppliesTo of
    atConcept :      Result := 'usp_ConceptRelation_Delete';
    atMeaning :      Result := 'usp_MeaningRelation_Delete';
    atTermVersion :  Result := 'usp_TermVersionRelation_Delete';
  end;
end;  // TRelationshipNode.GetDeleteStoredProc 

{-------------------------------------------------------------------------------
}
function TRelationshipNode.GetDetailsFrameClass: TBaseDetailFrameClass;
begin
  Result := TfraRelationship;
end;  // TRelationshipNode.GetDetailsFrameClass 

{-------------------------------------------------------------------------------
  Return the image index for the node type. 
}
function TRelationshipNode.GetInternalImageIndex: Integer;
begin
  Result := 2;
end;  // TRelationshipNode.GetInternalImageIndex 

{-------------------------------------------------------------------------------
}
function TRelationshipNode.GetTableName: string;
begin
  case FAppliesTo of
    atConcept :      Result := TN_CONCEPT_RELATION;
    atMeaning :      Result := TN_MEANING_RELATION;
    atTermVersion :  Result := TN_TERM_VERSION_RELATION;
  end;
end;  // TRelationshipNode.GetTableName 

{-------------------------------------------------------------------------------
}
procedure TRelationshipNode.Initialise(ARecordset: _Recordset);
begin
  inherited;
  AppliesTo := ARecordset.Fields['Applies_To'].Value;
end;  // TRelationshipNode.Initialise 

{-------------------------------------------------------------------------------
}
function TRelationshipNode.InternalGetProperty(const AName: String): Variant;
var
  lTopLevelNode: TFlyNode;
begin
  Result := inherited InternalGetProperty(AName);
  
  if Result = Unassigned then begin
    if AName = PROP_RELATIONSHIP_APPLIES_TO then
      Result := FAppliesTo
    else begin
      lTopLevelNode := TopLevelNode;
  
      if Assigned(lTopLevelNode) then
        if lTopLevelNode is TConceptDetailsTopNode then
          with TConceptDetailsTopNode(lTopLevelNode) do begin
            if AName = PROP_MEANING_KEY then
              Result := MeaningKey
            else if AName = PROP_TERM_VERSION_KEY then
              Result := TermVersionKey
            else if AName = PROP_CONCEPT_KEY then
              Result := Key
          end; // with TConceptDetailsTopNode(lTopLevelNode)
    end;
  end
end;  // TRelationshipNode.InternalGetProperty


{-==============================================================================
    TRelationshipsListNode
===============================================================================}
{-------------------------------------------------------------------------------
}
function TRelationshipsListNode.GetChildNodeType: TFlyNodeClass;
begin
  Result := TRelationshipNode;
end;  // TRelationshipsListNode.GetChildNodeType 

{-------------------------------------------------------------------------------
}
function TRelationshipsListNode.GetChildrenStoredProc: string;
begin
  Result := 'usp_Relationships_Select_ForConcept';
end;  // TRelationshipsListNode.GetChildrenStoredProc 

{-------------------------------------------------------------------------------
}
function TRelationshipsListNode.GetInternalImageIndex: Integer;
begin
  Result := 1;
end;  // TRelationshipsListNode.GetInternalImageIndex 

{ TConceptDetailsStructuralNode }

{-==============================================================================
    TConceptDetailsStructuralNode
===============================================================================}
{-------------------------------------------------------------------------------
}
function TConceptDetailsStructuralNode.CanDelete: Boolean;
begin
  Result := False;
end;  // TConceptDetailsStructuralNode.CanDelete 

{-------------------------------------------------------------------------------
}
function TConceptDetailsStructuralNode.CanEdit: Boolean;
begin
  Result := False;
end;  // TConceptDetailsStructuralNode.CanEdit 

{-==============================================================================
    TSynonymNode
===============================================================================}
{-------------------------------------------------------------------------------
}
function TSynonymNode.CanAdd: Boolean;
begin
  Result := False;
end;  // TSynonymNode.CanAdd 

{-------------------------------------------------------------------------------
}
function TSynonymNode.CanEdit: Boolean;
begin
  Result := False;
end;  // TSynonymNode.CanEdit 

{-------------------------------------------------------------------------------
}
class function TSynonymNode.ClassTableName: string;
begin
  Result := TN_CONCEPT;
end;  // TSynonymNode.ClassTableName 

{-------------------------------------------------------------------------------
}
function TSynonymNode.GetDeleteStoredProc: string;
begin
  Result := 'usp_Synonym_Delete';
end;  // TSynonymNode.GetDeleteStoredProc 

{-------------------------------------------------------------------------------
}
function TSynonymNode.GetDetailsFrameClass: TBaseDetailFrameClass;
begin
  Result := TfraSynonym;
end;  // TSynonymNode.GetDetailsFrameClass 

{-------------------------------------------------------------------------------
  Return the image index for the node type. 
}
function TSynonymNode.GetInternalImageIndex: Integer;
begin
  Result := 7;
end;  // TSynonymNode.GetInternalImageIndex 

{-------------------------------------------------------------------------------
}
function TSynonymNode.InternalGetProperty(const AName: String): Variant;
begin
  Result := inherited InternalGetProperty(AName);
  
  if Result = Unassigned then begin
    if AName = PROP_NODE_CLASS then
      Result := Self.Classname
    else if AName = PROP_CONCEPT_GROUP_KEY then
      if TopLevelNode is TConceptDetailsTopNode then
        Result := TConceptDetailsTopNode(TopLevelNode).ConceptGroupKey
  end;
end;  // TSynonymNode.InternalGetProperty

{-==============================================================================
    TTermVersionNode
===============================================================================}
{-------------------------------------------------------------------------------
}
function TTermVersionNode.CanAdd: Boolean;
begin
  Result := False;
end;  // TTermVersionNode.CanAdd 

{-------------------------------------------------------------------------------
}
function TTermVersionNode.CanDelete: Boolean;
begin
  Result := FIsThisVersion;
end;  // TTermVersionNode.CanDelete 

{-------------------------------------------------------------------------------
}
function TTermVersionNode.CanEdit: Boolean;
begin
  Result := FIsThisVersion;
end;  // TTermVersionNode.CanEdit 

{-------------------------------------------------------------------------------
}
class function TTermVersionNode.ClassTableName: string;
begin
  Result := TN_TERM_VERSION;
end;  // TTermVersionNode.ClassTableName 

{-------------------------------------------------------------------------------
}
function TTermVersionNode.GetDeleteStoredProc: string;
begin
  Result := 'usp_TermVersion_Delete';
end;  // TTermVersionNode.GetDeleteStoredProc 

{-------------------------------------------------------------------------------
  Retrieve term version details when selected. 
}
function TTermVersionNode.GetDetailsFrameClass: TBaseDetailFrameClass;
begin
  Result := TfraTermVersion;
end;  // TTermVersionNode.GetDetailsFrameClass 

{-------------------------------------------------------------------------------
  Return the image index for the node type. 
}
function TTermVersionNode.GetInternalImageIndex: Integer;
begin
  Result := 17;
end;  // TTermVersionNode.GetInternalImageIndex 

{-------------------------------------------------------------------------------
}
procedure TTermVersionNode.Initialise(ARecordset: _Recordset);
begin
  inherited;
  FIsThisVersion := ARecordset.Fields['This_Term_Version'].Value=1;
end;  // TTermVersionNode.Initialise 

{-------------------------------------------------------------------------------
}
function TTermVersionNode.InternalGetProperty(const AName: String): Variant;
begin
  if AName=PROP_THISTERMVERSION then
    Result := FIsThisVersion
  else if AName = PROP_NODE_CLASS then
    Result := Self.Classname
  else
    Result := inherited InternalGetProperty(AName);
end;  // TTermVersionNode.InternalGetProperty 

{-==============================================================================
    TParentNode
===============================================================================}
{-------------------------------------------------------------------------------
}
constructor TParentNode.Create(AOwner : TTreeCollection);
begin
  inherited Create(AOwner);
  
  // Parents item node is navigable
  OverlayIndex := 0;
end;  // TParentNode.Create 

{-------------------------------------------------------------------------------
}
function TParentNode.CanAdd: Boolean;
begin
  Result := True;
end;  // TParentNode.CanAdd 

{-------------------------------------------------------------------------------
}
function TParentNode.CanEdit: Boolean;
begin
  Result := False;
end;  // TParentNode.CanEdit 

{-------------------------------------------------------------------------------
}
class function TParentNode.ClassTableName: string;
begin
  Result := TN_CONCEPT;
end;  // TParentNode.ClassTableName 

{-------------------------------------------------------------------------------
}
function TParentNode.GetDeleteStoredProc: string;
begin
  Result := 'usp_Synonym_Delete';
end;  // TParentNode.GetDeleteStoredProc 

{-------------------------------------------------------------------------------
}
function TParentNode.GetDetailsFrameClass: TBaseDetailFrameClass;
begin
  Result := TfraSynonym;
end;  // TParentNode.GetDetailsFrameClass 

{-------------------------------------------------------------------------------
}
function TParentNode.GetInternalImageIndex: Integer;
begin
  Result := 7;
end;  // TParentNode.GetInternalImageIndex 

{-------------------------------------------------------------------------------
}
function TParentNode.InternalGetProperty(const AName: String): Variant;
begin
  Result := inherited InternalGetProperty(AName);
  
  if Result = Unassigned then begin
    if AName = PROP_NODE_CLASS then
      Result := Self.Classname
    else if AName = PROP_CONCEPT_GROUP_KEY then
      if TopLevelNode is TConceptDetailsTopNode then
        Result := TConceptDetailsTopNode(TopLevelNode).ConceptGroupKey
  end;
end;  // TParentNode.InternalGetProperty 

{-==============================================================================
    TListSynonymNode
===============================================================================}
{-------------------------------------------------------------------------------
}
function TListSynonymNode.CanEdit: Boolean;
begin
  Result := True;
end;  // TListSynonymNode.CanEdit 

{-==============================================================================
    TAllSynonymNode
===============================================================================}
{-------------------------------------------------------------------------------
}
constructor TAllSynonymNode.Create(AOwner : TTreeCollection);
begin
  inherited Create(AOwner);

  // All synonyms item node is navigable
  OverlayIndex := 0;
end;  // TAllSynonymNode.Create

{-------------------------------------------------------------------------------
}
procedure TAllSynonymNode.Initialise(ARecordset: _Recordset);
begin
  inherited;

  FConceptGroupKey := VarToStr(ARecordset.Fields['Concept_Group_Key'].Value);
end;  // TAllSynonymNode.Initialise

{-==============================================================================
    TPotentialSynonymNode
===============================================================================}
{-------------------------------------------------------------------------------
}
constructor TPotentialSynonymNode.Create(AOwner : TTreeCollection);
begin
  inherited Create(AOwner);
  
  // Potential synonyms item node is navigable
  OverlayIndex := 0;
end;  // TPotentialSynonymNode.Create 

{-------------------------------------------------------------------------------
  You cannot delete from the Potential synonyms list. 
}
function TPotentialSynonymNode.CanDelete: Boolean;
begin
  Result := False;
end;  // TPotentialSynonymNode.CanDelete 

{-==============================================================================
    THomonymNode
===============================================================================}
{-------------------------------------------------------------------------------
}
function THomonymNode.GetDeleteStoredProc: String;
begin
  Result := 'usp_HomonymPair_Delete';
end;

end.
