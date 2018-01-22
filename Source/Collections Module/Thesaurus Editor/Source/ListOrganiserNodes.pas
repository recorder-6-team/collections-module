unit ListOrganiserNodes;

interface

uses
  SysUtils, Windows, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, RapTree, TreeColl, BaseDetailFrameUnit, DataTypes,
  DataClasses, GeneralData, LuxembourgConstants, Variants, BaseNavigatorFrame;

type
  TBaseListOrganiserNode = class(TNavigatorNode, IAdditionalProperties)
  private
    FKey: string;
    function GetParentKey: TKeyString;
    function GetProperty(const AName: string): Variant;
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    procedure SetKey(const Value: string);
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  protected
    function GetChildNodeType: TFlyNodeClass; virtual; abstract;
    function GetChildrenStoredProc: string; virtual; abstract;
    function GetDetailFrameClass: TBaseDetailFrameClass; virtual; abstract;
    function GetTableName: string; virtual;
  public
    class function ClassTableName: string; virtual; abstract;
    procedure DoDelete; virtual; abstract;
    property ChildNodeType: TFlyNodeClass read GetChildNodeType;
    property ChildrenStoredProc: string read GetChildrenStoredProc;
    property DetailFrameClass: TBaseDetailFrameClass read GetDetailFrameClass;
    property Key: string read FKey write SetKey;
    property ParentKey: TKeyString read GetParentKey;
    property TableName: string read GetTableName;
  end;
  
  TSubjectAreaNode = class(TBaseListOrganiserNode)
  protected
    function GetChildNodeType: TFlyNodeClass; override;
    function GetChildrenStoredProc: string; override;
    function GetDetailFrameClass: TBaseDetailFrameClass; override;
  public
    constructor Create(AOwner : TTreeCollection); override;
    class function ClassTableName: string; override;
    procedure DoDelete; override;
  end;
  
  TDomainNode = class(TBaseListOrganiserNode)
  protected
    function GetChildNodeType: TFlyNodeClass; override;
    function GetChildrenStoredProc: string; override;
    function GetDetailFrameClass: TBaseDetailFrameClass; override;
  public
    constructor Create(AOwner : TTreeCollection); override;
    class function ClassTableName: string; override;
    procedure DoDelete; override;
  end;
  
  {-----------------------------------------------------------------------------
    Retrieves the type child node types.
  }
  TLocalDomainNode = class(TBaseListOrganiserNode)
  protected
    function GetChildNodeType: TFlyNodeClass; override;
    function GetChildrenStoredProc: string; override;
    function GetDetailFrameClass: TBaseDetailFrameClass; override;
  public
    constructor Create(AOwner : TTreeCollection); override;
    class function ClassTableName: string; override;
    procedure DoDelete; override;
  end;
  
  {-----------------------------------------------------------------------------
    Retrieves the type child node types.
  }
  TConceptGroupNode = class(TBaseListOrganiserNode)
  protected
    function GetChildNodeType: TFlyNodeClass; override;
    function GetChildrenStoredProc: string; override;
    function GetDetailFrameClass: TBaseDetailFrameClass; override;
  public
    constructor Create(AOwner : TTreeCollection); override;
    class function ClassTableName: string; override;
    procedure DoDelete; override;
  end;
  
  TConceptGroupVersionNode = class(TBaseListOrganiserNode)
  protected
    function GetDetailFrameClass: TBaseDetailFrameClass; override;
  public
    constructor Create(AOwner : TTreeCollection); override;
    class function ClassTableName: string; override;
    procedure DoDelete; override;
  end;
  

implementation

uses
  SubjectArea, Domain, LocalDomain, ConceptGroup, ConceptGroupVersion,
  ThesaurusApplicationSettings;

{-==============================================================================
    TBaseListOrganiserNode
===============================================================================}
{-------------------------------------------------------------------------------
  Gets the key of the parent node. 
}
function TBaseListOrganiserNode.GetParentKey: TKeyString;
var
  lCurrentNode: TFlyNode;
begin
  lCurrentNode := self;
  if Assigned(lCurrentNode.Parent) then
    lCurrentNode := lCurrentNode.Parent;
  Result := TBaseListOrganiserNode(lCurrentNode).Key;
end;  // TBaseListOrganiserNode.GetParentKey 

{-------------------------------------------------------------------------------
  As TBaseListOrganiserNode implements IAdditionalProperties, the GetProperty
          method is required. It is used to get the Key and ParentKey.
}
function TBaseListOrganiserNode.GetProperty(const AName: string): Variant;
begin
  (*  if AName = 'Key' then
      Result := FKey
    else if AName = 'ParentKey' then
      Result := GetParentKey;             *)
  
  if AName = PROP_KEY then
    Result := FKey
  else if AName = PROP_PARENT_KEY then
    Result := GetParentKey
  else if AName = PROP_TABLE_NAME then
    Result := TableName
  else
    Result := Unassigned;
end;  // TBaseListOrganiserNode.GetProperty 

{-------------------------------------------------------------------------------
}
function TBaseListOrganiserNode.GetTableName: string;
begin
  Result := ClassTableName;
end;  // TBaseListOrganiserNode.GetTableName 

{-------------------------------------------------------------------------------
}
function TBaseListOrganiserNode.QueryInterface(const IID: TGUID; out Obj):
        HResult;
begin
  if GetInterface(IID, Obj) then
    Result := 0
  else
    Result := E_NOINTERFACE;
end;  // TBaseListOrganiserNode.QueryInterface 

{-------------------------------------------------------------------------------
  Accessor method - sets the node's Key. 
}
procedure TBaseListOrganiserNode.SetKey(const Value: string);
begin
  FKey := Value;
end;  // TBaseListOrganiserNode.SetKey 

{-------------------------------------------------------------------------------
}
function TBaseListOrganiserNode._AddRef: Integer;
begin
  Result := -1; //Don't worry about reference counting
end;  // TBaseListOrganiserNode._AddRef 

{-------------------------------------------------------------------------------
}
function TBaseListOrganiserNode._Release: Integer;
begin
  Result := -1; //Don't worry about reference counting
end;  // TBaseListOrganiserNode._Release 


{-==============================================================================
    TSubjectAreaNode
===============================================================================}
{-------------------------------------------------------------------------------
  Setup node standard behaviour 
}
constructor TSubjectAreaNode.Create(AOwner : TTreeCollection);
begin
  inherited Create(AOwner);
  
  ImageIndex := 0;
  SelectedIndex := 0;
end;  // TSubjectAreaNode.Create 

{-------------------------------------------------------------------------------
}
class function TSubjectAreaNode.ClassTableName: string;
begin
  Result := TN_SUBJECT_AREA;
end;  // TSubjectAreaNode.ClassTableName 

{-------------------------------------------------------------------------------
}
procedure TSubjectAreaNode.DoDelete;
begin
  dmGeneral.RunDeleteStoredProc('usp_SubjectArea_Delete', ['@Key', Key])
end;  // TSubjectAreaNode.DoDelete 

{-------------------------------------------------------------------------------
  Retrieves the type child node types. 
}
function TSubjectAreaNode.GetChildNodeType: TFlyNodeClass;
begin
  Result := TDomainNode;
end;  // TSubjectAreaNode.GetChildNodeType 

{-------------------------------------------------------------------------------
  Retrieve the stored procedure name used to populate the child nodes. 
}
function TSubjectAreaNode.GetChildrenStoredProc: string;
begin
  Result := 'usp_Domains_Select_ForSubjectArea';
end;  // TSubjectAreaNode.GetChildrenStoredProc 

{-------------------------------------------------------------------------------
  Gets the associated frame for concept group version nodes. 
}
function TSubjectAreaNode.GetDetailFrameClass: TBaseDetailFrameClass;
begin
  Result := TfraSubjectArea;
end;  // TSubjectAreaNode.GetDetailFrameClass 


{-==============================================================================
    TDomainNode
===============================================================================}
{-------------------------------------------------------------------------------
  Setup node standard behaviour 
}
constructor TDomainNode.Create(AOwner : TTreeCollection);
begin
  inherited Create(AOwner);
  
  ImageIndex := 1;
  SelectedIndex := 1;
end;  // TDomainNode.Create 

{-------------------------------------------------------------------------------
}
class function TDomainNode.ClassTableName: string;
begin
  Result := TN_DOMAIN;
end;  // TDomainNode.ClassTableName 

{-------------------------------------------------------------------------------
}
procedure TDomainNode.DoDelete;
begin
  dmGeneral.RunDeleteStoredProc('usp_Domain_Delete', ['@Key', Key])
end;  // TDomainNode.DoDelete 

{-------------------------------------------------------------------------------
  Retrieves the type child node types. 
}
function TDomainNode.GetChildNodeType: TFlyNodeClass;
begin
  Result := TLocalDomainNode;
end;  // TDomainNode.GetChildNodeType 

{-------------------------------------------------------------------------------
  Retrieve the stored procedure name used to populate the child nodes. 
}
function TDomainNode.GetChildrenStoredProc: string;
begin
  Result := 'usp_LocalDomains_Select_ForDomain';
end;  // TDomainNode.GetChildrenStoredProc 

{-------------------------------------------------------------------------------
  Gets the associated frame for concept group version nodes. 
}
function TDomainNode.GetDetailFrameClass: TBaseDetailFrameClass;
begin
  Result := TfraDomain;
end;  // TDomainNode.GetDetailFrameClass 



{-==============================================================================
    TLocalDomainNode
===============================================================================}
{-------------------------------------------------------------------------------
  Setup node standard behaviour 
}
constructor TLocalDomainNode.Create(AOwner : TTreeCollection);
begin
  inherited Create(AOwner);
  
  ImageIndex := 2;
  SelectedIndex := 2;
end;  // TLocalDomainNode.Create 

{-------------------------------------------------------------------------------
}
class function TLocalDomainNode.ClassTableName: string;
begin
  Result := TN_LOCAL_DOMAIN;
end;  // TLocalDomainNode.ClassTableName 

{-------------------------------------------------------------------------------
}
procedure TLocalDomainNode.DoDelete;
begin
  dmGeneral.RunDeleteStoredProc('usp_LocalDomain_Delete', ['@Key', Key])
end;  // TLocalDomainNode.DoDelete 

{-------------------------------------------------------------------------------
  Retrieves the type child node types. 
}
function TLocalDomainNode.GetChildNodeType: TFlyNodeClass;
begin
  Result := TConceptGroupNode;
end;  // TLocalDomainNode.GetChildNodeType 

{-------------------------------------------------------------------------------
  Retrieve the stored procedure name used to populate the child nodes. 
}
function TLocalDomainNode.GetChildrenStoredProc: string;
begin
  Result := 'usp_ConceptGroups_Select_ForLocalDomain';
end;  // TLocalDomainNode.GetChildrenStoredProc 

{-------------------------------------------------------------------------------
  Gets the associated frame for concept group version nodes. 
}
function TLocalDomainNode.GetDetailFrameClass: TBaseDetailFrameClass;
begin
  Result := TfraLocalDomain;
end;  // TLocalDomainNode.GetDetailFrameClass 


{-==============================================================================
    TConceptGroupNode
===============================================================================}
{-------------------------------------------------------------------------------
  Setup node standard behaviour 
}
constructor TConceptGroupNode.Create(AOwner : TTreeCollection);
begin
  inherited Create(AOwner);
  
  ImageIndex := 3;
  SelectedIndex := 3;
end;  // TConceptGroupNode.Create 

{-------------------------------------------------------------------------------
}
class function TConceptGroupNode.ClassTableName: string;
begin
  Result := TN_CONCEPT_GROUP;
end;  // TConceptGroupNode.ClassTableName 

{-------------------------------------------------------------------------------
}
procedure TConceptGroupNode.DoDelete;
begin
  dmGeneral.RunDeleteStoredProc('usp_ConceptGroup_Delete',
           ['@Key', Key,
           '@SyncTaxonDict', ThesApplicationSettings.SyncTaxonDictDeletions]);
end;  // TConceptGroupNode.DoDelete 

{-------------------------------------------------------------------------------
}
function TConceptGroupNode.GetChildNodeType: TFlyNodeClass;
begin
  Result := TConceptGroupVersionNode;
end;  // TConceptGroupNode.GetChildNodeType 

{-------------------------------------------------------------------------------
  Retrieve the stored procedure name used to populate the child nodes. 
}
function TConceptGroupNode.GetChildrenStoredProc: string;
begin
  Result := 'usp_ConceptGroupVersions_Select_ForConceptGroup';
end;  // TConceptGroupNode.GetChildrenStoredProc 

{-------------------------------------------------------------------------------
  Gets the associated frame for concept group version nodes. 
}
function TConceptGroupNode.GetDetailFrameClass: TBaseDetailFrameClass;
begin
  Result := TfraConceptGroup;
end;  // TConceptGroupNode.GetDetailFrameClass 


{-==============================================================================
    TConceptGroupVersionNode
===============================================================================}
{-------------------------------------------------------------------------------
  Setup node standard behaviour 
}
constructor TConceptGroupVersionNode.Create(AOwner : TTreeCollection);
begin
  inherited Create(AOwner);
  
  ImageIndex := 4;
  SelectedIndex := 4;
end;  // TConceptGroupVersionNode.Create 

{-------------------------------------------------------------------------------
}
class function TConceptGroupVersionNode.ClassTableName: string;
begin
  Result := TN_CONCEPT_GROUP_VERSION;
end;  // TConceptGroupVersionNode.ClassTableName 

{-------------------------------------------------------------------------------
}
procedure TConceptGroupVersionNode.DoDelete;
begin
  dmGeneral.RunDeleteStoredProc('usp_ConceptGroupVersion_Delete',
          ['@Key', Key,
           '@SyncTaxonDict', ThesApplicationSettings.SyncTaxonDictDeletions]);
end;  // TConceptGroupVersionNode.DoDelete 

{-------------------------------------------------------------------------------
  Gets the associated frame for concept group version nodes. 
}
function TConceptGroupVersionNode.GetDetailFrameClass: TBaseDetailFrameClass;
begin
  Result := TfraConceptGroupVersion;
end;  // TConceptGroupVersionNode.GetDetailFrameClass 



end.
