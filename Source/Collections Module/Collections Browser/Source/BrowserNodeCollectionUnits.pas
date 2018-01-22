{===============================================================================
  Unit:        BrowserNodeCollectionUnits.pas

  Defines:     Many Classes

  Description: Contains node classes common to more than one of the three
               collection units

  Model:       BrowserNodes.mpb

  Created:     August 2003

===============================================================================}
unit BrowserNodeCollectionUnits;

interface

uses
  SysUtils, Windows, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, TreeColl, BrowserNodeFramework, ResourceStrings,
  BaseDetailFrameUnit, ADODB, CommonNameFetchQueue, RapTree, DataTypes,
  Menus, SearchManager, DataClasses, FrameAddSpecimenUnit, StdCtrls;

type
  TRelatedCollectionUnitLeafNode = class(THyperlinkLeafNode, IAdditionalProperties)
  private
    function GetProperty(const AName: string): Variant;
  protected
    function GetAssociatedFrame: TBaseDetailFrameClass; override;
    function GetCanEdit: Boolean; override;
    function GetImageIndex: Integer; override;
    function GetNodeContext: TNodeContext; override;
    procedure InitialiseHyperlinkKey(ARecordset: _Recordset); override;
    function InternalGetProperty(const AName: String): Variant; override;
    procedure SetCaption(ARecordset: _Recordset); override;
  public
    class function ClassTableName: String; override;
    procedure FindAndSetDragDropKey; override;
    procedure FindAndSetHyperlinkKey; override;
    procedure SetSecurity; override;
  end;
  
  TCollectorDeterminerFolderNode = class(TFolderNode)
  protected
    function GetCanAdd: Boolean; override;
    function GetChildNodeType(Index: Integer): TBrowserNodeClass; override;
    function GetChildNodeTypeCount: Integer; override;
    procedure SetCaption; override;
    function StoredProcByChildType(ALeafNodeClass: TLeafNodeClass): String; override;
  public
    class function ClassTableName: String; override;
  end;
  
  TCollectionUnitFolderNode = class(TFolderNode)
  private
    function GetIsLoanIn: Boolean;
  public
    function ValidateNewNode(const AKey: TKeyString; const ACaption: string): Boolean;
        override;
  end;
  
  TCollectionFolderNode = class(TCollectionUnitFolderNode, IAddMenuOptions)
  protected
    function GetAddButtonMenuCaption(Index: Integer): String; override;
    function GetAddButtonMenuCaptionsCount: Integer; override;
    function GetAddMenuIsAdd(AMenuIndex: Integer): Boolean; override;
    function GetCanAdd: Boolean; override;
    function GetChildNodeType(Index: Integer): TBrowserNodeClass; override;
    function GetChildNodeTypeCount: Integer; override;
    function GetNodeContext: TNodeContext; override;
    function GetSearchType: SearchManager.TSearchType; override;
    function GetStoredProcParams: TVariantArray; override;
    procedure SetCaption; override;
    function StoredProcByChildType(ALeafNodeClass: TLeafNodeClass): String; override;
  public
    class function ClassTableName: String; override;
    procedure DeleteLink(const AJoinTableKey: String); override;
    function UpdateNodeRelationship(const NewNodeKey: String): String; override;
  end;
  
  TSpecimenFolderNode = class(TCollectionUnitFolderNode, IAddMenuOptions)
  private
    FCommonNameFetchQueue: TCommonNameFetchQueue;
    FUpdateUsualLocation: Boolean;
  protected
    function GetAddButtonMenuCaption(Index: Integer): String; override;
    function GetAddButtonMenuCaptionsCount: Integer; override;
    function GetAddMenuIsAdd(AMenuIndex: Integer): Boolean; override;
    function GetCanAdd: Boolean; override;
    function GetChildNodeType(Index: Integer): TBrowserNodeClass; override;
    function GetChildNodeTypeCount: Integer; override;
    function GetNodeContext: TNodeContext; override;
    function GetSearchType: SearchManager.TSearchType; override;
    function GetStoredProcParams: TVariantArray; override;
    procedure PopulateFromDatabase(ALeafNodeClass: TLeafNodeClass; const AStoredProcName:
        String); override;
    procedure SetCaption; override;
    function StoredProcByChildType(ALeafNodeClass: TLeafNodeClass): String; override;
  public
    constructor Create(AOwner: TTreeCollection); override;
    destructor Destroy; override;
    class function ClassTableName: String; override;
    function ConfirmNodeAction(const AKey: String; const ACaption: String): Boolean; override;
    procedure DeleteLink(const AJoinTableKey: String); override;
    function UpdateNodeRelationship(const NewNodeKey: String): String; override;
  end;
  
  TStoreFolderNode = class(TCollectionUnitFolderNode, IAddMenuOptions)
  protected
    function GetAddButtonMenuCaption(Index: Integer): String; override;
    function GetAddButtonMenuCaptionsCount: Integer; override;
    function GetAddMenuIsAdd(AMenuIndex: Integer): Boolean; override;
    function GetCanAdd: Boolean; override;
    function GetChildNodeType(Index: Integer): TBrowserNodeClass; override;
    function GetChildNodeTypeCount: Integer; override;
    function GetNodeContext: TNodeContext; override;
    function GetSearchType: SearchManager.TSearchType; override;
    procedure SetCaption; override;
    function StoredProcByChildType(ALeafNodeClass: TLeafNodeClass): String; override;
  public
    class function ClassTableName: String; override;
    procedure DeleteLink(const AJoinTableKey: String); override;
    function UpdateNodeRelationship(const NewNodeKey: String): String; override;
  end;
  
  TCollectionUnitLeafNode = class(THyperlinkLeafNode)
  protected
    function GetDomainStoredProc: String; override;
    procedure SetCaption(ARecordset: _Recordset); override;
  end;
  
  TCollectionLeafNode = class(TCollectionUnitLeafNode)
  protected
    function GetAssociatedFrame: TBaseDetailFrameClass; override;
    function GetCanDelete: Boolean; override;
    function GetImageIndex: Integer; override;
    function GetNodeContext: TNodeContext; override;
  public
    class function ClassTableName: String; override;
  end;

  TSpecimenLeafNode = class(TCollectionUnitLeafNode, IConceptNameCaption)
  private
    FLifeSciences: Boolean;
    function GetConceptCaption: String;
    procedure SetConceptCaption(const Value: String);
  protected
    function GetAssociatedFrame: TBaseDetailFrameClass; override;
    function GetCanDelete: Boolean; override;
    function GetImageIndex: Integer; override;
    function GetNodeContext: TNodeContext; override;
    function InternalGetProperty(const AName: string): Variant; override;
    procedure InternalInitialiseAddNode; override;
    procedure InternalInitialiseLinkNode(const AKey: string = ''; const ACaption: string ='');
        override;
    procedure SetCaption(ARecordset: _Recordset); override;
  public
    class function ClassTableName: String; override;
    property ConceptCaption: String read GetConceptCaption write SetConceptCaption;
    property LifeSciences: Boolean read FLifeSciences write FLifeSciences;
  end;
  
  TStoreLeafNode = class(TCollectionUnitLeafNode)
  private
    FDrawHierarchically: Boolean;
    FHierarchyDrawSpacer: Integer;
    FImageIndex: Integer;
    procedure SetDrawHierarchically(Value: Boolean);
    procedure SetHierarchyDrawSpacer(Value: Integer);
  protected
    function GetAssociatedFrame: TBaseDetailFrameClass; override;
    function GetCanDelete: Boolean; override;
    function GetImageIndex: Integer; override;
    function GetNodeContext: TNodeContext; override;
  public
    constructor Create(AOwner: TTreeCollection); override;
    class function ClassTableName: String; override;
    property DrawHierarchically: Boolean read FDrawHierarchically write SetDrawHierarchically;
    property HierarchyDrawSpacer: Integer read FHierarchyDrawSpacer write
        SetHierarchyDrawSpacer;
  end;
  
  TCollectionUnitTopLevelNode = class(TTopLevelNode)
  protected
    function GetDomainStoredProc: String; override;
    procedure SetCaption(ARecordset: _Recordset); override;
    function StoredProcByChildType(ALeafNodeClass: TLeafNodeClass): String; override;
  end;
  
  TCollectionTopLevelNode = class(TCollectionUnitTopLevelNode, IAdditionalProperties)
  private
    function GetProperty(const AName: string): Variant;
  protected
    procedure AddChildNodesOfType(ABrowserNodeClass: TBrowserNodeClass); override;
    function GetAssociatedFrame: TBaseDetailFrameClass; override;
    function GetChildNodeType(Index: Integer): TBrowserNodeClass; override;
    function GetChildNodeTypeCount: Integer; override;
    function GetImageIndex: Integer; override;
    function GetNodeContext: TNodeContext; override;
    function InternalGetProperty(const AName: string): Variant; override;
  public
    class function ClassTableName: String; override;
  end;
  
  TSpecimenTopLevelNode = class(TCollectionUnitTopLevelNode, IAdditionalProperties,
      IConceptNameCaption)
  private
    FLifeSciences: Boolean;
    function GetConceptCaption: String;
    function GetProperty(const AName: string): Variant;
    procedure SetConceptCaption(const Value: String);
  protected
    procedure AddChildNodesOfType(ABrowserNodeClass: TBrowserNodeClass); override;
    function GetAssociatedFrame: TBaseDetailFrameClass; override;
    function GetChildNodeType(Index: Integer): TBrowserNodeClass; override;
    function GetChildNodeTypeCount: Integer; override;
    function GetImageIndex: Integer; override;
    function GetNodeContext: TNodeContext; override;
    function InternalGetProperty(const AName: string): Variant; override;
    procedure InternalInialiseNewNode; override;
    procedure InternalInitialise(ARecordset: _Recordset); override;
    procedure SetCaption(ARecordset: _Recordset); override;
    procedure InternalRefreshCaption; override;
  public
    class function ClassTableName: String; override;
    property ConceptCaption: String read GetConceptCaption write SetConceptCaption;
    property LifeSciences: Boolean read FLifeSciences write FLifeSciences;
  end;
  
  TStoreTopLevelNode = class(TCollectionUnitTopLevelNode, IAdditionalProperties)
  private
    function GetProperty(const AName: string): Variant;
  protected
    procedure AddChildNodesOfType(ABrowserNodeClass: TBrowserNodeClass); override;
    function GetAssociatedFrame: TBaseDetailFrameClass; override;
    function GetChildNodeType(Index: Integer): TBrowserNodeClass; override;
    function GetChildNodeTypeCount: Integer; override;
    function GetImageIndex: Integer; override;
    function GetNodeContext: TNodeContext; override;
    function InternalGetProperty(const AName: string): Variant; override;
  public
    class function ClassTableName: String; override;
  end;
  
  TCollectionUnitLinkedFolderNode = class(TFolderNode)
  protected
    function GetCanAdd: Boolean; override;
  end;

  TCollectionLinkedFolderNode = class(TCollectionUnitLinkedFolderNode)
  protected
    procedure AddChildNodesOfType(ABrowserNodeClass: TBrowserNodeClass); override;
    function GetChildNodeType(Index: Integer): TBrowserNodeClass; override;
    function GetChildNodeTypeCount: Integer; override;
    function GetSearchType: SearchManager.TSearchType; override;
    procedure SetCaption; override;
    function StoredProcByChildType(ALeafNodeClass: TLeafNodeClass): String; override;
  public
    class function ClassTableName: String; override;
  end;

  TStoreLinkedFolderNode = class(TCollectionUnitLinkedFolderNode)
  protected
    procedure AddChildNodesOfType(ABrowserNodeClass: TBrowserNodeClass); override;
    function GetChildNodeType(Index: Integer): TBrowserNodeClass; override;
    function GetChildNodeTypeCount: Integer; override;
    function GetSearchType: SearchManager.TSearchType; override;
    procedure SetCaption; override;
    function StoredProcByChildType(ALeafNodeClass: TLeafNodeClass): String; override;
  public
    class function ClassTableName: String; override;
  end;
  
  TSpecimenLinkedFolderNode = class(TCollectionUnitLinkedFolderNode, IAddMenuOptions)
  private
    FCommonNameFetchQueue: TCommonNameFetchQueue;
  protected
    function GetAddButtonMenuCaption(Index: Integer): String; override;
    function GetAddButtonMenuCaptionsCount: Integer; override;
    function GetAddMenuIsAdd(AMenuIndex: Integer): Boolean; override;
    function GetCanAdd: Boolean; override;
    function GetChildNodeType(Index: Integer): TBrowserNodeClass; override;
    function GetChildNodeTypeCount: Integer; override;
    function GetNodeContext: TNodeContext; override;
    function GetSearchType: TSearchType; override;
    function GetStoredProcParams: TVariantArray; override;
    procedure SetCaption; override;
    function StoredProcByChildType(ALeafNodeClass: TLeafNodeClass): String; override;
  public
    constructor Create(AOwner: TTreeCollection); override;
    destructor Destroy; override;
    class function ClassTableName: String; override;
    function ValidateNewNode(const AKey: TKeyString; const ACaption: string): Boolean;
        override;
  end;

  TCollectionUnitLinkedIncludesFolderNode = class(TFolderNode)
  end;
  
  TCollectionLinkedIncludesFolderNode = class(TCollectionUnitLinkedIncludesFolderNode,
      IAddMenuOptions)
  protected
    function GetAddButtonMenuCaption(Index: Integer): String; override;
    function GetAddButtonMenuCaptionsCount: Integer; override;
    function GetAddMenuIsAdd(AMenuIndex: Integer): Boolean; override;
    function GetChildNodeType(Index: Integer): TBrowserNodeClass; override;
    function GetChildNodeTypeCount: Integer; override;
    function GetNodeContext: TNodeContext; override;
    function GetSearchType: SearchManager.TSearchType; override;
    procedure SetCaption; override;
    function StoredProcByChildType(ALeafNodeClass: TLeafNodeClass): String; override;
  public
    class function ClassTableName: String; override;
    procedure DeleteLink(const AJoinTableKey: String); override;
    function UpdateNodeRelationship(const NewNodeKey: String): String; override;
    function ValidateNewNode(const AKey: TKeyString; const ACaption: string): Boolean;
        override;
  end;
  
  TStoreHierarchyFolderNode = class(TCollectionUnitLinkedIncludesFolderNode)
  private
    procedure PopulateImmediateChild(ABrowserNodeClass: TBrowserNodeClass; const
        AStoredProcName: String);
  protected
    procedure AddChildNodesOfType(ABrowserNodeClass: TBrowserNodeClass); override;
    function GetCanAdd: Boolean; override;
    function GetChildNodeType(Index: Integer): TBrowserNodeClass; override;
    function GetChildNodeTypeCount: Integer; override;
    function GetNodeContext: TNodeContext; override;
    function GetSearchType: SearchManager.TSearchType; override;
    function GetStoredProcParams: TVariantArray; override;
    procedure SetCaption; override;
    function StoredProcByChildType(ALeafNodeClass: TLeafNodeClass): String; override;
  public
    class function ClassTableName: String; override;
  end;
  
  TCollectionUnitLinkedIncludedInFolderNode = class(TFolderNode)
  protected
    function GetAddButtonMenuCaption(Index: Integer): String; override;
    function GetAddButtonMenuCaptionsCount: Integer; override;
    function GetAddMenuIsAdd(AMenuIndex: Integer): Boolean; override;
  end;
  
  TCollectionLinkedIncludedInFolderNode = class(TCollectionUnitLinkedIncludedInFolderNode,
      IAddMenuOptions)
  protected
    function GetChildNodeType(Index: Integer): TBrowserNodeClass; override;
    function GetChildNodeTypeCount: Integer; override;
    function GetNodeContext: TNodeContext; override;
    function GetSearchType: SearchManager.TSearchType; override;
    procedure SetCaption; override;
    function StoredProcByChildType(ALeafNodeClass: TLeafNodeClass): String; override;
  public
    class function ClassTableName: String; override;
    procedure DeleteLink(const AJoinTableKey: String); override;
    function UpdateNodeRelationship(const NewNodeKey: String): String; override;
    function ValidateNewNode(const AKey: TKeyString; const ACaption: string): Boolean;
        override;
  end;
  
  TStoragePlaceFolderNode = class(TCollectionUnitLinkedIncludedInFolderNode, IAddMenuOptions)
  protected
    function GetCanAdd: Boolean; override;
    function GetChildNodeType(Index: Integer): TBrowserNodeClass; override;
    function GetChildNodeTypeCount: Integer; override;
    function GetNodeContext: TNodeContext; override;
    function GetSearchType: SearchManager.TSearchType; override;
    procedure PopulateFromDatabase(ALeafNodeClass: TLeafNodeClass; const AStoredProcName:
        String); override;
    procedure SetCaption; override;
    function StoredProcByChildType(ALeafNodeClass: TLeafNodeClass): String; override;
  public
    class function ClassTableName: String; override;
    function UpdateNodeRelationship(const NewNodeKey: String): String; override;
    function ValidateNewNode(const AKey: TKeyString; const ACaption: string): Boolean;
        override;
  end;
  
  TCollectionUnitLinkedOtherFolderNode = class(TFolderNode)
  public
    class function ClassTableName: String; override;
  end;
  
  TCollectionLinkedOtherFolderNode = class(TCollectionUnitLinkedOtherFolderNode,
      IAddMenuOptions)
  protected
    function GetAddButtonMenuCaption(Index: Integer): String; override;
    function GetAddButtonMenuCaptionsCount: Integer; override;
    function GetAddMenuIsAdd(AMenuIndex: Integer): Boolean; override;
    function GetChildNodeType(Index: Integer): TBrowserNodeClass; override;
    function GetChildNodeTypeCount: Integer; override;
    function GetNodeContext: TNodeContext; override;
    function GetSearchType: SearchManager.TSearchType; override;
    procedure SetCaption; override;
    function StoredProcByChildType(ALeafNodeClass: TLeafNodeClass): String; override;
  end;
  
  TStoreLinkedOtherFolderNode = class(TCollectionUnitLinkedOtherFolderNode, IAddMenuOptions)
  protected
    function GetAddButtonMenuCaption(Index: Integer): String; override;
    function GetAddButtonMenuCaptionsCount: Integer; override;
    function GetAddMenuIsAdd(AMenuIndex: Integer): Boolean; override;
    function GetChildNodeType(Index: Integer): TBrowserNodeClass; override;
    function GetChildNodeTypeCount: Integer; override;
    function GetNodeContext: TNodeContext; override;
    function GetSearchType: SearchManager.TSearchType; override;
    procedure SetCaption; override;
    function StoredProcByChildType(ALeafNodeClass: TLeafNodeClass): String; override;
  end;
  
  {-----------------------------------------------------------------------------
    This foldernode has the imageindex and associatedframe properties of the TStoreLeafNode.
    Its children can only be itself and TStoreLeafNodes.
  }
  TStoreHierarchySubFolderNode = class(TFolderNode, IAddMenuOptions)
  private
    FBottomLevel: Boolean;
    FDblClickNavigates: Boolean;
    FHyperlinkKey: TKeyString;
    FJoinKey: String;
    procedure PopulateImmediateChild(ABrowserNodeClass: TBrowserNodeClass; const
        AStoredProcName: String);
    procedure SetBottomLevel(const Value: Boolean);
  protected
    procedure AddChildNodesOfType(ABrowserNodeClass: TBrowserNodeClass); override;
    function AddNode(ATree: TRapidTree; AMenuIndex: integer): TFlyNode;
    function GetAddButtonMenuCaption(Index: Integer): String; override;
    function GetAddButtonMenuCaptionsCount: Integer; override;
    function GetAddMenuIsAdd(AMenuIndex: Integer): Boolean; override;
    function GetAssociatedFrame: TBaseDetailFrameClass; override;
    function GetCanAdd: Boolean; override;
    function GetCanEdit: Boolean; override;
    function GetChildNodeType(Index: Integer): TBrowserNodeClass; override;
    function GetChildNodeTypeCount: Integer; override;
    function GetImageIndex: Integer; override;
    function GetKeyList: TKeyList; override;
    function GetNodeContext: TNodeContext; override;
    function GetSearchType: SearchManager.TSearchType; override;
    function GetStoredProcParams: TVariantArray; override;
    function InternalGetProperty(const AName: String): Variant; override;
    procedure InternalInitialise(AKey: String); override;
    procedure InternalRefresh; override;
    procedure SetCaption; override;
    function StoredProcByChildType(ALeafNodeClass: TLeafNodeClass): String; override;
  public
    class function ClassTableName: String; override;
    procedure FindAndSetHyperlinkKey; virtual;
    function LinkNode(const AKey: string; const ACaption: string): TBrowserNode; override;
    function ValidateNewNode(const AKey: TKeyString; const ACaption: string): Boolean;
        override;
    property BottomLevel: Boolean read FBottomLevel write SetBottomLevel;
    property DblClickNavigates: Boolean read FDblClickNavigates write FDblClickNavigates;
    property HyperlinkKey: TKeyString read FHyperlinkKey write FHyperlinkKey;
    property JoinKey: String read FJoinKey write FJoinKey;
  end;

  TStoreHierarchyTopLevelNode = class(THyperlinkTopLevelNode, IAdditionalProperties,
      IAddMenuOptions)
  private
    FBottomLevel: Boolean;
    function AddNode(ATree: TRapidTree; AMenuIndex: integer): TFlyNode;
    function GetProperty(const AName: string): Variant;
    procedure PopulateImmediateChild(ABrowserNodeClass: TBrowserNodeClass; const
        AStoredProcName: String);
    procedure SetBottomLevel(const Value: Boolean);
  protected
    procedure AddChildNodesOfType(ABrowserNodeClass: TBrowserNodeClass); override;
    function GetAddButtonMenuCaption(Index: Integer): String; override;
    function GetAddButtonMenuCaptionsCount: Integer; override;
    function GetAddMenuIsAdd(AMenuIndex: Integer): Boolean; override;
    function GetAssociatedFrame: TBaseDetailFrameClass; override;
    function GetCanAdd: Boolean; override;
    function GetChildNodeType(Index: Integer): TBrowserNodeClass; override;
    function GetChildNodeTypeCount: Integer; override;
    function GetImageIndex: Integer; override;
    function GetKeyList: TKeyList; override;
    function GetNodeContext: TNodeContext; override;
    function GetSearchType: SearchManager.TSearchType; override;
    function GetStoredProcParams: TVariantArray; override;
    function InternalGetProperty(const AName: string): Variant; override;
    procedure InternalInitialise(ARecordset: _Recordset); override;
    procedure InternalRefresh; override;
    procedure SetCaption(ARecordset: _Recordset); override;
    function StoredProcByChildType(ALeafNodeClass: TLeafNodeClass): String; override;
  public
    class function ClassTableName: String; override;
    function LinkNode(const AKey: string; const ACaption: string): TBrowserNode; override;
    function ValidateNewNode(const AKey: TKeyString; const ACaption: string): Boolean;
        override;
    property BottomLevel: Boolean read FBottomLevel write SetBottomLevel;
  end;

function RemoveBrowserNodeTags(const Caption: string): string;

//==============================================================================
implementation

uses BrowserNodeCommon, BrowserNodeMovement, BrowserNodeConditionCheck,
     BrowserNodeSpecimen, Variants, GeneralData, ApplicationSettings,
     FrameCollection, FrameListViewer, FrameSpecimen, BrowserViewTypes,
     FrameRelated, FrameStorage, LuxembourgConstants, GeneralFunctions,
     UserMessages, SpecimenRelocation, StrUtils;

{ ------------------------------------------------------------------------------
  Removes markup tags from the caption of a browser node.
}
function RemoveBrowserNodeTags(const Caption: string): string;
begin
  Result := RemoveSubStrings(Caption, ['<i>', '</i>', '<b/>*', '<b/>']);
end;

{ ------------------------------------------------------------------------------
  Formats the caption of a specimen node according to the nomenclatural status
  of its determinations.
}
function FormatSpecimenCaption(
  const Caption,
  NomenclaturalStatus: string): string;
begin
  if AnsiContainsText(NomenclaturalStatus, 'type') then
    Result := '<b/>*' + Caption
  else if AnsiStartsText('figured,', NomenclaturalStatus) or
    AnsiEndsText(',figured', NomenclaturalStatus) or
    AnsiContainsText(NomenclaturalStatus, ',figured')
  then
    Result := '<b/>' + Caption
  else
    Result := Caption;
end;

{-==============================================================================
    TRelatedCollectionUnitLeafNode
===============================================================================}
{-------------------------------------------------------------------------------
}
class function TRelatedCollectionUnitLeafNode.ClassTableName: String;
begin
  Result := TN_COLLECTION_UNIT_RELATION;
end;  // TRelatedCollectionUnitLeafNode.ClassTableName 

{-------------------------------------------------------------------------------
}
procedure TRelatedCollectionUnitLeafNode.FindAndSetDragDropKey;
begin
  DragDropKey := dmGeneral.GetStoredProcOutputParam(
      'usp_CollectionUnitRelation_DragDropKey_Get',
                          ['@Key', Key], '@DragDropKey');
end;  // TRelatedCollectionUnitLeafNode.FindAndSetDragDropKey 

{-------------------------------------------------------------------------------
}
procedure TRelatedCollectionUnitLeafNode.FindAndSetHyperlinkKey;
begin
  HyperlinkKey := dmGeneral.GetStoredProcOutputParam(
                      'usp_CollectionUnitRelation_DragDropKey_Get',
                      ['@Key', Key,
                      '@ParentCollectionUnitKey', ParentKey], '@DragDropKey');
end;  // TRelatedCollectionUnitLeafNode.FindAndSetHyperlinkKey 

{-------------------------------------------------------------------------------
}
function TRelatedCollectionUnitLeafNode.GetAssociatedFrame: TBaseDetailFrameClass;
begin
  Result := TfraRelated;
end;  // TRelatedCollectionUnitLeafNode.GetAssociatedFrame 

{-------------------------------------------------------------------------------
}
function TRelatedCollectionUnitLeafNode.GetCanEdit: Boolean;
var
  lTopLevelNode: TFlyNode;
begin
  lTopLevelNode := TopLevelNode;
  if Assigned(lTopLevelNode) then
    if lTopLevelNode is TTopLevelNode then
      Result := TTopLevelNode(lTopLevelNode).CanEdit
    else
      raise EBrowserNodeError.Create(ResStr_DomainMaskNotFoundOnTopLevelNode)
  else
    raise EBrowserNodeError.Create(ResStr_DomainMaskNotFoundOnTopLevelNode);
end;  // TRelatedCollectionUnitLeafNode.GetCanEdit 

{-------------------------------------------------------------------------------
}
function TRelatedCollectionUnitLeafNode.GetImageIndex: Integer;
begin
  Result := 30;
end;  // TRelatedCollectionUnitLeafNode.GetImageIndex 

{-------------------------------------------------------------------------------
}
function TRelatedCollectionUnitLeafNode.GetNodeContext: TNodeContext;
begin
  Result := TopNodeContext;
end;  // TRelatedCollectionUnitLeafNode.GetNodeContext 

{-------------------------------------------------------------------------------
}
function TRelatedCollectionUnitLeafNode.GetProperty(const AName: string): Variant;
begin
  Result := InternalGetProperty(AName);
end;  // TRelatedCollectionUnitLeafNode.GetProperty 

{-------------------------------------------------------------------------------
}
procedure TRelatedCollectionUnitLeafNode.InitialiseHyperlinkKey(ARecordset: _Recordset);
begin
  HyperlinkKey := ARecordset.Fields['Hyperlink_Item_Key'].Value;
end;  // TRelatedCollectionUnitLeafNode.InitialiseHyperlinkKey 

{-------------------------------------------------------------------------------
}
function TRelatedCollectionUnitLeafNode.InternalGetProperty(const AName: String): Variant;
var
  lTopLevelNode: TFlyNode;
begin
  Result := inherited InternalGetProperty(AName);
  
  // Store this locally so don't need to re-evaluate it for each 'if' statement.
  lTopLevelNode := self.TopLevelNode;
  
  if Result = Unassigned then
    if AName = PROP_NODE_CONTEXT then
      if Assigned(lTopLevelNode) then
        begin
          if lTopLevelNode is TSpecimenTopLevelNode then
            Result := ncSpecimen
          else if lTopLevelNode is TStoreTopLevelNode then
            Result := ncStore
          else if lTopLevelNode is TCollectionTopLevelNode then
            Result := ncCollection
          else
            raise EBrowserNodeError.Create(ResStr_InvalidNodeContext)
        end
      else
        raise EBrowserNodeError.Create(ResStr_InvalidNodeContext);
end;  // TRelatedCollectionUnitLeafNode.InternalGetProperty 

{-------------------------------------------------------------------------------
}
procedure TRelatedCollectionUnitLeafNode.SetCaption(ARecordset: _Recordset);
var
  lCaption: String;
begin
  // If a Specimen with no determination has been linked, the caption will be
  // null. Hence, the caption 'No Determination' needs to be shown.
  lCaption := VarToStr(ARecordset.Fields['Item_Name'].Value);
  if lCaption = '' then lCaption := ResStr_NoDetermination;
  
  if ARecordset.Fields['Number'].Value = Null then
    Caption := lCaption
  else
    Caption := lCaption + ' - ' + VarToStr(ARecordset.Fields['Number'].Value);
end;  // TRelatedCollectionUnitLeafNode.SetCaption 

{-------------------------------------------------------------------------------
}
procedure TRelatedCollectionUnitLeafNode.SetSecurity;
var
  lTopLevelNode: TFlyNode;
begin
  lTopLevelNode := TopLevelNode;
  if Assigned(lTopLevelNode) then
    if lTopLevelNode is TTopLevelNode then
      DomainMask := TTopLevelNode(lTopLevelNode).DomainMask;
end;  // TRelatedCollectionUnitLeafNode.SetSecurity 

{-==============================================================================
    TCollectorDeterminerFolderNode
===============================================================================}
{-------------------------------------------------------------------------------
}
class function TCollectorDeterminerFolderNode.ClassTableName: String;
begin
  Result := TN_NAME;
end;  // TCollectorDeterminerFolderNode.ClassTableName 

{-------------------------------------------------------------------------------
}
function TCollectorDeterminerFolderNode.GetCanAdd: Boolean;
begin
  Result := False;
end;  // TCollectorDeterminerFolderNode.GetCanAdd 

{-------------------------------------------------------------------------------
}
function TCollectorDeterminerFolderNode.GetChildNodeType(Index: Integer): TBrowserNodeClass;
begin
  case Index of
    0: Result := TPeopleOrganisationLeafNode;
  else
    raise EBrowserNodeError.Create(ResStr_InvalidChildNodeTypeIndexRequest);
  end;
end;  // TCollectorDeterminerFolderNode.GetChildNodeType 

{-------------------------------------------------------------------------------
}
function TCollectorDeterminerFolderNode.GetChildNodeTypeCount: Integer;
begin
  Result:= 1;
end;  // TCollectorDeterminerFolderNode.GetChildNodeTypeCount 

{-------------------------------------------------------------------------------
}
procedure TCollectorDeterminerFolderNode.SetCaption;
begin
  Text := ResStr_CollectorsAndDeterminers;
end;  // TCollectorDeterminerFolderNode.SetCaption 

{-------------------------------------------------------------------------------
}
function TCollectorDeterminerFolderNode.StoredProcByChildType(ALeafNodeClass: TLeafNodeClass):
    String;
begin
  Result := '';
  
  if ALeafNodeClass = TPeopleOrganisationLeafNode then
    if ParentNodeContext = ncCollection then
      Result := 'usp_CollectorsAndDeterminers_Select_ForCollection'
    else if ParentNodeContext = ncSpecimen then
      Result := 'usp_CollectorsAndDeterminers_Select_ForSpecimen'
    else
      raise EBrowserNodeError.Create(ResStr_ChildNodeContextInvalid)
  else
    raise EBrowserNodeError.Create(ResStr_InvalidChildNodeTypeIndexRequest);
end;  // TCollectorDeterminerFolderNode.StoredProcByChildType 

{-==============================================================================
    TCollectionFolderNode
===============================================================================}
{-------------------------------------------------------------------------------
}
class function TCollectionFolderNode.ClassTableName: String;
begin
  Result := TN_COLLECTION;
end;  // TCollectionFolderNode.ClassTableName 

{-------------------------------------------------------------------------------
}
procedure TCollectionFolderNode.DeleteLink(const AJoinTableKey: String);
begin
  case ParentNodeContext of
    ncAccession,
    ncLoan,
    ncMovement,
    ncMovementIn,
    ncMovementOut    : dmGeneral.RunStoredProc('usp_MovementCollectionUnit_Delete',
                                               ['@Key', AJoinTableKey]);
    ncConditionCheck : dmGeneral.RunStoredProc('usp_CollectionUnitCheck_Delete',
                                               ['@Key', AJoinTableKey]);
    ncEnquiry        : dmGeneral.RunStoredProc('usp_CollectionUnitEnquiry_Delete',
                                               ['@Key', AJoinTableKey]);
    ncValuation      : dmGeneral.RunStoredProc('usp_CollectionUnitValuation_Delete',
                                               ['@Key', AJoinTableKey]);
  else
    raise EBrowserNodeError.Create(ResStr_ChildNodeContextInvalid);
  end;
end;  // TCollectionFolderNode.DeleteLink 

{-------------------------------------------------------------------------------
}
function TCollectionFolderNode.GetAddButtonMenuCaption(Index: Integer): String;
begin
  case Index of
   0:  if ParentNodeContext in [ncAccession, ncLoan, ncMovement,
                                ncMovementIn, ncMovementOut] then
         Result := ResStr_AddNew
       else
         Result := ResStr_Link;
  
   1:  Result := ResStr_LinkToExisting;
  else
    raise EAddMenuItemError.Create(ResStr_InvalidAddMenuIndexRequest);
  end;
end;  // TCollectionFolderNode.GetAddButtonMenuCaption 

{-------------------------------------------------------------------------------
}
function TCollectionFolderNode.GetAddButtonMenuCaptionsCount: Integer;
begin
  case ParentNodeContext of
    ncAccession,
    ncLoan,
    ncMovement,
    ncMovementIn,
    ncMovementOut:
      Result := 2;
  
    ncValuation,
    ncConditionCheck,
    ncEnquiry,
    ncSpecimen:
        // Specimen, Condition Check, Enquiry, Valuation have no popup menu.
      Result := 1;
  
    ncJob,           // Job and store have add button disabled.
    ncStore:
      Result := 0;
    else
      raise EAddMenuItemError.Create(ResStr_InvalidAddMenuIndexRequest);
  end;
end;  // TCollectionFolderNode.GetAddButtonMenuCaptionsCount 

{-------------------------------------------------------------------------------
}
function TCollectionFolderNode.GetAddMenuIsAdd(AMenuIndex: Integer): Boolean;
begin
  case AddButtonMenuCaptionsCount of
    1:  case AMenuIndex of
          0 :    Result := False;
          else   Result := True;
        end;
    else  // i.e. AddButtonMenuCaptionsCount = 2
        case AMenuIndex of
          0 :    Result := True;
          else   Result := False;
        end;
  end;
end;  // TCollectionFolderNode.GetAddMenuIsAdd 

{-------------------------------------------------------------------------------
}
function TCollectionFolderNode.GetCanAdd: Boolean;
begin
  if (AppSettings.AddDomainMask > 0) and not (ParentNodeContext in [ncJob, ncStore]) then
    Result := inherited GetCanAdd
  else
    Result := False;
end;  // TCollectionFolderNode.GetCanAdd 

{-------------------------------------------------------------------------------
}
function TCollectionFolderNode.GetChildNodeType(Index: Integer): TBrowserNodeClass;
begin
  case Index of
    0: Result := TCollectionLeafNode;
  else
    raise EBrowserNodeError.Create(ResStr_InvalidChildNodeTypeIndexRequest);
  end;
end;  // TCollectionFolderNode.GetChildNodeType 

{-------------------------------------------------------------------------------
}
function TCollectionFolderNode.GetChildNodeTypeCount: Integer;
begin
  Result:= 1;
end;  // TCollectionFolderNode.GetChildNodeTypeCount 

{-------------------------------------------------------------------------------
}
function TCollectionFolderNode.GetNodeContext: TNodeContext;
begin
  Result := ncCollection;
end;  // TCollectionFolderNode.GetNodeContext 

{-------------------------------------------------------------------------------
}
function TCollectionFolderNode.GetSearchType: SearchManager.TSearchType;
begin
  Result := stCollection;
end;  // TCollectionFolderNode.GetSearchType 

{-------------------------------------------------------------------------------
}
function TCollectionFolderNode.GetStoredProcParams: TVariantArray;
var
  lSortIndex: Integer;
begin
  lSortIndex := ViewTypeManager.ViewTypeByNodeContext(NodeContext).SortOrderDefaultIndex;
  
  if ParentNodeContext = ncValuation then
    Result:= VarArrayOf(['@ParentKey', Key, '@UserID', AppSettings.UserID,
                         '@SortOrderIndex', lSortIndex])
  else
    Result := VarArrayOf(['@ParentKey', Key, '@SortOrderIndex', lSortIndex]);
end;  // TCollectionFolderNode.GetStoredProcParams 

{-------------------------------------------------------------------------------
}
procedure TCollectionFolderNode.SetCaption;
begin
  if ParentNodeContext = ncConditionCheck then
    Text := ResStr_CollectionsChecked
  else
    Text := ResStr_Collections;
end;  // TCollectionFolderNode.SetCaption 

{-------------------------------------------------------------------------------
}
function TCollectionFolderNode.StoredProcByChildType(ALeafNodeClass: TLeafNodeClass): String;
begin
  Result := '';
  
  if ALeafNodeClass = TCollectionLeafNode then
    case ParentNodeContext of
      ncSpecimen       : Result := 'usp_Collections_Select_ForSpecimen';
      ncStore          : Result := 'usp_Collections_Select_ForStore';
      ncJob            : Result := 'usp_Collections_Select_ForJob';
      ncAccession,
      ncLoan,
      ncMovement       : Result := 'usp_Collections_Select_ForMovement';
      ncMovementIn     : Result := 'usp_Collections_Select_ForMovementIn';
      ncMovementOut    : Result := 'usp_Collections_Select_ForMovementOut';
      ncConditionCheck : Result := 'usp_Collections_Select_ForConditionCheck';
      ncEnquiry        : Result := 'usp_Collections_Select_ForEnquiry';
      ncValuation      : Result := 'usp_Collections_Select_ForValuation';
    else
      raise EBrowserNodeError.Create(ResStr_ChildNodeContextInvalid);
    end
  else
    raise EBrowserNodeError.Create(ResStr_InvalidChildNodeTypeIndexRequest);
end;  // TCollectionFolderNode.StoredProcByChildType 

{-------------------------------------------------------------------------------
}
function TCollectionFolderNode.UpdateNodeRelationship(const NewNodeKey: String): String;
begin
  case ParentNodeContext of
    ncSpecimen :
      begin
        Result := VarToStr(dmGeneral.GetStoredProcOutputParam(
                           'usp_CollectionUnit_Update_ForSpecimen',
                           ['@ParentKey', Key, '@ChildKey', NewNodeKey], '@JoinKey'));
        EnforceSingleChildNode(NewNodeKey);
      end;
    ncMovement : Result := VarToStr(dmGeneral.GetStoredProcOutputParam(
        'usp_CollectionUnit_Update_ForMovement',
                     ['@ParentKey', Key, '@ChildKey', NewNodeKey], '@JoinKey'));
    ncLoan :  Result := VarToStr(dmGeneral.GetStoredProcOutputParam(
        'usp_CollectionUnit_Update_ForMovement',
                     ['@ParentKey', Key, '@ChildKey', NewNodeKey,
                     '@IsInbound', GetIsLoanIn], '@JoinKey'));
    ncMovementIn,   // Inbound exchange
    ncMovementOut,  // Outbound exchange
    ncAccession : Result := VarToStr(dmGeneral.GetStoredProcOutputParam(
        'usp_CollectionUnit_Update_ForMovement',
                     ['@ParentKey', Key, '@ChildKey', NewNodeKey,
                     '@IsInbound', ParentNodeContext <> ncMovementOut,
                     '@IsAccessionOrExchange', 1], '@JoinKey'));
    ncValuation: Result := VarToStr(dmGeneral.GetStoredProcOutputParam(
        'usp_CollectionUnit_Update_ForValuation',
                     ['@ParentKey', Key, '@ChildKey', NewNodeKey], '@JoinKey'));
    ncEnquiry : Result := VarToStr(dmGeneral.GetStoredProcOutputParam(
        'usp_CollectionUnit_Update_ForEnquiry',
                     ['@ParentKey', Key, '@ChildKey', NewNodeKey], '@JoinKey'));
    ncConditionCheck : Result := VarToStr(dmGeneral.GetStoredProcOutputParam(
        'usp_CollectionUnit_Update_ForConditionCheck',
                     ['@ParentKey', Key, '@ChildKey', NewNodeKey], '@JoinKey'));
  else
    raise EAddMenuItemError.Create(ResStr_InvalidNodeContext);
  end;
end;  // TCollectionFolderNode.UpdateNodeRelationship 


{-==============================================================================
    TSpecimenFolderNode
===============================================================================}
{-------------------------------------------------------------------------------
}
constructor TSpecimenFolderNode.Create(AOwner: TTreeCollection);
begin
  inherited Create(AOwner);
  
  FCommonNameFetchQueue := TCommonNameFetchQueue.Create(Tree);
  FCommonNameFetchQueue.Parent := Tree;
  FCommonNameFetchQueue.TreeView := TRapidTree(Tree);
end;  // TSpecimenFolderNode.Create 

{-------------------------------------------------------------------------------
}
destructor TSpecimenFolderNode.Destroy;
begin
  FCommonNameFetchQueue.Free;
  
  inherited Destroy;
end;  // TSpecimenFolderNode.Destroy 

{-------------------------------------------------------------------------------
}
class function TSpecimenFolderNode.ClassTableName: String;
begin
  Result := TN_SPECIMEN_UNIT;
end;  // TSpecimenFolderNode.ClassTableName 

{-------------------------------------------------------------------------------
  Called when moving a specimen from one location to another. Need to know
  whether both usual and current locations should be updated, or just the
  current location of the specimen.
}
function TSpecimenFolderNode.ConfirmNodeAction(const AKey: String; const ACaption: String): Boolean;
begin
  // Permanent/temporary relocation only applies when top node is store.
  if TopNodeContext <> ncStore then begin
    Result := True;
    FUpdateUsualLocation := False;
  end else
  if NodeActionCancelled then
    Result := False
  else
  if not ConfirmAppliesToAll then begin
    Result := False;
    FUpdateUsualLocation := False;
    with TdlgSpecimenRelocation.Create(nil) do
      try
        RelocationInfo := ACaption;
        if not ConfirmApplyToAllOption then HideApplyToAllOption;

        if ShowModal = mrOk then begin
          ConfirmAppliesToAll  := ApplyToAll;
          FUpdateUsualLocation := chkPermanent.Checked;
          Result               := True;
        end else
          NodeActionCancelled := True;
      finally
        Free;
      end;
  end else
    Result := True;
end;  // TSpecimenFolderNode.ConfirmNodeAction

{-------------------------------------------------------------------------------
}
procedure TSpecimenFolderNode.DeleteLink(const AJoinTableKey: String);
begin
  case ParentNodeContext of
    ncStore          : dmGeneral.RunStoredProc('usp_Specimen_Delete_ForStore',
                                               ['@Key', AJoinTableKey]);
    ncAccession,
    ncLoan,
    ncMovement,
    ncMovementIn,
    ncMovementOut    : dmGeneral.RunStoredProc('usp_MovementCollectionUnit_Delete',
                                               ['@Key', AJoinTableKey]);
    ncConditionCheck : dmGeneral.RunStoredProc('usp_CollectionUnitCheck_Delete',
                                               ['@Key', AJoinTableKey]);
    ncEnquiry        : dmGeneral.RunStoredProc('usp_CollectionUnitEnquiry_Delete',
                                               ['@Key', AJoinTableKey]);
    ncValuation      : dmGeneral.RunStoredProc('usp_CollectionUnitValuation_Delete',
                                               ['@Key', AJoinTableKey]);
  else
    raise EBrowserNodeError.Create(ResStr_ChildNodeContextInvalid);
  end;
end;  // TSpecimenFolderNode.DeleteLink 

{-------------------------------------------------------------------------------
}
function TSpecimenFolderNode.GetAddButtonMenuCaption(Index: Integer): String;
begin
  case ParentNodeContext of
    ncMovementIn,
    ncMovementOut,
    ncAccession,
    ncCollection,
    ncEnquiry:
      begin
        case Index of
          0:  Result := ResStr_AddNew;
          1:  Result := ResStr_LinkToExisting;
        else
          raise EAddMenuItemError.Create(ResStr_InvalidAddMenuIndexRequest);
        end;
      end;
    ncLoan,
    ncMovement,
    ncStore,
    ncConditionCheck,
    ncValuation:
      begin
        if Index = 0 then
          Result := ResStr_Link
        else
          raise EAddMenuItemError.Create(ResStr_InvalidAddMenuIndexRequest);
      end;
  end;
end;  // TSpecimenFolderNode.GetAddButtonMenuCaption 

{-------------------------------------------------------------------------------
}
function TSpecimenFolderNode.GetAddButtonMenuCaptionsCount: Integer;
begin
  case ParentNodeContext of
    ncMovementIn,
    ncMovementOut,
    ncAccession,
    ncCollection,
    ncEnquiry:   Result := 2;
    ncLoan,
    ncMovement,
    ncStore,
    ncConditionCheck,
    ncValuation: Result := 1;
    ncJob:       Result := 0;
  else
    Result := 0;
  end;
end;  // TSpecimenFolderNode.GetAddButtonMenuCaptionsCount 

{-------------------------------------------------------------------------------
}
function TSpecimenFolderNode.GetAddMenuIsAdd(AMenuIndex: Integer): Boolean;
begin
  case ParentNodeContext of
    ncMovementIn,
    ncMovementOut,
    ncAccession,
    ncCollection,
    ncEnquiry:
      begin
        if AMenuIndex = 0 then
          Result := True
        else
          Result := False;
      end;
    ncLoan,
    ncMovement,
    ncStore,
    ncConditionCheck,
    ncValuation:
      Result := False;
    else
      raise EBrowserNodeError.Create(ResStr_InvalidNodeContext);
  end;
end;  // TSpecimenFolderNode.GetAddMenuIsAdd 

{-------------------------------------------------------------------------------
}
function TSpecimenFolderNode.GetCanAdd: Boolean;
begin
  if (AppSettings.AddDomainMask > 0) and not (ParentNodeContext = ncJob) then
    Result := inherited GetCanAdd
  else
    Result := False;
end;  // TSpecimenFolderNode.GetCanAdd 

{-------------------------------------------------------------------------------
}
function TSpecimenFolderNode.GetChildNodeType(Index: Integer): TBrowserNodeClass;
begin
  case Index of
    0: Result := TSpecimenLeafNode;
  else
    raise EBrowserNodeError.Create(ResStr_InvalidChildNodeTypeIndexRequest);
  end;
end;  // TSpecimenFolderNode.GetChildNodeType 

{-------------------------------------------------------------------------------
}
function TSpecimenFolderNode.GetChildNodeTypeCount: Integer;
begin
  Result:= 1;
end;  // TSpecimenFolderNode.GetChildNodeTypeCount 

{-------------------------------------------------------------------------------
}
function TSpecimenFolderNode.GetNodeContext: TNodeContext;
begin
  Result := ncSpecimen;
end;  // TSpecimenFolderNode.GetNodeContext 

{-------------------------------------------------------------------------------
}
function TSpecimenFolderNode.GetSearchType: SearchManager.TSearchType;
begin
  Result := stSpecimen;
end;  // TSpecimenFolderNode.GetSearchType 

{-------------------------------------------------------------------------------
}
function TSpecimenFolderNode.GetStoredProcParams: TVariantArray;
var
  lSortIndex: Integer;
begin
  lSortIndex := ViewTypeManager.ViewTypeByNodeContext(NodeContext).SortOrderDefaultIndex;
  
  if ParentNodeContext = ncValuation then
    Result:= VarArrayOf(['@ParentKey', Key, '@UserID', AppSettings.UserID,
                         '@SortOrderIndex', lSortIndex])
  else
    Result:= VarArrayOf(['@ParentKey', Key, '@SortOrderIndex', lSortIndex]);
end;  // TSpecimenFolderNode.GetStoredProcParams 

{-------------------------------------------------------------------------------
}
procedure TSpecimenFolderNode.PopulateFromDatabase(ALeafNodeClass: TLeafNodeClass; const
    AStoredProcName: String);
var
  lNewNode: TLeafNode;
  lRecordset: _Recordset;
begin
  lRecordset := dmGeneral.GetRecordset(AStoredProcName, GetStoredProcParams);
  
  Tree.Items.BeginUpdate;
  try
    if lRecordset.RecordCount > 0 then begin
      lRecordSet.MoveFirst;
      while not lRecordset.EOF do begin
        lNewNode:= TLeafNode(Tree.Items.AddTypedChild(Self, ALeafNodeClass));
        //Will never return a non-LeafNode so NodeContext needn't be set.
        lNewNode.Initialise(lRecordset); //Sets Key and Caption
        TSpecimenLeafNode(lNewNode).LifeSciences :=
                          lRecordset.Fields['Life_Sciences'].Value;
        // For thesaurus nodes, add to the common name fetch queue
        if (lRecordset.Fields['Life_Sciences'].Value=0) and
            not (lRecordset.Fields['Det_Item_Key'].Value = Null) then
          FCommonNameFetchQueue.Add(lRecordset.Fields['Det_Item_Key'].Value,
              lNewNode);
        lRecordset.MoveNext;
      end;
    end;
  finally
    Tree.Items.EndUpdate;
    lRecordset.Close;
  end;
  FCommonNameFetchQueue.ProcessWhenReady;
end;  // TSpecimenFolderNode.PopulateFromDatabase 

{-------------------------------------------------------------------------------
}
procedure TSpecimenFolderNode.SetCaption;
begin
  if ParentNodeContext = ncConditionCheck then
    Text := ResStr_SpecimensChecked
  else
    Text := ResStr_Specimens;
end;  // TSpecimenFolderNode.SetCaption 

{-------------------------------------------------------------------------------
}
function TSpecimenFolderNode.StoredProcByChildType(ALeafNodeClass: TLeafNodeClass): String;
begin
  Result := '';
  
  if ALeafNodeClass = TSpecimenLeafNode then
    case ParentNodeContext of
      ncCollection     : Result := 'usp_Specimens_Select_ForCollection';
      ncStore          : Result := 'usp_Specimens_Select_ForStore';
      ncJob            : Result := 'usp_Specimens_Select_ForJob';
      ncAccession,
      ncLoan,
      ncMovement       : Result := 'usp_Specimens_Select_ForMovement';
      ncMovementIn     : Result := 'usp_Specimens_Select_ForMovementIn';
      ncMovementOut    : Result := 'usp_Specimens_Select_ForMovementOut';
      ncConditionCheck : Result := 'usp_Specimens_Select_ForConditionCheck';
      ncEnquiry        : Result := 'usp_Specimens_Select_ForEnquiry';
      ncValuation      : Result := 'usp_Specimens_Select_ForValuation';
    else
      raise EBrowserNodeError.Create(ResStr_ChildNodeContextInvalid);
    end
  else
    raise EBrowserNodeError.Create(ResStr_InvalidChildNodeTypeIndexRequest);
end;  // TSpecimenFolderNode.StoredProcByChildType 

{-------------------------------------------------------------------------------
}
function TSpecimenFolderNode.UpdateNodeRelationship(const NewNodeKey: String): String;
begin
  case ParentNodeContext of
    ncCollection : Result := VarToStr(dmGeneral.GetStoredProcOutputParam(
        'usp_Specimen_Update_ForCollection',
        ['@ParentKey', Key,
         '@ChildKey', NewNodeKey],
        '@JoinKey'));
    ncStore : Result := VarToStr(dmGeneral.GetStoredProcOutputParam(
        'usp_Specimen_Update_ForStore',
        ['@ParentKey', Key,
         '@ChildKey', NewNodeKey,
         '@UpdateUsualLocation', FUpdateUsualLocation],
        '@JoinKey'));
    ncMovement : Result := VarToStr(dmGeneral.GetStoredProcOutputParam(
        'usp_CollectionUnit_Update_ForMovement',
        ['@ParentKey', Key,
         '@ChildKey', NewNodeKey],
        '@JoinKey'));
    ncLoan :  Result := VarToStr(dmGeneral.GetStoredProcOutputParam(
        'usp_CollectionUnit_Update_ForMovement',
        ['@ParentKey', Key,
         '@ChildKey', NewNodeKey,
         '@IsInbound', GetIsLoanIn],
        '@JoinKey'));
    ncMovementIn,   // Inbound exchange
    ncMovementOut,  // Outbound exchange
    ncAccession : Result := VarToStr(dmGeneral.GetStoredProcOutputParam(
        'usp_CollectionUnit_Update_ForMovement',
        ['@ParentKey', Key,
         '@ChildKey', NewNodeKey,
         '@IsInbound', ParentNodeContext <> ncMovementOut,
         '@IsAccessionOrExchange', 1],
        '@JoinKey'));
    ncValuation: Result := VarToStr(dmGeneral.GetStoredProcOutputParam(
        'usp_CollectionUnit_Update_ForValuation',
        ['@ParentKey', Key,
         '@ChildKey', NewNodeKey],
        '@JoinKey'));
    ncEnquiry : Result := VarToStr(dmGeneral.GetStoredProcOutputParam(
        'usp_CollectionUnit_Update_ForEnquiry',
        ['@ParentKey', Key,
         '@ChildKey', NewNodeKey],
        '@JoinKey'));
    ncConditionCheck : Result := VarToStr(dmGeneral.GetStoredProcOutputParam(
        'usp_CollectionUnit_Update_ForConditionCheck',
        ['@ParentKey', Key,
         '@ChildKey', NewNodeKey],
        '@JoinKey'));
  else
    raise EAddMenuItemError.Create(ResStr_InvalidNodeContext);
  end;
end;  // TSpecimenFolderNode.UpdateNodeRelationship 

{-==============================================================================
    TStoreFolderNode
===============================================================================}
{-------------------------------------------------------------------------------
}
class function TStoreFolderNode.ClassTableName: String;
begin
  Result := TN_STORE;
end;  // TStoreFolderNode.ClassTableName 

{-------------------------------------------------------------------------------
  Method to handle the deletion of links. 
}
procedure TStoreFolderNode.DeleteLink(const AJoinTableKey: String);
begin
  case ParentNodeContext of
    ncAccession,
    ncLoan,
    ncMovement,
    ncMovementIn,
    ncMovementOut    : dmGeneral.RunStoredProc('usp_MovementCollectionUnit_Delete',
                                               ['@Key', AJoinTableKey]);
    ncConditionCheck : dmGeneral.RunStoredProc('usp_CollectionUnitCheck_Delete',
                                               ['@Key', AJoinTableKey]);
    ncEnquiry        : dmGeneral.RunStoredProc('usp_CollectionUnitEnquiry_Delete',
                                               ['@Key', AJoinTableKey]);
    ncValuation      : dmGeneral.RunStoredProc('usp_CollectionUnitValuation_Delete',
                                               ['@Key', AJoinTableKey]);
  else
    raise EBrowserNodeError.Create(ResStr_ChildNodeContextInvalid);
  end;
end;  // TStoreFolderNode.DeleteLink 

{-------------------------------------------------------------------------------
}
function TStoreFolderNode.GetAddButtonMenuCaption(Index: Integer): String;
begin
  case Index of
    0:  Result := ResStr_AddNew;
    1:  Result := ResStr_LinkToExisting;
  else
    raise EAddMenuItemError.Create(ResStr_InvalidAddMenuIndexRequest);
  end;
end;  // TStoreFolderNode.GetAddButtonMenuCaption 

{-------------------------------------------------------------------------------
}
function TStoreFolderNode.GetAddButtonMenuCaptionsCount: Integer;
begin
  case NodeContext of
    ncCollection,
    ncJob:
      Result := 0;
  else
      Result := 2;
  end;
end;  // TStoreFolderNode.GetAddButtonMenuCaptionsCount 

{-------------------------------------------------------------------------------
}
function TStoreFolderNode.GetAddMenuIsAdd(AMenuIndex: Integer): Boolean;
begin
  case AMenuIndex of
    1 : Result := False;
  else
    Result := True;
  end;
end;  // TStoreFolderNode.GetAddMenuIsAdd 

{-------------------------------------------------------------------------------
}
function TStoreFolderNode.GetCanAdd: Boolean;
begin
  if (AppSettings.AddDomainMask > 0) and not(ParentNodeContext in [ncCollection, ncJob]) then
    Result := inherited GetCanAdd
  else
    Result := false;
end;  // TStoreFolderNode.GetCanAdd 

{-------------------------------------------------------------------------------
}
function TStoreFolderNode.GetChildNodeType(Index: Integer): TBrowserNodeClass;
begin
  case Index of
    0: Result := TStoreLeafNode;
  else
    raise EBrowserNodeError.Create(ResStr_InvalidChildNodeTypeIndexRequest);
  end;
end;  // TStoreFolderNode.GetChildNodeType 

{-------------------------------------------------------------------------------
}
function TStoreFolderNode.GetChildNodeTypeCount: Integer;
begin
  Result:= 1;
end;  // TStoreFolderNode.GetChildNodeTypeCount 

{-------------------------------------------------------------------------------
}
function TStoreFolderNode.GetNodeContext: TNodeContext;
begin
  Result := ncStore;
end;  // TStoreFolderNode.GetNodeContext 

{-------------------------------------------------------------------------------
}
function TStoreFolderNode.GetSearchType: SearchManager.TSearchType;
begin
  Result := stStoreName;
end;  // TStoreFolderNode.GetSearchType 

{-------------------------------------------------------------------------------
}
procedure TStoreFolderNode.SetCaption;
begin
  if ParentNodeContext = ncConditionCheck then
    Text := ResStr_StoresChecked
  else
    Text := ResStr_Stores;
end;  // TStoreFolderNode.SetCaption 

{-------------------------------------------------------------------------------
}
function TStoreFolderNode.StoredProcByChildType(ALeafNodeClass: TLeafNodeClass): String;
begin
  Result := '';
  
  if ALeafNodeClass = TStoreLeafNode then
    case ParentNodeContext of
      ncCollection     : Result := 'usp_Stores_Select_ForCollection';
      ncJob            : Result := 'usp_Stores_Select_ForJob';
      ncAccession,
      ncLoan,
      ncMovement       : Result := 'usp_Stores_Select_ForMovement';
      ncMovementIn     : Result := 'usp_Stores_Select_ForMovementIn';
      ncMovementOut    : Result := 'usp_Stores_Select_ForMovementOut';
      ncConditionCheck : Result := 'usp_Stores_Select_ForConditionCheck';
      ncEnquiry        : Result := 'usp_Stores_Select_ForEnquiry';
    else
      raise EBrowserNodeError.Create(ResStr_ChildNodeContextInvalid);
    end
  else
    raise EBrowserNodeError.Create(ResStr_InvalidChildNodeTypeIndexRequest);
end;  // TStoreFolderNode.StoredProcByChildType 

{-------------------------------------------------------------------------------
}
function TStoreFolderNode.UpdateNodeRelationship(const NewNodeKey: String): String;
begin
  case ParentNodeContext of
    ncMovement : Result := VarToStr(dmGeneral.GetStoredProcOutputParam(
        'usp_CollectionUnit_Update_ForMovement',
                     ['@ParentKey', Key, '@ChildKey', NewNodeKey], '@JoinKey'));
    ncLoan :  Result := VarToStr(dmGeneral.GetStoredProcOutputParam(
        'usp_CollectionUnit_Update_ForMovement',
                     ['@ParentKey', Key, '@ChildKey', NewNodeKey,
                     '@IsInbound', GetIsLoanIn], '@JoinKey'));
    ncMovementIn,
    ncMovementOut,
    ncAccession : Result := VarToStr(dmGeneral.GetStoredProcOutputParam(
        'usp_CollectionUnit_Update_ForMovement',
                     ['@ParentKey', Key, '@ChildKey', NewNodeKey,
                     '@IsInbound', ParentNodeContext <> ncMovementOut,
                     '@IsAccessionOrExchange', 1], '@JoinKey'));
    ncEnquiry : Result := VarToStr(dmGeneral.GetStoredProcOutputParam(
        'usp_CollectionUnit_Update_ForEnquiry',
                     ['@ParentKey', Key, '@ChildKey', NewNodeKey], '@JoinKey'));
    ncConditionCheck : Result := VarToStr(dmGeneral.GetStoredProcOutputParam(
        'usp_CollectionUnit_Update_ForConditionCheck',
                     ['@ParentKey', Key, '@ChildKey', NewNodeKey], '@JoinKey'));
  else
    raise EAddMenuItemError.Create(ResStr_InvalidNodeContext);
  end;
end;  // TStoreFolderNode.UpdateNodeRelationship 


{-==============================================================================
    TCollectionUnitLeafNode
===============================================================================}
{-------------------------------------------------------------------------------
}
function TCollectionUnitLeafNode.GetDomainStoredProc: String;
begin
  Result := 'usp_CollectionUnit_DomainMask_Get';
end;  // TCollectionUnitLeafNode.GetDomainStoredProc 

{-------------------------------------------------------------------------------
}
procedure TCollectionUnitLeafNode.SetCaption(ARecordset: _Recordset);
begin
  if ARecordset.Fields['Number'].Value = Null then
    Caption:= ARecordset.Fields['Item_Name'].Value
  else
    Caption:= ARecordset.Fields['Item_Name'].Value + ' - ' +
              VarToStr(ARecordset.Fields['Number'].Value);
end;  // TCollectionUnitLeafNode.SetCaption 

{-==============================================================================
    TCollectionLeafNode
===============================================================================}
{-------------------------------------------------------------------------------
}
class function TCollectionLeafNode.ClassTableName: String;
begin
  Result := TN_COLLECTION;
end;  // TCollectionLeafNode.ClassTableName 

{-------------------------------------------------------------------------------
}
function TCollectionLeafNode.GetAssociatedFrame: TBaseDetailFrameClass;
begin
  Result := TfraCollection;
end;  // TCollectionLeafNode.GetAssociatedFrame 

{-------------------------------------------------------------------------------
}
function TCollectionLeafNode.GetCanDelete: Boolean;
begin
  Result := not (TopNodeContext in [ncSpecimen, ncStore, ncJob]) and
            inherited GetCanDelete;
end;  // TCollectionLeafNode.GetCanDelete 

{-------------------------------------------------------------------------------
}
function TCollectionLeafNode.GetImageIndex: Integer;
begin
  Result := 2;
end;  // TCollectionLeafNode.GetImageIndex 

{-------------------------------------------------------------------------------
}
function TCollectionLeafNode.GetNodeContext: TNodeContext;
begin
  Result := ncCollection;
end;  // TCollectionLeafNode.GetNodeContext 

{-==============================================================================
    TSpecimenLeafNode
===============================================================================}
{-------------------------------------------------------------------------------
}
class function TSpecimenLeafNode.ClassTableName: String;
begin
  Result := TN_SPECIMEN_UNIT;
end;  // TSpecimenLeafNode.ClassTableName 

{-------------------------------------------------------------------------------
}
function TSpecimenLeafNode.GetAssociatedFrame: TBaseDetailFrameClass;
begin
  Result := TfraSpecimen;
end;  // TSpecimenLeafNode.GetAssociatedFrame 

{-------------------------------------------------------------------------------
}
function TSpecimenLeafNode.GetCanDelete: Boolean;
begin
  Result := not (TopNodeContext in [ncCollection, ncJob]) and
            inherited GetCanDelete;
end;  // TSpecimenLeafNode.GetCanDelete 

{-------------------------------------------------------------------------------
}
function TSpecimenLeafNode.GetConceptCaption: String;
begin
  Result := Text;
end;  // TSpecimenLeafNode.GetConceptCaption 

{-------------------------------------------------------------------------------
}
function TSpecimenLeafNode.GetImageIndex: Integer;
begin
  Result := 3;
end;  // TSpecimenLeafNode.GetImageIndex 

{-------------------------------------------------------------------------------
}
function TSpecimenLeafNode.GetNodeContext: TNodeContext;
begin
  Result := ncSpecimen;
end;  // TSpecimenLeafNode.GetNodeContext 

{-------------------------------------------------------------------------------
}
function TSpecimenLeafNode.InternalGetProperty(const AName: string): Variant;
begin
  Result := inherited InternalGetProperty(AName);
  
  if Result=Unassigned then
    if AName = PROP_SPECIMEN_IS_LIFESCIENCES then
      Result := LifeSciences;
end;  // TSpecimenLeafNode.InternalGetProperty 

{-------------------------------------------------------------------------------
}
procedure TSpecimenLeafNode.InternalInitialiseAddNode;
var
  //frmDialog: TFrameAddSpecimen;
  dialog : TForm;
  btnYes : TButton;
  btnNo : TButton;
begin
  inherited InternalInitialiseAddNode;
  // New Specimen Dialog. Same as Top Level InitializeNewNode.
  dialog := CreateMessageDialog(
    '',
    mtCustom,
    [mbYes, mbNo]);
  btnYes := TButton(dialog.FindComponent('Yes'));
  btnNo := TButton(dialog.FindComponent('No'));
  btnYes.Caption := 'Use Taxon Dictionary';
  btnYes.Width := 60 + btnYes.Width;
  btnYes.Left := 25;
  btnYes.Top := 25;

  btnNo.Caption := 'Use Thesaurus';
  btnNo.Width := btnYes.Width;
  btnNo.Left := 25;
  btnNo.Top := btnYes.Top + btnYes.Height + 15;

  dialog.Width := btnYes.Width + 50;
  dialog.Height := btnYes.Height + btnNo.Height + 85;
  dialog.Caption := 'New Specimen';

  //frmDialog := TFrameAddSpecimen.Create(Tree.Owner);

  with dialog do
  try
    case dialog.ShowModal of
      mrYes : LifeSciences := true;
      mrNo  : LifeSciences := false;
    else begin
        PostMessage(TWinControl(Tree.Owner).Handle, WM_REMOVE_DELETED_NODE,
                        integer(self), 0);
        Abort;
      end;
    end;
  finally
    Free;
  end;
end;  // TSpecimenLeafNode.InternalInitialiseAddNode 

{-------------------------------------------------------------------------------
}
procedure TSpecimenLeafNode.InternalInitialiseLinkNode(const AKey: string = ''; const
    ACaption: string ='');
begin
  LifeSciences := dmGeneral.GetStoredProcOutputParam(
      'usp_SpecimenIsLifeSciences_ByKey_Get',
     ['@Key', AKey], '@LifeSciences');
  
  inherited InternalInitialiseLinkNode(AKey, ACaption);
end;  // TSpecimenLeafNode.InternalInitialiseLinkNode

{-------------------------------------------------------------------------------
}
procedure TSpecimenLeafNode.SetCaption(ARecordset: _Recordset);
begin
  if ARecordset.Fields['Number'].Value = Null then begin
    if ARecordset.Fields['Item_Name'].Value = Null then
      Caption := ResStr_NoDetermination
    else
      Caption := ARecordset.Fields['Item_Name'].Value;
    end
  else begin
    if ARecordset.Fields['Item_Name'].Value = Null then
      Caption := ResStr_NoDetermination + ' - ' +
          ARecordset.Fields['Number'].Value
    else
      Caption := ARecordset.Fields['Item_Name'].Value + ' - ' +
                            ARecordset.Fields['Number'].Value;
  end;

  Caption := FormatSpecimenCaption(
      Caption,
      VarToStr(ARecordset.Fields['NomenclaturalStatus'].Value));
end;  // TSpecimenLeafNode.SetCaption

{-------------------------------------------------------------------------------
}
procedure TSpecimenLeafNode.SetConceptCaption(const Value: String);
begin
  Text := Value;
end;  // TSpecimenLeafNode.SetConceptCaption 

{-==============================================================================
    TStoreLeafNode
===============================================================================}
{-------------------------------------------------------------------------------
}
constructor TStoreLeafNode.Create(AOwner: TTreeCollection);
begin
  inherited Create(AOwner);
  
  //Default to normal draw
  FDrawHierarchically := false;
  FImageIndex := 4;
  FHierarchyDrawSpacer := 0;
end;  // TStoreLeafNode.Create

{-------------------------------------------------------------------------------
}
class function TStoreLeafNode.ClassTableName: String;
begin
  Result := TN_STORE;
end;  // TStoreLeafNode.ClassTableName 

{-------------------------------------------------------------------------------
}
function TStoreLeafNode.GetAssociatedFrame: TBaseDetailFrameClass;
begin
  Result := TfraStorage;
end;  // TStoreLeafNode.GetAssociatedFrame 

{-------------------------------------------------------------------------------
}
function TStoreLeafNode.GetCanDelete: Boolean;
begin
  Result := not (TopNodeContext in [ncSpecimen, ncStore, ncJob]) and
            inherited GetCanDelete;
end;  // TStoreLeafNode.GetCanDelete 

{-------------------------------------------------------------------------------
}
function TStoreLeafNode.GetImageIndex: Integer;
begin
  Result := FImageIndex;
end;  // TStoreLeafNode.GetImageIndex 

{-------------------------------------------------------------------------------
}
function TStoreLeafNode.GetNodeContext: TNodeContext;
begin
  Result := ncStore;
end;  // TStoreLeafNode.GetNodeContext 

{-------------------------------------------------------------------------------
  If DrawHierarchically is turned on then turn off the imageindex to allow the owner draw to
      take-over, else turn on the Store imageindex.
}
procedure TStoreLeafNode.SetDrawHierarchically(Value: Boolean);
begin
  FDrawHierarchically := Value;

  if Value then
    FImageIndex := -1
  else
    FImageIndex := 4;
end;  // TStoreLeafNode.SetDrawHierarchically 

{-------------------------------------------------------------------------------
  Accessor method.
}
procedure TStoreLeafNode.SetHierarchyDrawSpacer(Value: Integer);
begin
  FHierarchyDrawSpacer := Value;
end;  // TStoreLeafNode.SetHierarchyDrawSpacer 



{-==============================================================================
    TCollectionUnitTopLevelNode
===============================================================================}
{-------------------------------------------------------------------------------
}
function TCollectionUnitTopLevelNode.GetDomainStoredProc: String;
begin
  Result := 'usp_CollectionUnit_DomainMask_Get';
end;  // TCollectionUnitTopLevelNode.GetDomainStoredProc 

{-------------------------------------------------------------------------------
}
procedure TCollectionUnitTopLevelNode.SetCaption(ARecordset: _Recordset);
begin
  if ARecordset.Fields['Number'].Value = Null then
    Caption:= ARecordset.Fields['Item_Name'].Value
  else
    Caption:= ARecordset.Fields['Item_Name'].Value + ' - ' +
              ARecordset.Fields['Number'].Value;
end;  // TCollectionUnitTopLevelNode.SetCaption 

{-------------------------------------------------------------------------------
}
function TCollectionUnitTopLevelNode.StoredProcByChildType(ALeafNodeClass: TLeafNodeClass):
    String;
begin
  Result := '';
  raise EBrowserNodeError.Create(ResStr_InvalidChildNodeTypeIndexRequest);
end;  // TCollectionUnitTopLevelNode.StoredProcByChildType 

{-==============================================================================
    TCollectionTopLevelNode
===============================================================================}
{-------------------------------------------------------------------------------
}
procedure TCollectionTopLevelNode.AddChildNodesOfType(ABrowserNodeClass: TBrowserNodeClass);
var
  lNewNode: TFolderNode;
begin
  if (ABrowserNodeClass = TAccessionFolderNode) or
     (ABrowserNodeClass = TConditionCheckFolderNode) or
     (ABrowserNodeClass = TEnquiryFolderNode) or (ABrowserNodeClass = THistoryFolderNode) or
     (ABrowserNodeClass = TCollectionLinkedFolderNode) or
     (ABrowserNodeClass = TLoanFolderNode) or (ABrowserNodeClass = TMeasurementFolderNode) or
     (ABrowserNodeClass = TMultimediaFolderNode) or
     (ABrowserNodeClass = TNumberingHistoryFolderNode) or
     (ABrowserNodeClass = TPeopleOrganisationFolderNode) or
     (ABrowserNodeClass = TProcessFolderNode) or (ABrowserNodeClass = TSpecimenFolderNode) or
     (ABrowserNodeClass = TStoreFolderNode) or
     ((ABrowserNodeClass = TValuationFolderNode) and AppSettings.AllowFinance) or
     (ABrowserNodeClass = TMovementFolderNode) then
  begin
    lNewNode:= TFolderNode(Tree.Items.AddTypedChild(Self, ABrowserNodeClass));
    lNewNode.Initialise(Key);
  end else
  if ((ABrowserNodeClass = TValuationFolderNode) and not AppSettings.AllowFinance) then
    Exit //Do nothing
  else
    raise EBrowserNodeError.Create(ResStr_InvalidChildNodeTypeIndexRequest);
end;  // TCollectionTopLevelNode.AddChildNodesOfType 

{-------------------------------------------------------------------------------
}
class function TCollectionTopLevelNode.ClassTableName: String;
begin
  Result := TN_COLLECTION;
end;  // TCollectionTopLevelNode.ClassTableName 

{-------------------------------------------------------------------------------
}
function TCollectionTopLevelNode.GetAssociatedFrame: TBaseDetailFrameClass;
begin
  Result := TfraCollection;
end;  // TCollectionTopLevelNode.GetAssociatedFrame 

{-------------------------------------------------------------------------------
}
function TCollectionTopLevelNode.GetChildNodeType(Index: Integer): TBrowserNodeClass;
begin
  case Index of
    0: Result := TAccessionFolderNode;
    1: Result := TConditionCheckFolderNode;
    2: Result := TEnquiryFolderNode;
    3: Result := THistoryFolderNode;
    4: Result := TCollectionLinkedFolderNode;
    5: Result := TLoanFolderNode;
    6: Result := TMeasurementFolderNode;
    7: Result := TMovementFolderNode;
    8: Result := TMultimediaFolderNode;
    9: Result := TNumberingHistoryFolderNode;
    10: Result := TPeopleOrganisationFolderNode;
    11: Result := TProcessFolderNode;
    12: Result := TSpecimenFolderNode;
    13: Result := TStoreFolderNode;
    14: Result := TValuationFolderNode;
  else
    raise EBrowserNodeError.Create(ResStr_InvalidChildNodeTypeIndexRequest);
  end;
end;  // TCollectionTopLevelNode.GetChildNodeType 

{-------------------------------------------------------------------------------
}
function TCollectionTopLevelNode.GetChildNodeTypeCount: Integer;
begin
  Result := 15;
end;  // TCollectionTopLevelNode.GetChildNodeTypeCount 

{-------------------------------------------------------------------------------
}
function TCollectionTopLevelNode.GetImageIndex: Integer;
begin
  Result := 2;
end;  // TCollectionTopLevelNode.GetImageIndex 

{-------------------------------------------------------------------------------
}
function TCollectionTopLevelNode.GetNodeContext: TNodeContext;
begin
  Result := ncCollection;
end;  // TCollectionTopLevelNode.GetNodeContext 

{-------------------------------------------------------------------------------
}
function TCollectionTopLevelNode.GetProperty(const AName: string): Variant;
begin
  Result := InternalGetProperty(AName);
end;  // TCollectionTopLevelNode.GetProperty 

{-------------------------------------------------------------------------------
}
function TCollectionTopLevelNode.InternalGetProperty(const AName: string): Variant;
begin
  Result := inherited InternalGetProperty(AName);
  
  if Result = Unassigned then begin
    if AName = PROP_NODE_CONTEXT then
      Result := ncCollection;
  end;
end;  // TCollectionTopLevelNode.InternalGetProperty

{-==============================================================================
    TSpecimenTopLevelNode
===============================================================================}
{-------------------------------------------------------------------------------
}
procedure TSpecimenTopLevelNode.AddChildNodesOfType(ABrowserNodeClass: TBrowserNodeClass);
var
  lNewNode: TFolderNode;
begin
  if (ABrowserNodeClass = TAccessionFolderNode) or
     (ABrowserNodeClass = TCollectionFolderNode) or
     (ABrowserNodeClass = TConditionCheckFolderNode) or
     (ABrowserNodeClass = TDeterminationFolderNode) or
     (ABrowserNodeClass = TEnquiryFolderNode) or (ABrowserNodeClass = TFieldDataFolderNode) or
     (ABrowserNodeClass = THistoryFolderNode) or
     (ABrowserNodeClass = TInscriptionLabelFolderNode) or
     (ABrowserNodeClass = TJobAppliedToFolderNode) or (ABrowserNodeClass = TLoanFolderNode) or
     (ABrowserNodeClass = TMeasurementFolderNode) or
     (ABrowserNodeClass = TMovementFolderNode) or
     (ABrowserNodeClass = TMultimediaFolderNode) or
     (ABrowserNodeClass = TNumberingHistoryFolderNode) or
     (ABrowserNodeClass = TSpecimenLinkedFolderNode) or
     (ABrowserNodeClass = TProcessFolderNode) or
     ((ABrowserNodeClass = TValuationFolderNode) and AppSettings.AllowFinance) or
     (ABrowserNodeClass = TStoragePlaceFolderNode) or
     (ABrowserNodeClass = TPeopleOrganisationFolderNode) then
  begin
    lNewNode:= TFolderNode(Tree.Items.AddTypedChild(Self, ABrowserNodeClass));
    lNewNode.Initialise(Key);
  end else
  if ((ABrowserNodeClass = TValuationFolderNode) and not AppSettings.AllowFinance) then
    Exit //Do nothing
  else
    raise EBrowserNodeError.Create(ResStr_InvalidChildNodeTypeIndexRequest);
end;  // TSpecimenTopLevelNode.AddChildNodesOfType 

{-------------------------------------------------------------------------------
}
class function TSpecimenTopLevelNode.ClassTableName: String;
begin
  Result := TN_SPECIMEN_UNIT;
end;  // TSpecimenTopLevelNode.ClassTableName

{-------------------------------------------------------------------------------
}
function TSpecimenTopLevelNode.GetAssociatedFrame: TBaseDetailFrameClass;
begin
  Result := TfraSpecimen;
end;  // TSpecimenTopLevelNode.GetAssociatedFrame 

{-------------------------------------------------------------------------------
}
function TSpecimenTopLevelNode.GetChildNodeType(Index: Integer): TBrowserNodeClass;
begin
  case Index of
    0: Result := TAccessionFolderNode;
    1: Result := TCollectionFolderNode;
    2: Result := TConditionCheckFolderNode;
    3: Result := TDeterminationFolderNode;
    4: Result := TEnquiryFolderNode;
    5: Result := TFieldDataFolderNode;
    6: Result := THistoryFolderNode;
    7: Result := TInscriptionLabelFolderNode;
    8: Result := TJobAppliedToFolderNode;
    9: Result := TSpecimenLinkedFolderNode;
    10: Result := TLoanFolderNode;
    11: Result := TMeasurementFolderNode;
    12: Result := TMovementFolderNode;
    13: Result := TMultimediaFolderNode;
    14: Result := TNumberingHistoryFolderNode;
    15: Result := TPeopleOrganisationFolderNode;
    16: Result := TProcessFolderNode;
    17: Result := TStoragePlaceFolderNode;
    18: Result := TValuationFolderNode;
  else
    raise EBrowserNodeError.Create(ResStr_InvalidChildNodeTypeIndexRequest);
  end;
end;  // TSpecimenTopLevelNode.GetChildNodeType 

{-------------------------------------------------------------------------------
}
function TSpecimenTopLevelNode.GetChildNodeTypeCount: Integer;
begin
  Result := 19;
end;  // TSpecimenTopLevelNode.GetChildNodeTypeCount 

{-------------------------------------------------------------------------------
}
function TSpecimenTopLevelNode.GetConceptCaption: String;
begin
  Result := Text;
end;  // TSpecimenTopLevelNode.GetConceptCaption 

{-------------------------------------------------------------------------------
}
function TSpecimenTopLevelNode.GetImageIndex: Integer;
begin
  Result := 3;
end;  // TSpecimenTopLevelNode.GetImageIndex 

{-------------------------------------------------------------------------------
}
function TSpecimenTopLevelNode.GetNodeContext: TNodeContext;
begin
  Result := ncSpecimen;
end;  // TSpecimenTopLevelNode.GetNodeContext 

{-------------------------------------------------------------------------------
}
function TSpecimenTopLevelNode.GetProperty(const AName: string): Variant;
begin
  Result := InternalGetProperty(AName);
end;  // TSpecimenTopLevelNode.GetProperty 

{-------------------------------------------------------------------------------
}
function TSpecimenTopLevelNode.InternalGetProperty(const AName: string): Variant;
begin
  Result := inherited InternalGetProperty(AName);
  
  if Result = Unassigned then begin
    if AName = PROP_SPECIMEN_IS_LIFESCIENCES then
      Result := LifeSciences;
    if AName = PROP_NODE_CONTEXT then
      Result := ncSpecimen;
  end;
end;  // TSpecimenTopLevelNode.InternalGetProperty

{-------------------------------------------------------------------------------
}
procedure TSpecimenTopLevelNode.InternalInialiseNewNode;
var
  dialog : TForm;
  btnYes : TButton;
  btnNo : TButton;
begin
  inherited InternalInialiseNewNode;
  dialog := CreateMessageDialog(
    '',
    mtCustom,
    [mbYes, mbNo]);
  btnYes := TButton(dialog.FindComponent('Yes'));
  btnNo := TButton(dialog.FindComponent('No'));
  btnYes.Caption := 'Use Taxon Dictionary';
  btnYes.Width := 60 + btnYes.Width;
  btnYes.Left := 25;
  btnYes.Top := 25;

  btnNo.Caption := 'Use Thesaurus';
  btnNo.Width := btnYes.Width;
  btnNo.Left := 25;
  btnNo.Top := btnYes.Top + btnYes.Height + 15;

  dialog.Width := btnYes.Width + 50;
  dialog.Height := btnYes.Height + btnNo.Height + 85;
  dialog.Caption := 'New Specimen';

  case dialog.ShowModal of // ConfirmYesNo(ResStr_LifeSciencesSpecimenQu) of
    mrYes : LifeSciences := true;
    mrNo  : LifeSciences := false;
  else begin
      PostMessage(TWinControl(Tree.Owner).Handle, WM_REMOVE_DELETED_NODE,
                      integer(self), 0);
      Abort;
    end;
  end;
end;  // TSpecimenTopLevelNode.InternalInialiseNewNode 

{-------------------------------------------------------------------------------
}
procedure TSpecimenTopLevelNode.InternalInitialise(ARecordset: _Recordset);
begin
  inherited InternalInitialise(ARecordset);
  
  LifeSciences := ARecordset.Fields['Life_Sciences'].Value;
end;  // TSpecimenTopLevelNode.InternalInitialise

procedure TSpecimenTopLevelNode.InternalRefreshCaption;
begin
  inherited;
end; // TSpecimenTopLevelNode.InternalRefreshCaption

{-------------------------------------------------------------------------------
}
procedure TSpecimenTopLevelNode.SetCaption(ARecordset: _Recordset);
begin
  if ARecordset.Fields['Number'].Value = Null then begin
    if ARecordset.Fields['Item_Name'].Value = Null then
      Caption := ResStr_NoDetermination
    else
      Caption := ARecordset.Fields['Item_Name'].Value;
    end
  else begin
    if ARecordset.Fields['Item_Name'].Value = Null then
      Caption := ResStr_NoDetermination + ' - ' +
          ARecordset.Fields['Number'].Value
    else
      Caption := ARecordset.Fields['Item_Name'].Value + ' - ' +
                            ARecordset.Fields['Number'].Value;
  end;

  Caption := FormatSpecimenCaption(
      Caption,
      VarToStr(ARecordset.Fields['NomenclaturalStatus'].Value));
end;  // TSpecimenTopLevelNode.SetCaption 

{-------------------------------------------------------------------------------
}
procedure TSpecimenTopLevelNode.SetConceptCaption(const Value: String);
begin
  Text := Value;
end;  // TSpecimenTopLevelNode.SetConceptCaption 

{-==============================================================================
    TStoreTopLevelNode
===============================================================================}
{-------------------------------------------------------------------------------
}
procedure TStoreTopLevelNode.AddChildNodesOfType(ABrowserNodeClass: TBrowserNodeClass);
var
  lNewNode: TFolderNode;
begin
  if (ABrowserNodeClass = TAccessionFolderNode) or
     (ABrowserNodeClass = TCollectionFolderNode) or
     (ABrowserNodeClass = TConditionCheckFolderNode) or
     (ABrowserNodeClass = TEnquiryFolderNode) or (ABrowserNodeClass = THistoryFolderNode) or
     (ABrowserNodeClass = TLoanFolderNode) or (ABrowserNodeClass = TMeasurementFolderNode) or
     (ABrowserNodeClass = TMovementFolderNode) or
     (ABrowserNodeClass = TMultimediaFolderNode) or
     (ABrowserNodeClass = TNumberingHistoryFolderNode) or
     (ABrowserNodeClass = TProcessFolderNode) or
     (ABrowserNodeClass = TStoreLinkedFolderNode) or
     (ABrowserNodeClass = TSpecimenFolderNode) then
  begin
    lNewNode:= TFolderNode(Tree.Items.AddTypedChild(Self, ABrowserNodeClass));
    lNewNode.Initialise(Key);
  end else
    raise EBrowserNodeError.Create(ResStr_InvalidChildNodeTypeIndexRequest);
end;  // TStoreTopLevelNode.AddChildNodesOfType 

{-------------------------------------------------------------------------------
}
class function TStoreTopLevelNode.ClassTableName: String;
begin
  Result := TN_STORE;
end;  // TStoreTopLevelNode.ClassTableName 

{-------------------------------------------------------------------------------
}
function TStoreTopLevelNode.GetAssociatedFrame: TBaseDetailFrameClass;
begin
  Result := TfraStorage;
end;  // TStoreTopLevelNode.GetAssociatedFrame 

{-------------------------------------------------------------------------------
}
function TStoreTopLevelNode.GetChildNodeType(Index: Integer): TBrowserNodeClass;
begin
  case Index of
    0: Result := TAccessionFolderNode;
    1: Result := TCollectionFolderNode;
    2: Result := TConditionCheckFolderNode;
    3: Result := TEnquiryFolderNode;
    4: Result := THistoryFolderNode;
    5: Result := TStoreLinkedFolderNode;
    6: Result := TLoanFolderNode;
    7: Result := TMeasurementFolderNode;
    8: Result := TMovementFolderNode;
    9: Result := TMultimediaFolderNode;
    10: Result := TNumberingHistoryFolderNode;
    11: Result := TProcessFolderNode;
    12: Result := TSpecimenFolderNode;
  else
    raise EBrowserNodeError.Create(ResStr_InvalidChildNodeTypeIndexRequest);
  end;
end;  // TStoreTopLevelNode.GetChildNodeType 

{-------------------------------------------------------------------------------
}
function TStoreTopLevelNode.GetChildNodeTypeCount: Integer;
begin
  Result := 13;
end;  // TStoreTopLevelNode.GetChildNodeTypeCount 

{-------------------------------------------------------------------------------
}
function TStoreTopLevelNode.GetImageIndex: Integer;
begin
  Result := 4;
end;  // TStoreTopLevelNode.GetImageIndex 

{-------------------------------------------------------------------------------
}
function TStoreTopLevelNode.GetNodeContext: TNodeContext;
begin
  Result := ncStore;
end;  // TStoreTopLevelNode.GetNodeContext 

{-------------------------------------------------------------------------------
}
function TStoreTopLevelNode.GetProperty(const AName: string): Variant;
begin
  Result := InternalGetProperty(AName);
end;  // TStoreTopLevelNode.GetProperty 

{-------------------------------------------------------------------------------
}
function TStoreTopLevelNode.InternalGetProperty(const AName: string): Variant;
begin
  Result := inherited InternalGetProperty(AName);
  
  if Result = Unassigned then begin
    if AName = PROP_NODE_CONTEXT then
      Result := ncStore;
  end;
end;  // TStoreTopLevelNode.InternalGetProperty 


{-==============================================================================
    TCollectionUnitLinkedFolderNode
===============================================================================}
{-------------------------------------------------------------------------------
}
function TCollectionUnitLinkedFolderNode.GetCanAdd: Boolean;
begin
  Result := False;
end;  // TCollectionUnitLinkedFolderNode.GetCanAdd 

{-==============================================================================
    TCollectionLinkedFolderNode
===============================================================================}
{-------------------------------------------------------------------------------
}
procedure TCollectionLinkedFolderNode.AddChildNodesOfType(ABrowserNodeClass:
    TBrowserNodeClass);
var
  lNewNode: TFolderNode;
begin
  if (ABrowserNodeClass = TCollectionLinkedIncludedInFolderNode) or
     (ABrowserNodeClass = TCollectionLinkedIncludesFolderNode) or
     (ABrowserNodeClass = TCollectionLinkedOtherFolderNode) then begin
    lNewNode:= TFolderNode(Tree.Items.AddTypedChild(Self, ABrowserNodeClass));
    lNewNode.Initialise(Key);
  end else
    raise EBrowserNodeError.Create(ResStr_InvalidChildNodeTypeIndexRequest);
end;  // TCollectionLinkedFolderNode.AddChildNodesOfType 

{-------------------------------------------------------------------------------
}
class function TCollectionLinkedFolderNode.ClassTableName: String;
begin
  Result := '';
end;  // TCollectionLinkedFolderNode.ClassTableName 

{-------------------------------------------------------------------------------
}
function TCollectionLinkedFolderNode.GetChildNodeType(Index: Integer): TBrowserNodeClass;
begin
  case Index of
    0: Result := TCollectionLinkedIncludedInFolderNode;
    1: Result := TCollectionLinkedIncludesFolderNode;
    2: Result := TCollectionLinkedOtherFolderNode;
  else
    raise EBrowserNodeError.Create(ResStr_InvalidChildNodeTypeIndexRequest);
  end;
end;  // TCollectionLinkedFolderNode.GetChildNodeType 

{-------------------------------------------------------------------------------
}
function TCollectionLinkedFolderNode.GetChildNodeTypeCount: Integer;
begin
  Result:= 3;
end;  // TCollectionLinkedFolderNode.GetChildNodeTypeCount 

{-------------------------------------------------------------------------------
}
function TCollectionLinkedFolderNode.GetSearchType: SearchManager.TSearchType;
begin
  Result := stCollection;
end;  // TCollectionLinkedFolderNode.GetSearchType 

{-------------------------------------------------------------------------------
}
procedure TCollectionLinkedFolderNode.SetCaption;
begin
  Text := ResStr_LinkedCollections;
end;  // TCollectionLinkedFolderNode.SetCaption 

{-------------------------------------------------------------------------------
}
function TCollectionLinkedFolderNode.StoredProcByChildType(ALeafNodeClass: TLeafNodeClass):
    String;
begin
  Result := '';
  raise EBrowserNodeError.Create(ResStr_InvalidChildNodeTypeIndexRequest);
end;  // TCollectionLinkedFolderNode.StoredProcByChildType 

{-==============================================================================
    TStoreLinkedFolderNode
===============================================================================}
{-------------------------------------------------------------------------------
}
procedure TStoreLinkedFolderNode.AddChildNodesOfType(ABrowserNodeClass: TBrowserNodeClass);
var
  lNewNode: TFolderNode;
begin
  if (ABrowserNodeClass = TStoragePlaceFolderNode) or
      (ABrowserNodeClass = TStoreHierarchyFolderNode) or
        (ABrowserNodeClass = TStoreLinkedOtherFolderNode) then begin
    lNewNode:= TFolderNode(Tree.Items.AddTypedChild(Self, ABrowserNodeClass));
    lNewNode.Initialise(Key);
  end
  else
    raise EBrowserNodeError.Create(ResStr_InvalidChildNodeTypeIndexRequest);
end;  // TStoreLinkedFolderNode.AddChildNodesOfType 

{-------------------------------------------------------------------------------
}
class function TStoreLinkedFolderNode.ClassTableName: String;
begin
  Result := '';
end;  // TStoreLinkedFolderNode.ClassTableName 

{-------------------------------------------------------------------------------
}
function TStoreLinkedFolderNode.GetChildNodeType(Index: Integer): TBrowserNodeClass;
begin
  case Index of
    0: Result := TStoragePlaceFolderNode;
    1: Result := TStoreHierarchyFolderNode;
    2: Result := TStoreLinkedOtherFolderNode;
  else
    raise EBrowserNodeError.Create(ResStr_InvalidChildNodeTypeIndexRequest);
  end;
end;  // TStoreLinkedFolderNode.GetChildNodeType 

{-------------------------------------------------------------------------------
}
function TStoreLinkedFolderNode.GetChildNodeTypeCount: Integer;
begin
  Result:= 3;
end;  // TStoreLinkedFolderNode.GetChildNodeTypeCount 

{-------------------------------------------------------------------------------
}
function TStoreLinkedFolderNode.GetSearchType: SearchManager.TSearchType;
begin
  Result := stStoreName;
end;  // TStoreLinkedFolderNode.GetSearchType

{-------------------------------------------------------------------------------
}
procedure TStoreLinkedFolderNode.SetCaption;
begin
  Text := ResStr_StoresLinked;
end;  // TStoreLinkedFolderNode.SetCaption 

{-------------------------------------------------------------------------------
}
function TStoreLinkedFolderNode.StoredProcByChildType(ALeafNodeClass: TLeafNodeClass): String;
begin
  Result := '';
  raise EBrowserNodeError.Create(ResStr_InvalidChildNodeTypeIndexRequest);
end;  // TStoreLinkedFolderNode.StoredProcByChildType 

{-==============================================================================
    TSpecimenLinkedFolderNode
===============================================================================}
{-------------------------------------------------------------------------------
}
constructor TSpecimenLinkedFolderNode.Create(AOwner: TTreeCollection);
begin
  inherited Create(AOwner);
  FCommonNameFetchQueue := TCommonNameFetchQueue.Create(Tree);
  FCommonNameFetchQueue.Parent := Tree;
  FCommonNameFetchQueue.TreeView := TRapidTree(Tree);
end;  // TSpecimenLinkedFolderNode.Create 

{-------------------------------------------------------------------------------
}
destructor TSpecimenLinkedFolderNode.Destroy;
begin
  FCommonNameFetchQueue.Free;
  inherited Destroy;
end;  // TSpecimenLinkedFolderNode.Destroy 

{-------------------------------------------------------------------------------
}
class function TSpecimenLinkedFolderNode.ClassTableName: String;
begin
  Result := TN_SPECIMEN_UNIT;
end;  // TSpecimenLinkedFolderNode.ClassTableName 

{-------------------------------------------------------------------------------
}
function TSpecimenLinkedFolderNode.GetAddButtonMenuCaption(Index: Integer): String;
begin
  case Index of
    0: Result := ResStr_Add;
  else
    raise EAddMenuItemError.Create(ResStr_InvalidAddMenuIndexRequest);
  end;
end;  // TSpecimenLinkedFolderNode.GetAddButtonMenuCaption 

{-------------------------------------------------------------------------------
}
function TSpecimenLinkedFolderNode.GetAddButtonMenuCaptionsCount: Integer;
begin
  Result := 1;
end;  // TSpecimenLinkedFolderNode.GetAddButtonMenuCaptionsCount 

{-------------------------------------------------------------------------------
}
function TSpecimenLinkedFolderNode.GetAddMenuIsAdd(AMenuIndex: Integer): Boolean;
begin
  case AMenuIndex of
    0 : Result := True
  else
    Result := False;
  end;
end;  // TSpecimenLinkedFolderNode.GetAddMenuIsAdd 

{-------------------------------------------------------------------------------
}
function TSpecimenLinkedFolderNode.GetCanAdd: Boolean;
begin
  Result := (AppSettings.AddDomainMask > 0);
end;  // TSpecimenLinkedFolderNode.GetCanAdd 

{-------------------------------------------------------------------------------
}
function TSpecimenLinkedFolderNode.GetChildNodeType(Index: Integer): TBrowserNodeClass;
begin
  case Index of
    0: Result := TRelatedCollectionUnitLeafNode;
  else
    raise EBrowserNodeError.Create(ResStr_InvalidChildNodeTypeIndexRequest);
  end;
end;  // TSpecimenLinkedFolderNode.GetChildNodeType 

{-------------------------------------------------------------------------------
}
function TSpecimenLinkedFolderNode.GetChildNodeTypeCount: Integer;
begin
  Result:= 1;
end;  // TSpecimenLinkedFolderNode.GetChildNodeTypeCount 

{-------------------------------------------------------------------------------
}
function TSpecimenLinkedFolderNode.GetNodeContext: TNodeContext;
begin
  Result := ncSpecimen;
end;  // TSpecimenLinkedFolderNode.GetNodeContext 

{-------------------------------------------------------------------------------
}
function TSpecimenLinkedFolderNode.GetSearchType: TSearchType;
begin
  Result := stSpecimen;
end;  // TSpecimenLinkedFolderNode.GetSearchType 

{-------------------------------------------------------------------------------
}
function TSpecimenLinkedFolderNode.GetStoredProcParams: TVariantArray;
var
  SortIndex: Integer;
begin
  SortIndex := ViewTypeManager.ViewTypeByNodeContext(
      NodeContext).SortOrderDefaultIndex;
  
  Result:= VarArrayOf(['@ParentKey', Key, '@ShowCommonNames',
      AppSettings.DisplayCommonNames, '@SortOrderIndex', SortIndex]);
end;  // TSpecimenLinkedFolderNode.GetStoredProcParams 

{-------------------------------------------------------------------------------
}
procedure TSpecimenLinkedFolderNode.SetCaption;
begin
  Text := ResStr_SpecimensLinked;
end;  // TSpecimenLinkedFolderNode.SetCaption 

{-------------------------------------------------------------------------------
}
function TSpecimenLinkedFolderNode.StoredProcByChildType(ALeafNodeClass: TLeafNodeClass):
    String;
begin
  Result := '';
  
  if ALeafNodeClass = TRelatedCollectionUnitLeafNode then
    Result := 'usp_Specimens_Select_ForLinkedOther'
  else
    raise EBrowserNodeError.Create(ResStr_InvalidChildNodeTypeIndexRequest);
end;  // TSpecimenLinkedFolderNode.StoredProcByChildType 

{-------------------------------------------------------------------------------
}
function TSpecimenLinkedFolderNode.ValidateNewNode(const AKey: TKeyString; const ACaption:
    string): Boolean;
begin
  Result := (AKey <> Key);
  if not Result then begin
    MessageDlg(Format(ResStr_RecursionFailureCannotBeLinkedToItself, [ResStr_Specimen]),
               mtInformation, [mbOK], 0);
    Exit;
  end;
end;  // TSpecimenLinkedFolderNode.ValidateNewNode 

{-==============================================================================
    TCollectionLinkedIncludesFolderNode
===============================================================================}
{-------------------------------------------------------------------------------
}
class function TCollectionLinkedIncludesFolderNode.ClassTableName: String;
begin
  Result := TN_COLLECTION;
end;  // TCollectionLinkedIncludesFolderNode.ClassTableName 

{-------------------------------------------------------------------------------
}
procedure TCollectionLinkedIncludesFolderNode.DeleteLink(const AJoinTableKey: String);
begin
  dmGeneral.RunStoredProc('usp_CollectionLink_Delete', ['@Key', AJoinTableKey]);
end;  // TCollectionLinkedIncludesFolderNode.DeleteLink 

{-------------------------------------------------------------------------------
}
function TCollectionLinkedIncludesFolderNode.GetAddButtonMenuCaption(Index: Integer): String;
begin
  case Index of
    0: Result := ResStr_Link;
  
  else
    raise EAddMenuItemError.Create(ResStr_InvalidAddMenuIndexRequest);
  end;
end;  // TCollectionLinkedIncludesFolderNode.GetAddButtonMenuCaption 

{-------------------------------------------------------------------------------
}
function TCollectionLinkedIncludesFolderNode.GetAddButtonMenuCaptionsCount: Integer;
begin
  Result := 1;
end;  // TCollectionLinkedIncludesFolderNode.GetAddButtonMenuCaptionsCount 

{-------------------------------------------------------------------------------
}
function TCollectionLinkedIncludesFolderNode.GetAddMenuIsAdd(AMenuIndex: Integer): Boolean;
begin
  case AMenuIndex of
    0 : Result := False
  else
    Result := True;
  end;
end;  // TCollectionLinkedIncludesFolderNode.GetAddMenuIsAdd 

{-------------------------------------------------------------------------------
}
function TCollectionLinkedIncludesFolderNode.GetChildNodeType(Index: Integer):
    TBrowserNodeClass;
begin
  case Index of
    0: Result := TCollectionLeafNode;
  else
    raise EBrowserNodeError.Create(ResStr_InvalidChildNodeTypeIndexRequest);
  end;
end;  // TCollectionLinkedIncludesFolderNode.GetChildNodeType 

{-------------------------------------------------------------------------------
}
function TCollectionLinkedIncludesFolderNode.GetChildNodeTypeCount: Integer;
begin
  Result:= 1;
end;  // TCollectionLinkedIncludesFolderNode.GetChildNodeTypeCount 

{-------------------------------------------------------------------------------
}
function TCollectionLinkedIncludesFolderNode.GetNodeContext: TNodeContext;
begin
  Result := ncCollection;
end;  // TCollectionLinkedIncludesFolderNode.GetNodeContext 

{-------------------------------------------------------------------------------
}
function TCollectionLinkedIncludesFolderNode.GetSearchType: SearchManager.TSearchType;
begin
  Result := stCollection;
end;  // TCollectionLinkedIncludesFolderNode.GetSearchType 

{-------------------------------------------------------------------------------
}
procedure TCollectionLinkedIncludesFolderNode.SetCaption;
begin
  Text := ResStr_Includes;
end;  // TCollectionLinkedIncludesFolderNode.SetCaption 

{-------------------------------------------------------------------------------
}
function TCollectionLinkedIncludesFolderNode.StoredProcByChildType(ALeafNodeClass:
    TLeafNodeClass): String;
begin
  Result := '';
  
  if ALeafNodeClass = TCollectionLeafNode then
    Result := 'usp_Collections_Select_ForIncludes'
  else
    raise EBrowserNodeError.Create(ResStr_InvalidChildNodeTypeIndexRequest);
end;  // TCollectionLinkedIncludesFolderNode.StoredProcByChildType 

{-------------------------------------------------------------------------------
}
function TCollectionLinkedIncludesFolderNode.UpdateNodeRelationship(const NewNodeKey: String):
    String;
begin
  Result := VarToStr(dmGeneral.GetStoredProcOutputParam(
                     'usp_CollectionLink_Insert_ForLinkedIncludes',
                     ['@ParentKey', Key, '@ChildKey', NewNodeKey], '@JoinKey'));
end;  // TCollectionLinkedIncludesFolderNode.UpdateNodeRelationship 

{-------------------------------------------------------------------------------
}
function TCollectionLinkedIncludesFolderNode.ValidateNewNode(const AKey: TKeyString; const
    ACaption: string): Boolean;
begin
  Result := (dmGeneral.GetStoredProcOutputParam('usp_Collection_Contains_Get',
             ['@ContainerCollectionKey', AKey, '@ContainedCollectionKey', Key],
             '@Contains') = 0);
  
  if not Result then begin
    MessageDlg(Format(ResStr_RecursionFailureCollectionContainedBy, [ACaption]),
               mtInformation, [mbOK], 0);
    Exit;
  end;
  
  Result := (AKey <> Key);
  if not Result then begin
    MessageDlg(Format(ResStr_RecursionFailureCannotBeLinkedToItself, [ResStr_Collection]),
               mtInformation, [mbOK], 0);
    Exit;
  end;
end;  // TCollectionLinkedIncludesFolderNode.ValidateNewNode 

{-==============================================================================
    TStoreHierarchyFolderNode
===============================================================================}
{-------------------------------------------------------------------------------
}
procedure TStoreHierarchyFolderNode.AddChildNodesOfType(ABrowserNodeClass: TBrowserNodeClass);
begin
  if (ABrowserNodeClass = TStoreHierarchySubFolderNode) or
     (ABrowserNodeClass = TStoreLeafNode) then
    PopulateImmediateChild(ABrowserNodeClass,
                           StoredProcByChildType(TLeafNodeClass(ABrowserNodeClass)))
  else
    raise EBrowserNodeError.Create(ResStr_InvalidChildNodeTypeIndexRequest);
end;  // TStoreHierarchyFolderNode.AddChildNodesOfType 

{-------------------------------------------------------------------------------
}
class function TStoreHierarchyFolderNode.ClassTableName: String;
begin
  Result := TN_STORE;
end;  // TStoreHierarchyFolderNode.ClassTableName 

{-------------------------------------------------------------------------------
}
function TStoreHierarchyFolderNode.GetCanAdd: Boolean;
begin
  Result := False;
end;  // TStoreHierarchyFolderNode.GetCanAdd 

{-------------------------------------------------------------------------------
}
function TStoreHierarchyFolderNode.GetChildNodeType(Index: Integer): TBrowserNodeClass;
begin
  case Index of
    0: Result := TStoreHierarchySubFolderNode;
  else
    raise EBrowserNodeError.Create(ResStr_InvalidChildNodeTypeIndexRequest);
  end;
end;  // TStoreHierarchyFolderNode.GetChildNodeType

{-------------------------------------------------------------------------------
}
function TStoreHierarchyFolderNode.GetChildNodeTypeCount: Integer;
begin
  Result:= 1;
end;  // TStoreHierarchyFolderNode.GetChildNodeTypeCount 

{-------------------------------------------------------------------------------
}
function TStoreHierarchyFolderNode.GetNodeContext: TNodeContext;
begin
  Result := ncStore;
end;  // TStoreHierarchyFolderNode.GetNodeContext 

{-------------------------------------------------------------------------------
}
function TStoreHierarchyFolderNode.GetSearchType: SearchManager.TSearchType;
begin
  Result := stStoreName;
end;  // TStoreHierarchyFolderNode.GetSearchType 

{-------------------------------------------------------------------------------
}
function TStoreHierarchyFolderNode.GetStoredProcParams: TVariantArray;
var
  SortIndex: Integer;
begin
  SortIndex := ViewTypeManager.ViewTypeByNodeContext(ncStore).SortOrderDefaultIndex;
  Result := VarArrayOf(['@ParentKey', Key, '@SortOrderIndex', SortIndex]);
end;  // TStoreHierarchyFolderNode.GetStoredProcParams 

{-------------------------------------------------------------------------------
}
procedure TStoreHierarchyFolderNode.PopulateImmediateChild(ABrowserNodeClass:
    TBrowserNodeClass; const AStoredProcName: String);
var
  lRecordset: _Recordset;
  lNewNode: TBrowserNode;
begin
  lRecordset := dmGeneral.GetRecordset(AStoredProcName, GetStoredProcParams);
  
  Tree.Items.BeginUpdate;
  try
    if lRecordset.RecordCount > 0 then begin
      lRecordSet.MoveFirst;
      while not lRecordset.EOF do begin
        if (ABrowserNodeClass = TStoreHierarchySubFolderNode) then begin
          lNewNode:= TFolderNode(Tree.Items.AddTypedChild(Self,
              ABrowserNodeClass));
          TFolderNode(lNewNode).Initialise(lRecordset.Fields['Item_Key'].Value);
          if lRecordset.Fields['Number'].Value = Null then
             lNewNode.Caption:= lRecordset.Fields['Item_Name'].Value
          else
             lNewNode.Caption:= lRecordset.Fields['Item_Name'].Value + ' - ' +
                VarToStr(lRecordset.Fields['Number'].Value);
          if (lRecordset.Fields['Bottom_Level'].Value = true) then begin
            lNewNode.OverlayIndex := 0;
            lNewNode.HasChildren := False;
          end;
        end;
        lRecordset.MoveNext
      end;
    end;
  finally
    Tree.Items.EndUpdate;
    lRecordset.Close;
  end;
end;  // TStoreHierarchyFolderNode.PopulateImmediateChild 

{-------------------------------------------------------------------------------
}
procedure TStoreHierarchyFolderNode.SetCaption;
begin
  Text := ResStr_StoreHierarchy;
end;  // TStoreHierarchyFolderNode.SetCaption 

{-------------------------------------------------------------------------------
}
function TStoreHierarchyFolderNode.StoredProcByChildType(ALeafNodeClass: TLeafNodeClass):
    String;
begin
  Result := 'usp_StoreHierarchy_Child_Select_ForStore';
end;  // TStoreHierarchyFolderNode.StoredProcByChildType 


{-==============================================================================
    TCollectionUnitLinkedIncludedInFolderNode
===============================================================================}
{-------------------------------------------------------------------------------
}
function TCollectionUnitLinkedIncludedInFolderNode.GetAddButtonMenuCaption(Index: Integer):
    String;
begin
  if Index = 0 then
    Result := ResStr_Link
  else
    raise EAddMenuItemError.Create(ResStr_InvalidAddMenuIndexRequest);
end;  // TCollectionUnitLinkedIncludedInFolderNode.GetAddButtonMenuCaption 

{-------------------------------------------------------------------------------
}
function TCollectionUnitLinkedIncludedInFolderNode.GetAddButtonMenuCaptionsCount: Integer;
begin
  Result := 1;
end;  // TCollectionUnitLinkedIncludedInFolderNode.GetAddButtonMenuCaptionsCount 

{-------------------------------------------------------------------------------
}
function TCollectionUnitLinkedIncludedInFolderNode.GetAddMenuIsAdd(AMenuIndex: Integer):
    Boolean;
begin
  Result := AMenuIndex <> 0;
end;  // TCollectionUnitLinkedIncludedInFolderNode.GetAddMenuIsAdd 

{-==============================================================================
    TCollectionLinkedIncludedInFolderNode
===============================================================================}
{-------------------------------------------------------------------------------
}
class function TCollectionLinkedIncludedInFolderNode.ClassTableName: String;
begin
  Result := TN_COLLECTION;
end;  // TCollectionLinkedIncludedInFolderNode.ClassTableName 

{-------------------------------------------------------------------------------
}
procedure TCollectionLinkedIncludedInFolderNode.DeleteLink(const AJoinTableKey: String);
begin
  dmGeneral.RunStoredProc('usp_CollectionLink_Delete', ['@Key', AJoinTableKey]);
end;  // TCollectionLinkedIncludedInFolderNode.DeleteLink 

{-------------------------------------------------------------------------------
}
function TCollectionLinkedIncludedInFolderNode.GetChildNodeType(Index: Integer):
    TBrowserNodeClass;
begin
  if Index = 0 then
    Result := TCollectionLeafNode
  else
    raise EBrowserNodeError.Create(ResStr_InvalidChildNodeTypeIndexRequest);
end;  // TCollectionLinkedIncludedInFolderNode.GetChildNodeType 

{-------------------------------------------------------------------------------
}
function TCollectionLinkedIncludedInFolderNode.GetChildNodeTypeCount: Integer;
begin
  Result:= 1;
end;  // TCollectionLinkedIncludedInFolderNode.GetChildNodeTypeCount 

{-------------------------------------------------------------------------------
}
function TCollectionLinkedIncludedInFolderNode.GetNodeContext: TNodeContext;
begin
  Result := ncCollection;
end;  // TCollectionLinkedIncludedInFolderNode.GetNodeContext 

{-------------------------------------------------------------------------------
}
function TCollectionLinkedIncludedInFolderNode.GetSearchType: SearchManager.TSearchType;
begin
  Result := stCollection;
end;  // TCollectionLinkedIncludedInFolderNode.GetSearchType 

{-------------------------------------------------------------------------------
}
procedure TCollectionLinkedIncludedInFolderNode.SetCaption;
begin
  Text := ResStr_IncludedIn;
end;  // TCollectionLinkedIncludedInFolderNode.SetCaption 

{-------------------------------------------------------------------------------
}
function TCollectionLinkedIncludedInFolderNode.StoredProcByChildType(ALeafNodeClass:
    TLeafNodeClass): String;
begin
  Result := '';
  
  if ALeafNodeClass = TCollectionLeafNode then
    Result := 'usp_Collections_Select_ForIncludedIn'
  else
    raise EBrowserNodeError.Create(ResStr_InvalidChildNodeTypeIndexRequest);
end;  // TCollectionLinkedIncludedInFolderNode.StoredProcByChildType 

{-------------------------------------------------------------------------------
}
function TCollectionLinkedIncludedInFolderNode.UpdateNodeRelationship(const NewNodeKey:
    String): String;
begin
  Result := VarToStr(dmGeneral.GetStoredProcOutputParam(
                     'usp_CollectionLink_Insert_ForLinkedIncludes',
                     ['@ParentKey', NewNodeKey, '@ChildKey', Key], '@JoinKey'));
  EnforceSingleChildNode(NewNodeKey);
end;  // TCollectionLinkedIncludedInFolderNode.UpdateNodeRelationship 

{-------------------------------------------------------------------------------
}
function TCollectionLinkedIncludedInFolderNode.ValidateNewNode(const AKey: TKeyString; const
    ACaption: string): Boolean;
begin
  Result := dmGeneral.GetStoredProcOutputParam('usp_Collection_Contains_Get',
                ['@ContainerCollectionKey', Key, '@ContainedCollectionKey', AKey],
                '@Contains') = 0;
  
  if not Result then begin
    MessageDlg(Format(ResStr_RecursionFailureCollectionContains, [ACaption]),
               mtInformation, [mbOK], 0);
    Exit;
  end;
  
  Result := (AKey <> Key);
  if not Result then begin
    MessageDlg(Format(ResStr_RecursionFailureCannotBeLinkedToItself, [ResStr_Collection]),
               mtInformation, [mbOK], 0);
    Exit;
  end;
end;  // TCollectionLinkedIncludedInFolderNode.ValidateNewNode 

{-==============================================================================
    TStoragePlaceFolderNode
===============================================================================}
{-------------------------------------------------------------------------------
}
class function TStoragePlaceFolderNode.ClassTableName: String;
begin
  Result := TN_STORE;
end;  // TStoragePlaceFolderNode.ClassTableName 

{-------------------------------------------------------------------------------
}
function TStoragePlaceFolderNode.GetCanAdd: Boolean;
begin
  Result := False;
end;  // TStoragePlaceFolderNode.GetCanAdd 

{-------------------------------------------------------------------------------
}
function TStoragePlaceFolderNode.GetChildNodeType(Index: Integer): TBrowserNodeClass;
begin
  if Index = 0 then
    Result := TStoreLeafNode
  else
    raise EBrowserNodeError.Create(ResStr_InvalidChildNodeTypeIndexRequest);
end;  // TStoragePlaceFolderNode.GetChildNodeType 

{-------------------------------------------------------------------------------
}
function TStoragePlaceFolderNode.GetChildNodeTypeCount: Integer;
begin
  Result:= 1;
end;  // TStoragePlaceFolderNode.GetChildNodeTypeCount 

{-------------------------------------------------------------------------------
}
function TStoragePlaceFolderNode.GetNodeContext: TNodeContext;
begin
  Result := ncStoragePlace;
end;  // TStoragePlaceFolderNode.GetNodeContext 

{-------------------------------------------------------------------------------
}
function TStoragePlaceFolderNode.GetSearchType: SearchManager.TSearchType;
begin
  Result := stStoreName;
end;  // TStoragePlaceFolderNode.GetSearchType 

{-------------------------------------------------------------------------------
}
procedure TStoragePlaceFolderNode.PopulateFromDatabase(ALeafNodeClass: TLeafNodeClass; const
    AStoredProcName: String);
var
  lRecordset: _Recordset;
  lNewNode: TLeafNode;
  lIdx: Integer;
begin
  lRecordset := dmGeneral.GetRecordset(AStoredProcName, GetStoredProcParams);
  lIdx := 0;
  
  Tree.Items.BeginUpdate;
  try
    if lRecordset.RecordCount > 0 then begin
      lRecordSet.MoveFirst;
      while not lRecordset.EOF do begin
        lNewNode:= TLeafNode(Tree.Items.AddTypedChild(Self, ALeafNodeClass));
        //Will never return a non-LeafNode so NodeContext needn't be set.
        if ALeafNodeClass = TStoreLeafNode then begin//Should always be
          TStoreLeafNode(lNewNode).DrawHierarchically := true;
          //SmallIcon multiplier + 2 pixels default spacing either side
          TStoreLeafNode(lNewNode).HierarchyDrawSpacer := lIdx * 20;
          lIdx := lIdx + 1;
        end;
        lNewNode.Initialise(lRecordset); //Sets Key and Caption
        lRecordset.MoveNext;
      end;
    end;
  finally
    Tree.Items.EndUpdate;
    lRecordset.Close;
  end;
end;  // TStoragePlaceFolderNode.PopulateFromDatabase 

{-------------------------------------------------------------------------------
}
procedure TStoragePlaceFolderNode.SetCaption;
begin
  Text := ResStr_StoragePlace;
end;  // TStoragePlaceFolderNode.SetCaption 

{-------------------------------------------------------------------------------
}
function TStoragePlaceFolderNode.StoredProcByChildType(ALeafNodeClass: TLeafNodeClass): String;
begin
  Result := '';
  
  if ALeafNodeClass = TStoreLeafNode then
    Result := 'usp_StoragePlace_Select_ForCollectionUnit'
  else
    raise EBrowserNodeError.Create(ResStr_InvalidChildNodeTypeIndexRequest);
end;  // TStoragePlaceFolderNode.StoredProcByChildType 

{-------------------------------------------------------------------------------
}
function TStoragePlaceFolderNode.UpdateNodeRelationship(const NewNodeKey: String): String;
var
  lIsPermanentMove: Boolean;
begin
  if ConfirmYesNoDefaultNo(ResStr_IsThisAPermanentMove) = mrYes then
    lIsPermanentMove := True
  else
    lIsPermanentMove := False;
  
  Result := VarToStr(dmGeneral.GetStoredProcOutputParam(
                     'usp_StoreLink_Insert_ForLinkedIncludes',
                     ['@ParentKey', NewNodeKey, '@ChildKey', Key,
                      '@IsPermanentMove', lIsPermanentMove], '@JoinKey'));
  // Repopulate folder
  InternalRefresh;
end;  // TStoragePlaceFolderNode.UpdateNodeRelationship 

{-------------------------------------------------------------------------------
}
function TStoragePlaceFolderNode.ValidateNewNode(const AKey: TKeyString; const ACaption:
    string): Boolean;
begin
  Result := (dmGeneral.GetStoredProcOutputParam('usp_Store_Contains_Get',
             ['@ContainerStoreKey', Key, '@ContainedStoreKey', AKey, '@IsCurrentLocation', 1],
             '@Contains') = 0) or
            (dmGeneral.GetStoredProcOutputParam('usp_Store_Contains_Get',
             ['@ContainerStoreKey', Key, '@ContainedStoreKey', AKey, '@IsCurrentLocation', 0],
             '@Contains') = 0);
  
  if not Result then begin
    ShowInformation(Format(ResStr_RecursionFailureStoreContains, [ACaption]));
    Exit;
  end;
  
  Result := (AKey <> Key);
  if not Result then
    ShowInformation(Format(ResStr_RecursionFailureCannotBeLinkedToItself,
        [ResStr_Collection]));
end;  // TStoragePlaceFolderNode.ValidateNewNode 

{-==============================================================================
    TCollectionUnitLinkedOtherFolderNode
===============================================================================}
{-------------------------------------------------------------------------------
}
class function TCollectionUnitLinkedOtherFolderNode.ClassTableName: String;
begin
  Result := 'Collection_Unit_Relation';
end;  // TCollectionUnitLinkedOtherFolderNode.ClassTableName 

{-==============================================================================
    TCollectionLinkedOtherFolderNode
===============================================================================}
{-------------------------------------------------------------------------------
}
function TCollectionLinkedOtherFolderNode.GetAddButtonMenuCaption(Index: Integer): String;
begin
  if Index = 0 then
    Result := ResStr_Add
  else
    raise EAddMenuItemError.Create(ResStr_InvalidAddMenuIndexRequest);
end;  // TCollectionLinkedOtherFolderNode.GetAddButtonMenuCaption 

{-------------------------------------------------------------------------------
}
function TCollectionLinkedOtherFolderNode.GetAddButtonMenuCaptionsCount: Integer;
begin
  Result := 1;
end;  // TCollectionLinkedOtherFolderNode.GetAddButtonMenuCaptionsCount 

{-------------------------------------------------------------------------------
}
function TCollectionLinkedOtherFolderNode.GetAddMenuIsAdd(AMenuIndex: Integer): Boolean;
begin
  Result := AMenuIndex = 0;
end;  // TCollectionLinkedOtherFolderNode.GetAddMenuIsAdd 

{-------------------------------------------------------------------------------
}
function TCollectionLinkedOtherFolderNode.GetChildNodeType(Index: Integer): TBrowserNodeClass;
begin
  if Index = 0 then
    Result := TRelatedCollectionUnitLeafNode
  else
    raise EBrowserNodeError.Create(ResStr_InvalidChildNodeTypeIndexRequest);
end;  // TCollectionLinkedOtherFolderNode.GetChildNodeType 

{-------------------------------------------------------------------------------
}
function TCollectionLinkedOtherFolderNode.GetChildNodeTypeCount: Integer;
begin
  Result:= 1;
end;  // TCollectionLinkedOtherFolderNode.GetChildNodeTypeCount 

{-------------------------------------------------------------------------------
}
function TCollectionLinkedOtherFolderNode.GetNodeContext: TNodeContext;
begin
  Result := ncCollection;
end;  // TCollectionLinkedOtherFolderNode.GetNodeContext 

{-------------------------------------------------------------------------------
}
function TCollectionLinkedOtherFolderNode.GetSearchType: SearchManager.TSearchType;
begin
  Result := stCollection;
end;  // TCollectionLinkedOtherFolderNode.GetSearchType 

{-------------------------------------------------------------------------------
}
procedure TCollectionLinkedOtherFolderNode.SetCaption;
begin
  Text := ResStr_Other;
end;  // TCollectionLinkedOtherFolderNode.SetCaption 

{-------------------------------------------------------------------------------
}
function TCollectionLinkedOtherFolderNode.StoredProcByChildType(ALeafNodeClass:
    TLeafNodeClass): String;
begin
  Result := '';
  
  if ALeafNodeClass = TRelatedCollectionUnitLeafNode then
    Result := 'usp_Collections_Select_ForLinkedOther'
  else
    raise EBrowserNodeError.Create(ResStr_InvalidChildNodeTypeIndexRequest);
end;  // TCollectionLinkedOtherFolderNode.StoredProcByChildType 

{-==============================================================================
    TStoreLinkedOtherFolderNode
===============================================================================}
{-------------------------------------------------------------------------------
}
function TStoreLinkedOtherFolderNode.GetAddButtonMenuCaption(Index: Integer): String;
begin
  if Index = 0 then
    Result := ResStr_Add
  else
    raise EAddMenuItemError.Create(ResStr_InvalidAddMenuIndexRequest);
end;  // TStoreLinkedOtherFolderNode.GetAddButtonMenuCaption 

{-------------------------------------------------------------------------------
}
function TStoreLinkedOtherFolderNode.GetAddButtonMenuCaptionsCount: Integer;
begin
  Result := 1;
end;  // TStoreLinkedOtherFolderNode.GetAddButtonMenuCaptionsCount 

{-------------------------------------------------------------------------------
}
function TStoreLinkedOtherFolderNode.GetAddMenuIsAdd(AMenuIndex: Integer): Boolean;
begin
  Result := AMenuIndex = 0;
end;  // TStoreLinkedOtherFolderNode.GetAddMenuIsAdd 

{-------------------------------------------------------------------------------
}
function TStoreLinkedOtherFolderNode.GetChildNodeType(Index: Integer): TBrowserNodeClass;
begin
  if Index = 0 then
    Result := TRelatedCollectionUnitLeafNode
  else
    raise EBrowserNodeError.Create(ResStr_InvalidChildNodeTypeIndexRequest);
end;  // TStoreLinkedOtherFolderNode.GetChildNodeType 

{-------------------------------------------------------------------------------
}
function TStoreLinkedOtherFolderNode.GetChildNodeTypeCount: Integer;
begin
  Result:= 1;
end;  // TStoreLinkedOtherFolderNode.GetChildNodeTypeCount 

{-------------------------------------------------------------------------------
}
function TStoreLinkedOtherFolderNode.GetNodeContext: TNodeContext;
begin
  Result := ncStore;
end;  // TStoreLinkedOtherFolderNode.GetNodeContext 

{-------------------------------------------------------------------------------
}
function TStoreLinkedOtherFolderNode.GetSearchType: SearchManager.TSearchType;
begin
  Result := stStoreName;
end;  // TStoreLinkedOtherFolderNode.GetSearchType 

{-------------------------------------------------------------------------------
}
procedure TStoreLinkedOtherFolderNode.SetCaption;
begin
  Text := ResStr_Other;
end;  // TStoreLinkedOtherFolderNode.SetCaption 

{-------------------------------------------------------------------------------
}
function TStoreLinkedOtherFolderNode.StoredProcByChildType(ALeafNodeClass: TLeafNodeClass):
    String;
begin
  Result := '';
  
  if ALeafNodeClass = TRelatedCollectionUnitLeafNode then
    Result := 'usp_Stores_Select_ForLinkedOther'
  else
    raise EBrowserNodeError.Create(ResStr_InvalidChildNodeTypeIndexRequest);
end;  // TStoreLinkedOtherFolderNode.StoredProcByChildType 

{-==============================================================================
    TStoreHierarchySubFolderNode
===============================================================================}
{-------------------------------------------------------------------------------
  Add the different types of children. 
}
procedure TStoreHierarchySubFolderNode.AddChildNodesOfType(ABrowserNodeClass:
    TBrowserNodeClass);
begin
  if ABrowserNodeClass = TStoreHierarchySubFolderNode then
    PopulateImmediateChild(ABrowserNodeClass, StoredProcByChildType(
        TLeafNodeClass(ABrowserNodeClass)))
  else
    raise EBrowserNodeError.Create(ResStr_InvalidChildNodeTypeIndexRequest);
end;  // TStoreHierarchySubFolderNode.AddChildNodesOfType 

{-------------------------------------------------------------------------------
}
function TStoreHierarchySubFolderNode.AddNode(ATree: TRapidTree; AMenuIndex: integer):
    TFlyNode;
begin
  Result := TFolderNode(ATree.Items.AddTypedChild(Self, TStoreHierarchySubFolderNode));
  TFolderNode(Result).Initialise('');
  Result.Caption:= ResStr_NewNode;
  Result.OverlayIndex := 0;
end;  // TStoreHierarchySubFolderNode.AddNode 

{-------------------------------------------------------------------------------
}
class function TStoreHierarchySubFolderNode.ClassTableName: String;
begin
  Result := TN_STORE;
end;  // TStoreHierarchySubFolderNode.ClassTableName 

{-------------------------------------------------------------------------------
}
procedure TStoreHierarchySubFolderNode.FindAndSetHyperlinkKey;
begin
  HyperlinkKey := Key;
end;  // TStoreHierarchySubFolderNode.FindAndSetHyperlinkKey 

{-------------------------------------------------------------------------------
}
function TStoreHierarchySubFolderNode.GetAddButtonMenuCaption(Index: Integer): String;
begin
  case Index of
   0:  Result := ResStr_AddNew;
   1:  Result := ResStr_LinkToExisting;
  else
    raise EAddMenuItemError.Create(ResStr_InvalidAddMenuIndexRequest);
  end;
end;  // TStoreHierarchySubFolderNode.GetAddButtonMenuCaption 

{-------------------------------------------------------------------------------
}
function TStoreHierarchySubFolderNode.GetAddButtonMenuCaptionsCount: Integer;
begin
  Result := 2;
end;  // TStoreHierarchySubFolderNode.GetAddButtonMenuCaptionsCount 

{-------------------------------------------------------------------------------
}
function TStoreHierarchySubFolderNode.GetAddMenuIsAdd(AMenuIndex: Integer): Boolean;
begin
  case AMenuIndex of
    0 :    Result := True;
    else   Result := False;
  end;
end;  // TStoreHierarchySubFolderNode.GetAddMenuIsAdd 

{-------------------------------------------------------------------------------
  Returns the normal store frame. 
}
function TStoreHierarchySubFolderNode.GetAssociatedFrame: TBaseDetailFrameClass;
begin
  Result := TfraStorage;
end;  // TStoreHierarchySubFolderNode.GetAssociatedFrame 

{-------------------------------------------------------------------------------
}
function TStoreHierarchySubFolderNode.GetCanAdd: Boolean;
begin
  Result := True;
end;  // TStoreHierarchySubFolderNode.GetCanAdd 

{-------------------------------------------------------------------------------
}
function TStoreHierarchySubFolderNode.GetCanEdit: Boolean;
begin
  Result := True;
end;  // TStoreHierarchySubFolderNode.GetCanEdit 

{-------------------------------------------------------------------------------
}
function TStoreHierarchySubFolderNode.GetChildNodeType(Index: Integer): TBrowserNodeClass;
begin
  case Index of
    0: Result := TStoreHierarchySubFolderNode;
  else
    raise EBrowserNodeError.Create(ResStr_InvalidChildNodeTypeIndexRequest);
  end;
end;  // TStoreHierarchySubFolderNode.GetChildNodeType 

{-------------------------------------------------------------------------------
}
function TStoreHierarchySubFolderNode.GetChildNodeTypeCount: Integer;
begin
  Result:= 1;
end;  // TStoreHierarchySubFolderNode.GetChildNodeTypeCount 

{-------------------------------------------------------------------------------
}
function TStoreHierarchySubFolderNode.GetImageIndex: Integer;
begin
  Result := 4;
end;  // TStoreHierarchySubFolderNode.GetImageIndex 

{-------------------------------------------------------------------------------
}
function TStoreHierarchySubFolderNode.GetKeyList: TKeyList;
begin
  //  Result := inherited GetKeyList;
  Result := TEditableKeylist.Create;
  TEditableKeyList(Result).SetTable(TableName);
    // Add key, plus a pointer to self so it can be refreshed after a drag operation
  TEditableKeyList(Result).AddItem(Key, IntToStr(Integer(Self)));
end;  // TStoreHierarchySubFolderNode.GetKeyList 

{-------------------------------------------------------------------------------
}
function TStoreHierarchySubFolderNode.GetNodeContext: TNodeContext;
begin
  Result := ncStore;
end;  // TStoreHierarchySubFolderNode.GetNodeContext 

{-------------------------------------------------------------------------------
}
function TStoreHierarchySubFolderNode.GetSearchType: SearchManager.TSearchType;
begin
  Result := stStoreName;
end;  // TStoreHierarchySubFolderNode.GetSearchType 

{-------------------------------------------------------------------------------
}
function TStoreHierarchySubFolderNode.GetStoredProcParams: TVariantArray;
var
  SortIndex: Integer;
begin
  SortIndex := ViewTypeManager.ViewTypeByNodeContext(ncStore).SortOrderDefaultIndex;
  Result := VarArrayOf(['@ParentKey', Key, '@SortOrderIndex', SortIndex]);
end;  // TStoreHierarchySubFolderNode.GetStoredProcParams 

{-------------------------------------------------------------------------------
  This method is used to retrieve the PROP_PARENT_KEY. It will get a result for this property in the inherited method. However, the method used by the inherited method to obtain the key doesn't work properly for this kind of hierarchy. Hence, the correct parent key overwrites it here.
}
function TStoreHierarchySubFolderNode.InternalGetProperty(const AName: String): Variant;
begin
  Result := inherited InternalGetProperty(AName);
  if AName = PROP_PARENT_KEY then
    Result := TBrowserNode(Parent).Key;
end;  // TStoreHierarchySubFolderNode.InternalGetProperty 

{-------------------------------------------------------------------------------
}
procedure TStoreHierarchySubFolderNode.InternalInitialise(AKey: String);
begin
  inherited InternalInitialise(AKey);
  BottomLevel := dmGeneral.GetStoredProcOutputParam
                                ('usp_StoreIsBottomLevel_Get',
                                ['@Key', AKey],
                                '@IsBottomLevel');
  JoinKey := AKey;
  HyperlinkKey := AKey;
  DblClickNavigates := True;
end;  // TStoreHierarchySubFolderNode.InternalInitialise 

{-------------------------------------------------------------------------------
}
procedure TStoreHierarchySubFolderNode.InternalRefresh;
begin
  inherited;
  BottomLevel := dmGeneral.GetStoredProcOutputParam
                                ('usp_StoreIsBottomLevel_Get',
                                ['@Key', Key],
                                '@IsBottomLevel');
  if FBottomLevel then begin
    Expanded := True;
    DblClickNavigates := True;
  end else begin
    DblClickNavigates := False;
    DeleteChildren;
    Populated := False;
    Populate;
  end;
end;  // TStoreHierarchySubFolderNode.InternalRefresh 

{-------------------------------------------------------------------------------
}
function TStoreHierarchySubFolderNode.LinkNode(const AKey: string; const ACaption: string):
    TBrowserNode;
var
  lCheckIdx: Integer;
  lDuplicate, lUpdateUsualLocation: Boolean;
begin
  // First check this key not already in folder
  lDuplicate := False;
  for lCheckIdx := 0 to Count-1 do
    with TBrowserNode(Items[lCheckIdx]) do
      if (Key = AKey) and (ClassType = GetChildNodeType(0)) then
        lDuplicate := True;
  if lDuplicate then
    raise EDuplicateNodeError.CreateNonCritical(ResStr_DuplicationInFolder);
  //GetChildNodeType can only return TBrowserNodeClass
  //Added node will be a TLeafNode
  Result := TBrowserNode(Tree.Items.AddTypedChild(Self, GetChildNodeType(0)));
  try
    with TFolderNode(Result) do begin
      Initialise(AKey);
      Caption := ACaption;
    end;
    // We need to update the Current location (and optionally, the Usual location)
    lUpdateUsualLocation := (MessageDlg(ResStr_AskUpdateUsualLocation,
                                    mtConfirmation, [mbYes, mbNo], 0) = mrYes);
    dmGeneral.RunUpdateStoredProc('usp_Store_Location_Update',
                                  ['@Key', AKey,
                                  '@CurrentContainerKey', Key,
                                  '@SessionID', AppSettings.SessionID,
                                  '@UpdateUsualContainer', lUpdateUsualLocation]);
    PostMessage(TWinControl(Tree.Owner).Handle, WM_REMOVE_DRAGGED_NODE, 0, 0);
    BottomLevel := False;
  except
    on EBrowserNodeError do begin
      Result.Free;
      raise;
    end;
  end;
end;  // TStoreHierarchySubFolderNode.LinkNode 

{-------------------------------------------------------------------------------
  Populates with its immediate children. 
}
procedure TStoreHierarchySubFolderNode.PopulateImmediateChild(ABrowserNodeClass:
    TBrowserNodeClass; const AStoredProcName: String);
var
  lRecordset: _Recordset;
  lNewNode: TBrowserNode;
begin
  lRecordset := dmGeneral.GetRecordset(AStoredProcName, GetStoredProcParams);
  
  Tree.Items.BeginUpdate;
  try
    if lRecordset.RecordCount > 0 then begin
      lRecordSet.MoveFirst;
      while not lRecordset.EOF do begin
        if (ABrowserNodeClass = TStoreHierarchySubFolderNode) then begin
          lNewNode:= TFolderNode(Tree.Items.AddTypedChild(Self,
              ABrowserNodeClass));
          TFolderNode(lNewNode).Initialise(lRecordset.Fields['Item_Key'].Value);
          if lRecordset.Fields['Number'].Value = Null then
             lNewNode.Caption:= lRecordset.Fields['Item_Name'].Value
          else
             lNewNode.Caption:= lRecordset.Fields['Item_Name'].Value + ' - ' +
                VarToStr(lRecordset.Fields['Number'].Value);
          if (lRecordset.Fields['Bottom_Level'].Value = true) then begin
            lNewNode.OverlayIndex := 0;
            lNewNode.HasChildren := False;
          end;
        end;
        lRecordset.MoveNext
      end;
    end;
  finally
    Tree.Items.EndUpdate;
    lRecordset.Close;
  end;
end;  // TStoreHierarchySubFolderNode.PopulateImmediateChild 

{-------------------------------------------------------------------------------
}
procedure TStoreHierarchySubFolderNode.SetBottomLevel(
  const Value: Boolean);
begin
  FBottomLevel := Value;
  if FBottomLevel then
    OverlayIndex := 0
  else
    OverlayIndex := -1;
end;

{-------------------------------------------------------------------------------
}
procedure TStoreHierarchySubFolderNode.SetCaption;
begin
  //Does nothing. The caption for this folder can only be set by its parent.
end;  // TStoreHierarchySubFolderNode.SetCaption 

{-------------------------------------------------------------------------------
}
function TStoreHierarchySubFolderNode.StoredProcByChildType(ALeafNodeClass: TLeafNodeClass):
    String;
begin
  Result := 'usp_StoreHierarchy_Child_Select_ForStore';
end;  // TStoreHierarchySubFolderNode.StoredProcByChildType 

{-------------------------------------------------------------------------------
}
function TStoreHierarchySubFolderNode.ValidateNewNode(const AKey: TKeyString; const ACaption:
    string): Boolean;
begin
  inherited ValidateNewNode(AKey, ACaption);
  Result := (dmGeneral.GetStoredProcOutputParam('usp_Store_Contains_Get',
             ['@ContainerStoreKey', AKey, '@ContainedStoreKey', Key, '@IsCurrentLocation', 1],
             '@Contains') = 0);
  if not Result then begin
    ShowInformation(Format(ResStr_RecursionFailureStoreContains, [ACaption]));
    Exit;
  end;
end;  // TStoreHierarchySubFolderNode.ValidateNewNode 




{-==============================================================================
    TCollectionUnitFolderNode
===============================================================================}
{-------------------------------------------------------------------------------
  See if the top level node is a Loan In node. 
}
function TCollectionUnitFolderNode.GetIsLoanIn: Boolean;
begin
  Result := TBaseMovementTopLevelNode(TopLevelNode).MovementType = 2;
end;  // TCollectionUnitFolderNode.GetIsLoanIn 

{-------------------------------------------------------------------------------
}
function TCollectionUnitFolderNode.ValidateNewNode(const AKey: TKeyString; const ACaption:
    string): Boolean;
var
  lErrorMsg: String;
begin
  Result := True;
  lErrorMsg := Format(ResStr_MovementCannotBeAdded,
                      [RemoveSubStrings(ACaption, ['<i>', '</i>', '<b/>*'])]);

  if TopLevelNode is TBaseMovementTopLevelNode then begin
    Result := dmGeneral.GetStoredProcOutputParam('usp_CollectionUnit_OwnedByHoldingOrg_Get',
                              ['@CollectionUnitKey', AKey,
                              '@MovementKey', TBrowserNode(TopLevelNode).Key],
                              '@OwnedByHoldingOrg') = True;

    // Additional check if dealing with loans. Direction is important.
    if TopLevelNode.NodeContext = ncLoan then begin
      Result := GetIsLoanIn <> Result;
      if GetIsLoanIn then
        lErrorMsg := Format(ResStr_LoanInCannotBeAdded,
                            [RemoveSubStrings(ACaption, ['<i>', '</i>', '<b/>*'])])
      else
        lErrorMsg := Format(ResStr_LoanOutCannotBeAdded,
                            [RemoveSubStrings(ACaption, ['<i>', '</i>', '<b/>*'])]);
    end;
  
    if not Result then
      raise EBrowserNodeError.CreateNonCritical(lErrorMsg);
  end;
end;  // TCollectionUnitFolderNode.ValidateNewNode 

{-==============================================================================
    TStoreHierarchyTopLevelNode
===============================================================================}
{-------------------------------------------------------------------------------
}
procedure TStoreHierarchyTopLevelNode.AddChildNodesOfType(ABrowserNodeClass:
    TBrowserNodeClass);
begin
  if (ABrowserNodeClass = TStoreHierarchySubFolderNode) then
    PopulateImmediateChild(ABrowserNodeClass, 'usp_StoreHierarchy_Child_Select_ForStore')
  else
    raise EBrowserNodeError.Create(ResStr_InvalidChildNodeTypeIndexRequest);
end;  // TStoreHierarchyTopLevelNode.AddChildNodesOfType 

{-------------------------------------------------------------------------------
}
function TStoreHierarchyTopLevelNode.AddNode(ATree: TRapidTree; AMenuIndex: integer): TFlyNode;
begin
  // Add new child Store leaf node
  Result := TFolderNode(ATree.Items.AddTypedChild(Self, TStoreHierarchySubFolderNode));
  TFolderNode(Result).Initialise('');
  Result.Caption:= ResStr_NewNode;
  Result.OverlayIndex := 0;
end;  // TStoreHierarchyTopLevelNode.AddNode 

{-------------------------------------------------------------------------------
}
class function TStoreHierarchyTopLevelNode.ClassTableName: String;
begin
  Result := TN_STORE;
end;  // TStoreHierarchyTopLevelNode.ClassTableName 

{-------------------------------------------------------------------------------
}
function TStoreHierarchyTopLevelNode.GetAddButtonMenuCaption(Index: Integer): String;
begin
  case Index of
    0 : Result := ResStr_AddNew;
    1 : Result := ResStr_LinkToExisting;
  else
    raise EAddMenuItemError.Create(ResStr_InvalidAddMenuIndexRequest);
  end;
end;  // TStoreHierarchyTopLevelNode.GetAddButtonMenuCaption 

{-------------------------------------------------------------------------------
}
function TStoreHierarchyTopLevelNode.GetAddButtonMenuCaptionsCount: Integer;
begin
  Result := 2;
end;  // TStoreHierarchyTopLevelNode.GetAddButtonMenuCaptionsCount 

{-------------------------------------------------------------------------------
}
function TStoreHierarchyTopLevelNode.GetAddMenuIsAdd(AMenuIndex: Integer): Boolean;
begin
  case AMenuIndex of
    0 : Result := True
  else
    Result := False;
  end;
end;  // TStoreHierarchyTopLevelNode.GetAddMenuIsAdd 

{-------------------------------------------------------------------------------
}
function TStoreHierarchyTopLevelNode.GetAssociatedFrame: TBaseDetailFrameClass;
begin
  Result := TfraStorage;
end;  // TStoreHierarchyTopLevelNode.GetAssociatedFrame 

{-------------------------------------------------------------------------------
}
function TStoreHierarchyTopLevelNode.GetCanAdd: Boolean;
begin
  Result := True;
end;  // TStoreHierarchyTopLevelNode.GetCanAdd 

{-------------------------------------------------------------------------------
}
function TStoreHierarchyTopLevelNode.GetChildNodeType(Index: Integer): TBrowserNodeClass;
begin
  case Index of
    0: Result := TStoreHierarchySubFolderNode;
  else
    raise EBrowserNodeError.Create(ResStr_InvalidChildNodeTypeIndexRequest);
  end;
end;  // TStoreHierarchyTopLevelNode.GetChildNodeType 

{-------------------------------------------------------------------------------
}
function TStoreHierarchyTopLevelNode.GetChildNodeTypeCount: Integer;
begin
  Result := 1;
end;  // TStoreHierarchyTopLevelNode.GetChildNodeTypeCount 

{-------------------------------------------------------------------------------
}
function TStoreHierarchyTopLevelNode.GetImageIndex: Integer;
begin
  Result := 4;
end;  // TStoreHierarchyTopLevelNode.GetImageIndex 

{-------------------------------------------------------------------------------
}
function TStoreHierarchyTopLevelNode.GetKeyList: TKeyList;
begin
  //  Result := inherited GetKeyList;
  Result := TEditableKeylist.Create;
  TEditableKeyList(Result).SetTable(TableName);
    // Add key, plus a pointer to self so it can be refreshed after a drag operation
  TEditableKeyList(Result).AddItem(Key, IntToStr(Integer(Self)));
end;  // TStoreHierarchyTopLevelNode.GetKeyList 

{-------------------------------------------------------------------------------
}
function TStoreHierarchyTopLevelNode.GetNodeContext: TNodeContext;
begin
  Result := ncStoreHierarchy;
end;  // TStoreHierarchyTopLevelNode.GetNodeContext 

{-------------------------------------------------------------------------------
}
function TStoreHierarchyTopLevelNode.GetProperty(const AName: string): Variant;
begin
  Result := InternalGetProperty(AName);
end;  // TStoreHierarchyTopLevelNode.GetProperty 

{-------------------------------------------------------------------------------
}
function TStoreHierarchyTopLevelNode.GetSearchType: SearchManager.TSearchType;
begin
  Result := SearchManager.stStoreName;
end;  // TStoreHierarchyTopLevelNode.GetSearchType 

{-------------------------------------------------------------------------------
}
function TStoreHierarchyTopLevelNode.GetStoredProcParams: TVariantArray;
var
  SortIndex: Integer;
begin
  SortIndex := ViewTypeManager.ViewTypeByNodeContext(ncStore).SortOrderDefaultIndex;
  Result := VarArrayOf(['@ParentKey', Key, '@SortOrderIndex', SortIndex]);
end;  // TStoreHierarchyTopLevelNode.GetStoredProcParams 

{-------------------------------------------------------------------------------
}
function TStoreHierarchyTopLevelNode.InternalGetProperty(const AName: string): Variant;
begin
  Result := inherited InternalGetProperty(AName);
  
  if Result = Unassigned then begin
    if AName = PROP_NODE_CONTEXT then
      Result := GetNodeContext
    else
      Result := Unassigned;
  end;
end;  // TStoreHierarchyTopLevelNode.InternalGetProperty 

{-------------------------------------------------------------------------------
}
procedure TStoreHierarchyTopLevelNode.InternalInitialise(ARecordset: _Recordset);
begin
  inherited;
  BottomLevel := ARecordset.Fields['Bottom_Level'].Value;

  if FBottomLevel then
    Expanded := True
  else begin
    DblClickNavigates := False;
  end;
end;  // TStoreHierarchyTopLevelNode.InternalInitialise 

{-------------------------------------------------------------------------------
}
procedure TStoreHierarchyTopLevelNode.InternalRefresh;
begin
    // Don't want to call inherited because it will check to see if the node has
    // been deleted. However, if the node has been dragged from the top level to
    // become a child, it will be reported as deleted even though it hasn't been -
    // it simply isn't top level any more.
  (*  DeleteChildren;
    Populated := False;
    Populate;
  *)
    // Specific TStoreHierarchyTopLevelNode.InternalRefresh code
  BottomLevel := dmGeneral.GetStoredProcOutputParam
                                ('usp_StoreIsBottomLevel_Get',
                                ['@Key', Key],
                                '@IsBottomLevel');
  if FBottomLevel then begin
    Expanded := True;
    DblClickNavigates := True;
  end else begin
    DblClickNavigates := False;
    DeleteChildren;
    Populated := False;
    Populate;
  end;
end;  // TStoreHierarchyTopLevelNode.InternalRefresh

{-------------------------------------------------------------------------------
}
function TStoreHierarchyTopLevelNode.LinkNode(const AKey: string; const ACaption: string):
    TBrowserNode;
var
  lUpdateUsualLocation: Boolean;
begin
  Result := inherited LinkNode(AKey, ACaption);
  // We need to update the Current location (and optionally, the Usual location)
  lUpdateUsualLocation := (MessageDlg(ResStr_AskUpdateUsualLocation,
                                  mtConfirmation, [mbYes, mbNo], 0) = mrYes);
  dmGeneral.RunUpdateStoredProc('usp_Store_Location_Update',
                                ['@Key', AKey,
                                '@CurrentContainerKey', Key,
                                '@SessionID', AppSettings.SessionID,
                                '@UpdateUsualContainer', lUpdateUsualLocation]);
  PostMessage(TWinControl(Tree.Owner).Handle, WM_REMOVE_DRAGGED_NODE, 0, 0);
  BottomLevel := False;
end;  // TStoreHierarchyTopLevelNode.LinkNode 

{-------------------------------------------------------------------------------
}
procedure TStoreHierarchyTopLevelNode.PopulateImmediateChild(ABrowserNodeClass:
    TBrowserNodeClass; const AStoredProcName: String);
var
  lRecordset: _Recordset;
  lNewNode: TBrowserNode;
begin
  //  if not FBottomLevel then begin
  lRecordset := dmGeneral.GetRecordset(AStoredProcName, GetStoredProcParams);
  
  Tree.Items.BeginUpdate;
  try
    if lRecordset.RecordCount > 0 then begin
      lRecordSet.MoveFirst;
      while not lRecordset.EOF do begin
        if (ABrowserNodeClass = TStoreHierarchySubFolderNode) then begin
          lNewNode:= TFolderNode(Tree.Items.AddTypedChild(Self,
              ABrowserNodeClass));
          TFolderNode(lNewNode).Initialise(lRecordset.Fields['Item_Key'].Value);
          if lRecordset.Fields['Number'].Value = Null then
             lNewNode.Caption:= lRecordset.Fields['Item_Name'].Value
          else
             lNewNode.Caption:= lRecordset.Fields['Item_Name'].Value + ' - ' +
                VarToStr(lRecordset.Fields['Number'].Value);
          if (lRecordset.Fields['Bottom_Level'].Value = true) then begin
            lNewNode.OverlayIndex := 0;
            lNewNode.HasChildren := False;
          end;
        end;
        lRecordset.MoveNext
      end;
    end;
  finally
    Tree.Items.EndUpdate;
    lRecordset.Close;
  end;
  //  end;
end;  // TStoreHierarchyTopLevelNode.PopulateImmediateChild

{-------------------------------------------------------------------------------
}
procedure TStoreHierarchyTopLevelNode.SetBottomLevel(const Value: Boolean);
begin
  FBottomLevel := Value;
  if FBottomLevel then
    OverlayIndex := 0
  else
    OverlayIndex := -1;  
end;

{-------------------------------------------------------------------------------
}
procedure TStoreHierarchyTopLevelNode.SetCaption(ARecordset: _Recordset);
begin
  if ARecordset.Fields['Number'].Value = Null then
    Caption:= ARecordset.Fields['Item_Name'].Value
  else
    Caption:= ARecordset.Fields['Item_Name'].Value + ' - ' +
              ARecordset.Fields['Number'].Value;
end;  // TStoreHierarchyTopLevelNode.SetCaption 

{-------------------------------------------------------------------------------
}
function TStoreHierarchyTopLevelNode.StoredProcByChildType(ALeafNodeClass: TLeafNodeClass):
    String;
begin
  Result := 'usp_StoreHierarchy_Child_Select_ForStore';
end;  // TStoreHierarchyTopLevelNode.StoredProcByChildType 

{-------------------------------------------------------------------------------
}
function TStoreHierarchyTopLevelNode.ValidateNewNode(const AKey: TKeyString; const ACaption:
    string): Boolean;
begin
  inherited ValidateNewNode(AKey, ACaption);
  Result := (dmGeneral.GetStoredProcOutputParam('usp_Store_Contains_Get',
             ['@ContainerStoreKey', AKey, '@ContainedStoreKey', Key, '@IsCurrentLocation', 1],
             '@Contains') = 0);
  if not Result then begin
    ShowInformation(Format(ResStr_RecursionFailureStoreContains, [ACaption]));
    Exit;
  end;
end;  // TStoreHierarchyTopLevelNode.ValidateNewNode

end.


