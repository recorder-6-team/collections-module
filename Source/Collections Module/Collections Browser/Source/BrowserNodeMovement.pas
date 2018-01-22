{===============================================================================
  Unit:        BrowserNodeMovement.pas

  Defines:     Many Classes

  Description: Contains node classes related to movements

  Model:       BrowserNodes.mpb

  Created:     August 2003

  Last revision information:
    $Revision: 60 $
    $Date: 21/02/05 11:18 $
    $Author: Ericsalmon $

===============================================================================}
unit BrowserNodeMovement;

interface

uses
  SysUtils, Windows, Messages, Classes, Graphics, Controls, Forms, Dialogs, TreeColl,
  BrowserNodeFramework, ResourceStrings, RapTree, BaseDetailFrameUnit, ADODB, DataTypes,
  SearchManager, FrameMovement, GeneralData, DataClasses, LuxembourgConstants,
  FrameCBNavigation, GeneralFunctions;

type
  TBaseMovementFolderNode = class(TFolderNode)
  private
    FMovementType: Integer;
  protected
    function InternalGetProperty(const AName: String): Variant; override;
    procedure SetMovementType(Value: Integer); virtual;
    function StoredProcByChildType(ALeafNodeClass: TLeafNodeClass): String; override;
  public
    class function ClassTableName: String; override;
    procedure DeleteLink(const AJoinTableKey: String); override;
    function UpdateNodeRelationship(const NewNodeKey: String): String; override;
    function ValidateNewNode(const AKey: TKeyString; const ACaption: string): Boolean;
        override;
    property MovementType: Integer read FMovementType write SetMovementType;
  end;
  
  TBaseMovementLeafNode = class(THyperlinkLeafNode)
  private
    FMovementType: Integer;
  protected
    function GetCanAdd: Boolean; override;
    function GetCanDelete: Boolean; override;
    function GetCanEdit: Boolean; override;
    function GetDomainStoredProc: String; override;
    function InternalGetProperty(const AName: String): Variant; override;
    procedure InternalInitialise(ARecordset: _Recordset); override;
    procedure InternalInitialiseLinkNode(const AKey: string = ''; const ACaption: string ='');
        override;
    procedure SetCaption(ARecordset: _Recordset); override;
    procedure SetMovementType(Value: Integer); virtual;
  public
    class function ClassTableName: String; override;
    property MovementType: Integer read FMovementType write SetMovementType;
  end;
  
  TBaseMovementTopLevelNode = class(TTopLevelNode)
  private
    FMovementType: Integer;
    FWithAcquisition: Boolean;
  protected
    function GetCanAdd: Boolean; override;
    function GetCanDelete: Boolean; override;
    function GetCanEdit: Boolean; override;
    function GetDomainStoredProc: String; override;
    function InternalGetProperty(const AName: String): Variant; override;
    procedure InternalInitialise(ARecordset: _Recordset); override;
    procedure SetCaption(ARecordset: _Recordset); override;
    procedure SetMovementType(Value: Integer); virtual;
    procedure SetWithAcquisition(Value: Boolean = false); virtual;
  public
    class function ClassTableName: String; override;
    property MovementType: Integer read FMovementType write SetMovementType;
    property WithAcquisition: Boolean read FWithAcquisition write FWithAcquisition;
  end;
  
  TAccessionFolderNode = class(TBaseMovementFolderNode, IAddMenuOptions)
  private
    function AddNode(ATree: TRapidTree; AMenuIndex: integer): TFlyNode;
  protected
    function GetAddButtonMenuCaption(Index: Integer): String; override;
    function GetAddButtonMenuCaptionsCount: Integer; override;
    function GetAddMenuIsAdd(AMenuIndex: Integer): Boolean; override;
    function GetCanAdd: Boolean; override;
    function GetCanDelete: Boolean; override;
    function GetCanEdit: Boolean; override;
    function GetChildNodeType(Index: Integer): TBrowserNodeClass; override;
    function GetChildNodeTypeCount: Integer; override;
    function GetNodeContext: TNodeContext; override;
    function GetSearchType: SearchManager.TSearchType; override;
    function GetStoredProcParams: TVariantArray; override;
    procedure SetCaption; override;
  public
    function UpdateNodeRelationship(const NewNodeKey: String): String; override;
  end;
  
  TAccessionLeafNode = class(TBaseMovementLeafNode, IAdditionalProperties)
  private
    FWithAcquisition: Boolean;
    function GetProperty(const AName: string): Variant;
  protected
    function GetAssociatedFrame: TBaseDetailFrameClass; override;
    function GetImageIndex: Integer; override;
    function GetNodeContext: TNodeContext; override;
    function InternalGetProperty(const AName: String): Variant; override;
    procedure SetMovementType(Value: Integer); override;
  public
    property WithAcquisition: Boolean read FWithAcquisition write FWithAcquisition;
  end;
  
  TAccessionTopLevelNode = class(TBaseMovementTopLevelNode, IAddMenuOptions,
      IAdditionalProperties)
  private
    function AddNode(ATree: TRapidTree; AMenuIndex: integer): TFlyNode;
    function GetProperty(const AName: string): Variant;
    function ReturnMovementFolder(Inbound: boolean): TBrowserNode;
  protected
    procedure AddChildNodesOfType(ABrowserNodeClass: TBrowserNodeClass); override;
    function GetAddButtonMenuCaption(Index: Integer): String; override;
    function GetAddButtonMenuCaptionsCount: Integer; override;
    function GetAddMenuIsAdd(AMenuIndex: Integer): Boolean; override;
    function GetAssociatedFrame: TBaseDetailFrameClass; override;
    function GetChildNodeType(Index: Integer): TBrowserNodeClass; override;
    function GetChildNodeTypeCount: Integer; override;
    function GetImageIndex: Integer; override;
    function GetNodeContext: TNodeContext; override;
    function InternalGetProperty(const AName: string): Variant; override;
    procedure SetMovementType(Value: Integer); override;
    function StoredProcByChildType(ALeafNodeClass: TLeafNodeClass): String; override;
  end;
  
  TAcquisitionDetailsLeafNode = class(TLeafNode, IAdditionalProperties)
  private
    function GetProperty(const AName: string): Variant;
  protected
    function GetAssociatedFrame: TBaseDetailFrameClass; override;
    function GetCanAdd: Boolean; override;
    function GetCanDelete: Boolean; override;
    function GetCanEdit: Boolean; override;
    function GetDomainStoredProc: String; override;
    function GetImageIndex: Integer; override;
    function InternalGetProperty(const AName: String): Variant; override;
    procedure SetCaption(ARecordset: _Recordset); override;
  public
    class function ClassTableName: String; override;
    procedure SetSecurity; override;
  end;
  
  TAccessionDetailsLeafNode = class(TLeafNode, IAdditionalProperties)
  private
    function GetProperty(const AName: string): Variant;
  protected
    function GetAssociatedFrame: TBaseDetailFrameClass; override;
    function GetCanAdd: Boolean; override;
    function GetCanDelete: Boolean; override;
    function GetCanEdit: Boolean; override;
    function GetDomainStoredProc: String; override;
    function GetImageIndex: Integer; override;
    function InternalGetProperty(const AName: String): Variant; override;
    procedure SetCaption(ARecordset: _Recordset); override;
  public
    class function ClassTableName: String; override;
    procedure SetSecurity; override;
  end;
  
  TLoanTopLevelNode = class(TBaseMovementTopLevelNode, IAdditionalProperties)
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
    procedure SetMovementType(Value: Integer); override;
    function StoredProcByChildType(ALeafNodeClass: TLeafNodeClass): String; override;
  end;
  
  TDisposalDetailsLeafNode = class(TLeafNode, IAdditionalProperties)
  private
    function GetProperty(const AName: string): Variant;
  protected
    function GetAssociatedFrame: TBaseDetailFrameClass; override;
    function GetCanAdd: Boolean; override;
    function GetCanDelete: Boolean; override;
    function GetCanEdit: Boolean; override;
    function GetDomainStoredProc: String; override;
    function GetImageIndex: Integer; override;
    function InternalGetProperty(const AName: String): Variant; override;
    procedure SetCaption(ARecordset: _Recordset); override;
  public
    class function ClassTableName: String; override;
    procedure SetSecurity; override;
  end;
  
  TDisposalUnknownDetailsLeafNode = class(TLeafNode, IAdditionalProperties)
  private
    function GetProperty(const AName: string): Variant;
  protected
    function GetAssociatedFrame: TBaseDetailFrameClass; override;
    function GetCanAdd: Boolean; override;
    function GetCanDelete: Boolean; override;
    function GetCanEdit: Boolean; override;
    function GetDomainStoredProc: String; override;
    function GetImageIndex: Integer; override;
    function InternalGetProperty(const AName: String): Variant; override;
    procedure SetCaption(ARecordset: _Recordset); override;
  public
    class function ClassTableName: String; override;
    procedure SetSecurity; override;
  end;
  
  TMovementLeafNode = class(TBaseMovementLeafNode)
  protected
    function GetAssociatedFrame: TBaseDetailFrameClass; override;
    function GetImageIndex: Integer; override;
    function GetNodeContext: TNodeContext; override;
    procedure SetMovementType(Value: Integer); override;
  end;
  
  TMovementTopLevelNode = class(TBaseMovementTopLevelNode, IAddMenuOptions,
      IAdditionalProperties)
  private
    function AddNode(ATree: TRapidTree; AMenuIndex: integer): TFlyNode;
    function GetProperty(const AName: string): Variant;
  protected
    procedure AddChildNodesOfType(ABrowserNodeClass: TBrowserNodeClass); override;
    function GetAddButtonMenuCaption(Index: Integer): String; override;
    function GetAddButtonMenuCaptionsCount: Integer; override;
    function GetAddMenuIsAdd(AMenuIndex: Integer): Boolean; override;
    function GetAssociatedFrame: TBaseDetailFrameClass; override;
    function GetChildNodeType(Index: Integer): TBrowserNodeClass; override;
    function GetChildNodeTypeCount: Integer; override;
    function GetImageIndex: Integer; override;
    function GetNodeContext: TNodeContext; override;
    function InternalGetProperty(const AName: string): Variant; override;
    procedure SetMovementType(Value: Integer); override;
    function StoredProcByChildType(ALeafNodeClass: TLeafNodeClass): String; override;
  end;
  
  TLoanFolderNode = class(TBaseMovementFolderNode, IAddMenuOptions)
  protected
    function GetChildNodeType(Index: Integer): TBrowserNodeClass; override;
    function GetChildNodeTypeCount: Integer; override;
    function GetNodeContext: TNodeContext; override;
    function GetSearchType: SearchManager.TSearchType; override;
    function GetStoredProcParams: TVariantArray; override;
    procedure SetCaption; override;
  end;
  
  TLoanLeafNode = class(TBaseMovementLeafNode)
  protected
    function GetAssociatedFrame: TBaseDetailFrameClass; override;
    function GetCanAdd: Boolean; override;
    function GetImageIndex: Integer; override;
    function GetNodeContext: TNodeContext; override;
    procedure SetMovementType(Value: Integer); override;
  end;
  
  TMovementFolderNode = class(TBaseMovementFolderNode, IAddMenuOptions)
  private
    function AddNode(ATree: TRapidTree; AMenuIndex: integer): TFlyNode;
  protected
    function GetAddButtonMenuCaption(Index: Integer): String; override;
    function GetAddButtonMenuCaptionsCount: Integer; override;
    function GetAddMenuIsAdd(AMenuIndex: Integer): Boolean; override;
    function GetCanAdd: Boolean; override;
    function GetCanDelete: Boolean; override;
    function GetCanEdit: Boolean; override;
    function GetChildNodeType(Index: Integer): TBrowserNodeClass; override;
    function GetChildNodeTypeCount: Integer; override;
    function GetNodeContext: TNodeContext; override;
    function GetSearchType: SearchManager.TSearchType; override;
    function GetStoredProcParams: TVariantArray; override;
    procedure SetCaption; override;
  end;
  
  TMovementInFolderNode = class(TFolderNode, IAddMenuOptions)
  protected
    procedure AddChildNodesOfType(ABrowserNodeClass: TBrowserNodeClass); override;
    function AddNode(ATree: TRapidTree; AMenuIndex: integer): TFlyNode;
    function GetAddButtonMenuCaption(Index: Integer): String; override;
    function GetAddButtonMenuCaptionsCount: Integer; override;
    function GetAddMenuIsAdd(AMenuIndex: Integer): Boolean; override;
    function GetChildNodeType(Index: Integer): TBrowserNodeClass; override;
    function GetChildNodeTypeCount: Integer; override;
    function GetNodeContext: TNodeContext; override;
    procedure SetCaption; override;
    function StoredProcByChildType(ALeafNodeClass: TLeafNodeClass): String; override;
  public
    class function ClassTableName: String; override;
  end;
  
  TMovementOutFolderNode = class(TFolderNode, IAddMenuOptions)
  protected
    procedure AddChildNodesOfType(ABrowserNodeClass: TBrowserNodeClass); override;
    function AddNode(ATree: TRapidTree; AMenuIndex: integer): TFlyNode;
    function GetAddButtonMenuCaption(Index: Integer): String; override;
    function GetAddButtonMenuCaptionsCount: Integer; override;
    function GetAddMenuIsAdd(AMenuIndex: Integer): Boolean; override;
    function GetChildNodeType(Index: Integer): TBrowserNodeClass; override;
    function GetChildNodeTypeCount: Integer; override;
    function GetNodeContext: TNodeContext; override;
    procedure SetCaption; override;
    function StoredProcByChildType(ALeafNodeClass: TLeafNodeClass): String; override;
  public
    class function ClassTableName: String; override;
  end;
  
  THostedMaterialDetailsLeafNode = class(TLeafNode)
  protected
    function GetAssociatedFrame: TBaseDetailFrameClass; override;
    function GetCanAdd: Boolean; override;
    function GetCanDelete: Boolean; override;
    function GetCanEdit: Boolean; override;
    function GetDomainStoredProc: String; override;
    function GetImageIndex: Integer; override;
    procedure SetCaption(ARecordset: _Recordset); override;
  public
    class function ClassTableName: String; override;
    procedure SetSecurity; override;
  end;
  
  TObjectFolderNode = class(TFolderNode)
  protected
    procedure AddChildNodesOfType(ABrowserNodeClass: TBrowserNodeClass); override;
    function GetCanAdd: Boolean; override;
    function GetChildNodeType(Index: Integer): TBrowserNodeClass; override;
    function GetChildNodeTypeCount: Integer; override;
    procedure SetCaption; override;
    function StoredProcByChildType(ALeafNodeClass: TLeafNodeClass): String; override;
  public
    class function ClassTableName: String; override;
  end;
  
  TTransferOfOwnershipDetailsLeafNode = class(TLeafNode, IAdditionalProperties)
  private
    function GetProperty(const AName: string): Variant;
  protected
    function GetAssociatedFrame: TBaseDetailFrameClass; override;
    function GetCanAdd: Boolean; override;
    function GetCanDelete: Boolean; override;
    function GetCanEdit: Boolean; override;
    function GetImageIndex: Integer; override;
    function InternalGetProperty(const AName: String): Variant; override;
    procedure SetCaption(ARecordset: _Recordset); override;
  public
    class function ClassTableName: String; override;
  end;
  
//==============================================================================
implementation

uses
  BrowserNodeConditionCheck, BrowserNodeCommon, BrowserNodeCollectionUnits, Variants,
  FrameListViewer, BrowserViewTypes, FrameMovementDetails, FrameOwnershipDetails,
  FrameMaterialToUnknownDestination, ApplicationSettings, FrameMaterialHostedMaterial,
  FrameMaterialMovement, BaseADODataModule, UserMessages;

{-==============================================================================
    TBaseMovementFolderNode
===============================================================================}
{-------------------------------------------------------------------------------
}
class function TBaseMovementFolderNode.ClassTableName: String;
begin
  Result := TN_MOVEMENT;
end;  // TBaseMovementFolderNode.ClassTableName 

{-------------------------------------------------------------------------------
}
procedure TBaseMovementFolderNode.DeleteLink(const AJoinTableKey: String);
begin
  case ParentNodeContext of
    ncCollection,
    ncSpecimen,
    ncStore      : dmGeneral.RunStoredProc('usp_MovementCollectionUnit_Delete',
                                           ['@Key', AJoinTableKey]);
    ncValuation  : dmGeneral.RunStoredProc('usp_MovementValuation_Delete',
                                           ['@Key', AJoinTableKey]);
    ncEnquiry    : dmGeneral.RunStoredProc('usp_MovementEnquiry_Delete',
                                           ['@Key', AJoinTableKey]);
  else
    raise EBrowserNodeError.Create(ResStr_ChildNodeContextInvalid);
  end;
end;  // TBaseMovementFolderNode.DeleteLink 

{-------------------------------------------------------------------------------
}
function TBaseMovementFolderNode.InternalGetProperty(const AName: String): Variant;
begin
  Result := inherited InternalGetProperty(AName);
  
  if Result=Unassigned then begin
    if AName = PROP_MOVEMENT_TYPE then
      Result := FMovementType;
  end;
end;  // TBaseMovementFolderNode.InternalGetProperty 

{-------------------------------------------------------------------------------
}
procedure TBaseMovementFolderNode.SetMovementType(Value: Integer);
begin
  FMovementType := Value;
end;  // TBaseMovementFolderNode.SetMovementType 

{-------------------------------------------------------------------------------
}
function TBaseMovementFolderNode.StoredProcByChildType(ALeafNodeClass: TLeafNodeClass): String;
begin
  Result := '';
  
  if (ALeafNodeClass = TAccessionLeafNode) or
     (ALeafNodeClass = TLoanLeafNode) or
     (ALeafNodeClass = TMovementLeafNode) then begin
    case ParentNodeContext of
      ncCollection,
      ncSpecimen,
      ncStore      : Result := 'usp_Movements_Select_ForCollectionUnit';
      ncValuation  : Result := 'usp_Movements_Select_ForValuation';
      ncEnquiry    : Result := 'usp_Movements_Select_ForEnquiry';
    else
      raise EBrowserNodeError.Create(ResStr_ChildNodeContextInvalid);
    end;
  end else
    raise EBrowserNodeError.Create(ResStr_InvalidChildNodeTypeIndexRequest);
end;  // TBaseMovementFolderNode.StoredProcByChildType 

{-------------------------------------------------------------------------------
}
function TBaseMovementFolderNode.UpdateNodeRelationship(const NewNodeKey: String): String;
begin
  case ParentNodeContext of
    ncCollection,
    ncSpecimen,
    ncStore : Result := VarToStr(dmGeneral.GetStoredProcOutputParam(
        'usp_Movement_Update_ForCollectionUnit',
                     ['@ParentKey', Key, '@ChildKey', NewNodeKey], '@JoinKey'));
    ncEnquiry : Result := VarToStr(dmGeneral.GetStoredProcOutputParam(
        'usp_Movement_Update_ForEnquiry',
                     ['@ParentKey', Key, '@ChildKey', NewNodeKey], '@JoinKey'));
    ncValuation: Result := VarToStr(dmGeneral.GetStoredProcOutputParam(
        'usp_Movement_Update_ForValuation',
                     ['@ParentKey', Key, '@ChildKey', NewNodeKey], '@JoinKey'));
  else
    raise EAddMenuItemError.Create(ResStr_InvalidNodeContext);
  end;
end;  // TBaseMovementFolderNode.UpdateNodeRelationship 

{-------------------------------------------------------------------------------
}
function TBaseMovementFolderNode.ValidateNewNode(const AKey: TKeyString; const ACaption:
    string): Boolean;
begin
  Result := True;
  if TopLevelNode is TCollectionUnitTopLevelNode then begin
    Result := dmGeneral.GetStoredProcOutputParam('usp_CollectionUnit_OwnedByHoldingOrg_Get',
                          ['@CollectionUnitKey', TBrowserNode(TopLevelNode).Key,
                          '@MovementKey', AKey],
                          '@OwnedByHoldingOrg') = True;
    if not Result then
      raise EBrowserNodeError.CreateNonCritical(
          Format(ResStr_MovementCannotBeAdded, [RemoveSubStrings(ACaption, ['<i>', '</i>'])]));
  end;
end;  // TBaseMovementFolderNode.ValidateNewNode 

{-==============================================================================
    TBaseMovementLeafNode
===============================================================================}
{-------------------------------------------------------------------------------
}
class function TBaseMovementLeafNode.ClassTableName: String;
begin
  Result := TN_MOVEMENT;
end;  // TBaseMovementLeafNode.ClassTableName 

{-------------------------------------------------------------------------------
}
function TBaseMovementLeafNode.GetCanAdd: Boolean;
begin
  Result := AppSettings.AllowMovementEdit and inherited GetCanAdd
end;  // TBaseMovementLeafNode.GetCanAdd 

{-------------------------------------------------------------------------------
}
function TBaseMovementLeafNode.GetCanDelete: Boolean;
begin
  Result := AppSettings.AllowMovementEdit and inherited GetCanDelete;
end;  // TBaseMovementLeafNode.GetCanDelete 

{-------------------------------------------------------------------------------
}
function TBaseMovementLeafNode.GetCanEdit: Boolean;
begin
  Result := AppSettings.AllowMovementEdit and inherited GetCanEdit;
end;  // TBaseMovementLeafNode.GetCanEdit 

{-------------------------------------------------------------------------------
}
function TBaseMovementLeafNode.GetDomainStoredProc: String;
begin
  Result := 'usp_Movement_DomainMask_Get';
end;  // TBaseMovementLeafNode.GetDomainStoredProc 

{-------------------------------------------------------------------------------
}
function TBaseMovementLeafNode.InternalGetProperty(const AName: String): Variant;
begin
  Result := inherited InternalGetProperty(AName);
  
  if Result=Unassigned then begin
    if AName = PROP_MOVEMENT_TYPE then
      Result := FMovementType;
  end;
end;  // TBaseMovementLeafNode.InternalGetProperty 

{-------------------------------------------------------------------------------
}
procedure TBaseMovementLeafNode.InternalInitialise(ARecordset: _Recordset);
begin
  MovementType := ARecordset.Fields['Movement_Type'].Value;
  
  inherited InternalInitialise(ARecordset);
end;  // TBaseMovementLeafNode.InternalInitialise 

{-------------------------------------------------------------------------------
}
procedure TBaseMovementLeafNode.InternalInitialiseLinkNode(const AKey: string = ''; const
    ACaption: string ='');
begin
  MovementType := dmGeneral.GetStoredProcOutputParam('usp_MovementType_ByKey_Get',
                                                     ['@Key', AKey], '@MovementType');
  
  inherited InternalInitialiseLinkNode(AKey, ACaption);
end;  // TBaseMovementLeafNode.InternalInitialiseLinkNode 

{-------------------------------------------------------------------------------
}
procedure TBaseMovementLeafNode.SetCaption(ARecordset: _Recordset);
begin
  Caption:= VarToStr(ARecordset.Fields['Display_Caption'].Value);
end;  // TBaseMovementLeafNode.SetCaption 

{-------------------------------------------------------------------------------
}
procedure TBaseMovementLeafNode.SetMovementType(Value: Integer);
begin
  FMovementType := Value;
end;  // TBaseMovementLeafNode.SetMovementType 

{-==============================================================================
    TBaseMovementTopLevelNode
===============================================================================}
{-------------------------------------------------------------------------------
}
class function TBaseMovementTopLevelNode.ClassTableName: String;
begin
  Result := TN_MOVEMENT;
end;  // TBaseMovementTopLevelNode.ClassTableName 

{-------------------------------------------------------------------------------
}
function TBaseMovementTopLevelNode.GetCanAdd: Boolean;
begin
  Result := AppSettings.AllowMovementEdit and inherited GetCanAdd;
end;  // TBaseMovementTopLevelNode.GetCanAdd 

{-------------------------------------------------------------------------------
}
function TBaseMovementTopLevelNode.GetCanDelete: Boolean;
begin
  Result := AppSettings.AllowMovementEdit and inherited GetCanDelete;
end;  // TBaseMovementTopLevelNode.GetCanDelete 

{-------------------------------------------------------------------------------
}
function TBaseMovementTopLevelNode.GetCanEdit: Boolean;
begin
  Result := AppSettings.AllowMovementEdit and inherited GetCanEdit;
end;  // TBaseMovementTopLevelNode.GetCanEdit 

{-------------------------------------------------------------------------------
}
function TBaseMovementTopLevelNode.GetDomainStoredProc: String;
begin
  Result := 'usp_Movement_DomainMask_Get';
end;  // TBaseMovementTopLevelNode.GetDomainStoredProc 

{-------------------------------------------------------------------------------
}
function TBaseMovementTopLevelNode.InternalGetProperty(const AName: String): Variant;
begin
  Result := inherited InternalGetProperty(AName);
  
  if Result=Unassigned then begin
    if AName = PROP_MOVEMENT_TYPE then
      Result := FMovementType;
    if AName = PROP_WITH_ACQUISITION then
      Result := FWithAcquisition;
  end;
end;  // TBaseMovementTopLevelNode.InternalGetProperty 

{-------------------------------------------------------------------------------
}
procedure TBaseMovementTopLevelNode.InternalInitialise(ARecordset: _Recordset);
begin
  MovementType := ARecordset.Fields['Movement_Type'].Value;
  
  inherited InternalInitialise(ARecordset);
end;  // TBaseMovementTopLevelNode.InternalInitialise 

{-------------------------------------------------------------------------------
}
procedure TBaseMovementTopLevelNode.SetCaption(ARecordset: _Recordset);
begin
  Caption:= VarToStr(ARecordset.Fields['Display_Caption'].Value);
  if Caption = '' then Caption := ResStr_Unspecified;
end;  // TBaseMovementTopLevelNode.SetCaption 

{-------------------------------------------------------------------------------
}
procedure TBaseMovementTopLevelNode.SetMovementType(Value: Integer);
begin
  FMovementType := Value;
end;  // TBaseMovementTopLevelNode.SetMovementType 

{-------------------------------------------------------------------------------
}
procedure TBaseMovementTopLevelNode.SetWithAcquisition(Value: Boolean = false);
begin
  FWithAcquisition := Value;
end;  // TBaseMovementTopLevelNode.SetWithAcquisition 

{-==============================================================================
    TAccessionFolderNode
===============================================================================}
{-------------------------------------------------------------------------------
  Adds a node to the treeview of the required type. 
}
function TAccessionFolderNode.AddNode(ATree: TRapidTree; AMenuIndex: integer): TFlyNode;
begin
  Result := TLeafNode(Tree.Items.AddTypedChild(Self, TAccessionLeafNode));
  
  case AMenuIndex of
    0: TAccessionLeafNode(Result).MovementType := 0;
    1: begin
        TAccessionLeafNode(Result).MovementType := 0;
        TAccessionLeafNode(Result).WithAcquisition := True;
       end;
    2: TAccessionLeafNode(Result).MovementType := 1;
  else
    raise EBrowserNodeError.Create(ResStr_IncorrectMovementType);
  end;
  TLeafNode(Result).InitialiseNewNode;
end;  // TAccessionFolderNode.AddNode 

{-------------------------------------------------------------------------------
}
function TAccessionFolderNode.GetAddButtonMenuCaption(Index: Integer): String;
begin
  case Index of
    0:  Result := ResStr_AddAccession;
    1:  Result := ResStr_AddAccessionAndAcquisition;
    2:  Result := ResStr_AddExchange;
    3:  Result := ResStr_LinkToExisting;
  else
    raise EAddMenuItemError.Create(ResStr_InvalidAddMenuIndexRequest);
  end;
end;  // TAccessionFolderNode.GetAddButtonMenuCaption 

{-------------------------------------------------------------------------------
}
function TAccessionFolderNode.GetAddButtonMenuCaptionsCount: Integer;
begin
  (*  if HasChildren  then Result := 1
                    else*) Result := 4;
end;  // TAccessionFolderNode.GetAddButtonMenuCaptionsCount 

{-------------------------------------------------------------------------------
}
function TAccessionFolderNode.GetAddMenuIsAdd(AMenuIndex: Integer): Boolean;
begin
  case AMenuIndex of
  (*    0 : if HasChildren  then Result := False
                          else Result := True;    *)
    3 : Result := False
  else
    Result := True;
  end;
end;  // TAccessionFolderNode.GetAddMenuIsAdd 

{-------------------------------------------------------------------------------
}
function TAccessionFolderNode.GetCanAdd: Boolean;
begin
  Result := AppSettings.AllowMovementEdit and inherited GetCanAdd;
end;  // TAccessionFolderNode.GetCanAdd 

{-------------------------------------------------------------------------------
}
function TAccessionFolderNode.GetCanDelete: Boolean;
begin
  Result := AppSettings.AllowMovementEdit and inherited GetCanDelete;
end;  // TAccessionFolderNode.GetCanDelete 

{-------------------------------------------------------------------------------
}
function TAccessionFolderNode.GetCanEdit: Boolean;
begin
  Result := AppSettings.AllowMovementEdit and inherited GetCanEdit;
end;  // TAccessionFolderNode.GetCanEdit 

{-------------------------------------------------------------------------------
}
function TAccessionFolderNode.GetChildNodeType(Index: Integer): TBrowserNodeClass;
begin
  case Index of
    0: Result := TAccessionLeafNode;
  else
    raise EBrowserNodeError.Create(ResStr_InvalidChildNodeTypeIndexRequest);
  end;
end;  // TAccessionFolderNode.GetChildNodeType 

{-------------------------------------------------------------------------------
}
function TAccessionFolderNode.GetChildNodeTypeCount: Integer;
begin
  Result:= 1;
end;  // TAccessionFolderNode.GetChildNodeTypeCount 

{-------------------------------------------------------------------------------
}
function TAccessionFolderNode.GetNodeContext: TNodeContext;
begin
  Result := ncAccession;
end;  // TAccessionFolderNode.GetNodeContext 

{-------------------------------------------------------------------------------
}
function TAccessionFolderNode.GetSearchType: SearchManager.TSearchType;
begin
  Result := stAccession;
end;  // TAccessionFolderNode.GetSearchType 

{-------------------------------------------------------------------------------
}
function TAccessionFolderNode.GetStoredProcParams: TVariantArray;
var
  lSortIndex: Integer;
begin
  lSortIndex := ViewTypeManager.ViewTypeByNodeContext(NodeContext).SortOrderDefaultIndex;
  
  Result := VarArrayOf(['@ParentKey', Key, '@MovementGroupType', 0,
                        '@SortOrderIndex', lSortIndex]);
end;  // TAccessionFolderNode.GetStoredProcParams 

{-------------------------------------------------------------------------------
}
procedure TAccessionFolderNode.SetCaption;
begin
  Text := ResStr_AccessionsAndExchanges;
end;  // TAccessionFolderNode.SetCaption 

{-------------------------------------------------------------------------------
}
function TAccessionFolderNode.UpdateNodeRelationship(const NewNodeKey: String): String;
begin
  case ParentNodeContext of
    ncCollection,
    ncSpecimen,
    ncStore :
      begin
        Result := VarToStr(dmGeneral.GetStoredProcOutputParam(
                           'usp_Movement_Update_ForCollectionUnit',
                           ['@ParentKey', Key, '@ChildKey', NewNodeKey,
                            '@IsAccessionOrExchange', 1], '@JoinKey'));
        // Refresh easier than finding out which nodes to keep, and which to discard.
        // Need to use a PostMessage because of timing with clipboard stuff.
        PostMessage(TWinControl(Tree.Owner).Handle, WM_REFRESH_NODE, Integer(Self), 0);
      end;
    ncEnquiry :
      begin
        Result := VarToStr(dmGeneral.GetStoredProcOutputParam(
                           'usp_Movement_Update_ForEnquiry',
                           ['@ParentKey', Key, '@ChildKey', NewNodeKey,
                            '@IsAccessionOrExchange', 1], '@JoinKey'));

        EnforceSingleChildNode(NewNodeKey);
      end;
    ncValuation:
      begin
        Result := VarToStr(dmGeneral.GetStoredProcOutputParam(
                           'usp_Movement_Update_ForValuation',
                           ['@ParentKey', Key, '@ChildKey', NewNodeKey,
                            '@IsAccessionOrExchange', 1], '@JoinKey'));

        EnforceSingleChildNode(NewNodeKey);
      end;
  else
    raise EAddMenuItemError.Create(ResStr_InvalidNodeContext);
  end;
end;  // TAccessionFolderNode.UpdateNodeRelationship

{-==============================================================================
    TAccessionLeafNode
===============================================================================}
{-------------------------------------------------------------------------------
}
function TAccessionLeafNode.GetAssociatedFrame: TBaseDetailFrameClass;
begin
  Result := TfraMovement;
end;  // TAccessionLeafNode.GetAssociatedFrame 

{-------------------------------------------------------------------------------
}
function TAccessionLeafNode.GetImageIndex: Integer;
begin
  case MovementType of
    0: Result := 8;
    1: Result := 9;
  else
    Result := -1;
  end;
end;  // TAccessionLeafNode.GetImageIndex 

{-------------------------------------------------------------------------------
}
function TAccessionLeafNode.GetNodeContext: TNodeContext;
begin
  Result := ncAccession;
end;  // TAccessionLeafNode.GetNodeContext 

{-------------------------------------------------------------------------------
}
function TAccessionLeafNode.GetProperty(const AName: string): Variant;
begin
  Result := InternalGetProperty(AName);
end;  // TAccessionLeafNode.GetProperty 

{-------------------------------------------------------------------------------
}
function TAccessionLeafNode.InternalGetProperty(const AName: String): Variant;
begin
  Result := inherited InternalGetProperty(AName);
  
  if Result = Unassigned then begin
    if AName = PROP_WITH_ACQUISITION then
      Result := FWithAcquisition;
  end;
end;  // TAccessionLeafNode.InternalGetProperty 

{-------------------------------------------------------------------------------
}
procedure TAccessionLeafNode.SetMovementType(Value: Integer);
begin
  if not (Value in [0, 1]) then raise EBrowserNodeError.Create(
      ResStr_InvalidMovementChildNodeTypeIndexRequest);
  
  inherited SetMovementType(Value);
end;  // TAccessionLeafNode.SetMovementType 

{-==============================================================================
    TAccessionTopLevelNode
===============================================================================}
{-------------------------------------------------------------------------------
}
procedure TAccessionTopLevelNode.AddChildNodesOfType(ABrowserNodeClass: TBrowserNodeClass);
var
  lNewNode: TFolderNode;
begin
  if (ABrowserNodeClass = TAccessionDetailsLeafNode) or
     (ABrowserNodeClass = TAcquisitionDetailsLeafNode) then
    PopulateFromDatabase(TLeafNodeClass(ABrowserNodeClass),
                         StoredProcByChildType(TLeafNodeClass(ABrowserNodeClass)))
  else
  if (ABrowserNodeClass = TConditionCheckFolderNode) or
     (ABrowserNodeClass = TMovementInFolderNode) or
     ((ABrowserNodeClass = TFundingFolderNode) and AppSettings.AllowFinance) or
     (ABrowserNodeClass = TMovementOutFolderNode) or
     (ABrowserNodeClass = TMultimediaFolderNode) or
     (ABrowserNodeClass = TObjectFolderNode) or (ABrowserNodeClass = TEnquiryFolderNode) or
     ((ABrowserNodeClass = TValuationFolderNode) and AppSettings.AllowFinance) then
  begin
    lNewNode:= TFolderNode(Tree.Items.AddTypedChild(Self, ABrowserNodeClass));
    lNewNode.Initialise(Key);
  end else
  if ((ABrowserNodeClass = TFundingFolderNode) and not AppSettings.AllowFinance) or
     ((ABrowserNodeClass = TValuationFolderNode) and not AppSettings.AllowFinance) then
    Exit //Do not add
  else
    raise EBrowserNodeError.Create(ResStr_InvalidChildNodeTypeIndexRequest);
end;  // TAccessionTopLevelNode.AddChildNodesOfType 

{-------------------------------------------------------------------------------
  Adds a node to the treeview of the required type. 
}
function TAccessionTopLevelNode.AddNode(ATree: TRapidTree; AMenuIndex: integer): TFlyNode;
var
  lFolderNode: TBrowserNode;
begin
  // When adding leaf nodes that may well be in subfolders, the subfolders
  // need to be expanded before the nodes can be added.
  if MovementType = 0 then begin
    Result := TBrowserNode(Tree.Items.AddTypedChild(
        tree.CurrentTopLevelNode, TAcquisitionDetailsLeafNode));
  end
  else if MovementType = 1 then begin
    if AMenuIndex = 0 then begin
      lFolderNode := ReturnMovementFolder(True);
      lFolderNode.Expanded := True;
      Result := TBrowserNode(Tree.Items.AddTypedChild(
        lFolderNode, TAcquisitionDetailsLeafNode))
    end
    else begin
      lFolderNode := ReturnMovementFolder(False);
      lFolderNode.Expanded := True;
      Result := TBrowserNode(Tree.Items.AddTypedChild(
        lFolderNode, TDisposalDetailsLeafNode));
    end;
  end
  else
    raise EBrowserNodeError.Create(ResStr_IncorrectMovementType);
  
  TLeafNode(Result).InitialiseNewNode;
end;  // TAccessionTopLevelNode.AddNode 

{-------------------------------------------------------------------------------
}
function TAccessionTopLevelNode.GetAddButtonMenuCaption(Index: Integer): String;
begin
  case Index of
    0:  Result := ResStr_AddAcquisitionToSelectedItem;
    1:  Result := ResStr_AddDisposalToSelectedItem;
  else
    raise EAddMenuItemError.Create(ResStr_InvalidAddMenuIndexRequest);
  end;
end;  // TAccessionTopLevelNode.GetAddButtonMenuCaption 

{-------------------------------------------------------------------------------
}
function TAccessionTopLevelNode.GetAddButtonMenuCaptionsCount: Integer;
begin
  if MovementType = 0 then
    Result := 1
  else
    Result := 2;
end;  // TAccessionTopLevelNode.GetAddButtonMenuCaptionsCount 

{-------------------------------------------------------------------------------
}
function TAccessionTopLevelNode.GetAddMenuIsAdd(AMenuIndex: Integer): Boolean;
begin
  Result := True;
end;  // TAccessionTopLevelNode.GetAddMenuIsAdd 

{-------------------------------------------------------------------------------
}
function TAccessionTopLevelNode.GetAssociatedFrame: TBaseDetailFrameClass;
begin
  Result := TfraMovement;
end;  // TAccessionTopLevelNode.GetAssociatedFrame 

{-------------------------------------------------------------------------------
}
function TAccessionTopLevelNode.GetChildNodeType(Index: Integer): TBrowserNodeClass;
begin
  case Index of
    0:   if MovementType = 0 then
           Result := TAccessionDetailsLeafNode
         else
           Result := TMovementInFolderNode;
    1:   if MovementType = 0 then
           Result := TAcquisitionDetailsLeafNode
         else
           Result := TMovementOutFolderNode;
    2:   if MovementType = 0 then
           Result := TObjectFolderNode
         else
           Result := TConditionCheckFolderNode;
    3:   if MovementType = 0 then
           Result := TConditionCheckFolderNode
         else
           Result := TEnquiryFolderNode;
    4:   if MovementType = 0 then
           Result := TEnquiryFolderNode
         else
           Result := TMultimediaFolderNode;
    5:   if MovementType = 0 then
           Result := TMultimediaFolderNode
         else
           Result := TValuationFolderNode;
    6:   if MovementType = 0 then
           Result := TValuationFolderNode
         else
           Result := TFundingFolderNode;
    7: Result := TFundingFolderNode;
  else
    raise EBrowserNodeError.Create(ResStr_InvalidChildNodeTypeIndexRequest);
  end;
end;  // TAccessionTopLevelNode.GetChildNodeType 

{-------------------------------------------------------------------------------
}
function TAccessionTopLevelNode.GetChildNodeTypeCount: Integer;
begin
  if MovementType = 0 then
    Result:= 8
  else
    Result := 7;
end;  // TAccessionTopLevelNode.GetChildNodeTypeCount 

{-------------------------------------------------------------------------------
}
function TAccessionTopLevelNode.GetImageIndex: Integer;
begin
  if MovementType = 0 then
    Result := 8
  else
    Result := 9;
end;  // TAccessionTopLevelNode.GetImageIndex 

{-------------------------------------------------------------------------------
}
function TAccessionTopLevelNode.GetNodeContext: TNodeContext;
begin
  Result := ncAccession;
end;  // TAccessionTopLevelNode.GetNodeContext 

{-------------------------------------------------------------------------------
}
function TAccessionTopLevelNode.GetProperty(const AName: string): Variant;
begin
  Result := InternalGetProperty(AName);
end;  // TAccessionTopLevelNode.GetProperty 

{-------------------------------------------------------------------------------
}
function TAccessionTopLevelNode.InternalGetProperty(const AName: string): Variant;
begin
  Result := inherited InternalGetProperty(AName);
  
  if Result = Unassigned then begin
    if AName = PROP_NODE_CONTEXT then
      Result := ncAccession;
  end;
end;  // TAccessionTopLevelNode.InternalGetProperty 

{-------------------------------------------------------------------------------
}
function TAccessionTopLevelNode.ReturnMovementFolder(Inbound: boolean): TBrowserNode;
var
  i: Integer;
begin
  Result := nil;
  
  if Inbound then begin
    for i := 0 to Count -1 do
      if Item[i] is TMovementInFolderNode then Result := TBrowserNode(Item[i]);
  end else begin
    for i := 0 to Count -1 do
      if Item[i] is TMovementOutFolderNode then Result := TBrowserNode(Item[i]);
  end;
end;  // TAccessionTopLevelNode.ReturnMovementFolder 

{-------------------------------------------------------------------------------
}
procedure TAccessionTopLevelNode.SetMovementType(Value: Integer);
begin
  if not (Value in [0, 1]) then
    raise EBrowserNodeError(ResStr_InvalidMovementChildNodeTypeIndexRequest);
  
  inherited SetMovementType(Value);
end;  // TAccessionTopLevelNode.SetMovementType 

{-------------------------------------------------------------------------------
}
function TAccessionTopLevelNode.StoredProcByChildType(ALeafNodeClass: TLeafNodeClass): String;
begin
  Result := '';
  
  if (ALeafNodeClass = TAccessionDetailsLeafNode) then
    Result := 'usp_AccessionDetails_Select_ForMovement'
  else if (ALeafNodeClass = TAcquisitionDetailsLeafNode) then
    Result := 'usp_AcquisitionDetails_Select_ForMovement'
  else
    raise EBrowserNodeError.Create(ResStr_InvalidChildNodeTypeIndexRequest);
end;  // TAccessionTopLevelNode.StoredProcByChildType 

{-==============================================================================
    TAcquisitionDetailsLeafNode
===============================================================================}
{-------------------------------------------------------------------------------
}
class function TAcquisitionDetailsLeafNode.ClassTableName: String;
begin
  Result := '';
end;  // TAcquisitionDetailsLeafNode.ClassTableName 

{-------------------------------------------------------------------------------
  The MovementDetailsCollectionUnits is only loaded if there are two Movement_Of_Material records for the same Movement_Direction_Key. The stored proc. checks to see if this is true, and if it is, the associated frame will be TfraMovementDetails (which loads two tab pages). If it isn't true, then the TfraMaterialMovementDetails page will be loaded.
  
  This check cannot be done in the TfraMovementDetails frame, and has to be done here in the
      node. If the check was done in the BasePageControlFrame, and only one page was required,
      this one page would be loaded as a single tab-page (as opposed to a whole page).
}
function TAcquisitionDetailsLeafNode.GetAssociatedFrame: TBaseDetailFrameClass;
var
  lCount: Integer;
begin
  lCount := dmGeneral.GetStoredProcOutputParam('usp_MovementMaterialCount_Get',
                                              ['@Key', Key],
                                              '@Count');
  if lCount > 1 then Result := TfraMovementDetails
                else Result := TfraMaterialMovementDetails;
end;  // TAcquisitionDetailsLeafNode.GetAssociatedFrame 

{-------------------------------------------------------------------------------
}
function TAcquisitionDetailsLeafNode.GetCanAdd: Boolean;
begin
  Result := false;
end;  // TAcquisitionDetailsLeafNode.GetCanAdd 

{-------------------------------------------------------------------------------
}
function TAcquisitionDetailsLeafNode.GetCanDelete: Boolean;
begin
  Result := AppSettings.AllowMovementEdit and inherited GetCanDelete;
end;  // TAcquisitionDetailsLeafNode.GetCanDelete 

{-------------------------------------------------------------------------------
}
function TAcquisitionDetailsLeafNode.GetCanEdit: Boolean;
begin
  Result := AppSettings.AllowMovementEdit and inherited GetCanEdit;
end;  // TAcquisitionDetailsLeafNode.GetCanEdit 

{-------------------------------------------------------------------------------
}
function TAcquisitionDetailsLeafNode.GetDomainStoredProc: String;
begin
  Result := 'usp_Movement_DomainMask_Get';
end;  // TAcquisitionDetailsLeafNode.GetDomainStoredProc 

{-------------------------------------------------------------------------------
}
function TAcquisitionDetailsLeafNode.GetImageIndex: Integer;
begin
  Result := 22;
end;  // TAcquisitionDetailsLeafNode.GetImageIndex 

{-------------------------------------------------------------------------------
}
function TAcquisitionDetailsLeafNode.GetProperty(const AName: string): Variant;
begin
  Result := InternalGetProperty(AName);
end;  // TAcquisitionDetailsLeafNode.GetProperty 

{-------------------------------------------------------------------------------
  Go to the top level node to get the movement type 
}
function TAcquisitionDetailsLeafNode.InternalGetProperty(const AName: String): Variant;
begin
  Result := inherited InternalGetProperty(AName);
  
  if Result=Unassigned then begin
    if AName = PROP_MOVEMENT_TYPE then begin
      if (Assigned(TopLevelNode) and (TopLevelNode is TBaseMovementTopLevelNode)) then
        Result := TBaseMovementTopLevelNode(TopLevelNode).MovementType
      else
        raise EBrowserNodeError.Create(ResStr_InvalidNode);
    end else
    if AName = PROP_MOVEMENT_OUTBOUND then
      Result := 0;
  end;
end;  // TAcquisitionDetailsLeafNode.InternalGetProperty 

{-------------------------------------------------------------------------------
}
procedure TAcquisitionDetailsLeafNode.SetCaption(ARecordset: _Recordset);
begin
  case TBaseMovementTopLevelNode(TopLevelNode).MovementType of
    2 : Caption := ResStr_MaterialLoaned;   // Loan in
    3 : Caption := ResStr_MaterialReturned; // Loan out
  else
    Caption:= ResStr_AcquisitionDetails;
  end;
  Caption := Caption + ' - ' +
                      dmGeneral.GetVagueDateStringFromRecordset(ARecordset, '');
end;  // TAcquisitionDetailsLeafNode.SetCaption 

{-------------------------------------------------------------------------------
}
procedure TAcquisitionDetailsLeafNode.SetSecurity;
var
  lTopLevelNode: TFlyNode;
begin
  lTopLevelNode := TopLevelNode;
  if Assigned(lTopLevelNode) then
    if lTopLevelNode is TTopLevelNode then
      DomainMask := TTopLevelNode(lTopLevelNode).DomainMask;
end;  // TAcquisitionDetailsLeafNode.SetSecurity 

{-==============================================================================
    TAccessionDetailsLeafNode
===============================================================================}
{-------------------------------------------------------------------------------
}
class function TAccessionDetailsLeafNode.ClassTableName: String;
begin
  Result := '';
end;  // TAccessionDetailsLeafNode.ClassTableName 

{-------------------------------------------------------------------------------
}
function TAccessionDetailsLeafNode.GetAssociatedFrame: TBaseDetailFrameClass;
begin
  Result := TfraOwnershipDetails;
end;  // TAccessionDetailsLeafNode.GetAssociatedFrame 

{-------------------------------------------------------------------------------
}
function TAccessionDetailsLeafNode.GetCanAdd: Boolean;
begin
  Result := false;
end;  // TAccessionDetailsLeafNode.GetCanAdd 

{-------------------------------------------------------------------------------
}
function TAccessionDetailsLeafNode.GetCanDelete: Boolean;
begin
  Result := False;
end;  // TAccessionDetailsLeafNode.GetCanDelete 

{-------------------------------------------------------------------------------
}
function TAccessionDetailsLeafNode.GetCanEdit: Boolean;
begin
  Result := AppSettings.AllowMovementEdit and inherited GetCanEdit;
end;  // TAccessionDetailsLeafNode.GetCanEdit 

{-------------------------------------------------------------------------------
}
function TAccessionDetailsLeafNode.GetDomainStoredProc: String;
begin
  Result := 'usp_Movement_DomainMask_Get';
end;  // TAccessionDetailsLeafNode.GetDomainStoredProc 

{-------------------------------------------------------------------------------
}
function TAccessionDetailsLeafNode.GetImageIndex: Integer;
begin
  Result := 20;
end;  // TAccessionDetailsLeafNode.GetImageIndex 

{-------------------------------------------------------------------------------
}
function TAccessionDetailsLeafNode.GetProperty(const AName: string): Variant;
begin
  Result := InternalGetProperty(AName);
end;  // TAccessionDetailsLeafNode.GetProperty 

{-------------------------------------------------------------------------------
}
function TAccessionDetailsLeafNode.InternalGetProperty(const AName: String): Variant;
begin
  Result := inherited InternalGetProperty(AName);
  
  if Result=Unassigned then begin
    if AName = PROP_MOVEMENT_TYPE then
      Result := 0; // This node can only have one Movement_Type.
  end;
end;  // TAccessionDetailsLeafNode.InternalGetProperty 

{-------------------------------------------------------------------------------
}
procedure TAccessionDetailsLeafNode.SetCaption(ARecordset: _Recordset);
begin
  Caption:= ResStr_AccessionDetails + ' - ' +
      dmGeneral.GetVagueDateStringFromRecordset(ARecordset, '');
end;  // TAccessionDetailsLeafNode.SetCaption 

{-------------------------------------------------------------------------------
}
procedure TAccessionDetailsLeafNode.SetSecurity;
var
  lTopLevelNode: TFlyNode;
begin
  lTopLevelNode := TopLevelNode;
  if Assigned(lTopLevelNode) then
    if lTopLevelNode is TTopLevelNode then
      DomainMask := TTopLevelNode(lTopLevelNode).DomainMask;
end;  // TAccessionDetailsLeafNode.SetSecurity 

{-==============================================================================
    TLoanTopLevelNode
===============================================================================}
{-------------------------------------------------------------------------------
}
procedure TLoanTopLevelNode.AddChildNodesOfType(ABrowserNodeClass: TBrowserNodeClass);
var
  lNewNode: TFolderNode;
begin
  if (ABrowserNodeClass = TAcquisitionDetailsLeafNode) or
     (ABrowserNodeClass = TDisposalDetailsLeafNode) then
    PopulateFromDatabase(TLeafNodeClass(ABrowserNodeClass),
                         StoredProcByChildType(TLeafNodeClass(ABrowserNodeClass)))
  else
  if (ABrowserNodeClass = TConditionCheckFolderNode) or
     ((ABrowserNodeClass = TFundingFolderNode) and AppSettings.AllowFinance) or
     (ABrowserNodeClass = TMultimediaFolderNode) or (ABrowserNodeClass = TObjectFolderNode) or
     ((ABrowserNodeClass = TValuationFolderNode) and AppSettings.AllowFinance) or
     (ABrowserNodeClass = TEnquiryFolderNode) then
  begin
    lNewNode:= TFolderNode(Tree.Items.AddTypedChild(Self, ABrowserNodeClass));
    lNewNode.Initialise(Key);
  end else
  if ((ABrowserNodeClass = TFundingFolderNode) and not AppSettings.AllowFinance) or
     ((ABrowserNodeClass = TValuationFolderNode) and not AppSettings.AllowFinance) then
    Exit //Do not add
  else
    raise EBrowserNodeError.Create(ResStr_InvalidChildNodeTypeIndexRequest);
end;  // TLoanTopLevelNode.AddChildNodesOfType 

{-------------------------------------------------------------------------------
}
function TLoanTopLevelNode.GetAssociatedFrame: TBaseDetailFrameClass;
begin
  Result := TfraMovement;
end;  // TLoanTopLevelNode.GetAssociatedFrame 

{-------------------------------------------------------------------------------
}
function TLoanTopLevelNode.GetChildNodeType(Index: Integer): TBrowserNodeClass;
begin
  case Index of
    0: if MovementType = 2 then
         Result := TAcquisitionDetailsLeafNode
       else
         Result := TDisposalDetailsLeafNode;
    1: if MovementType = 2 then
         Result := TDisposalDetailsLeafNode
       else
         Result := TAcquisitionDetailsLeafNode;
    2: Result := TObjectFolderNode;
    3: Result := TConditionCheckFolderNode;
    4: Result := TEnquiryFolderNode;
    5: Result := TMultimediaFolderNode;
    6: Result := TValuationFolderNode;
    7: Result := TFundingFolderNode;
  else
    raise EBrowserNodeError.Create(ResStr_InvalidChildNodeTypeIndexRequest);
  end;
end;  // TLoanTopLevelNode.GetChildNodeType 

{-------------------------------------------------------------------------------
}
function TLoanTopLevelNode.GetChildNodeTypeCount: Integer;
begin
  Result := 8;
end;  // TLoanTopLevelNode.GetChildNodeTypeCount 

{-------------------------------------------------------------------------------
}
function TLoanTopLevelNode.GetImageIndex: Integer;
begin
  if MovementType = 2 then
    Result := 11
  else
    Result := 12;
end;  // TLoanTopLevelNode.GetImageIndex 

{-------------------------------------------------------------------------------
}
function TLoanTopLevelNode.GetNodeContext: TNodeContext;
begin
  Result := ncLoan;
end;  // TLoanTopLevelNode.GetNodeContext 

{-------------------------------------------------------------------------------
}
function TLoanTopLevelNode.GetProperty(const AName: string): Variant;
begin
  Result := InternalGetProperty(AName);
end;  // TLoanTopLevelNode.GetProperty 

{-------------------------------------------------------------------------------
}
function TLoanTopLevelNode.InternalGetProperty(const AName: string): Variant;
begin
  Result := inherited InternalGetProperty(AName);
  
  if Result = Unassigned then
    if AName = PROP_NODE_CONTEXT then
      Result := ncLoan;
end;  // TLoanTopLevelNode.InternalGetProperty 

{-------------------------------------------------------------------------------
}
procedure TLoanTopLevelNode.SetMovementType(Value: Integer);
begin
  if not (Value in [2, 3]) then
    raise EBrowserNodeError(ResStr_InvalidMovementChildNodeTypeIndexRequest);
  
  inherited SetMovementType(Value);
end;  // TLoanTopLevelNode.SetMovementType 

{-------------------------------------------------------------------------------
}
function TLoanTopLevelNode.StoredProcByChildType(ALeafNodeClass: TLeafNodeClass): String;
begin
  Result := '';
  
  if (ALeafNodeClass = TAcquisitionDetailsLeafNode) then
    Result := 'usp_AcquisitionDetails_Select_ForMovement'
  else if (ALeafNodeClass = TDisposalDetailsLeafNode) then
    Result := 'usp_DisposalDetails_Select_ForMovement'
  else
    raise EBrowserNodeError.Create(ResStr_InvalidChildNodeTypeIndexRequest);
end;  // TLoanTopLevelNode.StoredProcByChildType 

{-==============================================================================
    TDisposalDetailsLeafNode
===============================================================================}
{-------------------------------------------------------------------------------
}
class function TDisposalDetailsLeafNode.ClassTableName: String;
begin
  Result := '';
end;  // TDisposalDetailsLeafNode.ClassTableName 

{-------------------------------------------------------------------------------
  Needs to check the number of Disposal details nodes so it knows whether to display the
      TfraMovementDetails base page control frame, or just TfraMaterialMovementDetails as the
      whole page (i.e. no point having the frame as a tab page if there is only one tab).
}
function TDisposalDetailsLeafNode.GetAssociatedFrame: TBaseDetailFrameClass;
var
  lCount: Integer;
  lTopLevelNode: TFlyNode;
begin
  Result := nil;  // To remove compile-time warning.
  
  lCount := dmGeneral.GetStoredProcOutputParam('usp_MovementMaterialCount_Get',
                                              ['@Key', Key],
                                              '@Count');
  // If there is more than one Disposal Details node we will want the Movement
  // Collection Unit tab page to come up, so load the base page with 2 tabs
  if lCount > 1 then
    Result := TfraMovementDetails
  // Otherwise, we need to decide whether to show the 'Material to Unknown
  // Destination' tab, or the 'Material Movement Details' tab page. The appropriate
  // tab page to show is chosen here because one tab page is required, and
  // choosing it in the base page will display it as a single tab page (as
  // opposed to filling the whole frame).
  else begin
    lTopLevelNode := TopLevelNode;
    if Assigned(lTopLevelNode) then
      if lTopLevelNode is TBaseMovementTopLevelNode then
        if TBaseMovementTopLevelNode(lTopLevelNode).MovementType in [4, 7] then
          Result := TfraMaterialToUnknownDest
        else
          Result := TfraMaterialMovementDetails;
  end;
  
  if not Assigned(Result) then
    raise EBrowserNodeError.Create(ResStr_IncorrectMovementType);
end;  // TDisposalDetailsLeafNode.GetAssociatedFrame 

{-------------------------------------------------------------------------------
}
function TDisposalDetailsLeafNode.GetCanAdd: Boolean;
begin
  Result := false;
end;  // TDisposalDetailsLeafNode.GetCanAdd 

{-------------------------------------------------------------------------------
}
function TDisposalDetailsLeafNode.GetCanDelete: Boolean;
begin
  Result := AppSettings.AllowMovementEdit and inherited GetCanDelete;
end;  // TDisposalDetailsLeafNode.GetCanDelete 

{-------------------------------------------------------------------------------
}
function TDisposalDetailsLeafNode.GetCanEdit: Boolean;
begin
  Result := AppSettings.AllowMovementEdit and inherited GetCanEdit;
end;  // TDisposalDetailsLeafNode.GetCanEdit 

{-------------------------------------------------------------------------------
}
function TDisposalDetailsLeafNode.GetDomainStoredProc: String;
begin
  Result := 'usp_Movement_DomainMask_Get';
end;  // TDisposalDetailsLeafNode.GetDomainStoredProc 

{-------------------------------------------------------------------------------
}
function TDisposalDetailsLeafNode.GetImageIndex: Integer;
begin
  Result := 23;
end;  // TDisposalDetailsLeafNode.GetImageIndex 

{-------------------------------------------------------------------------------
}
function TDisposalDetailsLeafNode.GetProperty(const AName: string): Variant;
begin
  Result := InternalGetProperty(AName);
end;  // TDisposalDetailsLeafNode.GetProperty 

{-------------------------------------------------------------------------------
}
function TDisposalDetailsLeafNode.InternalGetProperty(const AName: String): Variant;
begin
  Result := inherited InternalGetProperty(AName);
  
  if Result=Unassigned then begin
    if AName = PROP_MOVEMENT_TYPE then begin
      if (Assigned(TopLevelNode) and (TopLevelNode is TBaseMovementTopLevelNode)) then
        Result := TBaseMovementTopLevelNode(TopLevelNode).MovementType
      else
        raise EBrowserNodeError.Create(ResStr_InvalidNode);
    end else
    if AName = PROP_MOVEMENT_OUTBOUND then
      Result := 1;
  end;
end;  // TDisposalDetailsLeafNode.InternalGetProperty 

{-------------------------------------------------------------------------------
}
procedure TDisposalDetailsLeafNode.SetCaption(ARecordset: _Recordset);
begin
  case TBaseMovementTopLevelNode(TopLevelNode).MovementType of
    2 : Caption := ResStr_MaterialReturned;   // Loan in
    3 : Caption := ResStr_MaterialLoaned;     // Loan out
  else
    Caption:= ResStr_DisposalDetails;
  end;
  Caption := Caption + ' - ' +
                      dmGeneral.GetVagueDateStringFromRecordset(ARecordset, '');
end;  // TDisposalDetailsLeafNode.SetCaption 

{-------------------------------------------------------------------------------
}
procedure TDisposalDetailsLeafNode.SetSecurity;
var
  lTopLevelNode: TFlyNode;
begin
  lTopLevelNode := TopLevelNode;
  if Assigned(lTopLevelNode) then
    if lTopLevelNode is TTopLevelNode then
      DomainMask := TTopLevelNode(lTopLevelNode).DomainMask;
end;  // TDisposalDetailsLeafNode.SetSecurity 

{-==============================================================================
    TDisposalUnknownDetailsLeafNode
===============================================================================}
{-------------------------------------------------------------------------------
}
class function TDisposalUnknownDetailsLeafNode.ClassTableName: String;
begin
  Result := '';
end;  // TDisposalUnknownDetailsLeafNode.ClassTableName 

{-------------------------------------------------------------------------------
  Needs to check the number of Disposal details nodes so it knows whether to display the
      TfraMovementDetails base page control frame, or just TfraMaterialToUnknownDest as the
      whole page (i.e. no point having the frame as a tab page if there is only one tab).
}
function TDisposalUnknownDetailsLeafNode.GetAssociatedFrame: TBaseDetailFrameClass;
var
  lCount: Integer;
begin
  lCount := dmGeneral.GetStoredProcOutputParam('usp_MovementMaterialCount_Get',
                                              ['@Key', Key],
                                              '@Count');
  if lCount > 1 then Result := TfraMovementDetails
                else Result := TfraMaterialToUnknownDest;
end;  // TDisposalUnknownDetailsLeafNode.GetAssociatedFrame 

{-------------------------------------------------------------------------------
}
function TDisposalUnknownDetailsLeafNode.GetCanAdd: Boolean;
begin
  Result := false;
end;  // TDisposalUnknownDetailsLeafNode.GetCanAdd 

{-------------------------------------------------------------------------------
}
function TDisposalUnknownDetailsLeafNode.GetCanDelete: Boolean;
begin
  Result := AppSettings.AllowMovementEdit and inherited GetCanDelete;
end;  // TDisposalUnknownDetailsLeafNode.GetCanDelete 

{-------------------------------------------------------------------------------
}
function TDisposalUnknownDetailsLeafNode.GetCanEdit: Boolean;
begin
  Result := AppSettings.AllowMovementEdit and inherited GetCanEdit;
end;  // TDisposalUnknownDetailsLeafNode.GetCanEdit 

{-------------------------------------------------------------------------------
}
function TDisposalUnknownDetailsLeafNode.GetDomainStoredProc: String;
begin
  Result := 'usp_Movement_DomainMask_Get';
end;  // TDisposalUnknownDetailsLeafNode.GetDomainStoredProc 

{-------------------------------------------------------------------------------
}
function TDisposalUnknownDetailsLeafNode.GetImageIndex: Integer;
begin
  Result := 24;
end;  // TDisposalUnknownDetailsLeafNode.GetImageIndex 

{-------------------------------------------------------------------------------
}
function TDisposalUnknownDetailsLeafNode.GetProperty(const AName: string): Variant;
begin
  Result := InternalGetProperty(AName);
end;  // TDisposalUnknownDetailsLeafNode.GetProperty 

{-------------------------------------------------------------------------------
}
function TDisposalUnknownDetailsLeafNode.InternalGetProperty(const AName: String): Variant;
begin
  Result := inherited InternalGetProperty(AName);
  
  if Result=Unassigned then begin
    if AName = PROP_MOVEMENT_TYPE then begin
      if (Assigned(TopLevelNode) and (TopLevelNode is
          TBaseMovementTopLevelNode)) then
        Result := TBaseMovementTopLevelNode(TopLevelNode).MovementType
      else
        raise EBrowserNodeError.Create(ResStr_InvalidNode);
    end else if AName = PROP_MOVEMENT_OUTBOUND then
      Result := 1;
  end;
end;  // TDisposalUnknownDetailsLeafNode.InternalGetProperty 

{-------------------------------------------------------------------------------
}
procedure TDisposalUnknownDetailsLeafNode.SetCaption(ARecordset: _Recordset);
begin
  Caption:= ResStr_DisposalDetails + ' - ' +
      dmGeneral.GetVagueDateStringFromRecordset(ARecordset, '');
end;  // TDisposalUnknownDetailsLeafNode.SetCaption 

{-------------------------------------------------------------------------------
}
procedure TDisposalUnknownDetailsLeafNode.SetSecurity;
var
  lTopLevelNode: TFlyNode;
begin
  lTopLevelNode := TopLevelNode;
  if Assigned(lTopLevelNode) then
    if lTopLevelNode is TTopLevelNode then
      DomainMask := TTopLevelNode(lTopLevelNode).DomainMask;
end;  // TDisposalUnknownDetailsLeafNode.SetSecurity 

{-==============================================================================
    TMovementLeafNode
===============================================================================}
{-------------------------------------------------------------------------------
}
function TMovementLeafNode.GetAssociatedFrame: TBaseDetailFrameClass;
begin
  Result := TfraMovement;
end;  // TMovementLeafNode.GetAssociatedFrame 

{-------------------------------------------------------------------------------
}
function TMovementLeafNode.GetImageIndex: Integer;
begin
  case MovementType of
    4: Result := 13;
    5: Result := 14;
    6: Result := 15;
    7: Result := 17;
    8: Result := 19;
    9: Result := 16;
  else
    Result := -1;
  end;
end;  // TMovementLeafNode.GetImageIndex 

{-------------------------------------------------------------------------------
}
function TMovementLeafNode.GetNodeContext: TNodeContext;
begin
  Result := ncMovement;
end;  // TMovementLeafNode.GetNodeContext 

{-------------------------------------------------------------------------------
}
procedure TMovementLeafNode.SetMovementType(Value: Integer);
begin
  if not (Value in [4, 5, 6, 7, 8, 9]) then raise EBrowserNodeError.Create(
      ResStr_InvalidMovementChildNodeTypeIndexRequest);
  
  inherited SetMovementType(Value);
end;  // TMovementLeafNode.SetMovementType 

{-==============================================================================
    TMovementTopLevelNode
===============================================================================}
{-------------------------------------------------------------------------------
}
procedure TMovementTopLevelNode.AddChildNodesOfType(ABrowserNodeClass: TBrowserNodeClass);
var
  lNewNode: TFolderNode;
begin
  if (ABrowserNodeClass = TAcquisitionDetailsLeafNode) or
     (ABrowserNodeClass = TDisposalDetailsLeafNode) or
     (ABrowserNodeClass = TDisposalUnknownDetailsLeafNode) or
     (ABrowserNodeClass = THostedMaterialDetailsLeafNode) or
     (ABrowserNodeClass = TTransferOfOwnershipDetailsLeafNode) then
    PopulateFromDatabase(TLeafNodeClass(ABrowserNodeClass),
                         StoredProcByChildType(TLeafNodeClass(ABrowserNodeClass)))
  else
  if (ABrowserNodeClass = TConditionCheckFolderNode) or
     ((ABrowserNodeClass = TFundingFolderNode) and AppSettings.AllowFinance) or
     (ABrowserNodeClass = TMultimediaFolderNode) or (ABrowserNodeClass = TObjectFolderNode) or
     ((ABrowserNodeClass = TValuationFolderNode) and AppSettings.AllowFinance) or
     (ABrowserNodeClass = TEnquiryFolderNode) then
  begin
    lNewNode:= TFolderNode(Tree.Items.AddTypedChild(Self, ABrowserNodeClass));
    lNewNode.Initialise(Key);
  end else
  if ((ABrowserNodeClass = TFundingFolderNode) and not AppSettings.AllowFinance) or
     ((ABrowserNodeClass = TValuationFolderNode) and not AppSettings.AllowFinance) then
    Exit //Do not add
  else
    raise EBrowserNodeError.Create(ResStr_InvalidChildNodeTypeIndexRequest);
end;  // TMovementTopLevelNode.AddChildNodesOfType 

{-------------------------------------------------------------------------------
  Adds a node to the treeview of the required type. 
}
function TMovementTopLevelNode.AddNode(ATree: TRapidTree; AMenuIndex: integer): TFlyNode;
begin
  case MovementType of
    4:     Result := Tree.Items.AddTypedChild(Self, TDisposalUnknownDetailsLeafNode);
    5,7,8: Result := Tree.Items.AddTypedChild(Self, TDisposalDetailsLeafNode);
    6:     Result := Tree.Items.AddTypedChild(Self, TAcquisitionDetailsLeafNode);
    else
      raise EBrowserNodeError.Create(ResStr_IncorrectMovementType);
  end;
  
  TLeafNode(Result).InitialiseNewNode;
end;  // TMovementTopLevelNode.AddNode 

{-------------------------------------------------------------------------------
}
function TMovementTopLevelNode.GetAddButtonMenuCaption(Index: Integer): String;
begin
  if Index = 0 then begin
    if MovementType = 6 then
      Result := ResStr_AddAcquisitionDetails
    else
    if MovementType in [4, 5, 7, 8] then
      Result := ResStr_AddDisposalDetails;
  end else
    raise EAddMenuItemError.Create(ResStr_InvalidAddMenuIndexRequest);
end;  // TMovementTopLevelNode.GetAddButtonMenuCaption 

{-------------------------------------------------------------------------------
}
function TMovementTopLevelNode.GetAddButtonMenuCaptionsCount: Integer;
begin
  if MovementType = 9 then
    Result := 0
  else
    Result := 1;
end;  // TMovementTopLevelNode.GetAddButtonMenuCaptionsCount 

{-------------------------------------------------------------------------------
}
function TMovementTopLevelNode.GetAddMenuIsAdd(AMenuIndex: Integer): Boolean;
begin
  Result := True;
end;  // TMovementTopLevelNode.GetAddMenuIsAdd 

{-------------------------------------------------------------------------------
}
function TMovementTopLevelNode.GetAssociatedFrame: TBaseDetailFrameClass;
begin
  Result := TfraMovement;
end;  // TMovementTopLevelNode.GetAssociatedFrame 

{-------------------------------------------------------------------------------
}
function TMovementTopLevelNode.GetChildNodeType(Index: Integer): TBrowserNodeClass;
begin
  case Index of
    0:   case MovementType of
           5: Result := TTransferOfOwnershipDetailsLeafNode;
           8: Result := TTransferOfOwnershipDetailsLeafNode;
           4: Result := TDisposalUnknownDetailsLeafNode;
           6: Result := TAcquisitionDetailsLeafNode;
           7: Result := TDisposalUnknownDetailsLeafNode;
         else //9
              Result := THostedMaterialDetailsLeafNode;
         end;
  
    1:   case MovementType of
           5: Result := TDisposalDetailsLeafNode;
           8: Result := TDisposalDetailsLeafNode;
           4: Result := TObjectFolderNode;
           6: Result := TObjectFolderNode;
           7: Result := TObjectFolderNode;
         else //9
              Result := TObjectFolderNode;
         end;
  
    2:   case MovementType of
           5: Result := TObjectFolderNode;
           8: Result := TObjectFolderNode;
           4: Result := TConditionCheckFolderNode;
           6: Result := TConditionCheckFolderNode;
           7: Result := TConditionCheckFolderNode;
         else //9
              Result := TConditionCheckFolderNode;
         end;
  
    3:   case MovementType of
           5: Result := TConditionCheckFolderNode;
           8: Result := TConditionCheckFolderNode;
           4: Result := TEnquiryFolderNode;
           6: Result := TEnquiryFolderNode;
           7: Result := TEnquiryFolderNode;
         else //9
              Result := TEnquiryFolderNode;
         end;
  
    4:   case MovementType of
           5: Result := TEnquiryFolderNode;
           8: Result := TEnquiryFolderNode;
           4: Result := TMultimediaFolderNode;
           6: Result := TMultimediaFolderNode;
           7: Result := TMultimediaFolderNode;
         else //9
              Result := TMultimediaFolderNode;
         end;
  
    5:   case MovementType of
           5: Result := TMultimediaFolderNode;
           8: Result := TMultimediaFolderNode;
           4: Result := TValuationFolderNode;
           6: Result := TValuationFolderNode;
           7: Result := TValuationFolderNode;
         else //9
              Result := TValuationFolderNode;
         end;
  
    6:   case MovementType of
           5: Result := TValuationFolderNode;
           8: Result := TValuationFolderNode;
           4: Result := TFundingFolderNode;
           6: Result := TFundingFolderNode;
           7: Result := TFundingFolderNode;
         else //9
              Result := TFundingFolderNode;
         end;
  
    7:   if MovementType = 5 then
           Result := TFundingFolderNode
         else //8
           Result := TFundingFolderNode;
  else
    raise EBrowserNodeError.Create(ResStr_InvalidChildNodeTypeIndexRequest);
  end;
end;  // TMovementTopLevelNode.GetChildNodeType 

{-------------------------------------------------------------------------------
}
function TMovementTopLevelNode.GetChildNodeTypeCount: Integer;
begin
  if MovementType in [5, 8] then
    Result := 8
  else //[4, 6, 7, 9]
    Result := 7;
end;  // TMovementTopLevelNode.GetChildNodeTypeCount 

{-------------------------------------------------------------------------------
}
function TMovementTopLevelNode.GetImageIndex: Integer;
begin
  case MovementType of
    4: Result := 13;
    5: Result := 14;
    6: Result := 15;
    7: Result := 17;
    8: Result := 19;
  else //9
    Result := 16;
  end;
end;  // TMovementTopLevelNode.GetImageIndex 

{-------------------------------------------------------------------------------
}
function TMovementTopLevelNode.GetNodeContext: TNodeContext;
begin
  Result := ncMovement;
end;  // TMovementTopLevelNode.GetNodeContext 

{-------------------------------------------------------------------------------
}
function TMovementTopLevelNode.GetProperty(const AName: string): Variant;
begin
  Result := InternalGetProperty(AName);
end;  // TMovementTopLevelNode.GetProperty 

{-------------------------------------------------------------------------------
}
function TMovementTopLevelNode.InternalGetProperty(const AName: string): Variant;
begin
  Result := inherited InternalGetProperty(AName);
  
  if Result = Unassigned then begin
    if AName = PROP_NODE_CONTEXT then
      Result := ncMovement;
  end;
end;  // TMovementTopLevelNode.InternalGetProperty 

{-------------------------------------------------------------------------------
}
procedure TMovementTopLevelNode.SetMovementType(Value: Integer);
begin
  if not (Value in [4, 5, 6, 7, 8, 9]) then
    raise EBrowserNodeError(ResStr_InvalidMovementChildNodeTypeIndexRequest);
  
  inherited SetMovementType(Value);
end;  // TMovementTopLevelNode.SetMovementType 

{-------------------------------------------------------------------------------
}
function TMovementTopLevelNode.StoredProcByChildType(ALeafNodeClass: TLeafNodeClass): String;
begin
  Result := '';
  
  if (ALeafNodeClass = TAcquisitionDetailsLeafNode) then
    Result :=  'usp_AcquisitionDetails_Select_ForMovement'
  else if (ALeafNodeClass = TDisposalDetailsLeafNode) then
    Result := 'usp_DisposalDetails_Select_ForMovement'
  else if (ALeafNodeClass = TDisposalUnknownDetailsLeafNode) then
    Result := 'usp_DisposalDetails_Select_ForMovement'
  else if (ALeafNodeClass = THostedMaterialDetailsLeafNode) then
    Result := 'usp_TransferOfOwnershipDetails_Select_ForMovement'
  else if (ALeafNodeClass = TTransferOfOwnershipDetailsLeafNode) then
    Result := 'usp_TransferOfOwnershipDetails_Select_ForMovement'
  else
    raise EBrowserNodeError.Create(ResStr_InvalidChildNodeTypeIndexRequest);
end;  // TMovementTopLevelNode.StoredProcByChildType 

{-==============================================================================
    TLoanFolderNode
===============================================================================}
{-------------------------------------------------------------------------------
}
function TLoanFolderNode.GetChildNodeType(Index: Integer): TBrowserNodeClass;
begin
  case Index of
    0: Result := TLoanLeafNode;
  else
    raise EBrowserNodeError.Create(ResStr_InvalidChildNodeTypeIndexRequest);
  end;
end;  // TLoanFolderNode.GetChildNodeType 

{-------------------------------------------------------------------------------
}
function TLoanFolderNode.GetChildNodeTypeCount: Integer;
begin
  Result:= 1;
end;  // TLoanFolderNode.GetChildNodeTypeCount 

{-------------------------------------------------------------------------------
}
function TLoanFolderNode.GetNodeContext: TNodeContext;
begin
  Result := ncLoan;
end;  // TLoanFolderNode.GetNodeContext 

{-------------------------------------------------------------------------------
}
function TLoanFolderNode.GetSearchType: SearchManager.TSearchType;
begin
  Result := stLoan;
end;  // TLoanFolderNode.GetSearchType 

{-------------------------------------------------------------------------------
}
function TLoanFolderNode.GetStoredProcParams: TVariantArray;
var
  lSortIndex: Integer;
begin
  lSortIndex := ViewTypeManager.ViewTypeByNodeContext(NodeContext).SortOrderDefaultIndex;
  
  Result := VarArrayOf(['@ParentKey', Key, '@MovementGroupType', 1,
                        '@SortOrderIndex', lSortIndex]);
end;  // TLoanFolderNode.GetStoredProcParams 

{-------------------------------------------------------------------------------
}
procedure TLoanFolderNode.SetCaption;
begin
  Text := ResStr_Loans;
end;  // TLoanFolderNode.SetCaption 

{-==============================================================================
    TLoanLeafNode
===============================================================================}
{-------------------------------------------------------------------------------
}
function TLoanLeafNode.GetAssociatedFrame: TBaseDetailFrameClass;
begin
  Result := TfraMovement;
end;  // TLoanLeafNode.GetAssociatedFrame 

{-------------------------------------------------------------------------------
}
function TLoanLeafNode.GetCanAdd: Boolean;
begin
  Result := false;
end;  // TLoanLeafNode.GetCanAdd 

{-------------------------------------------------------------------------------
}
function TLoanLeafNode.GetImageIndex: Integer;
begin
  case MovementType of
    2: Result := 11;
    3: Result := 12;
  else
    Result := -1;
  end;
end;  // TLoanLeafNode.GetImageIndex 

{-------------------------------------------------------------------------------
}
function TLoanLeafNode.GetNodeContext: TNodeContext;
begin
  Result := ncLoan;
end;  // TLoanLeafNode.GetNodeContext 

{-------------------------------------------------------------------------------
}
procedure TLoanLeafNode.SetMovementType(Value: Integer);
begin
  if not (Value in [2, 3]) then raise EBrowserNodeError.Create(
      ResStr_InvalidMovementChildNodeTypeIndexRequest);
  
  inherited SetMovementType(Value);
end;  // TLoanLeafNode.SetMovementType 

{-==============================================================================
    TMovementFolderNode
===============================================================================}
{-------------------------------------------------------------------------------
  Adds a node to the treeview of the required type. 
}
function TMovementFolderNode.AddNode(ATree: TRapidTree; AMenuIndex: integer): TFlyNode;
begin
  Result := TLeafNode(Tree.Items.AddTypedChild(Self, TMovementLeafNode));
  
  if AMenuIndex <= 5 then
    TMovementLeafNode(Result).MovementType := AMenuIndex + 4
  else
  if AMenuIndex = 6 then begin
    //do nothing yet
  end else
    raise EBrowserNodeError.Create(ResStr_IncorrectMovementType);
  
  TLeafNode(Result).InitialiseNewNode;
end;  // TMovementFolderNode.AddNode 

{-------------------------------------------------------------------------------
}
function TMovementFolderNode.GetAddButtonMenuCaption(Index: Integer): String;
begin
  case Index of
    0:  Result := ResStr_AddDestroyed;
    1:  Result := ResStr_AddDisposed;
    2:  Result := ResStr_AddInternalTransfer;
    3:  Result := ResStr_AddLost;
    4:  Result := ResStr_AddSold;
    5:  Result := ResStr_AddHostedMaterial;
    6:  Result := ResStr_LinkToExisting;
  else
    raise EAddMenuItemError.Create(ResStr_InvalidAddMenuIndexRequest);
  end;
end;  // TMovementFolderNode.GetAddButtonMenuCaption 

{-------------------------------------------------------------------------------
}
function TMovementFolderNode.GetAddButtonMenuCaptionsCount: Integer;
begin
  Result := 7;
end;  // TMovementFolderNode.GetAddButtonMenuCaptionsCount 

{-------------------------------------------------------------------------------
}
function TMovementFolderNode.GetAddMenuIsAdd(AMenuIndex: Integer): Boolean;
begin
  case AMenuIndex of
    6 : Result := False
  else
    Result := True;
  end;
end;  // TMovementFolderNode.GetAddMenuIsAdd 

{-------------------------------------------------------------------------------
}
function TMovementFolderNode.GetCanAdd: Boolean;
begin
  Result := AppSettings.AllowMovementEdit and inherited GetCanAdd;
end;  // TMovementFolderNode.GetCanAdd 

{-------------------------------------------------------------------------------
}
function TMovementFolderNode.GetCanDelete: Boolean;
begin
  Result := AppSettings.AllowMovementEdit and inherited GetCanDelete;
end;  // TMovementFolderNode.GetCanDelete 

{-------------------------------------------------------------------------------
}
function TMovementFolderNode.GetCanEdit: Boolean;
begin
  Result := AppSettings.AllowMovementEdit and inherited GetCanEdit;
end;  // TMovementFolderNode.GetCanEdit 

{-------------------------------------------------------------------------------
}
function TMovementFolderNode.GetChildNodeType(Index: Integer): TBrowserNodeClass;
begin
  case Index of
    0: Result := TMovementLeafNode;
  else
    raise EBrowserNodeError.Create(ResStr_InvalidChildNodeTypeIndexRequest);
  end;
end;  // TMovementFolderNode.GetChildNodeType 

{-------------------------------------------------------------------------------
}
function TMovementFolderNode.GetChildNodeTypeCount: Integer;
begin
  Result:= 1;
end;  // TMovementFolderNode.GetChildNodeTypeCount 

{-------------------------------------------------------------------------------
}
function TMovementFolderNode.GetNodeContext: TNodeContext;
begin
  Result := ncMovement;
end;  // TMovementFolderNode.GetNodeContext 

{-------------------------------------------------------------------------------
}
function TMovementFolderNode.GetSearchType: SearchManager.TSearchType;
begin
  Result := stMovement;
end;  // TMovementFolderNode.GetSearchType 

{-------------------------------------------------------------------------------
}
function TMovementFolderNode.GetStoredProcParams: TVariantArray;
var
  lSortIndex: Integer;
begin
  lSortIndex := ViewTypeManager.ViewTypeByNodeContext(NodeContext).SortOrderDefaultIndex;
  
  Result := VarArrayOf(['@ParentKey', Key, '@MovementGroupType', 2,
                        '@SortOrderIndex', lSortIndex]);
end;  // TMovementFolderNode.GetStoredProcParams 

{-------------------------------------------------------------------------------
}
procedure TMovementFolderNode.SetCaption;
begin
  Text := ResStr_Movements;
end;  // TMovementFolderNode.SetCaption 

{-==============================================================================
    TMovementInFolderNode
===============================================================================}
{-------------------------------------------------------------------------------
}
procedure TMovementInFolderNode.AddChildNodesOfType(ABrowserNodeClass: TBrowserNodeClass);
var
  lNewNode: TFolderNode;
begin
  if ABrowserNodeClass = TAccessionDetailsLeafNode then
    PopulateFromDatabase(TAccessionDetailsLeafNode,
                         StoredProcByChildType(TAccessionDetailsLeafNode))
  else
  if ABrowserNodeClass = TAcquisitionDetailsLeafNode then
    PopulateFromDatabase(TAcquisitionDetailsLeafNode,
                         StoredProcByChildType(TAcquisitionDetailsLeafNode))
  else
  if ABrowserNodeClass = TObjectFolderNode then begin
    lNewNode:= TFolderNode(Tree.Items.AddTypedChild(Self, ABrowserNodeClass));
    lNewNode.Initialise(Key);
  end else
    raise EBrowserNodeError.Create(ResStr_InvalidChildNodeTypeIndexRequest);
end;  // TMovementInFolderNode.AddChildNodesOfType 

{-------------------------------------------------------------------------------
}
function TMovementInFolderNode.AddNode(ATree: TRapidTree; AMenuIndex: integer): TFlyNode;
begin
  Result := ATree.Items.AddTypedChild(Self, TAcquisitionDetailsLeafNode);
  TLeafNode(Result).InitialiseNewNode;
end;  // TMovementInFolderNode.AddNode 

{-------------------------------------------------------------------------------
}
class function TMovementInFolderNode.ClassTableName: String;
begin
  Result := '';
end;  // TMovementInFolderNode.ClassTableName 

{-------------------------------------------------------------------------------
}
function TMovementInFolderNode.GetAddButtonMenuCaption(Index: Integer): String;
begin
  case Index of
    0:  Result := ResStr_Add;
  else
    raise EAddMenuItemError.Create(ResStr_InvalidAddMenuIndexRequest);
  end;
end;  // TMovementInFolderNode.GetAddButtonMenuCaption 

{-------------------------------------------------------------------------------
}
function TMovementInFolderNode.GetAddButtonMenuCaptionsCount: Integer;
begin
  Result := 1;
end;  // TMovementInFolderNode.GetAddButtonMenuCaptionsCount 

{-------------------------------------------------------------------------------
}
function TMovementInFolderNode.GetAddMenuIsAdd(AMenuIndex: Integer): Boolean;
begin
  Result := True;
end;  // TMovementInFolderNode.GetAddMenuIsAdd 

{-------------------------------------------------------------------------------
}
function TMovementInFolderNode.GetChildNodeType(Index: Integer): TBrowserNodeClass;
begin
  case Index of
    0: Result := TAccessionDetailsLeafNode;
    1: Result := TAcquisitionDetailsLeafNode;
    2: Result := TObjectFolderNode;
  else
    raise EBrowserNodeError.Create(ResStr_InvalidChildNodeTypeIndexRequest);
  end;
end;  // TMovementInFolderNode.GetChildNodeType 

{-------------------------------------------------------------------------------
}
function TMovementInFolderNode.GetChildNodeTypeCount: Integer;
begin
  Result:= 3;
end;  // TMovementInFolderNode.GetChildNodeTypeCount 

{-------------------------------------------------------------------------------
}
function TMovementInFolderNode.GetNodeContext: TNodeContext;
begin
  Result := ncMovementIn;
end;  // TMovementInFolderNode.GetNodeContext 

{-------------------------------------------------------------------------------
}
procedure TMovementInFolderNode.SetCaption;
begin
  Text := ResStr_MovementIn;
end;  // TMovementInFolderNode.SetCaption 

{-------------------------------------------------------------------------------
}
function TMovementInFolderNode.StoredProcByChildType(ALeafNodeClass: TLeafNodeClass): String;
begin
  Result := '';
  
  if ALeafNodeClass = TAccessionDetailsLeafNode then
    Result := 'usp_AccessionDetails_Select_ForMovement'
  else if ALeafNodeClass = TAcquisitionDetailsLeafNode then
    Result := 'usp_AcquisitionDetails_Select_ForMovement'
  else
    raise EBrowserNodeError.Create(ResStr_InvalidChildNodeTypeIndexRequest);
end;  // TMovementInFolderNode.StoredProcByChildType 

{-==============================================================================
    TMovementOutFolderNode
===============================================================================}
{-------------------------------------------------------------------------------
}
procedure TMovementOutFolderNode.AddChildNodesOfType(ABrowserNodeClass: TBrowserNodeClass);
var
  lNewNode: TFolderNode;
begin
  if ABrowserNodeClass = TDisposalDetailsLeafNode then
    PopulateFromDatabase(TDisposalDetailsLeafNode,
                         StoredProcByChildType(TDisposalDetailsLeafNode))
  else
  if ABrowserNodeClass = TTransferOfOwnershipDetailsLeafNode then
    PopulateFromDatabase(TTransferOfOwnershipDetailsLeafNode,
                         StoredProcByChildType(TTransferOfOwnershipDetailsLeafNode))
  else
  if ABrowserNodeClass = TObjectFolderNode then begin
    lNewNode:= TFolderNode(Tree.Items.AddTypedChild(Self, ABrowserNodeClass));
    lNewNode.Initialise(Key);
  end else
    raise EBrowserNodeError.Create(ResStr_InvalidChildNodeTypeIndexRequest);
end;  // TMovementOutFolderNode.AddChildNodesOfType 

{-------------------------------------------------------------------------------
}
function TMovementOutFolderNode.AddNode(ATree: TRapidTree; AMenuIndex: integer): TFlyNode;
begin
  Result := ATree.Items.AddTypedChild(Self, TDisposalDetailsLeafNode);
  TLeafNode(Result).InitialiseNewNode;
end;  // TMovementOutFolderNode.AddNode 

{-------------------------------------------------------------------------------
}
class function TMovementOutFolderNode.ClassTableName: String;
begin
  Result := '';
end;  // TMovementOutFolderNode.ClassTableName 

{-------------------------------------------------------------------------------
}
function TMovementOutFolderNode.GetAddButtonMenuCaption(Index: Integer): String;
begin
  case Index of
    0: Result := ResStr_Add;
  else
    raise EAddMenuItemError.Create(ResStr_InvalidAddMenuIndexRequest);
  end;
end;  // TMovementOutFolderNode.GetAddButtonMenuCaption 

{-------------------------------------------------------------------------------
}
function TMovementOutFolderNode.GetAddButtonMenuCaptionsCount: Integer;
begin
  Result := 1;
end;  // TMovementOutFolderNode.GetAddButtonMenuCaptionsCount 

{-------------------------------------------------------------------------------
}
function TMovementOutFolderNode.GetAddMenuIsAdd(AMenuIndex: Integer): Boolean;
begin
  case AMenuIndex of
    0 : Result := True
  else
    Result := False;
  end;
end;  // TMovementOutFolderNode.GetAddMenuIsAdd 

{-------------------------------------------------------------------------------
}
function TMovementOutFolderNode.GetChildNodeType(Index: Integer): TBrowserNodeClass;
begin
  case Index of
    0: Result := TDisposalDetailsLeafNode;
    1: Result := TTransferOfOwnershipDetailsLeafNode;
    2: Result := TObjectFolderNode;
  else
    raise EBrowserNodeError.Create(ResStr_InvalidChildNodeTypeIndexRequest);
  end;
end;  // TMovementOutFolderNode.GetChildNodeType 

{-------------------------------------------------------------------------------
}
function TMovementOutFolderNode.GetChildNodeTypeCount: Integer;
begin
  Result:= 3;
end;  // TMovementOutFolderNode.GetChildNodeTypeCount 

{-------------------------------------------------------------------------------
}
function TMovementOutFolderNode.GetNodeContext: TNodeContext;
begin
  Result := ncMovementOut;
end;  // TMovementOutFolderNode.GetNodeContext 

{-------------------------------------------------------------------------------
}
procedure TMovementOutFolderNode.SetCaption;
begin
  Text := ResStr_MovementOut;
end;  // TMovementOutFolderNode.SetCaption 

{-------------------------------------------------------------------------------
}
function TMovementOutFolderNode.StoredProcByChildType(ALeafNodeClass: TLeafNodeClass): String;
begin
  Result := '';
  
  if ALeafNodeClass = TDisposalDetailsLeafNode then
    Result := 'usp_DisposalDetails_Select_ForMovement'
  else if ALeafNodeClass = TTransferOfOwnershipDetailsLeafNode then
    Result := 'usp_TransferOfOwnershipDetails_Select_ForMovement'
  else
    raise EBrowserNodeError.Create(ResStr_InvalidChildNodeTypeIndexRequest);
end;  // TMovementOutFolderNode.StoredProcByChildType 

{-==============================================================================
    THostedMaterialDetailsLeafNode
===============================================================================}
{-------------------------------------------------------------------------------
}
class function THostedMaterialDetailsLeafNode.ClassTableName: String;
begin
  Result := '';
end;  // THostedMaterialDetailsLeafNode.ClassTableName 

{-------------------------------------------------------------------------------
}
function THostedMaterialDetailsLeafNode.GetAssociatedFrame: TBaseDetailFrameClass;
begin
  Result := TfraMaterialHostedMaterial;
end;  // THostedMaterialDetailsLeafNode.GetAssociatedFrame 

{-------------------------------------------------------------------------------
}
function THostedMaterialDetailsLeafNode.GetCanAdd: Boolean;
begin
  Result := false;
end;  // THostedMaterialDetailsLeafNode.GetCanAdd 

{-------------------------------------------------------------------------------
}
function THostedMaterialDetailsLeafNode.GetCanDelete: Boolean;
begin
  Result := False;
end;  // THostedMaterialDetailsLeafNode.GetCanDelete 

{-------------------------------------------------------------------------------
}
function THostedMaterialDetailsLeafNode.GetCanEdit: Boolean;
begin
  Result := AppSettings.AllowMovementEdit and inherited GetCanEdit;
end;  // THostedMaterialDetailsLeafNode.GetCanEdit 

{-------------------------------------------------------------------------------
}
function THostedMaterialDetailsLeafNode.GetDomainStoredProc: String;
begin
  Result := 'usp_Movement_DomainMask_Get';
end;  // THostedMaterialDetailsLeafNode.GetDomainStoredProc 

{-------------------------------------------------------------------------------
}
function THostedMaterialDetailsLeafNode.GetImageIndex: Integer;
begin
  Result := 18;
end;  // THostedMaterialDetailsLeafNode.GetImageIndex 

{-------------------------------------------------------------------------------
}
procedure THostedMaterialDetailsLeafNode.SetCaption(ARecordset: _Recordset);
begin
  Caption:= ResStr_HostedMaterial + ' - ' +
      dmGeneral.GetVagueDateStringFromRecordset(ARecordset, '');
end;  // THostedMaterialDetailsLeafNode.SetCaption 

{-------------------------------------------------------------------------------
}
procedure THostedMaterialDetailsLeafNode.SetSecurity;
var
  lTopLevelNode: TFlyNode;
begin
  lTopLevelNode := TopLevelNode;
  if Assigned(lTopLevelNode) then
    if lTopLevelNode is TTopLevelNode then
      DomainMask := TTopLevelNode(lTopLevelNode).DomainMask;
end;  // THostedMaterialDetailsLeafNode.SetSecurity 

{-==============================================================================
    TObjectFolderNode
===============================================================================}
{-------------------------------------------------------------------------------
}
procedure TObjectFolderNode.AddChildNodesOfType(ABrowserNodeClass: TBrowserNodeClass);
var
  lNewNode: TFolderNode;
begin
  if (ABrowserNodeClass = TCollectionFolderNode) or
     (ABrowserNodeClass = TSpecimenFolderNode) or
     (ABrowserNodeClass = TStoreFolderNode) then
  begin
    lNewNode:= TFolderNode(Tree.Items.AddTypedChild(Self, ABrowserNodeClass));
    lNewNode.Initialise(Key);
  end else
    raise EBrowserNodeError.Create(ResStr_InvalidChildNodeTypeIndexRequest);
end;  // TObjectFolderNode.AddChildNodesOfType 

{-------------------------------------------------------------------------------
}
class function TObjectFolderNode.ClassTableName: String;
begin
  Result := '';
end;  // TObjectFolderNode.ClassTableName 

{-------------------------------------------------------------------------------
}
function TObjectFolderNode.GetCanAdd: Boolean;
begin
  Result := false;
end;  // TObjectFolderNode.GetCanAdd 

{-------------------------------------------------------------------------------
}
function TObjectFolderNode.GetChildNodeType(Index: Integer): TBrowserNodeClass;
begin
  case Index of
    0: Result := TCollectionFolderNode;
    1: Result := TSpecimenFolderNode;
    2: Result := TStoreFolderNode;
  else
    raise EBrowserNodeError.Create(ResStr_InvalidChildNodeTypeIndexRequest);
  end;
end;  // TObjectFolderNode.GetChildNodeType 

{-------------------------------------------------------------------------------
}
function TObjectFolderNode.GetChildNodeTypeCount: Integer;
begin
  Result:= 3;
end;  // TObjectFolderNode.GetChildNodeTypeCount 

{-------------------------------------------------------------------------------
}
procedure TObjectFolderNode.SetCaption;
begin
  Text := ResStr_Objects;
end;  // TObjectFolderNode.SetCaption 

{-------------------------------------------------------------------------------
}
function TObjectFolderNode.StoredProcByChildType(ALeafNodeClass: TLeafNodeClass): String;
begin
  Result := '';
  raise EBrowserNodeError.Create(ResStr_InvalidChildNodeTypeIndexRequest);
end;  // TObjectFolderNode.StoredProcByChildType 

{-==============================================================================
    TTransferOfOwnershipDetailsLeafNode
===============================================================================}
{-------------------------------------------------------------------------------
}
class function TTransferOfOwnershipDetailsLeafNode.ClassTableName: String;
begin
  Result := '';;
end;  // TTransferOfOwnershipDetailsLeafNode.ClassTableName 

{-------------------------------------------------------------------------------
}
function TTransferOfOwnershipDetailsLeafNode.GetAssociatedFrame: TBaseDetailFrameClass;
begin
  Result := TfraOwnershipDetails;
end;  // TTransferOfOwnershipDetailsLeafNode.GetAssociatedFrame 

{-------------------------------------------------------------------------------
}
function TTransferOfOwnershipDetailsLeafNode.GetCanAdd: Boolean;
begin
  Result := false;
end;  // TTransferOfOwnershipDetailsLeafNode.GetCanAdd 

{-------------------------------------------------------------------------------
}
function TTransferOfOwnershipDetailsLeafNode.GetCanDelete: Boolean;
begin
  Result := False;
end;  // TTransferOfOwnershipDetailsLeafNode.GetCanDelete 

{-------------------------------------------------------------------------------
}
function TTransferOfOwnershipDetailsLeafNode.GetCanEdit: Boolean;
begin
  Result := AppSettings.AllowMovementEdit and inherited GetCanEdit;
end;  // TTransferOfOwnershipDetailsLeafNode.GetCanEdit 

{-------------------------------------------------------------------------------
}
function TTransferOfOwnershipDetailsLeafNode.GetImageIndex: Integer;
begin
  Result := 21;
end;  // TTransferOfOwnershipDetailsLeafNode.GetImageIndex 

{-------------------------------------------------------------------------------
}
function TTransferOfOwnershipDetailsLeafNode.GetProperty(const AName: string): Variant;
begin
  Result := InternalGetProperty(AName);
end;  // TTransferOfOwnershipDetailsLeafNode.GetProperty 

{-------------------------------------------------------------------------------
}
function TTransferOfOwnershipDetailsLeafNode.InternalGetProperty(const AName: String): Variant;
begin
  Result := inherited InternalGetProperty(AName);
  
  if Result=Unassigned then begin
    if AName = PROP_MOVEMENT_TYPE then begin
      if TopLevelNode is TBaseMovementTopLevelNode then
        Result := TBaseMovementTopLevelNode(TopLevelNode).MovementType;
    end;
  end;
end;  // TTransferOfOwnershipDetailsLeafNode.InternalGetProperty 

{-------------------------------------------------------------------------------
}
procedure TTransferOfOwnershipDetailsLeafNode.SetCaption(ARecordset: _Recordset);
begin
  Caption:= ResStr_TransferOfOwnershipDetails + ' - ' +
      dmGeneral.GetVagueDateStringFromRecordset(ARecordset, '');
end;  // TTransferOfOwnershipDetailsLeafNode.SetCaption 

end.
