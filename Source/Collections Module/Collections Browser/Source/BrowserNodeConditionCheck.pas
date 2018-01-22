{===============================================================================
  Unit:        BrowserNodeConditionCheck.pas

  Defines:     Many Classes

  Description: Contains node classes related to Condition Checks

  Model:       BrowserNodes.mpb

  Created:     August 2003

  Last revision information:
    $Revision: 46 $
    $Date: 3/01/05 15:26 $
    $Author: Anthonysimpson $

===============================================================================}
unit BrowserNodeConditionCheck;

interface

uses
  SysUtils, Windows, Messages, Classes, Graphics, Controls, Forms, Dialogs,
  TreeColl, BrowserNodeFramework, ResourceStrings, BaseDetailFrameUnit, ADODB,
  DataTypes, SearchManager, LuxembourgConstants, Variants, RapTree;

type
  TConditionCheckFolderNode = class (TFolderNode, IAddMenuOptions)
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
  end;
  
  TConditionCheckLeafNode = class (THyperlinkLeafNode)
  protected
    function GetAssociatedFrame: TBaseDetailFrameClass; override;
    function GetCanAdd: Boolean; override;
    function GetCanDelete: Boolean; override;
    function GetDomainStoredProc: String; override;
    function GetImageIndex: Integer; override;
    function GetNodeContext: TNodeContext; override;
    procedure SetCaption(ARecordset: _Recordset); override;
  public
    class function ClassTableName: String; override;
  end;
  
  TConditionCheckTopLevelNode = class (TTopLevelNode, IAdditionalProperties)
  private
    function GetProperty(const AName: string): Variant;
  protected
    procedure AddChildNodesOfType(ABrowserNodeClass: TBrowserNodeClass); override;
    function GetAssociatedFrame: TBaseDetailFrameClass; override;
    function GetChildNodeType(Index: Integer): TBrowserNodeClass; override;
    function GetChildNodeTypeCount: Integer; override;
    function GetDomainStoredProc: String; override;
    function GetImageIndex: Integer; override;
    function GetNodeContext: TNodeContext; override;
    function InternalGetProperty(const AName: string): Variant; override;
    procedure SetCaption(ARecordset: _Recordset); override;
    function StoredProcByChildType(ALeafNodeClass: TLeafNodeClass): String; override;
  public
    class function ClassTableName: String; override;
  end;
  
  TFundingFolderNode = class (TFolderNode, IAddMenuOptions)
  protected
    function GetAddButtonMenuCaption(Index: Integer): String; override;
    function GetAddButtonMenuCaptionsCount: Integer; override;
    function GetAddMenuIsAdd(AMenuIndex: Integer): Boolean; override;
    function GetChildNodeType(Index: Integer): TBrowserNodeClass; override;
    function GetChildNodeTypeCount: Integer; override;
    function GetStoredProcParams: TVariantArray; override;
    function GetTableName: String; override;
    procedure SetCaption; override;
    function StoredProcByChildType(ALeafNodeClass: TLeafNodeClass): String; override;
  public
    class function ClassTableName: String; override;
  end;
  
  TFundingLeafNode = class (TLeafNode, IAdditionalProperties)
  private
    function GetProperty(const AName: string): Variant;
  protected
    function GetAssociatedFrame: TBaseDetailFrameClass; override;
    function GetCanAdd: Boolean; override;
    function GetCanDelete: Boolean; override;
    function GetCanEdit: Boolean; override;
    function GetImageIndex: Integer; override;
    function GetTableName: String; override;
    function InternalGetProperty(const AName: String): Variant; override;
    procedure SetCaption(ARecordset: _Recordset); override;
  public
    class function ClassTableName: String; override;
  end;
  
  TJobLeafNode = class (THyperlinkLeafNode)
  protected
    function GetAssociatedFrame: TBaseDetailFrameClass; override;
    function GetCanDelete: Boolean; override;
    function GetDomainStoredProc: String; override;
    function GetImageIndex: Integer; override;
    function GetNodeContext: TNodeContext; override;
    procedure SetCaption(ARecordset: _Recordset); override;
  public
    class function ClassTableName: String; override;
  end;
  
  TJobFolderNode = class (TFolderNode, IAddMenuOptions)
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
  end;
  
  TJobTopLevelNode = class (TTopLevelNode, IAdditionalProperties)
  private
    function GetProperty(const AName: string): Variant;
  protected
    procedure AddChildNodesOfType(ABrowserNodeClass: TBrowserNodeClass); override;
    function GetAssociatedFrame: TBaseDetailFrameClass; override;
    function GetChildNodeType(Index: Integer): TBrowserNodeClass; override;
    function GetChildNodeTypeCount: Integer; override;
    function GetDomainStoredProc: String; override;
    function GetImageIndex: Integer; override;
    function GetNodeContext: TNodeContext; override;
    function InternalGetProperty(const AName: string): Variant; override;
    procedure SetCaption(ARecordset: _Recordset); override;
    function StoredProcByChildType(ALeafNodeClass: TLeafNodeClass): String; override;
  public
    class function ClassTableName: String; override;
  end;
  
  TJobAppliedToFolderNode = class (TFolderNode)
  protected
    function GetCanAdd: Boolean; override;
    function GetChildNodeType(Index: Integer): TBrowserNodeClass; override;
    function GetChildNodeTypeCount: Integer; override;
    function GetNodeContext: TNodeContext; override;
    procedure SetCaption; override;
    function StoredProcByChildType(ALeafNodeClass: TLeafNodeClass): String; override;
  public
    class function ClassTableName: String; override;
  end;
  
  TTaskIdentifiedFolderNode = class (TFolderNode, IAddMenuOptions)
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
  
  TTaskIdentifiedLeafNode = class (THyperlinkLeafNode)
  protected
    function GetAssociatedFrame: TBaseDetailFrameClass; override;
    function GetDomainStoredProc: String; override;
    function GetImageIndex: Integer; override;
    function GetNodeContext: TNodeContext; override;
    procedure SetCaption(ARecordset: _Recordset); override;
  public
    class function ClassTableName: String; override;
  end;
  
  TTaskIdentifiedTopLevelNode = class (TTopLevelNode, IAdditionalProperties)
  private
    function GetProperty(const AName: string): Variant;
  protected
    procedure AddChildNodesOfType(ABrowserNodeClass: TBrowserNodeClass); override;
    function GetAssociatedFrame: TBaseDetailFrameClass; override;
    function GetCanAdd: Boolean; override;
    function GetChildNodeType(Index: Integer): TBrowserNodeClass; override;
    function GetChildNodeTypeCount: Integer; override;
    function GetDomainStoredProc: String; override;
    function GetImageIndex: Integer; override;
    function GetNodeContext: TNodeContext; override;
    function InternalGetProperty(const AName: string): Variant; override;
    procedure SetCaption(ARecordset: _Recordset); override;
    function StoredProcByChildType(ALeafNodeClass: TLeafNodeClass): String; override;
  public
    class function ClassTableName: String; override;
  end;
  
//==============================================================================
implementation

uses
  BrowserNodeCollectionUnits, BrowserNodeCommon, BrowserNodeMovement, GeneralData,
  ApplicationSettings, FrameCondition, FrameTask, FrameJob, FrameFunding;

{-==============================================================================
    TConditionCheckFolderNode
===============================================================================}
{-------------------------------------------------------------------------------
}
class function TConditionCheckFolderNode.ClassTableName: String;
begin
  Result := TN_CONSERVATION_CHECK;
end;  // TConditionCheckFolderNode.ClassTableName 

{-------------------------------------------------------------------------------
}
procedure TConditionCheckFolderNode.DeleteLink(const AJoinTableKey: String);
begin
  case ParentNodeContext of
    ncCollection,
    ncSpecimen,
    ncStore      : dmGeneral.RunStoredProc('usp_CollectionUnitCheck_Delete',
                                           ['@Key', AJoinTableKey]);
    ncAccession,
    ncLoan,
    ncMovement   : dmGeneral.RunStoredProc('usp_MovementConservationCheck_Delete',
                                           ['@Key', AJoinTableKey]);
  else
    raise EBrowserNodeError.Create(ResStr_ChildNodeContextInvalid);
  end
end;  // TConditionCheckFolderNode.DeleteLink 

{-------------------------------------------------------------------------------
}
function TConditionCheckFolderNode.GetAddButtonMenuCaption(Index: Integer): String;
begin
  case Index of
    0:  Result := ResStr_AddNew;
    1:  Result := ResStr_LinkToExisting;
  else
    raise EAddMenuItemError.Create(ResStr_InvalidAddMenuIndexRequest);
  end;
end;  // TConditionCheckFolderNode.GetAddButtonMenuCaption 

{-------------------------------------------------------------------------------
}
function TConditionCheckFolderNode.GetAddButtonMenuCaptionsCount: Integer;
begin
  Result := 2;
end;  // TConditionCheckFolderNode.GetAddButtonMenuCaptionsCount 

{-------------------------------------------------------------------------------
}
function TConditionCheckFolderNode.GetAddMenuIsAdd(AMenuIndex: Integer): Boolean;
begin
  Result := (AMenuIndex = 0);
end;  // TConditionCheckFolderNode.GetAddMenuIsAdd 

{-------------------------------------------------------------------------------
}
function TConditionCheckFolderNode.GetChildNodeType(Index: Integer): TBrowserNodeClass;
begin
  case Index of
    0: Result := TConditionCheckLeafNode;
  else
    raise EBrowserNodeError.Create(ResStr_InvalidChildNodeTypeIndexRequest);
  end;
end;  // TConditionCheckFolderNode.GetChildNodeType 

{-------------------------------------------------------------------------------
}
function TConditionCheckFolderNode.GetChildNodeTypeCount: Integer;
begin
  Result:= 1;
end;  // TConditionCheckFolderNode.GetChildNodeTypeCount 

{-------------------------------------------------------------------------------
}
function TConditionCheckFolderNode.GetNodeContext: TNodeContext;
begin
  Result := ncConditionCheck;
end;  // TConditionCheckFolderNode.GetNodeContext 

{-------------------------------------------------------------------------------
}
function TConditionCheckFolderNode.GetSearchType: SearchManager.TSearchType;
begin
  Result := stConditionCheck;
end;  // TConditionCheckFolderNode.GetSearchType 

{-------------------------------------------------------------------------------
}
procedure TConditionCheckFolderNode.SetCaption;
begin
  Text := ResStr_ConditionChecks;
end;  // TConditionCheckFolderNode.SetCaption 

{-------------------------------------------------------------------------------
}
function TConditionCheckFolderNode.StoredProcByChildType(ALeafNodeClass: TLeafNodeClass): 
    String;
begin
  Result := '';
  
  if ALeafNodeClass = TConditionCheckLeafNode then
    case ParentNodeContext of
      ncCollection,
      ncSpecimen,
      ncStore      : Result := 'usp_ConditionChecks_Select_ForCollectionUnit';
      ncTask       : Result := 'usp_ConditionChecks_Select_ForTask';
      ncAccession,
      ncLoan,
      ncMovement   : Result := 'usp_ConditionChecks_Select_ForMovement';
    else
      raise EBrowserNodeError.Create(ResStr_ChildNodeContextInvalid);
    end
  else
    raise EBrowserNodeError.Create(ResStr_InvalidChildNodeTypeIndexRequest);
end;  // TConditionCheckFolderNode.StoredProcByChildType 

{-------------------------------------------------------------------------------
}
function TConditionCheckFolderNode.UpdateNodeRelationship(const NewNodeKey: String): String;
begin
  case ParentNodeContext of
    ncCollection,
    ncSpecimen,
    ncStore      : Result := VarToStr(dmGeneral.GetStoredProcOutputParam(
        'usp_ConditionCheck_Update_ForCollectionUnit',
                     ['@ParentKey', Key, '@ChildKey', NewNodeKey], '@JoinKey'));
    ncMovement,
    ncAccession,
    ncLoan       : Result := VarToStr(dmGeneral.GetStoredProcOutputParam(
        'usp_ConditionCheck_Update_ForMovement',
                     ['@ParentKey', Key, '@ChildKey', NewNodeKey], '@JoinKey'));
    ncTask       :
      begin
        dmGeneral.RunStoredProc('usp_ConditionCheck_Update_ForTask',
                     ['@ParentKey', Key, '@ChildKey', NewNodeKey]);
        EnforceSingleChildNode(NewNodeKey);
      end;
  else
    raise EAddMenuItemError.Create(ResStr_InvalidNodeContext);
  end;
end;  // TConditionCheckFolderNode.UpdateNodeRelationship 

{-==============================================================================
    TConditionCheckLeafNode
===============================================================================}
{-------------------------------------------------------------------------------
}
class function TConditionCheckLeafNode.ClassTableName: String;
begin
  Result := TN_CONSERVATION_CHECK;
end;  // TConditionCheckLeafNode.ClassTableName 

{-------------------------------------------------------------------------------
}
function TConditionCheckLeafNode.GetAssociatedFrame: TBaseDetailFrameClass;
begin
  Result := TfraCondition;
end;  // TConditionCheckLeafNode.GetAssociatedFrame 

{-------------------------------------------------------------------------------
}
function TConditionCheckLeafNode.GetCanAdd: Boolean;
begin
  if NodeContext in [ncCollection, ncSpecimen, ncStore, ncTask,
                     ncAccession, ncLoan, ncMovement] then
    Result := inherited GetCanAdd
  else
    Result := False;
end;  // TConditionCheckLeafNode.GetCanAdd 

{-------------------------------------------------------------------------------
}
function TConditionCheckLeafNode.GetCanDelete: Boolean;
begin
  Result := (TopNodeContext <> ncTask) and inherited GetCanDelete;
end;  // TConditionCheckLeafNode.GetCanDelete 

{-------------------------------------------------------------------------------
}
function TConditionCheckLeafNode.GetDomainStoredProc: String;
begin
  Result := 'usp_ConditionCheck_DomainMask_Get';
end;  // TConditionCheckLeafNode.GetDomainStoredProc 

{-------------------------------------------------------------------------------
}
function TConditionCheckLeafNode.GetImageIndex: Integer;
begin
  Result := 25;
end;  // TConditionCheckLeafNode.GetImageIndex 

{-------------------------------------------------------------------------------
}
function TConditionCheckLeafNode.GetNodeContext: TNodeContext;
begin
  Result := ncConditionCheck;
end;  // TConditionCheckLeafNode.GetNodeContext 

{-------------------------------------------------------------------------------
}
procedure TConditionCheckLeafNode.SetCaption(ARecordset: _Recordset);
begin
  Caption:= VarToStr(ARecordset.Fields['Display_Caption'].Value);
end;  // TConditionCheckLeafNode.SetCaption 

{-==============================================================================
    TConditionCheckTopLevelNode
===============================================================================}
{-------------------------------------------------------------------------------
}
procedure TConditionCheckTopLevelNode.AddChildNodesOfType(ABrowserNodeClass: 
    TBrowserNodeClass);
var
  lNewNode: TFolderNode;
begin
  if (ABrowserNodeClass = TCollectionFolderNode) or
     (ABrowserNodeClass = TSpecimenFolderNode) or
     (ABrowserNodeClass = TStoreFolderNode) or
     (ABrowserNodeClass = TTaskIdentifiedFolderNode) or
     (ABrowserNodeClass = TMultimediaFolderNode) then
  begin
    lNewNode:= TFolderNode(Tree.Items.AddTypedChild(Self, ABrowserNodeClass));
    lNewNode.Initialise(Key);
  end else
    raise EBrowserNodeError.Create(ResStr_InvalidChildNodeTypeIndexRequest);
end;  // TConditionCheckTopLevelNode.AddChildNodesOfType 

{-------------------------------------------------------------------------------
}
class function TConditionCheckTopLevelNode.ClassTableName: String;
begin
  Result := TN_CONSERVATION_CHECK;
end;  // TConditionCheckTopLevelNode.ClassTableName 

{-------------------------------------------------------------------------------
}
function TConditionCheckTopLevelNode.GetAssociatedFrame: TBaseDetailFrameClass;
begin
  Result := TfraCondition;
end;  // TConditionCheckTopLevelNode.GetAssociatedFrame 

{-------------------------------------------------------------------------------
}
function TConditionCheckTopLevelNode.GetChildNodeType(Index: Integer): TBrowserNodeClass;
begin
  case Index of
    0: Result := TCollectionFolderNode;
    1: Result := TSpecimenFolderNode;
    2: Result := TStoreFolderNode;
    3: Result := TTaskIdentifiedFolderNode;
    4: Result := TMultimediaFolderNode;
  else
    raise EBrowserNodeError.Create(ResStr_InvalidChildNodeTypeIndexRequest);
  end;
end;  // TConditionCheckTopLevelNode.GetChildNodeType 

{-------------------------------------------------------------------------------
}
function TConditionCheckTopLevelNode.GetChildNodeTypeCount: Integer;
begin
  Result := 5;
end;  // TConditionCheckTopLevelNode.GetChildNodeTypeCount 

{-------------------------------------------------------------------------------
}
function TConditionCheckTopLevelNode.GetDomainStoredProc: String;
begin
  Result := 'usp_ConditionCheck_DomainMask_Get';
end;  // TConditionCheckTopLevelNode.GetDomainStoredProc 

{-------------------------------------------------------------------------------
}
function TConditionCheckTopLevelNode.GetImageIndex: Integer;
begin
  Result := 25;
end;  // TConditionCheckTopLevelNode.GetImageIndex 

{-------------------------------------------------------------------------------
}
function TConditionCheckTopLevelNode.GetNodeContext: TNodeContext;
begin
  Result := ncConditionCheck;
end;  // TConditionCheckTopLevelNode.GetNodeContext 

{-------------------------------------------------------------------------------
}
function TConditionCheckTopLevelNode.GetProperty(const AName: string): Variant;
begin
  Result := InternalGetProperty(AName);
end;  // TConditionCheckTopLevelNode.GetProperty 

{-------------------------------------------------------------------------------
}
function TConditionCheckTopLevelNode.InternalGetProperty(const AName: string): Variant;
begin
  Result := inherited InternalGetProperty(AName);
  
  if Result = Unassigned then begin
    if AName = PROP_NODE_CONTEXT then
      Result := ncConditionCheck;
  end;
end;  // TConditionCheckTopLevelNode.InternalGetProperty 

{-------------------------------------------------------------------------------
}
procedure TConditionCheckTopLevelNode.SetCaption(ARecordset: _Recordset);
begin
  Caption:= VarToStr(ARecordset.Fields['Display_Caption'].Value);
end;  // TConditionCheckTopLevelNode.SetCaption

{-------------------------------------------------------------------------------
}
function TConditionCheckTopLevelNode.StoredProcByChildType(ALeafNodeClass: TLeafNodeClass): 
    String;
begin
  Result := '';
  raise EBrowserNodeError.Create(ResStr_InvalidChildNodeTypeIndexRequest);
end;  // TConditionCheckTopLevelNode.StoredProcByChildType 

{-==============================================================================
    TFundingFolderNode
===============================================================================}
{-------------------------------------------------------------------------------
  Cannot retrieve a class table name since this is context sensitive. 
}
class function TFundingFolderNode.ClassTableName: String;
begin
  Result := '';
end;  // TFundingFolderNode.ClassTableName 

{-------------------------------------------------------------------------------
}
function TFundingFolderNode.GetAddButtonMenuCaption(Index: Integer): String;
begin
  case Index of
    0: Result := ResStr_Add;
  else
    raise EAddMenuItemError.Create(ResStr_InvalidAddMenuIndexRequest);
  end;
end;  // TFundingFolderNode.GetAddButtonMenuCaption 

{-------------------------------------------------------------------------------
}
function TFundingFolderNode.GetAddButtonMenuCaptionsCount: Integer;
begin
  Result := 1;
end;  // TFundingFolderNode.GetAddButtonMenuCaptionsCount 

{-------------------------------------------------------------------------------
}
function TFundingFolderNode.GetAddMenuIsAdd(AMenuIndex: Integer): Boolean;
begin
  case AMenuIndex of
    0 : Result := True
  else
    Result := False;
  end;
end;  // TFundingFolderNode.GetAddMenuIsAdd 

{-------------------------------------------------------------------------------
}
function TFundingFolderNode.GetChildNodeType(Index: Integer): TBrowserNodeClass;
begin
  case Index of
    0: Result := TFundingLeafNode;
  else
    raise EBrowserNodeError.Create(ResStr_InvalidChildNodeTypeIndexRequest);
  end;
end;  // TFundingFolderNode.GetChildNodeType 

{-------------------------------------------------------------------------------
}
function TFundingFolderNode.GetChildNodeTypeCount: Integer;
begin
  Result:= 1;
end;  // TFundingFolderNode.GetChildNodeTypeCount 

{-------------------------------------------------------------------------------
}
function TFundingFolderNode.GetStoredProcParams: TVariantArray;
begin
  Result:= VarArrayOf(['@ParentKey', Key, '@UserID', AppSettings.UserID]);
end;  // TFundingFolderNode.GetStoredProcParams 

{-------------------------------------------------------------------------------
  Override table name accessor since the ClassTableName method cannot be used. 
}
function TFundingFolderNode.GetTableName: String;
begin
  if ParentNodeContext in [ncMovement, ncAccession, ncLoan] then
    Result := TN_MOVEMENT_FUNDING
  else
    Result := TN_CONSERVATION_JOB_FUNDING;
end;  // TFundingFolderNode.GetTableName 

{-------------------------------------------------------------------------------
}
procedure TFundingFolderNode.SetCaption;
begin
  Text := ResStr_Funding;
end;  // TFundingFolderNode.SetCaption 

{-------------------------------------------------------------------------------
}
function TFundingFolderNode.StoredProcByChildType(ALeafNodeClass: TLeafNodeClass): String;
begin
  Result := '';
  
  if ALeafNodeClass = TFundingLeafNode then
    case ParentNodeContext of
      ncJob            : Result := 'usp_Funding_Select_ForJob';
      ncAccession,
      ncLoan,
      ncMovement       : Result := 'usp_Funding_Select_ForMovement';
    else
      raise EBrowserNodeError.Create(ResStr_ChildNodeContextInvalid);
    end
  else
    raise EBrowserNodeError.Create(ResStr_InvalidChildNodeTypeIndexRequest);
end;  // TFundingFolderNode.StoredProcByChildType 

{-==============================================================================
    TFundingLeafNode
===============================================================================}
{-------------------------------------------------------------------------------
  Class table name cannot be retrieved here since it is context sensitive. 
}
class function TFundingLeafNode.ClassTableName: String;
begin
  Result := '';
end;  // TFundingLeafNode.ClassTableName 

{-------------------------------------------------------------------------------
}
function TFundingLeafNode.GetAssociatedFrame: TBaseDetailFrameClass;
begin
  Result := TFraFunding;
end;  // TFundingLeafNode.GetAssociatedFrame 

{-------------------------------------------------------------------------------
}
function TFundingLeafNode.GetCanAdd: Boolean;
begin
  Result := AppSettings.AllowFinance and inherited GetCanAdd;
end;  // TFundingLeafNode.GetCanAdd 

{-------------------------------------------------------------------------------
}
function TFundingLeafNode.GetCanDelete: Boolean;
begin
  Result := AppSettings.AllowFinance and inherited GetCanDelete;
end;  // TFundingLeafNode.GetCanDelete 

{-------------------------------------------------------------------------------
}
function TFundingLeafNode.GetCanEdit: Boolean;
begin
  Result := AppSettings.AllowFinance and inherited GetCanEdit;
end;  // TFundingLeafNode.GetCanEdit 

{-------------------------------------------------------------------------------
}
function TFundingLeafNode.GetImageIndex: Integer;
begin
  Result := 19;
end;  // TFundingLeafNode.GetImageIndex 

{-------------------------------------------------------------------------------
}
function TFundingLeafNode.GetProperty(const AName: string): Variant;
begin
  Result := InternalGetProperty(AName);
end;  // TFundingLeafNode.GetProperty 

{-------------------------------------------------------------------------------
  Accessor method override, since ClassTableName does not work. 
}
function TFundingLeafNode.GetTableName: String;
begin
  if Assigned(Parent) then
    Result := TBrowserNode(Parent).TableName
  else
    Result := '';
end;  // TFundingLeafNode.GetTableName 

{-------------------------------------------------------------------------------
}
function TFundingLeafNode.InternalGetProperty(const AName: String): Variant;
begin
  Result := inherited InternalGetProperty(AName);
  
  if Result = Unassigned then begin
    if AName = PROP_NODE_CONTEXT then
      if Assigned(TopLevelNode) and (TopLevelNode is TTopLevelNode) then
        Result := TTopLevelNode(TopLevelNode).NodeContext
      else
        raise EBrowserNodeError.Create(ResStr_InvalidNodeContext);
  end;
end;  // TFundingLeafNode.InternalGetProperty 

{-------------------------------------------------------------------------------
}
procedure TFundingLeafNode.SetCaption(ARecordset: _Recordset);
begin
  Caption:= ARecordset.Fields['Item_Name'].Value;
end;  // TFundingLeafNode.SetCaption 

{-==============================================================================
    TJobLeafNode
===============================================================================}
{-------------------------------------------------------------------------------
}
class function TJobLeafNode.ClassTableName: String;
begin
  Result := TN_CONSERVATION_JOB;
end;  // TJobLeafNode.ClassTableName 

{-------------------------------------------------------------------------------
}
function TJobLeafNode.GetAssociatedFrame: TBaseDetailFrameClass;
begin
  Result := TfraJob;
end;  // TJobLeafNode.GetAssociatedFrame 

{-------------------------------------------------------------------------------
}
function TJobLeafNode.GetCanDelete: Boolean;
begin
  Result := (TopNodeContext <> ncSpecimen) and inherited GetCanDelete;
end;  // TJobLeafNode.GetCanDelete 

{-------------------------------------------------------------------------------
}
function TJobLeafNode.GetDomainStoredProc: String;
begin
  Result := 'usp_ConservationJob_DomainMask_Get';
end;  // TJobLeafNode.GetDomainStoredProc 

{-------------------------------------------------------------------------------
}
function TJobLeafNode.GetImageIndex: Integer;
begin
  Result := 26;
end;  // TJobLeafNode.GetImageIndex 

{-------------------------------------------------------------------------------
}
function TJobLeafNode.GetNodeContext: TNodeContext;
begin
  Result := ncJob;
end;  // TJobLeafNode.GetNodeContext 

{-------------------------------------------------------------------------------
}
procedure TJobLeafNode.SetCaption(ARecordset: _Recordset);
begin
  Caption:= VarToStr(ARecordset.Fields['Display_Caption'].Value);
end;  // TJobLeafNode.SetCaption

{-==============================================================================
    TJobFolderNode
===============================================================================}
{-------------------------------------------------------------------------------
}
class function TJobFolderNode.ClassTableName: String;
begin
  Result := TN_CONSERVATION_JOB;
end;  // TJobFolderNode.ClassTableName 

{-------------------------------------------------------------------------------
}
procedure TJobFolderNode.DeleteLink(const AJoinTableKey: String);
begin
  if ParentNodeContext = ncTask then
    dmGeneral.RunStoredProc('usp_Task_Delete_ForJob', ['@Key', AJoinTableKey])
  else
    raise EBrowserNodeError.Create(ResStr_ChildNodeContextInvalid);
end;  // TJobFolderNode.DeleteLink 

{-------------------------------------------------------------------------------
}
function TJobFolderNode.GetAddButtonMenuCaption(Index: Integer): String;
begin
  case Index of
    0:  Result := ResStr_AddNew;
    1:  Result := ResStr_LinkToExisting;
  else
    raise EAddMenuItemError.Create(ResStr_InvalidAddMenuIndexRequest);
  end;
end;  // TJobFolderNode.GetAddButtonMenuCaption 

{-------------------------------------------------------------------------------
}
function TJobFolderNode.GetAddButtonMenuCaptionsCount: Integer;
begin
  Result := 2;
end;  // TJobFolderNode.GetAddButtonMenuCaptionsCount 

{-------------------------------------------------------------------------------
}
function TJobFolderNode.GetAddMenuIsAdd(AMenuIndex: Integer): Boolean;
begin
  if AMenuIndex = 0 then
    Result := True
  else
    Result := False;
end;  // TJobFolderNode.GetAddMenuIsAdd 

{-------------------------------------------------------------------------------
}
function TJobFolderNode.GetChildNodeType(Index: Integer): TBrowserNodeClass;
begin
  case Index of
    0: Result := TJobLeafNode;
  else
    raise EBrowserNodeError.Create(ResStr_InvalidChildNodeTypeIndexRequest);
  end;
end;  // TJobFolderNode.GetChildNodeType 

{-------------------------------------------------------------------------------
}
function TJobFolderNode.GetChildNodeTypeCount: Integer;
begin
  Result:= 1;
end;  // TJobFolderNode.GetChildNodeTypeCount 

{-------------------------------------------------------------------------------
}
function TJobFolderNode.GetNodeContext: TNodeContext;
begin
  Result := ncJob;
end;  // TJobFolderNode.GetNodeContext 

{-------------------------------------------------------------------------------
}
function TJobFolderNode.GetSearchType: SearchManager.TSearchType;
begin
  Result := stJob;
end;  // TJobFolderNode.GetSearchType 

{-------------------------------------------------------------------------------
}
procedure TJobFolderNode.SetCaption;
begin
  Text := ResStr_Jobs;
end;  // TJobFolderNode.SetCaption 

{-------------------------------------------------------------------------------
}
function TJobFolderNode.StoredProcByChildType(ALeafNodeClass: TLeafNodeClass): String;
begin
  Result := '';
  
  if ALeafNodeClass = TJobLeafNode then
    Result := 'usp_Jobs_Select_ForTask'
  else
    raise EBrowserNodeError.Create(ResStr_InvalidChildNodeTypeIndexRequest);
end;  // TJobFolderNode.StoredProcByChildType 

{-------------------------------------------------------------------------------
}
function TJobFolderNode.UpdateNodeRelationship(const NewNodeKey: String): String;
begin
  case ParentNodeContext of
    ncTask:
      begin
        // JoinKey returned is same as Task key, as Task table is join table.
        Result := VarToStr(dmGeneral.GetStoredProcOutputParam(
                           'usp_Job_Update_ForTask',
                           ['@ParentKey', Key, '@ChildKey', NewNodeKey], '@JoinKey'));
        EnforceSingleChildNode(NewNodeKey);
      end;
  else
    raise EAddMenuItemError.Create(ResStr_InvalidNodeContext);
  end;
end;  // TJobFolderNode.UpdateNodeRelationship 

{-==============================================================================
    TJobTopLevelNode
===============================================================================}
{-------------------------------------------------------------------------------
}
procedure TJobTopLevelNode.AddChildNodesOfType(ABrowserNodeClass: TBrowserNodeClass);
var
  lNewNode: TFolderNode;
begin
  if (ABrowserNodeClass = TMultimediaFolderNode) or (ABrowserNodeClass = TObjectFolderNode) or
     (ABrowserNodeClass = TTaskIdentifiedFolderNode) or
     ((ABrowserNodeClass = TFundingFolderNode) and AppSettings.AllowFinance) then
  begin
    lNewNode:= TFolderNode(Tree.Items.AddTypedChild(Self, ABrowserNodeClass));
    lNewNode.Initialise(Key);
  end else
  if ((ABrowserNodeClass = TFundingFolderNode) and not AppSettings.AllowFinance) then
    Exit //Do not add
  else
    raise EBrowserNodeError.Create(ResStr_InvalidChildNodeTypeIndexRequest);
end;  // TJobTopLevelNode.AddChildNodesOfType 

{-------------------------------------------------------------------------------
}
class function TJobTopLevelNode.ClassTableName: String;
begin
  Result := TN_CONSERVATION_JOB;
end;  // TJobTopLevelNode.ClassTableName 

{-------------------------------------------------------------------------------
}
function TJobTopLevelNode.GetAssociatedFrame: TBaseDetailFrameClass;
begin
  Result := TfraJob;
end;  // TJobTopLevelNode.GetAssociatedFrame 

{-------------------------------------------------------------------------------
}
function TJobTopLevelNode.GetChildNodeType(Index: Integer): TBrowserNodeClass;
begin
  case Index of
    0: Result := TObjectFolderNode;
    1: Result := TTaskIdentifiedFolderNode;
    2: Result := TFundingFolderNode;
    3: Result := TMultimediaFolderNode;
  else
    raise EBrowserNodeError.Create(ResStr_InvalidChildNodeTypeIndexRequest);
  end;
end;  // TJobTopLevelNode.GetChildNodeType 

{-------------------------------------------------------------------------------
}
function TJobTopLevelNode.GetChildNodeTypeCount: Integer;
begin
  Result := 4;
end;  // TJobTopLevelNode.GetChildNodeTypeCount 

{-------------------------------------------------------------------------------
}
function TJobTopLevelNode.GetDomainStoredProc: String;
begin
  Result := 'usp_ConservationJob_DomainMask_Get';
end;  // TJobTopLevelNode.GetDomainStoredProc 

{-------------------------------------------------------------------------------
}
function TJobTopLevelNode.GetImageIndex: Integer;
begin
  Result := 26;
end;  // TJobTopLevelNode.GetImageIndex 

{-------------------------------------------------------------------------------
}
function TJobTopLevelNode.GetNodeContext: TNodeContext;
begin
  Result := ncJob;
end;  // TJobTopLevelNode.GetNodeContext 

{-------------------------------------------------------------------------------
}
function TJobTopLevelNode.GetProperty(const AName: string): Variant;
begin
  Result := InternalGetProperty(AName);
end;  // TJobTopLevelNode.GetProperty 

{-------------------------------------------------------------------------------
}
function TJobTopLevelNode.InternalGetProperty(const AName: string): Variant;
begin
  Result := inherited InternalGetProperty(AName);
  
  if Result = Unassigned then begin
    if AName = PROP_NODE_CONTEXT then
      Result := ncJob;
  end;
end;  // TJobTopLevelNode.InternalGetProperty 

{-------------------------------------------------------------------------------
}
procedure TJobTopLevelNode.SetCaption(ARecordset: _Recordset);
begin
  Caption:= VarToStr(ARecordset.Fields['Display_Caption'].Value);
end;  // TJobTopLevelNode.SetCaption 

{-------------------------------------------------------------------------------
}
function TJobTopLevelNode.StoredProcByChildType(ALeafNodeClass: TLeafNodeClass): String;
begin
  Result := '';
  raise EBrowserNodeError.Create(ResStr_InvalidChildNodeTypeIndexRequest);
end;  // TJobTopLevelNode.StoredProcByChildType 

{-==============================================================================
    TJobAppliedToFolderNode
===============================================================================}
{-------------------------------------------------------------------------------
}
class function TJobAppliedToFolderNode.ClassTableName: String;
begin
  Result := TN_CONSERVATION_JOB;
end;  // TJobAppliedToFolderNode.ClassTableName 

{-------------------------------------------------------------------------------
}
function TJobAppliedToFolderNode.GetCanAdd: Boolean;
begin
  Result := False;
end;  // TJobAppliedToFolderNode.GetCanAdd 

{-------------------------------------------------------------------------------
}
function TJobAppliedToFolderNode.GetChildNodeType(Index: Integer): TBrowserNodeClass;
begin
  case Index of
    0: Result := TJobLeafNode;
  else
    raise EBrowserNodeError.Create(ResStr_InvalidChildNodeTypeIndexRequest);
  end;
end;  // TJobAppliedToFolderNode.GetChildNodeType 

{-------------------------------------------------------------------------------
}
function TJobAppliedToFolderNode.GetChildNodeTypeCount: Integer;
begin
  Result:= 1;
end;  // TJobAppliedToFolderNode.GetChildNodeTypeCount 

{-------------------------------------------------------------------------------
}
function TJobAppliedToFolderNode.GetNodeContext: TNodeContext;
begin
  Result := ncJob;
end;  // TJobAppliedToFolderNode.GetNodeContext 

{-------------------------------------------------------------------------------
}
procedure TJobAppliedToFolderNode.SetCaption;
begin
  Text := ResStr_JobAppliedTo;
end;  // TJobAppliedToFolderNode.SetCaption 

{-------------------------------------------------------------------------------
}
function TJobAppliedToFolderNode.StoredProcByChildType(ALeafNodeClass: TLeafNodeClass): String;
begin
  Result := '';
  
  if ALeafNodeClass = TJobLeafNode then
    Result := 'usp_JobsAppliedTo_Select_ForSpecimen'
  else
    raise EBrowserNodeError.Create(ResStr_InvalidChildNodeTypeIndexRequest);
end;  // TJobAppliedToFolderNode.StoredProcByChildType 

{-==============================================================================
    TTaskIdentifiedFolderNode
===============================================================================}
{-------------------------------------------------------------------------------
}
class function TTaskIdentifiedFolderNode.ClassTableName: String;
begin
  Result := TN_CONSERVATION_TASK;
end;  // TTaskIdentifiedFolderNode.ClassTableName 

{-------------------------------------------------------------------------------
}
procedure TTaskIdentifiedFolderNode.DeleteLink(const AJoinTableKey: String);
begin
  if ParentNodeContext = ncJob then
    dmGeneral.RunStoredProc('usp_Task_Delete_ForJob', ['@Key', AJoinTableKey])
  else
    raise EBrowserNodeError.Create(ResStr_ChildNodeContextInvalid)
end;  // TTaskIdentifiedFolderNode.DeleteLink 

{-------------------------------------------------------------------------------
}
function TTaskIdentifiedFolderNode.GetAddButtonMenuCaption(Index: Integer): String;
begin
  case ParentNodeContext of
    ncConditionCheck:
      case Index of
        0:  Result := ResStr_AddNew;
        1:  Result := ResStr_LinkToExisting;
      else
        raise EAddMenuItemError.Create(ResStr_InvalidAddMenuIndexRequest);
      end;
    ncJob:
      if Index = 0 then
        Result := ResStr_Link
      else
        raise EAddMenuItemError.Create(ResStr_InvalidAddMenuIndexRequest);
  end;
end;  // TTaskIdentifiedFolderNode.GetAddButtonMenuCaption 

{-------------------------------------------------------------------------------
}
function TTaskIdentifiedFolderNode.GetAddButtonMenuCaptionsCount: Integer;
begin
  if ParentNodeContext = ncConditionCheck then
    Result := 2
  else
    Result := 1;
end;  // TTaskIdentifiedFolderNode.GetAddButtonMenuCaptionsCount 

{-------------------------------------------------------------------------------
}
function TTaskIdentifiedFolderNode.GetAddMenuIsAdd(AMenuIndex: Integer): Boolean;
begin
  case AMenuIndex of
    0 : Result := True
  else
    Result := False;
  end;
end;  // TTaskIdentifiedFolderNode.GetAddMenuIsAdd 

{-------------------------------------------------------------------------------
}
function TTaskIdentifiedFolderNode.GetCanAdd: Boolean;
begin
  if (AppSettings.AddDomainMask > 0) and (ParentNodeContext = ncConditionCheck) then
    Result := inherited GetCanAdd
  else
    Result := False;
end;  // TTaskIdentifiedFolderNode.GetCanAdd 

{-------------------------------------------------------------------------------
}
function TTaskIdentifiedFolderNode.GetChildNodeType(Index: Integer): TBrowserNodeClass;
begin
  case Index of
    0: Result := TTaskIdentifiedLeafNode;
  else
    raise EBrowserNodeError.Create(ResStr_InvalidChildNodeTypeIndexRequest);
  end;
end;  // TTaskIdentifiedFolderNode.GetChildNodeType 

{-------------------------------------------------------------------------------
}
function TTaskIdentifiedFolderNode.GetChildNodeTypeCount: Integer;
begin
  Result:= 1;
end;  // TTaskIdentifiedFolderNode.GetChildNodeTypeCount 

{-------------------------------------------------------------------------------
}
function TTaskIdentifiedFolderNode.GetNodeContext: TNodeContext;
begin
  Result := ncTask;
end;  // TTaskIdentifiedFolderNode.GetNodeContext 

{-------------------------------------------------------------------------------
}
function TTaskIdentifiedFolderNode.GetSearchType: SearchManager.TSearchType;
begin
  Result := stTasksIdentified;
end;  // TTaskIdentifiedFolderNode.GetSearchType 

{-------------------------------------------------------------------------------
}
procedure TTaskIdentifiedFolderNode.SetCaption;
begin
  Text := ResStr_TasksIdentified;
end;  // TTaskIdentifiedFolderNode.SetCaption 

{-------------------------------------------------------------------------------
}
function TTaskIdentifiedFolderNode.StoredProcByChildType(ALeafNodeClass: TLeafNodeClass): 
    String;
begin
  Result := '';
  
  if ALeafNodeClass = TTaskIdentifiedLeafNode then
    case ParentNodeContext of
      ncConditionCheck : Result := 'usp_Tasks_Select_ForConditionCheck';
      ncJob            : Result := 'usp_Tasks_Select_ForJob';
    else
      raise EBrowserNodeError.Create(ResStr_ChildNodeContextInvalid);
    end
  else
    raise EBrowserNodeError.Create(ResStr_InvalidChildNodeTypeIndexRequest);
end;  // TTaskIdentifiedFolderNode.StoredProcByChildType 

{-------------------------------------------------------------------------------
}
function TTaskIdentifiedFolderNode.UpdateNodeRelationship(const NewNodeKey: String): String;
begin
  case ParentNodeContext of
    ncConditionCheck: Result := VarToStr(dmGeneral.GetStoredProcOutputParam(
        'usp_Task_Update_ForConditionCheck',
                     ['@ParentKey', Key, '@ChildKey', NewNodeKey], '@JoinKey'));
  
    // JoinKey returned is same as Task key, as Task table is join table.
    ncJob: Result := VarToStr(dmGeneral.GetStoredProcOutputParam(
        'usp_Task_Update_ForJob',
                     ['@ParentKey', Key, '@ChildKey', NewNodeKey], '@JoinKey'));
  else
    raise EAddMenuItemError.Create(ResStr_InvalidNodeContext);
  end;
end;  // TTaskIdentifiedFolderNode.UpdateNodeRelationship 

{-==============================================================================
    TTaskIdentifiedLeafNode
===============================================================================}
{-------------------------------------------------------------------------------
}
class function TTaskIdentifiedLeafNode.ClassTableName: String;
begin
  Result := TN_CONSERVATION_TASK;
end;  // TTaskIdentifiedLeafNode.ClassTableName 

{-------------------------------------------------------------------------------
}
function TTaskIdentifiedLeafNode.GetAssociatedFrame: TBaseDetailFrameClass;
begin
  Result := TfraTask;
end;  // TTaskIdentifiedLeafNode.GetAssociatedFrame 

{-------------------------------------------------------------------------------
}
function TTaskIdentifiedLeafNode.GetDomainStoredProc: String;
begin
  Result := 'usp_Task_DomainMask_Get';
end;  // TTaskIdentifiedLeafNode.GetDomainStoredProc 

{-------------------------------------------------------------------------------
}
function TTaskIdentifiedLeafNode.GetImageIndex: Integer;
begin
  Result := 27;
end;  // TTaskIdentifiedLeafNode.GetImageIndex 

{-------------------------------------------------------------------------------
}
function TTaskIdentifiedLeafNode.GetNodeContext: TNodeContext;
begin
  Result := ncTask;
end;  // TTaskIdentifiedLeafNode.GetNodeContext 

{-------------------------------------------------------------------------------
}
procedure TTaskIdentifiedLeafNode.SetCaption(ARecordset: _Recordset);
begin
  Caption:= VarToStr(ARecordset.Fields['Display_Caption'].Value);
end;  // TTaskIdentifiedLeafNode.SetCaption 

{-==============================================================================
    TTaskIdentifiedTopLevelNode
===============================================================================}
{-------------------------------------------------------------------------------
}
procedure TTaskIdentifiedTopLevelNode.AddChildNodesOfType(ABrowserNodeClass: 
    TBrowserNodeClass);
var
  lNewNode: TFolderNode;
begin
  if (ABrowserNodeClass = TConditionCheckFolderNode) or (ABrowserNodeClass = TJobFolderNode) or
     (ABrowserNodeClass = TMultimediaFolderNode) then
  begin
    lNewNode:= TFolderNode(Tree.Items.AddTypedChild(Self, ABrowserNodeClass));
    lNewNode.Initialise(Key);
  end else
    raise EBrowserNodeError.Create(ResStr_InvalidChildNodeTypeIndexRequest);
end;  // TTaskIdentifiedTopLevelNode.AddChildNodesOfType 

{-------------------------------------------------------------------------------
}
class function TTaskIdentifiedTopLevelNode.ClassTableName: String;
begin
  Result := TN_CONSERVATION_TASK;
end;  // TTaskIdentifiedTopLevelNode.ClassTableName 

{-------------------------------------------------------------------------------
}
function TTaskIdentifiedTopLevelNode.GetAssociatedFrame: TBaseDetailFrameClass;
begin
  Result := TfraTask;
end;  // TTaskIdentifiedTopLevelNode.GetAssociatedFrame 

{-------------------------------------------------------------------------------
}
function TTaskIdentifiedTopLevelNode.GetCanAdd: Boolean;
begin
  Result := False;
end;  // TTaskIdentifiedTopLevelNode.GetCanAdd 

{-------------------------------------------------------------------------------
}
function TTaskIdentifiedTopLevelNode.GetChildNodeType(Index: Integer): TBrowserNodeClass;
begin
  case Index of
    0: Result := TConditionCheckFolderNode;
    1: Result := TJobFolderNode;
    2: Result := TMultimediaFolderNode;
  else
    raise EBrowserNodeError.Create(ResStr_InvalidChildNodeTypeIndexRequest);
  end;
end;  // TTaskIdentifiedTopLevelNode.GetChildNodeType 

{-------------------------------------------------------------------------------
}
function TTaskIdentifiedTopLevelNode.GetChildNodeTypeCount: Integer;
begin
  Result := 3;
end;  // TTaskIdentifiedTopLevelNode.GetChildNodeTypeCount 

{-------------------------------------------------------------------------------
}
function TTaskIdentifiedTopLevelNode.GetDomainStoredProc: String;
begin
  Result := 'usp_Task_DomainMask_Get';
end;  // TTaskIdentifiedTopLevelNode.GetDomainStoredProc 

{-------------------------------------------------------------------------------
}
function TTaskIdentifiedTopLevelNode.GetImageIndex: Integer;
begin
  Result := 27;
end;  // TTaskIdentifiedTopLevelNode.GetImageIndex 

{-------------------------------------------------------------------------------
}
function TTaskIdentifiedTopLevelNode.GetNodeContext: TNodeContext;
begin
  Result := ncTask;
end;  // TTaskIdentifiedTopLevelNode.GetNodeContext 

{-------------------------------------------------------------------------------
}
function TTaskIdentifiedTopLevelNode.GetProperty(const AName: string): Variant;
begin
  Result := InternalGetProperty(AName);
end;  // TTaskIdentifiedTopLevelNode.GetProperty 

{-------------------------------------------------------------------------------
}
function TTaskIdentifiedTopLevelNode.InternalGetProperty(const AName: string): Variant;
begin
  Result := inherited InternalGetProperty(AName);
  
  if Result = Unassigned then begin
    if AName = PROP_NODE_CONTEXT then
      Result := ncTask;
  end;
end;  // TTaskIdentifiedTopLevelNode.InternalGetProperty 

{-------------------------------------------------------------------------------
}
procedure TTaskIdentifiedTopLevelNode.SetCaption(ARecordset: _Recordset);
begin
  Caption:= VarToStr(ARecordset.Fields['Display_Caption'].Value);
end;  // TTaskIdentifiedTopLevelNode.SetCaption

{-------------------------------------------------------------------------------
}
function TTaskIdentifiedTopLevelNode.StoredProcByChildType(ALeafNodeClass: TLeafNodeClass): 
    String;
begin
  Result := '';
  raise EBrowserNodeError.Create(ResStr_InvalidChildNodeTypeIndexRequest);
end;  // TTaskIdentifiedTopLevelNode.StoredProcByChildType 

end.
