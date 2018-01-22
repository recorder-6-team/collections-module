{===============================================================================
  Unit:        BrowserNodeCommon.pas

  Defines:     Many Classes

  Description: Contains node classes that are common all over the application

  Model:       BrowserNodes.mpb

  Created:     August 2003

  Last revision information:
    $Revision: 74 $
    $Date: 30/06/09 14:30 $
    $Author: Ericsalmon $

===============================================================================}
unit BrowserNodeCommon;

interface

uses
  SysUtils, Windows, Messages, Classes, Graphics, Controls, Forms, Dialogs, TreeColl,
  BrowserNodeFramework, ResourceStrings, BrowserNodeCollectionUnits, BaseDetailFrameUnit,
  ADODB, DataTypes, SearchManager, Variants, RapTree, LuxembourgConstants, DataClasses;

type
  TEnquiryFolderNode = class (TFolderNode, IAddMenuOptions)
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
  
  TEnquiryLeafNode = class (THyperlinkLeafNode)
  protected
    function GetAssociatedFrame: TBaseDetailFrameClass; override;
    function GetDomainStoredProc: String; override;
    function GetImageIndex: Integer; override;
    function GetNodeContext: TNodeContext; override;
    procedure SetCaption(ARecordset: _Recordset); override;
  public
    class function ClassTableName: String; override;
  end;
  
  TEnquiryTopLevelNode = class (TTopLevelNode, IAdditionalProperties)
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
  
  TTermFolderNode = class (TFolderNode, IAddMenuOptions)
  protected
    function GetAddButtonMenuCaption(Index: Integer): String; override;
    function GetAddButtonMenuCaptionsCount: Integer; override;
    function GetAddMenuIsAdd(AMenuIndex: Integer): Boolean; override;
    function GetChildNodeType(Index: Integer): TBrowserNodeClass; override;
    function GetChildNodeTypeCount: Integer; override;
    function GetNodeContext: TNodeContext; override;
    function GetSearchType: TSearchType; override;
    procedure SetCaption; override;
    function StoredProcByChildType(ALeafNodeClass: TLeafNodeClass): String; override;
  public
    class function ClassTableName: String; override;
    procedure DeleteLink(const AJoinTableKey: String); override;
    function UpdateNodeRelationship(const NewNodeKey: String): String; override;
  end;
  
  TTermLeafNode = class (THyperlinkLeafNode)
  protected
    function GetAssociatedFrame: TBaseDetailFrameClass; override;
    function GetCanDelete: Boolean; override;
    function GetCanEdit: Boolean; override;
    function GetImageIndex: Integer; override;
    function GetNodeContext: TNodeContext; override;
  public
    class function ClassTableName: String; override;
  end;
  
  THistoryFolderNode = class (TFolderNode, IAddMenuOptions)
  protected
    function GetAddButtonMenuCaption(Index: Integer): String; override;
    function GetAddButtonMenuCaptionsCount: Integer; override;
    function GetAddMenuIsAdd(AMenuIndex: Integer): Boolean; override;
    function GetChildNodeType(Index: Integer): TBrowserNodeClass; override;
    function GetChildNodeTypeCount: Integer; override;
    procedure SetCaption; override;
    function StoredProcByChildType(ALeafNodeClass: TLeafNodeClass): String; override;
  public
    class function ClassTableName: String; override;
  end;
  
  THistoryLeafNode = class (TLeafNode)
  protected
    function GetAssociatedFrame: TBaseDetailFrameClass; override;
    function GetImageIndex: Integer; override;
    procedure SetCaption(ARecordset: _Recordset); override;
  public
    class function ClassTableName: String; override;
  end;
  
  TMeasurementFolderNode = class (TFolderNode, IAddMenuOptions)
  protected
    function GetAddButtonMenuCaption(Index: Integer): String; override;
    function GetAddButtonMenuCaptionsCount: Integer; override;
    function GetAddMenuIsAdd(AMenuIndex: Integer): Boolean; override;
    function GetChildNodeType(Index: Integer): TBrowserNodeClass; override;
    function GetChildNodeTypeCount: Integer; override;
    procedure SetCaption; override;
    function StoredProcByChildType(ALeafNodeClass: TLeafNodeClass): String; override;
  public
    class function ClassTableName: String; override;
  end;
  
  TMeasurementLeafNode = class (TLeafNode)
  protected
    function GetAssociatedFrame: TBaseDetailFrameClass; override;
    function GetCanDelete: Boolean; override;
    function GetCanEdit: Boolean; override;
    function GetImageIndex: Integer; override;
    procedure SetCaption(ARecordset: _Recordset); override;
  public
    class function ClassTableName: String; override;
  end;

  TMultimediaFolderNode = class (TFolderNode, IAddMenuOptions)
  private
    function AddFile(const AFileName: String): String;
    procedure MakeThumbnail(const AFileName: String);
  protected
    function GetAddButtonMenuCaption(Index: Integer): String; override;
    function GetAddButtonMenuCaptionsCount: Integer; override;
    function GetAddMenuIsAdd(AMenuIndex: Integer): Boolean; override;
    function GetChildNodeType(Index: Integer): TBrowserNodeClass; override;
    function GetChildNodeTypeCount: Integer; override;
    function GetStoredProcParams: TVariantArray; override;
    procedure SetCaption; override;
    function StoredProcByChildType(ALeafNodeClass: TLeafNodeClass): String; override;
  public
    procedure AddFiles(AFiles: TStringList); override;
    class function ClassTableName: String; override;
    class function IsFileAcceptor(AFiles: TStringList): Boolean; override;
  end;

  TMultimediaLeafNode = class (TLeafNode, IAdditionalProperties)
  private
    function GetProperty(const AName: String): Variant;
  protected
    function GetAssociatedFrame: TBaseDetailFrameClass; override;
    function GetCanDelete: Boolean; override;
    function GetCanEdit: Boolean; override;
    function GetImageIndex: Integer; override;
    function InternalGetProperty(const AName: String): Variant; override;
    procedure SetCaption(ARecordset: _Recordset); override;
  public
    class function ClassTableName: String; override;
  end;
  
  TPeopleOrganisationFolderNode = class (TFolderNode)
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
  
  TPeopleOrganisationOtherFolderNode = class (TFolderNode, IAddMenuOptions)
  protected
    function GetAddButtonMenuCaption(Index: Integer): String; override;
    function GetAddButtonMenuCaptionsCount: Integer; override;
    function GetAddMenuIsAdd(AMenuIndex: Integer): Boolean; override;
    function GetChildNodeType(Index: Integer): TBrowserNodeClass; override;
    function GetChildNodeTypeCount: Integer; override;
    procedure SetCaption; override;
    function StoredProcByChildType(ALeafNodeClass: TLeafNodeClass): String; override;
  public
    class function ClassTableName: String; override;
  end;
  
  TPeopleOrganisationLeafNode = class (THyperlinkLeafNode, IAdditionalProperties)
  private
    function GetProperty(const AName: string): Variant;
  protected
    function GetAssociatedFrame: TBaseDetailFrameClass; override;
    function GetCanAdd: Boolean; override;
    function GetCanDelete: Boolean; override;
    function GetCanEdit: Boolean; override;
    function GetImageIndex: Integer; override;
    function GetNodeContext: TNodeContext; override;
    function InternalGetProperty(const AName: string): Variant; override;
  public
    class function ClassTableName: String; override;
  end;
  
  TRelatedNameLeafNode = class (THyperlinkLeafNode, IAdditionalProperties)
  private
    function GetProperty(const AName: string): Variant;
  protected
    function GetAssociatedFrame: TBaseDetailFrameClass; override;
    function GetCanEdit: Boolean; override;
    function GetImageIndex: Integer; override;
    function GetNodeContext: TNodeContext; override;
    procedure InitialiseDragDropKey(ARecordset: _Recordset); override;
    procedure InitialiseHyperlinkKey(ARecordset: _Recordset); override;
    function InternalGetProperty(const AName: string): Variant; override;
    procedure SetCaption(ARecordset: _Recordset); override;
  public
    class function ClassTableName: String; override;
    procedure FindAndSetDragDropKey; override;
  end;
  
  TNumberingHistoryFolderNode = class (TFolderNode, IAddMenuOptions)
  private
    function AddNode(ATree: TRapidTree; AMenuIndex: integer): TFlyNode;
  protected
    procedure AddChildNodesOfType(ABrowserNodeClass: TBrowserNodeClass); override;
    function GetAddButtonMenuCaption(Index: Integer): String; override;
    function GetAddButtonMenuCaptionsCount: Integer; override;
    function GetAddMenuIsAdd(AMenuIndex: Integer): Boolean; override;
    function GetChildNodeType(Index: Integer): TBrowserNodeClass; override;
    function GetChildNodeTypeCount: Integer; override;
    procedure PopulateFromDatabase(ALeafNodeClass: TLeafNodeClass; const AStoredProcName: 
        String); override;
    procedure SetCaption; override;
    function StoredProcByChildType(ALeafNodeClass: TLeafNodeClass): String; override;
  public
    class function ClassTableName: String; override;
  end;
  
  TAccessionNumberLeafNode = class (THyperlinkLeafNode)
  protected
    function GetAssociatedFrame: TBaseDetailFrameClass; override;
    function GetCanAdd: Boolean; override;
    function GetCanDelete: Boolean; override;
    function GetCanEdit: Boolean; override;
    function GetImageIndex: Integer; override;
    function GetNodeContext: TNodeContext; override;
    procedure SetCaption(ARecordset: _Recordset); override;
  public
    class function ClassTableName: String; override;
  end;
  
  TRegistrationNumberLeafNode = class (TLeafNode)
  protected
    function GetAssociatedFrame: TBaseDetailFrameClass; override;
    function GetCanDelete: Boolean; override;
    function GetCanEdit: Boolean; override;
    function GetImageIndex: Integer; override;
    procedure SetCaption(ARecordset: _Recordset); override;
  public
    class function ClassTableName: String; override;
  end;
  
  TProcessFolderNode = class (TFolderNode, IAddMenuOptions)
  protected
    function GetAddButtonMenuCaption(Index: Integer): String; override;
    function GetAddButtonMenuCaptionsCount: Integer; override;
    function GetAddMenuIsAdd(AMenuIndex: Integer): Boolean; override;
    function GetChildNodeType(Index: Integer): TBrowserNodeClass; override;
    function GetChildNodeTypeCount: Integer; override;
    procedure SetCaption; override;
    function StoredProcByChildType(ALeafNodeClass: TLeafNodeClass): String; override;
  public
    class function ClassTableName: String; override;
  end;
  
  TProcessLeafNode = class (TLeafNode, IAdditionalProperties)
  private
    function GetProperty(const AName: string): Variant;
  protected
    function GetAssociatedFrame: TBaseDetailFrameClass; override;
    function GetCanDelete: Boolean; override;
    function GetCanEdit: Boolean; override;
    function GetImageIndex: Integer; override;
    function InternalGetProperty(const AName: string): Variant; override;
  public
    class function ClassTableName: String; override;
  end;
  
  TValuationFolderNode = class (TFolderNode, IAddMenuOptions)
  protected
    procedure AddChildNodesOfType(ABrowserNodeClass: TBrowserNodeClass); override;
    function GetAddButtonMenuCaption(Index: Integer): String; override;
    function GetAddButtonMenuCaptionsCount: Integer; override;
    function GetAddMenuIsAdd(AMenuIndex: Integer): Boolean; override;
    function GetChildNodeType(Index: Integer): TBrowserNodeClass; override;
    function GetChildNodeTypeCount: Integer; override;
    function GetNodeContext: TNodeContext; override;
    function GetStoredProcParams: TVariantArray; override;
    procedure SetCaption; override;
    function StoredProcByChildType(ALeafNodeClass: TLeafNodeClass): String; override;
  public
    class function ClassTableName: String; override;
    procedure DeleteLink(const AJoinTableKey: String); override;
    function UpdateNodeRelationship(const NewNodeKey: String): String; override;
  end;
  
  TValuationLeafNode = class (THyperlinkLeafNode)
  protected
    function GetAssociatedFrame: TBaseDetailFrameClass; override;
    function GetCanAdd: Boolean; override;
    function GetCanDelete: Boolean; override;
    function GetCanEdit: Boolean; override;
    function GetDomainStoredProc: String; override;
    function GetImageIndex: Integer; override;
    function GetNodeContext: TNodeContext; override;
    procedure SetCaption(ARecordset: _Recordset); override;
  public
    class function ClassTableName: String; override;
  end;

  TValuationTopLevelNode = class (TTopLevelNode, IAdditionalProperties)
  private
    function GetProperty(const AName: string): Variant;
  protected
    procedure AddChildNodesOfType(ABrowserNodeClass: TBrowserNodeClass); override;
    function GetAssociatedFrame: TBaseDetailFrameClass; override;
    function GetCanAdd: Boolean; override;
    function GetCanDelete: Boolean; override;
    function GetCanEdit: Boolean; override;
    function GetChildNodeType(Index: Integer): TBrowserNodeClass; override;
    function GetChildNodeTypeCount: Integer; override;
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
  BrowserNodeConditionCheck, BrowserNodeMovement, GeneralData, ApplicationSettings,
  FrameNumberHistory, FrameNumberHistoryReadOnly, BrowserViewTypes, FrameEnquiry,
  FrameValuation, FramePeople, FrameProcess, FrameMeasurements, FrameMultimedia,
  FrameHistory, FramePeopleGeneral, ConceptHTMLDetail, LuxembourgFunctions,
  APIUtils, GeneralFunctions, ComObj, ActiveX, ImageMagickObject_TLB, UserMessages,
  EasyShell, ShellAPI;

resourcestring
  ResStr_NoFilesAdded = 'None of the files were of a format that could be added.';
  ResStr_SomeFilesAdded = 'Some of the files could not be added because they were of the wrong format.';
  ResStr_ConfirmThumbnailOverwrite = 'The thumbnail for this file already exists.  Do you want to overwrite it?';
  ResStr_CannotDeleteThumbnail = 'The thumbnail for this file cannot be overwritten.';

{-==============================================================================
    TEnquiryFolderNode
===============================================================================}
{-------------------------------------------------------------------------------
}
class function TEnquiryFolderNode.ClassTableName: String;
begin
  Result := TN_ENQUIRY;
end;  // TEnquiryFolderNode.ClassTableName 

{-------------------------------------------------------------------------------
}
procedure TEnquiryFolderNode.DeleteLink(const AJoinTableKey: String);
begin
  case ParentNodeContext of
    ncCollection,
    ncSpecimen,
    ncStore      : dmGeneral.RunStoredProc('usp_CollectionUnitEnquiry_Delete',
                                           ['@Key', AJoinTableKey]);
    ncAccession,
    ncLoan,
    ncMovement   : dmGeneral.RunStoredProc('usp_MovementEnquiry_Delete',
                                           ['@Key', AJoinTableKey]);
  else
    raise EBrowserNodeError.Create(ResStr_ChildNodeContextInvalid);
  end
end;  // TEnquiryFolderNode.DeleteLink

{-------------------------------------------------------------------------------
}
function TEnquiryFolderNode.GetAddButtonMenuCaption(Index: Integer): String;
begin
  case Index of
    0 : Result := ResStr_AddNew;
    1 : Result := ResStr_LinkToExisting;
  else
    raise EAddMenuItemError.Create(ResStr_InvalidAddMenuIndexRequest);
  end;
end;  // TEnquiryFolderNode.GetAddButtonMenuCaption 

{-------------------------------------------------------------------------------
}
function TEnquiryFolderNode.GetAddButtonMenuCaptionsCount: Integer;
begin
  Result := 2;
end;  // TEnquiryFolderNode.GetAddButtonMenuCaptionsCount 

{-------------------------------------------------------------------------------
}
function TEnquiryFolderNode.GetAddMenuIsAdd(AMenuIndex: Integer): Boolean;
begin
  case AMenuIndex of
    0 : Result := True
  else
    Result := False;
  end;
end;  // TEnquiryFolderNode.GetAddMenuIsAdd 

{-------------------------------------------------------------------------------
}
function TEnquiryFolderNode.GetChildNodeType(Index: Integer): TBrowserNodeClass;
begin
  case Index of
    0: Result := TEnquiryLeafNode;
  else
    raise EBrowserNodeError.Create(ResStr_InvalidChildNodeTypeIndexRequest);
  end;
end;  // TEnquiryFolderNode.GetChildNodeType 

{-------------------------------------------------------------------------------
}
function TEnquiryFolderNode.GetChildNodeTypeCount: Integer;
begin
  Result:= 1;
end;  // TEnquiryFolderNode.GetChildNodeTypeCount 

{-------------------------------------------------------------------------------
}
function TEnquiryFolderNode.GetNodeContext: TNodeContext;
begin
  Result := ncEnquiry;
end;  // TEnquiryFolderNode.GetNodeContext 

{-------------------------------------------------------------------------------
}
function TEnquiryFolderNode.GetSearchType: SearchManager.TSearchType;
begin
  Result := stEnquiry;
end;  // TEnquiryFolderNode.GetSearchType 

{-------------------------------------------------------------------------------
}
procedure TEnquiryFolderNode.SetCaption;
begin
  Text := ResStr_Enquiries;
end;  // TEnquiryFolderNode.SetCaption 

{-------------------------------------------------------------------------------
}
function TEnquiryFolderNode.StoredProcByChildType(ALeafNodeClass: TLeafNodeClass): String;
begin
  Result := '';
  
  if ALeafNodeClass = TEnquiryLeafNode then
  case ParentNodeContext of
      ncCollection,
      ncSpecimen,
      ncStore          : Result := 'usp_Enquiries_Select_ForCollectionUnit';
      ncAccession,
      ncLoan,
      ncMovement       : Result := 'usp_Enquiries_Select_ForMovement';
    else
      raise EBrowserNodeError.Create(ResStr_ChildNodeContextInvalid);
    end
  else
    raise EBrowserNodeError.Create(ResStr_InvalidChildNodeTypeIndexRequest);
end;  // TEnquiryFolderNode.StoredProcByChildType 

{-------------------------------------------------------------------------------
}
function TEnquiryFolderNode.UpdateNodeRelationship(const NewNodeKey: String): String;
begin
  case ParentNodeContext of
    ncCollection,
    ncSpecimen,
    ncStore      : Result := VarToStr(dmGeneral.GetStoredProcOutputParam(
        'usp_Enquiry_Update_ForCollectionUnit',
                     ['@ParentKey', Key, '@ChildKey', NewNodeKey], '@JoinKey'));
    ncAccession,
    ncLoan,
    ncMovement  : Result := VarToStr(dmGeneral.GetStoredProcOutputParam(
        'usp_Enquiry_Update_ForMovement',
                     ['@ParentKey', Key, '@ChildKey', NewNodeKey], '@JoinKey'));
  else
    raise EAddMenuItemError.Create(ResStr_InvalidNodeContext);
  end;
end;  // TEnquiryFolderNode.UpdateNodeRelationship 

{-==============================================================================
    TEnquiryLeafNode
===============================================================================}
{-------------------------------------------------------------------------------
}
class function TEnquiryLeafNode.ClassTableName: String;
begin
  Result := TN_ENQUIRY;
end;  // TEnquiryLeafNode.ClassTableName 

{-------------------------------------------------------------------------------
}
function TEnquiryLeafNode.GetAssociatedFrame: TBaseDetailFrameClass;
begin
  Result := TfraEnquiry;
end;  // TEnquiryLeafNode.GetAssociatedFrame 

{-------------------------------------------------------------------------------
}
function TEnquiryLeafNode.GetDomainStoredProc: String;
begin
  Result := 'usp_Enquiry_DomainMask_Get';
end;  // TEnquiryLeafNode.GetDomainStoredProc 

{-------------------------------------------------------------------------------
}
function TEnquiryLeafNode.GetImageIndex: Integer;
begin
  Result := 5;
end;  // TEnquiryLeafNode.GetImageIndex 

{-------------------------------------------------------------------------------
}
function TEnquiryLeafNode.GetNodeContext: TNodeContext;
begin
  Result := ncEnquiry;
end;  // TEnquiryLeafNode.GetNodeContext 

{-------------------------------------------------------------------------------
}
procedure TEnquiryLeafNode.SetCaption(ARecordset: _Recordset);
begin
  Caption:= VarToStr(ARecordset.Fields['Display_Caption'].Value);
end;  // TEnquiryLeafNode.SetCaption 

{-==============================================================================
    TEnquiryTopLevelNode
===============================================================================}
{-------------------------------------------------------------------------------
}
procedure TEnquiryTopLevelNode.AddChildNodesOfType(ABrowserNodeClass: TBrowserNodeClass);
var
  lNewNode: TFolderNode;
begin
  if (ABrowserNodeClass = TCollectionFolderNode) or
     (ABrowserNodeClass = TSpecimenFolderNode) or
     (ABrowserNodeClass = TStoreFolderNode) or (ABrowserNodeClass = TTermFolderNode) or
     (ABrowserNodeClass = TAccessionFolderNode) or (ABrowserNodeClass = TMovementFolderNode) or
     (ABrowserNodeClass = TLoanFolderNode) then
  begin
    lNewNode:= TFolderNode(Tree.Items.AddTypedChild(Self, ABrowserNodeClass));
    lNewNode.Initialise(Key);
  end else
    raise EBrowserNodeError.Create(ResStr_InvalidChildNodeTypeIndexRequest);
end;  // TEnquiryTopLevelNode.AddChildNodesOfType 

{-------------------------------------------------------------------------------
}
class function TEnquiryTopLevelNode.ClassTableName: String;
begin
  Result := TN_ENQUIRY;
end;  // TEnquiryTopLevelNode.ClassTableName 

{-------------------------------------------------------------------------------
}
function TEnquiryTopLevelNode.GetAssociatedFrame: TBaseDetailFrameClass;
begin
  Result := TfraEnquiry;
end;  // TEnquiryTopLevelNode.GetAssociatedFrame 

{-------------------------------------------------------------------------------
}
function TEnquiryTopLevelNode.GetChildNodeType(Index: Integer): TBrowserNodeClass;
begin
  case Index of
    0: Result := TCollectionFolderNode;
    1: Result := TSpecimenFolderNode;
    2: Result := TStoreFolderNode;
    3: Result := TAccessionFolderNode;
    4: Result := TLoanFolderNode;
    5: Result := TMovementFolderNode;
    6: Result := TTermFolderNode;
  else
    raise EBrowserNodeError.Create(ResStr_InvalidChildNodeTypeIndexRequest);
  end;
end;  // TEnquiryTopLevelNode.GetChildNodeType 

{-------------------------------------------------------------------------------
}
function TEnquiryTopLevelNode.GetChildNodeTypeCount: Integer;
begin
  Result := 7;
end;  // TEnquiryTopLevelNode.GetChildNodeTypeCount 

{-------------------------------------------------------------------------------
}
function TEnquiryTopLevelNode.GetDomainStoredProc: String;
begin
  Result := 'usp_Enquiry_DomainMask_Get';
end;  // TEnquiryTopLevelNode.GetDomainStoredProc 

{-------------------------------------------------------------------------------
}
function TEnquiryTopLevelNode.GetImageIndex: Integer;
begin
  Result := 5;
end;  // TEnquiryTopLevelNode.GetImageIndex 

{-------------------------------------------------------------------------------
}
function TEnquiryTopLevelNode.GetNodeContext: TNodeContext;
begin
  Result := ncEnquiry;
end;  // TEnquiryTopLevelNode.GetNodeContext 

{-------------------------------------------------------------------------------
}
function TEnquiryTopLevelNode.GetProperty(const AName: string): Variant;
begin
  Result := InternalGetProperty(AName);
end;  // TEnquiryTopLevelNode.GetProperty 

{-------------------------------------------------------------------------------
}
function TEnquiryTopLevelNode.InternalGetProperty(const AName: string): Variant;
begin
  Result := inherited InternalGetProperty(AName);
  
  if Result = Unassigned then begin
    if AName = PROP_NODE_CONTEXT then
      Result := ncEnquiry;
  end;
end;  // TEnquiryTopLevelNode.InternalGetProperty 

{-------------------------------------------------------------------------------
}
procedure TEnquiryTopLevelNode.SetCaption(ARecordset: _Recordset);
begin
  Caption:= VarToStr(ARecordset.Fields['Display_Caption'].Value);
end;  // TEnquiryTopLevelNode.SetCaption 

{-------------------------------------------------------------------------------
}
function TEnquiryTopLevelNode.StoredProcByChildType(ALeafNodeClass: TLeafNodeClass): String;
begin
  Result := '';
  raise EBrowserNodeError.Create(ResStr_InvalidChildNodeTypeIndexRequest);
end;  // TEnquiryTopLevelNode.StoredProcByChildType 

{-==============================================================================
    TTermFolderNode
===============================================================================}
{-------------------------------------------------------------------------------
}
class function TTermFolderNode.ClassTableName: String;
begin
  Result := TN_CONCEPT;
end;  // TTermFolderNode.ClassTableName 

{-------------------------------------------------------------------------------
}
procedure TTermFolderNode.DeleteLink(const AJoinTableKey: String);
begin
  dmGeneral.RunStoredProc('usp_EnquiryConcept_Delete', ['@Key', AJoinTableKey]);
end;  // TTermFolderNode.DeleteLink 

{-------------------------------------------------------------------------------
}
function TTermFolderNode.GetAddButtonMenuCaption(Index: Integer): String;
begin
  if Index = 0 then
    Result := ResStr_Link
  else
    raise EAddMenuItemError.Create(ResStr_InvalidAddMenuIndexRequest);
end;  // TTermFolderNode.GetAddButtonMenuCaption 

{-------------------------------------------------------------------------------
}
function TTermFolderNode.GetAddButtonMenuCaptionsCount: Integer;
begin
  Result := 1;
end;  // TTermFolderNode.GetAddButtonMenuCaptionsCount 

{-------------------------------------------------------------------------------
}
function TTermFolderNode.GetAddMenuIsAdd(AMenuIndex: Integer): Boolean;
begin
  case AMenuIndex of
    0 : Result := False
  else
    Result := True;
  end;
end;  // TTermFolderNode.GetAddMenuIsAdd 

{-------------------------------------------------------------------------------
}
function TTermFolderNode.GetChildNodeType(Index: Integer): TBrowserNodeClass;
begin
  case Index of
    0: Result := TTermLeafNode;
  else
    raise EBrowserNodeError.Create(ResStr_InvalidChildNodeTypeIndexRequest);
  end;
end;  // TTermFolderNode.GetChildNodeType 

{-------------------------------------------------------------------------------
}
function TTermFolderNode.GetChildNodeTypeCount: Integer;
begin
  Result:= 1;
end;  // TTermFolderNode.GetChildNodeTypeCount 

{-------------------------------------------------------------------------------
}
function TTermFolderNode.GetNodeContext: TNodeContext;
begin
  Result := ncRecorder;
end;  // TTermFolderNode.GetNodeContext 

{-------------------------------------------------------------------------------
}
function TTermFolderNode.GetSearchType: TSearchType;
begin
  Result := stConcept;
end;  // TTermFolderNode.GetSearchType 

{-------------------------------------------------------------------------------
}
procedure TTermFolderNode.SetCaption;
begin
  Text := ResStr_Terms;
end;  // TTermFolderNode.SetCaption 

{-------------------------------------------------------------------------------
}
function TTermFolderNode.StoredProcByChildType(ALeafNodeClass: TLeafNodeClass): String;
begin
  Result := '';
  
  if ALeafNodeClass = TTermLeafNode then
    Result := 'usp_Terms_Select_ForEnquiry'
  else
    raise EBrowserNodeError.Create(ResStr_InvalidChildNodeTypeIndexRequest);
end;  // TTermFolderNode.StoredProcByChildType 

{-------------------------------------------------------------------------------
}
function TTermFolderNode.UpdateNodeRelationship(const NewNodeKey: String): String;
begin
  if ParentNodeContext = ncEnquiry then
     Result := VarToStr(dmGeneral.GetStoredProcOutputParam(
          'usp_Term_Update_ForEnquiry',
                     ['@ParentKey', Key, '@ChildKey', NewNodeKey], '@JoinKey'))
  else
    raise EAddMenuItemError.Create(ResStr_InvalidNodeContext);
end;  // TTermFolderNode.UpdateNodeRelationship 

{-==============================================================================
    TTermLeafNode
===============================================================================}
{-------------------------------------------------------------------------------
}
class function TTermLeafNode.ClassTableName: String;
begin
  Result := TN_CONCEPT;
end;  // TTermLeafNode.ClassTableName 

{-------------------------------------------------------------------------------
}
function TTermLeafNode.GetAssociatedFrame: TBaseDetailFrameClass;
begin
  Result := TfraConceptHTMLDetail;
end;  // TTermLeafNode.GetAssociatedFrame 

{-------------------------------------------------------------------------------
}
function TTermLeafNode.GetCanDelete: Boolean;
var
  lTopLevelNode: TFlyNode;
begin
  lTopLevelNode := TopLevelNode;
  if Assigned(lTopLevelNode) then
    if lTopLevelNode is TTopLevelNode then
      Result := TTopLevelNode(lTopLevelNode).CanDelete
    else
      raise EBrowserNodeError.Create(ResStr_DomainMaskNotFoundOnTopLevelNode)
  else
    raise EBrowserNodeError.Create(ResStr_DomainMaskNotFoundOnTopLevelNode);
end;  // TTermLeafNode.GetCanDelete 

{-------------------------------------------------------------------------------
}
function TTermLeafNode.GetCanEdit: Boolean;
begin
  Result := False;
end;  // TTermLeafNode.GetCanEdit 

{-------------------------------------------------------------------------------
}
function TTermLeafNode.GetImageIndex: Integer;
begin
  Result := 6;
end;  // TTermLeafNode.GetImageIndex 

{-------------------------------------------------------------------------------
}
function TTermLeafNode.GetNodeContext: TNodeContext;
begin
  Result := ncRecorder;
end;  // TTermLeafNode.GetNodeContext 

{-==============================================================================
    THistoryFolderNode
===============================================================================}
{-------------------------------------------------------------------------------
}
class function THistoryFolderNode.ClassTableName: String;
begin
  Result := TN_COLLECTION_UNIT_HISTORY;
end;  // THistoryFolderNode.ClassTableName 

{-------------------------------------------------------------------------------
}
function THistoryFolderNode.GetAddButtonMenuCaption(Index: Integer): String;
begin
  case Index of
    0: Result := ResStr_Add;
  else
    raise EAddMenuItemError.Create(ResStr_InvalidAddMenuIndexRequest);
  end;
end;  // THistoryFolderNode.GetAddButtonMenuCaption 

{-------------------------------------------------------------------------------
}
function THistoryFolderNode.GetAddButtonMenuCaptionsCount: Integer;
begin
  Result := 1;
end;  // THistoryFolderNode.GetAddButtonMenuCaptionsCount 

{-------------------------------------------------------------------------------
}
function THistoryFolderNode.GetAddMenuIsAdd(AMenuIndex: Integer): Boolean;
begin
  Result := (AMenuIndex = 0);
end;  // THistoryFolderNode.GetAddMenuIsAdd 

{-------------------------------------------------------------------------------
}
function THistoryFolderNode.GetChildNodeType(Index: Integer): TBrowserNodeClass;
begin
  case Index of
    0: Result := THistoryLeafNode;
  else
    raise EBrowserNodeError.Create(ResStr_InvalidChildNodeTypeIndexRequest);
  end;
end;  // THistoryFolderNode.GetChildNodeType 

{-------------------------------------------------------------------------------
}
function THistoryFolderNode.GetChildNodeTypeCount: Integer;
begin
  Result:= 1;
end;  // THistoryFolderNode.GetChildNodeTypeCount 

{-------------------------------------------------------------------------------
}
procedure THistoryFolderNode.SetCaption;
begin
  Text := ResStr_History;
end;  // THistoryFolderNode.SetCaption 

{-------------------------------------------------------------------------------
}
function THistoryFolderNode.StoredProcByChildType(ALeafNodeClass: TLeafNodeClass): String;
begin
  Result := '';
  
  if ALeafNodeClass = THistoryLeafNode then
    case ParentNodeContext of
      ncCollection,
      ncSpecimen,
      ncStore          : Result := 'usp_History_Select_ForCollectionUnit';
    else
      raise EBrowserNodeError.Create(ResStr_ChildNodeContextInvalid);
    end
  else
    raise EBrowserNodeError.Create(ResStr_InvalidChildNodeTypeIndexRequest);
end;  // THistoryFolderNode.StoredProcByChildType 

{-==============================================================================
    THistoryLeafNode
===============================================================================}
{-------------------------------------------------------------------------------
}
class function THistoryLeafNode.ClassTableName: String;
begin
  Result := TN_COLLECTION_UNIT_HISTORY;
end;  // THistoryLeafNode.ClassTableName 

{-------------------------------------------------------------------------------
}
function THistoryLeafNode.GetAssociatedFrame: TBaseDetailFrameClass;
begin
  Result := TfraHistory;
end;  // THistoryLeafNode.GetAssociatedFrame 

{-------------------------------------------------------------------------------
}
function THistoryLeafNode.GetImageIndex: Integer;
begin
  Result := 28;
end;  // THistoryLeafNode.GetImageIndex 

{-------------------------------------------------------------------------------
}
procedure THistoryLeafNode.SetCaption(ARecordset: _Recordset);
begin
  Caption := ARecordset.Fields['Item_Name'].Value + ' - ' +
      dmGeneral.GetVagueDateStringFromRecordset(ARecordset, 'From');
end;  // THistoryLeafNode.SetCaption 

{-==============================================================================
    TMeasurementFolderNode
===============================================================================}
{-------------------------------------------------------------------------------
}
class function TMeasurementFolderNode.ClassTableName: String;
begin
  Result := TN_COLLECTION_UNIT_DATA;
end;  // TMeasurementFolderNode.ClassTableName 

{-------------------------------------------------------------------------------
}
function TMeasurementFolderNode.GetAddButtonMenuCaption(Index: Integer): String;
begin
  case Index of
    0: Result := ResStr_Add;
  else
    raise EAddMenuItemError.Create(ResStr_InvalidAddMenuIndexRequest);
  end;
end;  // TMeasurementFolderNode.GetAddButtonMenuCaption 

{-------------------------------------------------------------------------------
}
function TMeasurementFolderNode.GetAddButtonMenuCaptionsCount: Integer;
begin
  Result := 1;
end;  // TMeasurementFolderNode.GetAddButtonMenuCaptionsCount 

{-------------------------------------------------------------------------------
}
function TMeasurementFolderNode.GetAddMenuIsAdd(AMenuIndex: Integer): Boolean;
begin
  Result := (AMenuIndex = 0);
end;  // TMeasurementFolderNode.GetAddMenuIsAdd 

{-------------------------------------------------------------------------------
}
function TMeasurementFolderNode.GetChildNodeType(Index: Integer): TBrowserNodeClass;
begin
  case Index of
    0: Result := TMeasurementLeafNode;
  else
    raise EBrowserNodeError.Create(ResStr_InvalidChildNodeTypeIndexRequest);
  end;
end;  // TMeasurementFolderNode.GetChildNodeType 

{-------------------------------------------------------------------------------
}
function TMeasurementFolderNode.GetChildNodeTypeCount: Integer;
begin
  Result:= 1;
end;  // TMeasurementFolderNode.GetChildNodeTypeCount 

{-------------------------------------------------------------------------------
}
procedure TMeasurementFolderNode.SetCaption;
begin
  Text := ResStr_Measurements;
end;  // TMeasurementFolderNode.SetCaption 

{-------------------------------------------------------------------------------
}
function TMeasurementFolderNode.StoredProcByChildType(ALeafNodeClass: TLeafNodeClass): String;
begin
  Result := '';
  
  if ALeafNodeClass = TMeasurementLeafNode then
    Result := 'usp_Measurements_Select_ForCollectionUnit'
  else
    raise EBrowserNodeError.Create(ResStr_InvalidChildNodeTypeIndexRequest);
end;  // TMeasurementFolderNode.StoredProcByChildType 

{-==============================================================================
    TMeasurementLeafNode
===============================================================================}
{-------------------------------------------------------------------------------
}
class function TMeasurementLeafNode.ClassTableName: String;
begin
  Result := TN_COLLECTION_UNIT_DATA;
end;  // TMeasurementLeafNode.ClassTableName 

{-------------------------------------------------------------------------------
}
function TMeasurementLeafNode.GetAssociatedFrame: TBaseDetailFrameClass;
begin
  Result := TfraMeasurements;
end;  // TMeasurementLeafNode.GetAssociatedFrame 

{-------------------------------------------------------------------------------
}
function TMeasurementLeafNode.GetCanDelete: Boolean;
var
  lTopLevelNode: TFlyNode;
begin
  lTopLevelNode := TopLevelNode;
  if Assigned(lTopLevelNode) then
    if lTopLevelNode is TTopLevelNode then
      Result := TTopLevelNode(lTopLevelNode).CanDelete
    else
     raise EBrowserNodeError.Create(ResStr_DomainMaskNotFoundOnTopLevelNode)
  else
    raise EBrowserNodeError.Create(ResStr_DomainMaskNotFoundOnTopLevelNode);
end;  // TMeasurementLeafNode.GetCanDelete 

{-------------------------------------------------------------------------------
}
function TMeasurementLeafNode.GetCanEdit: Boolean;
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
end;  // TMeasurementLeafNode.GetCanEdit 

{-------------------------------------------------------------------------------
}
function TMeasurementLeafNode.GetImageIndex: Integer;
begin
  Result := 31;
end;  // TMeasurementLeafNode.GetImageIndex 

{-------------------------------------------------------------------------------
}
procedure TMeasurementLeafNode.SetCaption(ARecordset: _Recordset);
begin
  Caption := ARecordset.Fields['Item_Name'].Value + ' - ' +
             DateToStr(ARecordset.Fields['Date_Time_Start'].Value);
end;  // TMeasurementLeafNode.SetCaption

{-==============================================================================
    TMultimediaFolderNode
===============================================================================}

{-------------------------------------------------------------------------------
  Adds all the image files in AFiles to this node, copying them to the
  relevant folder, creating thumbnail images if possible and adding them
  to the database, finally setting focus to the first new node.
}
procedure TMultimediaFolderNode.AddFiles(AFiles: TStringList);
var
  i, lAcceptedFileCount: Integer;
  lFileName, lDestFileName, lFirstKeyAdded, lKeyAdded: String;
  lPrefSpecimenNumber: String;
  lCursor: TCursor;
begin
  lAcceptedFileCount := 0;
  lFirstKeyAdded := '';
  lCursor := HourglassCursor;
  dmGeneral.Recorder.RecorderMainForm.StartProgressBar;

  if TopNodeContext = ncSpecimen then
    lPrefSpecimenNumber := Trim(VarToStr(dmGeneral.GetStoredProcOutputParam(
        'usp_CollectionUnitNumber_Get', ['@Key', Self.TopLevelNode.Key], '@Number')));

  try
    for i := 0 to Pred(AFiles.Count) do begin
      if FormatSupportedByImageMagick(AFiles[i]) then
      begin
        lFileName := ExtractFileName(AFiles[i]);

        // If specimen, use pref number as basis for filename.
        if TopNodeContext = ncSpecimen then
          lFileName := lPrefSpecimenNumber + ExtractFileExt(AFiles[i]);

        lDestFileName := AppSettings.SpecimenImagePath + lFileName;
        if FileExists(lDestFileName) then
          lDestFileName := GetUniqueFileName(lDestFileName);

        gpcAPICheck(CopyFile(PAnsiChar(AFiles[i]), PAnsiChar(lDestFileName), True));
        dmGeneral.Recorder.RecorderMainForm.Progress := Trunc((i + 0.33) * 100 / AFiles.Count);
        MakeThumbnail(lDestFileName);
        dmGeneral.Recorder.RecorderMainForm.Progress := Trunc((i + 0.66) * 100 / AFiles.Count);
        lKeyAdded := AddFile(lDestFileName);
        if Length(lFirstKeyAdded) = 0 then
          lFirstKeyAdded := lKeyAdded;
        dmGeneral.Recorder.RecorderMainForm.Progress := (i + 1) * 100 div AFiles.Count;
        Inc(lAcceptedFileCount);
      end;
    end;
  finally
    DefaultCursor(lCursor);
    dmGeneral.Recorder.RecorderMainForm.StopProgressBar;
    dmGeneral.Recorder.RecorderMainForm.Progress := 0;
  end;

  if lAcceptedFileCount = 0 then
    ShowInformation(ResStr_NoFilesAdded)
  else
  if lAcceptedFileCount < AFiles.Count then
    ShowInformation(ResStr_SomeFilesAdded);

  if lAcceptedFileCount > 0 then begin
    Refresh;
    if not Expanded then
      Expand(False);
    for i := 0 to Pred(Count) do
      if (Item[i] is TBrowserNode) and (TBrowserNode(Item[i]).Key = lFirstKeyAdded) then begin
        Item[i].Focused := True;
        Break;
      end;
  end;    // if lAcceptedFileCount > 0
end;    // TMultimediaFolderNode.AddFiles

{-------------------------------------------------------------------------------
  Adds the specified file to the Source_File table for this node
}
function TMultimediaFolderNode.AddFile(const AFileName: String): String;
var
  lParams: Array of Variant;
  lTableName, lTitle: String;
  lParentNode: TBrowserNode;
begin
  lTitle := ExtractWithoutExt(AFileName);
  if Parent is TBrowserNode then begin
    lParentNode := TBrowserNode(Parent);

    case TopNodeContext of
      ncCollection:       lTableName := TN_COLLECTION;
      ncSpecimen:         lTableName := TN_SPECIMEN_UNIT;
      ncStore:            lTableName := TN_STORE;
      ncMovement,
      ncLoan,
      ncAccession:        lTableName := TN_MOVEMENT;
      ncJob:              lTableName := TN_CONSERVATION_JOB;
      ncConditionCheck:   lTableName := TN_CONSERVATION_CHECK;
      ncTask:             lTableName := TN_CONSERVATION_TASK;
      ncInscriptionLabel: lTableName := TN_SPECIMEN_LABEL;
    else
      raise EBrowserNodeContextError.Create(ResStr_InvalidNodeContext);
    end;

    lParams := VarArrayOf(['@Key', '',
                           '@RecordKey', lParentNode.Key,
                           '@Filename', AFileName,
                           '@Title', lTitle,
                           '@Preferred', False,
                           '@TableName', lTableName
                          ]);
    Result := VarToStr(dmGeneral.RunInsertStoredProc(
        TN_SOURCE_FILE, 'usp_Multimedia_Insert', lParams, '@Key'));
  end else begin
    raise EBrowserNodeError.Create('Internal Error: Invalid Parent Node (not TBrowserNode)');
  end;    // if Parent is TBrowserNode
end;    // TMultimediaFolderNode.AddFile

{-------------------------------------------------------------------------------
}
class function TMultimediaFolderNode.ClassTableName: String;
begin
  Result := TN_SOURCE_JOIN;
end;  // TMultimediaFolderNode.ClassTableName 

{-------------------------------------------------------------------------------
}
function TMultimediaFolderNode.GetAddButtonMenuCaption(Index: Integer): String;
begin
  case Index of
    0: Result := ResStr_Add;
  else
    raise EAddMenuItemError.Create(ResStr_InvalidAddMenuIndexRequest);
  end;
end;  // TMultimediaFolderNode.GetAddButtonMenuCaption 

{-------------------------------------------------------------------------------
}
function TMultimediaFolderNode.GetAddButtonMenuCaptionsCount: Integer;
begin
  Result := 1;
end;  // TMultimediaFolderNode.GetAddButtonMenuCaptionsCount 

{-------------------------------------------------------------------------------
}
function TMultimediaFolderNode.GetAddMenuIsAdd(AMenuIndex: Integer): Boolean;
begin
  case AMenuIndex of
    0 : Result := True
  else
    Result := False;
  end;
end;  // TMultimediaFolderNode.GetAddMenuIsAdd 

{-------------------------------------------------------------------------------
}
function TMultimediaFolderNode.GetChildNodeType(Index: Integer): TBrowserNodeClass;
begin
  case Index of
    0: Result := TMultimediaLeafNode;
  else
    raise EBrowserNodeError.Create(ResStr_InvalidChildNodeTypeIndexRequest);
  end;
end;  // TMultimediaFolderNode.GetChildNodeType 

{-------------------------------------------------------------------------------
}
function TMultimediaFolderNode.GetChildNodeTypeCount: Integer;
begin
  Result:= 1;
end;  // TMultimediaFolderNode.GetChildNodeTypeCount

{-------------------------------------------------------------------------------
}
function TMultimediaFolderNode.GetStoredProcParams: TVariantArray;
var
  lTableName: String;
begin
  case ParentNodeContext of
    ncCollection:       lTableName := TN_COLLECTION;
    ncSpecimen:         lTableName := TN_SPECIMEN_UNIT;
    ncStore:            lTableName := TN_STORE;
    ncAccession,
    ncLoan,
    ncMovement:         lTableName := TN_MOVEMENT;
    ncConditionCheck:   lTableName := TN_CONSERVATION_CHECK;
    ncJob:              lTableName := TN_CONSERVATION_JOB;
    ncTask:             lTableName := TN_CONSERVATION_TASK;
    ncInscriptionLabel: lTableName := TN_SPECIMEN_LABEL;
  end;
  
  Result:= VarArrayOf(['@ParentKey', Key, '@TableName', lTableName]);
end;  // TMultimediaFolderNode.GetStoredProcParams

{-------------------------------------------------------------------------------
  Returns true if this node can accept any of the files in this list
}
class function TMultimediaFolderNode.IsFileAcceptor(AFiles: TStringList): Boolean;
var
  i: integer;
begin
  Result := False;
  for i := 0 to Pred(AFiles.Count) do
    if FormatSupportedByImageMagick(AFiles[i]) then begin
      Result := True;
      Break;
    end;
end;

{-------------------------------------------------------------------------------
  Use ImageMagick to create a thumbnail image
}
procedure TMultimediaFolderNode.MakeThumbnail(const AFileName: String);
var
  lThumbnailFileName: String;
  lImageFileMagick: IMagickImage;
  lImageSetList:TImageSetArray;
  v: Variant;
  s: String;
  i: integer;

    {-------------------------------------------------------------------------------
      Checks whether there are multiple thumbnails with the given name,
      but with a suffix of -n, where n is an integer.  If so, it removes the
      suffix from the first file, making it a normal thumbnail, deletes any
      other thumbnail files with sequential sufficies and returns True.
      If there are no such thumbnail files it returns False.
    }
    function RemoveMultipleThumbnails: Boolean;
    var
      idx: Integer;
      lCurrentFile, lFileWithoutExtension, lExtension: String;
    begin
      Result := False;
      lFileWithoutExtension := ChangeFileExt(lThumbnailFileName, '');
      lExtension            := ExtractFileExt(lThumbnailFileName);
      lCurrentFile          := lFileWithoutExtension + '-0' + lExtension;

      if FileExists(lCurrentFile) then
      begin
        // Use the first thumbnail detected as the genuine thumbnail.
        if FileExists(lThumbnailFileName) then
          DeleteFile(PAnsiChar(lCurrentFile))
        else
          RenameFile(lCurrentFile, lThumbnailFileName);

        // Remove any subsequent thumbnail images with sequential suffixes.
        idx := 1;
        lCurrentFile := lFileWithoutExtension + '-' + IntToStr(idx) + lExtension;
        while FileExists(lCurrentFile) do
        begin
          DeleteFile(PAnsiChar(lCurrentFile));
          Inc(idx);
          lCurrentFile := lFileWithoutExtension + '-' + IntToStr(idx) + lExtension;
        end;
        Result := True;
      end;
    end;    // RemoveMultipleThumbnails

begin
  lImageSetList := GetImageMagickParamSet(AFileName);

  for i := 0 to Length(lImageSetList) - 1 do
  begin
    lThumbnailFileName :=
        ChangeFileExt(AFileName, '')
        + '_' + lImageSetList[i].Identifier
        + '.' + lImageSetList[i].Extension;

    if FileExists(lThumbnailFileName) then
    begin
      if ConfirmYesNo(ResStr_ConfirmThumbnailOverwrite) <> mrYes then Exit;
      if not DeleteFile(PAnsiChar(lThumbnailFileName)) then
      begin
        ShowInformation(ResStr_CannotDeleteThumbnail);
        Exit;
      end;    // if not DeleteFile(PAnsiChar(lThumbnailFileName))
    end;    // if FileExists(lThumbnailFileName)

    lImageFileMagick := CreateOleObject('ImageMagickObject.MagickImage.1') as IMagickImage;
    v := VarArrayCreate([0, 4], varVariant);
    v[0] := AFileName;                   // Source file to use.
    v[1] := '-resize';                   // Option/command to perform.
    v[2] := lImageSetList[i].Dimension;  // Parameters for the option/command.
    v[3] := lThumbnailFileName;          // Target file, with extension.
    try
      s := lImageFileMagick.Convert(PSafeArray(System.TVarData(v).VArray));
      // Need to check for creation of multiple files irrespective of errors.
      if not FileExists(lThumbnailFileName) then
        RemoveMultipleThumbnails;
    except
      on E: EOleException do
      begin
        // Some installations of ImageMagick seem to throw an EOleException
        // exception after creating the thumbnail successfully!
        // Only raise it if the file doesn't seem to have been created successfully.
        if not FileExists(lThumbnailFileName) then
          if not RemoveMultipleThumbnails then
            raise;
      end;
    end;
  end;
end;  // TMultimediaFolderNode.MakeThumbnail

{-------------------------------------------------------------------------------
}
procedure TMultimediaFolderNode.SetCaption;
begin
  Text := ResStr_Multimedia;
end;  // TMultimediaFolderNode.SetCaption

{-------------------------------------------------------------------------------
}
function TMultimediaFolderNode.StoredProcByChildType(ALeafNodeClass: TLeafNodeClass): String;
begin
  Result := '';
  
  if ALeafNodeClass = TMultimediaLeafNode then
    case ParentNodeContext of
      ncCollection,
      ncSpecimen,
      ncStore,
      ncAccession,
      ncLoan,
      ncMovement,
      ncConditionCheck,
      ncJob,
      ncTask,
      ncInscriptionLabel: Result := 'usp_Multimedia_Select_ForAll';
    else
      raise EBrowserNodeError.Create(ResStr_ChildNodeContextInvalid);
    end
  else
    raise EBrowserNodeError.Create(ResStr_InvalidChildNodeTypeIndexRequest);
end;  // TMultimediaFolderNode.StoredProcByChildType 

{-==============================================================================
    TMultimediaLeafNode
===============================================================================}
{-------------------------------------------------------------------------------
}
class function TMultimediaLeafNode.ClassTableName: String;
begin
  Result := TN_SOURCE_JOIN;
end;  // TMultimediaLeafNode.ClassTableName 

{-------------------------------------------------------------------------------
}
function TMultimediaLeafNode.GetAssociatedFrame: TBaseDetailFrameClass;
begin
  Result := TfraMultimedia;
end;  // TMultimediaLeafNode.GetAssociatedFrame 

{-------------------------------------------------------------------------------
}
function TMultimediaLeafNode.GetCanDelete: Boolean;
var
  lTopLevelNode: TFlyNode;
begin
  lTopLevelNode := TopLevelNode;
  if Assigned(lTopLevelNode) then
    if lTopLevelNode is TTopLevelNode then
      Result := TTopLevelNode(lTopLevelNode).CanDelete
    else
      raise EBrowserNodeError.Create(ResStr_DomainMaskNotFoundOnTopLevelNode)
  else
    raise EBrowserNodeError.Create(ResStr_DomainMaskNotFoundOnTopLevelNode);
end;  // TMultimediaLeafNode.GetCanDelete 

{-------------------------------------------------------------------------------
}
function TMultimediaLeafNode.GetCanEdit: Boolean;
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
end;  // TMultimediaLeafNode.GetCanEdit 

{-------------------------------------------------------------------------------
}
function TMultimediaLeafNode.GetImageIndex: Integer;
begin
  Result := 29;
end;  // TMultimediaLeafNode.GetImageIndex 

{-------------------------------------------------------------------------------
  Retrieve additional properties.  By default this includes the Key. 
}
function TMultimediaLeafNode.GetProperty(const AName: String): Variant;
begin
  Result := InternalGetProperty(AName);
end;  // TMultimediaLeafNode.GetProperty 

{-------------------------------------------------------------------------------
}
function TMultimediaLeafNode.InternalGetProperty(const AName: String): Variant;
begin
  Result := inherited InternalGetProperty(AName);
  
  if Result=Unassigned then begin
    if AName = PROP_NODE_CONTEXT then
      if TopLevelNode is TContainerNode then
        Result := TContainerNode(TopLevelNode).NodeContext;
  end;
end;  // TMultimediaLeafNode.InternalGetProperty 

{-------------------------------------------------------------------------------
}
procedure TMultimediaLeafNode.SetCaption(ARecordset: _Recordset);
var
  lFileName: String;
begin
  if ARecordset.Fields['Title'].Value = Null then begin
    lFileName := ARecordset.Fields['File_Name'].Value;
  
    while (Pos('\', lFileName) <> 0) do begin
      lFileName := Copy(lFileName, Pos('\', lFileName) + 1, Length(lFileName));
    end;
  
    Caption := lFileName;
    end
  else
    Caption := ARecordset.Fields['Title'].Value;
end;  // TMultimediaLeafNode.SetCaption 

{-==============================================================================
    TPeopleOrganisationFolderNode
===============================================================================}
{-------------------------------------------------------------------------------
}
procedure TPeopleOrganisationFolderNode.AddChildNodesOfType(ABrowserNodeClass: 
    TBrowserNodeClass);
var
  lNewNode: TFolderNode;
begin
  if ABrowserNodeClass = TCollectorDeterminerFolderNode then begin
    lNewNode:= TFolderNode(Tree.Items.AddTypedChild(Self, ABrowserNodeClass));
    lNewNode.Initialise(Key);
  end else
  if ABrowserNodeClass = TPeopleOrganisationOtherFolderNode then begin
    lNewNode:= TFolderNode(Tree.Items.AddTypedChild(Self, ABrowserNodeClass));
    lNewNode.Initialise(Key);
  end else
    raise EBrowserNodeError.Create(ResStr_InvalidChildNodeTypeIndexRequest);
end;  // TPeopleOrganisationFolderNode.AddChildNodesOfType 

{-------------------------------------------------------------------------------
}
class function TPeopleOrganisationFolderNode.ClassTableName: String;
begin
  Result := '';
end;  // TPeopleOrganisationFolderNode.ClassTableName 

{-------------------------------------------------------------------------------
}
function TPeopleOrganisationFolderNode.GetCanAdd: Boolean;
begin
  Result := False;
end;  // TPeopleOrganisationFolderNode.GetCanAdd 

{-------------------------------------------------------------------------------
}
function TPeopleOrganisationFolderNode.GetChildNodeType(Index: Integer): TBrowserNodeClass;
begin
  case Index of
    0: Result := TCollectorDeterminerFolderNode;
    1: Result := TPeopleOrganisationOtherFolderNode;
  else
    raise EBrowserNodeError.Create(ResStr_InvalidChildNodeTypeIndexRequest);
  end;
end;  // TPeopleOrganisationFolderNode.GetChildNodeType 

{-------------------------------------------------------------------------------
}
function TPeopleOrganisationFolderNode.GetChildNodeTypeCount: Integer;
begin
  Result:= 2;
end;  // TPeopleOrganisationFolderNode.GetChildNodeTypeCount 

{-------------------------------------------------------------------------------
}
procedure TPeopleOrganisationFolderNode.SetCaption;
begin
  Text := ResStr_PeopleOrganisations;
end;  // TPeopleOrganisationFolderNode.SetCaption 

{-------------------------------------------------------------------------------
}
function TPeopleOrganisationFolderNode.StoredProcByChildType(ALeafNodeClass: TLeafNodeClass): 
    String;
begin
  Result := '';
  raise EBrowserNodeError.Create(ResStr_InvalidChildNodeTypeIndexRequest);
end;  // TPeopleOrganisationFolderNode.StoredProcByChildType 

{-==============================================================================
    TPeopleOrganisationOtherFolderNode
===============================================================================}
{-------------------------------------------------------------------------------
}
class function TPeopleOrganisationOtherFolderNode.ClassTableName: String;
begin
  Result := TN_NAME;
end;  // TPeopleOrganisationOtherFolderNode.ClassTableName 

{-------------------------------------------------------------------------------
}
function TPeopleOrganisationOtherFolderNode.GetAddButtonMenuCaption(Index: Integer): String;
begin
  if Index = 0 then
    Result := ResStr_Add
  else
    raise EAddMenuItemError.Create(ResStr_InvalidAddMenuIndexRequest);
end;  // TPeopleOrganisationOtherFolderNode.GetAddButtonMenuCaption 

{-------------------------------------------------------------------------------
}
function TPeopleOrganisationOtherFolderNode.GetAddButtonMenuCaptionsCount: Integer;
begin
  Result := 1;
end;  // TPeopleOrganisationOtherFolderNode.GetAddButtonMenuCaptionsCount 

{-------------------------------------------------------------------------------
}
function TPeopleOrganisationOtherFolderNode.GetAddMenuIsAdd(AMenuIndex: Integer): Boolean;
begin
  case AMenuIndex of
    0 : Result := True
  else
    Result := False;
  end;
end;  // TPeopleOrganisationOtherFolderNode.GetAddMenuIsAdd 

{-------------------------------------------------------------------------------
}
function TPeopleOrganisationOtherFolderNode.GetChildNodeType(Index: Integer): 
    TBrowserNodeClass;
begin
  case Index of
    0: Result := TRelatedNameLeafNode;
  else
    raise EBrowserNodeError.Create(ResStr_InvalidChildNodeTypeIndexRequest);
  end;
end;  // TPeopleOrganisationOtherFolderNode.GetChildNodeType 

{-------------------------------------------------------------------------------
}
function TPeopleOrganisationOtherFolderNode.GetChildNodeTypeCount: Integer;
begin
  Result:= 1;
end;  // TPeopleOrganisationOtherFolderNode.GetChildNodeTypeCount 

{-------------------------------------------------------------------------------
}
procedure TPeopleOrganisationOtherFolderNode.SetCaption;
begin
  Text := ResStr_PeopleOrganisationOther;
end;  // TPeopleOrganisationOtherFolderNode.SetCaption 

{-------------------------------------------------------------------------------
}
function TPeopleOrganisationOtherFolderNode.StoredProcByChildType(ALeafNodeClass: 
    TLeafNodeClass): String;
begin
  Result := '';
  
  if ALeafNodeClass = TRelatedNameLeafNode then
    if ParentNodeContext in [ncCollection, ncSpecimen] then
      Result := 'usp_PeopleOrganisationsOther_Select_ForCollectionUnit'
    else
      raise EBrowserNodeError.Create(ResStr_ChildNodeContextInvalid)
  else
    raise EBrowserNodeError.Create(ResStr_InvalidChildNodeTypeIndexRequest);
end;  // TPeopleOrganisationOtherFolderNode.StoredProcByChildType 


{-==============================================================================
    TPeopleOrganisationLeafNode
===============================================================================}
{-------------------------------------------------------------------------------
}
class function TPeopleOrganisationLeafNode.ClassTableName: String;
begin
  Result := TN_NAME;
end;  // TPeopleOrganisationLeafNode.ClassTableName 

{-------------------------------------------------------------------------------
}
function TPeopleOrganisationLeafNode.GetAssociatedFrame: TBaseDetailFrameClass;
begin
  Result := TfraPeople;
end;  // TPeopleOrganisationLeafNode.GetAssociatedFrame 

{-------------------------------------------------------------------------------
}
function TPeopleOrganisationLeafNode.GetCanAdd: Boolean;
begin
  Result := False;
end;  // TPeopleOrganisationLeafNode.GetCanAdd 

{-------------------------------------------------------------------------------
}
function TPeopleOrganisationLeafNode.GetCanDelete: Boolean;
begin
  Result := False;
end;  // TPeopleOrganisationLeafNode.GetCanDelete 

{-------------------------------------------------------------------------------
}
function TPeopleOrganisationLeafNode.GetCanEdit: Boolean;
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
end;  // TPeopleOrganisationLeafNode.GetCanEdit 

{-------------------------------------------------------------------------------
}
function TPeopleOrganisationLeafNode.GetImageIndex: Integer;
begin
  Result := 41;
end;  // TPeopleOrganisationLeafNode.GetImageIndex 

{-------------------------------------------------------------------------------
}
function TPeopleOrganisationLeafNode.GetNodeContext: TNodeContext;
begin
  Result := ncRecorder;
end;  // TPeopleOrganisationLeafNode.GetNodeContext 

{-------------------------------------------------------------------------------
}
function TPeopleOrganisationLeafNode.GetProperty(const AName: string): Variant;
begin
  Result := InternalGetProperty(AName);
end;  // TPeopleOrganisationLeafNode.GetProperty 

{-------------------------------------------------------------------------------
}
function TPeopleOrganisationLeafNode.InternalGetProperty(const AName: string): Variant;
begin
  Result := inherited InternalGetProperty(AName);
  
  if Result = Unassigned then
    if AName = PROP_IS_RELATED_NAME_LEAF_NODE then
      Result := False;
end;  // TPeopleOrganisationLeafNode.InternalGetProperty 

{-==============================================================================
    TRelatedNameLeafNode
===============================================================================}
{-------------------------------------------------------------------------------
}
class function TRelatedNameLeafNode.ClassTableName: String;
begin
  Result := TN_NAME;
end;  // TRelatedNameLeafNode.ClassTableName 

{-------------------------------------------------------------------------------
  When adding a new Related Name leaf node, the drag and drop key should be set immediately so 
      that the user can double click on the node and navigate to the Names and Addresses 
      module without having to refresh the node first. 
}
procedure TRelatedNameLeafNode.FindAndSetDragDropKey;
begin
  DragDropKey := dmGeneral.GetStoredProcOutputParam(
      'usp_CollectionUnitName_DragDropKey_Get',
                          ['@Key', Key], '@DragDropKey');
end;  // TRelatedNameLeafNode.FindAndSetDragDropKey 

{-------------------------------------------------------------------------------
}
function TRelatedNameLeafNode.GetAssociatedFrame: TBaseDetailFrameClass;
begin
  Result := TfraPeopleGeneral;
end;  // TRelatedNameLeafNode.GetAssociatedFrame 

{-------------------------------------------------------------------------------
}
function TRelatedNameLeafNode.GetCanEdit: Boolean;
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
end;  // TRelatedNameLeafNode.GetCanEdit 

{-------------------------------------------------------------------------------
}
function TRelatedNameLeafNode.GetImageIndex: Integer;
begin
  Result := 33;
end;  // TRelatedNameLeafNode.GetImageIndex 

{-------------------------------------------------------------------------------
}
function TRelatedNameLeafNode.GetNodeContext: TNodeContext;
begin
  Result := ncRecorder;
end;  // TRelatedNameLeafNode.GetNodeContext 

{-------------------------------------------------------------------------------
}
function TRelatedNameLeafNode.GetProperty(const AName: string): Variant;
begin
  Result := InternalGetProperty(AName);
end;  // TRelatedNameLeafNode.GetProperty 

{-------------------------------------------------------------------------------
}
procedure TRelatedNameLeafNode.InitialiseDragDropKey(ARecordset: _Recordset);
begin
  DragDropKey := ARecordset.Fields['Rec_Item_Key'].Value;
end;  // TRelatedNameLeafNode.InitialiseDragDropKey 

{-------------------------------------------------------------------------------
}
procedure TRelatedNameLeafNode.InitialiseHyperlinkKey(ARecordset: _Recordset);
begin
  HyperLinkKey := ARecordset.Fields['Rec_Item_Key'].Value;
end;  // TRelatedNameLeafNode.InitialiseHyperlinkKey 

{-------------------------------------------------------------------------------
}
function TRelatedNameLeafNode.InternalGetProperty(const AName: string): Variant;
begin
  Result := inherited InternalGetProperty(AName);
  
  if Result=Unassigned then
    if AName = PROP_IS_RELATED_NAME_LEAF_NODE then
      Result := True;
end;  // TRelatedNameLeafNode.InternalGetProperty 

{-------------------------------------------------------------------------------
}
procedure TRelatedNameLeafNode.SetCaption(ARecordset: _Recordset);
begin
  Caption:= ARecordset.Fields['Item_Name'].Value;
end;  // TRelatedNameLeafNode.SetCaption 


{-==============================================================================
    TNumberingHistoryFolderNode
===============================================================================}
{-------------------------------------------------------------------------------
}
procedure TNumberingHistoryFolderNode.AddChildNodesOfType(ABrowserNodeClass: 
    TBrowserNodeClass);
begin
  if ABrowserNodeClass = TAccessionNumberLeafNode then
    PopulateFromDatabase(nil, StoredProcByChildType(TAccessionNumberLeafNode))
  else
  if ABrowserNodeClass = TRegistrationNumberLeafNode then
  begin
  end
  //Do nothing as both types of node are added by usp_NumberHistory_Select_ForCollectionUnit
  else
    raise EBrowserNodeError.Create(ResStr_InvalidChildNodeTypeIndexRequest);
end;  // TNumberingHistoryFolderNode.AddChildNodesOfType 

{-------------------------------------------------------------------------------
  Adds a node to the treeview of the required type. 
}
function TNumberingHistoryFolderNode.AddNode(ATree: TRapidTree; AMenuIndex: integer): TFlyNode;
begin
  Result := TLeafNode(Tree.Items.AddTypedChild(Self,
      TRegistrationNumberLeafNode));
  TLeafNode(Result).InitialiseNewNode;
end;  // TNumberingHistoryFolderNode.AddNode 

{-------------------------------------------------------------------------------
}
class function TNumberingHistoryFolderNode.ClassTableName: String;
begin
  Result := TN_MIXED_DATA;
end;  // TNumberingHistoryFolderNode.ClassTableName 

{-------------------------------------------------------------------------------
}
function TNumberingHistoryFolderNode.GetAddButtonMenuCaption(Index: Integer): String;
begin
  case Index of
    0: Result := ResStr_Add;
  else
    raise EAddMenuItemError.Create(ResStr_InvalidAddMenuIndexRequest);
  end;
end;  // TNumberingHistoryFolderNode.GetAddButtonMenuCaption 

{-------------------------------------------------------------------------------
}
function TNumberingHistoryFolderNode.GetAddButtonMenuCaptionsCount: Integer;
begin
  Result := 1;
end;  // TNumberingHistoryFolderNode.GetAddButtonMenuCaptionsCount 

{-------------------------------------------------------------------------------
}
function TNumberingHistoryFolderNode.GetAddMenuIsAdd(AMenuIndex: Integer): Boolean;
begin
  case AMenuIndex of
    0 : Result := True
  else
    Result := False;
  end;
end;  // TNumberingHistoryFolderNode.GetAddMenuIsAdd 

{-------------------------------------------------------------------------------
}
function TNumberingHistoryFolderNode.GetChildNodeType(Index: Integer): TBrowserNodeClass;
begin
  case Index of
    0: Result := TAccessionNumberLeafNode;
    1: Result := TRegistrationNumberLeafNode;
  else
    raise EBrowserNodeError.Create(ResStr_InvalidChildNodeTypeIndexRequest);
  end;
end;  // TNumberingHistoryFolderNode.GetChildNodeType 

{-------------------------------------------------------------------------------
}
function TNumberingHistoryFolderNode.GetChildNodeTypeCount: Integer;
begin
  Result:= 2;
end;  // TNumberingHistoryFolderNode.GetChildNodeTypeCount 

{-------------------------------------------------------------------------------
}
procedure TNumberingHistoryFolderNode.PopulateFromDatabase(ALeafNodeClass: TLeafNodeClass; 
    const AStoredProcName: String);
var
  lNewNode: TLeafNode;
  lRecordset: _Recordset;
begin
  if not (ALeafNodeClass = nil) then Exit;
  
  Tree.Items.BeginUpdate;
  lRecordset := dmGeneral.GetRecordset(AStoredProcName, GetStoredProcParams);
  
  try
    if lRecordset.RecordCount > 0 then begin
      lRecordSet.MoveFirst;
      while not lRecordset.EOF do begin
        if lRecordset.Fields['IsAccession'].Value = 1 then
          lNewNode:= TLeafNode(Tree.Items.AddTypedChild(Self,
              TAccessionNumberLeafNode))
        else
          lNewNode:= TLeafNode(Tree.Items.AddTypedChild(Self,
              TRegistrationNumberLeafNode));
        //Will never return a non-LeafNode so NodeContext needn't be set.
        lNewNode.Initialise(lRecordset); //Sets Key and Caption
        lRecordset.MoveNext;
      end;
    end;
  finally
    Tree.Items.EndUpdate;
    lRecordset.Close;
  end;
end;  // TNumberingHistoryFolderNode.PopulateFromDatabase 

{-------------------------------------------------------------------------------
}
procedure TNumberingHistoryFolderNode.SetCaption;
begin
  Text := ResStr_NumberingHistory;
end;  // TNumberingHistoryFolderNode.SetCaption 

{-------------------------------------------------------------------------------
}
function TNumberingHistoryFolderNode.StoredProcByChildType(ALeafNodeClass: TLeafNodeClass): 
    String;
begin
  if (ALeafNodeClass = TAccessionNumberLeafNode) or
     (ALeafNodeClass = TRegistrationNumberLeafNode) then
    Result := 'usp_NumberHistory_Select_ForCollectionUnit'
  else
    raise EBrowserNodeError.Create(ResStr_InvalidChildNodeTypeIndexRequest);
end;  // TNumberingHistoryFolderNode.StoredProcByChildType 

{-==============================================================================
    TAccessionNumberLeafNode
===============================================================================}
{-------------------------------------------------------------------------------
}
class function TAccessionNumberLeafNode.ClassTableName: String;
begin
  Result := TN_MOVEMENT;
end;  // TAccessionNumberLeafNode.ClassTableName 

{-------------------------------------------------------------------------------
}
function TAccessionNumberLeafNode.GetAssociatedFrame: TBaseDetailFrameClass;
begin
  Result := TfraNumberHistoryReadOnly;
end;  // TAccessionNumberLeafNode.GetAssociatedFrame 

{-------------------------------------------------------------------------------
}
function TAccessionNumberLeafNode.GetCanAdd: Boolean;
begin
  Result := False;
end;  // TAccessionNumberLeafNode.GetCanAdd 

{-------------------------------------------------------------------------------
}
function TAccessionNumberLeafNode.GetCanDelete: Boolean;
begin
  Result := False;
end;  // TAccessionNumberLeafNode.GetCanDelete 

{-------------------------------------------------------------------------------
}
function TAccessionNumberLeafNode.GetCanEdit: Boolean;
begin
  Result := False;
end;  // TAccessionNumberLeafNode.GetCanEdit 

{-------------------------------------------------------------------------------
}
function TAccessionNumberLeafNode.GetImageIndex: Integer;
begin
  Result := 34;
end;  // TAccessionNumberLeafNode.GetImageIndex 

{-------------------------------------------------------------------------------
}
function TAccessionNumberLeafNode.GetNodeContext: TNodeContext;
begin
  Result := ncAccession;
end;  // TAccessionNumberLeafNode.GetNodeContext 

{-------------------------------------------------------------------------------
}
procedure TAccessionNumberLeafNode.SetCaption(ARecordset: _Recordset);
begin
  Caption:= ARecordset.Fields['Item_Name'].Value + ' - ' +
            ARecordset.Fields['Number'].Value;
end;  // TAccessionNumberLeafNode.SetCaption 

{-==============================================================================
    TRegistrationNumberLeafNode
===============================================================================}
{-------------------------------------------------------------------------------
}
class function TRegistrationNumberLeafNode.ClassTableName: String;
begin
  Result := TN_COLLECTION_UNIT_NUMBER;
end;  // TRegistrationNumberLeafNode.ClassTableName 

{-------------------------------------------------------------------------------
}
function TRegistrationNumberLeafNode.GetAssociatedFrame: TBaseDetailFrameClass;
begin
  Result := TfraNumberHistory;
end;  // TRegistrationNumberLeafNode.GetAssociatedFrame 

{-------------------------------------------------------------------------------
}
function TRegistrationNumberLeafNode.GetCanDelete: Boolean;
var
  lTopLevelNode: TFlyNode;
begin
  lTopLevelNode := TopLevelNode;
  if Assigned(lTopLevelNode) then
    if lTopLevelNode is TTopLevelNode then
      Result := TTopLevelNode(lTopLevelNode).CanDelete
    else
     raise EBrowserNodeError.Create(ResStr_DomainMaskNotFoundOnTopLevelNode)
  else
    raise EBrowserNodeError.Create(ResStr_DomainMaskNotFoundOnTopLevelNode);
end;  // TRegistrationNumberLeafNode.GetCanDelete 

{-------------------------------------------------------------------------------
}
function TRegistrationNumberLeafNode.GetCanEdit: Boolean;
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
end;  // TRegistrationNumberLeafNode.GetCanEdit 

{-------------------------------------------------------------------------------
}
function TRegistrationNumberLeafNode.GetImageIndex: Integer;
begin
  Result := 35;
end;  // TRegistrationNumberLeafNode.GetImageIndex 

{-------------------------------------------------------------------------------
}
procedure TRegistrationNumberLeafNode.SetCaption(ARecordset: _Recordset);
begin
  Caption:= ARecordset.Fields['Item_Name'].Value + ' - ' +
            ARecordset.Fields['Number'].Value;
end;  // TRegistrationNumberLeafNode.SetCaption 

{-==============================================================================
    TProcessFolderNode
===============================================================================}
{-------------------------------------------------------------------------------
}
class function TProcessFolderNode.ClassTableName: String;
begin
  Result := TN_COLLECTION_UNIT_PROCESS;
end;  // TProcessFolderNode.ClassTableName 

{-------------------------------------------------------------------------------
}
function TProcessFolderNode.GetAddButtonMenuCaption(Index: Integer): String;
begin
  if Index = 0 then
    Result := ResStr_Add
  else
    raise EAddMenuItemError.Create(ResStr_InvalidAddMenuIndexRequest);
end;  // TProcessFolderNode.GetAddButtonMenuCaption 

{-------------------------------------------------------------------------------
}
function TProcessFolderNode.GetAddButtonMenuCaptionsCount: Integer;
begin
  Result := 1;
end;  // TProcessFolderNode.GetAddButtonMenuCaptionsCount 

{-------------------------------------------------------------------------------
}
function TProcessFolderNode.GetAddMenuIsAdd(AMenuIndex: Integer): Boolean;
begin
  case AMenuIndex of
    0 : Result := True
  else
    Result := False;
  end;
end;  // TProcessFolderNode.GetAddMenuIsAdd 

{-------------------------------------------------------------------------------
}
function TProcessFolderNode.GetChildNodeType(Index: Integer): TBrowserNodeClass;
begin
  case Index of
    0: Result := TProcessLeafNode;
  else
    raise EBrowserNodeError.Create(ResStr_InvalidChildNodeTypeIndexRequest);
  end;
end;  // TProcessFolderNode.GetChildNodeType 

{-------------------------------------------------------------------------------
}
function TProcessFolderNode.GetChildNodeTypeCount: Integer;
begin
  Result:= 1;
end;  // TProcessFolderNode.GetChildNodeTypeCount 

{-------------------------------------------------------------------------------
}
procedure TProcessFolderNode.SetCaption;
begin
  Text := ResStr_Processes;
end;  // TProcessFolderNode.SetCaption 

{-------------------------------------------------------------------------------
}
function TProcessFolderNode.StoredProcByChildType(ALeafNodeClass: TLeafNodeClass): String;
begin
  Result := '';
  
  if ALeafNodeClass = TProcessLeafNode then
    Result := 'usp_Processes_Select_ForCollectionUnit'
  else
    raise EBrowserNodeError.Create(ResStr_InvalidChildNodeTypeIndexRequest);
end;  // TProcessFolderNode.StoredProcByChildType 

{-==============================================================================
    TProcessLeafNode
===============================================================================}
{-------------------------------------------------------------------------------
}
class function TProcessLeafNode.ClassTableName: String;
begin
  Result := TN_COLLECTION_UNIT_PROCESS;
end;  // TProcessLeafNode.ClassTableName 

{-------------------------------------------------------------------------------
}
function TProcessLeafNode.GetAssociatedFrame: TBaseDetailFrameClass;
begin
  Result := TfraProcess;
end;  // TProcessLeafNode.GetAssociatedFrame 

{-------------------------------------------------------------------------------
}
function TProcessLeafNode.GetCanDelete: Boolean;
var
  lTopLevelNode: TFlyNode;
begin
  lTopLevelNode := TopLevelNode;
  if Assigned(lTopLevelNode) then
    if lTopLevelNode is TTopLevelNode then
      Result := TTopLevelNode(lTopLevelNode).CanDelete
    else
     raise EBrowserNodeError.Create(ResStr_DomainMaskNotFoundOnTopLevelNode)
  else
    raise EBrowserNodeError.Create(ResStr_DomainMaskNotFoundOnTopLevelNode);
end;  // TProcessLeafNode.GetCanDelete 

{-------------------------------------------------------------------------------
}
function TProcessLeafNode.GetCanEdit: Boolean;
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
end;  // TProcessLeafNode.GetCanEdit 

{-------------------------------------------------------------------------------
}
function TProcessLeafNode.GetImageIndex: Integer;
begin
  Result := 32;
end;  // TProcessLeafNode.GetImageIndex 

{-------------------------------------------------------------------------------
}
function TProcessLeafNode.GetProperty(const AName: string): Variant;
begin
  Result := InternalGetProperty(AName);
end;  // TProcessLeafNode.GetProperty 

{-------------------------------------------------------------------------------
}
function TProcessLeafNode.InternalGetProperty(const AName: string): Variant;
begin
  Result := inherited InternalGetProperty(AName);
  
  if Result = Unassigned then begin
    if AName = PROP_NODE_CONTEXT then
      if TopLevelNode is TContainerNode then
        Result := TContainerNode(TopLevelNode).NodeContext;
  end;
end;  // TProcessLeafNode.InternalGetProperty 

{-==============================================================================
    TValuationFolderNode
===============================================================================}
{-------------------------------------------------------------------------------
}
procedure TValuationFolderNode.AddChildNodesOfType(ABrowserNodeClass: TBrowserNodeClass);
begin
  // dummy database loop to illustrate concept
  if ABrowserNodeClass = TValuationLeafNode then begin
    case ParentNodeContext of
      ncCollection,
      ncSpecimen       : PopulateFromDatabase(TValuationLeafNode,
          StoredProcByChildType(TValuationLeafNode));
      ncAccession,
      ncLoan,
      ncMovement       : PopulateFromDatabase(TValuationLeafNode,
          StoredProcByChildType(TValuationLeafNode));
    else
      raise EBrowserNodeError.Create(ResStr_ChildNodeContextInvalid);
    end;
  end
  else
    raise EBrowserNodeError.Create(ResStr_InvalidChildNodeTypeIndexRequest);
end;  // TValuationFolderNode.AddChildNodesOfType 

{-------------------------------------------------------------------------------
}
class function TValuationFolderNode.ClassTableName: String;
begin
  Result := TN_VALUATION;
end;  // TValuationFolderNode.ClassTableName 

{-------------------------------------------------------------------------------
}
procedure TValuationFolderNode.DeleteLink(const AJoinTableKey: String);
begin
  case ParentNodeContext of
    ncCollection,
    ncSpecimen   : dmGeneral.RunStoredProc('usp_CollectionUnitValuation_Delete',
                                           ['@Key', AJoinTableKey]);
    ncAccession,
    ncLoan,
    ncMovement   : dmGeneral.RunStoredProc('usp_MovementValuation_Delete',
                                           ['@Key', AJoinTableKey]);
  else
    raise EBrowserNodeError.Create(ResStr_ChildNodeContextInvalid);
  end;
end;  // TValuationFolderNode.DeleteLink 

{-------------------------------------------------------------------------------
}
function TValuationFolderNode.GetAddButtonMenuCaption(Index: Integer): String;
begin
  if Index = 0 then
    Result := ResStr_Add
  else
    raise EAddMenuItemError.Create(ResStr_InvalidAddMenuIndexRequest);
end;  // TValuationFolderNode.GetAddButtonMenuCaption 

{-------------------------------------------------------------------------------
}
function TValuationFolderNode.GetAddButtonMenuCaptionsCount: Integer;
begin
  Result := 1;
end;  // TValuationFolderNode.GetAddButtonMenuCaptionsCount 

{-------------------------------------------------------------------------------
}
function TValuationFolderNode.GetAddMenuIsAdd(AMenuIndex: Integer): Boolean;
begin
  case AMenuIndex of
    0 : Result := True
  else
    Result := False;
  end;
end;  // TValuationFolderNode.GetAddMenuIsAdd 

{-------------------------------------------------------------------------------
}
function TValuationFolderNode.GetChildNodeType(Index: Integer): TBrowserNodeClass;
begin
  case Index of
    0: Result := TValuationLeafNode;
  else
    raise EBrowserNodeError.Create(ResStr_InvalidChildNodeTypeIndexRequest);
  end;
end;  // TValuationFolderNode.GetChildNodeType 

{-------------------------------------------------------------------------------
}
function TValuationFolderNode.GetChildNodeTypeCount: Integer;
begin
  Result:= 1;
end;  // TValuationFolderNode.GetChildNodeTypeCount 

{-------------------------------------------------------------------------------
}
function TValuationFolderNode.GetNodeContext: TNodeContext;
begin
  Result := ncValuation;
end;  // TValuationFolderNode.GetNodeContext 

{-------------------------------------------------------------------------------
}
function TValuationFolderNode.GetStoredProcParams: TVariantArray;
var
  SortIndex: Integer;
begin
  SortIndex := ViewTypeManager.ViewTypeByNodeContext(
      NodeContext).SortOrderDefaultIndex;
  
  Result:= VarArrayOf(['@ParentKey', Key, '@UserID', AppSettings.UserID,
      '@SortOrderIndex', SortIndex]);
end;  // TValuationFolderNode.GetStoredProcParams 

{-------------------------------------------------------------------------------
}
procedure TValuationFolderNode.SetCaption;
begin
  Text:= ResStr_Valuations;
end;  // TValuationFolderNode.SetCaption 

{-------------------------------------------------------------------------------
}
function TValuationFolderNode.StoredProcByChildType(ALeafNodeClass: TLeafNodeClass): String;
begin
  Result := '';
  
  if ALeafNodeClass = TValuationLeafNode then begin
    case ParentNodeContext of
      ncCollection,
      ncSpecimen       : Result := 'usp_Valuations_Select_ForCollectionUnit';
      ncAccession,
      ncLoan,
      ncMovement       : Result := 'usp_Valuations_Select_ForMovement';
    else
      raise EBrowserNodeError.Create(ResStr_ChildNodeContextInvalid);
    end;
  end
  else
    raise EBrowserNodeError.Create(ResStr_InvalidChildNodeTypeIndexRequest);
end;  // TValuationFolderNode.StoredProcByChildType 

{-------------------------------------------------------------------------------
}
function TValuationFolderNode.UpdateNodeRelationship(const NewNodeKey: String): String;
begin
  case ParentNodeContext of
    ncCollection,
    ncSpecimen,
    ncStore:     Result := VarToStr(dmGeneral.GetStoredProcOutputParam(
        'usp_Valuation_Update_ForCollectionUnit',
                     ['@ParentKey', Key, '@ChildKey', NewNodeKey], '@JoinKey'));
    ncMovement,
    ncLoan,
    ncAccession: Result := VarToStr(dmGeneral.GetStoredProcOutputParam(
        'usp_Valuation_Update_ForMovement',
                     ['@ParentKey', Key, '@ChildKey', NewNodeKey], '@JoinKey'));
  else
    raise EAddMenuItemError.Create(ResStr_InvalidNodeContext);
  end;
end;  // TValuationFolderNode.UpdateNodeRelationship 

{-==============================================================================
    TValuationLeafNode
===============================================================================}
{-------------------------------------------------------------------------------
}
class function TValuationLeafNode.ClassTableName: String;
begin
  Result := TN_VALUATION;
end;  // TValuationLeafNode.ClassTableName 

{-------------------------------------------------------------------------------
}
function TValuationLeafNode.GetAssociatedFrame: TBaseDetailFrameClass;
begin
  Result := TfraValuation;
end;  // TValuationLeafNode.GetAssociatedFrame 

{-------------------------------------------------------------------------------
}
function TValuationLeafNode.GetCanAdd: Boolean;
begin
  Result := AppSettings.AllowFinance and inherited GetCanAdd;
end;  // TValuationLeafNode.GetCanAdd 

{-------------------------------------------------------------------------------
}
function TValuationLeafNode.GetCanDelete: Boolean;
begin
  Result := AppSettings.AllowFinance and inherited GetCanDelete;
end;  // TValuationLeafNode.GetCanDelete 

{-------------------------------------------------------------------------------
}
function TValuationLeafNode.GetCanEdit: Boolean;
begin
  Result := AppSettings.AllowFinance and inherited GetCanEdit;
end;  // TValuationLeafNode.GetCanEdit 

{-------------------------------------------------------------------------------
}
function TValuationLeafNode.GetDomainStoredProc: String;
begin
  Result := 'usp_Valuation_DomainMask_Get';
end;  // TValuationLeafNode.GetDomainStoredProc 

{-------------------------------------------------------------------------------
}
function TValuationLeafNode.GetImageIndex: Integer;
begin
  Result := 7;
end;  // TValuationLeafNode.GetImageIndex 

{-------------------------------------------------------------------------------
}
function TValuationLeafNode.GetNodeContext: TNodeContext;
begin
  Result := ncValuation;
end;  // TValuationLeafNode.GetNodeContext 

{-------------------------------------------------------------------------------
}
procedure TValuationLeafNode.SetCaption(ARecordset: _Recordset);
begin
  Caption:= VarToStr(ARecordset.Fields['Display_Caption'].Value);
end;  // TValuationLeafNode.SetCaption 

{-==============================================================================
    TValuationTopLevelNode
===============================================================================}
{-------------------------------------------------------------------------------
}
procedure TValuationTopLevelNode.AddChildNodesOfType(ABrowserNodeClass: TBrowserNodeClass);
var
  lNewNode: TFolderNode;
begin
  if (ABrowserNodeClass = TCollectionFolderNode) or
     (ABrowserNodeClass = TSpecimenFolderNode) or
     (ABrowserNodeClass = TAccessionFolderNode) or (ABrowserNodeClass = TLoanFolderNode) or
     (ABrowserNodeClass = TMovementFolderNode) then
  begin
    lNewNode:= TFolderNode(Tree.Items.AddTypedChild(Self, ABrowserNodeClass));
    lNewNode.Initialise(Key);
  end else
    raise EBrowserNodeError.Create(ResStr_InvalidChildNodeTypeIndexRequest);
end;  // TValuationTopLevelNode.AddChildNodesOfType 

{-------------------------------------------------------------------------------
}
class function TValuationTopLevelNode.ClassTableName: String;
begin
  Result := TN_VALUATION;
end;  // TValuationTopLevelNode.ClassTableName 

{-------------------------------------------------------------------------------
}
function TValuationTopLevelNode.GetAssociatedFrame: TBaseDetailFrameClass;
begin
  Result := TfraValuation;
end;  // TValuationTopLevelNode.GetAssociatedFrame 

{-------------------------------------------------------------------------------
}
function TValuationTopLevelNode.GetCanAdd: Boolean;
begin
  Result := AppSettings.AllowFinance and inherited GetCanAdd;
end;  // TValuationTopLevelNode.GetCanAdd 

{-------------------------------------------------------------------------------
}
function TValuationTopLevelNode.GetCanDelete: Boolean;
begin
  Result := AppSettings.AllowFinance and inherited GetCanDelete;
end;  // TValuationTopLevelNode.GetCanDelete 

{-------------------------------------------------------------------------------
}
function TValuationTopLevelNode.GetCanEdit: Boolean;
begin
  Result := AppSettings.AllowFinance and inherited GetCanEdit;
end;  // TValuationTopLevelNode.GetCanEdit 

{-------------------------------------------------------------------------------
}
function TValuationTopLevelNode.GetChildNodeType(Index: Integer): TBrowserNodeClass;
begin
  case Index of
    0: Result := TCollectionFolderNode;
    1: Result := TSpecimenFolderNode;
    2: Result := TAccessionFolderNode;
    3: Result := TLoanFolderNode;
    4: Result := TMovementFolderNode;
  else
    raise EBrowserNodeError.Create(ResStr_InvalidChildNodeTypeIndexRequest);
  end;
end;  // TValuationTopLevelNode.GetChildNodeType 

{-------------------------------------------------------------------------------
}
function TValuationTopLevelNode.GetChildNodeTypeCount: Integer;
begin
  Result := 5;
end;  // TValuationTopLevelNode.GetChildNodeTypeCount 

{-------------------------------------------------------------------------------
}
function TValuationTopLevelNode.GetImageIndex: Integer;
begin
  Result := 7;
end;  // TValuationTopLevelNode.GetImageIndex 

{-------------------------------------------------------------------------------
}
function TValuationTopLevelNode.GetNodeContext: TNodeContext;
begin
  Result := ncValuation;
end;  // TValuationTopLevelNode.GetNodeContext 

{-------------------------------------------------------------------------------
}
function TValuationTopLevelNode.GetProperty(const AName: string): Variant;
begin
  Result := InternalGetProperty(AName);
end;  // TValuationTopLevelNode.GetProperty 

{-------------------------------------------------------------------------------
}
function TValuationTopLevelNode.InternalGetProperty(const AName: string): Variant;
begin
  Result := inherited InternalGetProperty(AName);
  
  if Result = Unassigned then begin
    if AName = PROP_NODE_CONTEXT then
      Result := ncValuation;
  end;
end;  // TValuationTopLevelNode.InternalGetProperty 

{-------------------------------------------------------------------------------
}
procedure TValuationTopLevelNode.SetCaption(ARecordset: _Recordset);
begin
  Caption:= VarToStr(ARecordset.Fields['Display_Caption'].Value);
end;  // TValuationTopLevelNode.SetCaption 

{-------------------------------------------------------------------------------
}
function TValuationTopLevelNode.StoredProcByChildType(ALeafNodeClass: TLeafNodeClass): String;
begin
  Result := '';
  raise EBrowserNodeError.Create(ResStr_InvalidChildNodeTypeIndexRequest);
end;  // TValuationTopLevelNode.StoredProcByChildType 

end.
