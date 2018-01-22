{===============================================================================
  Unit:        BrowserNodeFramework.pas

  Defines:     Many Classes

  Description: Contains framework node classes

  Model:       BrowserNodes.mpb

  Created:     August 2003

  Last revision information:
    $Revision: 81 $
    $Date: 17/09/10 13:42 $
    $Author: Robertjohnson $

===============================================================================}
unit BrowserNodeFramework;

interface

uses
  SysUtils, Windows, Messages, Classes, Graphics, Controls, Forms, Dialogs,
  RapTree, TreeColl, Menus, ADODB, BaseDetailFrameUnit, GeneralData,
  ResourceStrings, DataTypes, DataClasses, SearchManager, LuxembourgConstants,
  ComCtrls, Registry, InterfaceDataModule, ExceptionForm, DSSDataTypes;

const
  IID_ISortOrderProvider: TGUID = '{933D9ED4-FB50-49ED-85F7-E149648131AE}';

type
  {-----------------------------------------------------------------------------
    Interface allowing an object to expose Add menu options for the Collections Browser.
  }
  IAddMenuOptions = interface(IInterface)
    ['{B98E30D4-6B34-4839-8F5A-E9507DD10163}']
    function AddNode(ATree: TRapidTree; AMenuIndex: Integer): TFlyNode;
    function GetAddButtonMenuCaption(Index: Integer): String;
    function GetAddButtonMenuCaptionsCount: Integer;
    function GetAddMenuIsAdd(AMenuIndex: Integer): Boolean;
    property AddButtonMenuCaption[Index: Integer]: String read GetAddButtonMenuCaption;
    property AddButtonMenuCaptionsCount: Integer read GetAddButtonMenuCaptionsCount;
    property AddMenuIsAdd[AMenuIndex: Integer]: Boolean read GetAddMenuIsAdd;
  end;
  
  ISortOrderProvider = interface(IInterface)
    ['{933D9ED4-FB50-49ED-85F7-E149648131AE}']
    function Get_SortOrderCaption(AIndex: Integer): WideString; safecall;
    function Get_SortOrderCount: Integer; safecall;
    procedure SelectSort(AIndex: Integer); safecall;
    property SortOrderCaption[AIndex: Integer]: WideString read Get_SortOrderCaption;
    property SortOrderCount: Integer read Get_SortOrderCount;
  end;
  
  // Forward declarations for mutual referencing.
  TLeafNode = class;
  TViewTypeManager = class;
  TTopLevelNode = class;

  // Meta-classes.
  TBrowserNodeClass = class of TBrowserNode;
  TTopLevelNodeClass = class of TTopLevelNode;
  TLeafNodeClass = class of TLeafNode;

  TSearchControlType = (ctNormal, ctIndividual, ctName,
      ctConditionCheckConditionCombo, ctStatusCombo, ctDetermination,
      ctSpecimenTypeCombo, ctLocation, ctStoreTypeCombo, ctPriorityCombo,
      ctTaskTypeCombo, ctNumber);

  TBaseViewType = class(TObject, ISortOrderProvider)
  private
    FOnFrameNotification: TFrameNotificationEvent;
    FRefCount: Integer;
    FReg: TRegistry;
    FSearchDefaultIndex: Integer;
    FSortOrderDefaultIndex: Integer;
    FViewTypeManager: TViewTypeManager;
    function GetImageList: TImageList;
    procedure SetOnFrameNotification(Value: TFrameNotificationEvent);
    procedure SetSearchDefaultIndex(Value: Integer);
    procedure SetSortOrderDefaultIndex(Value: Integer);
  protected
    function GetCanAdd: Boolean; virtual;
    function GetImageIndex: Integer; virtual; abstract;
    function GetName: String; virtual; abstract;
    function GetNodeContext: TNodeContext; virtual; abstract;
    function GetNodeContextText: String; virtual; abstract;
    function GetPopulateTopLevelStoredProcName: String; virtual; abstract;
    function GetSearchCaption(Index: Integer): String; virtual; abstract;
    function GetSearchControlType(Index: Integer): TSearchControlType; virtual; abstract;
    function GetSearchCount: Integer; virtual; abstract;
    function GetSearchStoredProcName(Index: Integer): String; virtual; abstract;
    function GetSearchTextRequired(Index: Integer): Boolean; virtual;
    function GetSortStoredProcIndex(Index: Integer): Integer; virtual;
    function GetStoredProcParams: TVariantArray; virtual;
    function GetTopLevelNodeClass: TTopLevelNodeClass; virtual; abstract;
    function GetTransformSearchText(ASearchCaptionIndex: Integer; ASearchText: string): String;
        virtual;
    function Get_SortOrderCaption(AIndex: Integer): WideString; safecall;
    function Get_SortOrderCount: Integer; safecall;
    function InternalGetSortOrderCaption(AIndex: integer): Widestring; virtual; abstract;
    function InternalGetSortOrderCount: Integer; virtual; abstract;
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    procedure SelectSort(AIndex: Integer); safecall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  public
    constructor Create(AViewTypeManager: TViewTypeManager);
    destructor Destroy; override;
    function TransformSearchText(SearchCaptionIndex: Integer; ASearchText: string): String;
    property CanAdd: Boolean read GetCanAdd;
    property ImageIndex: Integer read GetImageIndex;
    property ImageList: TImageList read GetImageList;
    property Name: String read GetName;
    property NodeContext: TNodeContext read GetNodeContext;
    property NodeContextText: String read GetNodeContextText;
    property OnFrameNotification: TFrameNotificationEvent read FOnFrameNotification write
        SetOnFrameNotification;
    property PopulateTopLevelStoredProcName: String read GetPopulateTopLevelStoredProcName;
    property SearchCaption[Index: Integer]: String read GetSearchCaption;
    property SearchControlType[Index: Integer]: TSearchControlType read GetSearchControlType;
    property SearchCount: Integer read GetSearchCount;
    property SearchDefaultIndex: Integer read FSearchDefaultIndex write SetSearchDefaultIndex;
    property SearchStoredProcName[Index: Integer]: String read GetSearchStoredProcName;
    property SearchTextRequired[Index: Integer]: Boolean read GetSearchTextRequired;
    property SortOrderDefaultIndex: Integer read FSortOrderDefaultIndex write
        SetSortOrderDefaultIndex;
    property SortStoredProcIndex[Index: Integer]: Integer read GetSortStoredProcIndex;
    property StoredProcParams: TVariantArray read GetStoredProcParams;
    property TopLevelNodeClass: TTopLevelNodeClass read GetTopLevelNodeClass;
  end;
  
  TViewTypeManager = class(TObject)
  private
    FComboBox: TComboBoxEx;
    FHiddenViewTypes: Array of TBaseViewType;
    FOnSearchTypeChange: TNotifyEvent;
    FSelectedViewType: TBaseViewType;
    FViewTypes: Array of TBaseViewType;
    function GetSelected: TBaseViewType;
    function GetViewType(Index: Integer): TBaseViewType;
    function GetViewTypeCount: Integer;
    procedure SetComboBox(Value: TComboBoxEx);
    procedure SetSelected(Value: TBaseViewType);
  public
    constructor Create;
    destructor Destroy; override;
    function ViewTypeByNodeContext(ANodeContext: TNodeContext): TBaseViewType;
    property ComboBox: TComboBoxEx read FComboBox write SetComboBox;
    property OnSearchTypeChange: TNotifyEvent read FOnSearchTypeChange write
        FOnSearchTypeChange;
    property Selected: TBaseViewType read GetSelected write SetSelected;
    property ViewType[Index: Integer]: TBaseViewType read GetViewType;
    property ViewTypeCount: Integer read GetViewTypeCount;
  end;
  
  TBrowserNode = class(TFlyNode, IAdditionalProperties)
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  private
    FDiscardOnCancel: Boolean;
    FDragDropKey: TKeyString;
    FKey: TKeyString;
    FRefCount: Integer;
    FHint: String;
    function GetParentKey: TKeyString;
    function GetParentNodeCaption: String;
    function GetParentNodeContext: TNodeContext;
    function GetProperty(const AName: string): Variant;
    function GetTopLevelNode: TTopLevelNode;
    function GetTopNodeCaption: String;
    function GetTopNodeContext: TNodeContext;
    function GetHint: String;
  protected
    function GetAssociatedFrame: TBaseDetailFrameClass; virtual;
    function GetCanAdd: Boolean; virtual;
    function GetCanDelete: Boolean; virtual;
    function GetCanEdit: Boolean; virtual;
    function GetImageIndex: Integer; virtual; abstract;
    function GetKeyList: TKeyList; virtual;
    function GetNodeContext: TNodeContext; virtual;
    function GetTableName: String; virtual;
    procedure InitialiseDragDropKey(ARecordset: _Recordset); virtual;
    function InternalGetProperty(const AName: String): Variant; virtual;
    procedure InternalRefresh; virtual; abstract;
    procedure InternalRefreshCaption; virtual; abstract;
    procedure SetDragDropKey(const Value: TKeyString); virtual;
    procedure SetKey(const AValue: TKeyString); virtual;
    function ViewTypeManager: TViewTypeManager;
  public
    destructor Destroy; override;
    class function ClassTableName: String; virtual; abstract;
    class function IsFileAcceptor(AFiles: TStringList): Boolean; virtual;
    class function GetCaption(const Original: String): String;
    procedure AddFiles(AFiles: TStringList); virtual;
    function CanAcceptFiles(AFiles: TStringList): Boolean; virtual;
    procedure Refresh;
    procedure RefreshCaption;
    property AssociatedFrame: TBaseDetailFrameClass read GetAssociatedFrame;
    property CanAdd: Boolean read GetCanAdd;
    property CanDelete: Boolean read GetCanDelete;
    property CanEdit: Boolean read GetCanEdit;
    property DiscardOnCancel: Boolean read FDiscardOnCancel write FDiscardOnCancel;
    property DragDropKey: TKeyString read FDragDropKey write SetDragDropKey;
    property Key: TKeyString read FKey write SetKey;
    property KeyList: TKeyList read GetKeyList;
    property NodeContext: TNodeContext read GetNodeContext;
    property ParentKey: TKeyString read GetParentKey;
    property ParentNodeCaption: String read GetParentNodeCaption;
    property ParentNodeContext: TNodeContext read GetParentNodeContext;
    property TableName: String read GetTableName;
    property TopLevelNode: TTopLevelNode read GetTopLevelNode;
    property TopNodeCaption: String read GetTopNodeCaption;
    property TopNodeContext: TNodeContext read GetTopNodeContext;
    property Hint: String read GetHint write FHint;
  end;
  
  TContainerNode = class(TBrowserNode, IAdditionalProperties)
  private
    FConnection: TADOConnection;
    FPopulated: Boolean;
    procedure SetPopulated(Value: Boolean);
  protected
    procedure AddChildNodesOfType(ABrowserNodeClass: TBrowserNodeClass); virtual; abstract;
    function GetAddButtonMenuCaption(Index: Integer): String; virtual;
    function GetAddButtonMenuCaptionsCount: Integer; virtual;
    function GetAddMenuIsAdd(AMenuIndex: Integer): Boolean; virtual;
    function GetAssociatedFrame: TBaseDetailFrameClass; override;
    function GetChildNodeType(Index: Integer): TBrowserNodeClass; virtual; abstract;
    function GetChildNodeTypeCount: Integer; virtual; abstract;
    function GetStoredProcParams: TVariantArray; virtual;
    procedure InternalRefresh; override;
    procedure PopulateFromDatabase(ALeafNodeClass: TLeafNodeClass; const AStoredProcName:
        String); virtual;
    procedure SetKey(const AValue: TKeystring); override;
    function StoredProcByChildType(ALeafNodeClass: TLeafNodeClass): String; virtual; abstract;
  public
    constructor Create(AOwner: TTreeCollection); override;
    function AddNode: TBrowserNode; overload; virtual; abstract;
    function AddNode(ApmAddMenuIndex : ShortInt): TBrowserNode; overload; virtual; abstract;
    function KeyDeleted(AChildLeafNode: TLeafNode): Boolean; virtual; abstract;
    procedure Populate; virtual;
    property AddButtonMenuCaption[Index: Integer]: String read GetAddButtonMenuCaption;
    property AddButtonMenuCaptionsCount: Integer read GetAddButtonMenuCaptionsCount;
    property AddMenuIsAdd[AMenuIndex: Integer]: Boolean read GetAddMenuIsAdd;
    function CanAcceptFiles(AFiles: TStringList): Boolean; override;
    property ChildNodeType[Index: Integer]: TBrowserNodeClass read GetChildNodeType;
    property ChildNodeTypeCount: Integer read GetChildNodeTypeCount;
    property Connection: TADOConnection read FConnection write FConnection;
    property Populated: Boolean read FPopulated write SetPopulated;
  end;

  {-----------------------------------------------------------------------------
    Validates newly added/linked/dropped nodes. Ensures that certain nodes cannot be linked to
    themselves recursively or otherwise.
  }
  TFolderNode = class(TContainerNode)
  private
    FFindDialogKey: TKeyString;
    FConfirmAppliesToAll: Boolean;
    FConfirmApplyToAllOption: Boolean;
    FNodeActionCancelled: Boolean;
    procedure SetConfirmAppliesToAll(value: Boolean);
    procedure SetConfirmApplyToAllOption(value: Boolean);
  protected
    procedure AddChildNodesOfType(ABrowserNodeClass: TBrowserNodeClass); override;
    function AddNode(ATree: TRapidTree; AMenuIndex: integer): TFlyNode;
    procedure EnforceSingleChildNode(const ANodeKey: String);
    function GetAssociatedFrame: TBaseDetailFrameClass; override;
    function GetCanAdd: Boolean; override;
    function GetCanDelete: Boolean; override;
    function GetCanEdit: Boolean; override;
    function GetImageIndex: Integer; override;
    function GetKeyList: TKeyList; override;
    function GetSearchType: SearchManager.TSearchType; virtual;
    procedure InternalInitialise(AKey: string); virtual;
    procedure SetCaption; virtual; abstract;
  public
    function ConfirmNodeAction(const AKey: String; const ACaption: String): Boolean; virtual;
    procedure DeleteLink(const AJoinTableKey: String); virtual; abstract;
    procedure Initialise(AKey: string);
    function KeyDeleted(AChildLeafNode: TLeafNode): Boolean; override;
    function LinkNode(const AKey: string; const ACaption: string): TBrowserNode; virtual;
    procedure ResetConfirmation;
    function UpdateNodeRelationship(const NewNodeKey: String): String; virtual;
    function ValidateNewNode(const AKey: TKeyString; const ACaption: string): Boolean; virtual;
    property ConfirmAppliesToAll: Boolean read FConfirmAppliesToAll write SetConfirmAppliesToAll;
    property ConfirmApplyToAllOption: Boolean read FConfirmApplyToAllOption write SetConfirmApplyToAllOption;
    property FindDialogKey: TKeyString read FFindDialogKey write FFindDialogKey;
    property NodeActionCancelled: Boolean read FNodeActionCancelled write FNodeActionCancelled;
    property SearchType: SearchManager.TSearchType read GetSearchType;
  end;
  
  TTopLevelNode = class(TContainerNode)
  private
    FDomainMask: LongWord;
    FDomainMaskSet: Boolean;
    function GetDomainMaskSet: Boolean;
    procedure SetDomainMask(Value: LongWord);
    procedure SetDomainMaskSet(Value: Boolean);
  protected
    function GetCanAdd: Boolean; override;
    function GetCanDelete: Boolean; override;
    function GetCanEdit: Boolean; override;
    function GetDomainMask: LongWord;
    function GetDomainStoredProc: String; virtual;
    function GetKeyList: TKeyList; override;
    procedure InternalInialiseNewNode; virtual;
    procedure InternalInitialise(ARecordset: _Recordset); virtual;
    procedure InternalRefresh; override;
    procedure InternalRefreshCaption; override;
    procedure SetCaption(ARecordset: _Recordset); virtual;
  public
    constructor Create(AOwner: TTreeCollection); override;
    function AddNode: TBrowserNode; overload; override;
    function AddNode(ApmAddMenuIndex : ShortInt): TBrowserNode; overload; override;
    procedure FindAndSetDragDropKey; virtual;
    procedure Initialise(ARecordset: _Recordset);
    procedure InitialiseNewNode;
    function KeyDeleted: Boolean; reintroduce; overload; virtual;
    function KeyDeleted(AChildLeafNode: TLeafNode): Boolean; overload; override;
    procedure SetSecurity;
    property DomainMask: LongWord read GetDomainMask write SetDomainMask;
    property DomainMaskSet: Boolean read GetDomainMaskSet write SetDomainMaskSet;
  end;
  
  TLeafNode = class(TBrowserNode)
  private
    FDomainMask: LongWord;
    FDomainMaskSet: Boolean;
    function GetDomainMaskSet: Boolean;
    procedure SetDomainMask(Value: LongWord);
    procedure SetDomainMaskSet(Value: Boolean);
  protected
    function GetCanAdd: Boolean; override;
    function GetCanDelete: Boolean; override;
    function GetCanEdit: Boolean; override;
    function GetDomainMask: LongWord;
    function GetDomainStoredProc: String; virtual;
    function GetKeyList: TKeyList; override;
    procedure InternalInitialise(ARecordset: _Recordset); virtual;
    procedure InternalInitialiseAddNode; virtual;
    procedure InternalInitialiseLinkNode(const AKey: string = ''; const ACaption: string ='');
        virtual;
    procedure InternalRefresh; override;
    procedure SetCaption(ARecordset: _Recordset); virtual;
  public
    constructor Create(AOwner: TTreeCollection); override;
    function CanRemoveDependents: Boolean; virtual;
    procedure FindAndSetDragDropKey; virtual;
    procedure Initialise(ARecordset: _Recordset);
    procedure InitialiseNewNode(const AKey: string = ''; const ACaption: string ='');
    procedure SetSecurity; virtual;
    property DomainMask: LongWord read GetDomainMask write SetDomainMask;
    property DomainMaskSet: Boolean read GetDomainMaskSet write SetDomainMaskSet;
  end;
  
  THyperlinkLeafNode = class(TLeafNode)
  private
    FHyperlinkKey: TKeyString;
    FJoinKey: String;
  protected
    procedure InitialiseHyperlinkKey(ARecordset: _Recordset); virtual;
    procedure InternalInitialise(ARecordset: _Recordset); override;
    procedure InternalInitialiseAddNode; override;
    procedure InternalInitialiseLinkNode(const AKey: string = ''; const ACaption: string ='');
        override;
  public
    procedure FindAndSetHyperlinkKey; virtual;
    property HyperlinkKey: TKeyString read FHyperlinkKey write FHyperlinkKey;
    property JoinKey: String read FJoinKey write FJoinKey;
  end;
  
  ESecurityError = class(TExceptionPath)
  end;
  
  EBrowserNodeError = class(TExceptionPath)
  end;
  
  EAddMenuItemError = class(TExceptionPath)
  end;
  
  EBrowserNodeContextError = class(TExceptionPath)
  end;
  
  EDuplicateNodeError = class(TExceptionPath)
  end;
  
  THyperlinkTopLevelNode = class(TTopLevelNode)
  private
    FDblClickNavigates: Boolean;
    FFindDialogKey: TKeyString;
    FHyperlinkKey: TKeyString;
    FJoinKey: String;
  protected
    function GetSearchType: SearchManager.TSearchType; virtual;
    procedure InitialiseHyperlinkKey(ARecordset: _Recordset); virtual;
    procedure InternalInitialise(ARecordset: _Recordset); override;
  public
    procedure FindAndSetHyperlinkKey; virtual;
    function LinkNode(const AKey: string; const ACaption: string): TBrowserNode; virtual;
    function UpdateNodeRelationship(const NewNodeKey: String): String; virtual;
    function ValidateNewNode(const AKey: TKeyString; const ACaption: string): Boolean; virtual;
    property DblClickNavigates: Boolean read FDblClickNavigates write FDblClickNavigates;
    property FindDialogKey: TKeyString read FFindDialogKey write FFindDialogKey;
    property HyperlinkKey: TKeyString read FHyperlinkKey write FHyperlinkKey;
    property JoinKey: String read FJoinKey write FJoinKey;
    property SearchType: SearchManager.TSearchType read GetSearchType;
  end;
  
//==============================================================================
implementation

uses
  Variants, FrameListViewer, BrowserViewTypes, ApplicationSettings, UserMessages,
  FrameCBNavigation, GeneralFunctions;

{-==============================================================================
    TBrowserNode
===============================================================================}
{-------------------------------------------------------------------------------
}
destructor TBrowserNode.Destroy;
begin
  Assert(FRefCount=0,
      'Node destroyed when reference still exist to interfaces - '
      + IntToStr(FRefCount) + ' references to ' + Classname + '.' + Text);
  
  inherited Destroy;
end;  // TBrowserNode.Destroy 

{-------------------------------------------------------------------------------
  Add files to this node - to be overridden in relevant nodes
}
procedure TBrowserNode.AddFiles(AFiles: TStringList);
begin
  // No implementation required at this level.
end;

{-------------------------------------------------------------------------------
  Can this node accept files?
}
function TBrowserNode.CanAcceptFiles(AFiles: TStringList): Boolean;
begin
  // This is default - will be overridden when relevant
  Result := False;
end;

{-------------------------------------------------------------------------------
}
function TBrowserNode.GetAssociatedFrame: TBaseDetailFrameClass;
begin
  Result := TBaseDetailFrame;
  { TODO : Make this method abstract, here just to remove errors. }
end;  // TBrowserNode.GetAssociatedFrame 

{-------------------------------------------------------------------------------
}
function TBrowserNode.GetCanAdd: Boolean;
begin
  Result := True;
end;  // TBrowserNode.GetCanAdd 

{-------------------------------------------------------------------------------
}
function TBrowserNode.GetCanDelete: Boolean;
begin
  Result := True;
end;  // TBrowserNode.GetCanDelete 

{-------------------------------------------------------------------------------
}
function TBrowserNode.GetCanEdit: Boolean;
begin
  Result := True;
end;  // TBrowserNode.GetCanEdit 

{-------------------------------------------------------------------------------
  Accessor method.  Returns a keylist for the node.  For folder nodes, returns a keylist of
      the child nodes.  For top level nodes, returns just the one top level item.
}
function TBrowserNode.GetKeyList: TKeyList;
begin
  // Return an empty key list by default
  Result := TEditableKeylist.Create;
end;  // TBrowserNode.GetKeyList 

{-------------------------------------------------------------------------------
}
function TBrowserNode.GetNodeContext: TNodeContext;
begin
  Result := ncNone;
end;  // TBrowserNode.GetNodeContext 

{-------------------------------------------------------------------------------
}
function TBrowserNode.GetParentKey: TKeyString;
var
  lCurrentNode: TFlyNode;
begin
  lCurrentNode := self;
  while Assigned(lCurrentNode.Parent) do
    lCurrentNode := lCurrentNode.Parent;
  Result := TBrowserNode(lCurrentNode).Key;
end;  // TBrowserNode.GetParentKey 

{-------------------------------------------------------------------------------
}
function TBrowserNode.GetParentNodeCaption: String;
begin
  if Assigned(Parent) then Result := Parent.Caption
                      else Result := '';
end;  // TBrowserNode.GetParentNodeCaption 

{-------------------------------------------------------------------------------
}
function TBrowserNode.GetParentNodeContext: TNodeContext;
var
  lNode: TBrowserNode;
begin
  lNode := Self;
  
  // Find a NodeContext that is not ncNode, stop at the first parent node that has one.
  while Assigned(lNode.Parent) do
    if (lNode.Parent is TBrowserNode) then begin
      lNode := TBrowserNode(lNode.Parent);
      // Stop when one is found.
      if lNode.NodeContext <> ncNone then Break;
    end else
      raise EBrowserNodeError.Create(Format(ResStr_ParentWrongType, [Self.ClassName]));
  
  // Either return the correct context (through overridden accessor) or ncNone.
  Result := lNode.NodeContext;
end;  // TBrowserNode.GetParentNodeContext 

{-------------------------------------------------------------------------------
  Retrieve additional properties.  By default this includes the Key. 
}
function TBrowserNode.GetProperty(const AName: string): Variant;
begin
  Result := InternalGetProperty(AName);
end;  // TBrowserNode.GetProperty 

{-------------------------------------------------------------------------------
  Retrieve the associated table name.  In most cases, we can use the class method
      ClassTableName since the table name is fixed for the class.  However, in some cases this
      is not possible since the table name is context sensitive, so this method must be
      overriden.
}
function TBrowserNode.GetTableName: String;
begin
  Result := ClassTableName;
end;  // TBrowserNode.GetTableName 

{-------------------------------------------------------------------------------
  This method will get the Top Level node for the node we are currently dealing with (i.e. not
      necessarily the node that is selected on the tree).
}
function TBrowserNode.GetTopLevelNode: TTopLevelNode;
var
  lCurrentNode: TFlyNode;
begin
  lCurrentNode := Self;
  
  while Assigned(lCurrentNode.Parent) do
    lCurrentNode := lCurrentNode.Parent;
  Result := TTopLevelNode(lCurrentNode);
end;  // TBrowserNode.GetTopLevelNode 

{-------------------------------------------------------------------------------
}
function TBrowserNode.GetTopNodeCaption: String;
var
  lTopNode: TFlyNode;
begin
  lTopNode := TopLevelNode;
  if Assigned(lTopNode) then
    Result := lTopNode.Caption
  else
    Result := '';
end;  // TBrowserNode.GetTopNodeCaption 

{-------------------------------------------------------------------------------
}
function TBrowserNode.GetTopNodeContext: TNodeContext;
var
  lTopNode: TFlyNode;
begin
  lTopNode := TopLevelNode;
  if Assigned(lTopNode) then
    Result := TBrowserNode(lTopNode).NodeContext
  else
    Result := ncNone;
end;  // TBrowserNode.GetTopNodeContext

{-------------------------------------------------------------------------------
}
function TBrowserNode.GetHint: String;
begin
  Result := FHint;
end;  // TBrowserNode.GetHint

{-------------------------------------------------------------------------------
}
procedure TBrowserNode.InitialiseDragDropKey(ARecordset: _Recordset);
begin
  DragDropKey := Key;
end;  // TBrowserNode.InitialiseDragDropKey 

{-------------------------------------------------------------------------------
  Retrieve additional properties.  By default this includes the Key. 
}
function TBrowserNode.InternalGetProperty(const AName: String): Variant;
begin
  if AName = PROP_KEY then
    Result := FKey
  else if AName = PROP_DRAG_DROP_KEY then
    Result := DragDropKey
  else if AName = PROP_PARENT_KEY then
    Result := ParentKey
  else if AName = PROP_TABLE_NAME then
    Result := TableName
  else if AName = PROP_NODE_CAPTION then
    Result := Caption
  else if AName = PROP_NODE_CONTEXT then
    Result := NodeContext
  else if AName = PROP_PARENT_NODE_CAPTION then
    Result := ParentNodeCaption
  else if AName = PROP_PARENT_NODE_CONTEXT then
    Result := ParentNodeContext
  else if AName = PROP_TOP_NODE_CAPTION then
    Result := TopNodeCaption
  else if AName = PROP_TOP_NODE_CONTEXT then
    Result := TopNodeContext
  else if AName = PROP_DISCARD_ON_CANCEL then
    Result := FDiscardOnCancel
  else
    Result := Unassigned;
end;  // TBrowserNode.InternalGetProperty 

{-------------------------------------------------------------------------------
}
class function TBrowserNode.IsFileAcceptor(AFiles: TStringList): Boolean;
begin
  Result := False;
end;

{-------------------------------------------------------------------------------
}
function TBrowserNode.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if GetInterface(IID, Obj) then
    Result := 0
  else
    Result := E_NOINTERFACE;
end;  // TBrowserNode.QueryInterface 

{-------------------------------------------------------------------------------
}
procedure TBrowserNode.Refresh;
var
  lExpanded: Boolean;
begin
  Tree.Items.BeginUpdate;
  lExpanded := Expanded;
  try
    InternalRefresh;
  finally
    Tree.Items.EndUpdate;
  end;
  // reset original state
  if lExpanded then
    Expand(False);
end;  // TBrowserNode.Refresh

{-------------------------------------------------------------------------------
}
procedure TBrowserNode.RefreshCaption;
begin
  InternalRefreshCaption;
end;  // TBrowserNode.RefreshCaption

{-------------------------------------------------------------------------------
}
procedure TBrowserNode.SetDragDropKey(const Value: TKeyString);
begin
  FDragDropKey := Value;
end;  // TBrowserNode.SetDragDropKey 

{-------------------------------------------------------------------------------
}
procedure TBrowserNode.SetKey(const AValue: TKeyString);
begin
  FKey := AValue;
end;  // TBrowserNode.SetKey 

{-------------------------------------------------------------------------------
}
function TBrowserNode.ViewTypeManager: TViewTypeManager;
var
  lParent: TWinControl;
begin
  lParent := nil;
  if Assigned(Owner.Tree.Parent) then lParent := TWinControl(Owner.Tree.Parent);
  while not (lParent is TfraCBNavigation) do
    if Assigned(TWinControl(lParent).Parent) then
      lParent := TWinControl(lParent).Parent
    else
      Raise EBrowserNodeError.Create(Format(ResStr_ParentWrongType, ['fraCBNavigation']));
  
  Result := TfraCBNavigation(lParent).ViewTypeManager;
end;  // TBrowserNode.ViewTypeManager 

{-------------------------------------------------------------------------------
}
function TBrowserNode._AddRef: Integer;
begin
  //Result := InterlockedIncrement(FRefCount);
  Inc(FRefCount);
  Result := -1; //Don't worry about reference counting
end;  // TBrowserNode._AddRef 

{-------------------------------------------------------------------------------
}
function TBrowserNode._Release: Integer;
begin
  //Result := InterlockedDecrement(FRefCount);
  Result := -1; //Don't worry about reference counting
  Dec(FRefCount);
  // The tree nodes do not garbage collect as we use them as normal Delphi instances
end;  // TBrowserNode._Release 

{-==============================================================================
    TContainerNode
===============================================================================}
{-------------------------------------------------------------------------------
}
constructor TContainerNode.Create(AOwner: TTreeCollection);
begin
  inherited Create(aOwner);
end;  // TContainerNode.Create 

{-------------------------------------------------------------------------------
  Returns true if either this node or any children can accept the dropped files
}
function TContainerNode.CanAcceptFiles(AFiles: TStringList): Boolean;
var
  i: integer;
begin
  Result := IsFileAcceptor(AFiles);
  if not Result then
    for i := 0 to Pred(ChildNodeTypeCount) do
      if ChildNodeType[i].IsFileAcceptor(AFiles) then begin
        Result := True;
        Break;
      end;    // if ChildNodeType[i].IsFileAcceptor
end;

{-------------------------------------------------------------------------------
  Override this method in any children that have an AddButtonMenu. 
}
function TContainerNode.GetAddButtonMenuCaption(Index: Integer): String;
begin
  //Default to no stored proc.
  Result := '';
end;  // TContainerNode.GetAddButtonMenuCaption 

{-------------------------------------------------------------------------------
}
function TContainerNode.GetAddButtonMenuCaptionsCount: Integer;
begin
  Result := 0;
end;  // TContainerNode.GetAddButtonMenuCaptionsCount 

{-------------------------------------------------------------------------------
  Override this in descendants that have add menus. 
}
function TContainerNode.GetAddMenuIsAdd(AMenuIndex: Integer): Boolean;
begin
  Result := False;
end;  // TContainerNode.GetAddMenuIsAdd 

{-------------------------------------------------------------------------------
}
function TContainerNode.GetAssociatedFrame: TBaseDetailFrameClass;
begin
  Result := TfraListViewer;
end;  // TContainerNode.GetAssociatedFrame 

{-------------------------------------------------------------------------------
}
function TContainerNode.GetStoredProcParams: TVariantArray;
var
  SortIndex: Integer;
begin
  try
    if NodeContext in [ncCollection, ncSpecimen, ncStore, ncEnquiry,
        ncValuation,
                  ncAccession, ncLoan, ncMovement, ncConditionCheck, ncJob,
                  ncTask] then begin
  
      SortIndex := ViewTypeManager.ViewTypeByNodeContext(
          NodeContext).SortOrderDefaultIndex;
      Result := VarArrayOf(['@ParentKey', Key, '@SortOrderIndex', SortIndex]);
    end
    else
      Result := VarArrayOf(['@ParentKey', Key]);
  except on EInvalidViewTypeException do
    Result := VarArrayOf(['@ParentKey', Key]);
  end;
end;  // TContainerNode.GetStoredProcParams 

{-------------------------------------------------------------------------------
}
procedure TContainerNode.InternalRefresh;
begin
  DeleteChildren;
  Populated := False;
  Populate;
end;  // TContainerNode.InternalRefresh 

{-------------------------------------------------------------------------------
}
procedure TContainerNode.Populate;
var
  i: Integer;
  lCursor: TCursor;
begin
  if Populated then Exit;
  
  lCursor := HourglassCursor;
  try
    for i := 0 to ChildNodeTypeCount-1 do
      try
        AddChildNodesOfType(ChildNodeType[i]);
      except
        on ESecurityError do;  // drop any nodes that user not allowed to see
      end; // try
  
    Populated := True;
    HasChildren := Count > 0;
  finally
    DefaultCursor(lCursor);
  end;
end;  // TContainerNode.Populate 

{-------------------------------------------------------------------------------
}
procedure TContainerNode.PopulateFromDatabase(ALeafNodeClass: TLeafNodeClass; const
    AStoredProcName: String);
var
  lRecordset: _Recordset;
  lNewNode: TLeafNode;
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
        lRecordset.MoveNext;
      end;
    end;
  finally
    Tree.Items.EndUpdate;
    lRecordset.Close;
  end;
end;  // TContainerNode.PopulateFromDatabase 

{-------------------------------------------------------------------------------
}
procedure TContainerNode.SetKey(const AValue: TKeystring);
begin
  inherited SetKey(AValue);
  HasChildren := True;
end;  // TContainerNode.SetKey 

{-------------------------------------------------------------------------------
}
procedure TContainerNode.SetPopulated(Value: Boolean);
begin
  FPopulated := Value;
end;  // TContainerNode.SetPopulated 

{-==============================================================================
    TFolderNode
===============================================================================}
{-------------------------------------------------------------------------------
}
procedure TFolderNode.AddChildNodesOfType(ABrowserNodeClass: TBrowserNodeClass);
begin
  if ABrowserNodeClass.InheritsFrom(TLeafNode) then
    PopulateFromDatabase(TLeafNodeClass(ABrowserNodeClass),
                         StoredProcByChildType(TLeafNodeClass(ABrowserNodeClass)))
  else
    raise EBrowserNodeError.Create(ResStr_CannotPopulateNonLeafNodeFromDB);
end;  // TFolderNode.AddChildNodesOfType 

{-------------------------------------------------------------------------------
  Adds a node to the treeview of the required type. 
}
function TFolderNode.AddNode(ATree: TRapidTree; AMenuIndex: integer): TFlyNode;
begin
  Result := ATree.Items.AddTypedChild(Self, GetChildNodeType(0));
  TLeafNode(Result).InitialiseNewNode;
end;  // TFolderNode.AddNode 

{-------------------------------------------------------------------------------
}
procedure TFolderNode.EnforceSingleChildNode(const ANodeKey: String);
var
  i: Integer;
  lEvent1: TFTVChangedEvent;
  lEvent2: TFTVChangingEvent;
begin
  // Store the OnChange and OnChanging events and then unlink them. Deleting
  // the nodes we don't want would normally causes these events to be fired,
  // which will cause FrameContainerUnit to be destroyed, and as it is this
  // frame that is calling this method, we would get an access violation.
  lEvent1 := Tree.OnChange;
  lEvent2 := Tree.OnChanging;
  Tree.OnChange := nil;
  Tree.OnChanging := nil;
  try
    // Folder can only have one child, so drop the other
    for i := Count - 1 downto 0 do
      if Item[i] is TLeafNode then
        if TLeafNode(Item[i]).Key <> ANodeKey then
          Tree.Items.Delete(Item[i]);
  finally
    // Now relink the events.
    Tree.OnChange := lEvent1;
    Tree.OnChanging := lEvent2;
  end;
end;  // TFolderNode.EnforceSingleChildNode 

{-------------------------------------------------------------------------------
}
function TFolderNode.GetAssociatedFrame: TBaseDetailFrameClass;
begin
  Result := TfraListViewer;
end;  // TFolderNode.GetAssociatedFrame 

{-------------------------------------------------------------------------------
}
function TFolderNode.GetCanAdd: Boolean;
begin
  //Allow Add if user is allowed to add anything in the system
  Result := (AppSettings.AddDomainMask > 0) and
            (TUserAccessLevel(AppSettings.UserAccessLevel) in
             [ualAddOnly, ualFullUser, ualAdmin]);
end;  // TFolderNode.GetCanAdd 

{-------------------------------------------------------------------------------
}
function TFolderNode.GetCanDelete: Boolean;
begin
  Result := False;
end;  // TFolderNode.GetCanDelete 

{-------------------------------------------------------------------------------
}
function TFolderNode.GetCanEdit: Boolean;
begin
  Result := False;
end;  // TFolderNode.GetCanEdit 

{-------------------------------------------------------------------------------
}
function TFolderNode.GetImageIndex: Integer;
begin
  Result := 1;
end;  // TFolderNode.GetImageIndex 

{-------------------------------------------------------------------------------
}
function TFolderNode.GetKeyList: TKeyList;
var
  liChildIndex: Integer;
begin
  Result := inherited GetKeyList;
  
  TEditableKeyList(Result).SetTable(TableName);
  
  if CompareText(TableName, TN_MIXED_DATA) = 0 then
    for liChildIndex := 0 to Count -1 do
      TEditableKeyList(Result).AddItem(TBrowserNode(Item[liChildIndex]).DragDropKey,
                                       TBrowserNode(Item[liChildIndex]).TableName)
  else
    for liChildIndex := 0 to Count -1 do
      // Add the key, plus a pointer to the node so that it can be relocated
      // after a drop operation
      TEditableKeyList(Result).AddItem(TBrowserNode(Item[liChildIndex]).DragDropKey,
                                       IntToStr(Integer(Item[liChildIndex])));
end;  // TFolderNode.GetKeyList

{-------------------------------------------------------------------------------
}
function TFolderNode.GetSearchType: SearchManager.TSearchType;
begin
  Result := SearchManager.stNone;
end;  // TFolderNode.GetSearchType

{-------------------------------------------------------------------------------
}
procedure TFolderNode.Initialise(AKey: string);
begin
  InternalInitialise(AKey);
  Populate;
end;  // TFolderNode.Initialise 

{-------------------------------------------------------------------------------
}
function TFolderNode.ConfirmNodeAction(const AKey: String; const ACaption: String): Boolean;
begin
  Result := True;
end;  // TFolderNode.ConfirmNodeAction

{-------------------------------------------------------------------------------
}
procedure TFolderNode.InternalInitialise(AKey: string);
begin
  Populated := False;
  Key := AKey;
  DragDropKey := '';
  ImageIndex := GetImageIndex;
  SelectedIndex := ImageIndex;
  SetCaption;
end;  // TFolderNode.InternalInitialise

{-------------------------------------------------------------------------------
}
function TFolderNode.KeyDeleted(AChildLeafNode: TLeafNode): Boolean;
var
  lRecordset: _Recordset;
  lParams: TVariantArray;
begin
  lParams := GetStoredProcParams;
  SetLength(lParams, Length(lParams) + 2);
  lParams[High(lParams) -1] := '@Key';
  lParams[High(lParams)] := AChildLeafNode.Key;
  lRecordset := dmGeneral.GetRecordset(
                  StoredProcByChildType(TLeafNodeClass(AChildLeafNode.ClassType)), lParams);
  lRecordset.Filter := 'Item_Key = ''' + AChildLeafNode.Key + '''';
  
  try
    if lRecordset.RecordCount > 0 then begin
      Result := false;
      lRecordSet.MoveFirst;
      AChildLeafNode.SetCaption(lRecordset);
    end else
      Result := true;
  finally
    lRecordset.Close;
  end;
end;  // TFolderNode.KeyDeleted 

{-------------------------------------------------------------------------------
}
function TFolderNode.LinkNode(const AKey: string; const ACaption: string): TBrowserNode;
var
  lCheckIdx: Integer;
  lDuplicate: Boolean;
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
    TLeafNode(Result).InitialiseNewNode(AKey, ACaption);
  except
    on EBrowserNodeError do begin
      Result.Free;
      raise;
    end;
  end;
end;  // TFolderNode.LinkNode 

{-------------------------------------------------------------------------------
}
procedure TFolderNode.ResetConfirmation;
begin
  ConfirmAppliesToAll := False;
  NodeActionCancelled := False;
end;  // TFolderNode.ResetConfirmation

{-------------------------------------------------------------------------------
}
procedure TFolderNode.SetConfirmAppliesToAll(value: Boolean);
begin
  FConfirmAppliesToAll := value;
end;  // TFolderNode.SetConfirmAppliesToAll

{-------------------------------------------------------------------------------
}
procedure TFolderNode.SetConfirmApplyToAllOption(value: Boolean);
begin
  FConfirmApplyToAllOption := value;
  FConfirmAppliesToAll     := False;  // always reset, makes life easier.
end;  // TFolderNode.SetConfirmApplyToAllOption

{-------------------------------------------------------------------------------
}
function TFolderNode.UpdateNodeRelationship(const NewNodeKey: String): String;
begin
end;  // TFolderNode.UpdateNodeRelationship 

{-------------------------------------------------------------------------------
}
function TFolderNode.ValidateNewNode(const AKey: TKeyString; const ACaption: string): Boolean;
begin
  //Most nodes don't need validating before they are dropped.
  Result := true;
end;  // TFolderNode.ValidateNewNode 

{-==============================================================================
    TTopLevelNode
===============================================================================}
{-------------------------------------------------------------------------------
}
constructor TTopLevelNode.Create(AOwner: TTreeCollection);
begin
  inherited Create(AOwner);
  
  FDomainMaskSet := false;
end;  // TTopLevelNode.Create 

{-------------------------------------------------------------------------------
}
function TTopLevelNode.AddNode: TBrowserNode;
var
  lTopLevelNode: TTopLevelNode;
  lFlyNode: TFlyNode;
begin
  lFlyNode := Tree.Items.AddTypedChild(nil, TFlyNodeClass(self.classtype));
  //GetChildNodeType can only return TBrowserNodeClass
  //Added class will also be a TTopLevelNode
  lTopLevelNode := TTopLevelNode(lFlyNode);
  lTopLevelNode.InitialiseNewNode;
  Result := lTopLevelNode;
end;  // TTopLevelNode.AddNode 

{-------------------------------------------------------------------------------
}
function TTopLevelNode.AddNode(ApmAddMenuIndex : ShortInt): TBrowserNode;
var
  lTopLevelNode: TTopLevelNode;
  lFlyNode: TFlyNode;
begin
  lFlyNode := Tree.Items.AddTypedChild(nil, TFlyNodeClass(self.classtype));
  //GetChildNodeType can only return TBrowserNodeClass
  //Added class will also be a TTopLevelNode
  lTopLevelNode := TTopLevelNode(lFlyNode);
  lTopLevelNode.InitialiseNewNode;
  //ShowMessage('Here in a top level node');
  Result := lTopLevelNode;
end;  // TTopLevelNode.AddNode 

{-------------------------------------------------------------------------------
}
procedure TTopLevelNode.FindAndSetDragDropKey;
begin
  DragDropKey := Key;
end;  // TTopLevelNode.FindAndSetDragDropKey 

{-------------------------------------------------------------------------------
}
function TTopLevelNode.GetCanAdd: Boolean;
begin
  //Allow Add if user is allowed to add anything in the system
  Result := (AppSettings.AddDomainMask > 0) and
            (TUserAccessLevel(AppSettings.UserAccessLevel) in
                [ualAddOnly, ualFullUser, ualAdmin]);
end;  // TTopLevelNode.GetCanAdd 

{-------------------------------------------------------------------------------
}
function TTopLevelNode.GetCanDelete: Boolean;
begin
  Result := (AppSettings.AllowEdit[GetDomainMask] or (GetDomainMask = 0)) and
            (TUserAccessLevel(AppSettings.UserAccessLevel) in
              [ualFullUser, ualAdmin]);
end;  // TTopLevelNode.GetCanDelete 

{-------------------------------------------------------------------------------
}
function TTopLevelNode.GetCanEdit: Boolean;
begin
  Result := (AppSettings.AllowEdit[GetDomainMask] or (GetDomainMask = 0)) and
             (TUserAccessLevel(AppSettings.UserAccessLevel) in [ualFullUser,
                 ualAdmin]);
end;  // TTopLevelNode.GetCanEdit 

{-------------------------------------------------------------------------------
}
function TTopLevelNode.GetDomainMask: LongWord;
begin
  if not FDomainMaskSet then SetSecurity;
  Result := FDomainMask;
end;  // TTopLevelNode.GetDomainMask 

{-------------------------------------------------------------------------------
}
function TTopLevelNode.GetDomainMaskSet: Boolean;
begin
  Result := FDomainMaskSet;
end;  // TTopLevelNode.GetDomainMaskSet 

{-------------------------------------------------------------------------------
}
function TTopLevelNode.GetDomainStoredProc: String;
begin
  Result := '';
end;  // TTopLevelNode.GetDomainStoredProc

{-------------------------------------------------------------------------------
}
function TTopLevelNode.GetKeyList: TKeyList;
begin
  Result := inherited GetKeyList;
  
  TEditableKeyList(Result).SetTable(TableName);
  // Add key, plus a pointer to self so it can be refreshed after a drag operation
  TEditableKeyList(Result).AddItem(DragDropKey, IntToStr(Integer(Self)));
end;  // TTopLevelNode.GetKeyList 

{-------------------------------------------------------------------------------
}
procedure TTopLevelNode.Initialise(ARecordset: _Recordset);
begin
  InternalInitialise(ARecordset);
end;  // TTopLevelNode.Initialise 

{-------------------------------------------------------------------------------
}
procedure TTopLevelNode.InitialiseNewNode;
begin
  InternalInialiseNewNode;
end;  // TTopLevelNode.InitialiseNewNode 

{-------------------------------------------------------------------------------
}
procedure TTopLevelNode.InternalInialiseNewNode;
begin
  Populated := False;
  Key:= '';
  DragDropKey := '';
  ImageIndex := GetImageIndex;
  SelectedIndex := ImageIndex;
  Caption := ResStr_NewNode;
  //Not set yet. Allows Edit and Delete until the DomainMask is set in the save event.
  DomainMask := 0;
end;  // TTopLevelNode.InternalInialiseNewNode 

{-------------------------------------------------------------------------------
}
procedure TTopLevelNode.InternalInitialise(ARecordset: _Recordset);
begin
  Populated := False;
  Key := ARecordset.Fields['Item_Key'].Value;
  DragDropKey := Key;
  InitialiseDragDropKey(ARecordset);
  ImageIndex := GetImageIndex;
  SelectedIndex := ImageIndex;
  SetCaption(ARecordset);
end;  // TTopLevelNode.InternalInitialise 

{-------------------------------------------------------------------------------
}
procedure TTopLevelNode.InternalRefresh;
begin
  if KeyDeleted then begin
    MessageDlg(ResStr_BrowserNodeDeletedByOtherUser, mtInformation, [mbOK], 0);
    PostMessage(TWinControl(Tree.Owner).Handle, WM_REMOVE_DELETED_NODE, Integer(Self), 0);
  end else
    inherited InternalRefresh;
  //else
    //refresh rhs content.
end;  // TTopLevelNode.InternalRefresh

{-------------------------------------------------------------------------------
}
procedure TTopLevelNode.InternalRefreshCaption;
begin
  if KeyDeleted then begin
    MessageDlg(ResStr_BrowserNodeDeletedByOtherUser, mtInformation, [mbOK], 0);
    PostMessage(TWinControl(Tree.Owner).Handle, WM_REMOVE_DELETED_NODE, Integer(Self), 0);
  end;
end;  // TTopLevelNode.InternalRefreshCaption

{-------------------------------------------------------------------------------
}
function TTopLevelNode.KeyDeleted: Boolean;
var
  lRecordset: _Recordset;
  lParams: TVariantArray;
begin
  with ViewTypeManager.Selected do begin
    lParams := StoredProcParams;
    SetLength(lParams, Length(lParams) + 2);
    lParams[High(lParams) -1] := '@Key';
    lParams[High(lParams)] := Key;
    lRecordset := dmGeneral.GetRecordset(PopulateTopLevelStoredProcName,
        lParams);
  end;
  
  try
    if lRecordset.RecordCount > 0 then begin
      Result := false;
      lRecordSet.MoveFirst;
      SetCaption(lRecordset);
    end
    else
      Result := true;
  finally
    lRecordset.Close;
  end;
end;  // TTopLevelNode.KeyDeleted 

{-------------------------------------------------------------------------------
}
function TTopLevelNode.KeyDeleted(AChildLeafNode: TLeafNode): Boolean;
var
  lRecordset: _Recordset;
  lParams: TVariantArray;
begin
  lParams := GetStoredProcParams;
  SetLength(lParams, Length(lParams) + 2);
  lParams[High(lParams) -1] := '@Key';
  lParams[High(lParams)] := AChildLeafNode.Key;
  lRecordset := dmGeneral.GetRecordset(
      StoredProcByChildType(TLeafNodeClass(AChildLeafNode)), lParams);
  
  try
    if lRecordset.RecordCount > 0 then begin
      Result := false;
      lRecordSet.MoveFirst;
      SetCaption(lRecordset);
    end
    else
      Result := true;
  finally
    lRecordset.Close;
  end;
end;  // TTopLevelNode.KeyDeleted 

{-------------------------------------------------------------------------------
}
procedure TTopLevelNode.SetCaption(ARecordset: _Recordset);
begin
  Caption := TBrowserNode.GetCaption(ARecordset.Fields['Item_Name'].Value);
end;  // TTopLevelNode.SetCaption

{-------------------------------------------------------------------------------
}
procedure TTopLevelNode.SetDomainMask(Value: LongWord);
begin
  FDomainMask := Value;
end;  // TTopLevelNode.SetDomainMask 

{-------------------------------------------------------------------------------
}
procedure TTopLevelNode.SetDomainMaskSet(Value: Boolean);
begin
  FDomainMaskSet := Value;
end;  // TTopLevelNode.SetDomainMaskSet 

{-------------------------------------------------------------------------------
}
procedure TTopLevelNode.SetSecurity;
var
  lDomainMask: Variant;
begin
  FDomainMaskSet := true;
  
  if GetDomainStoredProc <> '' then begin
    lDomainMask := dmGeneral.GetStoredProcOutputParam(GetDomainStoredProc,
        ['@Key', Key], '@DomainMask');
    if not (lDomainMask = NULL) then
      FDomainMask := integer(lDomainMask)
    else begin
      MessageDlg(ResStr_BrowserNodeDeletedByOtherUser, mtInformation, [mbOK],
          0);
      PostMessage(TWinControl(Tree.Owner).Handle, WM_REMOVE_DELETED_NODE,
                      integer(self), 0);
    end;
      //raise ESecurityError.Create(ResStr_ErrorResolvingDomainMask);
  end;
end;  // TTopLevelNode.SetSecurity 

{-==============================================================================
    TLeafNode
===============================================================================}
{-------------------------------------------------------------------------------
}
constructor TLeafNode.Create(AOwner: TTreeCollection);
begin
  inherited Create(aOwner);
  
  FDomainMaskSet := false;
end;  // TLeafNode.Create 

{-------------------------------------------------------------------------------
}
function TLeafNode.CanRemoveDependents: Boolean;
begin
  Result := false;
end;  // TLeafNode.CanRemoveDependents 

{-------------------------------------------------------------------------------
}
procedure TLeafNode.FindAndSetDragDropKey;
begin
  DragDropKey := Key;
end;  // TLeafNode.FindAndSetDragDropKey 

{-------------------------------------------------------------------------------
}
function TLeafNode.GetCanAdd: Boolean;
begin
  if Assigned(Parent) then
    if Parent is TFolderNode then
      Result := TFolderNode(Parent).CanAdd
    else
      Result := false
  else
    Result := false;
end;  // TLeafNode.GetCanAdd 

{-------------------------------------------------------------------------------
}
function TLeafNode.GetCanDelete: Boolean;
begin
  Result := (AppSettings.AllowEdit[GetDomainMask] or (GetDomainMask = 0)) and
            (TUserAccessLevel(AppSettings.UserAccessLevel) in [ualFullUser, ualAdmin]);
end;  // TLeafNode.GetCanDelete 

{-------------------------------------------------------------------------------
}
function TLeafNode.GetCanEdit: Boolean;
var
  lIsCollectionOrStore: Boolean;
begin
  lIsCollectionOrStore := (NodeContext = ncCollection) or (NodeContext = ncStore);
  
  Result := (AppSettings.AllowEdit[GetDomainMask] or (GetDomainMask = 0)) and
            (TUserAccessLevel(AppSettings.UserAccessLevel) in [ualFullUser, ualAdmin])
            // Disable editing if this is a collection or store leaf node
            // with a specimen top level node
            and (not lIsCollectionOrStore
                or (lIsCollectionOrStore and (TopNodeContext <> ncSpecimen)));
end;  // TLeafNode.GetCanEdit

{-------------------------------------------------------------------------------
}
function TLeafNode.GetDomainMask: LongWord;
begin
  if not FDomainMaskSet then SetSecurity;
  Result := FDomainMask;
end;  // TLeafNode.GetDomainMask 

{-------------------------------------------------------------------------------
}
function TLeafNode.GetDomainMaskSet: Boolean;
begin
  Result := FDomainMaskSet;
end;  // TLeafNode.GetDomainMaskSet 

{-------------------------------------------------------------------------------
}
function TLeafNode.GetDomainStoredProc: String;
begin
  Result := '';
end;  // TLeafNode.GetDomainStoredProc 

{-------------------------------------------------------------------------------
}
function TLeafNode.GetKeyList: TKeyList;
begin
  Result := inherited GetKeyList;
  
  TEditableKeyList(Result).SetTable(TableName);
  // Add key, plus a pointer to self so it can be refreshed after a drag operation
  TEditableKeyList(Result).AddItem(DragDropKey, IntToStr(Integer(Self)));
end;  // TLeafNode.GetKeyList 

{-------------------------------------------------------------------------------
}
procedure TLeafNode.Initialise(ARecordset: _Recordset);
begin
  InternalInitialise(ARecordset);
end;  // TLeafNode.Initialise 

{-------------------------------------------------------------------------------
}
procedure TLeafNode.InitialiseNewNode(const AKey: string = ''; const ACaption: string ='');
begin
  if AKey = '' then
    InternalInitialiseAddNode //Only happens when adding new record/node
  else
    //Only happens when linking to an existing record/node
    InternalInitialiseLinkNode(AKey, ACaption);
end;  // TLeafNode.InitialiseNewNode 

{-------------------------------------------------------------------------------
}
procedure TLeafNode.InternalInitialise(ARecordset: _Recordset);
begin
  Key:= ARecordset.Fields['Item_Key'].Value;
  InitialiseDragDropKey(ARecordset);
  ImageIndex := GetImageIndex;
  SelectedIndex := ImageIndex;
  SetCaption(ARecordset);
  try
    Hint := VarToStr(ARecordset.Fields['Hint'].Value);
  except on E: Exception do
    Hint := '';
  end;
end;  // TLeafNode.InternalInitialise

{-------------------------------------------------------------------------------
}
procedure TLeafNode.InternalInitialiseAddNode;
begin
  Key:= '';
  DragDropKey := '';
  ImageIndex := GetImageIndex;
  SelectedIndex := ImageIndex;
  Caption := ResStr_NewNode;
  //Not set yet. Allows Edit and Delete until the DomainMask is set in the save event.
  DomainMask := 0;
end;  // TLeafNode.InternalInitialiseAddNode 

{-------------------------------------------------------------------------------
}
procedure TLeafNode.InternalInitialiseLinkNode(const AKey: string = '';
    const ACaption: string = '');
begin
  Key:= AKey;
  FindAndSetDragDropKey;
  ImageIndex := GetImageIndex;
  SelectedIndex := ImageIndex;

  Caption := TBrowserNode.GetCaption(ACaption);
 end;  // TLeafNode.InternalInitialiseLinkNode

{-------------------------------------------------------------------------------
}
procedure TLeafNode.InternalRefresh;
begin
  if Assigned(Parent) then
  begin
    if Parent is TContainerNode then
    begin
      if TContainerNode(Parent).KeyDeleted(self) then
      begin
        MessageDlg(ResStr_BrowserNodeDeletedByOtherUser,
            mtInformation, [mbOK], 0);
        PostMessage(TWinControl(Tree.Owner).Handle,
            WM_REMOVE_DELETED_NODE, Integer(Self), 0);
      end;
    end;
  end;
end;  // TLeafNode.InternalRefresh

{-------------------------------------------------------------------------------
}
procedure TLeafNode.SetCaption(ARecordset: _Recordset);
begin
  Caption := TBrowserNode.GetCaption(ARecordset.Fields['Item_Name'].Value);
end;  // TLeafNode.SetCaption

{-------------------------------------------------------------------------------
}
procedure TLeafNode.SetDomainMask(Value: LongWord);
begin
  FDomainMask := Value;
end;  // TLeafNode.SetDomainMask 

{-------------------------------------------------------------------------------
}
procedure TLeafNode.SetDomainMaskSet(Value: Boolean);
begin
  FDomainMaskSet := Value;
end;  // TLeafNode.SetDomainMaskSet 

{-------------------------------------------------------------------------------
}
procedure TLeafNode.SetSecurity;
var
  lDomainMask: Variant;
begin
  FDomainMaskSet := true;
  
  if GetDomainStoredProc <> '' then begin
    lDomainMask := dmGeneral.GetStoredProcOutputParam(GetDomainStoredProc,
                                                      ['@Key', Key], '@DomainMask');
    if not (lDomainMask = NULL) then
      FDomainMask := LongWord(lDomainMask)
    else begin
      MessageDlg(ResStr_BrowserNodeDeletedByOtherUser, mtInformation, [mbOK], 0);
      PostMessage(TWinControl(Tree.Owner).Handle, WM_REMOVE_DELETED_NODE, Integer(Self), 0);
    end;
    //raise ESecurityError.Create(ResStr_ErrorResolvingDomainMask);
  end;
end;  // TLeafNode.SetSecurity 

{-==============================================================================
    THyperlinkLeafNode
===============================================================================}
{-------------------------------------------------------------------------------
}
procedure THyperlinkLeafNode.FindAndSetHyperlinkKey;
begin
  HyperlinkKey := Key;
end;  // THyperlinkLeafNode.FindAndSetHyperlinkKey 

{-------------------------------------------------------------------------------
}
procedure THyperlinkLeafNode.InitialiseHyperlinkKey(ARecordset: _Recordset);
begin
  HyperlinkKey:= Key;
end;  // THyperlinkLeafNode.InitialiseHyperlinkKey 

{-------------------------------------------------------------------------------
}
procedure THyperlinkLeafNode.InternalInitialise(ARecordset: _Recordset);
begin
  inherited InternalInitialise(ARecordset);
  
  JoinKey := ARecordset.Fields['Join_Key'].Value;
  InitialiseHyperlinkKey(ARecordset);
  
  OverlayIndex := 0;
end;  // THyperlinkLeafNode.InternalInitialise 

{-------------------------------------------------------------------------------
}
procedure THyperlinkLeafNode.InternalInitialiseAddNode;
begin
  inherited InternalInitialiseAddNode;
  
  OverlayIndex := 0;
end;  // THyperlinkLeafNode.InternalInitialiseAddNode 

{-------------------------------------------------------------------------------
}
procedure THyperlinkLeafNode.InternalInitialiseLinkNode(const AKey: string = ''; const
    ACaption: string ='');
begin
  inherited InternalInitialiseLinkNode(AKey, ACaption);
  
  FindAndSetHyperlinkKey;
  OverlayIndex := 0;
end;  // THyperlinkLeafNode.InternalInitialiseLinkNode 

{-==============================================================================
    TBaseViewType
===============================================================================}
{-------------------------------------------------------------------------------
}
constructor TBaseViewType.Create(AViewTypeManager: TViewTypeManager);
var
  RegSearchIndex: Integer;
  RegSortOrderIndex: Integer;
begin
  inherited Create;
  
  FViewTypeManager := AViewTypeManager;
  FRefCount := 0;
  try
    FReg := TRegistry.Create;
    FReg.RootKey := HKEY_CURRENT_USER;
    //True because we want to create it if it doesn’t exist}
    FReg.OpenKey(VIEW_MANAGER_SETTINGS_REG_PATH, True);
  
    if FReg.ValueExists(NodeContextText + 'SearchDefault') then begin
      RegSearchIndex := FReg.ReadInteger(NodeContextText + 'SearchDefault');
      if RegSearchIndex <= SearchCount then
        SearchDefaultIndex := RegSearchIndex
      else
        SearchDefaultIndex := 0;
    end
    else begin
      SearchDefaultIndex := 0;
      FReg.WriteInteger(NodeContextText + 'SearchDefault', 0);
    end;
  
    if FReg.ValueExists(NodeContextText + 'SortOrderDefault') then begin
      RegSortOrderIndex := FReg.ReadInteger(NodeContextText +
          'SortOrderDefault');
      if RegSortOrderIndex <= InternalGetSortOrderCount then
        SortOrderDefaultIndex := (RegSortOrderIndex +
            ON_CREATE_SELECT_SORT_INDEX)
      else
        SortOrderDefaultIndex := (0 + ON_CREATE_SELECT_SORT_INDEX);
    end
    else begin
      SortOrderDefaultIndex := 0;
      FReg.WriteInteger(NodeContextText + 'SortOrderDefault', 0);
    end;
    FReg.CloseKey;
  except on ERegistryException do
    begin
      SearchDefaultIndex := 0;
      SortOrderDefaultIndex := (0 + ON_CREATE_SELECT_SORT_INDEX);
    end;
  end;
end;  // TBaseViewType.Create 

{-------------------------------------------------------------------------------
}
destructor TBaseViewType.Destroy;
begin
  FReg.OpenKey(VIEW_MANAGER_SETTINGS_REG_PATH, True);
  FReg.WriteInteger(NodeContextText + 'SearchDefault', SearchDefaultIndex);
  FReg.WriteInteger(NodeContextText + 'SortOrderDefault',
      SortOrderDefaultIndex);
  FReg.CloseKey;
  FReg.Free;
  (*Assert(FRefCount=0,
      'Node destroyed when reference still exist to interfaces');*)
  
  inherited Destroy;
end;  // TBaseViewType.Destroy 

{-------------------------------------------------------------------------------
}
function TBaseViewType.GetCanAdd: Boolean;
begin
  //Allow Add if user is allowed to add anything in the system
  Result := (AppSettings.AddDomainMask > 0)
             and (TUserAccessLevel(AppSettings.UserAccessLevel)
                  in [ualAddOnly, ualFullUser, ualAdmin]);
end;  // TBaseViewType.GetCanAdd 

{-------------------------------------------------------------------------------
}
function TBaseViewType.GetImageList: TImageList;
begin
  Result := dmInterface.ilBrowserNodes;
end;  // TBaseViewType.GetImageList 

{-------------------------------------------------------------------------------
}
function TBaseViewType.GetSearchTextRequired(Index: Integer): Boolean;
begin
  Result := true;
end;  // TBaseViewType.GetSearchTextRequired 

{-------------------------------------------------------------------------------
}
function TBaseViewType.GetSortStoredProcIndex(Index: Integer): Integer;
begin
  Result := Index;
end;  // TBaseViewType.GetSortStoredProcIndex 

{-------------------------------------------------------------------------------
}
function TBaseViewType.GetStoredProcParams: TVariantArray;
begin
  SetLength(Result, 2);
  Result[High(Result) -1] := '@SortOrderIndex';
  Result[High(Result)] := SortOrderDefaultIndex;
end;  // TBaseViewType.GetStoredProcParams 

{-------------------------------------------------------------------------------
}
function TBaseViewType.GetTransformSearchText(ASearchCaptionIndex: Integer; ASearchText:
    string): String;
begin
  Result := StringReplace(ASearchText, '*', '%', [rfReplaceAll]);
end;  // TBaseViewType.GetTransformSearchText 

{-------------------------------------------------------------------------------
}
function TBaseViewType.Get_SortOrderCaption(AIndex: Integer): WideString;
begin
  Result := InternalGetSortOrderCaption(AIndex);
end;  // TBaseViewType.Get_SortOrderCaption 

{-------------------------------------------------------------------------------
}
function TBaseViewType.Get_SortOrderCount: Integer;
begin
  Result := InternalGetSortOrderCount;
end;  // TBaseViewType.Get_SortOrderCount 

{-------------------------------------------------------------------------------
}
function TBaseViewType.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if GetInterface(IID, Obj) then
    Result := 0
  else
    Result := E_NOINTERFACE;
end;  // TBaseViewType.QueryInterface 

{-------------------------------------------------------------------------------
}
procedure TBaseViewType.SelectSort(AIndex: Integer);
var
  lArray: TVariantArray;
begin
  SetLength(lArray, 0);
  
  //Convert MenuIndex into StoredProcSortIndex
  if AIndex >= ON_CREATE_SELECT_SORT_INDEX then
    FSortOrderDefaultIndex := GetSortStoredProcIndex(AIndex -
        ON_CREATE_SELECT_SORT_INDEX)
  else begin
    FSortOrderDefaultIndex := GetSortStoredProcIndex(AIndex);
    if Assigned(FOnFrameNotification) then
      FOnFrameNotification(Self, etSortOrderChange, lArray);
  end;
end;  // TBaseViewType.SelectSort 

{-------------------------------------------------------------------------------
}
procedure TBaseViewType.SetOnFrameNotification(Value: TFrameNotificationEvent);
begin
  FOnFrameNotification := Value;
end;  // TBaseViewType.SetOnFrameNotification 

{-------------------------------------------------------------------------------
}
procedure TBaseViewType.SetSearchDefaultIndex(Value: Integer);
begin
  FSearchDefaultIndex := Value;
  
  if Assigned(FViewTypeManager.OnSearchTypeChange) then
    FViewTypeManager.OnSearchTypeChange(Self);
end;  // TBaseViewType.SetSearchDefaultIndex 

{-------------------------------------------------------------------------------
}
procedure TBaseViewType.SetSortOrderDefaultIndex(Value: Integer);
begin
  // Pass request on to the com interface method.
  SelectSort(Value);
end;  // TBaseViewType.SetSortOrderDefaultIndex 

{-------------------------------------------------------------------------------
}
function TBaseViewType.TransformSearchText(SearchCaptionIndex: Integer; ASearchText: string):
    String;
begin
  Result := GetTransformSearchText(SearchCaptionIndex, ASearchText);
end;  // TBaseViewType.TransformSearchText 

{-------------------------------------------------------------------------------
}
function TBaseViewType._AddRef: Integer;
begin
  Result := -1;
  Inc(FRefCount);
  // Reference counting not used
end;  // TBaseViewType._AddRef 

{-------------------------------------------------------------------------------
}
function TBaseViewType._Release: Integer;
begin
  Result := -1;
  Dec(FRefCount);
  // Reference counting not used
end;  // TBaseViewType._Release 

{-==============================================================================
    TViewTypeManager
===============================================================================}
{-------------------------------------------------------------------------------
}
constructor TViewTypeManager.Create;
var
  lValuationsOffset: Integer;
begin
  inherited Create;
  
  if AppSettings.AllowFinance then
    lValuationsOffset := 0
  else
    lValuationsOffset := 1;
  
  SetLength(FViewTypes, ViewTypeCount);
  
  FViewTypes[0] := TCollectionsViewType.Create(self);
  FViewTypes[1] := TSpecimensViewType.Create(self);
  FViewTypes[2] := TStoresViewType.Create(self);
  FViewTypes[3] := TStoreHierarchyViewType.Create(self);
  FViewTypes[4] := TEnquiriesViewType.Create(self);
  if AppSettings.AllowFinance then
    FViewTypes[5] := TValuationsViewType.Create(self);
  FViewTypes[6 - lValuationsOffset] := TAccessionsViewType.Create(self);
  FViewTypes[7 - lValuationsOffset] := TLoansViewType.Create(self);
  FViewTypes[8 - lValuationsOffset] := TMovementsViewType.Create(self);
  FViewTypes[9 - lValuationsOffset] := TConditionChecksViewType.Create(self);
  FViewTypes[10 - lValuationsOffset] := TJobsViewType.Create(self);
  FViewTypes[11 - lValuationsOffset] := TTasksViewType.Create(self);
  
  SetLength(FHiddenViewTypes, 1);
  FHiddenViewTypes[0] := TInscriptionLabelViewType.Create(self);
  
  If Assigned(FOnSearchTypeChange) then
    FOnSearchTypeChange(Self);
end;  // TViewTypeManager.Create 

{-------------------------------------------------------------------------------
}
destructor TViewTypeManager.Destroy;
var
  liCounter: Integer;
begin
  for liCounter := 0 to High(FViewTypes) do
    FViewTypes[liCounter].Free;
  for liCounter := 0 to High(FHiddenViewTypes) do
    FHiddenViewTypes[liCounter].Free;
  
  inherited Destroy;
end;  // TViewTypeManager.Destroy 

{-------------------------------------------------------------------------------
}
function TViewTypeManager.GetSelected: TBaseViewType;
begin
  Result := FSelectedViewType;
end;  // TViewTypeManager.GetSelected 

{-------------------------------------------------------------------------------
}
function TViewTypeManager.GetViewType(Index: Integer): TBaseViewType;
begin
  Result := FViewTypes[Index];
end;  // TViewTypeManager.GetViewType 

{-------------------------------------------------------------------------------
}
function TViewTypeManager.GetViewTypeCount: Integer;
begin
  if AppSettings.AllowFinance then
    Result := 12
  else
    Result := 11;
end;  // TViewTypeManager.GetViewTypeCount 

{-------------------------------------------------------------------------------
}
procedure TViewTypeManager.SetComboBox(Value: TComboBoxEx);
var
  lIdx: Integer;
begin
  FComboBox := Value;
  
  for lIdx := 0 to (*ViewTypeManager.*)ViewTypeCount -1 do
    with ViewType[lIdx] do begin
      FComboBox.ItemsEx.AddItem(
          Name,
          ImageIndex,
          ImageIndex,
          0,
          0,
          nil);
      if ViewType[lIdx].NodeContextText=AppSettings.DefaultViewType then
        Selected := FViewTypes[lIdx];
    end;
end;  // TViewTypeManager.SetComboBox 

{-------------------------------------------------------------------------------
}
procedure TViewTypeManager.SetSelected(Value: TBaseViewType);
var
  lIndex: Integer;
begin
  FSelectedViewType := Value;
  
  for lIndex := 0 to (ViewTypeCount - 1) do begin
    if ViewType[lIndex] = Value then begin
      ComboBox.ItemIndex := lIndex;
      Exit;
    end;
  end;
  
  // Inform user interface that the search type could now be different
  If Assigned(FOnSearchTypeChange) then
    FOnSearchTypeChange(Self);
end;  // TViewTypeManager.SetSelected 

{-------------------------------------------------------------------------------
}
function TViewTypeManager.ViewTypeByNodeContext(ANodeContext: TNodeContext): TBaseViewType;
var
  lValuationsOffset: Integer;
begin
  if AppSettings.AllowFinance then
    lValuationsOffset := 0
  else
    lValuationsOffset := 1;
  
  case ANodeContext of
    ncCollection:       Result := FViewTypes[0];
    ncSpecimen:         Result := FViewTypes[1];
    ncInscriptionLabel: Result := FHiddenViewTypes[0];
    ncStore:            Result := FViewTypes[2];
    ncStoreHierarchy:   Result := FViewTypes[3];
    ncEnquiry:          Result := FViewTypes[4];
    ncValuation:        Result := FViewTypes[5];
    ncAccession:        Result := FViewTypes[6 - lValuationsOffset];
    ncLoan:             Result := FViewTypes[7 - lValuationsOffset];
    ncMovement,
    ncMovementIn,
    ncMovementOut:      Result := FViewTypes[8 - lValuationsOffset];
    ncConditionCheck:   Result := FViewTypes[9 - lValuationsOffset];
    ncJob:              Result := FViewTypes[10 - lValuationsOffset];
    ncTask:             Result := FViewTypes[11 - lValuationsOffset];
  else
    raise EInvalidViewTypeException(ResStr_InvalidViewTypeRequest);
  end;
end;  // TViewTypeManager.ViewTypeByNodeContext 

{-==============================================================================
    THyperlinkTopLevelNode
===============================================================================}
{-------------------------------------------------------------------------------
}
procedure THyperlinkTopLevelNode.FindAndSetHyperlinkKey;
begin
  HyperlinkKey := Key;
end;  // THyperlinkTopLevelNode.FindAndSetHyperlinkKey 

{-------------------------------------------------------------------------------
}
function THyperlinkTopLevelNode.GetSearchType: SearchManager.TSearchType;
begin
  Result := SearchManager.stNone;
end;  // THyperlinkTopLevelNode.GetSearchType 

{-------------------------------------------------------------------------------
}
procedure THyperlinkTopLevelNode.InitialiseHyperlinkKey(ARecordset: _Recordset);
begin
  HyperlinkKey:= Key;
end;  // THyperlinkTopLevelNode.InitialiseHyperlinkKey 

{-------------------------------------------------------------------------------
}
procedure THyperlinkTopLevelNode.InternalInitialise(ARecordset: _Recordset);
begin
  inherited InternalInitialise(ARecordset);
  
  JoinKey := ARecordset.Fields['Join_Key'].Value;
  InitialiseHyperlinkKey(ARecordset);
  FindAndSetHyperlinkKey;
  OverlayIndex := 0;
  DblClickNavigates := True;
end;  // THyperlinkTopLevelNode.InternalInitialise 

{-------------------------------------------------------------------------------
}
function THyperlinkTopLevelNode.LinkNode(const AKey: string; const ACaption: string):
    TBrowserNode;
var
  lCheckIdx: Integer;
  lDuplicate: Boolean;
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
    with TFolderNode(Result) do
    begin
      Initialise(AKey);
      Caption := TBrowserNode.GetCaption(ACaption);
    end;
  except
    on EBrowserNodeError do begin
      Result.Free;
      raise;
    end;
  end;
end;  // THyperlinkTopLevelNode.LinkNode 

{-------------------------------------------------------------------------------
}
function THyperlinkTopLevelNode.UpdateNodeRelationship(const NewNodeKey: String): String;
begin
end;  // THyperlinkTopLevelNode.UpdateNodeRelationship 

{-------------------------------------------------------------------------------
}
function THyperlinkTopLevelNode.ValidateNewNode(const AKey: TKeyString; const ACaption:
    string): Boolean;
begin
  //Most nodes don't need validating before they are dropped.
  Result := true;
end;  // THyperlinkTopLevelNode.ValidateNewNode 

{-------------------------------------------------------------------------------
  Removes tabs and new lines from the original string, replacing each instance
  with a double space. This makes the returned string a suitable node caption.
}
class function TBrowserNode.GetCaption(const Original: String): String;
begin
  Result := StringReplace(Original, #13#10, '  ', [rfReplaceAll]);
  Result := StringReplace(Result, #9, ' ', [rfReplaceAll]);
end;

end.
