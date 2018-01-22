{===============================================================================
  Unit:        BrowserNodeSpecimen.pas

  Defines:     Many Classes

  Description: Contains node classes related to Specimens only

  Model:       BrowserNodes.mpb

  Created:     August 2003

  Last revision information:
    $Revision: 46 $
    $Date: 29/06/16 10:50 $
    $Author: Christopherknight $  

===============================================================================}
unit BrowserNodeSpecimen;

interface

uses
  SysUtils, Windows, Messages, Classes, Graphics, Controls, DataTypes,
  Forms, Dialogs, TreeColl, BrowserNodeFramework, BrowserNodeCollectionUnits,
  ResourceStrings, BaseDetailFrameUnit, ADODB, RapTree, LuxembourgConstants,
  SearchManager, DataClasses;

type
  {-----------------------------------------------------------------------------
    Depending on whether the top level Specimen node is a Life Science or an Earth Science,
    the class table name of the Field Data folder node will be different.
  }
  TFieldDataLeafNode = class(THyperlinkLeafNode, IAdditionalProperties)
  private
    function GetProperty(const AName: string): Variant;
  protected
    function GetAssociatedFrame: TBaseDetailFrameClass; override;
    function GetDomainStoredProc: String; override;
    function GetImageIndex: Integer; override;
    function GetNodeContext: TNodeContext; override;
    procedure InitialiseDragDropKey(ARecordset: _Recordset); override;
    procedure InitialiseHyperlinkKey(ARecordset: _Recordset); override;
    function InternalGetProperty(const AName: string): Variant; override;
    procedure SetCaption(ARecordset: _Recordset); override;
  public
    class function ClassTableName: String; override;
    procedure FindAndSetDragDropKey; override;
    procedure FindAndSetHyperlinkKey; override;
  end;
  
  TFieldDataFolderNode = class(TFolderNode, IAddMenuOptions)
  private
    FIsLifeScience: Boolean;
  protected
    function GetAddButtonMenuCaption(Index: Integer): String; override;
    function GetAddButtonMenuCaptionsCount: Integer; override;
    function GetAddMenuIsAdd(AMenuIndex: Integer): Boolean; override;
    function GetChildNodeType(Index: Integer): TBrowserNodeClass; override;
    function GetChildNodeTypeCount: Integer; override;
    function GetSearchType: SearchManager.TSearchType; override;
    procedure InitialiseDragDropKey(ARecordset: _Recordset); override;
    procedure SetCaption; override;
    function StoredProcByChildType(ALeafNodeClass: TLeafNodeClass): String; override;
  public
    class function ClassTableName: String; override;
    function LinkNode(const AKey: string; const ACaption: string): TBrowserNode; override;
    function UpdateNodeRelationship(const NewNodeKey: String): String; override;
    function ValidateNewNode(const AKey: TKeyString; const ACaption: string): Boolean;
        override;
  end;
  
  TBaseDeterminationLeafNode = class(TLeafNode)
  private
    FRankColor: TColor;
  protected
    function GetImageIndex: Integer; override;
    procedure InternalInitialise(ARecordset: _Recordset); override;
    function IsRecorderLeafNode: Boolean; virtual; abstract;
  public
    function CanRemoveDependents: Boolean; override;
    procedure RefreshRankColor;
    property RankColor: TColor read FRankColor;
  end;
  
  TDeterminationLeafNode = class(TBaseDeterminationLeafNode, IAdditionalProperties)
  private
    function GetProperty(const AName: string): Variant;
  protected
    function GetAssociatedFrame: TBaseDetailFrameClass; override;
    function InternalGetProperty(const AName: string): Variant; override;
    function IsRecorderLeafNode: Boolean; override;
  public
    class function ClassTableName: String; override;
  end;
  
  TDeterminationRecorderLeafNode = class(TBaseDeterminationLeafNode, IAdditionalProperties)
  private
    function GetProperty(const AName: string): Variant;
  protected
    function GetAssociatedFrame: TBaseDetailFrameClass; override;
    function InternalGetProperty(const AName: string): Variant; override;
    function IsRecorderLeafNode: Boolean; override;
  public
    class function ClassTableName: String; override;
  end;

  TDeterminationFolderNode = class(TFolderNode, IAddMenuOptions)
  private
    FGroupDeterminationsByDomain: Boolean;
  protected
    procedure AddChildNodesOfType(ABrowserNodeClass: TBrowserNodeClass); override;
    procedure AddDomainNodes; virtual;
    function AddNode(ATree: TRapidTree; AMenuIndex: integer): TFlyNode;
    function GetAddButtonMenuCaption(Index: Integer): String; override;
    function GetAddButtonMenuCaptionsCount: Integer; override;
    function GetAddMenuIsAdd(AMenuIndex: Integer): Boolean; override;
    function GetChildNodeType(Index: Integer): TBrowserNodeClass; override;
    function GetChildNodeTypeCount: Integer; override;
    function GetDeterminationLeafNodeType: TLeafNodeClass; virtual;
    procedure SetCaption; override;
    function StoredProcByChildType(ALeafNodeClass: TLeafNodeClass): String; override;
    procedure MoveLeafNodeToDomainFolder(ALeafNode: TBaseDeterminationLeafNode);
    property GroupDeterminationsByDomain: Boolean
        read FGroupDeterminationsByDomain;
  public
    constructor Create(AOwner: TTreeCollection); override;
    class function ClassTableName: String; override;
    function UpdateNodeRelationship(const NewNodeKey: String): String; override;
  end;

  TDomainFolderNode = class(TFolderNode, IAddMenuOptions)
  protected
    function AddNode(ATree: TRapidTree; AMenuIndex: integer): TFlyNode;
    function GetAddButtonMenuCaption(Index: Integer): String; override;
    function GetAddButtonMenuCaptionsCount: Integer; override;
    function GetAddMenuIsAdd(AMenuIndex: Integer): Boolean; override;
    function GetChildNodeType(Index: Integer): TBrowserNodeClass; override;
    function GetChildNodeTypeCount: Integer; override;
    procedure SetCaption; override;
    function StoredProcByChildType(ALeafNodeClass: TLeafNodeClass): String; override;
    function GetStoredProcParams: TVariantArray; override;
  public
    class function ClassTableName: string; override;
    procedure Initialise(ARecordset: _Recordset); overload;
    procedure Initialise(const Caption: string; DomainMask: Integer); overload;
    function UpdateNodeRelationship(const NewNodeKey: String): String; override;
  end;

  TInscriptionLabelLeafNode = class(THyperlinkLeafNode)
  protected
    function GetAssociatedFrame: TBaseDetailFrameClass; override;
    function GetDomainStoredProc: String; override;
    function GetImageIndex: Integer; override;
    function GetNodeContext: TNodeContext; override;
  public
    class function ClassTableName: String; override;
  end;
  
  TInscriptionLabelTopLevelNode = class(TTopLevelNode, IAdditionalProperties)
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
    function StoredProcByChildType(ALeafNodeClass: TLeafNodeClass): String; override;
  public
    class function ClassTableName: String; override;
  end;
  
  TInscriptionLabelFolderNode = class(TFolderNode, IAddMenuOptions)
  protected
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

//==============================================================================
implementation

uses
  BrowserNodeCommon, BrowserNodeMovement, GeneralData, Variants, VagueDate,
  FrameInscription, FrameFieldData, FrameDetermination, ExceptionForm,
  BaseADODataModule, ApplicationSettings, StrUtils, ComCtrls, FrameCBNavigation;

{-==============================================================================
    TFieldDataLeafNode
===============================================================================}
{-------------------------------------------------------------------------------
  Depending on whether the top level Specimen node is a Life Science or an Earth Science, the
      class table name of the Field Data folder node will be different.
}
class function TFieldDataLeafNode.ClassTableName: String;
begin
  Result := TN_SAMPLE;
end;  // TFieldDataLeafNode.ClassTableName 

{-------------------------------------------------------------------------------
}
procedure TFieldDataLeafNode.FindAndSetDragDropKey;
begin
  // The VarToStr is required here as this method can get called "too early" for the
  // correct Key value, if a link has just been established. The correct key will be
  // set in TFieldDataFolderNode.UpdateNodeRelationship afterwards.
  DragDropKey := VarToStr(dmGeneral.GetStoredProcOutputParam(
      'usp_FieldData_DragDropKey_Get', ['@Key', Key], '@DragDropKey'));
end;  // TFieldDataLeafNode.FindAndSetDragDropKey 

{-------------------------------------------------------------------------------
}
procedure TFieldDataLeafNode.FindAndSetHyperlinkKey;
begin
  // Same reason for VarToStr as above.
  HyperlinkKey := VarToStr(dmGeneral.GetStoredProcOutputParam(
      'usp_FieldData_DragDropKey_Get', ['@Key', Key], '@DragDropKey'));
end;  // TFieldDataLeafNode.FindAndSetHyperlinkKey 

{-------------------------------------------------------------------------------
}
function TFieldDataLeafNode.GetAssociatedFrame: TBaseDetailFrameClass;
begin
  Result := TfraFieldData;
end;  // TFieldDataLeafNode.GetAssociatedFrame 

{-------------------------------------------------------------------------------
}
function TFieldDataLeafNode.GetDomainStoredProc: String;
begin
  Result := 'usp_FieldData_DomainMask_Get';
end;  // TFieldDataLeafNode.GetDomainStoredProc 

{-------------------------------------------------------------------------------
}
function TFieldDataLeafNode.GetImageIndex: Integer;
begin
  Result := 36;
end;  // TFieldDataLeafNode.GetImageIndex 

{-------------------------------------------------------------------------------
}
function TFieldDataLeafNode.GetNodeContext: TNodeContext;
begin
  Result := ncRecorder;
end;  // TFieldDataLeafNode.GetNodeContext 

{-------------------------------------------------------------------------------
}
function TFieldDataLeafNode.GetProperty(const AName: string): Variant;
begin
  Result := InternalGetProperty(AName);
end;  // TFieldDataLeafNode.GetProperty 

{-------------------------------------------------------------------------------
}
procedure TFieldDataLeafNode.InitialiseDragDropKey(ARecordset: _Recordset);
begin
  DragDropKey := ARecordset.Fields['Drag_Drop_Item_Key'].Value;
end;  // TFieldDataLeafNode.InitialiseDragDropKey

{-------------------------------------------------------------------------------
}
procedure TFieldDataLeafNode.InitialiseHyperlinkKey(ARecordset: _Recordset);
begin
  HyperlinkKey := ARecordset.Fields['Hyperlink_Item_Key'].Value;
end;  // TFieldDataLeafNode.InitialiseHyperlinkKey 

{-------------------------------------------------------------------------------
}
function TFieldDataLeafNode.InternalGetProperty(const AName: string): Variant;
begin
  Result := inherited InternalGetProperty(AName);
  // The determination leaf nodes need to know whether their parent specimen
  // top level node is a Life Science node or not.
  if Result = Unassigned then
    if AName = PROP_SPECIMEN_IS_LIFESCIENCES then begin
      if TopLevelNode is TSpecimenTopLevelNode then
        Result := TSpecimenTopLevelNode(TopLevelNode).LifeSciences = True
      else
        raise EBrowserNodeError.Create(Format(ResStr_TopLevelNodeWrongType,
                                              ['TSpecimenTopLevelNode']));
    end else
      Result := Unassigned;
end;  // TFieldDataLeafNode.InternalGetProperty 

{-------------------------------------------------------------------------------
}
procedure TFieldDataLeafNode.SetCaption(ARecordset: _Recordset);
begin
  Caption:= dmGeneral.GetVagueDateStringFromRecordset(ARecordset, '');
  if ARecordset.Fields['LocationName'].Value <> Null then
    Caption := Caption + ' - ' + ARecordset.Fields['LocationName'].Value;
end;  // TFieldDataLeafNode.SetCaption 

{-==============================================================================
    TFieldDataFolderNode
===============================================================================}
{-------------------------------------------------------------------------------
  Depending on whether the top level Specimen node is a Life Science or an Earth Science, the
      class table name of the Field Data folder node will be different.
}
class function TFieldDataFolderNode.ClassTableName: String;
begin
  Result := TN_SAMPLE;
end;  // TFieldDataFolderNode.ClassTableName 

{-------------------------------------------------------------------------------
}
function TFieldDataFolderNode.GetAddButtonMenuCaption(Index: Integer): String;
begin
  case Index of
    0:  Result := ResStr_AddNew;
    1:  Result := ResStr_LinkToExisting;
  else
    raise EAddMenuItemError.Create(ResStr_InvalidAddMenuIndexRequest);
  end;
end;  // TFieldDataFolderNode.GetAddButtonMenuCaption 

{-------------------------------------------------------------------------------
}
function TFieldDataFolderNode.GetAddButtonMenuCaptionsCount: Integer;
begin
  Result := 2;
end;  // TFieldDataFolderNode.GetAddButtonMenuCaptionsCount 

{-------------------------------------------------------------------------------
}
function TFieldDataFolderNode.GetAddMenuIsAdd(AMenuIndex: Integer): Boolean;
begin
  case AMenuIndex of
    0 : Result := True
  else
    Result := False;
  end;
end;  // TFieldDataFolderNode.GetAddMenuIsAdd 

{-------------------------------------------------------------------------------
}
function TFieldDataFolderNode.GetChildNodeType(Index: Integer): TBrowserNodeClass;
begin
  case Index of
    0: Result := TFieldDataLeafNode;
  else
    raise EBrowserNodeError.Create(ResStr_InvalidChildNodeTypeIndexRequest);
  end;
end;  // TFieldDataFolderNode.GetChildNodeType 

{-------------------------------------------------------------------------------
}
function TFieldDataFolderNode.GetChildNodeTypeCount: Integer;
begin
  Result:= 1;
end;  // TFieldDataFolderNode.GetChildNodeTypeCount 

{-------------------------------------------------------------------------------
  The search type depends on whether the specimen associated with this folder is a Life
      Science or Earth Science Specimen.
}
function TFieldDataFolderNode.GetSearchType: SearchManager.TSearchType;
begin
  if Assigned(TopLevelNode) and (TopLevelNode is TSpecimenTopLevelNode) then
    FIsLifeScience := TSpecimenTopLevelNode(TopLevelNode).LifeSciences
  else
    raise EBrowserNodeError.Create(Format(ResStr_TopLevelNodeWrongType,
                                          ['TSpecimenTopLevelNode']));
  
  if FIsLifeScience then Result := stTaxonOccurrence
                    else Result := stOccurrence;
end;  // TFieldDataFolderNode.GetSearchType 

{-------------------------------------------------------------------------------
}
procedure TFieldDataFolderNode.InitialiseDragDropKey(ARecordset: _Recordset);
begin
  DragDropKey := ARecordset.Fields['Drag_Drop_Item_Key'].Value;
end;  // TFieldDataFolderNode.InitialiseDragDropKey 

{-------------------------------------------------------------------------------
}
function TFieldDataFolderNode.LinkNode(const AKey: string; const ACaption: string):
    TBrowserNode;
begin
  DragDropKey := AKey;
  Result := inherited LinkNode(AKey, ACaption);
end;  // TFieldDataFolderNode.LinkNode

{-------------------------------------------------------------------------------
}
procedure TFieldDataFolderNode.SetCaption;
begin
  Text := ResStr_FieldData;
end;  // TFieldDataFolderNode.SetCaption

{-------------------------------------------------------------------------------
}
function TFieldDataFolderNode.StoredProcByChildType(ALeafNodeClass: TLeafNodeClass): String;
begin
  Result := '';
  if ALeafNodeClass = TFieldDataLeafNode then
    Result := 'usp_FieldData_Select_ForSpecimen'
  else
    raise EBrowserNodeError.Create(ResStr_InvalidChildNodeTypeIndexRequest);
end;  // TFieldDataFolderNode.StoredProcByChildType

{-------------------------------------------------------------------------------
  Insert the record into the Specimen_Field_Data table. Also has to ensure the new Field Data
      node has the correct key in it. By default, it has the (Taxon) Occurrence key - which is
      wrong. Hence, the last node for the folder (i.e. the new one) is given the new
      Specimen_Field_Data_Key.
}
function TFieldDataFolderNode.UpdateNodeRelationship(const NewNodeKey: String): String;
var
  lTaxonOccurrenceKey, lOccurrenceKey, lSFDKey: TKeyString;
  lVagueDate: TVagueDate;
begin
  // If the FindDialogKey = NewNodeKey, then we are dealing with a node added
  // by 'Link To Existing'. Hence, the key will be wrong. If it isn't, then
  // the the node was added correctly using the Field Data frame, and no
  // furthur records need to be added.
  if (FindDialogKey = NewNodeKey) or (DragDropKey = NewNodeKey) then begin
    if FIsLifeScience then begin
      lTaxonOccurrenceKey := NewNodeKey;
      lOccurrenceKey := '';
    end else begin
      lTaxonOccurrenceKey := '';
      lOccurrenceKey := NewNodeKey;
    end;
    lSFDKey := dmGeneral.RunInsertStoredProc(TN_SPECIMEN_FIELD_DATA,
                                      'usp_FieldData_Insert_ForSpecimen',
                                     ['@SpecimenKey', Key,
                                      '@TaxonOccurrenceKey', lTaxonOccurrenceKey,
                                      '@OccurrenceKey', lOccurrenceKey],
                                      '@Key');
    // The new leaf node currently has the key of the Occurrence. This is incorrect -
    // the leaf node should have the key of the new Specimen_Field_Data record.
    // It also has the wrong caption, so we need to generate the caption from
    // the vague date information stored in the record.
    with dmGeneral.GetRecordset('usp_FieldData_Select', ['@Key', lSFDKey]) do
      if not Eof then begin
        if not VarIsNull(Fields['Vague_Date_Start'].Value) then
          lVagueDate.StartDate := Fields['Vague_Date_Start'].Value;
        if not VarIsNull(Fields['Vague_Date_End'].Value) then
          lVagueDate.EndDate := Fields['Vague_Date_End'].Value;
        lVagueDate.DateTypeString := VarToStr(Fields['Vague_Date_Type'].Value);
      end;
    TBrowserNode(Item[Count-1]).Key := lSFDKey;
    TBrowserNode(Item[Count-1]).Caption := VagueDateToString(lVagueDate);
    Result := dmGeneral.GetStoredProcOutputParam('usp_FieldData_DragDropKey_Get',
                                                 ['@Key', lSFDKey], '@DragDropKey');
    FindDialogKey := '';
    DragDropKey := '';
  end;
end;  // TFieldDataFolderNode.UpdateNodeRelationship

{-------------------------------------------------------------------------------
  Before linking an Occurrence/Taxon Occurrence to the Specimen, we have to first check that it isn't already linked, because otherwise the user would see an error about trying to insert duplicate records.
}
function TFieldDataFolderNode.ValidateNewNode(const AKey: TKeyString; const ACaption: string):
    Boolean;
var
  lTaxonOccurrenceKey, lOccurrenceKey: TKeyString;
begin
  if FIsLifeScience then begin
    lTaxonOccurrenceKey := AKey;
    lOccurrenceKey := '';
  end else begin
    lTaxonOccurrenceKey := '';
    lOccurrenceKey := AKey;
  end;
  
  Result := (dmGeneral.GetStoredProcOutputParam('usp_FieldDataValid_Get',
                                   ['@SpecimenKey', Key,
                                    '@TaxonOccurrenceKey', lTaxonOccurrenceKey,
                                    '@OccurrenceKey', lOccurrenceKey],
                                    '@IsValid') = True);
  ValidateValue(Result, ResStr_OccurrenceAlreadyLinked);
end;  // TFieldDataFolderNode.ValidateNewNode 

{-==============================================================================
    TBaseDeterminationLeafNode
===============================================================================}
{-------------------------------------------------------------------------------
}
function TBaseDeterminationLeafNode.CanRemoveDependents: Boolean;
begin
  if (dmGeneral.GetStoredProcOutputParam('usp_Specimen_HasDetermination_Get',
      ['@SpecimenUnitKey', TopLevelNode.Key], '@HasDetermination') = 0) then
  begin
    Result := True;
    dmGeneral.RunDeleteStoredProc('usp_Specimen_Delete', ['@SpecimenKey', TopLevelNode.Key]);
  end else
    Result := False;
end;  // TBaseDeterminationLeafNode.CanRemoveDependents 

{-------------------------------------------------------------------------------
}
function TBaseDeterminationLeafNode.GetImageIndex: Integer;
begin
  Result := 37;
end;  // TBaseDeterminationLeafNode.GetImageIndex 

{-------------------------------------------------------------------------------
}
procedure TBaseDeterminationLeafNode.InternalInitialise(ARecordset: _Recordset);
begin
  inherited InternalInitialise(ARecordset);
  
  if not (ARecordset.Fields['Concept_Rank_Key'].Value = Null) then
    FRankColor := TColor(ARecordset.Fields['Color_R'].Value shl 16 +
                         ARecordset.Fields['Color_G'].Value shl 8 +
                         ARecordset.Fields['Color_B'].Value);
end;  // TBaseDeterminationLeafNode.InternalInitialise 

{-------------------------------------------------------------------------------
}
procedure TBaseDeterminationLeafNode.RefreshRankColor;
begin
  with dmGeneral.GetRecordset('usp_ConceptRankColor_ForDetermination_Select',
                              ['@Key', Key,
                              '@IsLifeScience', IsRecorderLeafNode]) do
    if not EOF then
      FRankColor := TColor(Fields['Color_R'].Value shl 16 +
                           Fields['Color_G'].Value shl 8 +
                           Fields['Color_B'].Value);
end;  // TBaseDeterminationLeafNode.RefreshRankColor 

{-==============================================================================
    TDeterminationLeafNode
===============================================================================}
{-------------------------------------------------------------------------------
}
class function TDeterminationLeafNode.ClassTableName: String;
begin
  Result := TN_DETERMINATION;
end;  // TDeterminationLeafNode.ClassTableName 

{-------------------------------------------------------------------------------
}
function TDeterminationLeafNode.GetAssociatedFrame: TBaseDetailFrameClass;
begin
  Result := TfraDetermination;
end;  // TDeterminationLeafNode.GetAssociatedFrame 

{-------------------------------------------------------------------------------
}
function TDeterminationLeafNode.GetProperty(const AName: string): Variant;
begin
  Result := InternalGetProperty(AName);
end;  // TDeterminationLeafNode.GetProperty 

{-------------------------------------------------------------------------------
}
function TDeterminationLeafNode.InternalGetProperty(const AName: string): Variant;
var
  lTopLevelNode: TTopLevelNode;
begin
  Result := inherited InternalGetProperty(AName);

  // The determination leaf nodes need to know whether their parent specimen
  // top level node is a Life Science node or not.
  if (Result = Unassigned) and (AName = PROP_SPECIMEN_IS_LIFESCIENCES) then
  begin
    lTopLevelNode := TopLevelNode;
    if not (lTopLevelNode is TSpecimenTopLevelNode) then
      raise EBrowserNodeError.CreateFmt(
          ResStr_TopLevelNodeWrongType, ['TSpecimenTopLevelNode']);

    Result := TSpecimenTopLevelNode(lTopLevelNode).LifeSciences;
  end;
end;  // TDeterminationLeafNode.InternalGetProperty

{-------------------------------------------------------------------------------
}
function TDeterminationLeafNode.IsRecorderLeafNode: Boolean;
begin
  Result := False;
end;  // TDeterminationLeafNode.IsRecorderLeafNode 

{-==============================================================================
    TDeterminationRecorderLeafNode
===============================================================================}
{-------------------------------------------------------------------------------
}
class function TDeterminationRecorderLeafNode.ClassTableName: String;
begin
  Result := TN_TAXON_DETERMINATION;
end;  // TDeterminationRecorderLeafNode.ClassTableName 

{-------------------------------------------------------------------------------
}
function TDeterminationRecorderLeafNode.GetAssociatedFrame: TBaseDetailFrameClass;
begin
  Result := TfraDetermination;
end;  // TDeterminationRecorderLeafNode.GetAssociatedFrame 

{-------------------------------------------------------------------------------
}
function TDeterminationRecorderLeafNode.GetProperty(const AName: string): Variant;
begin
  Result := InternalGetProperty(AName);
end;  // TDeterminationRecorderLeafNode.GetProperty 

{-------------------------------------------------------------------------------
}
function TDeterminationRecorderLeafNode.InternalGetProperty(const AName: string): Variant;
var
  lTopLevelNode: TTopLevelNode;
begin
  Result := inherited InternalGetProperty(AName);

  // The determination leaf nodes need to know whether their parent specimen
  // top level node is a Life Science node or not.
  if (Result = Unassigned) and (AName = PROP_SPECIMEN_IS_LIFESCIENCES) then
  begin
    lTopLevelNode := TopLevelNode;
    if not (lTopLevelNode is TSpecimenTopLevelNode) then
      raise EBrowserNodeError.CreateFmt(
          ResStr_TopLevelNodeWrongType, ['TSpecimenTopLevelNode']);

    Result := TSpecimenTopLevelNode(lTopLevelNode).LifeSciences;
  end;
end;  // TDeterminationRecorderLeafNode.InternalGetProperty

{-------------------------------------------------------------------------------
}
function TDeterminationRecorderLeafNode.IsRecorderLeafNode: Boolean;
begin
  Result := True;
end;  // TDeterminationRecorderLeafNode.IsRecorderLeafNode

{-==============================================================================
    TDeterminationFolderNode
===============================================================================}
{-------------------------------------------------------------------------------
}
class function TDeterminationFolderNode.ClassTableName: String;
begin
  Result := TN_MIXED_DATA;
end;  // TDeterminationFolderNode.ClassTableName

{-------------------------------------------------------------------------------
  Initialises the determination folder node.
}
constructor TDeterminationFolderNode.Create(AOwner: TTreeCollection);

  function NavigationFrame: TfraCBNavigation;
  var
    lParent: TWinControl;
  begin
    lParent := Owner.Tree.Parent;
    while not (lParent is TfraCBNavigation) do
    begin
      if not Assigned(lParent) then
        raise EBrowserNodeError.CreateFmt(
            ResStr_ParentWrongType, ['TfraCBNavigation']);
      lParent := lParent.Parent;
    end;
    Result := TfraCBNavigation(lParent);
  end;

begin
  inherited Create(AOwner);
  FGroupDeterminationsByDomain := NavigationFrame.GroupDeterminationsByDomain;
end;

{-------------------------------------------------------------------------------
  Loads the child nodes of the given type from the database.
}
procedure TDeterminationFolderNode.AddChildNodesOfType(
  ABrowserNodeClass: TBrowserNodeClass);
begin
  if not GroupDeterminationsByDomain then
    inherited
  else if ABrowserNodeClass = TDomainFolderNode then
    AddDomainNodes
  else
    raise EBrowserNodeError.Create(ResStr_InvalidChildNodeTypeIndexRequest);
end;

{-------------------------------------------------------------------------------
  Loads the domain folder nodes from the database.
}
procedure TDeterminationFolderNode.AddDomainNodes;
var
  lTopLevelNode: TTopLevelNode;
  lLifeSciences: Boolean;
  lRecordset: _Recordset;
  lChild: TDomainFolderNode;
begin
  lTopLevelNode := TopLevelNode;
  if not (TopLevelNode is TSpecimenTopLevelNode) then
    raise EBrowserNodeError.CreateFmt(
        ResStr_TopLevelNodeWrongType, ['TSpecimenTopLevelNode']);

  lLifeSciences := TSpecimenTopLevelNode(lTopLevelNode).LifeSciences;

  lRecordset := dmGeneral.GetRecordset(
      'dbo.usp_Domains_Select_ForSpecimen',
      ['@ParentKey', Key, '@LifeSciences', lLifeSciences]);

  Tree.Items.BeginUpdate;
  try
    if lRecordset.RecordCount > 0 then
    begin
      lRecordSet.MoveFirst;
      while not lRecordset.EOF do
      begin
        lChild := TDomainFolderNode(
            Tree.Items.AddTypedChild(Self, TDomainFolderNode));
        lChild.Initialise(lRecordset); //Sets Key and Caption
        lRecordset.MoveNext;
      end;
    end;

    // Add a Taxa dommy node.
    lChild := TDomainFolderNode(
    Tree.Items.AddTypedChild(Self, TDomainFolderNode));
    lChild.Initialise('Taxa', -1); //Sets Key and Caption
  finally
    Tree.Items.EndUpdate;
    lRecordset.Close;
  end;
end;

{-------------------------------------------------------------------------------
  Adds a new determination node to the current node.
}
function TDeterminationFolderNode.AddNode(ATree: TRapidTree;
  AMenuIndex: integer): TFlyNode;
begin
  Result := ATree.Items.AddTypedChild(Self, GetDeterminationLeafNodeType);
  TLeafNode(Result).InitialiseNewNode;
end;

{-------------------------------------------------------------------------------
}
function TDeterminationFolderNode.GetAddButtonMenuCaption(Index: Integer): String;
begin
  case Index of
    0: Result := ResStr_Add;
  else
    raise EAddMenuItemError.Create(ResStr_InvalidAddMenuIndexRequest);
  end;
end;  // TDeterminationFolderNode.GetAddButtonMenuCaption

{-------------------------------------------------------------------------------
}
function TDeterminationFolderNode.GetAddButtonMenuCaptionsCount: Integer;
begin
  Result := 1;
end;  // TDeterminationFolderNode.GetAddButtonMenuCaptionsCount 

{-------------------------------------------------------------------------------
}
function TDeterminationFolderNode.GetAddMenuIsAdd(AMenuIndex: Integer): Boolean;
begin
  case AMenuIndex of
    0 : Result := True
  else
    Result := False;
  end;
end;  // TDeterminationFolderNode.GetAddMenuIsAdd 

{-------------------------------------------------------------------------------
}
function TDeterminationFolderNode.GetChildNodeType(Index: Integer): TBrowserNodeClass;
begin
  if GroupDeterminationsByDomain then
    Result := TDomainFolderNode
  else
    Result := GetDeterminationLeafNodeType;
end;  // TDeterminationFolderNode.GetChildNodeType

{-------------------------------------------------------------------------------
}
function TDeterminationFolderNode.GetChildNodeTypeCount: Integer;
begin
  // This has been changed from 2 to 1, because the Determinations folder node
  // should only be able to show one type of node at a time.
  Result:= 1;
end;  // TDeterminationFolderNode.GetChildNodeTypeCount

{-------------------------------------------------------------------------------
  Gets the type of leaf nodes that may appear within this folder (either
  directly, or in domain folders). 
}
function TDeterminationFolderNode.GetDeterminationLeafNodeType: TLeafNodeClass;
var
  lTopLevelNode: TTopLevelNode;
begin
  lTopLevelNode := TopLevelNode;
  if not (lTopLevelNode is TSpecimenTopLevelNode) then
    raise EBrowserNodeError.CreateFmt(
        ResStr_TopLevelNodeWrongType, ['TSpecimenTopLevelNode']);

  if TSpecimenTopLevelNode(lTopLevelNode).LifeSciences then
    Result := TDeterminationRecorderLeafNode
  else
    Result := TDeterminationLeafNode;
end;

{-------------------------------------------------------------------------------
}
procedure TDeterminationFolderNode.SetCaption;
begin
  Text := ResStr_Determinations;
end;  // TDeterminationFolderNode.SetCaption

{-------------------------------------------------------------------------------
}
function TDeterminationFolderNode.StoredProcByChildType(ALeafNodeClass: TLeafNodeClass):
    String;
begin
  if GroupDeterminationsByDomain then
    raise EBrowserNodeError.Create(ResStr_InvalidChildNodeTypeIndexRequest);

  Result := '';
  if Assigned(Parent) then
    if (Parent is TSpecimenTopLevelNode) then
      if TSpecimenTopLevelNode(Parent).LifeSciences then
        Result := 'usp_DeterminationsRecorder_Select_ForSpecimen'
      else
        Result := 'usp_Determinations_Select_ForSpecimen'
    else
      raise EBrowserNodeError.Create(Format(ResStr_ParentWrongType,
                                            ['TSpecimenTopLevelNode']))
  else
    raise EBrowserNodeError.Create(ResStr_ParentUnassigned);
end;  // TDeterminationFolderNode.StoredProcByChildType

{-------------------------------------------------------------------------------
  If determinations are grouped by domain, and a determination node has just
  been saved, moves it into the correct domain folder.
}
function TDeterminationFolderNode.UpdateNodeRelationship(
  const NewNodeKey: String): String;
var
  lChild: TBrowserNode;
begin
  if not GroupDeterminationsByDomain then Exit;

  lChild := TBrowserNode(GetLastChild);
  while Assigned(lChild) do
  begin
    if (lChild.Key = NewNodeKey) and (lChild is TBaseDeterminationLeafNode) then
    begin
      MoveLeafNodeToDomainFolder(TBaseDeterminationLeafNode(lChild));
      Break;
    end;
    lChild := TBrowserNode(GetPrevChild(lChild));
  end;

  Result := '';
end;

{-------------------------------------------------------------------------------
  Moves the given determination leaf node into an appropriate domain folder
  beneath the current node.
}
procedure TDeterminationFolderNode.MoveLeafNodeToDomainFolder(
  ALeafNode: TBaseDeterminationLeafNode);
var
  lDomainMask: Integer;
  lDomainFolder: TDomainFolderNode;

  {-----------------------------------------------------------------------------
    Finds an existing child that is a domain folder with an appropriate mask.
    Returns nil if there is no such child.
  }
  function FindDomainFolderNode: TDomainFolderNode;
  var
    lNode: TFlyNode;
  begin
    lNode := GetFirstChild;
    while Assigned(lNode) do
    begin
      if lNode is TDomainFolderNode then
      begin
        if StrToInt(TDomainFolderNode(lNode).Key) and lDomainMask > 0 then
        begin
          Result := TDomainFolderNode(lNode);
          Exit;
        end;

        if (StrToInt(TDomainFolderNode(lNode).Key) = -1) and (lDomainMask = 0) then
        begin
          Result := TDomainFolderNode(lNode);
          Exit;
        end;
      end;
      lNode := GetNextChild(lNode);
    end;
    Result := nil;
  end;

  {-----------------------------------------------------------------------------
    Creates a new domain folder with an appropriate mask.
  }
  function MakeDomainFolderNode: TDomainFolderNode;
  begin
    Result := TDomainFolderNode(
        Tree.Items.AddTypedChild(Self, TDomainFolderNode));
    Result.Initialise(
        dmGeneral.GetStoredProcOutputParam(
            'usp_DomainsForMask_Get',
            ['@Mask', lDomainMask],
            '@Domains'),
        lDomainMask);
  end;

  {-----------------------------------------------------------------------------
    Removes the existing child from the domain folder that has the same key as
    the given leaf node.
  }
  procedure RemoveExistingChild;
  var
    lChild: TFlyNode;
  begin
    lChild := lDomainFolder.GetFirstChild;
    while Assigned(lChild) do
    begin
      if lChild is TBaseDeterminationLeafNode then
      begin
        if TBaseDeterminationLeafNode(lChild).Key = ALeafNode.Key then
        begin
          lChild.Delete;
          Break;
        end;
      end;
      lChild := lDomainFolder.GetNextChild(lChild);
    end;
  end;

begin
  lDomainMask := dmGeneral.GetStoredProcOutputParam(
      'usp_Determination_DomainMaskGet',
      ['@DeterminationKey', ALeafNode.Key,
       '@LifeSciences', TSpecimenTopLevelNode(TopLevelNode).LifeSciences],
      '@DomainMask');

  lDomainFolder := FindDomainFolderNode;
  if not Assigned(lDomainFolder) then
  begin
    lDomainFolder := MakeDomainFolderNode;
  end;

  if not lDomainFolder.Expanded then
  begin
    lDomainFolder.Expand(False);
    RemoveExistingChild;
  end;
  ALeafNode.MoveTo(lDomainFolder, naAddChild);
end;

{-==============================================================================
    TDomainFolderNode
===============================================================================}
{-------------------------------------------------------------------------------
}
class function TDomainFolderNode.ClassTableName: string;
begin
  Result := TN_MIXED_DATA;
end;

{-------------------------------------------------------------------------------
  Adds a new determination node to the parent node (the "Determinations"
  folder).
}
function TDomainFolderNode.AddNode(ATree: TRapidTree; AMenuIndex: integer):
  TFlyNode;
begin
  Result := ATree.Items.AddTypedChild(Parent, GetChildNodeType(0));
  TLeafNode(Result).InitialiseNewNode;
end;

{-------------------------------------------------------------------------------
  Gets a caption to be displayed on the add button menu.
}
function TDomainFolderNode.GetAddButtonMenuCaption(Index: Integer): String;
begin
  case Index of
    0: Result := ResStr_Add;
  else
    raise EAddMenuItemError.Create(ResStr_InvalidAddMenuIndexRequest);
  end;
end;

{-------------------------------------------------------------------------------
  Gets the number of items to be added to the add button menu.
}
function TDomainFolderNode.GetAddButtonMenuCaptionsCount: Integer;
begin
  Result := 1;
end;

{-------------------------------------------------------------------------------
  Gets a value indicating whether the specified item in the add button menu
  should add a node to the tree.
}
function TDomainFolderNode.GetAddMenuIsAdd(AMenuIndex: Integer): Boolean;
begin
  case AMenuIndex of
    0 : Result := True
  else
    Result := False;
  end;
end;

{-------------------------------------------------------------------------------
  Gets the specified child node type.
  Only one type is supported (Index = 0) which will be one of the determination
  leaf node types.
}
function TDomainFolderNode.GetChildNodeType(Index: Integer): TBrowserNodeClass;
var
  lDeterminationFolder: TDeterminationFolderNode;
begin
  if not Assigned(Parent) then
    raise EBrowserNodeError.Create(ResStr_ParentUnassigned);

  if not (Parent is TDeterminationFolderNode) then
    raise EBrowserNodeError.CreateFmt(
        ResStr_ParentWrongType,
        ['TDeterminationFolderNode']);

  lDeterminationFolder := TDeterminationFolderNode(Parent);
  Result := lDeterminationFolder.GetDeterminationLeafNodeType;
end;

{-------------------------------------------------------------------------------
  Gets the number of possible types of child nodes.
}
function TDomainFolderNode.GetChildNodeTypeCount: Integer;
begin
  Result:= 1;
end;

{-------------------------------------------------------------------------------
  Does nothing (required because method is abstract in the parent class).
}
procedure TDomainFolderNode.SetCaption;
begin
end;

{-------------------------------------------------------------------------------
  Initialises the domain folder with caption and domain mask taken from the
  current record of the given recordset.
}
procedure TDomainFolderNode.Initialise(ARecordset: _Recordset);
begin
  Initialise(
      ARecordset.Fields['Item_Name'].Value,
      ARecordset.Fields['Item_Key'].Value);
end;

{-------------------------------------------------------------------------------
  Initialises the domain folder with the given caption and domain mask.
}
procedure TDomainFolderNode.Initialise(const Caption: string;
  DomainMask: Integer);
begin
  inherited Initialise(IntToStr(DomainMask));
  Self.Caption := Caption;
end;

{-------------------------------------------------------------------------------
  Gets the name of the stored procedure that is used to obtain the
  determinations within this domain folder.
}
function TDomainFolderNode.StoredProcByChildType(
  ALeafNodeClass: TLeafNodeClass): String;
begin
  // Key = -1 for the dummy Taxa folder
  if Key<>'-1' then
    Result := 'usp_Determinations_Select_ForSpecimenAndDomain'
  else
    Result := 'usp_DeterminationsRecorder_Select_ForSpecimen';
end;

{-------------------------------------------------------------------------------
  Gets the arguments required to call the stored procedure that is used to
  obtain the determinations within this domain folder. 
}
function TDomainFolderNode.GetStoredProcParams: TVariantArray;
var
  lTopLevelNode: TTopLevelNode;
  lDeterminationFolder: TDeterminationFolderNode;
begin
  lTopLevelNode := TopLevelNode;
  if not (TopLevelNode is TSpecimenTopLevelNode) then
    raise EBrowserNodeError.CreateFmt(
        ResStr_TopLevelNodeWrongType, ['TSpecimenTopLevelNode']);

  lDeterminationFolder := TDeterminationFolderNode(Parent);
  // Key = -1 for the dummy Taxa folder
  if Key<>'-1' then
    Result := VarArrayOf([
        '@SpecimenCollectionUnitKey', lDeterminationFolder.Key,
        '@DomainMask', StrToInt(Key),
        '@LifeSciences', TSpecimenTopLevelNode(lTopLevelNode).LifeSciences])
  else
    Result := VarArrayOf([
        '@ParentKey', lDeterminationFolder.Key,
        '@ShowCommonNames', '1']);
end;

{-------------------------------------------------------------------------------
  Moves the child node that has just been saved into the correct domain folder.
}
function TDomainFolderNode.UpdateNodeRelationship(
  const NewNodeKey: String): String;
var
  lDeterminationFolder: TDeterminationFolderNode;
begin
  lDeterminationFolder := TDeterminationFolderNode(Parent);
  lDeterminationFolder.UpdateNodeRelationship(NewNodeKey);
end;

{-==============================================================================
    TInscriptionLabelLeafNode
===============================================================================}
{-------------------------------------------------------------------------------
}
class function TInscriptionLabelLeafNode.ClassTableName: String;
begin
  Result := TN_SPECIMEN_LABEL;
end;  // TInscriptionLabelLeafNode.ClassTableName 

{-------------------------------------------------------------------------------
}
function TInscriptionLabelLeafNode.GetAssociatedFrame: TBaseDetailFrameClass;
begin
  Result := TfraInscription;
end;  // TInscriptionLabelLeafNode.GetAssociatedFrame 

{-------------------------------------------------------------------------------
}
function TInscriptionLabelLeafNode.GetDomainStoredProc: String;
begin
  Result := 'usp_InscriptionAndLabel_DomainMask_Get';
end;  // TInscriptionLabelLeafNode.GetDomainStoredProc 

{-------------------------------------------------------------------------------
}
function TInscriptionLabelLeafNode.GetImageIndex: Integer;
begin
  Result := 40;
end;  // TInscriptionLabelLeafNode.GetImageIndex 

{-------------------------------------------------------------------------------
}
function TInscriptionLabelLeafNode.GetNodeContext: TNodeContext;
begin
  Result := ncInscriptionLabel;
end;  // TInscriptionLabelLeafNode.GetNodeContext 

{-==============================================================================
    TInscriptionLabelTopLevelNode
===============================================================================}
{-------------------------------------------------------------------------------
}
procedure TInscriptionLabelTopLevelNode.AddChildNodesOfType(ABrowserNodeClass:
    TBrowserNodeClass);
var
  lNewNode: TFolderNode;
begin
  if ABrowserNodeClass = TMultimediaFolderNode then
  begin
    lNewNode:= TFolderNode(Tree.Items.AddTypedChild(Self, ABrowserNodeClass));
    lNewNode.Initialise(Key);
  end else
    raise EBrowserNodeError.Create(ResStr_InvalidChildNodeTypeIndexRequest);
end;  // TInscriptionLabelTopLevelNode.AddChildNodesOfType 

{-------------------------------------------------------------------------------
}
class function TInscriptionLabelTopLevelNode.ClassTableName: String;
begin
  Result := TN_SPECIMEN_LABEL;
end;  // TInscriptionLabelTopLevelNode.ClassTableName 

{-------------------------------------------------------------------------------
}
function TInscriptionLabelTopLevelNode.GetAssociatedFrame: TBaseDetailFrameClass;
begin
  Result := TfraInscription; //Needs to be set to a derived class
end;  // TInscriptionLabelTopLevelNode.GetAssociatedFrame 

{-------------------------------------------------------------------------------
}
function TInscriptionLabelTopLevelNode.GetCanAdd: Boolean;
begin
  Result := False;
end;  // TInscriptionLabelTopLevelNode.GetCanAdd 

{-------------------------------------------------------------------------------
}
function TInscriptionLabelTopLevelNode.GetChildNodeType(Index: Integer): TBrowserNodeClass;
begin
  case Index of
    0: Result := TMultimediaFolderNode;
  else
    raise EBrowserNodeError.Create(ResStr_InvalidChildNodeTypeIndexRequest);
  end;
end;  // TInscriptionLabelTopLevelNode.GetChildNodeType 

{-------------------------------------------------------------------------------
}
function TInscriptionLabelTopLevelNode.GetChildNodeTypeCount: Integer;
begin
  Result := 1;
end;  // TInscriptionLabelTopLevelNode.GetChildNodeTypeCount 

{-------------------------------------------------------------------------------
}
function TInscriptionLabelTopLevelNode.GetDomainStoredProc: String;
begin
  Result := 'usp_InscriptionAndLabel_DomainMask_Get';
end;  // TInscriptionLabelTopLevelNode.GetDomainStoredProc 

{-------------------------------------------------------------------------------
}
function TInscriptionLabelTopLevelNode.GetImageIndex: Integer;
begin
  Result := 40;
end;  // TInscriptionLabelTopLevelNode.GetImageIndex 

{-------------------------------------------------------------------------------
}
function TInscriptionLabelTopLevelNode.GetNodeContext: TNodeContext;
begin
  Result := ncInscriptionLabel;
end;  // TInscriptionLabelTopLevelNode.GetNodeContext 

{-------------------------------------------------------------------------------
}
function TInscriptionLabelTopLevelNode.GetProperty(const AName: string): Variant;
begin
  Result := InternalGetProperty(AName);
end;  // TInscriptionLabelTopLevelNode.GetProperty 

{-------------------------------------------------------------------------------
}
function TInscriptionLabelTopLevelNode.InternalGetProperty(const AName: string): Variant;
begin
  Result := inherited InternalGetProperty(AName);
  
  if Result = Unassigned then
    if AName = PROP_NODE_CONTEXT then
      Result := ncInscriptionLabel;
end;  // TInscriptionLabelTopLevelNode.InternalGetProperty 

{-------------------------------------------------------------------------------
}
function TInscriptionLabelTopLevelNode.StoredProcByChildType(ALeafNodeClass: TLeafNodeClass):
    String;
begin
  Result := '';
  raise EBrowserNodeError.Create(ResStr_InvalidChildNodeTypeIndexRequest);
end;  // TInscriptionLabelTopLevelNode.StoredProcByChildType 

{-==============================================================================
    TInscriptionLabelFolderNode
===============================================================================}
{-------------------------------------------------------------------------------
}
class function TInscriptionLabelFolderNode.ClassTableName: String;
begin
  Result := TN_SPECIMEN_LABEL;
end;  // TInscriptionLabelFolderNode.ClassTableName 

{-------------------------------------------------------------------------------
}
function TInscriptionLabelFolderNode.GetAddButtonMenuCaption(Index: Integer): String;
begin
  case Index of
    0: Result := ResStr_Add;
  else
    raise EAddMenuItemError.Create(ResStr_InvalidAddMenuIndexRequest);
  end;
end;  // TInscriptionLabelFolderNode.GetAddButtonMenuCaption 

{-------------------------------------------------------------------------------
}
function TInscriptionLabelFolderNode.GetAddButtonMenuCaptionsCount: Integer;
begin
  Result := 1;
end;  // TInscriptionLabelFolderNode.GetAddButtonMenuCaptionsCount 

{-------------------------------------------------------------------------------
}
function TInscriptionLabelFolderNode.GetAddMenuIsAdd(AMenuIndex: Integer): Boolean;
begin
  case AMenuIndex of
    0 : Result := True
  else
    Result := False;
  end;
end;  // TInscriptionLabelFolderNode.GetAddMenuIsAdd 

{-------------------------------------------------------------------------------
}
function TInscriptionLabelFolderNode.GetChildNodeType(Index: Integer): TBrowserNodeClass;
begin
  case Index of
    0: Result := TInscriptionLabelLeafNode;
  else
    raise EBrowserNodeError.Create(ResStr_InvalidChildNodeTypeIndexRequest);
  end;
end;  // TInscriptionLabelFolderNode.GetChildNodeType 

{-------------------------------------------------------------------------------
}
function TInscriptionLabelFolderNode.GetChildNodeTypeCount: Integer;
begin
  Result:= 1;
end;  // TInscriptionLabelFolderNode.GetChildNodeTypeCount 

{-------------------------------------------------------------------------------
}
function TInscriptionLabelFolderNode.GetNodeContext: TNodeContext;
begin
  Result := ncInscriptionLabel;
end;  // TInscriptionLabelFolderNode.GetNodeContext 

{-------------------------------------------------------------------------------
}
procedure TInscriptionLabelFolderNode.SetCaption;
begin
  Text := ResStr_InscriptionLabels;
end;  // TInscriptionLabelFolderNode.SetCaption 

{-------------------------------------------------------------------------------
}
function TInscriptionLabelFolderNode.StoredProcByChildType(ALeafNodeClass: TLeafNodeClass):
    String;
begin
  Result := '';
  
  if ALeafNodeClass = TInscriptionLabelLeafNode then
    Result := 'usp_InscriptionsAndLabels_Select_ForSpecimen'
  else
    raise EBrowserNodeError.Create(ResStr_InvalidChildNodeTypeIndexRequest);
end;  // TInscriptionLabelFolderNode.StoredProcByChildType 

end.
