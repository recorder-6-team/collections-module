{===============================================================================
  Unit:        ConceptDetails

  Defines:     TfraConceptDetails

  Description: Details frame for all aspects of a concept

  Created:     June 2003

  Last revision information:
    $Revision: 54 $
    $Date: 12/03/14 11:52 $
    $Author: Andrewkemp $

===============================================================================}
unit ConceptDetails;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, BaseDetailFrameUnit, exgrid, RapTree, StdCtrls, ExtCtrls,
  ActnList, Menus, ImgList, ComCtrls, BaseDragFrameUnit, ResourceStrings,
  ImageListButton, ConceptDetailsNodes, DataTypes, DSSDataTypes, FactGeneral,
  ThesaurusNavigator, DataClasses, LuxembourgConstants, GeneralData,
  ExceptionForm, ADOInt, BaseTabSheetFrameUnit, RelationshipGeneral,
  ConceptOrganiser, UserMessages, BasePageControlFrameUnit, InterfaceDataModule,
  ConceptGeneral, SearchManager, Validation, DropSource, DropTarget;

type
  EConceptDetailsException = class(TExceptionPath)
  end;
  
  {-----------------------------------------------------------------------------
    Details for a selected concept.  Divided into 2 halves, the top half is a
    hierarchical breakdown of all aspects of the concept.  The bottom half
    shows the details of the currently selected aspect.
    When any edit is made to the concept that affects its position within the
    concept group hierarchy (e.g. assigning a parent or children), then when
    the change is saved the Thesaurus Navigator is refreshed to show just the
    single edited concept.  Subsequent navigations will then display the
    correct hierarchy.
  }
  TfraConceptDetails = class(TBaseDetailFrame)
    actMakeSynonym: TAction;
    actMakeSynonymListPreferred: TAction;
    alConcepts: TActionList;
    btnAddDetail: TImageListButton;
    btnDeleteDetail: TImageListButton;
    btnEditDetail: TImageListButton;
    ilHierarchy: TImageList;
    Label1: TLabel;
    mnuMakeConceptSynonym: TMenuItem;
    mnuMakeSynonymListPreferred: TMenuItem;
    pmAddNew: TMenuItem;
    pmConceptHierarchy: TPopupMenu;
    pnlConceptSubdivisions: TPanel;
    pnlDetailsFrame: TPanel;
    Splitter: TSplitter;
    tvHierarchy: TRapidTree;
    actRefresh: TAction;
    Refresh1: TMenuItem;
    mnuMakeConceptHomonym: TMenuItem;
    actMakeHomonym: TAction;
    procedure actMakeSynonymExecute(Sender: TObject);
    procedure actMakeSynonymListPreferredExecute(Sender: TObject);
    procedure btnAddDetailClick(Sender: TObject);
    procedure btnDeleteDetailClick(Sender: TObject);
    procedure btnEditDetailClick(Sender: TObject);
    procedure pmConceptHierarchyPopup(Sender: TObject);
    procedure tvHierarchyChange(Sender: TObject; Node: TFlyNode);
    procedure tvHierarchyChanging(Sender: TObject; Node: TFlyNode; var
            AllowChange: Boolean);
    procedure tvHierarchyDblClick(Sender: TObject);
    procedure tvHierarchyDrawCell(Sender: TObject; aCanvas: TCanvas; ACol,
            ARow: Integer; Rect: TRect; State: TExGridDrawState);
    procedure tvHierarchyExpanding(Sender: TObject; Node: TFlyNode; var
            AllowExpansion: Boolean);
    procedure tvHierarchyKeyDown(Sender: TObject; var Key: Word; Shift:
            TShiftState);
    procedure actRefreshExecute(Sender: TObject);
    procedure actMakeHomonymExecute(Sender: TObject);
  private
    FConceptGroupKey: TKeyString;
    FConceptGroupVersionKey: TKeyString;
    FCurrentFrame: TBaseDetailFrame;
    FDraggedConceptKey: TKeyString;
    FDraggedConceptGroupKey: TKeyString;
    FHierarchyRelationTypeKey: TKeyString;
    FLastNode: TFlyNode;
    FLockedItem: TConceptDetailsItemNode;
    FNodeAllSynonymy: TAllSynonymyListNode;
    FNodeConcept: TConceptDetailsTopNode;
    FNodeConceptGroupVersions: TConceptHistoryListNode;
    FNodeDesignations: TDesignationsListNode;
    FNodeFacts: TFactsListNode;
    FNodeListSynonymy: TListSynonymyListNode;
    FNodeParents: TParentsListNode;
    FNodePotentialSynonymy: TPotentialSynonymsListNode;
    FNodeHomonymy: THomonymsListNode;
    FNodeRelationships: TRelationshipsListNode;
    FNodeTermVersions: TTermVersionsListNode;
    FOnFrameNotfication: TFrameNotificationEvent;
    FTermVersionNode: TTermVersionNode;
    FTreeExpandToggle: Boolean;
    function AddNode(AParent: TFlyNode; const ACaption: string; AType:
            TFlyNodeClass): TFlyNode;
    procedure AddParent;
    procedure CheckNodeCanDrop(APoint: TPoint; const ATable, AKeyField: string;
            var Accept: boolean);
    function ConceptCaption: string;
    function ConstructDeletionMessage(const ANode: TFlyNode): string;
    procedure CreateCommonHierarchyNodes;
    procedure DeleteCurrentFrameContent;
    procedure DestroyContainer;
    procedure FrameNotification(Sender: TObject; AType: TEventType; const
            AParams: TVariantArray);
    function GetNodeTypeName(const ANode: TFlyNode): string;
    procedure PasteNode(const ADraggedKey: TKeyString; ATargetNode: TFlyNode);
    procedure PopulateChildren(AParentNode: TFlyNode);
    procedure RepopulateNode(const ANode: TFlyNode);
    procedure SetDetailForm(ANode: TFlyNode; ReloadControls: Boolean = False);
    procedure SetLockedItem(const Value: TConceptDetailsItemNode);
  protected
    procedure DeleteData; override;
    procedure DragTerm(const Sender: TObject; var oDropSource: TJNCCDropSource);
    procedure DropTerm(const Sender: TObject; const iFormat : integer; const
            iSourceData: TKeyList; const iTextStrings : TstringList; var
            ioHandled : boolean);
    procedure EnableControls(AEnabled: Boolean); override;
    function GetCaption: string; override;
    procedure LoadData; override;
    procedure RegisterDragDropComponents; override;
    procedure SaveData; override;
    procedure WMNavigateSynonym(var Message: TMessage); message
            WM_NAVIGATE_SYNONYM;
  public
    constructor Create(AOwner : TComponent); override;
    procedure AddConceptNode;
    procedure CancelAddNode;
    procedure DoCancel;
    procedure RefreshSynonyms;
    property ConceptGroupKey: TKeyString read FConceptGroupKey write
            FConceptGroupKey;
    property ConceptGroupVersionKey: TKeyString read FConceptGroupVersionKey
            write FConceptGroupVersionKey;
    property CurrentFrame: TBaseDetailFrame read FCurrentFrame write
            FCurrentFrame;
    property HierarchyRelationTypeKey: TKeyString read
            FHierarchyRelationTypeKey write FHierarchyRelationTypeKey;
    property LockedItem: TConceptDetailsItemNode read FLockedItem write
            SetLockedItem;
    property OnFrameNotfication: TFrameNotificationEvent read
            FOnFrameNotfication write FOnFrameNotfication;
  end;

//==============================================================================
implementation

uses
  Relationship, Fact, Designation, TermVersion, ConceptHistory, Concept, Synonym,
  BaseADODataModule, ComObj, GeneralFunctions;

{$R *.dfm}

const
  OVERLAY_INDEX=9;

{-==============================================================================
    TfraConceptDetails
===============================================================================}
{-------------------------------------------------------------------------------
}
constructor TfraConceptDetails.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  
  FLastNode := nil;
  // Create a shortcut overlay in the hierarchy imagelist
  ilHierarchy.Overlay(OVERLAY_INDEX, 0);
end;  // TfraConceptDetails.Create 

{-------------------------------------------------------------------------------
}
procedure TfraConceptDetails.actMakeSynonymExecute(Sender: TObject);
begin
  inherited;

  dmGeneral.RunUpdateStoredProc('usp_MakeConceptsSynonyms_Update',
      ['@ConceptKeySelected', Key,
      '@ConceptKeyPasted', TBaseConceptDetailsNode(tvHierarchy.Selected).Key]);
end;  // TfraConceptDetails.actMakeSynonymExecute

{-------------------------------------------------------------------------------
}
procedure TfraConceptDetails.actMakeSynonymListPreferredExecute(Sender:
        TObject);
begin
  inherited;
  dmGeneral.RunUpdateStoredProc('usp_Concept_Update_MakeListPreferred',
                    ['@Key', TBaseConceptDetailsNode(tvHierarchy.Selected).Key,
                    '@HierarchyRelationTypeKey', HierarchyRelationTypeKey]);
  FrameNotification(Self, etListPreferredChanged, nil);
end;  // TfraConceptDetails.actMakeSynonymListPreferredExecute

{-------------------------------------------------------------------------------
}
procedure TfraConceptDetails.actMakeHomonymExecute(Sender: TObject);
begin
  inherited;
  if tvHierarchy.Selected is TPotentialSynonymNode then
  begin
    dmGeneral.RunStoredProc('usp_HomonymPair_Insert',
        ['@Concept_Key_1', FNodeConcept.Key,
         '@Concept_Key_2', TPotentialSynonymNode(tvHierarchy.Selected).Key]);
  end;
end;  // TfraConceptDetails.actMakeHomonymExecute

{-------------------------------------------------------------------------------
}
procedure TfraConceptDetails.actRefreshExecute(Sender: TObject);
var
  i: Integer;
begin
  inherited;
  with tvHierarchy do
    if Assigned(Selected) then
      if Selected is TConceptDetailsTopNode then begin
        FrameNotification(Self, etRefreshNodeCaption, nil);
        for i := 0 to Selected.Count - 1 do
          RepopulateNode(Selected[i]);
        if Assigned(FCurrentFrame) then FCurrentFrame.ReloadContent;
      end else
      if Selected is TConceptDetailsFolderNode then
        if Selected is TSynonymyListNode then begin
          // The synonymy nodes are a bit special, kind of need to be refreshed in sync.
          RepopulateNode(FNodeListSynonymy);
          RepopulateNode(FNodeAllSynonymy);
          RepopulateNode(FNodePotentialSynonymy);
          RepopulateNode(FNodeHomonymy);
        end else
          RepopulateNode(Selected);
end;  // TfraConceptDetails.actRefreshExecute

{-------------------------------------------------------------------------------
}
procedure TfraConceptDetails.AddConceptNode;
begin
  // So that the Concept frame appears automatically.
  tvHierarchyChange(Self, tvHierarchy.Selected);
  LockedItem := TConceptDetailsItemNode(tvHierarchy.Selected);
  
  // When a concept has been added the common hierarchy nodes have been created,
  // but the 'This Term Version' node needs to be created.
  FTermVersionNode := TTermVersionNode(
            AddNode(FNodeTermVersions, ResStr_ThisTermVersion, TTermVersionNode));
  FTermVersionNode.HasChildren := False;
  TTermVersionNode(FTermVersionNode).IsThisVersion := True;
end;  // TfraConceptDetails.AddConceptNode

{-------------------------------------------------------------------------------
  This method is used to add nodes to the treeview. 
}
function TfraConceptDetails.AddNode(AParent: TFlyNode; const ACaption: string;
        AType: TFlyNodeClass): TFlyNode;
begin
  Result := tvHierarchy.Items.AddTypedChild(AParent, AType);
  Result.Text := ACaption;
  Result.HasChildren := True;
  // Folder nodes don't need a key - they get it from the top level
  if not (Result is TConceptDetailsFolderNode) then
    TBaseConceptDetailsNode(Result).Key := Key;
end;  // TfraConceptDetails.AddNode 

{-------------------------------------------------------------------------------
  Adds a parent record when the add button is pressed on a Parent (folder)
          node.
}
procedure TfraConceptDetails.AddParent;
var
  lCaption, lKey: string;
begin
  DoCheck(lKey, lCaption, stTermInConceptGroup, FConceptGroupKey);
  with tvHierarchy do begin
    if lKey <> '' then begin
      tvHierarchy.Items.BeginUpdate;
      try
        try
          dmGeneral.RunInsertStoredProc(TN_CONCEPT_RELATION,
                                      'usp_ConceptRelation_Insert',
                                      ['@FromConceptKey', lKey,
                                      '@ToConceptKey', Key],
                                      '@Key');
        except
          on E:EOleException do begin
            if CompareText(E.Message, 'Cyclical relationship')=0 then
              raise EConceptDetailsException.CreateNonCritical(Format(
                  ResStr_CyclicRelationship, [ResStr_Parent]), E)
            else
              raise E;
          end;
        end;
        if Selected is TParentsListNode then
          LockedItem := TConceptDetailsItemNode(Items.AddTypedChild
                          (Selected,
                          TConceptDetailsStructuralNode(Selected).ChildNodeType))
        else
          LockedItem := TConceptDetailsItemNode(Items.AddTypedChild
                          (Selected.Parent,
                          TConceptDetailsStructuralNode(Selected.Parent).ChildNodeType));
        LockedItem.Key := lKey;
        LockedItem.Caption := lCaption;
      finally
        tvHierarchy.Items.EndUpdate;
        Selected.Expanded := True;
        tvHierarchy.Selected := LockedItem;
      end;
    end;
  end;
end;  // TfraConceptDetails.AddParent 

{-------------------------------------------------------------------------------
}
procedure TfraConceptDetails.btnAddDetailClick(Sender: TObject);
begin
  inherited;
  with tvHierarchy do begin
    if Assigned(Selected.Parent) then begin
      if (Selected is TParentsListNode) or (Selected is TParentNode) then
        AddParent
      else begin
        tvHierarchy.Items.BeginUpdate;
        try
          if not (Selected is TConceptDetailsTopNode) then begin
            if Selected is TConceptDetailsStructuralNode then
              LockedItem := TConceptDetailsItemNode(Items.AddTypedChild
                              (Selected,
                              TConceptDetailsStructuralNode(Selected).ChildNodeType))
            else
              LockedItem := TConceptDetailsItemNode(Items.AddTypedChild
                              (Selected.Parent,
                              TConceptDetailsStructuralNode(Selected.Parent).ChildNodeType));
            LockedItem.Caption := ResStr_NewNode;
          end; // if not (Selected is TConceptDetailsTopNode)
        finally
          tvHierarchy.Items.EndUpdate;
          if Assigned(LockedItem) then
            tvHierarchy.Selected := LockedItem;
          FrameNotification(Self, etSetEditMode, nil);
          EnableControls(False);
        end;
      end;
    end;
  end; // with tvHierarchy
end;  // TfraConceptDetails.btnAddDetailClick

{-------------------------------------------------------------------------------
}
procedure TfraConceptDetails.btnDeleteDetailClick(Sender: TObject);
var
  lMessage: string;
  lRefreshAll: Boolean;
  lRefreshList: Boolean;
  lRefreshPotentials: Boolean;
  lDeletedNode: TFlyNode;
begin
  inherited;
  lMessage := ConstructDeletionMessage(tvHierarchy.Selected);

  if MessageDlg(lMessage, mtWarning, [mbYes, mbNo], 0) = mrYes then begin
    if Assigned(FCurrentFrame) then begin
      with tvHierarchy do begin
        if Selected is TConceptDetailsItemNode then begin
          with TConceptDetailsItemNode(Selected) do
            DeleteCurrentFrameContent;

          // List synonym will also appear in All synonyms. So refresh regardless.
          lRefreshAll := Selected is TListSynonymNode;
          // Synonym in All list can also appear in List synonyms. So refresh if same CG.
          lRefreshList := (Selected is TAllSynonymNode) and
                          (TAllSynonymNode(Selected).ConceptGroupKey = FConceptGroupKey);
          // Potential Synonyms also need a refresh when dealing with any synonym node.
          lRefreshPotentials := Selected is TSynonymNode;

          lDeletedNode := Selected;
          Selected := Selected.GetPrevVisible;
          lDeletedNode.Delete;
          if lRefreshAll then RepopulateNode(FNodeAllSynonymy);
          if lRefreshList then RepopulateNode(FNodeListSynonymy);
          if lRefreshPotentials then RepopulateNode(FNodePotentialSynonymy);

          if lRefreshAll or lRefreshList or lRefreshPotentials then
            FrameNotification(Self, etRefreshNodeCaption, nil);
        end;  // if Selected
      end;  // with tvHierarchy
    end;  // if Assigned(FCurrentFrame)
  end;  // if MessageDlg
end;  // TfraConceptDetails.btnDeleteDetailClick 

{-------------------------------------------------------------------------------
}
procedure TfraConceptDetails.btnEditDetailClick(Sender: TObject);
begin
  inherited;
  
  if Assigned(FCurrentFrame) then begin
    FrameNotification(Self, etSetEditMode, nil);
    LockedItem := TConceptDetailsItemNode(tvHierarchy.Selected);
    EnableControls(False);
  end;
end;  // TfraConceptDetails.btnEditDetailClick 

{-------------------------------------------------------------------------------
}
procedure TfraConceptDetails.CancelAddNode;
var
  lParentNode: TFlyNode;
begin
  lParentNode := LockedItem.Parent;
  LockedItem.Delete;
  LockedItem := nil;
  FreeAndNil(FCurrentFrame);
  tvHierarchy.Selected := lParentNode;
end;  // TfraConceptDetails.CancelAddNode 

{-------------------------------------------------------------------------------
}
procedure TfraConceptDetails.CheckNodeCanDrop(APoint: TPoint; const ATable,
  AKeyField: String; var Accept: boolean);
var
  lClickPoint: TPoint;
  lNode: TFlyNode;
begin
  Accept := False; // default
                                             
  // Not Concept? No need to go further.
  if not SameText(ATable, TN_CONCEPT) then Exit;

  // Find dragged over node
  lClickPoint := tvHierarchy.ScreenToClient(APoint);
  lNode := tvHierarchy.GetNodeAt(lClickPoint.X, lClickPoint.Y);

  // Update private fields. This is to avoid constantly querying the database.
  if FDraggedConceptKey <> AKeyField then begin
    FDraggedConceptKey := AKeyField;
    FDraggedConceptGroupKey := dmGeneral.GetStoredProcOutputParam(
        'usp_ConceptGroupKey_Get_ForConcept',
        ['@Key', AKeyField], '@ConceptGroupKey');
  end;

  if Assigned(lNode) and (FDraggedConceptKey <> Key) then // Reject D&D on itself.
    Accept := // Accept if List synonym AND same concept group
              (((lNode is TListSynonymNode) or (lNode is TListSynonymyListNode)) and
               (FDraggedConceptGroupKey = FConceptGroupKey)
                and not (tvHierarchy.Selected is THomonymNode))
              or
              // Accept if All Synonym, provided pasted node is not a homonym node
              (((lNode is TAllSynonymNode) or (lNode is TAllSynonymyListNode))
                and not (tvHierarchy.Selected is THomonymNode))
              or
              // Accept
              ((tvHierarchy.Selected is TPotentialSynonymNode)
                and ((lNode is THomonymsListNode) or (lNode is THomonymNode)));
end;  // TfraConceptDetails.CheckNodeCanDrop

{-------------------------------------------------------------------------------
  Returns the caption of the current concept
}
function TfraConceptDetails.ConceptCaption: string;
begin
  Result := dmGeneral.GetStoredProcOutputParam('usp_ConceptItemName_Get', [
      '@Key', Key,
      '@IncludeCommonName', False,
      '@IncludeAuthor', False,
      '@Formatted', True], '@ItemName')
end;  // TfraConceptDetails.ConceptCaption 

{-------------------------------------------------------------------------------
}
function TfraConceptDetails.ConstructDeletionMessage(const ANode: TFlyNode):
        string;
var
  lNodeType, lNodeCaption: string;
begin
  lNodeType := GetNodeTypeName(tvHierarchy.Selected);
  lNodeCaption := StringReplace(tvHierarchy.Selected.Caption, '<i>', '', [rfReplaceAll]);
  lNodeCaption := Trim(StringReplace(lNodeCaption, '</i>', '', [rfReplaceAll]));
  
  if (tvHierarchy.Selected is TSynonymNode) or (tvHierarchy.Selected is TParentNode) then
    Result := Format(ResStr_ConfirmLinkDelete, ['''' + lNodeCaption + ''''])
  else
    Result := Format(ResStr_ConfirmDelete,
                [lNodeType + ' ' + ResStr_Node + ' ''' + lNodeCaption + '''']);
end;  // TfraConceptDetails.ConstructDeletionMessage 

{-------------------------------------------------------------------------------
  Creates the structural nodes that appear in the hierarchy for all concepts. 
}
procedure TfraConceptDetails.CreateCommonHierarchyNodes;
begin
  // Populate the top level concept node.
  with dmGeneral.GetRecordset('usp_Concept_Select', ['@ConceptKey', Key]) do begin
    if not EOF then begin
      FNodeConcept := TConceptDetailsTopNode(AddNode(nil, ConceptCaption,
          TConceptDetailsTopNode));
      FNodeConcept.ConceptGroupKey := VarToStr(Fields['Concept_Group_Key'].Value);
      FNodeConcept.ConceptGroupVersionKey := FConceptGroupVersionKey;
      FNodeConcept.MeaningKey := VarToStr(Fields['Meaning_Key'].Value);
      FNodeConcept.TermVersionKey := VarToStr(Fields['Term_Version_Key'].Value);
    end
    else begin
      FNodeConcept := TConceptDetailsTopNode(AddNode(nil,
                                              ResStr_NewConcept,
                                              TConceptDetailsTopNode));
      FNodeConcept.ConceptGroupKey := FConceptGroupKey;
      FNodeConcept.ConceptGroupVersionKey := FConceptGroupVersionKey;
    end;
  end;
  
  // Populate the structural nodes
  FNodeListSynonymy := TListSynonymyListNode(AddNode(
      FNodeConcept, ResStr_ListSynonyms, TListSynonymyListNode));
  FNodeAllSynonymy := TAllSynonymyListNode(AddNode(
      FNodeConcept, ResStr_AllKnownSynonyms, TAllSynonymyListNode));
  FNodeHomonymy := THomonymsListNode(AddNode(
      FNodeConcept, ResStr_AllKnownHomonyms, THomonymsListNode));
  FNodePotentialSynonymy := TPotentialSynonymsListNode(AddNode(
      FNodeConcept, ResStr_PotentialSynonyms, TPotentialSynonymsListNode));
  FNodeRelationships := TRelationshipsListNode(AddNode(
      FNodeConcept, ResStr_Relationships, TRelationshipsListNode));
  // If the Concept Group is flat, then no need to show the Parents folder
  if HierarchyRelationTypeKey <> '' then
    FNodeParents := TParentsListNode(AddNode(
        FNodeConcept, ResStr_Parents, TParentsListNode));
  FNodeFacts := TFactsListNode(AddNode(
      FNodeConcept, ResStr_Facts, TFactsListNode));
  if VarToStr(dmGeneral.GetStoredProcOutputParam('usp_ConceptDesignationsAvailable_Get',
      ['@Key', FConceptGroupKey], '@DesignationTypesGroupKey')) <> '' then
    FNodeDesignations := TDesignationsListNode(AddNode(
        FNodeConcept, ResStr_Designations, TDesignationsListNode));
  FNodeConceptGroupVersions := TConceptHistoryListNode(AddNode(
      FNodeConcept, ResStr_ConceptHistory, TConceptHistoryListNode));
  FNodeTermVersions := TTermVersionsListNode(
      AddNode(FNodeConcept, ResStr_TermVersions, TTermVersionsListNode));
  
  tvHierarchy.Items[0].Expand(False);
  
  if Key = '' then
    AddConceptNode;
end;  // TfraConceptDetails.CreateCommonHierarchyNodes 

{-------------------------------------------------------------------------------
  Deletes the current frame content, and if necessary updates the top node
          caption.
}
procedure TfraConceptDetails.DeleteCurrentFrameContent;
begin
  FCurrentFrame.DeleteContent;
  if FCurrentFrame is TfraTermVersion then begin
    FNodeConcept.Caption := ConceptCaption;
    tvHierarchy.Invalidate;
  end;
end;  // TfraConceptDetails.DeleteCurrentFrameContent 

{-------------------------------------------------------------------------------
  Override to provide custom delete data code. 
}
procedure TfraConceptDetails.DeleteData;
begin
  // The following line forces the treeview to select its first node, i.e. the
  // Concept node. This is necessary so that the correct Delete method is called.
  // Without it, if the selected node is a Term Version node, and the delete
  // button is pressed, the Term Version's delete method would be called.
  tvHierarchyChange(Self, tvHierarchy.Items.GetFirstNode);
  FCurrentFrame.DeleteContent;
end;  // TfraConceptDetails.DeleteData 

{-------------------------------------------------------------------------------
}
procedure TfraConceptDetails.DestroyContainer;
begin
  FCurrentFrame.Free;
  FCurrentFrame := nil;
end;  // TfraConceptDetails.DestroyContainer 

{-------------------------------------------------------------------------------
  Make the contained frame reload if the cancel button is pressed. 
}
procedure TfraConceptDetails.DoCancel;
begin
  if Assigned(FCurrentFrame) then begin
    { If TermVersion is cancelled, Key may still be empty. So use LoadContent to
      clear the frame properly. }
    if FCurrentFrame.Key = '' then
      FCurrentFrame.LoadContent
    else
      FCurrentFrame.ReloadContent;
  end;
end;  // TfraConceptDetails.DoCancel 

{-------------------------------------------------------------------------------
}
procedure TfraConceptDetails.DragTerm(const Sender: TObject; var oDropSource:
        TJNCCDropSource);
begin
  if Assigned(tvHierarchy.Selected) and (tvHierarchy.Selected is TSynonymNode)
    and not (tvHierarchy.Selected is THomonymNode) then begin
    oDropSource.DropData.SetTable(TN_CONCEPT);
    oDropSource.DropData.AddItem(TSynonymNode(tvHierarchy.Selected).Key, '');
  end;
end;  // TfraConceptDetails.DragTerm 

{-------------------------------------------------------------------------------
}
procedure TfraConceptDetails.DropTerm(const Sender: TObject; const iFormat :
        integer; const iSourceData: TKeyList; const iTextStrings : TstringList;
        var ioHandled : boolean);
var
  lTargetNode: TFlyNode;
  lClickPoint: TPoint;
begin
  lClickPoint := tvHierarchy.ScreenToClient(Mouse.CursorPos);
  lTargetNode := tvHierarchy.GetNodeAt(lClickPoint.X, lClickPoint.Y);

  if Assigned(lTargetNode) then  // Don't want any surprises.
    PasteNode(iSourceData.Items[0].KeyField1, lTargetNode);
end;  // TfraConceptDetails.DropTerm 

{-------------------------------------------------------------------------------
  Controls behaviour when going into edit mode. 
}
procedure TfraConceptDetails.EnableControls(AEnabled: Boolean);
begin
  inherited;
  
  btnAddDetail.Enabled    := AEnabled;
  btnEditDetail.Enabled   := AEnabled;
  btnDeleteDetail.Enabled := AEnabled;

  if Assigned(tvHierarchy.Selected) then
    with TBaseConceptDetailsNode(tvHierarchy.Selected) do begin
      btnAddDetail.Enabled    := btnAddDetail.Enabled and CanAdd;
      btnEditDetail.Enabled   := btnEditDetail.Enabled and CanEdit;
      btnDeleteDetail.Enabled := btnDeleteDetail.Enabled and CanDelete;
    end;

  // Can't use this because you need to be able to drop nodes on it in edit mode.
  // tvHierarchy.Enabled := AEnabled;

  if Assigned(FCurrentFrame) then begin
    if AEnabled then
      FCurrentFrame.EditMode := emBrowse
    else begin
      FCurrentFrame.EditMode := emEdit;
      FCurrentFrame.Perform(WM_NEXTDLGCTL, 0, 0);
    end;
  end;
  pmAddNew.Enabled := btnAddDetail.Enabled;
  actRefresh.Enabled := AEnabled and (tvHierarchy.Selected is TConceptDetailsStructuralNode);
end;  // TfraConceptDetails.EnableControls

{-------------------------------------------------------------------------------
  Wrapper for FOnFrameNotification event
}
procedure TfraConceptDetails.FrameNotification(Sender: TObject; AType:
        TEventType; const AParams: TVariantArray);
begin
  if Assigned(OnFrameNotification) then
    OnFrameNotification(Self, AType, AParams)
end;  // TfraConceptDetails.FrameNotification 

{-------------------------------------------------------------------------------
}
function TfraConceptDetails.GetCaption: string;
begin
  Result := dmGeneral.GetStoredProcOutputParam('usp_ConceptItemName_Get', [
      '@Key', Key,
      '@IncludeCommonName', False,
      '@IncludeAuthor', False,
      '@Formatted', True], '@ItemName')
end;  // TfraConceptDetails.GetCaption 

{-------------------------------------------------------------------------------
  Method to convert the Node's classname to what it actually represents so that
          it can be shown to the user.
}
function TfraConceptDetails.GetNodeTypeName(const ANode: TFlyNode): string;
begin
  if ANode is TConceptDetailsTopNode then
    Result := ResStr_Concept
  else if ANode is TConceptHistoryNode then
    Result := ResStr_ConceptHistory
  else if ANode is TDesignationNode then
    Result := ResStr_Designation
  else if ANode is TFactNode then
    Result := ResStr_Fact
  else if ANode is TParentNode then
    Result := ResStr_Parent
  else if ANode is TRelationshipNode then
    Result := ResStr_Relationship
  else if ANode is TAllSynonymNode then
    Result := ResStr_KnownSynonymNode
  else if ANode is TListSynonymNode then
    Result := ResStr_ListSynonym
  else if ANode is TPotentialSynonymNode then
    Result := ResStr_PotentialSynonym
  else if ANode is THomonymNode then
    Result := ResStr_HomonymNode
  else if ANode is TTermVersionNode then
    Result := ResStr_TermVersion
  else
    Result := '';
end;  // TfraConceptDetails.GetNodeTypeName 

{-------------------------------------------------------------------------------
}
procedure TfraConceptDetails.LoadData;
begin
  inherited;
  tvHierarchy.Items.Clear;
  FLockedItem := nil;
  CreateCommonHierarchyNodes;

  // So that the Concept frame appears automatically.
  tvHierarchyChange(Self, tvHierarchy.Selected);
end;  // TfraConceptDetails.LoadData 

{-------------------------------------------------------------------------------
}
procedure TfraConceptDetails.PasteNode(const ADraggedKey: TKeyString; ATargetNode:
        TFlyNode);
var
  lNode: TFlyNode;
  lSynonymyNode: TFlyNode;
  lDraggedCaption: string;
  lSelectNewNode: Boolean;
  lSameConceptGroup: Boolean;

  // Return the caption with concept group name appended
  function AddConceptGroupName(const ACaption, AKey: string): string;
  begin
    with dmGeneral.GetRecordset('usp_ConceptGroup_Select', ['@Key', AKey]) do
      Result := ACaption + ' (' + VarToStr(Fields['Item_Name'].Value) + ')';
  end;

begin

  // The drop is only allowed on Synonym/Synonymy List nodes, and according to
  // dragged concept's group. So all plain sailing in here, with minimum checks needed.

  // Populate the folder node if necessary.
  if ATargetNode is TSynonymyListNode then begin
    if ATargetNode.Count = 0 then PopulateChildren(ATargetNode);
    lSynonymyNode := ATargetNode;
  end else
    lSynonymyNode := ATargetNode.Parent;

  lDraggedCaption := dmGeneral.GetStoredProcOutputParam('usp_ConceptItemName_Get',
        ['@Key', ADraggedKey, '@IncludeCommonName', 0, '@IncludeAuthor', 1, '@Formatted', 1],
        '@ItemName');

  // Can't drop on Potential Synonyms folder.
  if lSynonymyNode is TListSynonymyListNode then
    AddNode(lSynonymyNode, lDraggedCaption, TListSynonymNode)
  else
  if lSynonymyNode is TAllSynonymyListNode then begin
    // In 'All synonyms' list, include concept group name.
    // FDraggedConceptGroupKey already obtained in CheckNodeCanDrop.
    lDraggedCaption := AddConceptGroupName(lDraggedCaption, FDraggedConceptGroupKey);
    AddNode(lSynonymyNode, lDraggedCaption, TAllSynonymNode);
  end
  else if lSynonymyNode is THomonymsListNode then begin
    lDraggedCaption := AddConceptGroupName(lDraggedCaption, FDraggedConceptGroupKey);
    AddNode(lSynonymyNode, lDraggedCaption, THomonymNode);
  end;

  lSameConceptGroup := FConceptGroupKey = FDraggedConceptGroupKey;

  if lSynonymyNode is THomonymsListNode then begin
    dmGeneral.RunStoredProc('usp_HomonymPair_Insert',
        ['@Concept_Key_1', ADraggedKey,
         '@Concept_Key_2', Key]);
  end
  else begin

    // If Drag/Drop from within same list, then this is a Cut/Paste as synonym.
    if lSameConceptGroup then
      dmGeneral.RunUpdateStoredProc('usp_Concept_Update_ForPreferred',
                                  ['@PastedConceptKey', ADraggedKey,
                                   '@SelectedConceptKey', Key]);
       dmGeneral.RunUpdateStoredProc('usp_MakeConceptsSynonyms_Update',
                                  ['@ConceptKeyPasted', ADraggedKey,
                                   '@ConceptKeySelected', Key]);
     if lSameConceptGroup then
     dmGeneral.RunStoredProc('usp_SynonymChildrenToTargetConcept_Update',
                                  ['@TargetConceptKey', Key,
                                 '@SynonymConceptKey', ADraggedKey]);
  end;

     // These all need refreshing.
  RepopulateNode(FNodeListSynonymy);
  RepopulateNode(FNodeAllSynonymy);
  RepopulateNode(FNodePotentialSynonymy);
  RepopulateNode(FNodeHomonymy);

  lSelectNewNode := True;
  if Assigned(FCurrentFrame) then
    lSelectNewNode := FCurrentFrame.EditMode = emBrowse;

  if lSelectNewNode then begin
    // Now locate the new node and select it.
    lNode := lSynonymyNode.GetFirstChild;
    while Assigned(lNode) do
      if TBaseConceptDetailsNode(lNode).Key = ADraggedKey then
        Break
      else
        lNode := lNode.GetNextSibling;

    if Assigned(lNode) then
      tvHierarchy.Selected := lNode
    else
      tvHierarchy.Selected := lSynonymyNode;
  end;
  
  FrameNotification(Self, etRefreshNodeCaption, nil);
  // Ask container to deal with asking active MDI to either refresh or remove dragged node.
  FrameNotification(Self, etRefreshSynonymNode, VarArrayOf([lSameConceptGroup]));

  Application.ProcessMessages;
end;  // TfraConceptDetails.PasteNode

{-------------------------------------------------------------------------------
}
procedure TfraConceptDetails.pmConceptHierarchyPopup(Sender: TObject);
begin
  if Assigned(tvHierarchy.Selected) then
    with tvHierarchy.Selected do begin
      if ImageIndex = 7 then
        if (Parent is TAllSynonymyListNode) or
           (Parent is TListSynonymyListNode) or
           (Parent is THomonymsListNode) then begin
          actMakeSynonymListPreferred.Visible := True;
          actMakeSynonym.Visible              := False;
          actMakeHomonym.Visible              := False;
        end
        else begin
          actMakeSynonymListPreferred.Visible := False;
          actMakeSynonym.Visible              := True;
          actMakeHomonym.Visible              := True;
        end
      else begin
        actMakeSynonymListPreferred.Visible := False;
        actMakeSynonym.Visible              := False;
        actMakeHomonym.Visible              := False;
      end; // if ImageIndex = 7
    end;  // with
end;  // TfraConceptDetails.pmConceptHierarchyPopup 

{-------------------------------------------------------------------------------
}
procedure TfraConceptDetails.PopulateChildren(AParentNode: TFlyNode);
var
  lParentNode: TConceptDetailsStructuralNode;
  lNewNode: TConceptDetailsItemNode;
  lRecordset: _Recordset;
begin
  tvHierarchy.Items.BeginUpdate;
  try
    lParentNode := TConceptDetailsStructuralNode(AParentNode);
    lRecordset := dmGeneral.GetRecordset(
        lParentNode.ChildrenStoredProc,
        lParentNode.GetProcParams);

    if lRecordset.EOF then
      lParentNode.HasChildren := False
    else
    begin
      while not lRecordset.EOF do begin
        lNewNode := TConceptDetailsItemNode(tvHierarchy.Items.AddTypedChild(
            AParentNode,
            lParentNode.ChildNodeType));
        lNewNode.Initialise(lRecordset);
        lRecordset.MoveNext;
      end; // while
    end;
  finally
    tvHierarchy.Items.EndUpdate;
  end;
end;  // TfraConceptDetails.PopulateChildren

{-------------------------------------------------------------------------------
}
procedure TfraConceptDetails.RefreshSynonyms;
begin
  RepopulateNode(FNodeListSynonymy);
  RepopulateNode(FNodeAllSynonymy);
  RepopulateNode(FNodePotentialSynonymy);
  RepopulateNode(FNodeHomonymy);
end;  // TfraConceptDetails.RefreshSynonyms

{-------------------------------------------------------------------------------
}
procedure TfraConceptDetails.RegisterDragDropComponents;
begin
  inherited;
  RegisterDragComponent(tvHierarchy, DragTerm);
  RegisterDropComponent(tvHierarchy, DropTerm, [TN_CONCEPT], [CF_JNCCDATA], CheckNodeCanDrop);
end;  // TfraConceptDetails.RegisterDragDropComponents 

{-------------------------------------------------------------------------------
  Repopulate the node list in cases where it might have changed (e.g. after
          drag and drop operations which affect synonymy)
}
procedure TfraConceptDetails.RepopulateNode(const ANode: TFlyNode);
var
  lExpanded: Boolean;
begin
  tvHierarchy.Items.BeginUpdate;
  try
    // If no '+' or just '-', node has been expanded.
    lExpanded := ANode.Expanded;

    if (ANode.HasChildren) and (ANode.Count > 0) then
    begin
      // Empty the already populated node
      while Assigned(ANode.GetFirstChild()) do
        ANode.GetFirstChild.Free;
      ANode.HasChildren := True;
    end else
      // Force repopulate next time it is opened
      ANode.HasChildren := True;

    // Repopulate if it was open
    if lExpanded then begin
      ANode.Collapse(True); // Needed, otherwise, '-' showing with no child nodes!
      ANode.Expand(False);
    end;
  finally
    tvHierarchy.Items.EndUpdate;
  end;
end;  // TfraConceptDetails.RepopulateNode 

{-------------------------------------------------------------------------------
}
procedure TfraConceptDetails.SaveData;
begin
  inherited;
  with TBasePageControlFrame(FCurrentFrame) do begin
    SaveContent;
    if (LockedItem is TRelationshipNode) and (FCurrentFrame is TfraRelationship) then
      TRelationshipNode(LockedItem).AppliesTo :=
          TfraRelationship(FCurrentFrame).GetRelationshipType;
    // Update top level caption if required
    if FCurrentFrame is TfraTermVersion then begin
      FNodeConcept.Caption := ConceptCaption;
      tvHierarchy.Invalidate;
    end;
    if Assigned(LockedItem) then begin
      LockedItem.Caption := Caption;
      LockedItem.Key := Key;
    end;
  end;
  if FCurrentFrame is TfraConcept then
    Key := TBasePageControlFrame(FCurrentFrame).Key;
end;  // TfraConceptDetails.SaveData

{-------------------------------------------------------------------------------
}
procedure TfraConceptDetails.SetDetailForm(ANode: TFlyNode; ReloadControls:
        Boolean = False);
var
  lIntf: IAdditionalProperties;
begin
  if not (ANode is TBaseConceptDetailsNode) then begin
    if Assigned(FCurrentFrame) then
      DestroyContainer;
    Exit;
  end;

  if ReloadControls and Assigned(FCurrentFrame) then
    DestroyContainer;
  
  if not Assigned(FCurrentFrame) then begin
    FCurrentFrame := TBaseConceptDetailsNode(ANode).DetailsFrameClass.Create(Self);
  
    if Assigned(FCurrentFrame) then begin
      FCurrentFrame.Parent := pnlDetailsFrame;
      FCurrentFrame.Align  := alClient;
      FCurrentFrame.OnFrameNotification := FrameNotification;
    end;
  end;

  if Supports(TConceptDetailsItemNode(ANode), IID_IAdditionalProperties, lIntf) then begin
    if Assigned(FCurrentFrame.AdditionalProperties) then begin
      if FCurrentFrame.AdditionalProperties <> lIntf then
        FCurrentFrame.SetAdditionalProperties(TConceptDetailsItemNode(ANode));
    end else
      FCurrentFrame.SetAdditionalProperties(TConceptDetailsItemNode(ANode));
  end;
  FCurrentFrame.LoadContent;
end;  // TfraConceptDetails.SetDetailForm 

{-------------------------------------------------------------------------------
}
procedure TfraConceptDetails.SetLockedItem(const Value:
        TConceptDetailsItemNode);
begin
  FLockedItem := Value;
end;  // TfraConceptDetails.SetLockedItem

{-------------------------------------------------------------------------------
  When an item is selected in the hierarchy, ensure the correct frame is loaded
          and populated.
}
procedure TfraConceptDetails.tvHierarchyChange(Sender: TObject; Node: TFlyNode);
begin
  inherited;

  if (Assigned(Node)) then begin
    if (Node is TConceptDetailsItemNode) or (Node is TConceptDetailsTopNode) and
       Assigned(tvHierarchy.Selected) then
    begin
      if Assigned(FCurrentFrame) then begin
        if not (FCurrentFrame.EditMode in [emEdit]) then
          SetDetailForm(Node, not (TBaseConceptDetailsNode(Node).DetailsFrameClass =
            TBaseConceptDetailsNode(tvHierarchy.Selected).DetailsFrameClass));
      end else
        SetDetailForm(Node, not (TBaseConceptDetailsNode(Node).DetailsFrameClass =
          TBaseConceptDetailsNode(tvHierarchy.Selected).DetailsFrameClass));
    end else
      FreeAndNil(FCurrentFrame);
    if Node is TBaseConceptDetailsNode then begin
      btnAddDetail.Enabled := TBaseConceptDetailsNode(Node).CanAdd;
      btnEditDetail.Enabled := TBaseConceptDetailsNode(Node).CanEdit;
      btnDeleteDetail.Enabled := TBaseConceptDetailsNode(Node).CanDelete;
    end;
  end else
    FreeAndNil(FCurrentFrame);
  pmAddNew.Enabled := btnAddDetail.Enabled;

  // Refresh only relevant on top or folder nodes.
  actRefresh.Enabled := Node is TConceptDetailsStructuralNode;
end;  // TfraConceptDetails.tvHierarchyChange

{-------------------------------------------------------------------------------
}
procedure TfraConceptDetails.tvHierarchyChanging(Sender: TObject; Node:
        TFlyNode; var AllowChange: Boolean);
begin
  if Assigned(FCurrentFrame) then
    if FCurrentFrame.EditMode = emBrowse then begin
      if tvHierarchy.Selected <> Node then
        FCurrentFrame.DropInterfaceReferences;
    end else
      AllowChange := False;
end;  // TfraConceptDetails.tvHierarchyChanging 

{-------------------------------------------------------------------------------
  When double clicking on structural nodes, they should expand/contract in the
          same way that clicking the plus/minus button does.
}
procedure TfraConceptDetails.tvHierarchyDblClick(Sender: TObject);
begin
  inherited;
  // As This event is fired twice. Ignore half of them.
  FTreeExpandToggle := not FTreeExpandToggle;
  if FTreeExpandToggle then Exit;
  
  tvHierarchy.Selected.Expanded := not tvHierarchy.Selected.Expanded;
  // Navigation for synonym nodes
  if Assigned(FCurrentFrame) then
    if FCurrentFrame.EditMode = emBrowse then
      if (tvHierarchy.Selected is TAllSynonymNode) or
          (tvHierarchy.Selected is TPotentialSynonymNode) or
          (tvHierarchy.Selected is TParentNode) or
          (tvHierarchy.Selected is THomonymNode) then
        PostMessage(Self.Handle, WM_NAVIGATE_SYNONYM, 0, 0);
end;  // TfraConceptDetails.tvHierarchyDblClick 

{-------------------------------------------------------------------------------
}
procedure TfraConceptDetails.tvHierarchyDrawCell(Sender: TObject; aCanvas:
        TCanvas; ACol, ARow: Integer; Rect: TRect; State: TExGridDrawState);
begin
  inherited;
  Rect.Left := Rect.Left + TCustomRapidTree(Sender).Indent *
                  (TCustomRapidTree(Sender).GetNodeAtRow(ARow).Level + 2) + 2;
  dmInterface.DrawTerm(TRapidTree(Sender).Canvas, Rect,
      TCustomRapidTree(Sender).GetNodeAtRow(ARow).Caption,
      gdSelected in State);
end;  // TfraConceptDetails.tvHierarchyDrawCell 

{-------------------------------------------------------------------------------
}
procedure TfraConceptDetails.tvHierarchyExpanding(Sender: TObject; Node:
        TFlyNode; var AllowExpansion: Boolean);
begin
  inherited;
  
  if (Node is TConceptDetailsStructuralNode) and (Node.HasChildren) and (Node.Count = 0) then
    PopulateChildren(Node)
end;  // TfraConceptDetails.tvHierarchyExpanding 

{-------------------------------------------------------------------------------
}
procedure TfraConceptDetails.tvHierarchyKeyDown(Sender: TObject; var Key: Word;
        Shift: TShiftState);
begin
  inherited;
  if Key=VK_INSERT then begin
    btnAddDetail.Click;
    Key := 0;
  end
  else if Key=VK_DELETE then begin
    btnDeleteDetail.Click;
    Key := 0;
  end;
end;  // TfraConceptDetails.tvHierarchyKeyDown 

{-------------------------------------------------------------------------------
  During the double click of a node, its not a good idea to destroy the node.
          This method allows delayed navigation after a double click on a
          synonym node.
}
procedure TfraConceptDetails.WMNavigateSynonym(var Message: TMessage);
begin
  FrameNotification(Self, etNavigation, VarArrayOf([TBaseConceptDetailsNode(
      tvHierarchy.Selected).Key]))
end;  // TfraConceptDetails.WMNavigateSynonym


end.
