{===============================================================================
  Unit:        ThesaurusNavigatorEditable

  Defines:     TfraThesaurusNavigatorEditable

  Description: An editable version of the ThesaurusNavigator for use in
               the Thesaurus Editor (not Thesaurus Browser).

  Model:       ThesaurusNavigator.mpb

  Created:     February 2004

  Last revision information:
    $Revision: 28 $
    $Date: 30/08/11 15:17 $
    $Author: Jamesbichard $

===============================================================================}

unit ThesaurusNavigatorEditable;

interface

uses
  SysUtils, Windows, Messages, Classes, Graphics, Controls, Variants, StrUtils,
  Forms, Dialogs, ThesaurusNavigator, GeneralData, DataClasses, ResourceStrings,
  LuxembourgConstants, DropTarget, RapTree, Menus, BaseDragFrameUnit,
  DomainConceptGroupSelector, Buttons, StdCtrls, exgrid, UserMessages,
  ExceptionForm, ExtCtrls, Types;

resourcestring
  ResStr_PastingNodes = 'Pasting Nodes';
  ResStr_UpdatingRanks = 'Updating Ranks';

type
  EThesaurusNavigatorEditable = class(TExceptionPath)
  end;

  TfraThesaurusNavigatorEditable = class(TfraThesaurusNavigator)
    pmSearchModeUniqueConcepts: TMenuItem;
    pmSearchModeNonSynchronisedConcepts: TMenuItem;
    procedure pmSearchModeUniqueConceptsClick(Sender: TObject);
    procedure pmSearchModeNonSynchronisedConceptsClick(Sender: TObject);
  private
    procedure FixupRanks;
    procedure PasteAsSynonym(const ADraggedKey: TKeyString);
    function PasteNode(const AKey :TKeyString; const AParentKey : TKeyString =
        ''): string;  
    procedure PopulateNode(AKey : TKeyString; AParentNode : TTermNode); overload;
  protected
    procedure CheckNodeCanDrop(APoint: TPoint; const ATable, AKeyField: string; var
        Accept: boolean); override;
    procedure DropTerm(const Sender: TObject; const iFormat : integer; const
        iSourceData: TKeyList; const iTextStrings : TstringList; const
        iIsPasteOperation: boolean; var ioHandled : boolean); override;
    function GetConceptGroupKey(const AConceptKey: String): String;
    procedure NodeCopy; override;
    function CanDrop : Boolean; override;
    function GetTableList : TStringDynArray; override;
  public
    procedure InsertOrUpdateConceptRelation(const AKey :TKeyString; const
        AParentKey : TKeyString = '');
  end;

//==============================================================================
implementation

uses
  ThesaurusEditorMain, ConceptOrganiser, ComObj, GeneralFunctions, ConceptDetails,
  Concept;

{$R *.dfm}

{-==============================================================================
    TfraThesaurusNavigatorEditable
===============================================================================}
{-------------------------------------------------------------------------------
}
procedure TfraThesaurusNavigatorEditable.CheckNodeCanDrop(APoint: TPoint; const
    ATable, AKeyField: string; var Accept: boolean);
var
  lClickPoint: TPoint;
  lNode: TFlyNode;
begin
  Accept := (HierarchyRelationTypeKey <> '') and (ATable = TN_CONCEPT);

  // Find dragged over node, if any and check dragged node is different
  lClickPoint := tvHierarchy.ScreenToClient(APoint);
  lNode       := tvHierarchy.GetNodeAt(lClickPoint.X, lClickPoint.Y);
  if Assigned(lNode) then
    Accept := Accept and (TTermNode(lNode).ConceptKey <> AKeyField);
end;  // TfraThesaurusNavigatorEditable.CheckNodeCanDrop  

{-------------------------------------------------------------------------------
  Gets an array of all types which may be dragged into this drop target.
}
function TfraThesaurusNavigatorEditable.GetTableList : TStringDynArray;
begin
  SetLength(Result, 1);
  Result[0] := TN_CONCEPT;
end;

{-------------------------------------------------------------------------------
}
function TfraThesaurusNavigatorEditable.CanDrop : Boolean;
begin
  Result := True;
end;  // TfraThesaurusNavigatorEditable.CanDrop

{-------------------------------------------------------------------------------
}
procedure TfraThesaurusNavigatorEditable.DropTerm(const Sender: TObject; const
    iFormat : integer; const iSourceData: TKeyList; const iTextStrings :
    TstringList; const iIsPasteOperation: boolean; var ioHandled : boolean);
var
  lRecursionPassed: Boolean;
  lDraggedConceptGroupKey, lTargetConceptGroupKey: String;
  lNewNodeKey: string;
begin
  inherited;

  if FIsDrag then begin
    // If user dropped node on empty space, then paste as top level.
    IsPasteAsTopLevel := not Assigned(FTargetNode);
    { Paste as Synonym is only reachable through menu, not Drag/Drop within the tree,
      so reset the flag to avoid weird behaviour. }
    IsPasteAsSynonym := False;
  end else begin
    if IsPasteAsTopLevel then
      FTargetNode := nil
  end;

  // Check for and prevent circular reference.
  if not IsPasteAsTopLevel then begin
    lRecursionPassed := (dmGeneral.GetStoredProcOutputParam(
                    'usp_Concept_RecursionCheck_Get',
                    ['@PotentialChildKey', iSourceData.Items[0].KeyField1,
                     '@PotentialParentKey', TTermNode(FTargetNode).ConceptKey],
                    '@RecursionExists') = 0);
    ValidateValue(lRecursionPassed, ResStr_RecursionFailureCircularConceptRelation);
  end;

  // Check if Source node needs to be deleted from Source list.
  lDraggedConceptGroupKey := GetConceptGroupKey(iSourceData.Items[0].KeyField1);
  // Get the Concept Group from selector directly, in case tree is empty.
  lTargetConceptGroupKey := fraDomainConceptGroupsSelector.ConceptGroupKey;

  { If Drag/Drop, need to check if same concept groups. If so, drop will be a cut/paste, and
    if not, drop will be a copy/paste.}
  if FIsDrag then
    IsCut := lDraggedConceptGroupKey = lTargetConceptGroupKey;

  // Paste node.
  lNewNodeKey := PasteNode(iSourceData.Items[0].KeyField1, iSourceData.Items[0].KeyField2);

  if IsCut then
    { Refresh the parent of the dragged node. It should still be the selected one.
      As it could be that it comes from another ConceptOrganiser, use ActiveMDIChild
      and not Self, although ActiveMDIChild could be Self. Win-Win situation! }
    with frmThesaurusEditor do
      if ActiveMDIChild is TfrmConceptOrganiser then
        TfrmConceptOrganiser(ActiveMDIChild).RemoveCutNode;

  // Ensure the tree view is up to date.
  if IsPasteAsTopLevel then
    PopulateNode(lNewNodeKey, nil)
  else begin
    tvHierarchy.Selected := FTargetNode;
    pmNodeRefreshNodeChildrenClick(Self);
    FTargetNode.Expanded := True;
    // Locate and select the pasted node.
    FTargetNode := FTargetNode.GetFirstChild;
    while Assigned(FTargetNode) do
      if TTermNode(FTargetNode).ConceptKey = lNewNodeKey then
        Break
      else
        FTargetNode := FTargetNode.GetNextSibling;
    if Assigned(FTargetNode) then
      tvHierarchy.Selected := FTargetNode;
  end;
end;  // TfraThesaurusNavigatorEditable.DropTerm

{-------------------------------------------------------------------------------
}
function TfraThesaurusNavigatorEditable.GetConceptGroupKey(const AConceptKey: String): String;
begin
  Result := VarToStr(dmGeneral.GetStoredProcOutputParam(
                'usp_ConceptGroupKey_Get_ForConcept', ['@Key', AConceptKey],
                '@ConceptGroupKey'));
end;  // TfraThesaurusNavigatorEditable.GetConceptGroupKey

{-------------------------------------------------------------------------------
}
procedure TfraThesaurusNavigatorEditable.InsertOrUpdateConceptRelation(const
    AKey :TKeyString; const AParentKey : TKeyString = '');
var
  lTargetConceptKey: TKeyString;
begin
  try
    lTargetConceptKey := TTermNode(FTargetNode).ConceptKey;
    if FIsDrag then begin
      // Delete the previous Concept_Relation record (if there was one).
      dmGeneral.RunStoredProc('usp_ConceptRelation_Delete_ForPaste',
                ['@FromKey', AParentKey,
                '@ToKey', AKey]);
      dmGeneral.RunInsertStoredProc(TN_CONCEPT_RELATION,
                'usp_ConceptRelation_Insert',
                ['@FromConceptKey', lTargetConceptKey,
                //'@FromConceptKey', AParentKey,
                '@ToConceptKey', AKey,
                '@ThesaurusRelationTypeKey', HierarchyRelationTypeKey],
                '@Key');
    end
    // If the concept on the clipboard was a top level node, it has no
    // Concept_Relation record to give it a parent. As it is now becoming a child
    // node, a record must be inserted in the Concept_Relation table. Also,
    // if the node was copied it will need a new relation to join it to its
    // parent node.
    else if (AParentKey = '') or (not IsCut) then begin
      dmGeneral.RunInsertStoredProc(TN_CONCEPT_RELATION,
                'usp_ConceptRelation_Insert',
                ['@FromConceptKey', lTargetConceptKey,
                '@ToConceptKey', AKey,
                '@ThesaurusRelationTypeKey', HierarchyRelationTypeKey],
                '@Key');
    end
    // If the concept on the clipboard was already a child node, just update
    // the currently existing Concept_Relation record.
    else
      dmGeneral.RunUpdateStoredProc('usp_ConceptRelation_Update_ForCut',
            ['@OldParentConceptKey', AParentKey,
             '@NewParentConceptKey', lTargetConceptKey,
             '@ToConceptKey', AKey]);
  except
    on E:EOleException do
      raise EThesaurusNavigatorException.CreateNonCritical(
          ResStr_UpdateConceptRelationFailed);
  end;
end;  // TfraThesaurusNavigatorEditable.InsertOrUpdateConceptRelation 

{-------------------------------------------------------------------------------
}
procedure TfraThesaurusNavigatorEditable.NodeCopy;
var
  i: Integer;
begin
  inherited NodeCopy;
  
  // Need to let all Concept Organisers know that we have copied the node.
  // This can only be done by Concept Organiser, so need to fire an event
  // to let it know.
  with frmThesaurusEditor do
    for i := 0 to MDIChildCount-1 do
      if MDIChildren[i] is TfrmConceptOrganiser then
        TfrmConceptOrganiser(MDIChildren[i]).NodeIsCopied(True);
end;  // TfraThesaurusNavigatorEditable.NodeCopy 

{-------------------------------------------------------------------------------
}
procedure TfraThesaurusNavigatorEditable.PasteAsSynonym(const ADraggedKey:
    TKeyString);
var
  lTargetConceptKey, lTargetConceptGroupKey, lDraggedConceptGroupKey: TKeyString;
  lConceptsToUpdate: TStringList;
begin
  lTargetConceptKey := TTermNode(FTargetNode).ConceptKey;
  // Two system concepts cannot be made synonyms, so check they aren't.
  if (LeftStr(ADraggedKey, 8) = 'SYSTEM00') and
     (LeftStr(lTargetConceptKey, 8) = 'SYSTEM00') then
    raise EThesaurusNavigatorException.CreateNonCritical
                  (ResStr_SystemConceptsCannotBeSynonyms)
  else begin
    lDraggedConceptGroupKey := GetConceptGroupKey(ADraggedKey);
    lTargetConceptGroupKey := GetConceptGroupKey(lTargetConceptKey);

    // Cut/Paste, update pasted node concept group and stuff
    if IsCut then begin
      dmGeneral.GetStoredProcOutputParam('usp_Concept_Paste',
          ['@ConceptKey', ADraggedKey,
          '@DestConceptGroupKey', lTargetConceptGroupKey,
          '@DestParentConceptKey', '',
          '@IsCut', 1], '@DestConceptKey');
      // Cut/Paste across lists, ensure concept ranks copy correctly.
      if lDraggedConceptGroupKey <> lTargetConceptGroupKey then
        dmGeneral.RunStoredProc(
            'usp_ConceptRanks_FixDomain',
            ['@FixDomainKey', DomainKey,
             '@SystemSuppliedData', 0]);
    end;

    // If the two concepts have the same concept group then the list_preferred
    // field of the pasted node will need to be updated and the preferred field
    // might need to be updated.
    if IsCut then 
      dmGeneral.RunUpdateStoredProc('usp_Concept_Update_ForPreferred',
                                  ['@PastedConceptKey', ADraggedKey,
                                   '@SelectedConceptKey', lTargetConceptKey]);

    dmGeneral.RunUpdateStoredProc('usp_MakeConceptsSynonyms_Update',
                                  ['@ConceptKeyPasted', ADraggedKey,
                                   '@ConceptKeySelected', lTargetConceptKey]);
    // Make the children of the node on the clipboard become children of the
    // target concept. Uses RunStoredProc rather than RunUpdateStoredProc
    // because the proc. might not actually update any records. If it didn't,
    // RunUpdateStoredProc would raise an unwanted non-critical error.
    if IsCut then
      dmGeneral.RunStoredProc('usp_SynonymChildrenToTargetConcept_Update',
                                  ['@TargetConceptKey', lTargetConceptKey,
                                   '@SynonymConceptKey', ADraggedKey]);
    // Need to refresh the node children because the target node might need to
    // add the node on the clipboard's children.
    pmNodeRefreshNodeChildrenClick(Self);

    lConceptsToUpdate := TStringList.Create;
    lConceptsToUpdate.Add(TTermNode(FTargetNode).ConceptKey);
    RefreshNodeCaption(FTargetNode, lConceptsToUpdate);
    lConceptsToUpdate.Free;
  end;
end;  // TfraThesaurusNavigatorEditable.PasteAsSynonym

{-------------------------------------------------------------------------------
  AKey is the Concept Key of the node on the clipboard, i.e. the
      dragged/copied/to-be-pasted node.
  AParentKey is the Concept Key of the parent of the dragged node. This is also
      on the clipboard.
  FTargetNode is the Concept Key of the node that the user has dropped the node
      on / pasted the node on.
}
function TfraThesaurusNavigatorEditable.PasteNode(const AKey :TKeyString;
    const AParentKey : TKeyString = ''): string;
var
  lTargetKey: Variant;
  lCursor: TCursor;
begin
  lCursor := HourglassCursor;
  frmThesaurusEditor.SetStatus(ResStr_PastingNodes + '...', False);
  try
    if IsPasteAsSynonym then begin
      PasteAsSynonym(AKey);
      Result := AKey;
    end else begin
      if (not IsPasteAsTopLevel) and (HierarchyRelationTypeKey = '') then
        raise EThesaurusNavigatorException.CreateNonCritical
                                    (ResStr_ConceptGroupHasNoHierarchyRelation);
      if Assigned(FTargetNode) then
        lTargetKey := TTermNode(FTargetNode).ConceptKey
      else
        lTargetKey := Null;

      Result := dmGeneral.GetStoredProcOutputParam('usp_Concept_Paste',
          ['@ConceptKey', AKey,
          '@DestConceptGroupKey', ConceptGroupKey,
          '@DestParentConceptKey', lTargetKey,
          '@IsCut', IsCut], '@DestConceptKey');
    end;
    FixupRanks;
  finally
    frmThesaurusEditor.SetStatus('', False);
    DefaultCursor(lCursor);
  end;
end;  // TfraThesaurusNavigatorEditable.PasteNode

{-------------------------------------------------------------------------------
  When pasting concepts, ensure ranks are also copied over into the Domain
}
procedure TfraThesaurusNavigatorEditable.FixupRanks;
begin
  frmThesaurusEditor.SetStatus(ResStr_UpdatingRanks + '...', False);
  // ensure concept ranks copy correctly
  dmGeneral.RunStoredProc(
      'usp_ConceptRanks_FixDomain',
      ['@FixDomainKey', DomainKey,
       '@SystemSuppliedData', 0]);
  // update rank combo box
  with frmThesaurusEditor do
    if ActiveMDIChild is TfrmConceptOrganiser then
      if TfrmConceptOrganiser(ActiveMDIChild).CurrentFrame is TfraConceptDetails then
        with TfraConceptDetails(TfrmConceptOrganiser(ActiveMDIChild).CurrentFrame) do
          if CurrentFrame is TfraConcept then
            TfraConcept(CurrentFrame).RefreshRankCombo;
end;

{-------------------------------------------------------------------------------
}
procedure TfraThesaurusNavigatorEditable.PopulateNode(AKey : TKeyString;
    AParentNode : TTermNode);
var
  lPastedNode: TTermNode;
begin
  // Give the node the information it requires.
  with dmGeneral.GetRecordset('usp_Concept_Select', ['@ConceptKey', AKey]) do begin
    lPastedNode := TTermNode(tvHierarchy.Items.AddTypedChild(AParentNode, TTermNode));
    lPastedNode.Text := VarToStr(Fields['Item_Name'].Value);
    lPastedNode.HasChildren := Fields['HasChildren'].Value;
    lPastedNode.ConceptKey := AKey;
    if not VarIsNull(Fields['Concept_Rank_Key'].Value) then
      lPastedNode.Rank := FConceptRankCache.GetRank(Fields['Concept_Rank_Key'].Value);
  end;
  // Now select the pasted node.
  tvHierarchy.Selected := TFlyNode(lPastedNode);
end;  // TfraThesaurusNavigatorEditable.PopulateNode

{-------------------------------------------------------------------------------
}
procedure TfraThesaurusNavigatorEditable.pmSearchModeUniqueConceptsClick(
  Sender: TObject);
begin
  SetShowHierarchy(False);
  if fraDomainConceptGroupsSelector.btnHistory.Down then
  begin
    FetchConcepts(
      'usp_Concept_Select_VersionUnique',
      ['@ConceptGroupVersionKey', ConceptGroupVersionKey],
      False);
  end
  else begin
    FetchConcepts(
      'usp_Concept_Select_Unique',
      ['@ConceptGroupKey', ConceptGroupKey],
      False);
  end;
end;

{-------------------------------------------------------------------------------
}
procedure TfraThesaurusNavigatorEditable.pmSearchModeNonSynchronisedConceptsClick(
  Sender: TObject);
begin
  SetShowHierarchy(False);
  if fraDomainConceptGroupsSelector.btnHistory.Down then
  begin
    FetchConcepts(
      'usp_Concept_Select_VersionNonSynchronised',
      ['@ConceptGroupVersionKey', ConceptGroupVersionKey],
      False);
  end
  else begin
    FetchConcepts(
      'usp_Concept_Select_NonSynchronised',
      ['@ConceptGroupKey', ConceptGroupKey],
      False);
  end;
end;

end.

