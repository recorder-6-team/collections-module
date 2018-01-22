{===============================================================================
  Unit:        ConceptOrganiser

  Defines:     TfrmConceptOrganiser

  Description: MDI Child that allows concepts and related information to be
               set up.

  Created:     18/7/2003

  Last revision information:
    $Revision: 68 $
    $Date: 30/08/11 15:22 $
    $Author: Jamesbichard $

===============================================================================}
unit ConceptOrganiser;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, BaseMDIChildEditorUnit, StdCtrls, ImageListButton, ExtCtrls, ActnList,
  ImgList, Menus, exgrid, RapTree, ThesaurusEditorData, BaseDragFrameUnit,
  ThesaurusNavigator, DataTypes, UserMessages, GeneralData, ResourceStrings,
  BaseTabSheetFrameUnit, ExceptionForm, ClipBrd, ThesaurusNavigatorEditable,
  LuxembourgConstants, DataClasses, DSSDataTypes, BaseNavigatorFrame;

type
  {-----------------------------------------------------------------------------
    MDI child screen that allows concepts to be created, edited and organised
    into concept groups. The popup menu items documented below and names
    pmNode… are available from the treeview in addition to those declared
    within the Thesaurus Navigator frame.  The popup menu items below named
    pmAdd… are available from the Add menu button.
  }
  TfrmConceptOrganiser = class(TBaseMDIChildEditor)
    Bevel1: TBevel;
    fraThesaurusNavigator: TfraThesaurusNavigatorEditable;
    N1: TMenuItem;
    pmAdd: TPopupMenu;
    pmAddChildConcept: TMenuItem;
    pmAddConcept: TMenuItem;
    pmNodeCut: TMenuItem;
    pmNodeDelete: TMenuItem;
    pmNodePaste: TMenuItem;
    pmNodePasteAsSynonym: TMenuItem;
    pmNodePasteAsTopLevel: TMenuItem;
    procedure actMakeSynonymPreferredExecute(Sender: TObject);
    procedure btnAddClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure fraDomainConceptGroupsSelectorcmbConceptGroupsChange(Sender:
            TObject);
    procedure fraThesaurusNavigatorpmNodePopup(Sender: TObject);
    procedure fraThesaurusNavigatorpmSearchModePopulateTopLevelClick(Sender:
            TObject);
    procedure fraThesaurusNavigatortvHierarchyChange(Sender: TObject; Node:
            TFlyNode);
    procedure fraThesaurusNavigatortvHierarchyChanging(Sender: TObject; Node:
            TFlyNode; var AllowChange: Boolean);
    procedure fraThesaurusNavigatortvHierarchyDblClick(Sender: TObject);
    procedure pmAddChildConceptClick(Sender: TObject);
    procedure pmAddConceptClick(Sender: TObject);
    procedure pmNodeCutClick(Sender: TObject);
    procedure pmNodeDeleteClick(Sender: TObject);
    procedure pmNodePasteAsSynonymClick(Sender: TObject);
    procedure pmNodePasteAsTopLevelClick(Sender: TObject);
    procedure pmNodePasteClick(Sender: TObject);
    procedure pmNodeCopyClick(Sender: TObject);
  private
    FTreeExpandToggle: Boolean;
    FCopiedNodeKey: string;
    procedure FrameNotification(Sender: TObject; AType: TEventType; const
            AParams: TVariantArray);
    function GetConceptGroupKey: TKeyString;
    function GetConceptGroupVersionKey: TKeyString;
    procedure ListPreferredChanged;
    function NeedRepopulate(ANode: TTermNode): Boolean;
    procedure RefreshNodeCaption;
    procedure SetAddDeleteButtons(const ANode: TFlyNode = nil);
    function ConceptIsContainedInGroup(
        AConceptKey: String;
        AConceptGroupKey: String): Boolean;
    procedure PasteNode(
      AIsPasteAsSynonym: Boolean;
      AIsPasteAsTopLevel: Boolean;
      ACheckForSameConceptGroup: Boolean);
  protected
    FLockedItem: TTermNode;
    FLockedParentItem: TTermNode;
    function AskSaveBeforeClosing: Boolean; override;
    procedure DoAddConcept(AIsTopLevel : Boolean);
    procedure DoCancel; override;
    procedure DoDelete; override;
    procedure DoSave; override;
    procedure SetEditMode(Value: TEditMode); override;
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
    function GetCurrentItemDescription: string; override;
    function GetKey(const ATableName: String): String;
    procedure NodeIsCopied(ANodeIsCopied: Boolean);
    procedure RemoveCutNode;
  end;

//==============================================================================
implementation

{$R *.dfm}

uses
  ConceptDetails, InterfaceDataModule, BaseDetailFrameUnit, ConceptDetailsNodes,
  DomainConceptGroupSelector, ComboListID, ThesaurusEditorMain, GeneralFunctions,
  BaseADODataModule;

{-==============================================================================
    TfrmConceptOrganiser
===============================================================================}
{-------------------------------------------------------------------------------
  Constructor. We don't want the main edit button to be enabled. The smaller
          edit button in the Concept Details frame should be used.
}
constructor TfrmConceptOrganiser.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  fraThesaurusNavigator.ShowHierarchy := True;
  EditMode := emBrowse;                       
end;  // TfrmConceptOrganiser.Create

{-------------------------------------------------------------------------------
}
destructor TfrmConceptOrganiser.Destroy;
begin
  if Assigned(CurrentFrame) then
    CurrentFrame.DropInterfaceReferences;
  inherited;
end;  // TfrmConceptOrganiser.Destroy

{-------------------------------------------------------------------------------
}
procedure TfrmConceptOrganiser.actMakeSynonymPreferredExecute(Sender: TObject);
begin
  inherited;
  MessageDlg(ResStr_ConceptsSynonymPreferred, mtConfirmation, [mbOk], 0);
end;  // TfrmConceptOrganiser.actMakeSynonymPreferredExecute 

{-------------------------------------------------------------------------------
  Because the Concept Organiser contains the Concept Details frame, which
          contains frames itself, we need an override here to ask the user if
          they want to save any changes they may have made, if they are in edit
          mode when they click on the close button.
}
function TfrmConceptOrganiser.AskSaveBeforeClosing: Boolean;
var
  lResult: TModalResult;
begin
  Result := True; // default
  if Assigned(CurrentFrame) and Assigned(TfraConceptDetails(CurrentFrame).CurrentFrame) then
    if TfraConceptDetails(CurrentFrame).CurrentFrame.EditMode = emEdit then begin
      lResult :=  MessageDlg(ResStr_FrameToCloseInEditMode, mtWarning,
                                         [mbYes, mbNo, mbCancel], 0);
      if lResult = mrYes then begin
        DoSave
      end
      else if lResult = mrCancel then
        Result := False;
    end;
end;  // TfrmConceptOrganiser.AskSaveBeforeClosing 

{-------------------------------------------------------------------------------
}
procedure TfrmConceptOrganiser.btnAddClick(Sender: TObject);
var
  lPosPopup: TPoint;
begin
  if (fraThesaurusNavigator.HierarchyRelationTypeKey = '')
     or not Assigned(fraThesaurusNavigator.tvHierarchy.Selected) then
  begin
    DoAddConcept(True);
  end
  else begin
    // Work out where to show the popupmenu so that it appears just under
    // the button
    lPosPopup := btnAdd.ClientToScreen(Point(0, btnAdd.Height));
    pmAdd.Popup(lPosPopup.X, lPosPopup.Y);
  end;
end;  // TfrmConceptOrganiser.btnAddClick 

{-------------------------------------------------------------------------------
}
procedure TfrmConceptOrganiser.DoAddConcept(AIsTopLevel : Boolean);
begin
  if Assigned(FCurrentFrame) then
  begin
    FCurrentFrame.DropInterfaceReferences;
    FreeAndNil(FCurrentFrame);
  end;
  with fraThesaurusNavigator do begin
    if AIsTopLevel then
      CreateNewNode(nil)
    else
      CreateNewNode(TTermNode(tvHierarchy.Selected));
    FLockedItem := TTermNode(tvHierarchy.Selected);
    if Assigned(tvHierarchy.Selected.Parent) then
      FLockedParentItem := TTermNode(tvHierarchy.Selected.Parent);
  end;
  SetEditMode(emEdit);
end;  // TfrmConceptOrganiser.DoAddConcept 

{-------------------------------------------------------------------------------
  Cancel an add or edit. 
}
procedure TfrmConceptOrganiser.DoCancel;
var
  lFrame: TfraConceptDetails;
begin
  // Not inherited - Don't want main edit button to become enabled change back.
  if Assigned(FCurrentFrame) and (FCurrentFrame is TfraConceptDetails) then begin
    lFrame := TfraConceptDetails(FCurrentFrame);
    if Assigned(lFrame.LockedItem) then begin
      // See if the concept details node add was cancelled, so delete new node
      if (lFrame.LockedItem.Key = '') and not (lFrame.LockedItem is TTermVersionNode) then
        lFrame.CancelAddNode;
      // Is user decides to cancel adding whole concept
      if (lFrame.Key = '')  then
      begin
        FCurrentFrame.DropInterfaceReferences;
        FreeAndNil(FCurrentFrame);
      end;
    end;
  end;

  if Assigned(FLockedItem) then
    if FLockedItem.ConceptKey = '' then
      FreeAndNil(FLockedItem)
    else begin
      fraThesaurusNavigator.tvHierarchy.Selected := FLockedItem;
      FLockedItem := nil;
    end;
  if Assigned(FLockedParentItem) then FLockedParentItem := nil;
  
   SetEditMode(emBrowse);
  if Assigned(FCurrentFrame) and (FCurrentFrame is TfraConceptDetails) then
    TfraConceptDetails(FCurrentFrame).DoCancel;
end;  // TfrmConceptOrganiser.DoCancel 

{-------------------------------------------------------------------------------
  Delete a concept node. 
}
procedure TfrmConceptOrganiser.DoDelete;
var
  lCursor: TCursor;
begin
  inherited;
  ValidateValue(fraThesaurusNavigator.tvHierarchy.Selected.HasChildren = False,
                ResStr_CannotBeDeletedNodeHasChildren);

  if MessageDlg(ResStr_ConfirmNodeDelete, mtWarning, [mbYes, mbNo], 0) = mrYes then begin
    lCursor := HourglassCursor;
    try
      if Assigned(fraThesaurusNavigator.tvHierarchy.Selected) then begin
        if Assigned(FCurrentFrame) then begin
          if FCurrentFrame is TfraConceptDetails then
            FCurrentFrame.DeleteContent;
          FCurrentFrame.DropInterfaceReferences;
          FreeAndNil(FCurrentFrame);
        end;
        // This method deletes the selected node and then selects the appropriate
        // node afterwards
        fraThesaurusNavigator.DeleteNode(fraThesaurusNavigator.tvHierarchy.Selected);
       end;
    finally
      DefaultCursor(lCursor);
    end;
  end;
end;  // TfrmConceptOrganiser.DoDelete 

{-------------------------------------------------------------------------------
  Save an add or an edit. 
}
procedure TfrmConceptOrganiser.DoSave;
begin
  // Not inherited - Don't want main edit button to become enabled change back.
  dmGeneral.Connection.BeginTrans;
  try
    // Save before settings data entry controls to read-only. They need to be validated!
    FCurrentFrame.SaveContent;
    SetEditMode(emBrowse);
    if Assigned(FLockedItem) then begin
      if Assigned(FLockedParentItem) then
        dmGeneral.RunInsertStoredProc(TN_CONCEPT_RELATION,
            'usp_ConceptRelation_Insert',
            ['@FromConceptKey', FLockedParentItem.ConceptKey,
            '@ToConceptKey', TfraConceptDetails(FCurrentFrame).Key,
            '@ThesaurusRelationTypeKey', fraThesaurusNavigator.HierarchyRelationTypeKey],
            '@Key');
      fraThesaurusNavigator.tvHierarchy.SetFocus;
      // Needs to be nilled after it ceases to be useful or it could muck up
      // later saves.
      FLockedParentItem := nil;
    end;
    dmGeneral.Connection.CommitTrans;
    with TfraConceptDetails(FCurrentFrame) do begin
      CurrentFrame.ReloadContent;
      tvHierarchy.SetFocus;
      tvHierarchy.Selected := LockedItem;
      LockedItem := nil;
      // Next 2 lines refer to the locked item in THIS class
      FLockedItem.Caption := Caption;
      FLockedItem.ConceptKey := Key;
    end;
    fraThesaurusNavigator.RefreshNodeRank(FLockedItem);
  except
    on Exception do begin
      if dmGeneral.Connection.InTransaction then begin
        dmGeneral.Connection.RollbackTrans;
      end;
      SetEditMode(emEdit);
      raise;
    end;
  end;
end;  // TfrmConceptOrganiser.DoSave

{-------------------------------------------------------------------------------
  As the list organiser may have been used to modify the list of domains or
          concept groups, repopulate the navigator combo boxes when the screen
          is activated.
}
procedure TfrmConceptOrganiser.FormActivate(Sender: TObject);
var
  lConceptGroupKey: string;
  lDomainKey: string;
  lIdx: Integer;
begin
  inherited;
  // Disable the code that would clear the hierarchy
  fraThesaurusNavigator.fraDomainConceptGroupsSelector.cmbConceptGroups.OnChange := nil;
  try
    with fraThesaurusNavigator.fraDomainConceptGroupsSelector do begin
      if cmbDomains.Items.Count=0 then
        lDomainKey := ''
      else
        try
          lDomainKey := DomainKey;
        except on EDomainConceptGroupSelectorException do
          lDomainKey := '';
        end;
      if btnHistory.Down then
        lConceptGroupKey := ConceptGroupVersionKey
      else
        lConceptGroupKey := ConceptGroupKey;
      for lIdx := 0 to cmbDomains.Items.Count-1 do
        TObject(cmbDomains.Items.Objects[lIdx]).Free;
      cmbDomains.Items.Clear;
      PopulateDomainCombo;
      if lDomainKey<>'' then
        SelectDomain(lDomainKey);
      if lConceptGroupKey<>'' then
        SelectConceptGroup(lConceptGroupKey);
    end;
  finally
    // Enable the change handler
    fraThesaurusNavigator.fraDomainConceptGroupsSelector.cmbConceptGroups.OnChange :=
         fraDomainConceptGroupsSelectorcmbConceptGroupsChange;
  end; // try
end;  // TfrmConceptOrganiser.FormActivate 

{-------------------------------------------------------------------------------
}
procedure
        TfrmConceptOrganiser.fraDomainConceptGroupsSelectorcmbConceptGroupsChange(Sender: TObject);
begin
  inherited;
  // When changing the selected Concept Group, the treeview and the Concept
  // Details frame need to be cleared to stop their Concept Group Keys getting
  // out of sync. However, if the Concept Details frame is editable, the
  // Concept Details frame would not be cleared. Therefore, the Concept
  // Organiser needs to be put in browse mode before the display can be cleared.
  if Assigned(FCurrentFrame) and (FCurrentFrame.EditMode = emBrowse) then
    EditMode := emBrowse;

  fraThesaurusNavigator.fraDomainConceptGroupsSelectorcmbConceptGroupsChange(Sender);

  // When the Concept Group combobox is changed the Add button was being
  // enabled/disabled and this dependant on the Concept Group Key of the Domain
  // Concept Group Selector (which isn't always assigned). Now it depends on
  // whether the Concept Group combo box has an item selected.
  btnAdd.Enabled := (EditMode = emBrowse) and
                    (fraThesaurusNavigator.fraDomainConceptGroupsSelector
                              .cmbConceptGroups.CurrentStrID <> '');
end;  // TfrmConceptOrganiser.fraDomainConceptGroupsSelectorcmbConceptGroupsChange

{-------------------------------------------------------------------------------
  Calls SetEditMode and makes sure it is set to edit mode.
}
procedure TfrmConceptOrganiser.FrameNotification(Sender: TObject; AType:
        TEventType; const AParams: TVariantArray);
begin
  case AType of
    etTreeCleared:
      begin
        FLockedItem := nil;
        FLockedParentItem := nil;
        fraThesaurusNavigator.tvHierarchy.Selected := nil;
      end;
    etNodeCleared:
      begin
        FLockedItem := nil;
        FLockedParentItem := nil;
      end;
    etSetEditMode :
      begin
        SetEditMode(emEdit);
        FLockedItem := TTermNode(fraThesaurusNavigator.tvHierarchy.Selected);
      end;
    etSetBrowseMode:
      begin
        if EditMode = emBrowse then SetAddDeleteButtons;
        with fraThesaurusNavigator.tvHierarchy do
          if Assigned(FLockedItem) then
            Selected := FLockedItem
          else
          if Selected = nil then
            Selected := Items.GetFirstNode;
        FLockedItem := nil;
      end;
    etTermForManyConceptsChanged: fraThesaurusNavigator.DisplaySelectedConceptOnly;
    etNavigation: fraThesaurusNavigator.DisplayConcept(AParams[0]);
    etRefreshSelectedNode:
      begin
        fraThesaurusNavigatortvHierarchyChange(nil, nil);
        fraThesaurusNavigatortvHierarchyChange(nil,
            fraThesaurusNavigator.tvHierarchy.Selected);
      end;
    etListPreferredChanged:
      ListPreferredChanged;
    etRefreshNodeCaption:
      RefreshNodeCaption;
    etRefreshSynonymNode:
      begin
        // This means the current node needs to have its children refreshed.
        // But this might no be the currently selected one. Argh!

        // Get rid of active node on active ConceptOrganiser. That can be done safely.
        if frmThesaurusEditor.ActiveMDIChild is TfrmConceptOrganiser then
          with TfrmConceptOrganiser(frmThesaurusEditor.ActiveMDIChild) do
            if AParams[0] = True then
              RemoveCutNode
            else
            if Assigned(CurrentFrame) then
              TfraConceptDetails(CurrentFrame).RefreshSynonyms;
      end;
  end; // case
end;  // TfrmConceptOrganiser.FrameNotification

{-------------------------------------------------------------------------------
  Enable/Disable items of the popup menu. 
}
procedure TfrmConceptOrganiser.fraThesaurusNavigatorpmNodePopup(Sender:
        TObject);
var
  lCol, lRow: integer;
begin
  inherited;
  with fraThesaurusNavigator do begin
    // Store the mouse cursor position in the ThesaurusNavigator at the point
    // in time when the popup menu appeared. This needs to be done now
    MousePointer := tvHierarchy.ScreenToClient(Mouse.CursorPos);
    tvHierarchy.MouseToCell(MousePointer.X, MousePointer.Y, lCol, lRow);

    // Enable/Disable menu items on popup menu.
    pmNodeDelete.Enabled                := (lRow <> -1) and ShowHierarchy;
    pmNodeRefreshNodeChildren.Enabled   := (lRow <> -1) and ShowHierarchy;
    pmNodeShowAncestorHierarchy.Enabled := lRow <> -1;
    pmNodeCut.Enabled := pmNodeCut.Enabled and ShowHierarchy;

    // Enable/Disable Paste and Paste As Synonym menu items on popup menu.
    pmNodePasteAsTopLevel.Enabled := PasteEnabled and ShowHierarchy;
    pmNodePasteAsSynonym.Enabled  := (lRow <> -1) and PasteEnabled;
  end;  // with
end;  // TfrmConceptOrganiser.fraThesaurusNavigatorpmNodePopup 

{-------------------------------------------------------------------------------
}
procedure
        TfrmConceptOrganiser.fraThesaurusNavigatorpmSearchModePopulateTopLevelClick(Sender: TObject);
begin
  inherited;
  fraThesaurusNavigator.pmSearchModePopulateTopLevelClick(Sender);

end;  // TfrmConceptOrganiser.fraThesaurusNavigatorpmSearchModePopulateTopLevelClick

{-------------------------------------------------------------------------------
}
procedure TfrmConceptOrganiser.fraThesaurusNavigatortvHierarchyChange(Sender:
        TObject; Node: TFlyNode);
begin
  inherited;
  if EditMode = emBrowse then begin
    pmNodeCut.Enabled := Assigned(Node);
    pmNodePaste.Enabled := Assigned(Node) and fraThesaurusNavigator.PasteEnabled
                            and fraThesaurusNavigator.ShowHierarchy;
    fraThesaurusNavigator.tvHierarchyChange(Sender, Node);
    if Assigned(Node) then begin
      if NeedRepopulate(Node as TTermNode) then
      begin
        // Frame Notification for Thesaurus Navigator.
        fraThesaurusNavigator.OnFrameNotification := FrameNotification;
        if Assigned(fraThesaurusNavigator.tvHierarchy) then begin
          if not Assigned(FCurrentFrame) then begin
            FCurrentFrame := TfraConceptDetails.Create(Self);
            FCurrentFrame.Parent := pnlDetails;
            FCurrentFrame.Align := alClient;
            if Assigned(XPMenu) then XPMenu.InitComponent(FCurrentFrame);
          end;
          // Frame Notification for Concept Details.
          FCurrentFrame.OnFrameNotification := FrameNotification;

          if not Assigned(Node) then
            raise EBrowserFrameError.Create(ResStr_NodeUnassigned);
          FCurrentFrame.SetAdditionalProperties(Node as TTermNode);

          with TfraConceptDetails(FCurrentFrame) do begin
            ConceptGroupKey := GetConceptGroupKey;
            ConceptGroupVersionKey := GetConceptGroupVersionKey;
            HierarchyRelationTypeKey := fraThesaurusNavigator.HierarchyRelationTypeKey;
          end;
          FCurrentFrame.LoadContent;

        end;

      end;
      SetEditMode(emBrowse);
    end
    else
      FreeAndNil(FCurrentFrame);
  end;
  // Make sure buttons state are ALWAYS properly updated.
  SetAddDeleteButtons(Node);
end;  // TfrmConceptOrganiser.fraThesaurusNavigatortvHierarchyChange

{-------------------------------------------------------------------------------
  Drops the Interface References or otherwise we get nasty access violations. 
}
procedure TfrmConceptOrganiser.fraThesaurusNavigatortvHierarchyChanging(Sender:
        TObject; Node: TFlyNode; var AllowChange: Boolean);
begin
  inherited;
  if Assigned(FCurrentFrame) then
    if Assigned(TfraConceptDetails(FCurrentFrame).CurrentFrame) then
      if TfraConceptDetails(FCurrentFrame).CurrentFrame.EditMode = emBrowse then
        if fraThesaurusNavigator.tvHierarchy.Selected <> Node then
          TfraConceptDetails(FCurrentFrame).CurrentFrame.DropInterfaceReferences;
  SetAddDeleteButtons(Node);
end;  // TfrmConceptOrganiser.fraThesaurusNavigatortvHierarchyChanging

{-------------------------------------------------------------------------------
  When double clicking on structural nodes, they should expand/contract in the
          same way that clicking the plus/minus button does.
}
procedure TfrmConceptOrganiser.fraThesaurusNavigatortvHierarchyDblClick(Sender:
        TObject);
begin
  inherited;
  // As This event is fired twice. Ignore half of them.
  FTreeExpandToggle := not FTreeExpandToggle;
  if FTreeExpandToggle then Exit;

  with fraThesaurusNavigator.tvHierarchy do
    if Assigned(Selected) then
      Selected.Expanded := not Selected.Expanded;
end;  // TfrmConceptOrganiser.fraThesaurusNavigatortvHierarchyDblClick 

{-------------------------------------------------------------------------------
}
function TfrmConceptOrganiser.GetConceptGroupKey: TKeyString;
begin
  with fraThesaurusNavigator do
    if fraDomainConceptGroupsSelector.btnHistory.Down then
      Result := dmGeneral.GetStoredProcOutputParam
                        ('usp_ConceptGroupKey_Get_ForConceptGroupVersion',
                        ['@Key', ConceptGroupVersionKey],
                        '@ConceptGroupKey')
    else
      Result := ConceptGroupKey;
end;  // TfrmConceptOrganiser.GetConceptGroupKey

{-------------------------------------------------------------------------------
  This has been added to help the Concept frame get the Concept Group Version
          key. This is necessary so that a specific version of a concept group
          is viewed and the user attempts to add a concept, a concept history
          record for the new concept should be introduced in the current
          version. The best way of doing that is passing in the Concept Group
          Version key in these cases, and add the Concept History record if the
          CGV key is present.
}
function TfrmConceptOrganiser.GetConceptGroupVersionKey: TKeyString;
begin
  with fraThesaurusNavigator do
    if fraDomainConceptGroupsSelector.btnHistory.Down then
      Result := ConceptGroupVersionKey
    else
      Result := '';
end;  // TfrmConceptOrganiser.GetConceptGroupVersionKey 

{-------------------------------------------------------------------------------
  Get the description of the current item. 
}
function TfrmConceptOrganiser.GetCurrentItemDescription: string;
begin
  Result := fraThesaurusNavigator.tvHierarchy.Selected.Text + ' Concept';
end;  // TfrmConceptOrganiser.GetCurrentItemDescription

{-------------------------------------------------------------------------------
  If the list preferred item for the current concept has changed, then
    update the node's concept key and refresh it
}
procedure TfrmConceptOrganiser.ListPreferredChanged;
begin
  if Assigned(fraThesaurusNavigator.tvHierarchy.Selected) then begin
    with dmGeneral.GetRecordset('usp_ConceptListPreferred_Select_ForConcept',
        ['@ConceptGroupKey', TfraConceptDetails(FCurrentFrame).ConceptGroupKey,
        '@ConceptKey', TTermNode(fraThesaurusNavigator.tvHierarchy.Selected).ConceptKey]) do
      if not (EOF or BOF) then
        TTermNode(fraThesaurusNavigator.tvHierarchy.Selected).ConceptKey :=
            Fields['Concept_Key'].Value;
    fraThesaurusNavigatortvHierarchyChange(nil, nil);
    fraThesaurusNavigatortvHierarchyChange(nil,
        fraThesaurusNavigator.tvHierarchy.Selected);
    RefreshNodeCaption;        
  end;
end;

{-------------------------------------------------------------------------------
  Returns true if the current frame must be repopulated to show the new node. 
}
function TfrmConceptOrganiser.NeedRepopulate(ANode: TTermNode): Boolean;
begin
  Result := not Assigned(FCurrentFrame);
  if not Result then
    Result := (FCurrentFrame.EditMode = emEdit) and (ANode.ConceptKey <> FCurrentFrame.Key);
end;  // TfrmConceptOrganiser.NeedRepopulate 

{-------------------------------------------------------------------------------
}
procedure TfrmConceptOrganiser.NodeIsCopied(ANodeIsCopied: Boolean);
begin
  fraThesaurusNavigator.IsCut := not ANodeIsCopied;
end;  // TfrmConceptOrganiser.NodeIsCopied

{-------------------------------------------------------------------------------
}
procedure TfrmConceptOrganiser.pmAddChildConceptClick(Sender: TObject);
begin
  inherited;
  DoAddConcept(False);
end;  // TfrmConceptOrganiser.pmAddChildConceptClick 

{-------------------------------------------------------------------------------
}
procedure TfrmConceptOrganiser.pmAddConceptClick(Sender: TObject);
begin
  inherited;
  // Need to nil this, or a Concept Relation record will be incorrectly created
  // if the user has previously added a child concept.
  FLockedParentItem := nil;
  DoAddConcept(True);
end;  // TfrmConceptOrganiser.pmAddConceptClick

{-------------------------------------------------------------------------------
  Handles the copying of a concept.
}
procedure TfrmConceptOrganiser.pmNodeCopyClick(
  Sender: TObject);
begin
  inherited;
  FCopiedNodeKey := TTermNode(fraThesaurusNavigator.tvHierarchy.Selected).ConceptKey;
  fraThesaurusNavigator.pmNodeCopyClick(Sender);
end;

{-------------------------------------------------------------------------------
  Handles the cutting of a concept.
}
procedure TfrmConceptOrganiser.pmNodeCutClick(Sender: TObject);
var
  i: Integer;
begin
  inherited;
  if fraThesaurusNavigator.ShowHierarchy then
  begin
    FCopiedNodeKey := TTermNode(fraThesaurusNavigator.tvHierarchy.Selected).ConceptKey;
    fraThesaurusNavigator.pmNodeCopyClick(Sender);
    fraThesaurusNavigator.IsCut := True;

    with frmThesaurusEditor do
      for i := 0 to MDIChildCount - 1 do
        if MDIChildren[i] is TfrmConceptOrganiser then
          TfrmConceptOrganiser(MDIChildren[i]).NodeIsCopied(False);
  end;
end;  // TfrmConceptOrganiser.pmNodeCutClick

{-------------------------------------------------------------------------------
  Handles the deleting of a concept. 
}
procedure TfrmConceptOrganiser.pmNodeDeleteClick(Sender: TObject);
begin
  inherited;
  DoDelete;
end;  // TfrmConceptOrganiser.pmNodeDeleteClick

{-------------------------------------------------------------------------------
  Determines if a concept with the given name is contained in the given
  concept group.
}
function TfrmConceptOrganiser.ConceptIsContainedInGroup(
  AConceptKey: String;
  AConceptGroupKey: String): Boolean;
var
  lCount: Integer;
begin
  Result := false;
  lCount := dmGeneral.GetStoredProcOutputParam(
      'usp_ConceptCount_CopyCheck',
      ['@ParentKey', AConceptKey,
      '@ConceptGroupKey', AConceptGroupKey],
      '@Count');
  if (lCount > 0) then
      Result := true;
end;

{-------------------------------------------------------------------------------
  Handles the pasting of a concept.
}
procedure TfrmConceptOrganiser.PasteNode(
  AIsPasteAsSynonym: Boolean;
  AIsPasteAsTopLevel: Boolean;
  ACheckForSameConceptGroup: Boolean);
var
  lPerformPaste: Boolean;
  lButtonSelected: Integer;
begin
  lPerformPaste := true;
  If ACheckForSameConceptGroup
      // Are we copying (but not cutting) a node into the same concept group?
      and not fraThesaurusNavigator.IsCut
      and ConceptIsContainedInGroup(
          FCopiedNodeKey,
          fraThesaurusNavigator.ConceptGroupKey) then
  begin
    // If so, prompt the user for confirmation
    lButtonSelected := MessageDlg(
        ResStr_ConceptDuplicationWarning,
        mtWarning,
        [mbYes, mbNO],
        0);
    lPerformPaste := (lButtonSelected = mrYes);
  end;

  if lPerformPaste then
  // Paste the node
  begin
    inherited;
    with fraThesaurusNavigator do begin
      IsPasteAsSynonym := AIsPasteAsSynonym;
      IsPasteAsTopLevel := AIsPasteAsTopLevel;
      ExecutePaste(tvHierarchy);
      // Once a cut node is pasted, clear the clipboard.
      if IsCut then Clipboard.Clear;
    end;
  end;
end;

{-------------------------------------------------------------------------------
}
procedure TfrmConceptOrganiser.pmNodePasteAsSynonymClick(Sender: TObject);
begin
  inherited;
  PasteNode(True, False, False);
end;  // TfrmConceptOrganiser.pmNodePasteAsSynonymClick

{-------------------------------------------------------------------------------
}
procedure TfrmConceptOrganiser.pmNodePasteAsTopLevelClick(Sender: TObject);
begin
  inherited;
  PasteNode(False, True, True);
end;  // TfrmConceptOrganiser.pmNodePasteAsTopLevelClick

{-------------------------------------------------------------------------------
}
procedure TfrmConceptOrganiser.pmNodePasteClick(Sender: TObject);
begin
  inherited;
  if (fraThesaurusNavigator.PasteEnabled and fraThesaurusNavigator.ShowHierarchy) then
    PasteNode(False, False, True);
end;  // TfrmConceptOrganiser.pmNodePasteClick

{-------------------------------------------------------------------------------
  Update the caption for the current node
}
procedure TfrmConceptOrganiser.RefreshNodeCaption;
var
  lCurrentItem, lNodeToUpdate: TFlyNode;
  lConceptsToUpdate: TStringList;
begin
  with fraThesaurusNavigator do
  begin
    if Assigned(FLockedItem) then
      lCurrentItem := FLockedItem
    else
      lCurrentItem := tvHierarchy.Selected;
  end;
  lConceptsToUpdate := TStringList.Create;

  if lCurrentItem is TTermNode then
  begin
    with dmGeneral.GetRecordset(
        'usp_Concept_Select_ForCaptionUpdate',
        ['@ConceptKey', TTermNode(lCurrentItem).ConceptKey]) do
    begin
      while not Eof do
      begin
        lConceptsToUpdate.Add(VarToStr(Fields['Concept_Key'].Value));
        MoveNext;
      end;
    end;

    lNodeToUpdate := fraThesaurusNavigator.tvHierarchy.TopItem;
    while (lNodeToUpdate <> nil) and (lConceptsToUpdate.Count > 0) do
    begin
      fraThesaurusNavigator.RefreshNodeCaption(
        lNodeToUpdate,
        lConceptsToUpdate);
      lNodeToUpdate := lNodeToUpdate.GetNextSibling;
    end;

    lConceptsToUpdate.Free;
  end;

end;

{-------------------------------------------------------------------------------
  This method was introduced when drag and drop between two Concept Organisers (
          CO) was implemented. Dragging a Concept from one CO and dropping it
          into the other was fine, but the original Concept was not
          disappearing. This method causes the original concept to disappear.
}
procedure TfrmConceptOrganiser.RemoveCutNode;
begin
  with fraThesaurusNavigator do
    if Assigned(DraggedNode) then begin
      // Need to drop the node cleanly. So careful if it's the selected one.
      if DraggedNode = tvHierarchy.Selected then begin
        if Assigned(tvHierarchy.Selected.Parent) then
          tvHierarchy.Selected := tvHierarchy.Selected.Parent
        else
        if tvHierarchy.Selected.GetNextSibling <> nil then
          tvHierarchy.Selected := tvHierarchy.Selected.GetNextSibling
        else
          tvHierarchy.Selected := nil;
      end;
      DraggedNode.Delete;
      DraggedNode := nil;
      // It's cut and, presumably, pasted. So forget it now.
      IsCut := False;
    end;
end;  // TfrmConceptOrganiser.RemoveCutNode

{-------------------------------------------------------------------------------
  Set the add and delete buttons.  ANode is an optional parameter - by default
          the current hierarchy node is used.
}
procedure TfrmConceptOrganiser.SetAddDeleteButtons(const ANode: TFlyNode = nil);
var
  lNode: TFlyNode;
begin
  with fraThesaurusNavigator do begin
    if Assigned(ANode) then
      lNode := ANode
    else
      lNode := tvHierarchy.Selected;

    if fraDomainConceptGroupsSelector.btnHistory.Down then
      btnAdd.Enabled := btnAdd.Enabled and (ConceptGroupVersionKey <> '')
                        and ShowHierarchy
    else
      btnAdd.Enabled := btnAdd.Enabled and (ConceptGroupKey <> '')
                        and ShowHierarchy;

    btnDelete.Enabled := (EditMode = emBrowse) and Assigned(lNode)
                          and ShowHierarchy;
  end;
end;  // TfrmConceptOrganiser.SetAddDeleteButtons 

{-------------------------------------------------------------------------------
  Sets the edit mode.
}
procedure TfrmConceptOrganiser.SetEditMode(Value: TEditMode);
var
  lBrowsing: Boolean;
begin
  inherited;
  SetAddDeleteButtons;
  // The Concept Details edit mode should be inverse of the current details frame
  // so that its buttons are disabled at the right times
  if Assigned(FCurrentFrame) then begin
    if Value = emEdit then FCurrentFrame.EditMode := emBrowse
                      else FCurrentFrame.EditMode := emEdit;
  end;
  // Prevent switching concept groups whilst editing
  lBrowsing := EditMode = emBrowse;
  with fraThesaurusNavigator do begin
    fraDomainConceptGroupsSelector.Enabled := lBrowsing;
    btnSearchMode.Enabled                  := lBrowsing;
    btnGo.Enabled                          := lBrowsing;
    eSearch.Enabled                        := lBrowsing;
  end;
end;  // TfrmConceptOrganiser.SetEditMode

{-------------------------------------------------------------------------------
  Returns the key for the currently selected domain/concept group as
  specified
}
function TfrmConceptOrganiser.GetKey(const ATableName: String): String;
begin
  Result := '';
  if ATableName = 'Domain' then
  begin
    if fraThesaurusNavigator.fraDomainConceptGroupsSelector.cmbDomains.
          ItemIndex <> -1 then
      Result := fraThesaurusNavigator.DomainKey;
  end
  else if ATableName = 'ConceptGroup' then
  begin
    Result := fraThesaurusNavigator.ConceptGroupKey;
  end;
end;

end.
