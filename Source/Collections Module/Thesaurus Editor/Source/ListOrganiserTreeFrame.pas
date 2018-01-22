{===============================================================================
  Unit:        ListOrganiserTreeFrame

  Defines:     TfraListOrganiserTree

  Description: Frame containing a tree view with drag and drop functionality,
               for use in the List Organiser.

  Created:     4/12/2008

  Last revision information:
    $Revision: 11 $
    $Date: 2/02/09 11:10 $
    $Author: Pauldavies $

===============================================================================}
unit ListOrganiserTreeFrame;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, BaseNavigatorFrame, exgrid, RapTree, ExtCtrls, Menus, DropSource,
  LuxembourgConstants, ListOrganiserNodes, DataClasses, Types, ADODB,ComCtrls;

resourcestring
  ResStr_MayTakeLongTime = 'This action may take several minutes, are you sure you '+
                           'want to continue?';
  ResStr_MoveStatusMessage = 'Moving %s, this may take some time...';
  ResStr_MoveDialogMessage = 'Moving %s.';

type
  TfraListOrganiserTree = class(TfraBaseNavigator)
    pmCutPaste: TPopupMenu;
    pmCutPasteCut: TMenuItem;
    pmCutPastePaste: TMenuItem;
    procedure tvHierarchyExpanding(Sender: TObject; Node:
        TFlyNode; var AllowExpansion: Boolean);
    procedure pmCutPasteOnPopup(Sender: TObject);
    procedure pmCutPasteCutOnClick(Sender: TObject);
    procedure pmCutPastePasteOnClick(Sender: TObject);
  private
    FCutPasteObject : TJNCCDropSource;
    FCutNode : TFlyNode;
    FPopupMousePosition : TPoint;
    FAlreadyDropped: Boolean;
    function CheckValidPasteTarget(APoint: TPoint; const ATable, AKeyField : string): boolean;
    procedure DoPaste(const iSourceData: TKeyList);
    procedure StoredProcedureComplete;
    function  GetSelected : TFlyNode;
    procedure SetSelected(value : TFlyNode);
    procedure PopulateTopLevel;
    function GetParentKey(const ATable, AKeyField: String): String;
    function AddNode(AParentNode: TBaseListOrganiserNode;
        AChildNodeClass: TFlyNodeClass) : TBaseListOrganiserNode;  
    function CanPaste(pasteNode: TFlyNode; table, lParentKey: string): Boolean; overload;
  protected
    function CanDrag : Boolean; override;
    function CanDrop : Boolean; override;
    procedure SetDropSourceData(var oDropSource: TJNCCDropSource;
        var node : TFlyNode); override;
    procedure CheckNodeCanDrop(APoint: TPoint; const
        ATable, AKeyField: string; var Accept: boolean); override;
    procedure DropTerm(const Sender: TObject; const iFormat : integer; const
        iSourceData: TKeyList; const iTextStrings : TstringList; const
        iIsPasteOperation: boolean; var ioHandled : boolean); override;
    function GetTableList : TStringDynArray; override;
    procedure PopulateNode(ANode: TNavigatorNode); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure RemoveCutNode;
    function AddConceptGroup : TBaseListOrganiserNode;
    function AddConceptGroupVersion : TBaseListOrganiserNode;
    function AddDomain : TBaseListOrganiserNode;
    function AddLocalDomain : TBaseListOrganiserNode;
    function AddSubjectArea : TBaseListOrganiserNode;
    property Selected : TFlyNode read GetSelected write SetSelected;
    procedure UpdateNode(node : TBaseListOrganiserNode; newName: string;
        newKey: string);
    procedure Initialise;                   
    function CanPaste(pasteNode: TFlyNode): Boolean; overload;
    function CanCut(cutNode: TFlyNode): Boolean;
  {  procedure DeleteNode(doomedNode : TFlyNode);      }
  end;

var
  fraListOrganiserTree: TfraListOrganiserTree;

implementation

uses GeneralData, ListOrganiser, InterfaceDataModule, ThesaurusEditorMain,
  BaseDragFrameUnit, ComObj, GeneralFunctions, ProcessLongProcedureForm;

{-------------------------------------------------------------------------------
  Creates this form.
}
constructor TfraListOrganiserTree.Create(AOwner: TComponent);
begin
  inherited;
  FAlreadyDropped := False;
end;  // TfraListOrganiserTree.Create

{-------------------------------------------------------------------------------
  Sets up the initial state of the tree.
}
procedure TfraListOrganiserTree.Initialise;
begin
  PopulateTopLevel;
  if tvHierarchy.Items.Count > 0 then
    tvHierarchy.Selected := tvHierarchy.Items[0];
  tvHierarchy.SetFocus;    // Ensure the focus is set to the treeview.
end;


{-------------------------------------------------------------------------------
  Populates the top level subject areas in the tree view.
}
procedure TfraListOrganiserTree.PopulateTopLevel;
var
  lNewNode: TSubjectAreaNode;
begin
  tvHierarchy.Items.BeginUpdate;
  tvHierarchy.Items.Clear;
  try
    with dmGeneral.GetRecordset('usp_SubjectAreas_Select_ForTopLevel',[]) do
    begin
      while not EOF do begin
        lNewNode := TSubjectAreaNode(tvHierarchy.Items.AddTypedChild(nil,
            TSubjectAreaNode));
        lNewNode.Caption := Fields['Item_Name'].Value;
        lNewNode.Key := Fields['Key'].Value;
        lNewNode.HasChildren := Fields['Has_Children'].Value;
        MoveNext;
      end; // while
    end; // with
  finally
    tvHierarchy.Items.EndUpdate;
  end;
end;  // TfraListOrganiserTree.PopulateTopLevel

{-------------------------------------------------------------------------------
}
procedure TfraListOrganiserTree.UpdateNode(node: TBaseListOrganiserNode;
    newName: string; newKey: string);
begin
  node.Text := newName;
  node.Key := newKey;
  dmInterface.RepaintNode(node, tvHierarchy);
end;  // TfraListOrganiserTree.RepaintNode

{-------------------------------------------------------------------------------
}
function TfraListOrganiserTree.GetSelected : TFlyNode;
begin
  Result := tvHierarchy.Selected;
end;   // TfraListOrganiserTree.GetSelected

{-------------------------------------------------------------------------------
}
procedure TfraListOrganiserTree.SetSelected(value : TFlyNode);
begin
  tvHierarchy.Selected := value;
end;   // TfraListOrganiserTree.SetSelected

{-------------------------------------------------------------------------------
  Sets the properties of the TJNCCDropSource object when a node beings being
  dragged.
}
procedure TfraListOrganiserTree.SetDropSourceData(var oDropSource:
    TJNCCDropSource; var node : TFlyNode);
begin

  if node is TDomainNode then
    oDropSource.DropData.SetTable(TN_DOMAIN)
  else if node is TLocalDomainNode then
    oDropSource.DropData.SetTable(TN_LOCAL_DOMAIN)
  else if node is TConceptGroupNode then
    oDropSource.DropData.SetTable(TN_CONCEPT_GROUP)
  else
    Exit;

  // If the concept node that has been cut has a parent, we also want to store
  // the parent's key. With both of these keys we can establish which
  // record needs to be altered.
  if Assigned(node.Parent) then
    oDropSource.DropData.AddItem(TBaseListOrganiserNode(node).Key,
      TBaseListOrganiserNode(node).ParentKey)
  else
    oDropSource.DropData.AddItem(TBaseListOrganiserNode(node).Key, '');
end;  // TfraListOrganiserTree.SetDropSourceData

{-------------------------------------------------------------------------------
  Checks whether this tree view can be used as a drag source.
}
function TfraListOrganiserTree.CanDrag : Boolean;
begin
  Result := True;
end;  // TfraListOrganiserTree.CanDrag

{-------------------------------------------------------------------------------
  Checks whether a node can be dragged to a particular point.
}
procedure TfraListOrganiserTree.CheckNodeCanDrop(APoint: TPoint; const
    ATable, AKeyField: string; var Accept: boolean);
var lParentKey : string;
    lPos: TPoint;
    lHitTest: THitTests;
    lNode: TFlyNode;
begin
  lParentKey := GetParentKey(ATable, AKeyField);
 
  if SameText(ATable, TN_DOMAIN) or SameText(ATable, TN_LOCAL_DOMAIN) or SameText(ATable, TN_CONCEPT_GROUP) then
    with tvHierarchy do begin
      lPos := ScreenToClient(APoint);
      lHitTest := GetHitTestInfoAt(lPos.X, lPos.Y);

      // Expand node if hovering on (+).
      if htOnButton	in lHitTest then begin
        lNode := GetNodeAt(lPos.X, lPos.Y);
        if Assigned(lNode) and Assigned(FDraggedNode)then
          if FDraggedNode.Level > lNode.Level then // Don't expand higher level nodes.
            lNode.Expand(False);
      end else
      // Scroll tree up if hovering just above tree.
      if htAbove in lHitTest then begin
        lNode := TopItem;
        if lNode.GetPrevVisible <> nil then
          TopItem := lNode.GetPrevVisible;
      end else
      // Scroll tree down, if hovering just below tree.
      if htBelow in lHitTest then begin
        // Need to use -1, or we get the wrong last fully visible item.
        lNode := GetNodeAtRow(TopRow + VisibleRowCount - 1);
        if Assigned(lNode) then
          if lNode.GetNextVisible <> nil then
            TopItem := TopItem.GetNextVisible;
      end;
      Accept := CheckValidPasteTarget(APoint, ATable,lParentKey);
    end;

end; // TfraListOrganiserTree.CheckNodeCanDrop


{-------------------------------------------------------------------------------
  Checks whether or not a node can be dropped/pasted at a specific point.
}
function TfraListOrganiserTree.CheckValidPasteTarget(APoint: TPoint; const
    ATable, AKeyField : string) : boolean;
var
  lClickPoint: TPoint;
  lNode: TFlyNode;
begin
  lClickPoint := tvHierarchy.ScreenToClient(APoint);
  lNode       := tvHierarchy.GetNodeAt(lClickPoint.X, lClickPoint.Y);
  Result := CanPaste(lNode, ATable, AKeyField);
end;  // TfraListOrganiserTree.CheckValidPasteTarget

{-------------------------------------------------------------------------------
  Gets an array of all types which may be dragged into this drop target.
}
function TfraListOrganiserTree.GetTableList : TStringDynArray;
begin
  SetLength(Result, 3);
  Result[0] := TN_DOMAIN;
  Result[1] := TN_LOCAL_DOMAIN;
  Result[2] := TN_CONCEPT_GROUP;
end;  // TfraListOrganiserTree.GetTableList

{-------------------------------------------------------------------------------
  When a node is dropped or pasted onto another, this procedure performs the
  actual cut/paste operation.
}
procedure TfraListOrganiserTree.DropTerm(const Sender: TObject; const
    iFormat : integer; const iSourceData: TKeyList; const iTextStrings :
    TstringList; const iIsPasteOperation: boolean; var ioHandled : boolean);
begin
  inherited;
  // The AlreadyDropped flag stops this from being called multiple times
  if not FAlreadyDropped then
  begin
    FAlreadyDropped := True;
    DraggedNode := tvHierarchy.Selected;
    frmThesaurusEditor.SetProgress(0, 100);
    DoPaste(iSourceData);
    FAlreadyDropped := False;
  end;
end;  // TfraListOrganiserTree.DropTerm

procedure TfraListOrganiserTree.DoPaste(const iSourceData: TKeyList);
var
  lParentKey: string;
  lChildKey: string;
  level: integer;
  procedureName: string;
  cursor: TCursor;
  longProc: TfrmProcessLongProcedure;
begin
  if Assigned(FTargetNode) then
  begin
    level := DraggedNode.Level;
    lParentKey := TBaseListOrganiserNode(FTargetNode).Key;
    lChildKey := iSourceData.Items[0].KeyField1;

    // Chooses the stored procedure to use based on the level of the node in
    // the treeview, since all nodes of the same type are at the same level.
    case level of
      1: procedureName := 'usp_Domain_Update_ForSubjectArea'; //Domain
      2: procedureName := 'usp_LocalDomain_Update_ForDomain'; //Local domain
      3: procedureName := 'usp_ConceptGroup_Update_ForLocalDomain';
          //Subject group
    end;

    // Asks the user to confirm that they want to perform the move. In the case
    // of the bottom level, the move is fast, so no confirmation is needed.
    if (level = 1) or (MessageDlg(ResStr_MayTakeLongTime, mtWarning, [mbYes, mbNo], 0) = mrYes) then
    begin
      cursor := HourGlassCursor;
      try
        frmThesaurusEditor.SetStatus(Format(ResStr_MoveStatusMessage, [DraggedNode.Text]));

        // Displays a modal form which performs the stored procedure.
        longProc := TfrmProcessLongProcedure.Create(nil);
        try
          longProc.RunStoredProc(
                Format(ResStr_MoveDialogMessage, [DraggedNode.Text]),
                procedureName, [
                 '@ParentKey', lParentKey,
                 '@ChildKey', lChildKey]);
          if Assigned(longProc.Error) then
          begin
            ShowMessage(longProc.Error.Description);
            Exit;
          end;
        finally
          longProc.Free;
        end;

        StoredProcedureComplete;
      finally
        DefaultCursor(cursor);
      end;
    end;
  end;
end;  // TfraListOrganiserTree.DoPaste

{-------------------------------------------------------------------------------
  Checks whether or not this table can be used as a drop target.
}
function TfraListOrganiserTree.CanDrop : Boolean;
begin
  Result := True;
end;  // TfraListOrganiserTree.CanDrop

{$R *.dfm}

{-------------------------------------------------------------------------------
  Handle a click on the '+' next to a node in the treeview. 
}
procedure TfraListOrganiserTree.tvHierarchyExpanding(Sender: TObject; Node:
        TFlyNode; var AllowExpansion: Boolean);
begin
  inherited;
  PopulateNode(TNavigatorNode(Node));
end;  // TfraListOrganiserTree.tvHierarchyExpanding

{-------------------------------------------------------------------------------
  Perform the actual expansion of a node.
}
procedure TfraListOrganiserTree.PopulateNode(ANode: TNavigatorNode);
var
  lNewNode: TBaseListOrganiserNode;
begin
  if ANode.Count = 0 then begin
    tvHierarchy.Items.BeginUpdate;
    try
      with dmGeneral.GetRecordset(
                  TBaseListOrganiserNode(ANode).ChildrenStoredProc,
                  ['@ParentKey', TBaseListOrganiserNode(ANode).Key]) do
      begin
        while not EOF do begin
          lNewNode := TBaseListOrganiserNode(tvHierarchy.Items.AddTypedChild(
                              ANode,
                              TBaseListOrganiserNode(ANode).ChildNodeType));
          lNewNode.Caption := Fields['Item_Name'].Value;
          lNewNode.Key := Fields['Key'].Value;
          lNewNode.HasChildren := Fields['Has_Children'].Value;
          MoveNext;
        end; // while
      end; // with
    finally
      tvHierarchy.Items.EndUpdate;
    end;
  end;
end;  // TfraListOrganiserTree.PopulateNode

{-------------------------------------------------------------------------------
  On display of the right click popup menu, enable or disable the menu buttons,
  based on whether or not the selected node can be copied or pasted to. Also
  records the position the mouse was in when the menu was opened.
}
procedure TfraListOrganiserTree.pmCutPasteOnPopup(Sender: TObject);
var
  cutNode : TFlyNode;
  lClickPoint : TPoint;
begin
  FPopupMousePosition := Mouse.CursorPos;
  lClickPoint := tvHierarchy.ScreenToClient(FPopupMousePosition);
  cutNode := tvHierarchy.GetNodeAt(lClickPoint.X, lClickPoint.Y);
  pmCutPastePaste.Enabled := CanPaste(cutNode);
  pmCutPasteCut.Enabled := CanCut(cutNode);
end;  // TfraListOrganiserTree.pmCutPasteOnPopup

{-------------------------------------------------------------------------------
  Returns true if you can paste on the passed node, false if you cannot.
}
function TfraListOrganiserTree.CanPaste(pasteNode: TFlyNode): Boolean;
var
  table, lParentKey: string;
begin
  if Assigned(FCutPasteObject) then
  begin
    table := FCutPasteObject.DropData.ItemTable[0];
    lParentKey := GetParentKey(FCutPasteObject.DropData.ItemTable[0], TBaseListOrganiserNode(pasteNode).Key);
    Result := CanPaste(pasteNode, table, lParentKey);
  end else
    Result := False;
end;
{-------------------------------------------------------------------------------
  Returns true if you can paste on the passed node, false if you cannot.
}
function TfraListOrganiserTree.CanPaste(pasteNode: TFlyNode; table, lParentKey: string): Boolean;
begin
  Result := false;
  if ((Assigned(pasteNode)) and (TBaseListOrganiserNode(pasteNode).Key <> lParentKey ) )then
  begin
    if table = TN_DOMAIN then
      Result := pasteNode is TSubjectAreaNode
    else if table = TN_LOCAL_DOMAIN then
      Result := pasteNode is TDomainNode
    else if table = TN_CONCEPT_GROUP then
      Result := pasteNode is TLocalDomainNode
  end;
end;

{-------------------------------------------------------------------------------
  Returns true if you can cut from the passed node, false if you cannot.
}
function TfraListOrganiserTree.CanCut(cutNode: TFlyNode): Boolean;
begin
  Result := False;
  if Assigned(cutNode) then
    if (cutNode is TDomainNode) or
        (cutNode is TLocalDomainNode) or
        (cutNode is TConceptGroupNode) then
      Result := True;
end;

{-------------------------------------------------------------------------------
  Called when the 'Cut' popup menu item is clicked. Stores the type and key
  of the clicked item.
}
procedure TfraListOrganiserTree.pmCutPasteCutOnClick(Sender: TObject);
var
  lClickPoint : TPoint;
  cutNode : TFlyNode;
begin
  if Sender = pmCutPasteCut then
  begin
    lClickPoint := tvHierarchy.ScreenToClient(FPopupMousePosition);

    cutNode := tvHierarchy.GetNodeAt(lClickPoint.X, lClickPoint.Y);
  end else begin
    cutNode := tvHierarchy.Selected;
  end;

  if Assigned(cutNode) then
    if CanCut(cutNode) then
    begin
      FCutNode := cutNode;
      if not Assigned(FCutPasteObject) then
        FCutPasteObject := TJNCCDropSource.Create(self);

      SetDropSourceData(FCutPasteObject, FCutNode);
    end;
end;  // TfraListOrganiserTree.pmCutPasteCutOnClick

{-------------------------------------------------------------------------------
  Called when the 'Paste' popup menu item is clicked. Moves the cut object to
  the new location.
}
procedure TfraListOrganiserTree.pmCutPastePasteOnClick(Sender: TObject);
var
  lClickPoint: TPoint;
  targetNode: TFlyNode;
begin
  if Sender = pmCutPastePaste then
  begin
    lClickPoint := tvHierarchy.ScreenToClient(FPopupMousePosition);
    targetNode := tvHierarchy.GetNodeAt(lClickPoint.X, lClickPoint.Y);
  end else begin
    targetNode := tvHierarchy.Selected;
  end;
  if CanPaste(targetNode) then
  begin
    if Assigned(FCutNode) and Assigned(targetNode) then
    begin
      FTargetNode := targetNode;
      DraggedNode := FCutNode;
      DoPaste(FCutPasteObject.DropData);
      FCutNode := nil;
      if Assigned(FCutPasteObject) then
        FCutPasteObject.Destroy;
      FCutPasteObject := nil;
    end;
  end;
end;   // TfraListOrganiserTree.pmCutPastePasteOnClick

{-------------------------------------------------------------------------------
  Procedure to be called when an Asynchronus stored procedure completes.
  Performs the actual movement of the objects in the tree view, shuts down
  the progress bar thread and allows the user to interact with the interface
  again.
}
procedure TfraListOrganiserTree.StoredProcedureComplete;
var
  lChildKey : string;
begin
  if Assigned(DraggedNode) then begin
    //Removes the cut node from its previous parent.
    lChildKey := TBaseListOrganiserNode(DraggedNode).Key;
    with frmThesaurusEditor do
      if ActiveMDIChild is TfrmListOrganiser then
        TfrmListOrganiser(ActiveMDIChild).RemoveCutNode;

    // Ensure the tree view is up to date.
    tvHierarchy.Selected := FTargetNode;
    pmNodeRefreshNodeChildrenClick(Self);
    FTargetNode.Expanded := True;

    // Locate and select the pasted node.
    FTargetNode := FTargetNode.GetFirstChild;
    while Assigned(FTargetNode) do
      if TBaseListOrganiserNode(FTargetNode).Key = lChildKey then
        Break
      else
        FTargetNode := FTargetNode.GetNextSibling;
    if Assigned(FTargetNode) then
      tvHierarchy.Selected := FTargetNode;

    frmThesaurusEditor.SetStatus('');
  end;
end;   // TfraListOrganiserTree.StoredProcedureComplete

{-------------------------------------------------------------------------------
  This method was introduced when drag and drop between two Concept Organisers (
          CO) was implemented. Dragging a Concept from one CO and dropping it
          into the other was fine, but the original Concept was not
          disappearing. This method causes the original concept to disappear.
}
procedure TfraListOrganiserTree.RemoveCutNode;
begin
  if Assigned(DraggedNode) then
  begin
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
  end;
end;  // TfraListOrganiserTree.RemoveCutNode  

{-------------------------------------------------------------------------------
  Adds a Concept Group node. 
}
function TfraListOrganiserTree.AddConceptGroup : TBaseListOrganiserNode;
begin
  if tvHierarchy.Selected is TConceptGroupNode then
    Result := AddNode(TBaseListOrganiserNode(tvHierarchy.Selected.Parent),
        TConceptGroupNode)
  else
    Result := AddNode(TBaseListOrganiserNode(tvHierarchy.Selected),
        TConceptGroupNode);
end;  // TfraListOrganiserTree.AddConceptGroup

{-------------------------------------------------------------------------------
  Adds a concept group version node. 
}
function TfraListOrganiserTree.AddConceptGroupVersion : TBaseListOrganiserNode;
begin
  if tvHierarchy.Selected is TConceptGroupVersionNode then
    Result := AddNode(TBaseListOrganiserNode(tvHierarchy.Selected.Parent),
        TConceptGroupVersionNode)
  else
    Result := AddNode(TBaseListOrganiserNode(tvHierarchy.Selected),
        TConceptGroupVersionNode);
end;  // TfraListOrganiserTree.AddConceptGroupVersion

{-------------------------------------------------------------------------------
  Adds a domain node. 
}
function TfraListOrganiserTree.AddDomain : TBaseListOrganiserNode;
begin
  if tvHierarchy.Selected is TDomainNode then
    Result := AddNode(TBaseListOrganiserNode(tvHierarchy.Selected.Parent),
        TDomainNode)
  else
    Result := AddNode(TBaseListOrganiserNode(tvHierarchy.Selected),
        TDomainNode);
end;  // TfraListOrganiserTree.AddDomain

{-------------------------------------------------------------------------------
  Adds a local domain node. 
}
function TfraListOrganiserTree.AddLocalDomain : TBaseListOrganiserNode;
begin
  if tvHierarchy.Selected is TLocalDomainNode then
    Result := AddNode(TBaseListOrganiserNode(tvHierarchy.Selected.Parent),
        TLocalDomainNode)
  else
    Result := AddNode(TBaseListOrganiserNode(tvHierarchy.Selected),
        TLocalDomainNode);
end;  // TfraListOrganiserTree.AddLocalDomain

{-------------------------------------------------------------------------------
  Adds a subject area node. 
}
function TfraListOrganiserTree.AddSubjectArea : TBaseListOrganiserNode;
begin
  Result := AddNode(nil, TSubjectAreaNode);
end;  // TfraListOrganiserTree.AddSubjectArea

{-------------------------------------------------------------------------------
}
function TfraListOrganiserTree.AddNode(AParentNode: TBaseListOrganiserNode;
        AChildNodeClass: TFlyNodeClass) : TBaseListOrganiserNode;
var
  lNewNode: TBaseListOrganiserNode;
begin
  tvHierarchy.Items.BeginUpdate;
  try
    lNewNode :=
        TBaseListOrganiserNode(tvHierarchy.Items.AddTypedChild(AParentNode,
        AChildNodeClass));
    lNewNode.Caption := 'New Node';
    lNewNode.Key := '';
    lNewNode.HasChildren := False;  
  finally
    tvHierarchy.Items.EndUpdate;
  end;

  tvHierarchy.Selected := lNewNode;

  Result := lNewNode;
end;  // TfraListOrganiserTree.AddNode


{-----------------------------------------------------------------------
  Return the parent key of selected/dragged node
}
function TfraListOrganiserTree.GetParentKey(const ATable,
  AKeyField: String): String;
begin
  if ATable = TN_CONCEPT_GROUP then
  Result := VarToStr(dmGeneral.GetStoredProcOutputParam(
                'usp_LocalDomainKey_Get_ForConceptGroup', ['@Key', AKeyField],
                '@LocalDomainKey'))
  else if  ATable = TN_LOCAL_DOMAIN then
   Result := VarToStr(dmGeneral.GetStoredProcOutputParam(
                'usp_DomainKey_Get_ForLocalDomain', ['@Key', AKeyField],
                '@DomainKey'))
   else if ATable = TN_DOMAIN  then
    Result := VarToStr(dmGeneral.GetStoredProcOutputParam(
                'usp_SubjectAreaKey_Get_ForDomain', ['@Key', AKeyField],
                '@SubjectAreaKey'));
end;

end.
