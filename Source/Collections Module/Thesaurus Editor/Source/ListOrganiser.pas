{===============================================================================
  Unit:        ListOrganiser

  Defines:     TfrmListOrganiser

  Description: Screen allowing subject area, domain, local domain, concept group
               and concept group version to be edited.  These are all the
               thesaurus entities that control the organisation of each list
               of terms within the thesaurus.

  Created:     18/7/2003

  Last revision information:
    $Revision: 22 $
    $Date: 24/05/11 16:15 $
    $Author: Jamesbichard $

===============================================================================}
unit ListOrganiser;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, BaseMDIChildEditorUnit, StdCtrls, ImageListButton, ExtCtrls,
  ActnList, Menus, exgrid, RapTree, BaseDetailFrameUnit, ListOrganiserNodes,
  DSSDataTypes, UserMessages, BaseTabSheetFrameUnit, ResourceStrings,
  DataClasses, ExceptionForm, BaseDragFrameUnit, BaseNavigatorFrame,
  ListOrganiserTreeFrame, ComCtrls;

type
  TOnRefreshScreenInfo = procedure (Sender: TObject; const AKey, ACaption: 
          string) of object;
  {-----------------------------------------------------------------------------
    MDI child screen that allows the organisation of concept groups (list) 
    within the thesaurus into a defined structure.  The structure is defined as
    follows (least specialised first):
      Subject Area
      Domain
      Local Domain
      Concept Group
      Concept Group Version
    All these entities are managed by this screen.  The screen displays a 
    hierarchy of the above structure and embeds the appropriate window when 
    editing each entity.  The appropriate window is selected as follows:
      Select a subject area to display TfraSubjectArea
      Select a domain to display TfraDomain
      Select a local domain to display TfraLocalDomain
      Select a concept group to display TfraConceptGroup
      Select a concept group version to display TfraConceptGroupVersion
    The screen operates in 2 modes, browse and edit.  When in browse mode, 
    btnSave and btnCancel are disabled and the selected node's details screen 
    is read only (if there is one).  When in edit mode, btnAdd, btnEdit, 
    btnDelete and tvHierarchy are disabled and the selected node's details
    screen is editable.  The form defaults to browse mode.
  }
  TfrmListOrganiser = class (TBaseMDIChildEditor)
    actAddConceptGroup: TAction;
    actAddConceptGroupVersion: TAction;
    actAddDomain: TAction;
    actAddLocalDomain: TAction;
    actAddSubjectArea: TAction;
    alDomainsAndConceptGroups: TActionList;
    pmAdd: TPopupMenu;
    pmAddConceptGroup: TMenuItem;
    pmAddConceptGroupVersion: TMenuItem;
    pmAddDomain: TMenuItem;
    pmAddLocalDomain: TMenuItem;
    pmAddSubjectArea: TMenuItem;
    Bevel1: TBevel;
    fraListOrganiserTree: TfraListOrganiserTree;
    procedure actAddConceptGroupExecute(Sender: TObject);
    procedure actAddConceptGroupVersionExecute(Sender: TObject);
    procedure actAddDomainExecute(Sender: TObject);
    procedure actAddLocalDomainExecute(Sender: TObject);
    procedure actAddSubjectAreaExecute(Sender: TObject);
    procedure btnAddClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
    procedure tvHierarchyChange(Sender: TObject; Node: TFlyNode);
  private
    FExportAction: TAction;
    FLockedNode: TBaseListOrganiserNode;
    procedure OnNodeAdded(newNode: TBaseListOrganiserNode);
    procedure HideAddActions;
    procedure LockDetailFrame(ALockedNode: TBaseListOrganiserNode);
    procedure SetExportAllowed(ANode: TFlyNode);
    procedure ShowRelevantAddActions;
    function GetKeyFromNode(
      ANode: TBaseListOrganiserNode;
      const AClassName: String): String;
  protected
    procedure DoDelete; override;
    procedure SetEditMode(Value: TEditMode); override;
  public
    procedure RemoveCutNode;
    constructor Create(AOwner: TComponent); override;
    function GetCurrentItemDescription: string; override;
    function GetKey(const ATableName: String): String;
    property ExportAction: TAction read FExportAction write FExportAction;
  end;
  
//==============================================================================
implementation

{$R *.dfm}

uses
  SubjectArea, Domain, LocalDomain, ConceptGroup, ConceptGroupVersion,
  ThesaurusEditorData, GeneralData, InterfaceDataModule, ConceptGroupGeneral;

{-==============================================================================
    TfrmListOrganiser
===============================================================================}
{-------------------------------------------------------------------------------
}
constructor TfrmListOrganiser.Create(AOwner: TComponent);
begin
  inherited;
  fraListOrganiserTree.Initialise;
  SetEditMode(emBrowse);
  fraListOrganiserTree.SetFocus;
end;  // TfrmListOrganiser.Create

{-------------------------------------------------------------------------------
  Adds a Concept Group node. 
}
procedure TfrmListOrganiser.actAddConceptGroupExecute(Sender: TObject); 
var
  newNode: TBaseListOrganiserNode;
begin
  newNode := fraListOrganiserTree.AddConceptGroup;
  OnNodeAdded(newNode);
end;  // TfrmListOrganiser.actAddConceptGroupExecute 

{-------------------------------------------------------------------------------
  Adds a concept group version node.
}
procedure TfrmListOrganiser.actAddConceptGroupVersionExecute(Sender: TObject);  
var
  newNode: TBaseListOrganiserNode;
begin                           
  newNode := fraListOrganiserTree.AddConceptGroupVersion;
  OnNodeAdded(newNode);
end;  // TfrmListOrganiser.actAddConceptGroupVersionExecute 

{-------------------------------------------------------------------------------
  Adds a domain node. 
}
procedure TfrmListOrganiser.actAddDomainExecute(Sender: TObject);
var
  newNode: TBaseListOrganiserNode;
begin 
  newNode := fraListOrganiserTree.AddDomain;
  OnNodeAdded(newNode);
end;  // TfrmListOrganiser.actAddDomainExecute 

{-------------------------------------------------------------------------------
  Adds a local domain node. 
}
procedure TfrmListOrganiser.actAddLocalDomainExecute(Sender: TObject);
var
  newNode: TBaseListOrganiserNode;
begin
  newNode := fraListOrganiserTree.AddLocalDomain;
  OnNodeAdded(newNode);
end;  // TfrmListOrganiser.actAddLocalDomainExecute 

{-------------------------------------------------------------------------------
  Adds a subject area node. 
}
procedure TfrmListOrganiser.actAddSubjectAreaExecute(Sender: TObject);
var
  newNode: TBaseListOrganiserNode;
begin
  newNode := fraListOrganiserTree.AddSubjectArea;
  OnNodeAdded(newNode);
end;  // TfrmListOrganiser.actAddSubjectAreaExecute

{-------------------------------------------------------------------------------
}
procedure TfrmListOrganiser.OnNodeAdded(newNode: TBaseListOrganiserNode);
begin
  SetEditMode(emEdit);
  LockDetailFrame(newNode);
end;  // TfrmListOrganiser.AddNode

{-------------------------------------------------------------------------------
  Handles the click on the Add button and shows the pop-up menu. 
}
procedure TfrmListOrganiser.btnAddClick(Sender: TObject);
var
  lMenuLocation: TPoint; 
  newNode: TBaseListOrganiserNode;
begin
  if not Assigned(fraListOrganiserTree.Selected) then
  begin
    newNode := fraListOrganiserTree.AddSubjectArea;
    OnNodeAdded(newNode);
  end else begin
    HideAddActions;
    ShowRelevantAddActions;
    lMenuLocation := btnAdd.ClientToScreen(Point(0, btnAdd.Height));
    pmAdd.Popup(lMenuLocation.X, lMenuLocation.Y);
  end;
end;  // TfrmListOrganiser.btnAddClick 

{-------------------------------------------------------------------------------
  Handle a click of the cancel button. 
}
procedure TfrmListOrganiser.btnCancelClick(Sender: TObject);
begin
  inherited;
  SetEditMode(emBrowse);

  // If the user wanted to add a node, then clicked cancel, the new node is removed.
  // If the user wanted to edit a node, then clicked cancel, nothing happens.
  if Assigned(FLockedNode) then
    if FLockedNode.Key = '' then begin
      fraListOrganiserTree.DeleteNode(FLockedNode);
    end;
end;  // TfrmListOrganiser.btnCancelClick 

{-------------------------------------------------------------------------------
  Handle a click on the save button.
}
procedure TfrmListOrganiser.btnSaveClick(Sender: TObject);
var
  lHoldCursor: TCursor;
begin
  lHoldCursor := Screen.Cursor;
  Screen.Cursor := crHourglass;
  try
    try
      dmGeneral.Connection.BeginTrans;

      // Tell the frame to save its data.
      FCurrentFrame.SaveContent;

      dmGeneral.Connection.CommitTrans;

      // Give the node that has been added the correct caption and key.
      if Assigned(FLockedNode) then begin
        fraListOrganiserTree.UpdateNode(FLockedNode, FCurrentFrame.Caption,
            FCurrentFrame.Key);
      end;

      // Reload the frame to update timestamps
      FCurrentFrame.ReloadContent;

      // If the user has clicked on a different node whilst in edit mode, you
      // now want the frame to show these details.
      if fraListOrganiserTree.Selected <> FLockedNode then
        tvHierarchyChange(self, fraListOrganiserTree.Selected);

      // Empty the locked nodes.
      FLockedNode := nil;

      // Update edit mode
      inherited;  
    except 
      if dmGeneral.Connection.InTransaction then
          dmGeneral.Connection.RollbackTrans;
      raise;
    end;
  finally
    Screen.Cursor := lHoldCursor;
  end;
end;  // TfrmListOrganiser.btnSaveClick

{-------------------------------------------------------------------------------
}
procedure TfrmListOrganiser.DoDelete;
var
  lCursor: TCursor;
begin
  inherited;
  if MessageDlg(Format(ResStr_ConfirmNodeDelete, [ResStr_Row]),
                mtWarning, [mbYes, mbNo], 0) = mrYes then
  begin
    ValidateValue(fraListOrganiserTree.Selected.HasChildren = False,
                  ResStr_CannotBeDeletedNodeHasChildren);
    if Assigned(fraListOrganiserTree.Selected) then begin
      lCursor := Screen.Cursor;
      Screen.Cursor := crHourglass;
      try
        if Assigned(FCurrentFrame) then begin
          FCurrentFrame.DeleteContent;
          FreeAndNil(FCurrentFrame);
        end;
        fraListOrganiserTree.DeleteNode(fraListOrganiserTree.Selected);
      finally
        Screen.Cursor := lCursor;
      end;
    end;
  end;
end;  // TfrmListOrganiser.DoDelete

{-------------------------------------------------------------------------------
}
procedure TfrmListOrganiser.FormActivate(Sender: TObject);
begin
  inherited;
  SetExportAllowed(fraListOrganiserTree.Selected);
end;  // TfrmListOrganiser.FormActivate

{-------------------------------------------------------------------------------
}
procedure TfrmListOrganiser.FormDeactivate(Sender: TObject);
begin
  inherited;
  if Assigned(FExportAction) then FExportAction.Enabled := False;
end;  // TfrmListOrganiser.FormDeactivate

{-------------------------------------------------------------------------------
  Get the caption of the currently selected item in the treeview.
}
function TfrmListOrganiser.GetCurrentItemDescription: string;
begin
  if Assigned(fraListOrganiserTree.Selected) then
    Result := fraListOrganiserTree.Selected.Text;
end;  // TfrmListOrganiser.GetCurrentItemDescription 

{-------------------------------------------------------------------------------
  Remove the items from the pop-up menu.
}
procedure TfrmListOrganiser.HideAddActions;
var
  lIndex: Integer;
begin
  for lIndex := 0 to alDomainsAndConceptGroups.ActionCount-1 do
    if alDomainsAndConceptGroups.Actions[lIndex].Category='AddActions' then
      TAction(alDomainsAndConceptGroups.Actions[lIndex]).Visible := False;
end;  // TfrmListOrganiser.HideAddActions 

{-------------------------------------------------------------------------------
  Lock the current node. This is necessary in case the user clicks on another 
          node whilst they are in edit mode. If there was not a locked node, 
          then the node they clicked on afterwards would get updated, rather 
          than the one they originally intended. 
}
procedure TfrmListOrganiser.LockDetailFrame(ALockedNode: TBaseListOrganiserNode);
begin
  FLockedNode := ALockedNode;
  
  if Assigned(FCurrentFrame) then FCurrentFrame.EditMode := emEdit;
end;  // TfrmListOrganiser.LockDetailFrame

{-------------------------------------------------------------------------------
  Sets the edit mode.
}
procedure TfrmListOrganiser.SetEditMode(Value: TEditMode);
begin
  inherited;
  if Assigned(FCurrentFrame) then FCurrentFrame.EditMode := Value;
  fraListOrganiserTree.Enabled := Value = emBrowse;
  btnEdit.Enabled := btnDelete.Enabled and Assigned(fraListOrganiserTree.Selected);
  btnDelete.Enabled := btnDelete.Enabled and Assigned(fraListOrganiserTree.Selected);
  if Value = emEdit then
    LockDetailFrame(TBaseListOrganiserNode(fraListOrganiserTree.Selected));
end;  // TfrmListOrganiser.SetEditMode  

{-------------------------------------------------------------------------------
  This method was introduced when drag and drop between two Concept Organisers (
          CO) was implemented. Dragging a Concept from one CO and dropping it
          into the other was fine, but the original Concept was not
          disappearing. This method causes the original concept to disappear.
}
procedure TfrmListOrganiser.RemoveCutNode;
begin
  fraListOrganiserTree.RemoveCutNode;
end;  // TfrmListOrganiser.RemoveCutNode

{-------------------------------------------------------------------------------
}
procedure TfrmListOrganiser.SetExportAllowed(ANode: TFLyNode);
begin
  if Assigned(FExportAction) then
    if Assigned(ANode) then
      FExportAction.Enabled := (ANode is TConceptGroupNode) or
                               (ANode is TConceptGroupVersionNode)
    else
      FExportAction.Enabled := False;
end;  // TfrmListOrganiser.SetExportAllowed

{-------------------------------------------------------------------------------
  Show the correct items in the popup menu.
}
procedure TfrmListOrganiser.ShowRelevantAddActions;
begin
  if Assigned(fraListOrganiserTree.Selected) then begin
    if fraListOrganiserTree.Selected is TSubjectAreaNode then begin
      actAddSubjectArea.Visible := True;
      actAddDomain.Visible := True;
    end
    else if fraListOrganiserTree.Selected is TDomainNode then begin
      actAddDomain.Visible := True;
      actAddLocalDomain.Visible := True;
    end
    else if fraListOrganiserTree.Selected is TLocalDomainNode then begin
      actAddLocalDomain.Visible := True;
      actAddConceptGroup.Visible := True;
    end
    else if fraListOrganiserTree.Selected is TConceptGroupNode then begin
      actAddConceptGroup.Visible := True;
      actAddConceptGroupVersion.Visible := True;
    end
    else if fraListOrganiserTree.Selected is TConceptGroupVersionNode then begin
      actAddConceptGroupVersion.Visible := True;
    end;
  end;
end;  // TfrmListOrganiser.ShowRelevantAddActions

{-------------------------------------------------------------------------------
  Handles a change in the treeview.
}
procedure TfrmListOrganiser.tvHierarchyChange(Sender: TObject; Node: TFlyNode);
begin
  // Before we load an item into the frame, it needs to check its not in
  // browse mode.
  if (Assigned(FCurrentFrame) and (FCurrentFrame.EditMode in [emBrowse])) or
     (not Assigned(FCurrentFrame)) then
  begin
    FreeAndNil(FCurrentFrame);
    if Assigned(Node) then begin
      FCurrentFrame := TBaseListOrganiserNode(Node).DetailFrameClass.Create(Self);
      FCurrentFrame.SetAdditionalProperties(TBaseListOrganiserNode(Node));
      FCurrentFrame.Parent := pnlDetails;
      FCurrentFrame.Align := alClient;
      FCurrentFrame.LoadContent;
    end;
  end;
  SetExportAllowed(Node);
end;  // TfrmListOrganiser.tvHierarchyChange

{-------------------------------------------------------------------------------
  Returns the domain/concept group key as specified for the current node
}
function TfrmListOrganiser.GetKey(const ATableName: String): String;
var
  lNode: TBaseListOrganiserNode;
  lNodeType: String;
begin
  lNode := TBaseListOrganiserNode(fraListOrganiserTree.Selected);
  lNodeType := Format('T%sNode', [ATableName]);
  Result := GetKeyFromNode(lNode, lNodeType);
end;

{-------------------------------------------------------------------------------
  Returns the specified key for the specified node
}
function TfrmListOrganiser.GetKeyFromNode(
  ANode: TBaseListOrganiserNode;
  const AClassName: String): String;
var
  lNode: TBaseListOrganiserNode;
begin
  lNode := ANode;
  Result := '';
  while lNode.Parent <> nil do
  begin
    if lNode.ClassNameIs(AClassName) then
    begin
      Result := lNode.Key;
      Break;
    end;
    lNode := TBaseListOrganiserNode(lNode.Parent);
  end;
end;

end.

