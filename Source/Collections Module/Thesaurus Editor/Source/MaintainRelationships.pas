{===============================================================================
  Unit:        MaintainRelationships.pas

  Defines:     TfrmMaintainRelationships

  Description: Form containing peripheral controls to the TfraMaintainRelationships
               and TfraMaintainSemanticRelationships frames.

  Model:       ThesaurusEditor.mpb

  Created:     December 2003

  Last revision information:
    $Revision: 11 $
    $Date: 24/04/08 11:35 $
    $Author: Johnvanbreda $

===============================================================================}

unit MaintainRelationships;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, BaseMDIChildEditorUnit, StdCtrls, ComboListID, ImageListButton,
  ExtCtrls, ADOInt, BaseDetailFrameUnit, FrameMaintainRelationships,
  DataTypes, LuxembourgConstants, GeneralFunctions, DataClasses, DSSDataTypes,
  ResourceStrings, FrameMaintainSemanticRelationships,
  FrameBaseMaintainRelationships;

type
  {-----------------------------------------------------------------------------
    MDI child form that provides the facility to maintain the list of 
    relationship types in the system.  Both Thesaurus_Relation_Type and 
    Semantic_Relation records are included.  The form embeds a details frame 
    appropriate to the entity being edited.  The table edited and the details 
    frame loaded depend on the menu option used when the screen is called.
    The form inherits the Add, Edit and Delete buttons, as well as the Save and 
    Cancel buttons, from TBaseMDIChildEditor.
  }
  TfrmMaintainRelationships = class (TBaseMDIChildEditor, IAdditionalProperties)
    Label1: TLabel;
    lbRelationships: TIDListBox;
    pnlListTop: TPanel;
    procedure lbRelationshipsClick(Sender: TObject);
  private
    FContainedFrameClass: TBaseDetailFrameClass;
    FEditMode: TEditMode;
    FIsSemanticRelation: Boolean;
    FLockedListBoxItemID: Integer;
    FOnFrameNotification: TFrameNotificationEvent;
    function GetProperty(const AName: String): Variant;
    procedure PopulateListBox(AStoredProcName: String);
    procedure SetOnFrameNotification(Value: TFrameNotificationEvent);
  protected
    procedure DoAdd; override;
    procedure DoCancel; override;
    procedure DoDelete; override;
    procedure DoEdit; override;
    procedure DoSave; override;
    function InternalGetProperty(const AName: String): Variant; virtual;
    procedure SetEditMode(Value: TEditMode); override;
  public
    constructor Create(AOwner : TComponent); override;
    function GetCurrentItemDescription: string; override;
    procedure LoadContent(AContainedFrameClass: TBaseDetailFrameClass);
    procedure PrepareCaption;
    procedure PrepareListBox;
    procedure UnloadFrames;
    property EditMode: TEditMode read FEditMode write SetEditMode;
    property IsSemanticRelation: Boolean read FIsSemanticRelation write 
            FIsSemanticRelation;
    property OnFrameNotification: TFrameNotificationEvent read 
            FOnFrameNotification write SetOnFrameNotification;
  end;
  

implementation

uses GeneralData;

{$R *.dfm}

{ TfrmMaintainRelationships }

{-==============================================================================
    TfrmMaintainRelationships
===============================================================================}
{-------------------------------------------------------------------------------
}
constructor TfrmMaintainRelationships.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  // Set the caption
  FLockedListBoxItemID := -1;
  EditMode := emBrowse;
end;  // TfrmMaintainRelationships.Create 

{-------------------------------------------------------------------------------
}
procedure TfrmMaintainRelationships.DoAdd;
begin
  inherited;
  FLockedListBoxItemID := lbRelationships.Add('New relationship', '');
  
  lbRelationships.Selected[FLockedListBoxItemID] := True;
  
  // Clear the frame, so that it is empty and has no key when adding a new node.
  FCurrentFrame := nil;
  lbRelationshipsClick(self);
  
  FLockedListBoxItemID := lbRelationships.IDIndexOf(lbRelationships.CurrentStrID);
  EditMode := emEdit;
end;  // TfrmMaintainRelationships.DoAdd 

{-------------------------------------------------------------------------------
}
procedure TfrmMaintainRelationships.DoCancel;
begin
  inherited;
  FCurrentFrame.EditMode := emBrowse;
  
  if FLockedListBoxItemID <> -1 then
    if lbRelationships.StrID[FLockedListBoxItemID] = '' then
      lbRelationships.Delete(FLockedListBoxItemID);

  FLockedListBoxItemID := -1;
  FCurrentFrame.ReloadContent;
end;  // TfrmMaintainRelationships.DoCancel 

{-------------------------------------------------------------------------------
}
procedure TfrmMaintainRelationships.DoDelete;
var
  lListIndex: Integer;
begin
  if lbRelationships.CurrentStrID <> '' then begin
    inherited;
  
    // Get the list index position of the currently selected item.
    lListIndex := lbRelationships.IDIndexOf(lbRelationships.CurrentStrID);
  
    if ConfirmDeletionYesNo(lbRelationships.CurrentItem) = mrYes then begin
      dmGeneral.RunDeleteStoredProc
            (TfraBaseMaintainRelationships(FCurrentFrame).DeleteStoredProcName,
                                 ['@Key', lbRelationships.CurrentStrID]);
      lbRelationships.DeleteSelected;
      // After a delete, the listbox item that becomes selected is the one prior
      // to the deleted one
      if lListIndex > 0 then
        lbRelationships.Selected[lListIndex - 1] := True
      else
        if lbRelationships.Count > 0 then
          lbRelationships.Selected[lListIndex] := True;
    end;
  end;
  FCurrentFrame.ReloadContent;
end;  // TfrmMaintainRelationships.DoDelete 

{-------------------------------------------------------------------------------
}
procedure TfrmMaintainRelationships.DoEdit;
begin
  if lbRelationships.CurrentStrID <> '' then begin
    inherited;
    FLockedListBoxItemID := lbRelationships.IDIndexOf(lbRelationships.CurrentStrID);
    EditMode := emEdit;
  end;
end;  // TfrmMaintainRelationships.DoEdit 

{-------------------------------------------------------------------------------
}
procedure TfrmMaintainRelationships.DoSave;
begin
  FCurrentFrame.ValidateContent;
  FCurrentFrame.SaveContent;
  
  with lbRelationships do begin
    Items[FLockedListBoxItemID] := FCurrentFrame.Caption;
    StrID[FLockedListBoxItemID] := FCurrentFrame.Key;
  end;
  
  FCurrentFrame.EditMode := emBrowse;
  FLockedListBoxItemID := -1;
  EditMode := emBrowse;
  
  FCurrentFrame.ReloadContent;
  // Now the save is complete, set the edit mode back to Browse.
  inherited;
end;  // TfrmMaintainRelationships.DoSave 

{-------------------------------------------------------------------------------
}
function TfrmMaintainRelationships.GetCurrentItemDescription: string;
begin
  if lbRelationships.ItemIndex > -1 then
    Result := lbRelationships.CurrentItem
  else
    Result := EmptyStr;
end;  // TfrmMaintainRelationships.GetCurrentItemDescription 

{-------------------------------------------------------------------------------
}
function TfrmMaintainRelationships.GetProperty(const AName: String): Variant;
begin
  Result := InternalGetProperty(AName);
end;  // TfrmMaintainRelationships.GetProperty 

{-------------------------------------------------------------------------------
}
function TfrmMaintainRelationships.InternalGetProperty(const AName: String): 
        Variant;
begin
  if AName = PROP_KEY then
    Result := lbRelationships.CurrentStrID
  else
    Result := Unassigned;
end;  // TfrmMaintainRelationships.InternalGetProperty 

{-------------------------------------------------------------------------------
}
procedure TfrmMaintainRelationships.lbRelationshipsClick(Sender: TObject);
begin
  inherited;
  // Before we load an item into the frame, it needs to check its not in
  // browse mode.
  if (Assigned(FCurrentFrame) and (FCurrentFrame.EditMode in [emBrowse])) or
    (not Assigned(FCurrentFrame)) then begin
  
    if IsSemanticRelation then
      LoadContent(TfraMaintainSemanticRelationships)
    else
      LoadContent(TfraMaintainRelationships);
  end;
end;  // TfrmMaintainRelationships.lbRelationshipsClick 

{-------------------------------------------------------------------------------
}
procedure TfrmMaintainRelationships.LoadContent(AContainedFrameClass: 
        TBaseDetailFrameClass);
begin
  if not (FCurrentFrame is AContainedFrameClass) then begin
    UnloadFrames;
    FCurrentFrame := nil;
  end;
  
  if not Assigned(FCurrentFrame) then begin
    FContainedFrameClass := AContainedFrameClass;
    FCurrentFrame := AContainedFrameClass.Create(Self);
    // Set parent, so it appears on the form.
    FCurrentFrame.Parent := pnlDetails;
    FCurrentFrame.TabOrder := 0;
    FCurrentFrame.OnFrameNotification := OnFrameNotification;
    FCurrentFrame.Align := alClient;
  end;
  
  FCurrentFrame.SetAdditionalProperties(Self);
  FCurrentFrame.LoadContent;
  // Set EditMode, this triggers stuff in Frame too.
  EditMode := emBrowse;
end;  // TfrmMaintainRelationships.LoadContent 

{-------------------------------------------------------------------------------
  Populates the list box on the left hand side of the screen. It will either be 
          populated with Semantic_Relation records or Thesaurus_Relation_Type 
          records. 
}
procedure TfrmMaintainRelationships.PopulateListBox(AStoredProcName: String);
var
  lRecordset: _Recordset;
begin
  lbRelationships.Clear;
  lRecordset := dmGeneral.GetRecordset(AStoredProcName, []);
  try
    if lRecordset.RecordCount <> 0 then begin
      lRecordset.MoveFirst;
      while not lRecordset.EOF do begin
        lbRelationships.Add(VarToStr(lRecordset.Fields['Item_Name'].Value),
                            VarToStr(lRecordset.Fields['Key'].Value));
        lRecordset.MoveNext;
      end;
    end;
  finally
    lRecordset.Close;
  end;
  // Ensure a record is selected following the population of the listbox.
  if lbRelationships.Count > 0 then
    lbRelationships.Selected[0] := True;
  lbRelationships.SetFocus;
  lbRelationshipsClick(Self); // Load the frame with the node's details.
end;  // TfrmMaintainRelationships.PopulateListBox 

{-------------------------------------------------------------------------------
}
procedure TfrmMaintainRelationships.PrepareCaption;
begin
  if IsSemanticRelation then
    Caption := ResStr_MaintainSemanticRelationships
  else
    Caption := ResStr_MaintainRelationshipTypes;
  SetWindowMenuItemCaption(Caption);
end;  // TfrmMaintainRelationships.PrepareCaption 

{-------------------------------------------------------------------------------
}
procedure TfrmMaintainRelationships.PrepareListBox;
begin
  if IsSemanticRelation then
    PopulateListBox('usp_SemanticRelations_Select')
  else
    PopulateListBox('usp_ThesaurusRelationTypes_Select');
end;  // TfrmMaintainRelationships.PrepareListBox 

{-------------------------------------------------------------------------------
}
procedure TfrmMaintainRelationships.SetEditMode(Value: TEditMode);
begin
  FEditMode := Value;

  btnAdd.Enabled    := Value = emBrowse;
  btnEdit.Enabled   := Value = emBrowse;
  btnDelete.Enabled := Value = emBrowse;
  btnSave.Enabled   := Value = emEdit;
  btnCancel.Enabled := Value = emEdit;
  lbRelationships.Enabled := Value = emBrowse;
  
  if Assigned(FCurrentFrame) then
    FCurrentFrame.EditMode := Value;
end;  // TfrmMaintainRelationships.SetEditMode 

{-------------------------------------------------------------------------------
}
procedure TfrmMaintainRelationships.SetOnFrameNotification(Value: 
        TFrameNotificationEvent);
begin
  FOnFrameNotification := Value;
end;  // TfrmMaintainRelationships.SetOnFrameNotification 

{-------------------------------------------------------------------------------
}
procedure TfrmMaintainRelationships.UnloadFrames;
  
  procedure FindFrames(AParent: TWinControl);
  var i: Integer;
  begin
    // Check each control on parent
    for i := AParent.ControlCount - 1 downto 0 do
      // If parent is TWinControl, it can contain more controls, so recurse
      if AParent.Controls[i] is TWinControl then begin
        FindFrames(TWinControl(AParent.Controls[i]));
        // Now check is control is frame, and if so, free it.
        if AParent.Controls[i] is TFrame then
          AParent.Controls[i].Free;
      end;
  end;
  
begin
  FindFrames(Self);
end;  // TfrmMaintainRelationships.UnloadFrames 

end.

